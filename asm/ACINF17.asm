*          DATA SET ACINF17    AT LEVEL 008 AS OF 05/01/02                      
*PHASE T60511A,*,NOAUTO                                                         
*INCLUDE CHOPPER                                                                
         TITLE 'ACCOUNTS INFO PROGRAM - RECORD TYPE RE'                         
T60517   CSECT                                                                  
         PRINT NOGEN                                                            
*              TABLES FOR RECORD TYPE 'RE' IN ACCOUNTS INFO PROGRAM             
*                                                                               
*              OVERLAY ADDRESSES                                                
         DC    A(KEYTABLE-T60517)                                               
         DC    A(FILTABLE-T60517)                                               
         DC    A(PREHEADS-T60517)                                               
         DC    A(PRETABLE-T60517)                                               
         DC    A(HEADINGS-T60517)                                               
         DC    A(DATTABLE-T60517)                                               
         DC    A(KNTRYPNT-T60517)                                               
         DC    A(FNTRYPNT-T60517)                                               
         DC    A(DNTRYPNT-T60517)                                               
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
         DC    X'FF00'             CHECK FOR UNIT 3                             
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
KYACCLN  DC    AL1(12)                                                          
         DC    AL2(SCANBLCK-GWS+2)                                              
         DC    AL2(0)                                                           
*                                                                               
         DC    X'00'               END OF KEYS TABLE                            
*                                                                               
         EJECT                                                                  
*              FILTER TABLE COVERED BY DSECT FILTERSD                           
         SPACE 1                                                                
*                                                                               
FILTABLE DC    CL10'FILTER1'                                                    
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
         DC    CL10'SCHEME'                                                     
         DC    CL2'SC'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(0)                                                           
         DC    AL2(0)                                                           
         DC    AL1(L'ACDICODE)                                                  
         DC    X'FF00'                                                          
*                                                                               
         DC    CL10'JOB'                                                        
         DC    CL2'JO'                                                          
         DC    CL8' '                                                           
         DC    AL1(0)                                                           
         DC    AL2(0)                 ENTER OVERLAY FOR VALIDATION              
         DC    AL2((TRSBACNT+3)-TRSUBHD)                                        
         DC    AL1(L'TRSBACNT-3)                                                
         DC    X'FF00'                                                          
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
HEADINGS DC    CL39'ACCOUNT CODE  ------------ACCOUNT NAME-'                    
         DC    CL39'                                       '                    
         DC    CL39'-----------   SCHEME CODE        UNITS '                    
         DC    CL39'                                       '                    
*                                                                               
         EJECT                                                                  
*              DISPLAY DATA TABLE COVERED BY DSECT DATTABD                      
         SPACE 1                                                                
DATTABLE DC    AL2(AKEY-GWS)       DUMMY FOR DISTRIB CODE/UNITS                 
         DC    AL2(0)              TO FORCE ENTRY TO OVERLAY                    
         DC    AL1(SCHEMLEN)                                                    
         DC    X'FF00'                                                          
         DC    AL1(2)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    X'FF'               END OF DISPLAY DATA TABLE                    
*                                                                               
         EJECT                                                                  
KNTRYPNT DS    0D                  CHECK THAT UNIT KEY IS 3                     
         NMOD1 0,**AISE**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60517,RB                                                        
         USING T605TWA,RA                                                       
         XC    RULCOUNT,RULCOUNT   CLEAR COUNT OF X'62' ELS DISPLAYED           
         LA    R6,BINTAB                                                        
         USING LWD,R6                                                           
         MVC   SCHEME,SPACES                                                    
         MVC   JOB,SPACES                                                       
         MVI   JOBLEN,0                                                         
         MVI   OPTIONS,0           AND SCHEME CODE FILTER FLAG                  
         CLI   0(R2),C'3'                                                       
         BE    KXIT                                                             
         MVI   ERROR,UNITNVAL                                                   
         MVI   DMCB,1                                                           
KXIT     XIT1                                                                   
         EJECT                                                                  
FNTRYPNT DS    0D                  HANDLE SCHEME CODE FILTER                    
         NMOD1 0,**AISE**                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)              A(FILTER VALUE)                            
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60517,RB                                                        
         USING FTBD,R6                                                          
         USING FILTERSD,R4                                                      
         LA    RF,BINTAB                                                        
         USING LWD,RF                                                           
         CLC   FILFULKW,=CL10'JOB'                                              
         BE    F30                                                              
         CLC   FILFULKW,=CL10'SCHEME'                                           
         BNE   FXIT                                                             
         SR    R1,R1                                                            
         IC    R1,WORK+1                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SCHEME(0),0(R2)     SAVE THE SCHEME FILTER                       
         CLI   FTBSIGN,C'P'                                                     
         BNE   *+12                                                             
         OI    OPTIONS,PSCHM       POSITIVE SCHEME FILTER                       
         B     FXIT                                                             
         OI    OPTIONS,NSCHM       NEGATIVE SCHEME FILTER                       
         B     FXIT                                                             
         SPACE 1                                                                
F30      LA    RF,BINTAB                                                        
         USING LWD,RF                                                           
         SR    R1,R1                                                            
         IC    R1,WORK+1                                                        
         STC   R1,JOBLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   JOB(0),0(R2)          SAVE JOB FILTER                            
         OI    OPTIONS,JOBFLT                                                   
         CLI   FTBSIGN,C'N'         IF ONE BYTE NEGATIVE FILTER                 
         BNE   FXIT                 (I.E. *J)                                   
         LTR   R1,R1                                                            
         BNZ   FXIT                                                             
         CLI   JOB,C'J'              EXCLUDE ALL JOB ENTRIES                    
         BNE   FXIT                                                             
         MVC   JOB,SPACES                                                       
         OI    OPTIONS,JOBNO                                                    
         SPACE 1                                                                
FXIT     XIT1 REGS=(R2)                                                         
         DROP  RF                                                               
         EJECT                                                                  
DNTRYPNT DS    0D                  HANDLE DISTRIBUTION SCHEME DISPLAYS          
         NMOD1 0,**AIRE**                                                       
         L     RC,0(R1)                                                         
         USING GWS,RC                                                           
         L     RB,APHASE                                                        
         USING T60517,RB                                                        
         USING T605TWA,RA                                                       
         LA    R6,BINTAB                                                        
         USING LWD,R6                                                           
         TM    OPTIONS,JOBNO           EXCLUDE JOBS                             
         BO    *+14                                                             
         MVI   KYACCLN,29           CHANGE KEY LENGTH AFTER FIRST READ          
         XC    KEYMASK+3(29),KEYMASK+3                                          
         MVI   LNECNT,0                                                         
         L     R3,ADRIO                                                         
         USING ACKEYD,R3                                                        
         CLI   ACKEYACC+3,C'*'                                                  
         BE    DEND1                    SKIP THE TOTAL RECORDS                  
         L     RF,ASAVEHIE                                                      
         USING ACHEIRD,RF                                                       
         CLI   ACHRLEVA,12         DETERMINE IF THIS IS LOW ACCOUNT             
         BE    D03                 IF IT IS, KEEP IT                            
         LA    RF,ACHRLEVA                                                      
         LA    R0,3                                                             
         SR    R1,R1                                                            
D01      IC    R1,0(RF)            GET DISPLACEMENT TO LOW LEVEL                
         CLI   16(RF),12                                                        
         BE    D02                                                              
         LA    RF,16(RF)                                                        
         BCT   R0,D01                                                           
         DC    H'0'                BAD LEDGER RECORD                            
         SPACE 1                                                                
D02      LA    R1,3(R1,R3)         R1 TO LOW LEVEL ACCOUNT                      
         CLI   0(R1),C' '                                                       
         BE    DEND1               THIS IS HIGH LEVEL - SKIP IT                 
D03      SR    R0,R0                                                            
         LA    R9,ACRECORD                                                      
D04      CLI   0(R9),X'32'         ONLY WANT LOW LEVEL ACCOUNTS                 
         BE    D05                                                              
         CLI   0(R9),X'43'         AND CONTRA'S                                 
         BE    D05                                                              
         CLI   0(R9),0             END OF RECORD                                
         BE    DEND1               SKIP IT                                      
         IC    R0,1(R9)                                                         
         AR    R9,R0                                                            
         B     D04                                                              
         SPACE 1                                                                
D05      OC    ANAM,ANAM                                                        
         BZ    D06                                                              
         MVC   SAVRNME,SPACES           SAVE LAST NAME ELEMENT                  
         L     RE,ADRIO                                                         
         MVC   SAVRCDE,3(RE)            AND CODE                                
         MVC   SAVJCDE,SPACES                                                   
         ICM   R9,15,ANAM                                                       
         USING ACNAMED,R9                                                       
         SR    R1,R1                                                            
         IC    R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     D06                                                              
         MVC   SAVRNME(0),ACNMNAME                                              
         SPACE 1                                                                
D06      OC    ATRS,ATRS                                                        
         BZ    D09                                                              
         MVC   SAVJCDE,ACKEYCON+3          JOB FILTER CODE                      
         MVC   SAVJNME,SPACES                                                   
         SR    RE,RE                                                            
         ICM   RE,15,ATRS                                                       
         USING TRSUBHD,RE                                                       
         SR    R1,R1                                                            
         IC    R1,TRSBLEN                                                       
         SH    R1,=H'18'                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SAVJNME(0),TRSBNAME                                              
         SPACE 1                                                                
D09      LA    R5,UNITS            R5 = DISPLAY FIELD POINTER                   
         USING SCHD,R5                                                          
         LA    R7,20                                                            
         SH    R7,LINE                                                          
         LA    R7,1(R7)                                                         
         STC   R7,REMLNE           REMAINING LINES                              
         OC    ADIS,ADIS                                                        
         BZ    DEND1               NO DISTRIB ELS                               
         ICM   R9,15,ADIS          R9 = DISTRIB ELEMENT POINTER                 
         USING ACDISTD,R9                                                       
         CLC   INFKEY(4),=C'NEXT'  IF WE HAVE DISPLAYED SOME ELS FOR            
         BE    *+10                THIS RECORD ALREADY, BUMP TO THE             
         XC    RULCOUNT,RULCOUNT   FIRST UNDISPLAYED ELEMENT                    
         LH    RF,RULCOUNT                                                      
         LTR   RF,RF                                                            
         BZ    D15                FIRST TIME START AT FIRST ELEMENT             
         SR    R8,R8                                                            
         SPACE 1                                                                
D10      IC    R8,ACDILEN          BY-PASS ELEMENTS PREVIOUSLY                  
         AR    R9,R8               LOOKED AT                                    
         CLI   0(R9),X'62'         IF NOT FOUND - THEY CHANGED FILTER           
         BNE   D15                 SO START AT FIRST ELEMENT                    
         BCT   RF,D10                                                           
         B     D20                 START AT NEXT ELEMENT                        
         SPACE 1                                                                
D15      XC    RULCOUNT,RULCOUNT   START AT FIRST                               
         L     R9,ADIS                                                          
         SPACE 1                                                                
D20      LA    R6,BINTAB                                                        
         TM    OPTIONS,JOBFLT                                                   
         BZ    D22                      ALL ACCOUNTS AND CONTRA                 
         L     R3,ADRIO                                                         
         USING ACKEYD,R3                                                        
         SPACE 1                                                                
D21      SR    R1,R1                                                            
         IC    R1,JOBLEN                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ACKEYCON+3(0),JOB       JOB SPECIFIC FILTER                      
         BNE   DEND1                                                            
         SPACE 1                                                                
D22      TM    OPTIONS,PSCHM+NSCHM IF SCHEME FILTER ONLY SHOW FILTERED          
         BZ    D25                 NO SCHEME FILTERS                            
         BAS   RE,FILTER           ELEMENT-LEVEL FILTER                         
         BZ    D30                 EXCLUDE THIS ELEMENT                         
         SPACE 1                                                                
D25      CLI   REMLNE,0                                                         
         BE    DEND2               NO MORE ROOM                                 
         SR    R4,R4                                                            
         IC    R4,REMLNE                                                        
         SH    R4,=H'1'                                                         
         STC   R4,REMLNE                                                        
         IC    R4,LNECNT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,LNECNT                                                        
         MVC   0(SCHEMLEN,R5),SPACES                                            
         CLC   SAVRCDE,SPACES                                                   
         BE    D26                                                              
         MVC   SCHRTL,SAVRCDE          CODE                                     
         MVC   SCHNME,SAVRNME          AND NAME TO SCREEN                       
         SPACE 1                                                                
D26      CLC   SAVJCDE,SPACES          IS THERE A JOB CODE                      
         BE    D27                     IF NOT KEEP GOING                        
         CLC   SAVRCDE,SPACES                                                   
         BE    D26A                                                             
         CLI   REMLNE,0                                                         
         BE    DEND2                   NO MORE ROOM                             
         SR    R4,R4                    FOR JOB INFO                            
         IC    R4,REMLNE                                                        
         SH    R4,=H'1'                                                         
         STC   R4,REMLNE                                                        
         IC    R4,LNECNT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,LNECNT                                                        
         LA    R5,SCHEMLEN(R5)         BUMP TO NEXT LINE                        
         MVC   0(SCHEMLEN,R5),SPACES                                            
         SPACE 1                                                                
D26A     MVC   SCHJCDE,SAVJCDE         JOB                                      
         MVC   SCHJNME,SAVJNME         AND NAME TO SCREEN                       
         MVC   SAVJCDE,SPACES                                                   
         MVC   SAVJNME,SPACES                                                   
         SPACE 1                                                                
D27      DS    0H                                                               
         MVC   SAVRCDE,SPACES                                                   
         MVC   SAVRNME,SPACES                                                   
         MVC   SCHCDE,ACDICODE                                                  
         TM    LEDGSTAT,X'01'           PERCENTAGES ARE 4 DP                    
         BO    D28                                                              
         EDIT  ACDIVAL,(12,SCHUNTS),2                                           
         B     D29                                                              
         SPACE 1                                                                
D28      EDIT  ACDIVAL,(12,SCHUNTS),4,DROP=2                                    
         SPACE 1                                                                
D29      LA    R5,SCHEMLEN(R5)                                                  
         SPACE 1                                                                
D30      LH    R1,RULCOUNT         BUMP TO NEXT RULES ELEMENT                   
         LA    R1,1(R1)                                                         
         STH   R1,RULCOUNT                                                      
         SPACE 1                                                                
D32      SR    R8,R8                                                            
         IC    R8,ACDILEN                                                       
         AR    R9,R8                                                            
         CLI   0(R9),X'62'                                                      
         BE    D20                                                              
         CLI   0(R9),0                                                          
         BNE   D32                                                              
         SPACE 1                                                                
DEND1    XC    RULCOUNT,RULCOUNT   NO MORE RULES                                
         B     DXIT                                                             
         SPACE 1                                                                
DEND2    L     R3,ADRIO            NO MORE ROOM ON PAGE                         
         LA    R1,ACKEYACC+14                                                   
         CLI   ACKEYCON,C' '                                                    
         BE    *+8                                                              
         LA    R1,ACKEYCON+14                                                   
         CLI   LEVFILT,0           SO LOWER THE KEY IN IO TO FOOL THE           
         BE    DEND3               ROOT TO READ THIS RECORD AGAIN               
         CLC   LEVFILT,LEVEL+1                                                  
         BH    DEND3                                                            
         SR    RF,RF                                                            
         IC    RF,LEVFILT                                                       
         BCTR  RF,0                                                             
         SLL   RF,4                                                             
         SR    R1,R1                                                            
         IC    R1,SAVEHIER+2(RF)                                                
         L     RE,ADRIO                                                         
         LA    RE,2(RE)                                                         
         AR    R1,RE                                                            
         SPACE 1                                                                
DEND3    SR    RE,RE                                                            
         IC    RE,0(R1)            BY SUBTRACTING 1 FROM LAST SIGNFCNT          
         BCTR  RE,0                BYTE OF KEY                                  
         STC   RE,0(R1)                                                         
         SPACE 1                                                                
DXIT     MVI   DMCB,0              SET FOR NO ERROR                             
         LA    R6,BINTAB                                                        
         LA    R2,UNITS                                                         
         LA    R3,SCHEMLEN        WIDTH OF LINE                                 
         SR    R4,R4                                                            
         IC    R4,LNECNT          NUMBER OF LINES                               
         BCTR  R4,0                                                             
         XIT1  REGS=(R2,R4)       RETURN ADDRESS,LENGTH,NO. EXTRA LNES          
         EJECT                                                                  
*              WHERE POSITIVE SCHEME CODE FILTER(S) SELECT FOR DISPLAY          
*                                                                               
FILTER   NTR1                                                                   
         TM    OPTIONS,PSCHM       IS IT A POSITIVE FILTER                      
         BZ    FILTNEG             NO, MUST BE NEGATIVE                         
         CLC   ACDICODE,SCHEME                                                  
         BE    FILTYES                                                          
         B     FILTNO                                                           
         SPACE 1                                                                
FILTNEG  CLC   ACDICODE,SCHEME                                                  
         BNE   FILTYES                                                          
         SPACE 1                                                                
FILTNO   SR    R1,R1                                                            
         B     *+8                                                              
FILTYES  LA    R1,1                                                             
         LTR   R1,R1                                                            
FEXIT    XIT1                                                                   
         EJECT                                                                  
PSCHM    EQU   X'01'               POSITIVE SCHEME CODE FILTER IN USE           
NSCHM    EQU   X'02'               NEGATIVE SCHEME CODE FILTER IN USE           
JOBFLT   EQU   X'04'               JOB FILTER IN USE                            
JOBNO    EQU   X'08'               NO JOBS                                      
         LTORG                                                                  
LWD      DSECT                                                                  
LNECNT   DS    XL1                 LINES THIS TIME                              
REMLNE   DS    XL1                 REMAINING LINES                              
JOBLEN   DS    CL1                 LENGTH OF JOB KEY                            
JOB      DS    CL12                                                             
SCHEME   DS    CL2                 FILTER SCHEME CODE                           
UNITS    DS    (16*SCHEMLEN)C                                                   
*                                                                               
         EJECT                                                                  
*              DSECT FOR DISPLAY LINE                                           
SCHD     DSECT                                                                  
SCHRTL   DS    CL12                     RETAIL CODE                             
         DS    CL2                                                              
SCHNME   DS    CL36                     RETAILER NAME                           
         ORG   SCHRTL+1                                                         
SCHJCDE  DS    CL12                     JOB CODE                                
         DS    CL1                                                              
SCHJNME  DS    CL36                     JOB NAME                                
         DS    CL7                                                              
SCHCDE   DS    CL2                      SCHEME CODE                             
         DS    CL7                                                              
SCHUNTS  DS    CL12                                                             
SCHEMLEN EQU   *-SCHD              WIDTH OF ELEMENT DISPLAY                     
*              NESTED INCLUDE FOR ACINFDSECT                                    
         PRINT OFF                                                              
       ++INCLUDE ACINFDSECT                                                     
         PRINT ON                                                               
T605TWA  DSECT                                                                  
         ORG   T605TWA+3000                                                     
SAVRCDE  DS    CL12               RETAILER CODE                                 
SAVRNME  DS    CL36               SAVE RETAILER NAME                            
SAVJCDE  DS    CL12               JOB CODE                                      
SAVJNME  DS    CL36               JOB NAME                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACINF17   05/01/02'                                      
         END                                                                    
