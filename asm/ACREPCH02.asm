*          DATA SET ACREPCH02  AT LEVEL 049 AS OF 10/27/02                      
*PHASE ACCH02A,+0                                                               
*INCLUDE HELLO                                                                  
         TITLE 'ACCH - COST HRLY RATES'                                         
ACCH02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACCH**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACCHD,RC                                                         
         LA    R8,PROGPROF                                                      
         USING PROFD,R8                                                         
         EJECT                                                                  
*--------------------------------------------                                   
*        R U N F I R S T                                                        
*--------------------------------------------                                   
*                                                                               
RUNF00   CLI   MODE,RUNFRST                                                     
         BNE   REQF00                                                           
         RELOC RELO                                                             
         LA    RE,RELOTAB                                                       
         LA    R1,ATYPES           RELOCATE A-TYPES                             
RNF01    L     RF,0(RE)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RNF01                                                            
*                                                                               
*        BUILD TABLE OF METHOD CODES                                            
*                                                                               
         LA    R2,METTABL              FIRST CLEAR TABLE                        
         LA    R3,METENT                                                        
RNF03    XC    0(METLEN,R2),0(R2)                                               
         LA    R2,METLEN(R2)                                                    
         BCT   R3,RNF03                                                         
*                                                                               
         USING METD,R2                                                          
         LA    R2,METTABL                                                       
         USING CAHRECD,R6                                                       
         L     R6,ARECORD                                                       
         MVC   CAHKEY,SPACES                                                    
         MVI   CAHKTYP,CAHKTYPQ        X'3E' METHOD (HISTORY) RECORD            
         MVI   CAHKSUB,CAHKSUBQ        X'01'                                    
         MVC   CAHKCPY,RCCOMPFL        COMPANY                                  
         XC    CAHKOFC,CAHKOFC         CLEAR OFFICE TO 000 FOR NOW              
         MVC   SAVEKEY(L'CAHKEY),CAHKEY            SAVE THE KEY                 
         GOTO1 DATAMGR,DMCB,DMRDHI,=CL8'ACCOUNT',ARECORD,ARECORD                
         CLC   CAHKEY(3),SAVEKEY       SHOULD AT LEAST FIND THIS MUCH           
         BE    RNF10                   OF KEY                                   
         DC    H'0'                                                             
*                                                                               
RNF05    GOTO1 DATAMGR,DMCB,DMRSEQ,=CL8'ACCOUNT',ARECORD,ARECORD                
         CLC   CAHKCPY,RCCOMPFL        IF COMPANY HAS CHANGED HAVE READ         
         BNE   RNFXIT                  ALL METHOD RECORDS                       
*                                                                               
RNF10    DS    0H                                                               
         MVC   METNUMBR,CAHKMTHD       METHOD NUMBER FROM KEY TO TABLE          
         L     R4,ARECORD              GET METHOD CODE                          
         MVI   ELCODE,METELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   RNF05                                                            
         USING METELD,R4                                                        
         MVC   METSHORT,METCODE        METHOD CODE TO TABLE                     
*                                                                               
         L     R4,ARECORD                                                       
         MVI   ELCODE,NAMELQ           GET NAME ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   RNF05                                                            
         MVC   METLONG,SPACES          INITIALIZE TO SPACES                     
         USING NAMELD,R4                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BM    XIT                                                              
         EX    R1,*+4                                                           
         MVC   METLONG(0),NAMEREC      METHOD DESCRIPTION TO TABLE              
*                                                                               
         LA    R2,METLEN(R2)           BUMP TABLE                               
         CLI   0(R2),EOT               TABLE IS FULL - SIGNAL TABLE             
         BNE   RNF05                   MUST BE INCREASED                        
         DC    H'0'                                                             
*                                                                               
RNFXIT   DS    0H                                                               
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
*-------------------------------------------                                    
*              R E Q F I R S T                                                  
*-------------------------------------------                                    
*                                                                               
*                                                                               
REQF00   CLI   MODE,REQFRST                                                     
         BNE   LDG00                                                            
         GOTO1 DATCON,DMCB,(0,QSTART),(1,START)                                 
         MVC   END,START                                                        
         CLC   QEND,SPACES                                                      
         BNH   REQF02                                                           
         GOTO1 DATCON,DMCB,(0,QEND),(1,END)                                     
REQF02   MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   FCRDTRNS,C'N'                                                    
         MVI   FCRDHIST,C'N'                                                    
         MVI   METHOD,SPACE                                                     
         MVI   RCSUBPRG,0                                                       
         CLI   QMTHD,SPACE          SPECIFIC METHOD REQUESTED?                  
         BNH   XIT                                                              
         MVC   METHOD,QMTHD                                                     
         MVI   RCSUBPRG,1           PRINT ALLOCATION METHOD IN HEAD             
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*-----------------------------------------------------                          
*              F I R S T  F O R  L E D G E R                                    
*-----------------------------------------------------                          
*                                                                               
*                                                                               
LDG00    CLI   MODE,LEDGFRST                                                    
         BNE   LVA00                                                            
         L     RF,ADLDGHIR                                                      
         SR    R2,R2                       NUMBER OF ACTUAL LEVELS              
         LA    R2,1(R2)                    AT LEAST ONE LEVEL                   
         USING ACLELD,RF                                                        
         MVC   LLEVA(LLEVELLN),ACLVALS     LEVEL LENGTHS/NAMES                  
         LA    R3,LENLEVLS                 INDIVIDUAL LENGTHS OF LEVELS         
         SR    R4,R4                                                            
         LA    R1,LLEVA                    COMBINED LEVEL LENGTHS               
         LA    R0,LLEVLNUM                 MAXIMUM NUMBER OF LEVELS             
LDG02    DS    0H                                                               
         ZIC   R5,0(R1)                    PREVIOUS COMBINED LENGTH             
         SR    R5,R4                       MINUS NEW COMBINED LENGTH            
         BP    *+6                         EQUALS INDIVIDUAL LEVEL LEN          
         DC    H'0'                                                             
         STC   R5,0(R3)                    SAVE INDIVD LENGTH OF LEVEL          
         CLI   0(R1),MAXLEN                LAST LEV HAS MAXLEN FOR ACCT         
         BE    LDG04                                                            
         LA    R2,1(R2)                    ADD TO LEVEL COUNT                   
         ZIC   R4,0(R1)                    COMBINED LENGTH IN R4                
         LA    R1,LLEVALN(R1)              BUMP TO NEXT COMBINED LENGTH         
         LA    R3,L'LENLEVA(R3)            NEXT INDIVDUAL LEN SAVE AREA         
         BCT   R0,LDG02                                                         
         DC    H'0'                                                             
LDG04    STC   R2,NUMLEVLS                 ACTUAL NUMBER OF LEVELS              
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
*-----------------------------------------------------                          
*              L E V A F I R S T                                                
*-----------------------------------------------------                          
*                                                                               
*                                                                               
LVA00    CLI   MODE,LEVAFRST                                                    
         BNE   LVB00                                                            
*                                  1R HIGHER LEVEL CODE,NAME                    
         USING ACTRECD,R7                                                       
         L     R7,ADHEIRA          ADDR OF RECORD                               
*                                  1R HIGHER LEVEL CODE,NAME                    
         MVC   LEVACODE,ACTKACT                                                 
         LA    R3,LVANAME                                                       
         GOTO1 GETNAME,DMCB,ADLVANAM,(R3)   GET THE NAME INTO PARM2             
         B     XIT                                                              
         EJECT                                                                  
*-----------------------------------------------------                          
*              L E V B F I R S T                                                
*-----------------------------------------------------                          
*                                                                               
*                                                                               
LVB00    CLI   MODE,LEVBFRST                                                    
         BNE   LVC00                                                            
*                                                                               
         USING ACTRECD,R7                                                       
         L     R7,ADHEIRB          ADDR OF RECORD                               
*                                  1R HIGHER LEVEL CODE,NAME                    
         MVC   LEVBCODE,ACTKACT                                                 
         LA    R3,LVBNAME                                                       
         GOTO1 GETNAME,DMCB,ADLVBNAM,(R3)   GET THE NAME INTO PARM2             
         B     XIT                                                              
         EJECT                                                                  
*-----------------------------------------------------                          
*              L E V C F I R S T                                                
*-----------------------------------------------------                          
*                                                                               
*                                                                               
LVC00    CLI   MODE,LEVCFRST                                                    
         BNE   ACF00                                                            
*                                                                               
         USING ACTRECD,R7                                                       
         L     R7,ADHEIRC          ADDR OF RECORD                               
*                                  1R HIGHER LEVEL CODE,NAME                    
         MVC   LEVCCODE,ACTKACT                                                 
         LA    R3,LVCNAME                                                       
         GOTO1 GETNAME,DMCB,ADLVCNAM,(R3)   GET THE NAME INTO PARM2             
*                                                                               
         BAS   RE,HEADUP                                                        
         B     XIT                                                              
         EJECT                                                                  
*-----------------------------------------------------                          
*              F I R S T  F O R  A C C O U N T                                  
*-----------------------------------------------------                          
*                                                                               
*                                                                               
ACF00    CLI   MODE,PROCACC                                                     
         BNE   XIT                                                              
         CLI   PROFLOK,C'Y'        SUPPRESS LOCKED ACCOUNTS?                    
         BNE   ACF02                                                            
         L     R2,ADACCSTA                                                      
         USING RSTELD,R2                                                        
         TM    RSTSTAT,RSTSACIL    SUPPRESS LOCKED ACCOUNT                      
         BO    XIT                                                              
ACF02    DS    0H                                                               
         L     R7,ADACC                                                         
         USING ACTRECD,R7                                                       
*                                  CHECK IF IT'S AN OVERHEAD ACCOUNT            
         ZIC   R1,LLEVA            LENGTH OF LEVEL A                            
         BCTR  R1,0                                                             
         EXCLC R1,ACTKACT,NINES    CORPORATE LEVEL OVERHEAD                     
         BE    XIT                                                              
         LA    R2,ACTKACT                                                       
         ZIC   R0,LLEVA                                                         
         AR    R2,R0                                                            
         ZIC   R1,LENLEVB          LENGTH OF LEVEL B                            
         BCTR  R1,0                                                             
         EXCLC R1,0(R2),NINES      OFFICE LEVEL OVERHEAD                        
         BE    XIT                                                              
         LA    R2,ACTKACT                                                       
         ZIC   R0,LLEVC            LENGTH OF LEVEL A+B+C                        
         AR    R2,R0                                                            
         CLC   0(3,R2),NINES       DEPT LEVEL OVERHEAD                          
         BE    XIT                                                              
*                                  NOT AN OVERHEAD ACCOUNT                      
*                                                                               
         USING PRNTD,R5                                                         
         LA    R5,P                                                             
         MVC   P,SPACES                                                         
         ZIC   R1,LENLEVD          LENGTH OF EMPLOYEE CODE                      
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   PEMPL(0),0(R2)       EMPLOYEE CODE TO PRINT LINE                 
         LA    R3,PNAM                                                          
         GOTO1 GETNAME,DMCB,ADACCNAM,(R3)   GET THE NAME INTO PARM2             
*                                                                               
*                                                                               
         USING CPRRECD,R6          COSTING PERSONAL RATES RECORD                
         L     R6,ARECORD                                                       
         MVC   CPRKEY,SPACES                                                    
         MVI   CPRKTYP,CPRKTYPQ    X'3E'                                        
         MVI   CPRKSUB,CPRKSUBQ    SUB RECORD X'07'                             
         MVC   CPRKCPY,RCCOMPFL    COMPANY                                      
         MVC   CPRKUNT(L'EMPLEDG),EMPLEDG                                       
         MVC   CPRKACT,ACTKACT     EMPLOYEE CODE                                
         MVC   SVKEY,CPRKEY                                                     
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',ARECORD,ARECORD                  
         TM    8(R1),X'10'         DID I FIND IT?                               
         BO    ACF20               NO                                           
         BAS   RE,RATEIT           YES, MOVE RATES TO PRINT LINE                
         B     XIT                                                              
*                                                                               
ACF20    MVC   CPRKEY,SVKEY        RESTORE KEY                                  
         CLI   METHOD,C' '                                                      
         BNH   ACF30               WANT ALL RATES                               
         MVC   CPRKMTHD,METHOD                                                  
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',ARECORD,ARECORD                  
         TM    8(R1),X'10'         DID I FIND IT?                               
         BO    XIT                 NO - SKIP LOOKING FOR RATES                  
         BAS   RE,RATEIT           MOVE RATES TO PRINT LINE                     
         B     XIT                                                              
*                                                                               
ACF30    GOTO1 DATAMGR,DMCB,DMRDHI,=C'ACCOUNT',ARECORD,ARECORD                  
*                                                                               
ACF32    CLC   SVKEY(15),CPRKEY                                                 
         BNE   XIT                                                              
         BAS   RE,RATEIT                                                        
         GOTO1 DATAMGR,DMCB,DMRSEQ,=C'ACCOUNT',ARECORD,ARECORD                  
         B     ACF32                                                            
         DROP  R2,R6,R7                                                         
         EJECT                                                                  
*************************************************************                   
*              ROUTINE TO HEADUP NEW PAGE                                       
*************************************************************                   
*                                                                               
HEADUP   NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVI   LINE,1                SET LINE NUMBER TO ONE                     
         LA    R3,HEAD5+12                                                      
         USING HEADD,R3                                                         
         MVC   HEADD(HEADLN),SPACES                                             
         MVC   HEADCODE,LEVACODE                                                
         MVC   HEADDESC,LVANAME      NAME OF OFFICE                             
         LA    R3,HEAD6+12                                                      
         MVC   HEADD(HEADLN),SPACES                                             
         LA    R2,LEVBCODE         DEPARTMENT                                   
         SR    R0,R0                                                            
         IC    R0,LLEVA                                                         
         AR    R2,R0               BUMP PAST OFFICE CODE                        
         SR    R1,R1                                                            
         IC    R1,LENLEVB          ACTUAL LENGTH OF DEPT CODE                   
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   HEADCODE(0),0(R2)                                                
         MVC   HEADDESC,LVBNAME                                                 
         LA    R3,HEAD7+12                                                      
         MVC   HEADD(HEADLN),SPACES                                             
         LA    R2,LEVCCODE          SUB DEPARTMENT                              
         SR    R0,R0                                                            
         IC    R0,LLEVB             BUMP PAST OFFICE/DEPT                       
         AR    R2,R0                                                            
         SR    R1,R1                                                            
         IC    R1,LENLEVC                                                       
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+4                                                           
         MVC   HEADCODE(0),0(R2)                                                
         MVC   HEADDESC,LVCNAME                                                 
         MVC   HEAD6+95(L'METLONG),SPACES                                       
         CLI   RCSUBPRG,1           PRINT ALLOCATION METHOD IN HEAD             
         BNE   HEDUP12                                                          
*                                                                               
         LA    R2,METTABL                                                       
         USING METD,R2                                                          
HEDUP04  CLI   METNUMBR,EOT                                                     
         BE    HEDUP12                                                          
         CLC   METNUMBR,QMTHD                                                   
         BE    HEDUP08                                                          
         LA    R2,METLEN(R2)                                                    
         B     HEDUP04                                                          
HEDUP08  MVC   HEAD6+95(L'METLONG),METLONG                                      
*                                                                               
HEDUP12  B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
* **********************************************************                    
*        LOOKUP HOURLY RATES THEN EDIT INTO PRINT LINE                          
* **********************************************************                    
*                                                                               
RATEIT   NTR1                                                                   
         MVI   FINDIT,C'N'                                                      
         LA    R5,P                                                             
         USING PRNTD,R5                                                         
         L     R7,ARECORD                                                       
         AH    R7,DATADISP         TO FIRST ELEMENT                             
         USING PHRELD,R7                                                        
*                                                                               
RAT02    CLI   PHREL,0             END OF RECORD                                
         BE    RAT99                                                            
         CLI   PHREL,PHRELQ        HOURLY RATE ELEMENT                          
         BNE   RAT04                                                            
         CLC   PHRPER,START        COMPARE MONTH START                          
         BL    RAT04                                                            
         CLC   PHRPER,END          COMPARE MONTH END                            
         BH    RAT04                                                            
         CLI   METHOD,SPACE        METHOD SPECIFIED?                            
         BNH   RAT06                                                            
         CLC   PHRMTH,METHOD       COMPARE METHOD OF ALLOCATION                 
         BE    RAT08                                                            
RAT04    SR    R0,R0               NEXT ELEMENT                                 
         IC    R0,PHRLN                                                         
         AR    R7,R0                                                            
         B     RAT02                                                            
*                                                                               
RAT06    LA    R2,METTABL                                                       
         USING METD,R2                                                          
RAT06A   CLI   METNUMBR,EOT                                                     
         BE    RAT08                                                            
         CLC   METNUMBR,PHRMTH                                                  
         BE    RAT06B                                                           
         LA    R2,METLEN(R2)                                                    
         B     RAT06A                                                           
RAT06B   MVC   PMETHOD(L'METSHORT),METSHORT                                     
*                                                                               
RAT08    CLC   START,END                                                        
         BE    RAT08A                                                           
         XC    WORK,WORK                                                        
         MVC   WORK(L'PHRPER),PHRPER                                            
         GOTO1 DATCON,DMCB,(1,WORK),(6,PMONTH)                                  
RAT08A   SR    R3,R3                                                            
         ICM   R3,1,PHRNUM         NUMBER OF MINI RATE ELEMENTS                 
         BNP   RAT04                                                            
         LA    R6,PHRNTRY          1ST MINI                                     
         USING PHRNTRY,R6                                                       
RAT10    LA    R4,RATTABL                                                       
         USING RATD,R4                                                          
*                                                                               
RAT12    CLI   RTBUKTYP,EOT        END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   PHRTYPE,RTBUKTYP                                                 
         BE    RAT16                                                            
         LA    R4,RATLEN(R4)                                                    
         B     RAT12                                                            
*                                                                               
RAT16    ZAP   DUB,PHRRATE        HOURLY RATE TO 2DP(ORIGINALLY 4)              
         SRP   DUB,64-2,5                                                       
         LA    R2,P                ADDR OF PRINT LINE                           
         AH    R2,RTPRNT           DISP TO CORRECT COLUMN                       
         CURED DUB,(L'PSALRY,(R2)),2                                            
         LA    R6,PHRLN2Q(R6)      NEXT MINI ELEMENT                            
         BCT   R3,RAT10                                                         
*                                                                               
         CLI   LINE,MAXLIN                                                      
         BL    *+8                                                              
         BAS   RE,HEADUP                                                        
         GOTO1 ACREPORT            PRINT THE LINE                               
         MVI   FINDIT,C'Y'                                                      
         B     RAT04               NEXT ELEMENT                                 
*                                                                               
RAT99    CLI   FINDIT,C'Y'         DID YOU FIND ANY FOR EMPLOYEE?               
         BNE   XIT                                                              
         GOTO1 ACREPORT            YES - SKIP A LINE FOR NEXT                   
         B     XIT                                                              
         DROP  R2,R4,R5,R6,R7                                                   
         EJECT                                                                  
*******************************************                                     
*         GETTING THE NAME INTO WORK                                            
*******************************************                                     
*                                                                               
GETNAME  NTR1                                                                   
         L     R2,0(R1)            DID WE PASS ADDR OF ELEMENT                  
         L     R3,4(R1)            ADDR OF WHERE TO PUT THE NAME                
         LTR   R2,R2                                                            
         BNZ   GETNAM3                                                          
         L     R2,ARECORD                                                       
         AH    R2,DATADISP                                                      
GETNAM1  CLI   0(R2),0                                                          
         BE    GETNAM9                                                          
         CLI   0(R2),NAMELQ        NAME ELEMENT                                 
         BE    GETNAM3                                                          
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         LTR   R0,R0                                                            
         BP    GETNAM1                                                          
         DC    H'0'                                                             
         USING NAMELD,R2                                                        
GETNAM3  MVC   0(L'NAMEREC,R3),SPACES                                           
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BM    GETNAM9                                                          
         EX    R1,*+4                                                           
         MVC   0(0,R3),NAMEREC                                                  
GETNAM9  B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
****************************************************                            
*         SAVE ADDRESS OF LAST RECORD READ BY MONACC                            
****************************************************                            
***********************************************************************         
*AVEIT   NTR1                                                         *         
*        L     RF,AMONACC                                             *         
*        USING ACMD,RF                                                *         
*        L     RE,ADACCFIL         SET TO ACCFILE                     *         
*        TM    ACMINDS,ACMIEMUD    IS FILE EMULATED                   *         
*        BZ    *+8                                                    *         
*        L     RE,ACMADDIR         DIRECTORY INSTEAD                  *         
*        L     RE,ISPDKEY-ISDTF(RE)                                   *         
*        LA    RF,*+10                                                *         
*        O     RF,=X'80000000'                                        *         
*        BSM   0,RF                                                   *         
*        MVC   SAVEKEY,0(RE)       SAVE A(DCB KEY)                    *         
*        LA    RF,*+10                                                *         
*        N     RF,=X'7FFFFFFF'                                        *         
*        BSM   0,RF                                                   *         
*        B     XIT                                                    *         
*        EJECT                                                        *         
***************************************************                   *         
*         RESET SEQUENCE LAST RECORD READ BY MONACC                   *         
***************************************************                   *         
*                                                                     *         
*ESETIT  NTR1                                                         *         
*        GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',SAVEKEY,ARECORD        *         
*        CLI   DMCB+8,0                                               *         
*        BE    *+6                                                    *         
*        DC    H'0'                                                   *         
*        B     XIT                                                    *         
***********************************************************************         
*                                                                     *         
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
XIT      XMOD1 1                                                                
         EJECT                                                                  
******************************                                                  
*        CONSTANTS                                                              
******************************                                                  
*                                                                               
RELOTAB  DS    0A                                                               
         DC    A(RECORD)                                                        
         DC    V(HELLO)                                                         
         DC    X'FF'                                                            
*                                                                               
NINES    DC    CL12'99999999999'                                                
EMPLEDG  DC    CL2'1R'                                                          
*                                                                               
*                                                                               
*              TABLE OF HRLY RATES                                              
RATTABL  DS    0D                                                               
         DC    AL1(PHRTSAL),CL3'SAL',Y(PSALRY-PRNTD)                            
         DC    AL1(PHRTBEN),CL3'BEN',Y(PBENFT-PRNTD)                            
         DC    AL1(PHRTPEN),CL3'PEN',Y(PPENS-PRNTD)                             
         DC    AL1(PHRTADM),CL3'ADM',Y(PADMI-PRNTD)                             
         DC    AL1(PHRTTMP),CL3'TMP',Y(PTEMP-PRNTD)                             
         DC    AL1(PHRTBON),CL3'BON',Y(PBONUS-PRNTD)                            
         DC    AL1(PHRTOVT),CL3'OVT',Y(POVT-PRNTD)                              
         DC    AL1(PHRTTOT),CL3'TOT',Y(PTOT-PRNTD)                              
         DC    AL1(EOT)                                                         
*                                                                               
*                                                                               
*              TABLE OF ALLOCATION METHODS                                      
*METTABL  DS    0D                                                              
*         DC    CL1'1',CL5'FULL ',CL17'"FULL ABSORPTION"'                       
*         DC    CL1'2',CL5'STAND',CL17'"STANDARD HOURS" '                       
*         DC    AL1(EOT),CL5'?????',CL17'?????????????????'                     
*                                                                               
METTABL  DS    0H                                                               
         DS    (METENT)CL(METLEN)                                               
         DC    AL1(EOT)                                                         
*                                                                               
RECORD   DS    0D                                                               
         DS    CL42                                                             
         DS    CL2000                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
************************************************                                
*         D S E C T S                                                           
************************************************                                
*                                                                               
*                                                                               
*              DSECT FOR WORKING STORAGE                                        
ACCHD    DSECT                                                                  
*                                                                               
ATYPES   DS    0A                                                               
ARECORD  DS    A                                                                
VHELLO   DS    V                                                                
*                                                                               
RELO     DS    F                                                                
METHOD   DS    CL1                   ALLOCATION METHOD                          
FINDIT   DS    CL1                   DID I FIND A VALID RATE EL                 
ELCODE   DS    CL1                                                              
START    DS    CL2                   PACKED YYMM                                
END      DS    CL2                   PACKED YYMM                                
SAVEKEY  DS    CL42                                                             
SVKEY    DS    CL42                  FOR RATE RECORD                            
LEVACODE DS    CL12                  SAVE OFFICE CODE                           
LEVBCODE DS    CL12                  SAVE DEPT CODE                             
LEVCCODE DS    CL12                  SAVE SUB CODE                              
LVANAME  DS    CL(L'NAMEREC)                                                    
LVBNAME  DS    CL(L'NAMEREC)                                                    
LVCNAME  DS    CL(L'NAMEREC)                                                    
LEVSAV   EQU   *-LEVACODE            LENGTH OF SAVED LEVELS                     
*                                                                               
LLEVELS  EQU   *                                                                
LLEVA    DS    CL(L'ACLVLEN)         1R LEV A LENGTH                            
LLEVANAM DS    CL(L'ACLVDESC)        1R LEV A NAME                              
LLEVALN  EQU   *-LLEVELS                                                        
LLEVB    DS    CL(L'ACLVLEN)         1R LEV B LENGTH (A+B)                      
LLEVBNAM DS    CL(L'ACLVDESC)        1R LEV B NAME                              
LLEVC    DS    CL(L'ACLVLEN)         1R LEV C LENGTH (A+B+C)                    
LLEVCNAM DS    CL(L'ACLVDESC)        1R LEV C NAME                              
LLEVD    DS    CL(L'ACLVLEN)         1R LEV D LENGTH (A+B+C+D)                  
LLEVDNAM DS    CL(L'ACLVDESC)        1R LEV D NAME                              
LLEVELLN EQU   *-LLEVELS                                                        
LLEVLNUM EQU   LLEVELLN/LLEVALN                                                 
*                                                                               
NUMLEVLS DS    XL1        NUMBER OF LEVELS IN 1R                                
LENLEVLS EQU   *                                                                
LENLEVA  DS    XL1        REAL LENGTH OF LEVEL A                                
LENLEVB  DS    XL1        REAL LENGTH OF LEVEL B                                
LENLEVC  DS    XL1        REAL LENGTH OF LEVEL C                                
LENLEVD  DS    XL1        REAL LENGTH OF LEVEL D                                
LENLEVLN EQU   *-LENLEVLS                                                       
LENLVNUM EQU   LENLEVLN/L'LENLEVA                                               
*                                                                               
ELIST    DS    3A                                                               
ELERR    DS    0XL1                                                             
ELADDR   DS    A                                                                
ELADDR2  DS    A                                                                
*                                                                               
         EJECT                                                                  
*              DSECT FOR RATTABL HOURLY RATES TABLE                             
*                                                                               
*                                                                               
RATD     DSECT                                                                  
RTBUKTYP DS    CL1                 HOURLY RATE BUCKET TYPE                      
RTDESC   DS    CL3                 HOURLY RATE DESCRIPTION CODE                 
RTPRNT   DS    Y                   DISP TO PRNTD COLUMN FOR HOURLY RATE         
RATLEN   EQU   *-RATD                                                           
*                                                                               
*                                                                               
*              DSECT FOR METHOD OF ALLOCATION TABLE                             
*                                                                               
*                                                                               
METD     DSECT                                                                  
METNUMBR DS    CL1                 METHOD NUMBER                                
METSHORT DS    CL3                 "SHORT" NAME FOR COLUMN                      
METLONG  DS    CL36                "LONG" NAME FOR TITLE                        
METLEN   EQU   *-METD                                                           
*                                                                               
*                                                                               
*              DSECT FOR PROFILES                                               
*                                                                               
PROFD    DSECT                                                                  
PROFLOK  DS    CL1             SUPPRESS LOCKED 1R ACCOUNTS Y,N                  
         DS    CL15            N/D                                              
*                                                                               
*                                                                               
*                                                                               
*              DSECT FOR PRINT LINE                                             
*                                                                               
PRNTD    DSECT                                                                  
PEMPL    DS    CL7                 EMPLOY CODE                                  
         DS    CL1                 SPACES                                       
PNAM     DS    CL26                NAME                                         
         DS    CL1                 SPACES                                       
PMONTH   DS    CL6                 MMM/YY                                       
         DS    CL2                 SPACES                                       
PMETHOD  DS    CL5                 METHOD                                       
         DS    CL2                 SPACES                                       
PSALRY   DS    CL7                 SALARY                                       
         DS    CL2                 SPACES                                       
PBENFT   DS    CL7                 BENEFITS                                     
         DS    CL2                 SPACES                                       
PPENS    DS    CL7                 PENSIONS                                     
         DS    CL2                 SPACES                                       
PADMI    DS    CL7                 ADMINISTRATION                               
         DS    CL2                 SPACES                                       
PTEMP    DS    CL7                 TEMPORARY                                    
         DS    CL2                 SPACES                                       
PBONUS   DS    CL7                 BONUS                                        
         DS    CL2                 SPACES                                       
POVT     DS    CL7                 OVERTIME                                     
         DS    CL2                 SPACES                                       
PTOT     DS    CL7                 TOTAL HRLY RATE                              
PRNTLN   EQU   *-PRNTD             LENGTH OF PRINT LINE                         
*                                                                               
*                                                                               
*              DSECT FOR HEADLINES                                              
*                                                                               
HEADD    DSECT                                                                  
HEADCODE DS    CL6                 1R CODE                                      
HEADDESC DS    CL(L'NAMEREC)       DESCRIPTION                                  
HEADLN   EQU   *-HEADD                                                          
         EJECT                                                                  
*                                                                               
**************************************                                          
*        EQUATES                                                                
**************************************                                          
*                                                                               
*                                                                               
EOT      EQU   X'FF'               END OF TABLE                                 
SPACE    EQU   X'40'               SINGLE SPACE                                 
MAXLEN   EQU   12                  MAX LENGTH OF AN ACCOUNT                     
MAXLIN   EQU   56                  MAX LENGTH OF AN ACCOUNT                     
METENT   EQU   20                                                               
*                                                                               
*ACGENFILE                                                                      
*ACGENMODES                                                                     
*ACREPWORKD                                                                     
*DDREPMASTD                                                                     
*ACMASTD                                                                        
*DMDTFIS                                                                        
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE ACMASTD                                                        
*********INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049ACREPCH02 10/27/02'                                      
         END                                                                    
