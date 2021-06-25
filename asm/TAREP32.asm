*          DATA SET TAREP32    AT LEVEL 014 AS OF 05/01/02                      
*PHASE T70332A                                                                  
         TITLE 'T70332 - DISPLAY FACWK FILES'                                   
T70332   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T70332                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC             RC=A(CONTROLLER W/S)                         
         L     RA,ATWA                                                          
         USING T703FFD,RA          RA=A(SCREEN)                                 
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9          R9=A(SYSTEM W/S)                             
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8           R8=A(SPOOL DSECT)                            
         LA    R7,BUFF                                                          
         LA    R7,8(R7)                                                         
         USING LOCALD,R7                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE SCREEN                              
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 3                                                                
         CLI   MODE,PRINTREP       PROCESS REPORT                               
         BNE   XIT                                                              
         BAS   RE,PREP                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE SCREEN                                                  
         SPACE 1                                                                
VKEY     NTR1                                                                   
         LA    R2,SFWPROGH         PROGRAM                                      
         MVC   PROG,=C'TCK'                                                     
         CLI   5(R2),0                                                          
         BE    VK2                                                              
         CLI   5(R2),3                                                          
         BL    VK2                                                              
         MVC   PROG,8(R2)                                                       
         SPACE 1                                                                
VK2      MVC   SUBPROG,SFWSUB      SUB-PROGRAM                                  
         SPACE 1                                                                
         LA    R2,SFWDAYH          DAY                                          
         MVC   DAY,TGTODAY1+2                                                   
         CLI   5(R2),2                                                          
         BNE   VK4                                                              
         PACK  DUB,8(2,R2)                                                      
         MVO   FULL(3),DUB+6(2)                                                 
         MVC   DAY,FULL+1                                                       
         SPACE 1                                                                
VK4      LA    R2,SFWSEQH          SEQUENCE NUMBER                              
         XC    SEQUENCE,SEQUENCE                                                
         CLI   5(R2),0                                                          
         BE    VK10                                                             
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         STH   R1,SEQUENCE                                                      
         SPACE 1                                                                
VK10     LA    R2,SFWTRCH          TRACE RECORDS                                
         XC    TRACETAB,TRACETAB                                                
         CLI   5(R2),0                                                          
         BE    VKX                                                              
         LA    R3,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3),C',=,-'                                   
         ZIC   R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    FLDINV                                                           
         USING SCAND,R3            R3=A(SCAN BLOCK)                             
         LA    R4,TRACETAB         R4=A(TABLE)                                  
         SPACE 1                                                                
VK20     TM    SCVAL1,X'80'        INSURE NUMERIC INPUT ON LHS                  
         BZ    FLDINV                                                           
         CLI   SCLEN2,0            IF RHS INPUT                                 
         BE    VK30                                                             
         TM    SCVAL2,X'80'        INSURE NUMERIC INPUT ON RHS                  
         BZ    FLDINV                                                           
         CLC   SCBIN1,SCBIN2       AND INSURE LHS LE RHS                        
         BH    FLDINV                                                           
         SPACE 1                                                                
VK30     MVC   0(2,R4),SCBIN1+2    SAVE BOTH LHS                                
         MVC   2(2,R4),SCBIN2+2    AND RHS                                      
         SPACE 1                                                                
         LA    R3,SCANNEXT         LOOK FOR MORE                                
         LA    R4,4(R4)                                                         
         BCT   R0,VK20                                                          
         SPACE 1                                                                
VKX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS REPORT GENERATION                               
         SPACE 1                                                                
PREP     NTR1                                                                   
         ZAP   COUNT,=P'0'                                                      
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         LA    R1,MYSPECS                                                       
         ST    R1,SPECS                                                         
         MVI   RCSUBPRG,0                                                       
         OC    TRACETAB,TRACETAB                                                
         BZ    *+8                                                              
         MVI   RCSUBPRG,1                                                       
         SPACE 1                                                                
         LA    R2,INDEX            BUILD FACWRK INDEX                           
         USING UKRECD,R2                                                        
         XC    UKINDEX,UKINDEX                                                  
         MVC   UKUSRID,TWAORIG     USER ID                                      
         MVC   UKSYSPRG,PROG       PROGRAM                                      
         MVC   UKSUBPRG,SUBPROG    SUB-PROGRAM                                  
         MVC   UKDAY,DAY           DAY                                          
         MVI   UKCLASS,C'R'        CLASS                                        
         MVC   UKFILNO,SEQUENCE    SEQUENCE                                     
*                                                                               
         LA    RE,FACWRK           SET SPECIAL DMCB FOR FACWRK I/OS             
         ST    RE,FWAFILE                                                       
         ST    R2,FWANDX                                                        
         L     R3,AIO                                                           
         ST    R3,FWAREC                                                        
         USING RECD,R3                                                          
         LA    R4,BUFFER                                                        
         ST    R4,FWABUF                                                        
         USING WKRECD,R4                                                        
         GOTO1 DATAMGR,FWDMCB,(X'00',=C'BUF')                                   
*                                                                               
PR10     GOTO1 DATAMGR,FWDMCB,(X'08',=C'IND')                                   
         CLI   8(R1),0                                                          
         BE    PR20                                                             
         TM    8(R1),X'80'         TEST END OF INDEX                            
         BO    PRX                                                              
         DC    H'0'                DIE IF DISK ERROR                            
*                                                                               
PR20     DS    0H                                                               
***      TM    UKSTAT,WKSTHO       FILE MUST BE HOLD                            
***      BZ    PR10                                                             
***      TM    UKSTAT,WKSTKE       AND NOT KEEP                                 
***      BO    PR10                                                             
         CLI   UKCLASS,C'R'        MUST BE RECOVERY CLASS                       
         BNE   PR10                                                             
*                                                                               
PR30     GOTO1 DATAMGR,FWDMCB,(X'00',=C'REA')                                   
         CLI   8(R1),0                                                          
         BE    PR40                                                             
         TM    8(R1),X'80'         TEST END OF FILE                             
         BO    PR50                                                             
         DC    H'0'                DIE IF DISK/FORMAT ERROR                     
*                                                                               
PR40     AP    COUNT,=P'1'                                                      
         BAS   RE,DISPLAY          DISPLAY RECORD IN AIO                        
         B     PR30                                                             
*                                                                               
PR50     BAS   RE,PRNTIT                                                        
         EDIT  COUNT,(9,P+1),COMMAS=YES  DISPLAY TOTAL N'RECORDS                
         MVC   P+11(7),=C'RECORDS'                                              
         BAS   RE,PRNTIT                                                        
*                                                                               
PRX      B     XIT                                                              
         EJECT                                                                  
*              DISPLAY RECOVERY RECORD IN AIO                                   
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         OC    TRACETAB,TRACETAB   IF RECORD TRACE REQUESTED                    
         BZ    DIS6                                                             
         ZAP   DUB,COUNT           DETERMINE IF THIS RECORD REQUESTED           
         CVB   R1,DUB              R1=CURRENT RECORD NUMBER                     
         LA    R4,TRACETAB                                                      
DIS2     OC    0(2,R4),0(R4)       END OF TABLE                                 
         BZ    DISX                                                             
         OC    2(2,R4),2(R4)       IF END NUMBER NOT DEFINED                    
         BNZ   DIS3                                                             
         CLM   R1,3,0(R4)          MUST BE EQ START NUMBER                      
         BNE   DIS4                                                             
         B     DIS6                                                             
DIS3     CLM   R1,3,0(R4)          ELSE MUST BE GE START NUMBER                 
         BL    DIS4                                                             
         CLM   R1,3,2(R4)          AND LE END NUMBER                            
         BNH   DIS6                                                             
DIS4     LA    R4,4(R4)            TRY NEXT ENTRY IN TABLE                      
         B     DIS2                                                             
         SPACE 1                                                                
DIS6     LA    R2,P                R2=A(PRINT LINE)                             
         USING PRNTD,R2                                                         
         EDIT  COUNT,(5,PCOUNT)    RECORD COUNT                                 
         SPACE 1                                                                
         MVC   PTYPE,=C'ADD'                                                    
         CLI   RRECTY,ADDQ                                                      
         BE    DIS10                                                            
         MVC   PTYPE,=C'CPY'                                                    
         CLI   RRECTY,COPYQ                                                     
         BE    DIS10                                                            
         MVC   PTYPE,=C'CHG'                                                    
         CLI   RRECTY,CHANGEQ                                                   
         BE    DIS10                                                            
         MVC   PTYPE,=C'???'                                                    
         SPACE 1                                                                
DIS10    LA    R1,FILES            LOOK UP FILE NAME                            
DIS20    CLC   RFILTY,0(R1)                                                     
         BE    DIS30                                                            
         LA    R1,L'FILES(R1)                                                   
         CLI   0(R1),X'FF'                                                      
         BNE   DIS20                                                            
         SPACE 1                                                                
DIS30    MVC   PFILE,1(R1)                                                      
         SPACE 1                                                                
         CLC   =C'DIR',PFILE+3     IF THIS IS DIRECTORY RECORD                  
         BNE   DIS32                                                            
         GOTO1 HEXOUT,DMCB,RDATA+TLDRSTAT-TLDRD,PSTAT,2,0  STATUS               
         LA    RF,RDATA+TLDRDA-TLDRD  PRINT D/A FROM RECORD                     
         B     DIS35                                                            
         SPACE 1                                                                
DIS32    GOTO1 HEXOUT,DMCB,RDATA+TLRCSTAT-TLRCD,PSTAT,2,0  STATUS               
         OC    RVCHR,RVCHR         ELSE IF IN RECOVERY HEADER                   
         BZ    DIS40                                                            
         LA    RF,RVCHR            DISPLAY FROM THERE                           
DIS35    GOTO1 HEXOUT,DMCB,(RF),PDA,4,0                                         
         SPACE 1                                                                
DIS40    OC    TRACETAB,TRACETAB   IF RECORD TRACE REQUESTED                    
         BZ    DIS50                                                            
         MVI   FORCEHED,C'Y'                                                    
         EDIT  COUNT,(5,BLOCK),ALIGN=LEFT  GET L'COUNT                          
         MVC   WORK(17),=C'TRACE RECORD NO. '                                   
         LA    RF,5                                                             
         SR    RF,R0                                                            
         LA    R1,PCOUNT(RF)                                                    
         MVC   WORK+17(PHKEY-PCOUNT),0(R1) MOVE PRINT LINE TO WORK              
         LCR   RF,RF                                                            
         AH    RF,=AL2(PHKEY-PCOUNT+17-1)                                       
         CLC   =C'DIR',PFILE+3             IF THIS IS DIRECTORY RECORD          
         BNE   DIS45                                                            
         MVC   P,SPACES                    ERASE PRINT LINE                     
         GOTO1 TRACE,DMCB,RDATA,L'TLDRREC,WORK,(RF)  THEN TRACE IT              
         B     DISX                                                             
         SPACE 1                                                                
DIS45    MVC   P,SPACES                    ERASE PRINT LINE                     
         GOTO1 TRACE,DMCB,RDATA,0,WORK,(RF)  ELSE TRACE THE FILE RECORD         
         B     DISX                                                             
         SPACE 1                                                                
DIS50    GOTO1 HEXOUT,DMCB,RDATA,PHKEY,L'TLDRKEY,0   KEY IN HEX                 
         SPACE 1                                                                
         MVC   PKEY(L'TLDRKEY),RDATA  KEY IN EBCDIC (TRANSLATED)                
         TR    PKEY(L'TLDRKEY),TRNTBL                                           
         SPACE 1                                                                
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
DISX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE                                          
         SPACE 2                                                                
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              HEADLINES ETC                                                    
         SPACE 1                                                                
HOOK     NTR1                                                                   
         CLI   RCSUBPRG,0                                                       
         BNE   XIT                                                              
         L     R4,ABOX                                                          
         LTR   R4,R4                                                            
         BZ    XIT                                                              
         USING BOXD,R4                                                          
         MVC   BOXCOLS,SPACES                                                   
         LA    R2,BOXCOLS                                                       
         USING PRNTD,R2                                                         
         MVI   BL,C'L'                                                          
         MVI   BC1,C'C'                                                         
         MVI   BC2,C'C'                                                         
         MVI   BC3,C'C'                                                         
         MVI   BC4,C'C'                                                         
         MVI   BC5,C'C'                                                         
         MVI   BC6,C'C'                                                         
         MVI   BR,C'R'                                                          
         MVC   BOXROWS,SPACES                                                   
         MVI   BOXROWS+4,C'T'                                                   
         MVI   BOXROWS+6,C'M'                                                   
         MVI   BOXROWS+58,C'B'                                                  
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0                                                         
         MVI   BOXYORN,C'Y'                                                     
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS, EXITS                                                    
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 ERREX                                                            
         SPACE 1                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
FACWRK   DC    C'FACWRK '                                                       
         SPACE 1                                                                
FILES    DS    0CL7                                                             
         DC    X'71',C'TALDIR'                                                  
         DC    X'72',C'TALFIL'                                                  
         DC    X'75',C'CHKDIR'                                                  
         DC    X'76',C'CHKFIL'                                                  
         DC    X'FF',C'UNKNWN'                                                  
         SPACE 1                                                                
TRNTBL   DC    74C'.',X'4A4B4C4D4E4F50'                                         
         DC    09C'.',X'5A5B5C5D5E5F6061'                                       
         DC    09C'.',X'6B6C6D6E6F'                                             
         DC    10C'.',X'7A7B7C7D7E7F'                                           
         DC    01C'.',X'818283848586878889'                                     
         DC    07C'.',X'919293949596979899'                                     
         DC    08C'.',X'A2A3A4A5A6A7A8A9'                                       
         DC    23C'.',C'ABCDEFGHI'                                              
         DC    07C'.',C'JKLMNOPQR'                                              
         DC    08C'.',C'STUVWXYZ'                                               
         DC    06C'.',C'0123456789'                                             
         DC    06C'.'                                                           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SPROG 0,1                                                              
         SSPEC H1,2,RUN                                                         
         SSPEC H1,53,C'FACWRK RECOVERY FILE REPORT'                             
         SSPEC H2,53,27X'BF'                                                    
         SSPEC H3,2,AGYNAME                                                     
         SSPEC H1,99,REPORT                                                     
         SSPEC H1,115,PAGE                                                      
         SSPEC H2,99,REQUESTOR                                                  
         SPACE 1                                                                
         SPROG 0                                                                
         SSPEC H6,2,C'COUNT TYP  FILE     D/A'                                  
         SSPEC H6,57,C'HEX KEY'                                                 
         SSPEC H6,93,C'STAT'                                                    
         SSPEC H6,107,C'KEY'                                                    
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
         SPACE 1                                                                
LOCALD   DSECT                                                                  
FWDMCB   DS    0XL24               FACWRK DMCB                                  
FWAACTN  DS    A                                                                
FWAFILE  DS    A                                                                
FWANDX   DS    A                                                                
FWAREC   DS    A                                                                
FWABUF   DS    A                                                                
         DS    A                                                                
*                                                                               
COUNT    DS    PL4                 RECORD COUNT                                 
*                                                                               
INDEX    DS    XL32                FACWRK INDEX                                 
PROG     DS    CL3                 PROGRAM FOR FACWRK FILE                      
SUBPROG  DS    CL1                 SUB-PROGRAM                                  
DAY      DS    XL1                 PWOS DAY                                     
SEQUENCE DS    H                   SEQUENCE NUMBER                              
*                                                                               
RACTN    DS    CL1                 RECORD TYPE (COPY CHNG ADD)                  
COPYQ    EQU   X'01'                                                            
CHANGEQ  EQU   X'02'                                                            
ADDQ     EQU   X'03'                                                            
RECLEN   DS    XL2                 RECORD LENGTH                                
*                                                                               
TRACETAB DS    XL(64*4)            RECORD NUMBERS TO TRACE                      
*                                                                               
BUFFER   DS    4096C               FACWRK BUFFER                                
LOCALX   EQU   *                                                                
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
         SPACE 1                                                                
PRNTD    DSECT                                                                  
BL       DS    CL1                                                              
PCOUNT   DS    CL5                                                              
BC1      DS    CL1                                                              
PTYPE    DS    CL3                                                              
BC2      DS    CL1                                                              
PFILE    DS    CL6                                                              
BC3      DS    CL1                                                              
PDA      DS    CL8                                                              
BC4      DS    CL1                                                              
PHKEY    DS    CL64                                                             
BC5      DS    CL1                                                              
PSTAT    DS    CL4                                                              
BC6      DS    CL1                                                              
PKEY     DS    CL32                                                             
BR       DS    CL1                                                              
         EJECT                                                                  
*              FACWRK RECOVERY RECORD DSECT                                     
         SPACE 1                                                                
RECD     DSECT                                                                  
RLEN     DS    XL2                                                              
         DS    XL2                                                              
       ++INCLUDE DMRCVRHDR                                                      
RDATA    DS    X                                                                
         EJECT                                                                  
       ++INCLUDE DMWRKRD                                                        
         EJECT                                                                  
       ++INCLUDE TAREPFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TAREPD2D                                                       
         EJECT                                                                  
* DDGENTWA   (MUST FOLLOW LAST SCREEN)                                          
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDBIGBOX                                                                      
* TAGENFILE                                                                     
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DMWRKRK                                                                       
* TAREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DMWRKRK                                                        
       ++INCLUDE TAREPWORKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014TAREP32   05/01/02'                                      
         END                                                                    
