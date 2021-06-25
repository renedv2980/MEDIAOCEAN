*          DATA SET ACREPFW02  AT LEVEL 011 AS OF 12/02/04                      
*PHASE ACFW02A                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE SCANNER                                                                
         TITLE 'ACFW02 - PRINT FACWK FILES'                                     
ACFW02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,ACFW02                                                         
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACFWD,RC                                                         
         CLI   MODE,REQFRST                                                     
         BE    RQF00                                                            
XIT      XIT1                                                                   
*                                                                               
*              REQFRST                                                          
*                                                                               
RQF00    MVC   PROG,=C'A56'        PROGRAM                                      
         MVI   SUB,C'1'            SUB-PROGRAM                                  
         MVI   CLASS,C'R'          CLASS                                        
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,TODAY0)                                
         GOTO1 DATCON,DMCB,(0,TODAY0),(1,TODAY1)                                
         MVC   DAY,TODAY1+2        DAY                                          
         XC    SEQN,SEQN           SEQUENCE                                     
         XC    TRACETAB,TRACETAB                                                
         EJECT                                                                  
***********************************************************************         
*        READ AND VALIDATE INPUT CARDS                                *         
***********************************************************************         
*                                                                               
VAL01    GOTO1 CARDS,DMCB,CARDIO,=C'RE00'                                       
         CLC   CARDIO(2),=C'/*'                                                 
         BE    PREP                                                             
         LA    R2,OPTTAB           LIST OF VALID INPUT FIELDS                   
         SR    R1,R1                                                            
*                                                                               
VAL03    IC    R1,0(R2)            LENGTH FOR COMPARE                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   CARDIO(0),1(R2)     MATCH CARD TO TABLE                          
         BE    VAL05                                                            
         LA    R2,L'OPTTAB(R2)                                                  
         CLI   0(R2),X'FF'                                                      
         BNE   VAL03                                                            
         DC    H'0'                INVALID INPUT OPTION                         
*                                                                               
VAL05    SR    RF,RF               GET VALIDATION ROUTINE                       
         ICM   RF,3,9(R2)                                                       
         AR    RF,RB                                                            
         BASR  RE,RF               VALIDATE INPUT OPTION                        
         B     VAL01               GET NEXT CARD                                
         EJECT                                                                  
***********************************************************************         
*        VALIDATE A PROG=                                             *         
***********************************************************************         
*                                                                               
VALPROG  MVC   PROG,CARDIO+5        PROG=XXX                                    
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
*        VALIDATE A SUB=                                              *         
***********************************************************************         
*                                                                               
VALSUB   MVC   SUB,CARDIO+4         SUB=X                                       
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
*        VALIDATE DAY=                                                *         
***********************************************************************         
*                                                                               
VALDAY   PACK  DUB,CARDIO+4(2)     DAY=XX                                       
         MP    DUB,=P'10'                                                       
         MVC   DAY,DUB+6                                                        
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
*        VALIDATE CLASS=                                              *         
***********************************************************************         
*                                                                               
VALCLASS MVC   CLASS,CARDIO+6      CLASS=                                       
         BR    RE                                                               
         SPACE 2                                                                
***********************************************************************         
*        VALIDATE SEQ=                                                *         
***********************************************************************         
*                                                                               
VALSEQ   PACK  DUB,CARDIO+4(3)     SEQ=XXX                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,SEQN                                                        
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE TRACE=                                              *         
***********************************************************************         
*                                                                               
VALTRACE NTR1  ,                                                                
         MVC   WORKA,SPACES                                                     
         MVC   WORKA(74),CARDIO+6   TRACE=001-004,005=0007                      
         LA    R4,TRACETAB         R4=A(TABLE)                                  
         OC    0(4,R4),0(R4)       FIND A SLOT                                  
         BZ    *+12                                                             
         LA    R4,4(R4)                                                         
         B     *-14                                                             
*                                                                               
         LA    R2,WORKA                                                         
         LA    R3,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(C'C',(R2)),(R3),C',=,-'                            
         SR    R0,R0                                                            
         IC    R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                INVALID FORMAT                               
*                                                                               
         USING SCAND,R3            R3=A(SCAN BLOCK)                             
VALTR03  TM    SCVAL1,X'80'        INSURE NUMERIC INPUT ON LHS                  
         BO    *+6                                                              
         DC    H'0'                                                             
         CLI   SCLEN2,0            IF RHS INPUT                                 
         BE    VALTR05                                                          
         TM    SCVAL2,X'80'        INSURE NUMERIC INPUT ON RHS                  
         BO    *+6                                                              
         DC    H'0'                                                             
         CLC   SCBIN1,SCBIN2       AND INSURE LHS LE RHS                        
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALTR05  MVC   0(2,R4),SCBIN1+2    SAVE BOTH LHS                                
         MVC   2(2,R4),SCBIN2+2    AND RHS                                      
         LA    R3,SCANNEXT         LOOK FOR MORE                                
         LA    R4,4(R4)                                                         
         BCT   R0,VALTR03                                                       
         B     XIT                                                              
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE CONTROLS REPORT GENERATION                               
*                                                                               
PREP     ZAP   COUNT,=P'0'                                                      
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         MVI   RCSUBPRG,0                                                       
         OC    TRACETAB,TRACETAB                                                
         BZ    *+8                                                              
         MVI   RCSUBPRG,1                                                       
*                                                                               
         LA    R2,INDEX            BUILD FACWRK INDEX                           
         USING UKRECD,R2                                                        
         XC    UKINDEX,UKINDEX                                                  
         MVC   UKUSRID,ORIGINUM    USER ID                                      
         MVC   UKSYSPRG,PROG       PROGRAM                                      
         MVC   UKSUBPRG,SUB        SUB-PROGRAM (SYSTEM)                         
         MVC   UKDAY,DAY           DAY                                          
         MVC   UKCLASS,CLASS       CLASS                                        
         MVC   UKFILNO,SEQN        SEQUENCE                                     
*                                                                               
         LA    RE,FACWRK           SET SPECIAL DMCB FOR FACWRK I/OS             
         ST    RE,FWAFILE                                                       
         ST    R2,FWANDX                                                        
         L     R3,AIO                                                           
         ST    R3,FWAREC                                                        
         USING RECD,R3                                                          
         L     R4,ABUFFER                                                       
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
         CLC   UKCLASS,CLASS       MUST BE RECOVERY CLASS                       
         BNE   PR10                                                             
*                                                                               
PR30     GOTO1 DATAMGR,FWDMCB,(X'00',=C'REA')                                   
         CLI   8(R1),0                                                          
         BE    PR40                                                             
         TM    8(R1),X'80'         TEST END OF FILE                             
         BO    PR50                                                             
         DC    H'0'                DIE IF DISK/FORMAT ERROR                     
*                                                                               
         DROP  R3                                                               
         USING FWRECD,R3                                                        
PR40     CLC   FWRHDR,=C'SOON'     SKIP SOON CONTROL RECORDS                    
         BE    PR30                                                             
         CLC   FWRHDR,=C'TSO '                                                  
         BE    PR30                                                             
         CLC   FWRUSER,=C'USER='                                                
         BE    PR30                                                             
         CLC   FWRUSER,=C'LAST='                                                
         BE    PR30                                                             
*                                                                               
         DROP  R3                                                               
         USING RECD,R3                                                          
         AP    COUNT,=P'1'                                                      
         BAS   RE,DISPLAY          DISPLAY RECORD IN AIO                        
         B     PR30                                                             
*                                                                               
PR50     GOTO1 ACREPORT                                                         
         EDIT  COUNT,(9,P+1),COMMAS=YES  DISPLAY TOTAL N'RECORDS                
         MVC   P+11(7),=C'RECORDS'                                              
         L     RE,ADBXAREA                                                      
         MVI   BOXREQ-BOXD(RE),C'B'                                             
         GOTO1 ACREPORT                                                         
*                                                                               
PRX      B     XIT                                                              
         DROP  R2,R3,R4                                                         
         EJECT                                                                  
*              DISPLAY RECOVERY RECORD IN AIO                                   
*                                                                               
DISPLAY  NTR1                                                                   
         L     R3,AIO                                                           
         USING RECD,R3                                                          
         LA    R7,RDATA                                                         
         USING ACCRECD,R7                                                       
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
*                                                                               
DIS6     LA    R2,P                R2=A(PRINT LINE)                             
         USING PRNTD,R2                                                         
         MVC   TYPE,=C'ADD'                                                     
         CLI   RRECTY,ADDQ                                                      
         BE    DIS10                                                            
         MVC   TYPE,=C'CPY'                                                     
         CLI   RRECTY,COPYQ                                                     
         BE    DIS10                                                            
         MVC   TYPE,=C'CHG'                                                     
         CLI   RRECTY,CHANGEQ                                                   
         BE    DIS10                                                            
         MVC   TYPE,=C'???'                                                     
*                                                                               
DIS10    LA    R1,FILES            LOOK UP FILE NAME                            
DIS20    CLC   RFILTY,0(R1)                                                     
         BE    DIS30                                                            
         LA    R1,L'FILES(R1)                                                   
         CLI   0(R1),X'FF'                                                      
         BNE   DIS20                                                            
*                                                                               
DIS30    MVC   FILQ,0(R1)                                                       
         MVC   FILNME,1(R1)                                                     
         OC    TRACETAB,TRACETAB   IF RECORD TRACE REQUESTED                    
         BNZ   TRACE                                                            
         EDIT  COUNT,(5,PCOUNT)                                                 
         MVC   PTYPE,TYPE                                                       
         MVC   PFILE,FILNME                                                     
         CLI   FILQ,DIRQ           IF THIS IS DIRECTORY RECORD                  
         BNE   DIS32                                                            
         GOTO1 HEXOUT,DMCB,ACCKSTA,PSTAT,L'ACCKSTA,0  STATUS                    
         GOTO1 HEXOUT,DMCB,ACCKDA,PDA,4,0  DISPLAY DA                           
         B     DISX                                                             
*                                                                               
DIS32    CLI   FILQ,MSTQ           IF THIS IS MASTER RECORD                     
         BNE   DIS50                                                            
         GOTO1 HEXOUT,DMCB,ACCRSTA,PSTAT,L'ACCRSTA,0  STATUS                    
         OC    RVCHR,RVCHR         ELSE IF IN RECOVERY HEADER                   
         BZ    DIS50                                                            
         GOTO1 HEXOUT,DMCB,RVCHR,PDA,4,0  DISPLAY DA                            
*                                                                               
DIS50    GOTO1 HEXOUT,DMCB,ACCKEY,PHKEY,L'PKEY,0   KEY IN HEX                   
         LA    R2,PSECOND                                                       
         MVC   PKEY(L'TRNKEY),RDATA  KEY IN EBCDIC (TRANSLATED)                 
         TR    PKEY(L'TRNKEY),TRNTBL                                            
         GOTO1 ACREPORT                                                         
         L     RE,ADBXAREA                                                      
         MVI   BOXREQ-BOXD(RE),C'M'                                             
*                                                                               
DISX     B     XIT                                                              
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        TRACE DATA BLOCK                                                       
*--------------------------------------------------------------------*          
TRACE    MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT            PRINT ONE BLANK LINE FIRST                   
         MVI   P,C'-'              THEN FILL PRINT LINE WITH '-'S               
         MVC   P+1(131),P                                                       
*                                                                               
         MVC   BLOCK(50),SPACES                                                 
         MVC   BLOCK+1(L'TRACER),TRACER   "TRACE RECORD NO"                     
         LA    R5,BLOCK+L'TRACER+2                                              
         EDIT  COUNT,(5,0(R5)),ALIGN=LEFT  GET L'COUNT                          
         AR    R5,R0                                                            
         AHI   R5,1                                                             
         MVC   0(L'TYPE,R5),TYPE                                                
         AHI   R5,L'TYPE+1                                                      
         MVC   0(L'FILNME,R5),FILNME                                            
         AHI   R5,L'FILNME                                                      
         LA    RF,BLOCK                                                         
         SR    R5,RF                                                            
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P+50(0),BLOCK                                                    
         GOTO1 ACREPORT                                                         
                                                                                
TR10     DS    0H                                                               
         LHI   R3,ACCRFST-ACCKEY                                                
         CLI   FILQ,MSTQ                                                        
         BE    *+8                                                              
         LHI   R3,ACCKLEN                                                       
         GOTO1 PRNTBL,DMCB,0,ACCKEY,C'DUMP',(R3),                      X        
               =X'01C4',(C'P',PRINT)                                            
         CLI   FILQ,DIRQ                                                        
         BE    XIT                                                              
*                                                                               
TR15     LA    R3,ACCRFST          A(RECORD)                                    
         XR    R0,R0                                                            
*                                                                               
TR20     CLI   0(R3),0             EOR                                          
         BE    XIT                                                              
         IC    R0,1(R3)            CURRENT ELEMENT LENGTH                       
         GOTO1 PRNTBL,DMCB,0,(R3),C'DUMP',(R0),                        X        
               =X'01C4',(C'P',PRINT)                                            
         AR    R3,R0               DUMP TO NEXT ELEMENT                         
         B     TR20                                                             
         DROP  R2,R3,R7                                                         
         EJECT                                                                  
*              HEADLINES ETC                                                    
*                                                                               
HOOK     NTR1                                                                   
         CLI   RCSUBPRG,0                                                       
         BNE   XIT                                                              
         L     R4,ADBXAREA                                                      
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
         DROP  R2,R4                                                            
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
*                                                                               
***********************************************************************         
*        INPUT CARD OPTIONS                                           *         
***********************************************************************         
*                                                                               
OPTTAB   DS    0CL11                                                            
         DC    AL1(3),CL8'PROG    ',AL2(VALPROG-ACFW02)                         
         DC    AL1(2),CL8'SUB     ',AL2(VALSUB-ACFW02)                          
         DC    AL1(2),CL8'DAY     ',AL2(VALDAY-ACFW02)                          
         DC    AL1(2),CL8'SEQ     ',AL2(VALSEQ-ACFW02)                          
         DC    AL1(4),CL8'CLASS   ',AL2(VALCLASS-ACFW02)                        
         DC    AL1(4),CL8'TRACE   ',AL2(VALTRACE-ACFW02)                        
         DC    X'FF'                                                            
*                                                                               
AIO      DC    A(IO)                                                            
ABUFFER  DC    A(BUFFER)                                                        
SCANNER  DC    V(SCANNER)                                                       
PRNTBL   DC    V(PRNTBL)                                                        
*                                                                               
FACWRK   DC    C'FACWRK '                                                       
TRACER   DC    C'TRACE RECORD NO.'                                              
*                                                                               
FILES    DS    0CL8                                                             
         DC    X'69',C'ACCDIR '                                                 
         DC    X'6A',C'ACCMST '                                                 
         DC    X'FF',C'UNKNWN '                                                 
*                                                                               
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
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         DS    0D                                                               
         DC    CL8'**IO**'                                                      
IO       DS    XL2000                                                           
*                                                                               
         DS    0D                                                               
         DC    CL8'**IO**'                                                      
BUFFER   DC    8192X'00'           FACWRK BUFFER                                
*                                                                               
         EJECT                                                                  
*              DSECT TO COVER LOCAL WORKING STORAGE                             
*                                                                               
ACFWD    DSECT                                                                  
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
SUB      DS    CL1                 SUB-PROGRAM                                  
DAY      DS    XL1                 PWOS DAY                                     
CLASS    DS    CL1                 CLASS                                        
SEQN     DS    H                   SEQUENCE NUMBER                              
TODAY0   DS    CL6                                                              
TODAY1   DS    XL3                                                              
CARDIO   DS    CL80                                                             
WORKA    DS    CL100               WORK AREA                                    
*                                                                               
RACTN    DS    CL1                 RECORD TYPE (COPY CHNG ADD)                  
COPYQ    EQU   X'01'                                                            
CHANGEQ  EQU   X'02'                                                            
ADDQ     EQU   X'03'                                                            
RECLEN   DS    XL2                 RECORD LENGTH                                
*                                                                               
FILQ     DS    XL1                                                              
DIRQ     EQU   X'69'               DIRECTORY                                    
MSTQ     EQU   X'6A'               MASTER                                       
FILNME   DS    CL7                 FILE NAME                                    
TYPE     DS    CL3                 ADD/CHG                                      
*                                                                               
TRACETAB DS    XL(64*4)            RECORD NUMBERS TO TRACE                      
*                                                                               
BLOCK    DS    10XL32                                                           
         EJECT                                                                  
*              DSECT TO COVER 32-BYTE SCAN BLOCK                                
*                                                                               
SCAND    DSECT                                                                  
SCLEN1   DS    XL1  L'FIELD (OR L'FIRST HALF OF DIVIDED FIELD).                 
SCLEN2   DS    XL1  L'SECOND HALF OF DIVIDED FIELD OR ZERO.                     
SCVAL1   DS    XL1  VALIDITY BITS (X'80'=NUMERIC X'40'=ALPHA X'20'=HEX)         
SCVAL2   DS    XL1  VALIDITY BITS FOR SECOND HALF OF DIVIDED FIELDS.            
SCDISP1  DS    XL1  CUMULATIVE DISPLACEMENT INTO FIELD OF LHS                   
         ORG   *-1                                                              
SCBIN1   DS    F    BINARY VALUE OF VALID NUMERIC FIELDS.                       
SCDISP2  DS    XL1  CUMULATIVE DISPLACEMENT INTO FIELD OF RHS                   
         ORG   *-1                                                              
SCBIN2   DS    F    BINARY VALUE OF SECOND HALF OF DIVIDED FIELDS.              
SCDATA1  DS    CL10 LEFT JUSTIFIED FIELD DATA PADDED WITH SPACES.               
SCDATA2  DS    CL10 DATA FOR SECOND HALF OF DIVIDED FIELDS.                     
SCANNEXT EQU   *  (NOTE - UNDIVIDED FIELDS MAY BE UP TO 20 CHARACTERS.)         
         EJECT                                                                  
*              DSECT TO COVER PRINT LINE                                        
*                                                                               
PRNTD    DSECT                                                                  
BL       DS    CL1                                                              
PCOUNT   DS    CL5                                                              
BC1      DS    CL1                                                              
PTYPE    DS    CL3                                                              
BC2      DS    CL1                                                              
PFILE    DS    CL7                                                              
BC3      DS    CL1                                                              
PHKEY    DS    CL84                                                             
         ORG   PHKEY                                                            
PKEY     DS    CL42                                                             
         DS    CL42                                                             
BC4      DS    CL1                                                              
PSTAT    DS    CL16                                                             
BC5      DS    CL1                                                              
PDA      DS    CL8                                                              
BR       DS    CL1                                                              
         EJECT                                                                  
*              FACWRK RECOVERY RECORD DSECT                                     
*                                                                               
RECD     DSECT                                                                  
RLEN     DS    XL2                                                              
         DS    XL2                                                              
       ++INCLUDE DMRCVRHDR                                                      
RDATA    DS    X                                                                
         EJECT                                                                  
       ++INCLUDE SRUPDD                                                         
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* ACGENPOST                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDLOGOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
* DDMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDMASTD                                                        
         PRINT ON                                                               
* DDREPXTRAD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* DMDTFIS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
* DMWRKRK                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKRK                                                        
         PRINT ON                                                               
* DMWRKRKD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMWRKRD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011ACREPFW02 12/02/04'                                      
         END                                                                    
