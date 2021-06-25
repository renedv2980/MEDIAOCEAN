*          DATA SET GEKEY00S   AT LEVEL 046 AS OF 05/01/02                      
*PHASE TF0500A,+0                                                               
*INCLUDE DECODE                                                                 
*INCLUDE KHDUMMY                                                                
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:       TF0500 - PFM INTERFACE FOR KEYS (PFMINT00)            *         
*                                                                     *         
*  COMMENTS:    GETS CONTROL VIA PFM WITH THE = OPTION IN THE RECORD  *         
*               ID FIELD.                                             *         
*                                                                     *         
*  CALLED FROM: MONITOR. ON EXIT P1(1) = X'FF' IF ERROR, ELSE X'00'   *         
*                                                                     *         
*  CALLS TO:    GENCON                                                *         
*                                                                     *         
*  INPUTS:      SCREEN GEKEYFF (TF05FF)                               *         
*                                                                     *         
*  LOCALS:      REGISTER USAGE                                        *         
*               R0 - WORK                                             *         
*               R1 - WORK                                             *         
*               R2 - WORK                                             *         
*               R3 - WORK                                             *         
*               R4 - WORK                                             *         
*               R5 - WORK                                             *         
*               R6 - GETEL REGISTER                                   *         
*               R7 - SECOND BASE                                      *         
*               R8 - SPOOLD                                           *         
*               R9 - SYSD                                             *         
*               RA - TWA                                              *         
*               RB - FIRST BASE                                       *         
*               RC - GEND                                             *         
*               RD - SYSTEM                                           *         
*               RE - SYSTEM                                           *         
*               RF - SYSTEM                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'TF0500 - PFM INTERFACE FOR KEYS (GEKEY00)'                      
TF0500   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,*TF0500*,R7,RR=R2,CLEAR=YES                              
         ST    R2,RELO             SAVE RELOCATION FACTOR                       
         LR    R9,R1               A(SYSTEM PARAMETERS)                         
         LR    R8,RC               ADDRESS OF WORKING STORAGE                   
         USING SPOOLD,R8                                                        
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         LA    RC,SPOOLEND                                                      
         USING GEND,RC                                                          
*                                                                               
         ST    R9,SYSPARMS                                                      
         LA    R9,IO                                                            
         AH    R9,=Y(LENIOAS)      3 2000 BYTE I/O AREAS PLUS LABELS            
         USING SYSD,R9                                                          
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    R7,BASER7                                                        
         L     RE,SYSPARMS                                                      
*                                                                               
         L     R2,16(RE)           A(COMFACS)                                   
         USING COMFACSD,R2                                                      
         MVC   GETFACT,CGETFACT                                                 
         MVC   CALLOV,CCALLOV                                                   
         DROP  R2                                                               
*                                                                               
         L     RA,4(RE)            A(TWA)                                       
         USING CONHEADH-64,RA                                                   
*                                                                               
         CLI   CONACTNH+5,0                                                     
         BNE   *+18                                                             
         MVC   CONACTNH+8(5),=C'BUILD'                                          
         MVI   CONACTNH+5,5                                                     
         OI    CONACTNH+6,X'80'                                                 
         CLI   CONSYSTH+5,0        NOTHING FOR SYSTEM?                          
         BNE   TF0500B                                                          
         LA    R3,RECACT                                                        
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         IC    RF,FAOVSYS          GET SYSTEM NUMBER                            
         DROP  R1                                                               
TF0500A  CLI   0(R3),X'01'         STILL VALID SYSTEM ENTRY?                    
         BNE   TF0500B             NO                                           
         CLM   RF,1,9(R3)          SYSTEM NUMBERS THE SAME?                     
         BE    *+12                YES                                          
         LA    R3,12(R3)           NEXT ENTRY                                   
         B     TF0500A                                                          
         MVC   CONSYSTH+8(8),1(R3) COPY TEXT                                    
         MVI   CONSYSTH+5,8        LENGTH                                       
         OI    CONSYSTH+6,X'80'    TRANSMIT                                     
*                                                                               
TF0500B  BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT FIELDS           
         L     RE,SYSPARMS                                                      
         L     RF,28(RE)           A(TIOB)                                      
         ST    RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         ZIC   R0,TIOBAID          NUMBER OF PFKEY PRESSED                      
         CH    R0,=H'12'                                                        
         BNH   *+8                                                              
         SH    R0,=H'12'                                                        
         STC   R0,PFKEY            SAVE ADJUSTED PFKEY VALUE                    
         DROP  RF                                                               
         GOTO1 CALLOV,DMCB,0,X'D9000A30'                                        
         CLI   DMCB+4,X'FF'        WAS A(GENCON) RETURNED FROM CALLOV?          
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB             PICK UP A(GENCON)                            
         GOTO1 (RF),DMCB,(R8)      GO THERE - PASS A(WORKING STORAGE)           
*                                                                               
XIT      XIT1                      THEN WE'RE THROUGH                           
         EJECT                                                                  
* INITIALIZE SYSTEM DEPENDENT VALUES                                            
*                                                                               
SYSINIT  NTR1                                                                   
         ST    RD,SYSRD                                                         
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
*                                                                               
SYS2     L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO                                                          
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,SYS2                                                          
*                                                                               
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R5,VCOUNT                                                        
*                                                                               
SYS4     ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,SYS4                                                          
*                                                                               
         LA    R1,STARTSAV                                                      
         ST    R1,ASTARTSV                                                      
*        MVI   SYSTEM,C'C'         CONTROL                                      
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 2000 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,GETAGY      ROUTINE TO GET USER NAME AND ADDRESS         
         LA    R1,RECACT           RECORD/ACTION DIRECTORY                      
         ST    R1,ARECACT                                                       
         MVI   NTWA,X'81'          SAVE/RESTORE FULL 6K                         
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   DON'T BOTHER IF OFF-LINE                     
         BZ    *+14                                                             
*                                  CLEAR TWAVPRNT BECAUSE CHANGE IN PFM         
         XC    TWAVPRNT,TWAVPRNT       SETS IT SO IT APPEARS OFFLINE            
         B     SYS5                                                             
         CLI   TWAOFFC,C'*'        IF NOT AT DDS                                
         BE    SYS5                                                             
         LA    R1,RECACT1          DON'T ALLOW ALL RECORD TYPES                 
         ST    R1,ARECACT1                                                      
         LA    R1,RECACT3          AND ALL REC/ACT COMBOS                       
         ST    R1,ARECACT3                                                      
         EJECT                                                                  
SYS5     L     R3,ARECACT          A(RECORD TABLE)                              
         CLI   CONSYSTH+5,0        TEST ANYTHING IN SYSTEM FIELD YET            
         BE    SYS10               NO - DEFAULT TO GENDIR/GENFIL                
*                                                                               
         MVI   WORK,1              LOOK UP RECORD ENTRIES IN RECACT             
         ZIC   R1,CONSYSTH+5                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+1(0),CONSYST   PUT DATA IN WORK                             
*                                                                               
SYS6     EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),0(R3)       TEST MATCH ON RECORD NAME                    
         BE    SYS8                YES                                          
         LA    R3,12(R3)           TRY NEXT ENTRY                               
         CLI   0(R3),X'FF'         TEST END OF TABLE                            
         BNE   SYS6                NO                                           
         B     SYS10               YES - LET GENCON HANDLE ERROR                
*                                                                               
SYS8     LA    R2,CTFTAB           A(SYSTEM RECORD TABLE)                       
SYS9     CLC   9(1,R3),0(R2)       TEST MATCH IN TABLE                          
         BE    *+20                YES - USE SYSTEM                             
         LA    R2,1(R2)            BUMP TABLE POINTER                           
         CLI   0(R2),X'FF'         TEST END OF TABLE                            
         BE    SYS10               YES - DEFAULT TO GENDIR/GENFIL               
         B     SYS9                NO - TRY AGAIN                               
*                                                                               
         MVC   LKEY,=H'25'         CTFILE/CTUSER SPECIFICS                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'28'                                                  
         MVC   SYSFIL,=C'CTFILE  '                                              
         MVC   SYSDIR,=C'CTFILE  '                                              
         MVI   USEIO,C'Y'                                                       
         MVI   ACTELOPT,C'N'                                                    
         B     SYS12                                                            
*                                                                               
SYS10    MVC   LKEY,=H'32'         GENDIR/GENFIL SPECIFICS                      
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SYSFIL,=C'GENFIL  '                                              
         MVC   SYSDIR,=C'GENDIR  '                                              
*                                                                               
SYS12    MVC   REQFILE,=C'CTREQ  '                                              
         MVC   LWORK,=Y(LENWORK)   SOFT WORK AREA LENGTH                        
         MVC   RCPROG(2),=C'CT'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D90F0500'    PRESET FOR SYSTEM CALLOVS               
         XC    GBLOCK,GBLOCK       CLEAR GETTXT ERROR AREA                      
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY                                
*                                                                               
VCOMMON  NTR1  BASE=SYSRB                                                       
         ST    RD,COMMRD                                                        
         L     R9,ASYSD                                                         
         L     R7,BASER7                                                        
         L     R8,ASPOOLD                                                       
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
*                                                                               
VBRANCH  B     VUSER                                                            
         B     VSFMERR                                                          
         B     VDISPHLP                                                         
         B     VCLRSCN                                                          
*                                                                               
VCOUNT   EQU   (*-VBRANCH)/4                                                    
*                                                                               
BASER7   DC    A(0)                                                             
         EJECT                                                                  
* USER AGENCY                                                                   
*                                                                               
VUSER    CLI   SVUSRNAM,C' '       DO ONE TIME ONLY                             
         BNH   VUSER10                                                          
         MVC   USERNAME,SVUSRNAM                                                
         MVC   USERADDR,SVUSRADR                                                
         B     VUSERX                                                           
*                                  AGENCY NAME & ADDRESS FROM ID REC.           
VUSER10  XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4           BUILD ID RECORD KEY                          
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),10(RA)  FROM TWA                                     
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,AIO1                    
         MVI   SVUSRNAM,C'*'                                                    
*                                                                               
         L     R4,AIO1             ORIGIN DETAILS                               
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'36'                                                     
         BAS   RE,FIRSTEL                                                       
         BNE   VUSERX                                                           
         DROP  R4                                                               
*                                                                               
         USING CTORGD,R6                                                        
         MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         MVC   SVUSRNAM,USERNAME                                                
         MVC   SVUSRADR,USERADDR                                                
         DROP  R6                                                               
*                                                                               
VUSERX   B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE CALLS GENCON'S ERREX ROUTINE AND ASKS FOR GETTXT CALLS           
*                                                                               
VSFMERR  OI    GENSTAT2,USGETTXT   FLAGS ERREX TO CALL GETTXT                   
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTINDX,GINDEX       MESSAGE INDEX                                
         MVC   GTMSGNO,GERROR      MESSAGE NUMBER                               
         MVC   GTMTYP,GMSGTYPE     MESSAGE TYPE                                 
         MVC   GTLTXT,GLTXT        LENGTH OF INSERTION TEXT                     
         MVC   GTATXT,GATXT        A(INSERTION TEXT)                            
*                                                                               
         CLC   GERROR,=H'60'       MESSAGE NUMBER <= 60 ?                       
         BH    *+8                 NO -- USE CONTROL SYSTEM MESSAGES            
         MVI   GTMSYS,X'FF'        USE GENERAL SYSTEM                           
         DROP  RF                                                               
*                                                                               
         L     RE,SYSPARMS                                                      
         MVI   0(RE),X'FF'         SET ERROR FLAG                               
         GOTO1 ERREX                                                            
         EJECT                                                                  
**********************************************************************          
* CLEARS SCREEN AND SETS TO NORMAL INTENSITY                                    
**********************************************************************          
VCLRSCN  DS    0H                                                               
         L     R5,0(R1)            STARTING POSITION AT RECORD TYPE             
         LA    RF,CONIFH           ENDING POSITION                              
         SR    RE,RE               CLEAR                                        
VCLRSCN1 IC    RE,0(R5)            RE = LENGTH OF FIELD                         
         SH    RE,=H'9'                                                         
         TM    1(R5),X'02'         EXTENDED FIELD HEADER?                       
         BZ    VCLRSCN2            NO                                           
         SH    RE,=H'8'            YES                                          
VCLRSCN2 LTR   RE,RE                                                            
         BM    XIT                                                              
         EX    RE,*+8              RE = LENGTH OF DATA                          
         B     *+10                                                             
         XC    8(0,R5),8(R5)       NULL OUT THE DATA                            
         MVI   5(R5),0             CLEAR THE INPUT LENGTH                       
         OI    6(R5),X'80'         TRANSMIT THE FIELDS                          
         NI    1(R5),X'FF'-X'0C'   SET TO NORMAL INTENSITY                      
         OI    1(R5),X'20'         PROTECT THE FIELDS                           
         IC    RE,0(R5)            RE = LENGTH OF FIELD                         
VCLRSCN5 BXLE  R5,RE,VCLRSCN1                                                   
         B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* USER HELP TABLE GENERATOR                                                     
**********************************************************************          
VDISPHLP DS    0H                                                               
         L     R3,0(R1)            POINT TO RECTABLE                            
         LA    R2,CONP0H           POINT TO 1ST PROTECTED FIELD                 
         SR    R4,R4               CLEAR COUNT                                  
*                                                                               
*************** COLUMN 1                                                        
*                                                                               
VDISP10  CLI   0(R3),X'FF'                                                      
         BE    XIT                                                              
         C     R4,=F'16'           RUN OUT OF ROOM YET?                         
         BE    VDISP20             GOTO COLUMN 2                                
         LA    R4,1(R4)            INCREMENT COUNT                              
         OI    6(R2),X'80'         TRANSMIT                                     
         MVC   8(8,R2),0(R3)       MOVE RECORD TYPE                             
         ZIC   R1,0(R2)            GET LENGTH                                   
         AR    R2,R1               GOTO THE INPUT FIELD                         
         ZIC   R1,0(R2)            GET LENGTH                                   
         AR    R2,R1               GOTO THE INPUT FIELD                         
         LA    R3,12(R3)           NEXT COMPONENT IN KEY                        
         B     VDISP10                                                          
*                                                                               
*************** COLUMNS 2 THRU 6                                                
*                                                                               
VDISP20  LA    R5,8                START FROM COLUMN 2                          
VDISP30  SR    R4,R4                                                            
         LA    R2,CONP0H           POINT TO 1ST PROTECTED FIELD                 
VDISP40  CLI   0(R3),X'FF'                                                      
         BE    XIT                                                              
         CH    R4,=H'16'           RUN OUT OF ROOM YET?                         
         BE    VDISP50                                                          
         LA    R4,1(R4)            INCREMENT COUNT                              
*                                                                               
         ZIC   R1,0(R2)            GET LENGTH                                   
         AR    R2,R1               GOTO COLUMN 2                                
         OI    6(R2),X'80'         TRANSMIT                                     
         LA    R2,0(R2,R5)         COLUMN IS R5 OFF R2                          
         MVC   0(8,R2),0(R3)       MOVE RECORD TYPE                             
         SR    R2,R5               RESET R2 TO BASE                             
         ZIC   R1,0(R2)            GET LENGTH                                   
         AR    R2,R1               GOTO NEXT COLUMN 1                           
         LA    R3,12(R3)           NEXT COMPONENT IN KEY                        
         B     VDISP40                                                          
VDISP50  LA    R5,13(R5)           FIELD OF 8 + 5 SPACES BETWEEN                
         CH    R5,=H'60'           EXCEED LENGTH?                               
         BH    XIT                                                              
         B     VDISP30                                                          
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 5                                                                
* CONSTANTS, TABLES, ETC.                                                       
*                                                                               
RELO     DS    A                                                                
*                                                                               
SYSVCON  DS    0F                                                               
         DC    V(DUMMY)                                                         
         DC    V(DECODE)                                                        
*                                                                               
NVTYPES  EQU   (*-SYSVCON)/4                                                    
         SPACE 3                                                                
CTFTAB   DS    0F                  LIST OF SYSTEM NUMBERS                       
*                                                                               
         DC    AL1(02)             SPOT                                         
         DC    AL1(03)             NET                                          
         DC    AL1(04)             PRINT                                        
         DC    AL1(05)             MPL                                          
         DC    AL1(06)             ACC                                          
         DC    AL1(07)             TALENT                                       
         DC    AL1(08)             REP                                          
         DC    AL1(09)             MBA                                          
         DC    AL1(10)             CONTROL                                      
         DC    AL1(12)             CPP                                          
         DC    AL1(14)             PER                                          
         DC    X'FF'                                                            
         EJECT                                                                  
* DIRECTORY OF RECORDS AND ACTIONS                                              
*                                                                               
RECACT   DS    0D                                                               
*                                                                               
*                                  X'01' ENTRIES ARE AVAILABLE SYSTEMS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
*                                                                               
RECACT1  DC    X'01',C'SPOT    ',AL1(02),X'0000'                                
         DC    X'01',C'NET     ',AL1(03),X'0000'                                
         DC    X'01',C'PRINT   ',AL1(04),X'0000'                                
         DC    X'01',C'MPL     ',AL1(05),X'0000'                                
         DC    X'01',C'ACC     ',AL1(06),X'0000'                                
         DC    X'01',C'TALENT  ',AL1(07),X'0000'                                
         DC    X'01',C'REP     ',AL1(08),X'0000'                                
         DC    X'01',C'MBA     ',AL1(09),X'0000'                                
         DC    X'01',C'CONTROL ',AL1(10),X'0000'                                
         DC    X'01',C'CPP     ',AL1(12),X'0000'                                
         DC    X'01',C'PER     ',AL1(14),X'0000'                                
*                                                                               
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
*                                                                               
RECACT2  DC    X'02',C'BUILD   ',AL1(07,02,00)                                  
         EJECT                                                                  
* DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                                
*                                                                               
*                                  X'03' ENTRIES ARE OK REC/ACT COMBOS          
*                                  CL1 SYSTEM NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 PHASE NUMBER FOR SCREEN                  
*                                  CL1 PHASE NUMBER FOR EDIT                    
*                                  CL1 PHASE NUMBER FOR SPECS                   
*                                  CL1 PHASE NUMBER FOR REPORT                  
*                                  CL1 WHEN OK BITS 80=SCREEN 40=NOW            
*                                      20=SOON 10=OV 08=DDS                     
*                                  CL2 CODE FOR REPORTS                         
*                                  CL2 CODE FOR EOD HANDLING                    
*                                                                               
RECACT3  DC    X'03',AL1(02,02),X'0002000080',C'    '  SPOT      MAINT          
         DC    X'03',AL1(03,02),X'0003000080',C'    '  NET       MAINT          
         DC    X'03',AL1(04,02),X'0004000080',C'    '  PRINT     MAINT          
         DC    X'03',AL1(05,02),X'0005000080',C'    '  MPL       MAINT          
         DC    X'03',AL1(06,02),X'0006000080',C'    '  ACC       MAINT          
         DC    X'03',AL1(07,02),X'0007000080',C'    '  TALENT    MAINT          
         DC    X'03',AL1(08,02),X'0008000080',C'    '  REP       MAINT          
         DC    X'03',AL1(09,02),X'0009000080',C'    '  MBA       MAINT          
         DC    X'03',AL1(10,02),X'000A000080',C'    '  CONTROL   MAINT          
         DC    X'03',AL1(12,02),X'000C000080',C'    '  CPP       MAINT          
         DC    X'03',AL1(14,02),X'000E000080',C'    '  PER       MAINT          
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE GEKEYWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE GEKEYFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046GEKEY00S  05/01/02'                                      
         END                                                                    
