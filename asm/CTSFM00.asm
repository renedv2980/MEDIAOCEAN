*          DATA SET CTSFM00    AT LEVEL 081 AS OF 10/19/18                      
*PHASE TA0A00A                                                                  
*INCLUDE KHDUMMY                                                                
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:       TA0A00 - CONTROL FILE MAINTENANCE CONTROLLER (SFM)    *         
*                                                                     *         
*  COMMENTS:    GENCON INTERFACE TO CONTROL SYSTEM                    *         
*                                                                     *         
*  CALLED FROM: MONITOR                                               *         
*                                                                     *         
*  CALLS TO:    GENCON                                                *         
*                                                                     *         
*  INPUTS:      SCREEN CTSFMFF (TA0AFF)                               *         
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
         TITLE 'TA0A00 - SFM CONTROLLER (SUPER FILE MAINTENANCE)'               
TA0A00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,**TA0A00,R7,RR=R2,CLEAR=YES                              
         ST    R2,RELO                                                          
         LR    R9,R1                                                            
         LR    R8,RC                                                            
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
         ST    RD,SYSRD                                                         
         ST    R7,BASER7                                                        
         L     RE,SYSPARMS                                                      
         L     RA,20(RE)           A(TWA)                                       
         USING CONHEADH-64,RA                                                   
*                                                                               
         BAS   RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT FIELDS           
         L     RE,SYSPARMS                                                      
         L     R2,0(RE)            A(SYSFACS)                                   
         L     RF,28(RE)           A(TIOB)                                      
         ST    RF,ATIOB                                                         
         USING TIOBD,RF                                                         
         ZIC   R0,TIOBAID          NUMBER OF PFKEY PRESSED                      
         CH    R0,=H'12'                                                        
         BNH   *+8                                                              
         SH    R0,=H'12'                                                        
         STC   R0,PFKEY            SAVE ADJUSTED PFKEY VALUE                    
         DROP  RF                                                               
         L     RF,4(R2)            A(CALLOV)                                    
         GOTO1 (RF),DMCB,0,X'D9000A30'                                          
         CLI   DMCB+4,X'FF'        WAS A(GENCON) RETURNED FROM CALLOV?          
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   TWASCR,X'F2'        IS HELP MAINTENANCE SCREEN LOADED?           
         BNE   *+18                                                             
*&&US*&& CLC   =C'HEL',CONREC      RECORD HELP/PANEL?                           
*&&UK*&& CLC   =C'PAN',CONREC                                                   
         BNE   GOGENCON                                                         
         B     CHACHA                                                           
*                                                                               
         CLC   =C'PHA',CONREC                                                   
         BNE   *+12                                                             
         OI    GENSTAT4,CONFDEL                                                 
         B     GOGENCON                                                         
*                                                                               
         CLI   TWASCR,X'DF'        IS ESCAPE MAINTENANCE SCREEN LOADED?         
         BNE   GOGENCON                                                         
         CLC   =C'ESC',CONREC      RECORD ESCAPE?                               
         BNE   GOGENCON                                                         
*                                                                               
CHACHA   CLC   =C'SEL',CONACT      ACTION SELECT?                               
         BNE   GOGENCON                                                         
         CLI   THISLSEL,C'C'       'C' ENTERED AS LIST SELECTION?               
         BNE   GOGENCON                                                         
         CLI   PFKEY,3             ERASE A LINE?                                
         BE    *+20                                                             
         CLI   PFKEY,4             INSERT A LINE?                               
         BE    *+12                                                             
         CLI   PFKEY,5             TOGGLE HIGHLIGHTING?                         
         BNE   GOGENCON                                                         
         MVC   CONACT(3),=C'CHA'   FORCE ACTION TO CHANGE                       
*                                                                               
GOGENCON L     RF,DMCB             PICK UP A(GENERAL CONTROLLER)                
         GOTO1 (RF),DMCB,(R8)      GO THERE - PASS A(WORKING STORAGE)           
*                                                                               
XIT      XIT1                      THEN WE'RE THROUGH                           
         EJECT                                                                  
* INITIALIZE SYSTEM DEPENDENT VALUES                                            
*                                                                               
SYSINIT  NTR1                                                                   
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
         MVI   SYSTEM,C'C'         CONTROL                                      
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 2000 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,GETAGY      ROUTINE TO GET USER NAME AND ADDRESS         
         LA    R1,RECACT           RECORD/ACTION DIRECTORY                      
         ST    R1,ARECACT                                                       
*                                                                               
* NO-OP  MVI   NTWA,X'81'          SAVE/RESTORE FULL 6K                         
         MVC   LSVTWA0,=AL2(6144)  SAVE/RESTORE 6K AT BOTTOM OF TWA0            
         MVI   NTWA,0              DON'T NEED ANY TEMPSTR PAGES                 
         OI    GENSTAT3,RESTXE00   SET TO RESTORE ENTIRE LIST SCREEN            
         OI    GENSTAT2,DISTHSPG   SET TO STAY ON SAME LIST PAGE ON             
*                                  RETURN FROM DETAIL SCREEN                    
         OC    TWAVPRNT,TWAVPRNT   DON'T BOTHER IF OFF-LINE                     
         BNZ   SYS4A                                                            
         CLI   TWAOFFC,C'*'        IF NOT AT DDS                                
         BE    SYS5                                                             
         LA    R1,RECACT1          DON'T ALLOW ALL RECORD TYPES                 
         ST    R1,ARECACT1                                                      
         LA    R1,RECACT3          AND ALL REC/ACT COMBOS                       
         ST    R1,ARECACT3                                                      
         EJECT                                                                  
***********************************************************************         
* SPECIAL CHECK FOR JB IN LUID.  THIS GIVES THEM ACCESS TO STATION REC          
***********************************************************************         
         L     RF,SYSPARMS         A(COMFACS) DIFFERENT IN CTRL                 
         L     RF,12(RF)           ACOMFACS HERE FOR CONTROL SYSTEM!            
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     RF,DMCB                                                          
         USING FACTSD,RF                                                        
         CLC   =C'JB',FALINE                                                    
         BNE   *+12                                                             
         LA    R1,RECACTJB                                                      
         ST    R1,ARECACT1                                                      
         LA    R1,JBRECACT                                                      
         ST    R1,ARECACT3                                                      
         DROP  RF                                                               
*                                                                               
         B     SYS5                                                             
SYS4A    OI    GENSTAT1,RDUPAPPL   HANDLE OWN READ FOR UPDATE OFFLINE           
         MVI   RDUPDATE,C'N'                                                    
         EJECT                                                                  
SYS5     L     R3,ARECACT          A(RECORD TABLE)                              
         CLI   CONRECH+5,0         TEST ANYTHING IN RECORD FIELD YET            
         BE    SYS10               NO - DEFAULT TO GENDIR/GENFIL                
*                                                                               
         MVI   WORK,1              LOOK UP RECORD ENTRIES IN RECACT             
         ZIC   R1,CONRECH+5                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+1(0),CONREC    PUT DATA IN WORK                             
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
SYS8     LA    R2,CTFTAB           A(CTFILE RECORD TABLE)                       
SYS9     CLC   9(1,R3),0(R2)       TEST MATCH IN TABLE                          
         BE    *+20                YES - USE CTFILE                             
         LA    R2,1(R2)            BUMP TABLE POINTER                           
         CLI   0(R2),X'FF'         TEST END OF TABLE                            
         BE    SYS10               YES - DEFAULT TO GENDIR/GENFIL               
         B     SYS9                NO - TRY AGAIN                               
*                                                                               
         MVC   LKEY,=H'25'         CTFILE SPECIFICS                             
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'28'                                                  
         MVC   SYSFIL,=C'CTFILE  '                                              
         MVC   SYSDIR,=C'CTFILE  '                                              
         MVI   USEIO,C'Y'                                                       
         B     SYS12                                                            
*                                                                               
SYS10    MVC   LKEY,=H'32'         GENDIR/GENFIL SPECIFICS                      
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SYSFIL,=C'GENFIL  '                                              
         MVC   SYSDIR,=C'GENDIR  '                                              
*                                                                               
SYS12    MVC   REQFILE,=C'CTREQ  '                                              
         MVC   LWORK,=AL4(LENWORK) SOFT WORK AREA LENGTH                        
         MVC   RCPROG(2),=C'CT'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D90A0A00'    PRESET FOR SYSTEM CALLOVS               
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
*                                                                               
VCOUNT   EQU   (*-VBRANCH)/4                                                    
*                                                                               
BASER7   DC    A(0)                                                             
         EJECT                                                                  
* USER AGENCY                                                                   
*                                                                               
VUSER    XC    GBLOCK,GBLOCK       CLEAR GETTXT ERROR AREA                      
         CLI   OFFLINE,C'Y'        ALWAYS DO THIS OFFLINE                       
         BE    VUSER10                                                          
         CLI   TWAFIRST,0          FIRST TIME?                                  
         BE    VUSER10             YES                                          
         MVC   USERNAME(66),SVUSRNAM  AGY NAME/ADDRESS FROM ID RECORD           
         B     VUSERX                                                           
*                                                                               
VUSER10  MVI   TWAFIRST,1          WE'VE BEEN THROUGH HERE                      
         XC    KEY,KEY                                                          
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
         GOTO1 ERREX                                                            
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 5                                                                
* CONSTANTS, TABLES, ETC.                                                       
*                                                                               
RELO     DS    A                                                                
*                                                                               
SYSVCON  DS    0F                                                               
         DC    V(DUMMY)                                                         
*                                                                               
NVTYPES  EQU   (*-SYSVCON)/4                                                    
         SPACE 3                                                                
CTFTAB   DS    0F                  LIST OF RECORD NUMBERS WHICH ARE             
*                                  KEPT ON CTFILE RATHER THAN                   
*                                  GENDIR/GENFIL                                
*                                                                               
         DC    AL1(04)             USER                                         
         DC    AL1(05)             RATE                                         
         DC    AL1(06)             PRINTER                                      
         DC    AL1(07)             TERMINAL                                     
         DC    AL1(11)             PHASE                                        
         DC    AL1(12)             LOAD                                         
         DC    AL1(15)             ESCAPE                                       
         DC    AL1(16)             DIRADDS                                      
         DC    AL1(17)             EDICT                                        
         DC    AL1(18)             ADDS STATION                                 
         DC    AL1(31)             USER PROFILE                                 
         DC    AL1(33)             ALPHA MARKET                                 
         DC    AL1(34)             FAX INFORMATION                              
         DC    AL1(45)             ZENITH CLIENT RECS                           
         DC    AL1(46)             ZENITH REP RECS                              
         DC    AL1(47)             OFFICE RECS                                  
         DC    AL1(49)             DFORMULA RECS                                
         DC    AL1(52)             DCONTROL RECS                                
         DC    AL1(54)             OFFLIST RECS                                 
         DC    AL1(55)             IP SUBNET                                    
         DC    X'FF'                                                            
         EJECT                                                                  
* DIRECTORY OF RECORDS AND ACTIONS                                              
*                                                                               
RECACT   DS    0D                                                               
*                                                                               
*                                  X'01' ENTRIES ARE AVAILABLE RECORDS          
*                                  CL8 EXPANDED RECORD NAME                     
*                                  CL1 RECORD NUMBER                            
*                                  CL1 PHASE NUMBER FOR DATA DICTIONARY         
*                                  CL1 PHASE NUMBER FOR HELP SCREEN             
*                                                                               
*&&US*&& DC    X'01',C'HELP    ',AL1(02),X'0000'                                
*&&UK*&& DC    X'01',C'PANEL   ',AL1(02),X'0000'                                
*&&US*&& DC    X'01',C'ENTRY   ',AL1(03),X'0000'                                
         DC    X'01',C'LENTRY  ',AL1(03),X'0000'                                
         DC    X'01',C'UENTRY  ',AL1(01),X'0000'                                
         DC    X'01',C'DSTATION',AL1(43),X'0000'                                
         DC    X'01',C'DAGYROUT',AL1(44),X'0000'                                
         DC    X'01',C'PHASE   ',AL1(11),X'0000'                                
         DC    X'01',C'PRINTER ',AL1(06),X'0000'                                
         DC    X'01',C'TERMINAL',AL1(07),X'0000'                                
         DC    X'01',C'SCRIPT  ',AL1(08),X'0000'                                
         DC    X'01',C'BROADCAS',AL1(09),X'0000'                                
         DC    X'01',C'SPANEL  ',AL1(10),X'0000'                                
         DC    X'01',C'MPANEL  ',AL1(14),X'0000'                                
         DC    X'01',C'LOAD    ',AL1(12),X'0000'                                
         DC    X'01',C'DTYPE   ',AL1(13),X'0000'                                
         DC    X'01',C'ESCAPE  ',AL1(15),X'0000'                                
         DC    X'01',C'DIRADDS ',AL1(16),X'0000'                                
         DC    X'01',C'EDICT   ',AL1(17),X'0000'                                
         DC    X'01',C'TVDATA  ',AL1(24),X'0000'                                
         DC    X'01',C'AORREC  ',AL1(30),X'0000'                                
         DC    X'01',C'PROFILE ',AL1(31),X'0000'                                
         DC    X'01',C'STAFF   ',AL1(23),X'0000'                                
         DC    X'01',C'PASSIGN ',AL1(26),X'0000'                                
         DC    X'01',C'EDITRANS',AL1(32),X'0000'                                
         DC    X'01',C'ALPHAMKT',AL1(33),X'0000'                                
         DC    X'01',C'SOON    ',AL1(36),X'0000'                                
         DC    X'01',C'STUDIO  ',AL1(51),X'0000'                                
         DC    X'01',C'MQDEF   ',AL1(42),X'0000'                                
         DC    X'01',C'DCONTROL',AL1(52),X'0000'                                
         DC    X'01',C'REPIDS  ',AL1(53),X'0000'                                
         DC    X'01',C'IPSUBNET',AL1(55),X'0000'                                
         DC    X'01',C'DEMOSTA ',AL1(41),X'0000'                                
         DC    X'01',C'DFORMULA',AL1(49),X'0000'                                
         DC    X'01',C'EDIFEATR',AL1(56),X'0000'                                
         DC    X'01',C'EDIVENDR',AL1(57),X'0000'                                
         DC    X'01',C'EDIPARTR',AL1(58),X'0000'                                
***      DC    X'01',C'EDIAGNCY',AL1(59),X'0000'                                
***      DC    X'01',C'EDISTATN',AL1(60),X'0000'                                
         DC    X'01',C'STASRC  ',AL1(61),X'0000'                                
         DC    X'01',C'SDRVALS ',AL1(62),X'0000'                                
         DC    X'01',C'DCAT    ',AL1(63),X'0000'                                
*&&DO                                                                           
*=========================================================                      
* THESE ARE FOR TRAINING ONLY                                                   
* NOTE: THESE RECORD TYPES ARE USED FOR TRAINING ONLY, BUT THEY                 
*       ARE STILL "REAL" RECORDS! DO NOT USE THESE RECORD NUMBERS               
*       OR PHASE NUMBERS FOR ANYTHING ELSE: THEY ARE RESERVED!                  
*                                                                               
         DC    X'01',C'TRAIN1  ',AL1(48),X'0000'                                
         DC    X'01',C'TRAIN2  ',AL1(35),X'0000'                                
         DC    X'01',C'TRAIN3  ',AL1(37),X'0000'                                
         DC    X'01',C'TRAIN4  ',AL1(38),X'0000'                                
         DC    X'01',C'TRAIN5  ',AL1(39),X'0000'                                
         DC    X'01',C'TRAIN6  ',AL1(40),X'0000'                                
*&&                                                                             
*=========================================================                      
*                                                                               
*=========================================================                      
* SPECIAL ACCESS FOR JB (SKUI 7/8/92)                                           
*                                                                               
RECACTJB DC    X'01',C'STATION ',AL1(18),X'0000'                                
*                                                                               
*=========================================================                      
*                                                                               
RECACT1  DC    X'01',C'USER    ',AL1(04),X'0000'                                
         DC    X'01',C'RATE    ',AL1(05),X'0000'                                
         DC    X'01',C'FAX     ',AL1(34),X'0000'                                
         DC    X'01',C'CLIENT  ',AL1(45),X'0000'                                
         DC    X'01',C'REP     ',AL1(46),X'0000'                                
         DC    X'01',C'OFFICE  ',AL1(47),X'0000'                                
         DC    X'01',C'OFFLIST ',AL1(54),X'0000'                                
         DC    X'01',C'ETIREP  ',AL1(50),X'0000'                                
*                                                                               
*                                  X'02' ENTRIES ARE AVAILABLE ACTIONS          
*                                  CL8 EXPANDED ACTION NAME                     
*                                  CL1 ACTION NUMBER                            
*                                  CL1 ACTION EQUATE                            
*                                  CL1 SPARE                                    
*                                                                               
         DC    X'02',C'ADD     ',AL1(01,01,00)                                  
         DC    X'02',C'CHANGE  ',AL1(02,01,00)                                  
         DC    X'02',C'DISPLAY ',AL1(03,01,00)                                  
         DC    X'02',C'DELETE  ',AL1(04,01,00)                                  
         DC    X'02',C'SELECT  ',AL1(05,01,00)                                  
         DC    X'02',C'RESTORE ',AL1(06,01,00)                                  
         DC    X'02',C'RENAME  ',AL1(07,02,00)                                  
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         DC    X'02',C'COPY    ',AL1(15,15,00)                                  
         DC    X'02',C'UPDATE  ',AL1(16,16,00)                                  
         DC    X'02',C'BUILD   ',AL1(14,14,00)                                  
         DC    X'02',C'PURGE   ',AL1(08,03,00)                                  
         DC    X'02',C'UNPURGE ',AL1(09,03,00)                                  
         EJECT                                                                  
* TABLE OF USED PHASES -- INDEXED BY PHASE NUMBER                               
* A NON-BLANK ENTRY INDICATES C'P' FOR PROGRAM, C'S' FOR SCREEN                 
*                                                                               
*                0123456789ABCDEF                                               
*                P PPPPPPPPPPPPPP      00-0F                                    
*                PPPPPP  PPPPPPPP      10-1F                                    
*                PPP P         J       20-2F                                    
*                PPPP   P P    P       30-3F                                    
*                                      40-4F                                    
*                   PP  PPPP           50-5F                                    
*                 PPPPPP               60-6F                                    
*                                      70-7F                                    
*                SSSSS         SS      80-8F                                    
*                SSSSSSSSSSS SS S      90-9F                                    
*                SSSSSSSSSSSSSSSS      A0-AF                                    
*                SSSSSSSSSSSSSSSS      B0-BF                                    
*                SSSSSSSSSSSSSSSS      C0-CF                                    
*                SSSSSSSSSSSSSSSS      D0-DF                                    
*                SSSSSSSSJSSSSSSS      E0-EF                                    
*                 SSSSSSSSSSSSSSS      F0-FF                                    
*                0123456789ABCDEF                                               
         SPACE 5                                                                
* DIRECTORY OF PHASES FOR SELECTED RECORD/ACTION                                
*                                                                               
*                                  X'03' ENTRIES ARE OK REC/ACT COMBOS          
*                                  CL1 RECORD NUMBER                            
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
         DC    X'03',AL1(02,01),X'F202000080',C'    '  HELP      MAINT          
         DC    X'03',AL1(02,10),X'E202000080',C'    '            LIST           
         DC    X'03',AL1(02,12),X'D202003278',C'HRHR'            REPORT         
         DC    X'03',AL1(03,01),X'F303000080',C'    '  (L)ENTRY  MAINT          
         DC    X'03',AL1(03,10),X'E303000080',C'    '            LIST           
         DC    X'03',AL1(01,01),X'F103000080',C'    '  UENTRY    MAINT          
         DC    X'03',AL1(01,10),X'E303000080',C'    '            LIST           
*&&US*&& DC    X'03',AL1(03,12),X'D303000328',C'BRBR'            REPORT         
*&&UK*&& DC    X'03',AL1(03,12),X'D303000328',C'DRDR'            REPORT         
         DC    X'03',AL1(03,12),X'D301000328',C'DRDR'            REPORT         
         DC    X'03',AL1(06,02),X'F606000080',C'    '  PRINTER   RENAME         
         DC    X'03',AL1(07,03),X'F707000080',C'    '  TERMINAL  U/PURG         
         DC    X'03',AL1(08,12),X'F808000828',C'SRSR'  SCRIPT    REPORT         
         DC    X'03',AL1(09,01),X'F909000080',C'    '  BROADCAST MAINT          
         DC    X'03',AL1(09,10),X'E909000080',C'    '            LIST           
         DC    X'03',AL1(24,01),X'A618000080',C'    '  TVDATA    MAINT          
         DC    X'03',AL1(24,10),X'A518000080',C'    '            LIST           
         DC    X'03',AL1(24,12),X'D918003978',C'BRBR'            REPORT         
         DC    X'03',AL1(09,12),X'D909003978',C'BRBR'            REPORT         
         DC    X'03',AL1(10,14),X'D61A000081',C'    '  SPANEL    BUILD          
         DC    X'03',AL1(10,15),X'EA0A000081',C'    '            COPY           
         DC    X'03',AL1(10,16),X'FA0A000081',C'    '            UPDATE         
         DC    X'03',AL1(14,14),X'D61A000081',C'    '  MPANEL    BUILD          
         DC    X'03',AL1(14,15),X'EA0A000081',C'    '            COPY           
         DC    X'03',AL1(14,16),X'FE0A000081',C'    '            UPDATE         
         DC    X'03',AL1(15,01),X'DF0F000080',C'    '  ESCAPE    MAINT          
         DC    X'03',AL1(15,10),X'CF0F000080',C'    '            LIST           
         DC    X'03',AL1(15,12),X'EF0F000F40',C'ERER'            REPORT         
         DC    X'03',AL1(11,01),X'FB0B000080',C'    '  PHASE     MAINT          
         DC    X'03',AL1(11,10),X'EB0B000080',C'    '            LIST           
         DC    X'03',AL1(11,12),X'DB0B000B18',C'PRHR'            REPORT         
         DC    X'03',AL1(11,15),X'CB0B000B81',C'    '            COPY           
         DC    X'03',AL1(12,01),X'FC0C000080',C'    '  LOAD      MAINT          
         DC    X'03',AL1(13,01),X'FD0D000080',C'    '  DTYPE     MAINT          
         DC    X'03',AL1(13,10),X'ED0D000080',C'    '            LIST           
         DC    X'03',AL1(13,12),X'DD0D000D78',C'DRBR'            REPORT         
         DC    X'03',AL1(16,01),X'DC10000080',C'    '  DIRADDS   MAINT          
         DC    X'03',AL1(16,10),X'EC10000080',C'    '            LIST           
         DC    X'03',AL1(17,01),X'D41B000080',C'    '  EDICT     MAINT          
         DC    X'03',AL1(17,10),X'D51B000080',C'    '            LIST           
         DC    X'03',AL1(17,12),X'D71B000038',C'EAEA'            REPORT         
         DC    X'03',AL1(30,01),X'D030000080',C'    '  AORREC    MAINT          
         DC    X'03',AL1(30,10),X'E030000080',C'    '            LIST           
         DC    X'03',AL1(31,03),X'BF31000080',C'    '  PROFILE   U/PURG         
         DC    X'03',AL1(23,01),X'A454000080',C'    '  STAFF     MAINT          
         DC    X'03',AL1(23,10),X'B454000080',C'    '            LIST           
         DC    X'03',AL1(23,12),X'C454005478',C'SLSL'            REPORT         
         DC    X'03',AL1(26,01),X'A757000080',C'    '  PASSIGN   MAINT          
         DC    X'03',AL1(26,10),X'B757000080',C'    '            LIST           
         DC    X'03',AL1(26,12),X'C957005738',C'PABR'            REPORT         
         DC    X'03',AL1(32,12),X'C30E000E18',C'EDBR'  EDITRANS  REPORT         
         DC    X'03',AL1(33,01),X'DE1E000080',C'    '  ALPHAMKT  MAINT          
         DC    X'03',AL1(33,10),X'EE1E000080',C'    '            LIST           
         DC    X'03',AL1(33,12),X'DE1E001E40',C'AMBR'            REPORT         
         DC    X'03',AL1(36,01),X'E111000080',C'    '  SOON      MAINT          
         DC    X'03',AL1(36,10),X'E611000080',C'    '            LIST           
         DC    X'03',AL1(36,12),X'E711001178',C'SRHR'            REPORT         
         DC    X'03',AL1(43,01),X'B813000080',C'    '  DSTATION  MAINT          
         DC    X'03',AL1(43,10),X'B913000080',C'    '            LIST           
         DC    X'03',AL1(43,16),X'C813000081',C'    '            UPDATE         
         DC    X'03',AL1(43,12),X'D813001378',C'DSDS'            REPORT         
         DC    X'03',AL1(44,01),X'BA14000080',C'    '  DAGYROUT  MAINT          
         DC    X'03',AL1(44,10),X'BB14000080',C'    '            LIST           
         DC    X'03',AL1(51,01),X'AF37000080',C'    '  STUDIO    MAINT          
         DC    X'03',AL1(51,10),X'9F37000080',C'    '            LIST           
         DC    X'03',AL1(42,01),X'B666000080',C'    '  MQDEF     MAINT          
         DC    X'03',AL1(42,10),X'C566000080',C'    '            LIST           
         DC    X'03',AL1(52,01),X'9234000080',C'    '  DCONTROL  MAINT          
         DC    X'03',AL1(52,10),X'91340000C0',C'    '            LIST           
         DC    X'03',AL1(53,01),X'9320000088',C'    '  REPIDS    MAINT          
         DC    X'03',AL1(53,10),X'9420000088',C'    '            LIST           
         DC    X'03',AL1(55,01),X'9521000080',C'    '  IPSUBNET  MAINT          
         DC    X'03',AL1(55,10),X'9621000080',C'    '            LIST           
         DC    X'03',AL1(55,12),X'9721002178',C'IPHR'            REPORT         
         DC    X'03',AL1(41,01),X'A565000080',C'    '  DEMOSTA   MAINT          
         DC    X'03',AL1(41,10),X'A665000080',C'    '            LIST           
         DC    X'03',AL1(49,01),X'B522000080',C'    '  DFORMULA  MAINT          
         DC    X'03',AL1(49,10),X'98220000C0',C'    '            LIST           
         DC    X'03',AL1(56,01),X'9958000088',C'    '  EDIFEATR  MAINT          
         DC    X'03',AL1(56,10),X'9A58000088',C'    '            LIST           
         DC    X'03',AL1(57,01),X'8059000088',C'    '  EDIVENDR  MAINT          
         DC    X'03',AL1(57,10),X'8159000088',C'    '            LIST           
         DC    X'03',AL1(57,12),X'8459005948',C'VRVR'            REPORT         
         DC    X'03',AL1(58,01),X'825A000088',C'    '  EDIPARTR  MAINT          
         DC    X'03',AL1(58,10),X'835A000088',C'    '            LIST           
***                                                                             
***      DC    X'03',AL1(59,01),X'  5B000088',C'    '  EDIAGNCY  MAINT          
***      DC    X'03',AL1(59,10),X'  5B000088',C'    '            LIST           
***      DC    X'03',AL1(60,01),X'  5C000088',C'    '  EDISTATN  MAINT          
***      DC    X'03',AL1(60,10),X'  5C000088',C'    '            LIST           
*                                                                               
         DC    X'03',AL1(61,01),X'A15D000088',C'    '  STASRC    MAINT          
         DC    X'03',AL1(61,10),X'A05D000088',C'    '            LIST           
         DC    X'03',AL1(62,01),X'9B5E000088',C'    '  SDRVALS   MAINT          
         DC    X'03',AL1(62,10),X'9E5E000088',C'    '            LIST           
         DC    X'03',AL1(63,01),X'8E25000088',C'    '  DCAT      MAINT          
         DC    X'03',AL1(63,10),X'8F25000088',C'    '            LIST           
*&&DO                                                                           
*                                                                               
* THESE ARE FOR TRAINING ONLY                                                   
*                                                                               
         DC    X'03',AL1(48,01),X'A353000080',C'    '  TRAIN1    MAINT          
         DC    X'03',AL1(48,10),X'B353000080',C'    '            LIST           
         DC    X'03',AL1(48,12),X'D153005378',C'WSHR'            REPORT         
         DC    X'03',AL1(35,01),X'CE3E000080',C'    '  TRAIN2    MAINT          
         DC    X'03',AL1(35,10),X'BE3E000080',C'    '            LIST           
         DC    X'03',AL1(35,12),X'AE3E003E78',C'RSHR'            REPORT         
         DC    X'03',AL1(37,01),X'A061000080',C'    '  TRAIN3    MAINT          
         DC    X'03',AL1(37,10),X'A161000080',C'    '            LIST           
         DC    X'03',AL1(37,12),X'A861006178',C'SSHR'            REPORT         
         DC    X'03',AL1(38,01),X'A262000080',C'    '  TRAIN4    MAINT          
         DC    X'03',AL1(38,10),X'B062000080',C'    '            LIST           
         DC    X'03',AL1(38,12),X'A962006278',C'JSHR'            REPORT         
         DC    X'03',AL1(39,01),X'B163000080',C'    '  TRAIN5    MAINT          
         DC    X'03',AL1(39,10),X'B263000080',C'    '            LIST           
         DC    X'03',AL1(39,12),X'AA63006378',C'WTHR'            REPORT         
         DC    X'03',AL1(40,01),X'C064000080',C'    '  TRAIN6    MAINT          
         DC    X'03',AL1(40,10),X'C164000080',C'    '            LIST           
         DC    X'03',AL1(40,12),X'C264006478',C'RTHR'            REPORT         
*&&                                                                             
*=========================================================                      
         DC    X'03',AL1(05,01),X'F505000080',C'    '  RATE      MAINT          
*=========================================================                      
*                                                                               
JBRECACT DC    X'03',AL1(18,01),X'C61C000080',C'    '  STATION   MAINT          
         DC    X'03',AL1(18,10),X'C71C000080',C'    '            LIST           
         DC    X'03',AL1(18,12),X'C71C001C40',C'STST'            REPORT         
*                                                                               
*=========================================================                      
RECACT3  DC    X'03',AL1(05,10),X'E5050000C0',C'    '            LIST           
         DC    X'03',AL1(04,01),X'F404000080',C'    '  USER      MAINT          
         DC    X'03',AL1(04,10),X'E404000080',C'    '            LIST           
         DC    X'03',AL1(34,01),X'CA19000080',C'    '  FAX       MAINT          
         DC    X'03',AL1(34,10),X'DA19000080',C'    '            LIST           
         DC    X'03',AL1(34,12),X'AB19001958',C'FXFX'            REPORT         
         DC    X'03',AL1(45,01),X'AC1D000080',C'    '  CLI       MAINT          
         DC    X'03',AL1(45,10),X'BC1D000080',C'    '            LIST           
         DC    X'03',AL1(45,12),X'BC1D001D78',C'ZZHR'            REPORT         
         DC    X'03',AL1(46,01),X'AD1F000080',C'    '  REP       MAINT          
         DC    X'03',AL1(46,10),X'BD1F000080',C'    '            LIST           
         DC    X'03',AL1(46,12),X'BD1F001F78',C'ZZHR'            REPORT         
         DC    X'03',AL1(47,01),X'CC15000080',C'    '  OFFICE    MAINT          
         DC    X'03',AL1(47,10),X'CD15000080',C'    '            LIST           
         DC    X'03',AL1(47,12),X'CD15001578',C'OFHR'            REPORT         
         DC    X'03',AL1(54,01),X'9C17000080',C'    '  OFFLIST   MAINT          
         DC    X'03',AL1(54,10),X'9D17000080',C'    '            LIST           
         DC    X'03',AL1(54,12),X'9D17001778',C'OLHR'            REPORT         
         DC    X'03',AL1(50,12),X'E82E002E38',C'ETHR'  ETIREP    REPORT         
************************************************************                    
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
         EJECT                                                                  
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'081CTSFM00   10/19/18'                                      
         END                                                                    
