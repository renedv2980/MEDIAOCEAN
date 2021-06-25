*          DATA SET CTTRN00    AT LEVEL 051 AS OF 05/01/02                      
*PHASE TA1200A                                                                  
*INCLUDE KHDUMMY                                                                
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:       TA1200 - TRAINING CONTROLLER                          *         
*                                                                     *         
*  COMMENTS:    GENCON INTERFACE TO CONTROL SYSTEM                    *         
*                                                                     *         
*  CALLED FROM: MONITOR                                               *         
*                                                                     *         
*  CALLS TO:    GENCON                                                *         
*                                                                     *         
*  INPUTS:      SCREEN CTTRNFF (TA12FF)                               *         
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
         TITLE 'TA1200 - TRAINING CONTROLLER'                                   
TA1200   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,**TA1200,R7,RR=R2,CLEAR=YES                              
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
         L     RF,DMCB             PICK UP A(GENERAL CONTROLLER)                
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
         MVC   LSVTWA0,=AL2(6144)  SAVE/RESTORE 6K AT BOTTOM OF TWA0            
         MVI   NTWA,0              DON'T NEED ANY TEMPSTR PAGES                 
         OI    GENSTAT3,RESTXE00   SET TO RESTORE ENTIRE LIST SCREEN            
         OI    GENSTAT2,DISTHSPG   SET TO STAY ON SAME LIST PAGE ON             
*                                  RETURN FROM DETAIL SCREEN                    
         OI    GENSTAT1,RDUPAPPL   HANDLE OWN READ FOR UPDATE OFFLINE           
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
         MVC   LKEY,=H'25'         CTFILE/CTUSER SPECIFICS                      
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
         MVC   SYSPHASE,=X'D90A1200'    PRESET FOR SYSTEM CALLOVS               
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
         B     VTRNERR                                                          
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
VTRNERR  OI    GENSTAT2,USGETTXT   FLAGS ERREX TO CALL GETTXT                   
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
*                                  KEPT ON CTFILE/CTUSER RATHER THAN            
*                                  GENDIR/GENFIL                                
*                                                                               
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
         DC    X'01',C'AWSTAFF ',AL1(24),X'0000'                                
         DC    X'01',C'DBSTAFF ',AL1(35),X'0000'                                
         DC    X'01',C'DCSTAFF ',AL1(37),X'0000'                                
         DC    X'01',C'DRSTAFF ',AL1(38),X'0000'                                
         DC    X'01',C'SCSTAFF ',AL1(39),X'0000'                                
         DC    X'01',C'RTEAM   ',AL1(40),X'0000'                                
         DC    X'01',C'STEAM   ',AL1(41),X'0000'                                
         DC    X'01',C'JTEAM   ',AL1(42),X'0000'                                
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
         DC    X'02',C'LIST    ',AL1(10,10,00)                                  
         DC    X'02',C'REPORT  ',AL1(12,12,00)                                  
         EJECT                                                                  
* TABLE OF USED PHASES -- INDEXED BY PHASE NUMBER                               
* A NON-BLANK ENTRY INDICATES C'P' FOR PROGRAM, C'S' FOR SCREEN                 
*                                                                               
*                0123456789ABCDEF                                               
*                P PPPPPPPPPPPPPP      00-0F                                    
*                PPPPPP   PPPPPPP      10-1F                                    
*                              J       20-2F                                    
*                PPP    P P    P       30-3F                                    
*                                      40-4F                                    
*                   PP  P              50-5F                                    
*                 PPPPPP               60-6F                                    
*                                      70-7F                                    
*                                      80-8F                                    
*                               S      90-9F                                    
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
         DC    X'03',AL1(24,01),X'A353000080',C'    '  AWSTAFF   MAINT          
         DC    X'03',AL1(24,10),X'B353000080',C'    '            LIST           
         DC    X'03',AL1(24,12),X'D153005378',C'WSHR'            REPORT         
         DC    X'03',AL1(35,01),X'CE3E000080',C'    '  DBSTAFF   MAINT          
         DC    X'03',AL1(35,10),X'BE3E000080',C'    '            LIST           
         DC    X'03',AL1(35,12),X'AE3E003E78',C'RSHR'            REPORT         
         DC    X'03',AL1(37,01),X'A061000080',C'    '  DCSTAFF   MAINT          
         DC    X'03',AL1(37,10),X'A161000080',C'    '            LIST           
         DC    X'03',AL1(37,12),X'A861006178',C'SSHR'            REPORT         
         DC    X'03',AL1(38,01),X'A262000080',C'    '  DRSTAFF   MAINT          
         DC    X'03',AL1(38,10),X'B062000080',C'    '            LIST           
         DC    X'03',AL1(38,12),X'A962006278',C'JSHR'            REPORT         
         DC    X'03',AL1(39,01),X'B163000080',C'    '  SCSTAFF   MAINT          
         DC    X'03',AL1(39,10),X'B263000080',C'    '            LIST           
         DC    X'03',AL1(39,12),X'AA63006378',C'WTHR'            REPORT         
         DC    X'03',AL1(40,01),X'C064000080',C'    '  RTEAM     MAINT          
         DC    X'03',AL1(40,10),X'C164000080',C'    '            LIST           
         DC    X'03',AL1(40,12),X'C264006478',C'RTHR'            REPORT         
         DC    X'03',AL1(41,01),X'A565000080',C'    '  STEAM     MAINT          
         DC    X'03',AL1(41,10),X'A665000080',C'    '            LIST           
         DC    X'03',AL1(41,12),X'B565006578',C'STHR'            REPORT         
         DC    X'03',AL1(42,01),X'B666000080',C'    '  JTEAM     MAINT          
         DC    X'03',AL1(42,10),X'C566000080',C'    '            LIST           
         DC    X'03',AL1(42,12),X'C866006678',C'JTHR'            REPORT         
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTTRNWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE CTTRNFFD                                                       
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
**PAN#1  DC    CL21'051CTTRN00   05/01/02'                                      
         END                                                                    
