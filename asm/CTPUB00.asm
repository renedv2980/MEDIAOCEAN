*          DATA SET CTPUB00    AT LEVEL 003 AS OF 05/01/02                      
*PHASE TA1500A                                                                  
*INCLUDE KHDUMMY                                                                
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:       TA1500 - CONTROL PUB MAINTENANCE CONTROLLER           *         
*                                                                     *         
*  COMMENTS:    GENCON INTERFACE TO CONTROL SYSTEM                    *         
*                                                                     *         
*  CALLED FROM: MONITOR                                               *         
*                                                                     *         
*  CALLS TO:    GENCON                                                *         
*                                                                     *         
*  INPUTS:      SCREEN CTPUBFF (TA15FF)                               *         
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
         TITLE 'TA1500 - SFM CONTROLLER (SUPER FILE MAINTENANCE)'               
TA1500   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,**TA1500,R7,RR=R2,CLEAR=YES                              
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
SYS8     B     SYS10                                                            
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
         DC    X'01',C'PUB     ',AL1(01),X'0000'                                
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
         DC    X'02',C'PURGE   ',AL1(08,03,00)                                  
         DC    X'02',C'UNPURGE ',AL1(09,03,00)                                  
         EJECT                                                                  
         SPACE 2                                                                
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
         DC    X'03',AL1(01,01),X'F101000080',C'    '  PUB       MAINT          
         DC    X'03',AL1(01,10),X'E101000080',C'    '            LIST           
         DC    X'03',AL1(01,12),X'D101003278',C'PBPB'            REPORT         
************************************************************                    
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTPUBWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE CTPUBFFD                                                       
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
**PAN#1  DC    CL21'003CTPUB00   05/01/02'                                      
         END                                                                    
