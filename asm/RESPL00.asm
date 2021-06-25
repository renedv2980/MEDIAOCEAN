*          DATA SET RESPL00    AT LEVEL 112 AS OF 05/01/02                      
*PHASE T80800A,*                                                                
*INCLUDE REQTWA                                                                 
*INCLUDE KHDUMMY                                                                
         TITLE 'T80800 - RESPL00 - REPPAK SPOOL CONTROLLER'                     
*                                                                               
*********************************************************************           
*                                                                   *           
*- RESPL00 -- PHASE T80800 -- REPPAK SPOOL CONTROLLER               *           
*                                                                   *           
* ----------------------------------------------------------------- *           
*                                                                   *           
*  MOD LOG                                                          *           
*  -------                                                          *           
*  09/13/89  PJS  CHANGE DATAMGR CALLS TO KEYSAVE,KEY FORMAT        *           
*                                                                   *           
*  APR20/90 (MRR) --- UPDATE SPOOLD PACKET W/ COMFACS ADDR          *           
*                                                                   *           
*  DEC04/90 (MRR) --- ALLOW NOW FOR DDS TERMINAL AND 'SECRET' ID    *           
*                                                                   *           
*  MAR25/99 (BU ) --- REMOVE ALL ACCESS TO FUNCTIONS                *           
*                     THIS WILL PERMIT DDS TO SEE IF ANYONE IS      *           
*                     LEGITIMATELY USING THIS FACILITY PRIOR TO     *           
*                     TOTALLY DELETING IT                           *           
*  NOTE:  THIS CAN'T EVEN BE REASSEMBLED!  LIBRARY ENTRIES OR       *           
*         DATA NAMES OR INCLUDES HAVE CHANGED!!                     *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
T80800   CSECT                                                                  
         NMOD1 1402,**SPL***,R7,RR=R6                                           
*                                                                               
         ST    R6,RELO                                                          
         USING GEND,RC                                                          
         LR    R8,RC                                                            
         USING SPOOLD,R8                                                        
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
         LA    RC,SPOOLEND                                                      
         L     RA,4(R1)                                                         
         LR    R9,R1                                                            
         USING T808FFD,RA                                                       
         BAS   RE,INITIAL                                                       
         ST    R8,ASPOOLD                                                       
         MVI   ERROR,X'FF'                                                      
         ST    RB,BASERB                                                        
         ST    RD,BASERD                                                        
*                                                                               
         MVI   DDS,NO                                                           
         CLI   TWAOFFC,C'*'                                                     
         BNE   MAIN10                                                           
         MVI   DDS,YES                                                          
MAIN10   EQU   *                                                                
         EJECT                                                                  
*              CHECK SELECTED REPORT                                            
         SPACE 3                                                                
         LA    R2,CONIDH                                                        
         GOTO1 ANY                                                              
         LA    R2,CONREPH                                                       
         GOTO1 ANY                                                              
         LA    R4,REPLIST                                                       
         ZIC   R5,5(R2)                                                         
         BCTR  R5,0                                                             
         SPACE 2                                                                
REP2     EX    R5,REP4                                                          
         BE    REP6                                                             
         LA    R4,18(R4)                                                        
         CLI   0(R4),X'FF'                                                      
         BNE   REP2                                                             
         MVI   ERROR,INVREP                                                     
         B     ERREX                                                            
         SPACE 2                                                                
REP4     CLC   CONREP(0),0(R4)                                                  
         SPACE 2                                                                
REP6     MVC   CONREP(8),0(R4)     REPORT LOCATED - EXPAND NAME                 
         OI    6(R2),X'80'                                                      
         MVC   PHSCREEN(4),8(R4)                                                
         MVC   RCPROG(2),=C'RE'                                                 
         MVC   RCPROG+2(2),12(R4)                                               
         MVC   QCRDCODE,14(R4)                                                  
         MVC   ONOPT,16(R4)                                                     
         MVC   SOONOPT,17(R4)                                                   
         B     RUN                                                              
         SPACE 2                                                                
REPLIST  DS    0H                                                               
*        DC    C'HELP    ',X'F1000000',C'HE    '   REMOVED 8/28/86              
*        DC    C'MASTER  ',X'F2021222',C'MAOMYY'   REMOVED 3/13/87              
*        DC    C'TITLES  ',X'F3031323',C'TIOIYY'   REMOVED 3/13/87              
*        DC    C'PURE    ',X'F4041424',C'PUORYY'   REMOVED 8/28                 
*        DC    C'RANK    ',X'F5051525',C'RAORYY'   REMOVED 8/28                 
*        DC    C'FLEXI   ',X'F6061626',C'FLORYY'   REMOVED 8/28                 
*        DC    C'SEARCH  ',X'F7071727',C'SEORYY'   REMOVED 8/28                 
*        DC    C'HILITES ',X'F8081828',C'HIORYY'   REMOVED 8/28                 
*        DC    C'LAYOUT  ',X'F9091929',C'LAORYY'   REMOVED 8/28                 
*        DC    C'TREND   ',X'FA0A1A2A',C'TRORYY'   REMOVED 8/28                 
*        DC    C'DEMOLIST',X'00001C2C',C'DEORYY'   REMOVED 8/28                 
*        DC    C'PROGRAMS',X'FE0E1E2E',C'PROIYY'   REMOVED 8/28                 
*        DC    C'RESEARCH',X'D5758595',C'REORMY'   REMOVED 9/4                  
*        DC    C'OTRANS  ',X'D7778797',C'OTOTMN'                                
*        DC    C'OPROJECT',X'D8788898',C'OPOPMN'                                
*        DC    C'ODELETE ',X'D9798999',C'ODODMN'                                
         DC    X'FF'                                                            
         DS    0F                                                               
         EJECT                                                                  
*              SELECTED RUN CONTROLS                                            
         SPACE 3                                                                
RUN      EQU   *                                                                
         CLI   PHPRINT,X'90'       90 SERIES IS OVERNIGHT                       
         BL    RUNB                                                             
         CLI   DDS,NO                                                           
         BE    RUNA                                                             
         CLC   CONID(3),=C'666'                                                 
         BE    RUNB1                                                            
RUNA     EQU   *                                                                
         OC    TWAVPRNT,TWAVPRNT                                                
         BZ    RUNON                                                            
         SPACE 1                                                                
RUNB     EQU   *                   RANK IS ONLY ONLINE                          
         CLI   PHPRINT,X'25'                                                    
         BNE   *+12                                                             
RUNB1    EQU   *                                                                
         MVI   TWAWHEN,0                                                        
         B     LOAD                                                             
         CLI   TWARUNSW,1                                                       
         BE    LOAD                                                             
         OC    TWAVPRNT,TWAVPRNT                                                
         BNZ   LOAD                                                             
         CLC   CONWHNH(4),=C'WHEN' DO WE HAVE THE WHEN SCREEN                   
         BNE   LOAD                                                             
         LA    R2,CONWHENH                                                      
         MVI   TWAWHEN,0                                                        
         CLI   5(R2),0                                                          
         BE    RUNEND                                                           
         CLC   8(3,R2),=C'NOW'                                                  
         BE    RUNEND                                                           
         MVI   TWAWHEN,4                                                        
         CLC   8(2,R2),=C'ON'      OVERNIGHT REQUEST                            
         BE    RUN2                                                             
         MVI   TWAWHEN,2                                                        
         CLC   8(4,R2),=C'SOON'                                                 
         BE    LOAD                                                             
         SPACE 2                                                                
RUNINV   MVI   ERROR,INVALID                                                    
         B     ERREX                                                            
         SPACE 2                                                                
RUNON    MVI   TWAWHEN,4                                                        
         CLC   CONWHNH(4),=C'WHEN' DO WE HAVE THE WHEN SCREEN                   
         BNE   LOAD                                                             
         SPACE 2                                                                
RUN2     XC    TWAOUT,TWAOUT                                                    
         LA    R2,CONOUTH          OUTPUT TYPE                                  
         CLI   5(R2),0                                                          
         BE    RUN4                                                             
         GOTO1 ANY                                                              
         LA    R4,KEY              VALIDATE                                     
         XC    KEY,KEY                                                          
         USING CTOREC,R4                                                        
         MVI   CTOKTYP,C'O'                                                     
         MVC   CTOKID(6),WORK                                                   
         MVI   DMFILE,C'C'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(20),KEYSAVE                                                  
         BNE   RUNINV                                                           
         MVC   TWAOUT,CTOKID                                                    
         SPACE 2                                                                
RUN4     XC    TWADEST,TWADEST                                                  
         LA    R2,CONDESTH         DESTINATION                                  
         CLI   5(R2),0                                                          
         BE    RUN6                                                             
         GOTO1 ANY                                                              
         LA    R4,KEY              VALIDATE                                     
         XC    KEY,KEY                                                          
         USING CTIREC,R4                                                        
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID,WORK                                                      
         MVI   DMFILE,C'C'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   RUNINV                                                           
         MVI   ELCODE,X'02'                                                     
         LA    R6,IO                                                            
         MVC   DATADISP,=H'28'                                                  
         BAS   RE,GETEL                                                         
         MVC   DATADISP,=H'23'                                                  
         MVC   TWADEST,2(R6)       PICK UP ID NUM                               
         SPACE 2                                                                
RUN6     DS    0H                                                               
RUNEND   MVI   TWARUNSW,1                                                       
         MVI   DMFILE,C'D'                                                      
         EJECT                                                                  
*              LOAD CONTROL                                                     
         SPACE 3                                                                
LOAD     CLC   PHSCREEN,36(RA)     IS SCREEN LOADED ALREADY                     
         BE    LOAD2                                                            
         MVC   36(1,RA),PHSCREEN                                                
         CLI   PHSCREEN,0          DO WE NEED A SCREEN                          
         BE    LOAD2                                                            
         MVC   DMCB+4(3),=X'D90808'                                             
         MVC   DMCB+7(1),PHSCREEN                                               
         GOTO1 CALLOV,DMCB,CONTAGH,,0                                           
         BAS   RE,LOADCHEK                                                      
         CLI   PHSCREEN,X'F1'      FOR HELP                                     
         BNE   LOAD1                                                            
         OI    CONREPH+6,X'40'     REPOSITION CURSOR TO REPORT                  
         B     XIT                                                              
         SPACE 2                                                                
LOAD1    MVC   CONHEAD,SPACES                                                   
         MVC   CONHEAD(35),=C'NOW ENTER SPECIFICS FOR THIS REPORT'              
         OI    CONHEADH+6,X'80'                                                 
         B     XIT                                                              
         SPACE 2                                                                
LOAD2    CLI   PHEDIT,0            DO WE NEED AN EDIT                           
         BE    LOAD4                                                            
         MVC   DMCB+4(3),=X'D90808'                                             
         MVC   DMCB+7(1),PHEDIT                                                 
         L     R1,=V(DUMMY)                                                     
         A     R1,RELO                                                          
         ST    R1,DMCB                                                          
         GOTO1 CALLOV,DMCB,,,0     LOAD IN EDIT MODULE                          
         BAS   RE,LOADCHEK                                                      
         L     RF,DMCB             GO TO EDITING ROUTINE                        
         GOTO1 (RF),PARAS,(RC)                                                  
         CLI   ERROR,X'FF'                                                      
         BNE   ERREX                                                            
         CLI   TWAWHEN,0                                                        
         BNE   REQ                                                              
         SPACE 2                                                                
LOAD4    CLI   PHSPECS,0                                                        
         BE    XIT                                                              
         MVC   DMCB+4(3),=X'D90808'                                             
         MVC   DMCB+7(1),PHSPECS                                                
         L     R1,=V(DUMMY)                                                     
         A     R1,RELO                                                          
         ST    R1,DMCB                                                          
         GOTO1 CALLOV,DMCB,,,0     LOAD IN SPECS                                
         BAS   RE,LOADCHEK                                                      
         MVC   SPECS,DMCB                                                       
         L     R1,DMCB                                                          
         AH    R1,DMCB+10                                                       
         LA    R1,8(R1)                                                         
         SRL   R1,3                                                             
         SLL   R1,3                                                             
         ST    R1,DMCB             GET A(NEXT DOUBLE WORD)                      
         MVC   DMCB+4(3),=X'D90808'                                             
         MVC   DMCB+7(1),PHPRINT                                                
         GOTO1 CALLOV,DMCB,,,0     LOAD IN PRINT MODULE BEHIND SPECS            
         L     R3,0(R1)            SAVE ADDRESS OF PRINT MODULE                 
         LA    R2,CONIDH                                                        
         GOTO1 ANY                                                              
         CLI   WORK+2,C' '                                                      
         BNE   LOAD6                                                            
         MVI   WORK+2,C'*'                                                      
         CLI   WORK+1,C' '                                                      
         BNE   LOAD6                                                            
         MVI   WORK+1,C'*'                                                      
         SPACE 2                                                                
LOAD6    MVC   SPOOLID,WORK                                                     
         XC    SPOOLKEY,SPOOLKEY                                                
         MVC   SPOOLKEY+1(8),CONREP                                             
         MVC   SPOOLKEY+9(3),SPOOLID                                            
         MVC   SPOOLKEY+12(3),SPOOLID                                           
         MVI   SPOOLKEY+16,68      68 LINES TO A PAGE                           
         MVI   SPMODE,0            INITIALIZE SPOOL DSECT                       
         MVC   VPRINT,TWAVPRNT                                                  
         MVC   ABOX,TWAVBOX                                                     
         GOTO1 SPOOL,PARAS,(R8)                                                 
         MVC   SPOOLRPN,SPOOLKEY+19                                             
         MVC   BUFFALO,TWAVBUFF    XTRNS FOR OFF-LINE VERSIONS                  
         MVC   SORTER,TWAVSORT                                                  
         MVC   WORKER,TWAVWORK                                                  
         CLI   PHPRINT,X'23'       TITLES AND MASTER USE INVENTORY              
         BH    *+8                                                              
         MVI   DMFILE,C'I'                                                      
         CLI   PHPRINT,X'2E'                                                    
         BNE   *+8                                                              
         MVI   DMFILE,C'I'                                                      
         CLI   PHPRINT,X'90'                                                    
         BL    *+8                                                              
         MVI   DMFILE,C'I'         AND OVERNITES TOO                            
         OC    VPRINT,VPRINT                                                    
         BZ    LOAD8                                                            
         MVC   DMCB+12(4),ABOX                                                  
         MVI   DMCB+12,C'B'                                                     
         GOTO1 =V(REQTWA),DMCB,(3,(RA)),IO,VPRINT,RR=RB                         
         GOTO1 TWAPTCHR,DMCB,(RB),(R3)                                          
         SPACE 2                                                                
LOAD8    LR    RF,R3                                                            
         GOTO1 (RF),PARAS,(RC)     GO TO PRINT MODULE                           
         OC    VPRINT,VPRINT                                                    
         BNZ   XIT                                                              
         CLI   ERROR,X'FF'                                                      
         BNE   ERREX                                                            
         MVI   SPMODE,X'FF'                                                     
         GOTO1 SPOOL,PARAS,(R8)    WRAP UP PRINT CONTROL INTERVAL               
         MVC   CONHEAD(60),OKMESS                                               
         LA    R3,CONHEAD                                                       
         EDIT  (2,SPOOLPAG),(4,43(R3)),ALIGN=LEFT                               
         EDIT  (2,SPOOLLIN),(5,54(R3)),ALIGN=LEFT                               
         MVC   CONHEAD+28(3),SPOOLID                                            
         EDIT  (2,SPOOLRPN),(5,DMCB),ALIGN=LEFT                                 
         MVC   32(4,R3),DMCB                                                    
         MVC   CONID(3),SPOOLID                                                 
         MVC   CONID+3(5),=C','                                                 
         MVC   CONID+4(4),DMCB                                                  
         OI    CONIDH+6,X'80'                                                   
         GOTO1 SQUASHER,DMCB,CONHEAD,60                                         
         B     EXIT                                                             
         SPACE 2                                                                
OKMESS   DC    C'REPORT HAS BEEN SPOOLED. ID=XYZ,1234 PAGES=NNNN LIN'           
         DC    C'ES=NNNNN  '                                                    
         SPACE 2                                                                
LOADCHEK CLI   4(R1),X'FF'                                                      
         BNER  RE                                                               
         DC    H'0'                                                             
         SPACE 2                                                                
REQ      XC    IO(26),IO                                                        
         MVC   IO+26(80),SPACES                                                 
         LA    R4,IO+26                                                         
         MVC   0(2,R4),QCRDCODE    COMPLETE REQUEST                             
         MVC   2(2,R4),REP                                                      
         L     R4,ACOMFACS                                                      
         USING COMFACSD,R4                                                      
         GOTO1 CGETFACT,DMCB,0                                                  
         L     R4,DMCB                                                          
         USING FACTSD,R4                                                        
         EDIT  (4,FASIN),(6,IO+31),FILL=0                                       
         LA    R4,IO                                                            
         USING REQOFFC,R4                                                       
         MVC   REQOUT,TWAOUT                                                    
         MVC   REQDEST,TWADEST                                                  
         GOTO1 =V(REQTWA),DMCB,(RA),IO,(C'R',DATAMGR),RR=RB                     
         MVC   CONHEAD,SPACES                                                   
         MVC   CONHEAD(34),=C'REPORT WILL BE PROCESSED OVERNIGHT'               
         CLI   TWAWHEN,2                                                        
         BNE   *+10                                                             
         MVC   CONHEAD+25(10),=CL10'SOON'                                       
         B     EXIT                                                             
         EJECT                                                                  
*              INITIALIZATION OF DSECT                                          
         SPACE 3                                                                
INITIAL  NTR1                                                                   
         LA    RE,DUB              PRE-CLEAR EVERYTHING                         
         LA    RF,PAD                                                           
         AH    RF,=H'5511'                                                      
         SR    RF,RE                                                            
         XCEF                                                                   
         SPACE 2                                                                
         MVC   DUB-8(8),=C'**WORK**'         SEED DSECT                         
         MVC   DATAMGR-8(8),=C'**TRNS** '                                       
         MVC   KEY-8(8),=C'**KEYS**'                                            
         MVC   IO-8(8),=C'**I/O***'                                             
         MVC   PAD-8(8),=C'**PAD***'                                            
         MVC   BLOCK-8(8),=C'*BLOCK**'                                          
         SPACE 2                                                                
         LA    R2,IO                                                            
         ST    R2,AIOAREA                                                       
         MVC   TERMINAL,0(RA)                                                   
         LA    R2,64(RA)                                                        
         ST    R2,AERRAREA                                                      
         SPACE 2                                                                
         ST    RA,ATWA                                                          
         LM    R2,R4,8(R9)         A(SYSLIST) A(TIA) A(COMFACS)                 
         ST    R3,ATIA                                                          
         ST    R4,ACOMFACS                                                      
         MVC   DEMAND,CDEMAND-COMFACSD(R4)                                      
         MVC   DEMOUT,CDEMOUT-COMFACSD(R4)                                      
         MVC   DATAMGR(36),0(R2)   DATAMGR - GETDAY                             
         MVC   SCANNER(16),24(R4)  SCANNER UNSCAN HEXIN HEXOUT                  
         MVC   REP,14(RA)                                                       
         LA    R2,BOOKVAL          FIND ADDRESSES OF CORE RESIDENT              
         SR    R3,R3               MODULES WITH PHONY CALLOV READS.             
         LA    R4,19                                                            
         SPACE 2                                                                
INIT2    XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A00'                                           
         STC   R3,DMCB+7                                                        
         GOTO1 CALLOV,DMCB                                                      
         MVC   0(4,R2),DMCB                                                     
         LA    R2,4(R2)                                                         
         LA    R3,1(R3)                                                         
         BCT   R4,INIT2                                                         
         LA    R2,DEMOCON          NEW DEMO MODULE ADDRESSES                    
         LA    R3,DEMCRES                                                       
         SPACE 1                                                                
INIT3    XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A00'                                           
         MVC   DMCB+7(1),0(R3)                                                  
         GOTO1 CALLOV,DMCB                                                      
         MVC   0(4,R2),DMCB                                                     
         LA    R2,4(R2)                                                         
         LA    R3,1(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   INIT3                                                            
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,ANY                                                           
         LA    R5,VCOUNT                                                        
         SPACE 2                                                                
INIT4    ST    R2,0(R4)            A(COMMON)                                    
         STC   R3,0(R4)            ROUTINE NUMBER                               
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R5,INIT4                                                         
         SPACE 1                                                                
         ST    R2,DEMCON           INTERCEPT DEMCON                             
         STC   R3,DEMCON                                                        
         SPACE 2                                                                
         MVI   DMINBTS,X'C0'       PRESET VALUES                                
         MVI   DMOUTBTS,X'7D'                                                   
         MVC   DATADISP,=H'23'                                                  
         MVC   FILENAME,SPACES                                                  
         MVI   DMSOURCE,C'A'                                                    
         MVI   DMFILE,C'D'                                                      
         GOTO1 DATCON,DMCB,(5,0),(10,RCDATE)                                    
         MVI   PAVMEDIA,C'T'                                                    
         SPACE 2                                                                
         MVC   SPOOLDM,DATAMGR                                                  
         MVC   SPOOLBUF,ATIA                                                    
         XC    VPRINT,VPRINT                                                    
         MVC   RCDATCON,DATCON                                                  
         MVC   RCCOMFAC,ACOMFACS                                                
         B     XIT                                                              
         SPACE 1                                                                
DEMCRES  DC    X'E0D9FF'                                                        
         EJECT                                                                  
*              ROUTINES ENTERABLE FROM BASE OR OVERLAY                          
         SPACE 3                                                                
VCOMMON  NTR1  BASE=BASERB                                                      
         LA    R7,2048(RB)                                                      
         LA    R7,2048(R7)                                                      
         LA    R9,2048(RC)                                                      
         LA    R9,2048(R9)                                                      
         USING GEND+4096,R9                                                     
         SRL   RF,24                                                            
         B     VBRANCH(RF)                                                      
         SPACE 2                                                                
VBRANCH  B     VANY                                                             
         B     VHIGH                                                            
         B     VSEQ                                                             
         B     VREAD                                                            
         B     VGETREC                                                          
         B     VVALSRCE                                                         
         B     VVALBOOK                                                         
         B     VVALDEM                                                          
         B     VVALSTAT                                                         
         B     VVALDAY                                                          
         B     VVALTIME                                                         
         B     VVALLINE                                                         
         B     VVALDATE                                                         
         B     VVALNUM                                                          
         B     VWRITE                                                           
         B     VADDREC                                                          
         B     VPUTREC                                                          
         SPACE 2                                                                
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         SPACE 1                                                                
         B     VDEMCON                                                          
         SPACE 2                                                                
VANY     CLI   5(R2),0                                                          
         BNE   VANY2                                                            
         MVI   ERROR,MISSING                                                    
         B     ERREX                                                            
         SPACE 2                                                                
VANY2    MVC   WORK,SPACES                                                      
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   WORK(0),8(R2)                                                    
         SPACE 2                                                                
VHIGH    MVC   COMMAND,=C'DMRDHI'                                               
         MVI   DMCOM,C'H'                                                       
         MVC   KEYSAVE,KEY                                                      
         B     VDIR                                                             
         SPACE 2                                                                
VSEQ     MVC   COMMAND,=C'DMRSEQ'                                               
         MVI   DMCOM,C'S'                                                       
         B     VDIR                                                             
         SPACE 2                                                                
VREAD    MVC   COMMAND,=C'DMRDHI'                                               
         MVC   KEYSAVE,KEY                                                      
         MVI   DMCOM,C'R'                                                       
         B     VDIR                                                             
         SPACE 1                                                                
VWRITE   CLI   TWAWROPT,C'N'                                                    
         BE    XIT                                                              
         MVC   COMMAND,=C'DMWRT '                                               
         LA    R2,KEY                                                           
         B     VDIR2                                                            
         SPACE 2                                                                
VDIR     LA    R2,IO                                                            
         XC    IO(64),IO                                                        
VDIR2    MVC   FILENAME(6),=C'CTFILE'                                           
         CLI   DMFILE,C'C'                                                      
         BE    VALL                                                             
         MVC   FILENAME(6),=C'DEMFIL'                                           
         CLI   DMFILE,C'M'                                                      
         BNE   *+10                                                             
         MVC   FILENAME(6),=C'DEMDIR'                                           
         CLI   KEY,C'A'                                                         
         BE    *+10                                                             
         MVC   FILENAME(3),=C'PAV'                                              
         CLI   DMFILE,C'I'                                                      
         BNE   VALL                                                             
         MVC   FILENAME(6),=C'REPDIR'                                           
         SPACE 2                                                                
VALL     CLI   DMFILE,C'D'         FOR DEMFIL                                   
         BNE   *+16                                                             
         MVC   IO(20),KEY          PUT WHOLE KEY INTO I/O                       
         MVC   KEY(4),DAMAJOR      AND D/A OF MAJOR INTO KEY                    
*                                                                               
         MVC   KEYSAVE,KEY         SAVE INCOMING KEY                            
*                                                                               
         GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),FILENAME,                X        
               KEYSAVE,(R2),(TERMINAL,0)                                        
*******        KEY,(R2),(TERMINAL,0)    **OLD SOURCE**                          
         MVC   KEY,0(R2)                                                        
         CLI   DMFILE,C'M'                                                      
         BNE   *+10                                                             
         MVC   DAMAJOR,KEY+19                                                   
         CLI   DMCOM,C'R'                                                       
         BNE   DMCHECK                                                          
         CLI   DMFILE,C'M'                                                      
         BNE   VALL2                                                            
         CLC   KEY(18),KEYSAVE                                                  
         BE    *+8                                                              
         OI    DMCB+8,X'10'                                                     
         B     DMCHECK                                                          
         SPACE 2                                                                
VALL2    CLC   KEY(20),KEYSAVE                                                  
         BE    *+8                                                              
         OI    DMCB+8,X'10'                                                     
         B     DMCHECK                                                          
         SPACE 2                                                                
VADDREC  CLI   TWAWROPT,C'N'                                                    
         BE    XIT                                                              
         LA    R2,KEY                                                           
         MVC   COMMAND,=C'ADDREC'                                               
         B     VRECALL                                                          
         SPACE 1                                                                
VPUTREC  CLI   TWAWROPT,C'N'                                                    
         BE    XIT                                                              
         LA    R2,KEY+28                                                        
         MVC   COMMAND,=C'PUTREC'                                               
         B     VRECALL                                                          
         SPACE 1                                                                
VGETREC  LA    R2,KEY+28                                                        
         MVC   COMMAND,=C'GETREC'                                               
         SPACE 1                                                                
VRECALL  GOTO1 DATAMGR,DMCB,(DMINBTS,COMMAND),=C'REPFILE',(R2),IO,     X        
               (TERMINAL,DMWORK)                                                
         SPACE 2                                                                
DMCHECK  MVC   DUB(1),DMCB+8                                                    
         NC    DUB(1),DMOUTBTS                                                  
         BZ    XIT                                                              
         MVI   ERROR,0                                                          
         SPACE 2                                                                
ERREX    L     R4,AERRAREA                                                      
         GOTO1 GETMSG,PARAS,(ERROR,8(R4)),(8,DMCB),(TERMINAL,DATAMGR)           
         OC    TWAVPRNT,TWAVPRNT                                                
         BZ    EXIT                                                             
         GOTO1 =V(REQTWA),DMCB,(3,(RA)),IO,VPRINT,RR=RB                         
         SPACE 2                                                                
EXIT     OI    6(R2),X'40'         INSERT CURSOR                                
         L     R2,AERRAREA                                                      
         OI    6(R2),X'80'         TRANSMIT HEADER                              
         L     RD,BASERD                                                        
         SPACE 2                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*              COMMON VALIDATION ROUTINES                                       
         SPACE 3                                                                
VVALSRCE GOTO1 ANY                 SOURCE                                       
         MVI   SORN,C'S'                                                        
         MVC   DBCOMFCS,ACOMFACS   INITIALIZE DBLOCK                            
         LA    R1,IO                                                            
         ST    R1,DBAREC                                                        
         LA    R1,OPTAREA                                                       
         ST    R1,DBAOPT                                                        
         MVC   DBLOPT,=F'256'                                                   
         OI    6(R2),X'80'                                                      
         MVI   DBFUNCT,DBGETDEM                                                 
         MVC   RCPROG(2),=C'SP'                                                 
         MVI   DBSELMED,C'T'       US SPOT                                      
         MVC   DBFILE,=C'PAV'                                                   
         MVI   DBSELSRC,C'A'                                                    
         MVI   DMSOURCE,C'A'                                                    
         CLI   8(R2),C'A'                                                       
         BE    SRCE6                                                            
         MVI   DBSELSRC,C'N'                                                    
         MVI   DMSOURCE,C'N'                                                    
         CLI   8(R2),C'N'                                                       
         BE    SRCE4                                                            
         MVI   DBSELSRC,C'S'                                                    
         MVI   DMSOURCE,C'S'                                                    
         CLI   8(R2),C'S'                                                       
         BE    SRCE8                                                            
         MVI   ERROR,INVSRCE                                                    
         B     ERREX                                                            
         SPACE 1                                                                
SRCE4    MVC   8(3,R2),=C'NSI'                                                  
         B     XIT                                                              
         SPACE 1                                                                
SRCE6    MVC   8(3,R2),=C'ARB'                                                  
         B     XIT                                                              
         SPACE 1                                                                
SRCE8    MVC   8(3,R2),=C'SRC'                                                  
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATION OF BOOKS AND STATION                                  
         SPACE 3                                                                
VVALBOOK GOTO1 ANY                 BOOK                                         
         GOTO1 BOOKVAL,PARAS,(DMSOURCE,(R2)),(MAX,BOOK),(SORN,SCANNER)          
         CLI   4(R1),0                                                          
         BNE   EDIT4                                                            
         MVI   ERROR,INVBOOK                                                    
         B     ERREX                                                            
         SPACE 2                                                                
EDIT4    MVC   ACTUAL,4(R1)        PASS USER BACK NUMBER FOUND                  
         B     XIT                                                              
         SPACE 2                                                                
VVALDEM  GOTO1 ANY                 DEMO (S)                                     
         GOTO1 DEMVAL,PARAS,(R2),(MAX,DEMO),SCANNER,DEMOTAB                     
         CLI   4(R1),0                                                          
         BNE   EDIT4                                                            
         MVI   ERROR,INVDEMO                                                    
         B     ERREX                                                            
         SPACE 2                                                                
VVALSTAT GOTO1 ANY                                                              
         MVI   SORN,C'S'                                                        
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         LA    RE,IO                                                            
         ST    RE,DBAREC                                                        
         LA    RF,1000             CLEAR I/O AREA                               
         XCEF                                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBFUNCT,DBVLST      VALIDATE STATION                             
         MVI   DBSELSRC,C'A'       TRY ARB FIRST                                
         MVC   DBSELSTA,8(R2)                                                   
         MVI   DBSELSTA+4,C'T'                                                  
         CLI   5(R2),4                                                          
         BE    VALST4                                                           
         BH    VALST2                                                           
         MVI   DBSELSTA+3,C' '     3 CHARACTER CALL LETTERS                     
         B     VALST4                                                           
         SPACE 2                                                                
VALST2   LA    R1,12(R2)      S/B XXXX-X                                        
         CLI   5(R2),6                                                          
         BE    *+12                                                             
         MVI   DBSELSTA+3,C' ' OR XXX-X                                         
         LA    R1,11(R2)                                                        
         CLI   0(R1),C'-'                                                       
         BE    *+12                                                             
         MVI   ERROR,INVSTAT                                                    
         B     ERREX                                                            
         MVC   DBSELSTA+4(1),1(R1)                                              
         SPACE 2                                                                
VALST4   LA    R3,SRCTBL                                                        
VALST5   MVC   ACTSTAT,DBSELSTA    SAVE STATION                                 
         MVC   DBSELSRC,0(R3)                                                   
         GOTO1 DEMAND,PARAS,DBLOCK,0                                            
         CLI   DBERROR,0           TEST FOR ERROR                               
         BE    VALST6                                                           
         CLI   1(R3),X'FF'                                                      
         BE    VALST5K                                                          
         LA    R3,1(R3)                                                         
         B     VALST5                                                           
VALST5K  CLI   RDOKOPT,C'Y'        OPTION TO IGNORE STATION PROBLEMS            
         BE    XIT                                                              
         MVI   ERROR,0                                                          
         B     ERREX                                                            
         SPACE 2                                                                
VALST6   CLI   OPTION,C'N'         OPTION TO OUTPUT MARKET NAME                 
         BE    XIT                                                              
         CLI   DBERROR,0           ONLY GO FORWARD FOR NO ERRORS IN             
         BNE   XIT                 STATION VALIDATION                           
         BAS   RE,BUMP                                                          
         CLI   OPTION,C'Y'         IF OPTION NOT Y                              
         BE    *+8                                                              
         LA    R2,WORK             BUILD HEADER AND MKTNAME IN WORK             
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBSELRMK,DBACTRMK   USE ACTUAL RATING SERV MKT                   
         MVC   ACTMKT,DBACTRMK     AND SAVE IN STORAGE                          
         GOTO1 DEMAND,PARAS,DBLOCK,0                                            
         CLI   DBERROR,0           TEST FOR ERROR                               
         BNE   XIT                                                              
         SPACE 2                                                                
VALST8   LA    R6,IO               DIG OUT NAME ELEMENT                         
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING DMELEM,R6                                                        
         MVC   8(24,R2),SPACES                                                  
         ZIC   R1,DMLEN                                                         
         CH    R1,=H'28'                                                        
         BL    *+8                                                              
         LA    R1,28                                                            
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),DMMNAME                                                  
         OI    6(R2),X'80'                                                      
         B     XIT                                                              
         SPACE 2                                                                
SRCTBL   DC    C'ANS'              ARB, NSI, SRC                                
         DC    X'FF'                                                            
         SPACE 2                                                                
VVALDAY  GOTO1 ANY                 DAY                                          
         LA    R3,DAYTBL                                                        
         SR    R4,R4                                                            
         SPACE 2                                                                
EDIT6    STC   R4,ACTUAL                                                        
         CLC   0(3,R3),8(R2)                                                    
         BE    XIT                                                              
         LA    R3,3(R3)                                                         
         LA    R4,1(R4)                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   EDIT6                                                            
         MVI   ERROR,INVDAY                                                     
         B     ERREX                                                            
         SPACE 2                                                                
DAYTBL   DC    C'M-FMONTUEWEDTHUFRISATSUNM-SVAR'                                
         DC    X'FF'                                                            
         SPACE 2                                                                
VVALTIME GOTO1 ANY                                                              
         ZIC   R3,5(R2)                                                         
         GOTO1 TIMVAL,PARAS,((R3),8(R2)),WORK                                   
         CLI   0(R1),X'FF'                                                      
         BNE   EDIT8                                                            
         MVI   ERROR,INVTIME                                                    
         B     ERREX                                                            
         SPACE 2                                                                
EDIT8    LH    R1,WORK             CONVERT MILITARY TO QUARTER HOUR             
         CH    R1,=H'600'                                                       
         BNL   *+8                                                              
         LA    R1,2400(R1)                                                      
         SH    R1,=H'600'                                                       
         SR    R0,R0                                                            
         D     R0,=F'100'                                                       
         SLL   R1,2                HOURS IN R1                                  
         LR    R3,R1               1/4S IN R3                                   
         SRDA  R0,32                                                            
         D     R0,=F'15'                                                        
         AR    R3,R1                                                            
         STC   R3,ACTUAL                                                        
         B     XIT                                                              
         SPACE 2                                                                
VVALLINE GOTO1 ANY                                                              
         CLI   MAX,10                                                           
         BNH   *+8                                                              
         MVI   MAX,10                                                           
         GOTO1 SCANNER,PARAS,(R2),(MAX,BLOCK)                                   
         ZIC   R3,PARAS+4                                                       
         LTR   R3,R3                                                            
         BZ    INVALIN                                                          
         XC    LINELIST,LINELIST                                                
         STC   R3,ACTUAL                                                        
         LA    R4,BLOCK                                                         
         LA    R5,LINELIST                                                      
         SPACE 2                                                                
EDIT10   CLI   0(R4),3             LENGTH MUST BE 3                             
         BL    INVALIN                                                          
         CLI   0(R4),4                         OR 4                             
         BH    INVALIN                                                          
         MVC   DUB(3),=3X'F0'      FIRST THREE CHARACTERS                       
         MVZ   DUB(3),12(R4)                                                    
         CLC   DUB(3),=3X'F0'      MUST BE NUMERIC                              
         BNE   INVALIN                                                          
         PACK  DUB,12(3,R4)                                                     
         CVB   R1,DUB                                                           
         SR    R0,R0                                                            
         D     R0,=F'10'                                                        
         STC   R1,0(R5)            R1 NOW HAS 1/4 HOURS                         
         SLL   R0,4                R0 HAS DAY NUMBER                            
         STC   R0,1(R5)                                                         
         CLI   0(R4),3                                                          
         BE    EDIT16                                                           
         LA    R1,WKLIST           MATCH FOURTH CHARACTER V LIST                
         SPACE 2                                                                
EDIT12   CLI   0(R1),X'FF'                                                      
         BE    INVALIN                                                          
         CLC   0(1,R1),15(R4)                                                   
         BE    EDIT14                                                           
         LA    R1,2(R1)                                                         
         B     EDIT12                                                           
         SPACE 2                                                                
EDIT14   OC    1(1,R5),1(R1)                                                    
         SPACE 2                                                                
EDIT16   LA    R4,32(R4)                                                        
         LA    R5,2(R5)                                                         
         BCT   R3,EDIT10                                                        
         CLI   OPTION,C'Y'         OPTION TO GET PROGRAM NAME                   
         BNE   XIT                                                              
         XC    KEY,KEY                                                          
         LA    R4,KEY              BUILD KEY                                    
         USING PRKEY,R4                                                         
         MVI   PRCODE,C'P'                                                      
         MVC   PRMEDIA,PAVMEDIA                                                 
         MVC   PRSRC,DMSOURCE                                                   
         MVC   PRSTAT,ACTSTAT                                                   
         MVC   PRBOOK,BOOK+1                                                    
         MVI   DMFILE,C'M'                                                      
         GOTO1 READ                                                             
         MVI   DMFILE,C'D'                                                      
         MVC   PRSTIM(2),LINELIST                                               
         GOTO1 READ                                                             
         LA    R6,IO                                                            
         MVI   ELCODE,X'21'                                                     
         BAS   RE,BUMP                                                          
         MVC   8(16,R2),SPACES                                                  
         OI    6(R2),X'80'                                                      
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         USING PPNELEM,R6                                                       
         ZIC   R3,PPNELN                                                        
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     XIT                                                              
         MVC   8(0,R2),PPNNME                                                   
         SPACE 2                                                                
INVALIN  MVI   ERROR,INVALID                                                    
         B     ERREX                                                            
         SPACE 2                                                                
VVALDATE L     R3,0(R1)                                                         
         GOTO1 ANY                                                              
         GOTO1 DATVAL,DMCB,(0,8(R2)),(R3)                                       
         MVC   ACTUAL,DMCB+3                                                    
         CLI   ACTUAL,0                                                         
         BNE   XIT                                                              
         MVI   ERROR,INVDATE                                                    
         B     ERREX                                                            
         SPACE 2                                                                
VVALNUM  GOTO1 ANY                                                              
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         MVC   WORK(6),=6X'F0'                                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),8(R2)                                                    
         CLC   WORK(6),=6X'F0'                                                  
         BNE   VALNM2                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         LTR   R1,R1                                                            
         BZ    VALNM2                                                           
         CH    R1,=H'255'                                                       
         BH    VALNM2                                                           
         STC   R1,ACTUAL                                                        
         B     XIT                                                              
         SPACE 2                                                                
VALNM2   MVI   ERROR,NOTNUM                                                     
         B     ERREX                                                            
         EJECT                                                                  
*              HANDLE DEMCON TO DEMOCON                                         
         SPACE 3                                                                
VDEMCON  LA    RE,DBLOCK           ADJUST PARAMETERS                            
         ST    RE,8(R1)                                                         
         LA    RE,SPUSRNMS                                                      
         ST    RE,12(R1)                                                        
         OC    0(35,RE),0(RE)                                                   
         BZ    *+8                                                              
         MVI   8(R1),C'S'                                                       
         CLI   0(R1),1                                                          
         BNE   *+8                                                              
         MVI   0(R1),X'FF'         FF IN DEMOCON MEANS OLD SPOT NO              
         MVC   SAVDBFIL,DBFILE     SAVE DBLOCK VALUES                           
         MVC   SAVDBFIL,DBFILE     SAVE DBLOCK VALUES                           
         MVC   SAVDBMED,DBSELMED                                                
         MVC   DBCOMFCS,ACOMFACS                                                
         CLI   DBSELMED,C'C'       SKIP THIS FOR CANADA                         
         BE    VDEMCON6                                                         
         MVC   DBFILE,=C'TP '      FUDGE                                        
         MVI   DBSELMED,C'T'                                                    
         CLC   SAVDBFIL,=C'TP '    TEST FOR TIME PERIOD FILE                    
         BE    VDEMCON6            YES                                          
         CLC   SAVDBFIL,=C'PAV'    TEST FOR PAV FILE                            
         BE    VDEMCON6            YES                                          
         MVC   DBFILE,=C'NTI'      FORCE EVERYTHING ELSE TO NETWORK             
         MVI   DBSELMED,C'N'       DEMOCON USES PAV/NETW MODIFIER TAB           
         SPACE 1                                                                
VDEMCON6 L     R2,0(R1)            DEMOCON WILL DO THE WORK                     
         MVC   SAVDEMO,0(R2)                                                    
         CLI   1(R2),C'T'          DOESNT LIKE T                                
         BNE   *+8                                                              
         MVI   1(R2),C'I'                                                       
         GOTO1 DEMOCON                                                          
         MVC   0(3,R2),SAVDEMO                                                  
         MVC   DBFILE,SAVDBFIL     RESTORE DBLOCK VALUES                        
         MVC   DBSELMED,SAVDBMED                                                
         B     XIT                                                              
         DROP  R9                                                               
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
         GETEL (R6),DATADISP,ELCODE                                             
         SPACE 2                                                                
RELO     DS    A                                                                
         SPACE 1                                                                
WKLIST   DC    C'1',AL1(02)                                                     
         DC    C'2',AL1(04)                                                     
         DC    C'3',AL1(06)                                                     
         DC    C'4',AL1(08)                                                     
         DC    C'5',AL1(10)                                                     
         DC    C'6',AL1(12)                                                     
         DC    C'7',AL1(14)                                                     
         DC    C'0',AL1(01)                                                     
         DC    C'A',AL1(03)                                                     
         DC    C'B',AL1(05)                                                     
         DC    C'C',AL1(07)                                                     
         DC    C'D',AL1(09)                                                     
         DC    C'E',AL1(11)                                                     
         DC    C'F',AL1(13)                                                     
         DC    C'G',AL1(15)                                                     
         DC    X'FF'                                                            
*                                                                               
DDS      DS    CL1                                                              
YES      EQU   1                                                                
NO       EQU   0                                                                
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE RESPLWORKD                                                     
         ORG   PAD+4992                                                         
SPUSRNMS DS    CL35                                                             
SAVDBFIL DS    CL3                                                              
SAVDBMED DS    CL1                                                              
SAVDBSRC DS    CL1                                                              
SAVDBSTA DS    CL5                                                              
SAVDBFNC DS    CL1                                                              
SAVDEMO  DS    CL3                                                              
         DS    CL207                                                            
       ++INCLUDE DEDBLOCK                                                       
OPTAREA  DS    CL256                                                            
         EJECT                                                                  
       ++INCLUDE RESPLFFD                                                       
         ORG   CONHEADH-64                                                      
TWATASK  DS    C                                                                
TWAOFFC  DS    C                                                                
TWATRM   DS    H                                                                
TWALEN   DS    H                                                                
TWAACCS  DS    CL4                                                              
TWAORIG  DS    XL2                                                              
         DS    CL2                                                              
TWAAGY   DS    CL2                                                              
TWAMODE  DS    XL1                 0=ON-LINE 1=OFF-LINE                         
TWADEST  DS    XL2                                                              
TWAOUT   DS    CL6                                                              
TWAWHEN  DS    CL1                 0=NOW 2=SOON 4=OVERNIGHT                     
TWARUNSW DS    CL1                                                              
         DS    CL5                                                              
TWAVPRNT DS    V                                                                
TWASCR   DS    XL1                                                              
TWAWROPT DS    CL1                                                              
         DS    CL2                                                              
TWAVBUFF DS    V                                                                
TWAVSORT DS    V                                                                
TWAPTCHR DS    V                                                                
TWAVWORK DS    V                                                                
TWAVBOX  DS    V                                                                
         PRINT OFF                                                              
       ++INCLUDE DMDEMFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE DMREQHDR                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'112RESPL00   05/01/02'                                      
         END                                                                    
