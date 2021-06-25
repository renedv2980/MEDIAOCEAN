*          DATA SET PPADS00    AT LEVEL 013 AS OF 05/01/02                      
*PHASE T40B00A                                                                  
*INCLUDE KHDUMMY                                                                
*                                                                               
*        TITLE 'T40B00 - ADS CONTROLLER - SRDS ADS MAINTENANCE'                 
         TITLE 'T40B00 - ADS CONTROLLER - SRDS ADS MAINTENANCE'                 
***********************************************************************         
*                                                                     *         
*  TITLE:       T40B00 - SRDS PUB MAINTENANCE CONTROLLER              *         
*                                                                     *         
*  COMMENTS:    GENCON INTERFACE TO CONTROL SYSTEM                    *         
*                                                                     *         
*  CALLED FROM: MONITOR                                               *         
*                                                                     *         
*  CALLS TO:    GENCON                                                *         
*                                                                     *         
*  INPUTS:      SCREEN PPADSFF (T40BFF)                               *         
*                                                                     *         
*  LOCALS:      REGISTER USAGE                                        *         
*               R0 - WORK                                             *         
*               R1 - WORK                                             *         
*               R2 - WORK                                             *         
*               R3 - WORK                                             *         
*               R4 - WORK                                             *         
*               R5 - WORK                                             *         
*               R6 - GETEL REGISTER                                   *         
*               R7 - SPARE                                            *         
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
*                                                                               
         TITLE 'T40B00 - ADS CONTROLLER - SRDS ADS MAINT - INIT'                
***********************************************************************         
*                                                                     *         
*        PROGRAM HANDLES DISPLAYING OF SRDS DATA                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
T40B00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENWORK,**T40B00,R7,RR=R2,CLEAR=YES                              
*                                                                               
         LR    R9,R1               SAVE SYSTEM PARAMETERS POINTER               
*                                                                               
         LR    R8,RC               ESTABLISH DDSPOOL WORKING STORAGE            
         USING SPOOLD,R8                                                        
*                                                                               
         LA    RC,SPOOLEND         ESTABLISH GENCON WORKING STORAGE             
         USING GEND,RC                                                          
*                                                                               
         ST    R9,SYSPARMS         SAVE SYSTEM PARAMETERS POINTER               
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(131),SPACES                                             
*                                                                               
         LA    R9,IO                                                            
         AHI   R9,LENIOAS          3 4000 BYTE I/O AREAS PLUS LABELS            
         USING SYSD,R9             ESTABLLISH PROGRAM WORKING STORAGE           
*                                                                               
         ST    R2,RELO00           SAVE RELOCATION FACTOR                       
*                                                                               
         ST    RB,SYSRB            SYSTEM BASE REGS                             
         ST    RD,SYSRD                                                         
*                                                                               
         L     RE,SYSPARMS         ESTABLISH TWA                                
         L     RA,4(RE)            A(TWA)                                       
         USING CONHEADH-64,RA                                                   
*                                                                               
         TITLE 'T40B00 - ADS CONTROLLER - SRDS ADS MAINT - INITSYS'             
***********************************************************************         
*                                                                     *         
*        SET SYSTEM SPECIFIC VALUES                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INITSYS  DS    0H                                                               
*                                                                               
         BRAS  RE,SYSINIT          INITIALIZE SYSTEM DEPENDENT FIELDS           
*                                                                               
         L     RE,SYSPARMS                                                      
         L     RF,0(RE)            A(TIOB)                                      
         ST    RF,ATIOB                                                         
         USING TIOBD,RF            ESTABLISH TIOB                               
*                                                                               
         ZIC   R0,TIOBAID          NUMBER OF PFKEY PRESSED                      
         CH    R0,=H'12'                                                        
         JNH   *+8                                                              
         SH    R0,=H'12'                                                        
         STC   R0,PFKEY            SAVE ADJUSTED PFKEY VALUE                    
*                                                                               
INITSYSX DS    0H                                                               
*                                                                               
         DROP  RF                                                               
*                                                                               
         TITLE 'T40B00 - ADS CONTROLLER - SRDS ADS MAINT - DATAFLD'             
***********************************************************************         
*                                                                     *         
*        ANALYZE DATA FIELD                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DATAFLD  DS    0H                                                               
*                                                                               
         CLI   TWASCR,X'FF'        SKIP IF ONLY HEADER DISPLAYED                
         BE    DATAFLDX                                                         
*                                                                               
         LA    R2,LSTDATAH         POINT TO DATA FIELD                          
         USING FLDHDRD,R2                                                       
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLDILEN        GET INPUT LENGTH                             
         JZ    DATAFLDX            IGNORE IF NOT ENTERED                        
*                                                                               
         LHI   R3,SUBRECTB-T40B00  DISPLACEMENT OF SUB-RECORD TABLE             
         AR    R3,RB               RELOCATE ADDRESS                             
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                LOCATE OURSELVES                             
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   WORK(0),FLDDATA     MOVE DATA TO WORK AREA                       
*                                                                               
         OC    WORK,SPACES         MAKE UPPERCASE                               
*                                                                               
         BASR  RE,0                LOCATE OURSELVES                             
*                                                                               
         CLI   0(R3),X'FF'         SKIP IF END OF TABLE                         
         JE    DATAFLDX                                                         
         EX    RF,16(RE)                                                        
         J     *+10                                                             
         CLC   WORK(0),0(R3)       FIND ENTRY IN TABLE                          
         JE    *+12                                                             
         LA    R3,16(R3)           NEXT ENTRY IN TABLE                          
         J     *-30                                                             
*                                                                               
         CLI   PFKEY,11            IF NEXT PFKEY                                
         BNE   DATAFLD1                                                         
*                                                                               
         LA    R3,16(R3)           BUMP TO NEXT ENTRY IN TABLE                  
*                                                                               
         CLI   0(R3),X'FF'         AT END OF TABLE                              
         BNE   *+10                                                             
         LHI   R3,SUBRECTB-T40B00     REVERT TO FIRST IN TABLE                  
         AR    R3,RB                                                            
*                                                                               
         MVC   SRDDATA,0(R3)       SET NEW DATA FIELD                           
         MVC   FLDDATA(8),0(R3)    SET NEW DATA FIELD                           
         MVI   FLDILEN,8           SET INPUT LENGTH                             
         OI    FLDOIND,FOUTTRN     FORCE TRANSMISSION                           
*                                                                               
DATAFLD1 DS    0H                                                               
*                                                                               
         ICM   R1,15,12(R3)        GET A(RECACT TABLE)                          
         AR    R1,RB               RELOCATE ADDRESS                             
         ST    R1,ARECACT3         PASS TO GENCON                               
*                                                                               
DATAFLDX DS    0H                                                               
*                                                                               
         TITLE 'T40B00 - ADS CONTROLLER - SRDS ADS MAINT - SELFLD '             
***********************************************************************         
*                                                                     *         
*        ANALYZE SELECTION FIELD ON LIST SCREEN                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SELFLD   DS    0H                                                               
*                                                                               
         CLI   TWASCR,X'E0'        IF LIST    SCREEN                            
         BNE   SELFLDX                                                          
*                                                                               
         LA    R2,LSTSEL1H         POINT TO FIRST SELECT FIELD                  
         SR    RF,RF                                                            
*                                                                               
SELFLDLP DS    0H                                                               
*                                                                               
         CLI   0(R2),0             DONE IF END OF SCREEN                        
         BE    SELFLDDN                                                         
*                                                                               
         CLI   5(R2),1             LOOKING FOR FIRST FLD WITH ENTRY             
         BNL   SELFLDFD                                                         
*                                                                               
SELFLDCN DS    0H                                                               
*                                                                               
         IC    RF,0(R2)            BUMP TO NEXT FIELD ON SCREEN                 
         LA    R2,0(RF,R2)                                                      
*                                                                               
         CLI   0(R2),0             DONE IF END OF SCREEN                        
         BE    SELFLDDN                                                         
*                                                                               
         IC    RF,0(R2)            BUMP TO NEXT SEL FIELD ON SCREEN             
         LA    R2,0(RF,R2)                                                      
*                                                                               
         B     SELFLDLP                                                         
*                                                                               
SELFLDFD DS    0H                                                               
*                                                                               
         LHI   R3,SUBRECTB-T40B00  DISPLACEMENT OF SUB-RECORD TABLE             
         AR    R3,RB               RELOCATE ADDRESS                             
*                                                                               
         CLI   0(R3),X'FF'         DONE AT END OF TABLE                         
         BE    SELFLDDN                                                         
         CLC   8(1,R2),8(R3)       MATCH DATA FIELD SHORT FORM                  
         BE    *+12                                                             
         LA    R3,16(R3)                                                        
         B     *-22                                                             
*                                                                               
SELFLDF1 DS    0H                                                               
*                                                                               
         MVC   LSTDATA(8),0(R3)    SET NEW DATA FIELD                           
         MVI   LSTDATAH+5,8        SET INPUT LENGTH                             
*                                                                               
         MVI   8(R2),C'S'          SET FOR ACTION SELECT                        
*                                                                               
         ICM   R1,15,12(R3)        GET A(RECACT TABLE)                          
         AR    R1,RB               RELOCATE ADDRESS                             
         ST    R1,ARECACT3         PASS TO GENCON                               
*                                                                               
SELFLDDN DS    0H                                                               
*                                                                               
SELFLDX  DS    0H                                                               
*                                                                               
         TITLE 'T40B00 - ADS CONTROLLER - SRDS ADS MAINT - GOGENCON'            
***********************************************************************         
*                                                                     *         
*        GO TO GENCON                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
GOGENCON DS    0H                                                               
*                                                                               
         GOTO1 GENCON,DMCB,(R8)    GO GENCON - PASS A(WORKING STORAGE)          
*                                                                               
XIT      XIT1                      THEN WE'RE THROUGH                           
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40B00 - ADS CONTROLLER - SRDS ADS MAINT - SYSINIT'             
***********************************************************************         
*                                                                     *         
*        INITIALIZE SYSTEM DEPENDENT VALUES                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SYSINIT  NTR1  LABEL=*                                                          
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOL   WORKING STORAGE            
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
         USING SYSD,R9             ESTABLISH PROGRAM WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
*                                                                               
         LA    R2,SYSV                                                          
         LA    R3,SYSVCON                                                       
         LA    R4,NVTYPES                                                       
*                                                                               
         L     R1,0(R3)            RELOCATE SYSTEM VTYPES                       
         A     R1,RELO00                                                        
         ST    R1,0(R2)                                                         
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         BRCT  R4,*-20                                                          
*                                                                               
         LA    R2,VCOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    R4,SYSCOMM                                                       
         LA    R5,VCOUNT                                                        
*                                                                               
         ST    R2,0(R4)                                                         
         STC   R3,0(R4)                                                         
         LA    R3,4(R3)                                                         
         LA    R4,4(R4)                                                         
         BRCT  R5,*-16                                                          
*                                                                               
*        OBTAIN CORE-RESIDENT ADDRESSES                                         
*                                                                               
         XC    DMCB,DMCB           OBTAIN CORE-RESIDENT ADDRESSES               
         LA    R2,CORETAB                                                       
         LA    R0,CORES                                                         
         LA    R4,COREFACS         ADDRESS AREA                                 
*                                                                               
         L     R1,SYSPARMS         A(COMFACS)                                   
         L     R1,16(R1)                                                        
         USING COMFACSD,R1                                                      
*                                                                               
         MVC   VGLOBBER,CGLOBBER   SAVE GLOBBER ADDRESS                         
         MVC   VBINSRCH,CBINSRCH   SAVE BINSRCH ADDRESS                         
*                                                                               
         L     RF,CCALLOV                                                       
*                                                                               
         DROP  R1                                                               
*                                                                               
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
*                                                                               
SYS6     MVC   DMCB+7(1),0(R2)                                                  
         GOTO1 (RF),(R1),0                                                      
         CLI   DMCB+4,X'FF'                                                     
         JNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   0(4,R4),DMCB        SAVE MODULE ADDRESS                          
         LA    R2,1(R2)            NEXT MODULE NUMBER                           
         LA    R4,4(R4)            NEXT ADDRESS                                 
*                                                                               
         BRCT  R0,SYS6                                                          
*                                                                               
*        SET DATA AREA ADDRESSES                                                
*                                                                               
         LHI   RF,VLTABC-SYSD      A(PRVAL RESULTS TABLE)                       
         LA    RF,SYSD(RF)                                                      
         STCM  RF,15,AVLTAB                                                     
*                                                                               
         LHI   RF,HELPCBLK-SYSD    A(PRHELP CONTROL BLOCK)                      
         LA    RF,SYSD(RF)                                                      
         STCM  RF,15,AHLPCBLK                                                   
*                                                                               
         LHI   RF,VLPARMS-SYSD     A(PRVAL CONTROL BLOCK)                       
         LA    RF,SYSD(RF)                                                      
         STCM  RF,15,AVLPARMS                                                   
*                                                                               
         LA    R1,STARTSAV                                                      
         ST    R1,ASTARTSV                                                      
         MVI   SYSTEM,C'P'         PRINT                                        
         MVI   MAXIOS,NIOS         USES 3 I/O AREAS                             
         MVC   SIZEIO,=AL4(LIOS)   EACH I/O IS 4000 BYTES                       
         MVC   SYSDUMMY,VDUMMY     END OF SYSTEM BASE                           
         MVC   GETUSER,VUSER       ROUTINE TO GET USER NAME AND ADDRESS         
         LA    R1,RECACT           RECORD/ACTION DIRECTORY                      
         ST    R1,ARECACT                                                       
*                                                                               
* NO-OP  MVI   NTWA,X'81'          SAVE/RESTORE FULL 6K                         
         MVC   LSVTWA0,=AL2(6144)  SAVE/RESTORE 6K AT BOTTOM OF TWA0            
         MVI   NTWA,0              DON'T NEED ANY TEMPSTR PAGES                 
         OI    GENSTAT3,RESTXE00   SET TO RESTORE ENTIRE LIST SCREEN            
         OI    GENSTAT2,DISTHSPG   SET TO STAY ON SAME LIST PAGE ON             
*                                  RETURN FROM DETAIL SCREEN                    
         L     R3,ARECACT          A(RECORD TABLE)                              
*                                                                               
         CLI   CONRECH+5,0         TEST ANYTHING IN RECORD FIELD YET            
         JE    SYS10               NO - DEFAULT TO GENDIR/GENFIL                
*                                                                               
         MVI   WORK,1              LOOK UP RECORD ENTRIES IN RECACT             
         ZIC   R1,CONRECH+5                                                     
         EX    R1,*+8                                                           
         J     *+10                                                             
         MVC   WORK+1(0),CONREC    PUT DATA IN WORK                             
*                                                                               
SYS7     EX    R1,*+8                                                           
         J     *+10                                                             
         CLC   WORK(0),0(R3)       TEST MATCH ON RECORD NAME                    
         JE    SYS8                YES                                          
         LA    R3,12(R3)           TRY NEXT ENTRY                               
         CLI   0(R3),X'FF'         TEST END OF TABLE                            
         JNE   SYS7                NO                                           
         J     SYS10               YES - LET GENCON HANDLE ERROR                
*                                                                               
SYS8     J     SYS10                                                            
*                                                                               
SYS10    MVC   LKEY,=H'32'         GENDIR/GENFIL SPECIFICS                      
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SYSFIL,=C'GENFIL  '                                              
         MVC   SYSDIR,=C'GENDIR  '                                              
*                                                                               
SYS12    MVC   REQFILE,=C'PPREQ  '                                              
         MVC   LWORK,=AL4(LENWORK) SOFT WORK AREA LENGTH                        
         MVC   RCPROG(2),=C'PP'    PREFIX FOR REPORT NO.                        
         MVC   SYSPHASE,=X'D9040B00'    PRESET FOR SYSTEM CALLOVS               
*                                                                               
SYSINITX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         TITLE 'T40B00 - ADS CONTROLLER - SRDS ADS MAINT - GETEL'               
***********************************************************************         
*                                                                     *         
*        GETEL MACRO                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
***********************************************************************         
*                                                                     *         
*        BUMP ROUTINE                                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT SCREEN FIELD                    
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETURN =                                
         BR    RE                                                               
*                                                                               
BUMPU    ZIC   RF,0(R2)            BUMP TO NEXT UNPROTECTED FIELD               
         AR    R2,RF                                                            
         CLI   0(R2),0             EOS- RETRUN =                                
         BER   RE                                                               
         TM    1(R2),X'20'                                                      
         JNZ   BUMPU                                                            
         LTR   RE,RE               NOT EOS- RETURN NOT =                        
         BR    RE                                                               
*                                                                               
         TITLE 'T40B00 - ADS CONTROLLER - SRDS ADS MAINT - TABLES'              
***********************************************************************         
*                                                                     *         
*        GETEL MACRO                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
*                                                                               
SYSVCON  DS    0F                                                               
         DC    V(DUMMY)                                                         
*                                                                               
NVTYPES  EQU   (*-SYSVCON)/4                                                    
*                                                                               
*        TABLE OF CORE-RESIDENT PHASES NEEDED                                   
*                                                                               
CORETAB  DS    0X                                                               
         DC    AL1(QGENCON)                                                     
         DC    AL1(QLINUP)                                                      
         DC    AL1(QPRHELP)                                                     
         DC    AL1(QPRVAL)                                                      
CORES    EQU   (*-CORETAB)                                                      
*                                                                               
         TITLE 'T40B00 - ADS CONTROLLER - SRDS ADS MAINT - SUBRECTB'            
***********************************************************************         
*                                                                     *         
*        TABLE FOR SUB-RECORD IN DATA FIELD                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SUBRECTB DS    0D                                                               
*                                  CL8'SUB-RECORD NAME'                         
*                                  CL1'SELECT CODE'                             
*                                  XL3 - SPARE                                  
*                                  AL4(RECACT TABLE MINUS CSECT START)          
*                                                                               
         DC    CL8'CONTACTS',CL1'C',XL3'00',AL4(RECACT0-T40B00)                 
         DC    CL8'ADSIZE  ',CL1'A',XL3'00',AL4(RECACT1-T40B00)                 
         DC    CL8'BLEED   ',CL1'B',XL3'00',AL4(RECACT2-T40B00)                 
*****    DC    CL8'DIGITAL ',CL1'D',XL3'00',AL4(RECACT3-T40B00)                 
         DC    CL8'MATERIAL',CL1'M',XL3'00',AL4(RECACT4-T40B00)                 
         DC    CL8'ISSUE   ',CL1'I',XL3'00',AL4(RECACT5-T40B00)                 
*****    DC    CL8'NOTES   ',CL1'N',XL3'00',AL4(RECACT6-T40B00)                 
         DC    X'FF'               EOT                                          
*                                                                               
         EJECT                                                                  
*                                                                               
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
         DC    X'01',C'SRDS    ',AL1(01),X'0000'                                
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
         DC    X'02',C'PFM     ',AL1(13,13,00)                                  
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
******   DC    X'03',AL1(01,01),X'E002000080',C'    '  SRDS      MAINT          
         DC    X'03',AL1(01,10),X'E002000080',C'    '            LIST           
         DC    X'03',AL1(01,12),X'E002003278',C'SZSZ'            REPORT         
         DC    X'FF'               EOT                                          
*                                                                               
RECACT0  DS    0F                  DATA=CONTACTS                                
*                                                                               
         DC    X'03',AL1(01,01),X'D010000080',C'    '  CONTACTS  MAINT          
         DC    X'03',AL1(01,10),X'E002000080',C'    '            LIST           
         DC    X'03',AL1(01,12),X'E002003278',C'SZSZ'            REPORT         
         DC    X'FF'                                                            
*                                                                               
RECACT1  DS    0F                  DATA=ADSIZE                                  
*                                                                               
         DC    X'03',AL1(01,01),X'D111000080',C'    '  ADSIZE    MAINT          
         DC    X'03',AL1(01,10),X'E002000080',C'    '            LIST           
         DC    X'03',AL1(01,12),X'E002003278',C'SZSZ'            REPORT         
         DC    X'FF'                                                            
*                                                                               
RECACT2  DS    0F                  DATA=BLEED                                   
*                                                                               
         DC    X'03',AL1(01,01),X'D212000080',C'    '  BLEED     MAINT          
         DC    X'03',AL1(01,10),X'E002000080',C'    '            LIST           
         DC    X'03',AL1(01,12),X'E002003278',C'SZSZ'            REPORT         
         DC    X'FF'                                                            
*                                                                               
RECACT3  DS    0F                  DATA=DIGITAL                                 
*                                                                               
         DC    X'03',AL1(01,01),X'D313000080',C'    '  DIGITAL   MAINT          
         DC    X'03',AL1(01,10),X'E002000080',C'    '            LIST           
         DC    X'03',AL1(01,12),X'E002003278',C'SZSZ'            REPORT         
         DC    X'FF'                                                            
*                                                                               
RECACT4  DS    0F                  DATA=MATERIAL                                
*                                                                               
         DC    X'03',AL1(01,01),X'D414000080',C'    '  MATERIAL  MAINT          
         DC    X'03',AL1(01,10),X'E002000080',C'    '            LIST           
         DC    X'03',AL1(01,12),X'E002003278',C'SZSZ'            REPORT         
         DC    X'FF'                                                            
*                                                                               
RECACT5  DS    0F                  DATA=ISSUE                                   
*                                                                               
         DC    X'03',AL1(01,01),X'D515000080',C'    '  ISSUE     MAINT          
         DC    X'03',AL1(01,10),X'E002000080',C'    '            LIST           
         DC    X'03',AL1(01,12),X'E002003278',C'SZSZ'            REPORT         
         DC    X'FF'                                                            
*                                                                               
RECACT6  DS    0F                  DATA=NOTES                                   
*                                                                               
         DC    X'03',AL1(01,01),X'D616000080',C'    '  NOTES     MAINT          
         DC    X'03',AL1(01,10),X'E002000080',C'    '            LIST           
         DC    X'03',AL1(01,12),X'E002003278',C'SZSZ'            REPORT         
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T40B00 - ADS CONTROLLER - SRDS ADS MAINT - VCOMMON'             
***********************************************************************         
*                                                                     *         
*        SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VCOMMON  NTR1  BASE=SYSRB,LABEL=*                                               
*                                                                               
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
*                                                                               
         ST    RD,COMMRD                                                        
         L     R9,ASYSD            ESTABLISH WORKAREAS                          
         L     R8,ASPOOLD                                                       
         L     RA,ATWA                                                          
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOL   WORKING STORAGE            
         USING SYSD,R9             ESTABLISH PROGRAM WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
*                                                                               
         SRL   RF,24                                                            
         L     RF,VBRANCH(RF)                                                   
         AR    RF,RB               ADD RELOCATION FACTOR                        
         BASR  RE,RF               GOTO1 ROUTINE                                
*                                                                               
VCOMMONX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DS    0F                                                               
VBRANCH  DC    AL4(USER-T40B00)    VALIDATE USER                                
         DC    AL4(CURSERR-T40B00) ERROR EXIT WITH CURSOR POSITIONING           
         DC    AL4(VALSTYP-T40B00) VALIDATE SRD PUBLICATION TYPE                
         DC    AL4(DISSTYP-T40B00) DISPLAY  SRD PUBLICATION TYPE                
         DC    AL4(VALSTIT-T40B00) VALIDATE SRD PUBLICATION TITLE               
         DC    AL4(DISSTIT-T40B00) DISPLAY  SRD PUBLICATION TITLE               
         DC    AL4(HELPCHK-T40B00) CHECK IF IN MIDDLE OF HELP CALL              
         DC    AL4(VALSDAT-T40B00) VALIDATE SRD DATA TYPE                       
         DC    AL4(DISSDAT-T40B00) DISPLAY  SRD DATA TYPE                       
*                                                                               
VCOUNT   EQU   (*-VBRANCH)/4                                                    
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T40B00 - ADS CONTROLLER - SRDS ADS MAINT - USER'                
***********************************************************************         
*                                                                     *         
*        USER AGENCY                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
USER     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOL   WORKING STORAGE            
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
         USING SYSD,R9             ESTABLISH PROGRAM WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
*                                                                               
         GOTO1 SWITCH,DMCB,X'0AFFFFFF',0 SWITCH TO CONTROL SYSTEM               
*                                                                               
         TITLE 'T40B00 - ADS CONTROLLER - SRDS ADS MAINT - VALINI'              
***********************************************************************         
*                                                                     *         
*        INIT PRVAL BLOCK                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VALINI   DS    0H                                                               
*                                                                               
         XC    VLBLOCK(VLBLOCKL),VLBLOCK CLEAR PRVAL CONTROL BLOCK              
*                                                                               
         MVC   VLACFACS,ACOMFACS   A(COMFACS)                                   
         MVC   VLCTRY,CTRY         SET COUNTRY CODE                             
         MVC   VLLANG,LANG         LANGUAGE CODE                                
         MVC   VLAGENCY,TWAAGY     AGENCY                                       
         MVI   VLSYSTEM,VLSYSPRQ   PRINT SYSTEM                                 
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,VLTODAYC) GET TODAY'S DATE                  
*                                                                               
VALINIX  DS    0H                                                               
*                                                                               
         XC    GBLOCK,GBLOCK       CLEAR GETTXT ERROR AREA                      
*                                                                               
         CLI   OFFLINE,C'Y'        ALWAYS DO THIS OFFLINE                       
         JE    USER10                                                           
*                                                                               
         CLI   TWAFIRST,0          FIRST TIME?                                  
         JE    USER10              YES                                          
*                                                                               
         MVC   USERNAME(66),SVUSRNAM  AGY NAME/ADDRESS FROM ID RECORD           
*                                                                               
         J     USERX                                                            
*                                                                               
USER10   DS    0H                                                               
*                                                                               
         MVI   TWAFIRST,1          WE'VE BEEN THROUGH HERE                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4           BUILD ID RECORD KEY                          
*                                                                               
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),10(RA)  FROM TWA                                     
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD ',=C'CTFILE ',KEY,AIO1                    
*                                                                               
         MVI   SVUSRNAM,C'*'                                                    
*                                                                               
         L     R4,AIO1             ORIGIN DETAILS                               
         LA    R6,CTIDATA                                                       
         MVI   ELCODE,X'36'                                                     
*                                                                               
         BRAS  RE,FIRSTEL                                                       
         JNE   USERX                                                            
*                                                                               
         DROP  R4                                                               
*                                                                               
         USING CTORGD,R6                                                        
*                                                                               
         MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         MVC   SVUSRNAM,USERNAME                                                
         MVC   SVUSRADR,USERADDR                                                
*                                                                               
         DROP  R6                                                               
*                                                                               
USERX    DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T40B00 - ADS CONTROLLER - SRDS ADS MAINT - CURSERR'             
***********************************************************************         
*                                                                     *         
*        ERROR EXIT - USES GETTXT CALLS                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
CURSERR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOL   WORKING STORAGE            
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
         USING SYSD,R9             ESTABLISH PROGRAM WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
*                                                                               
         OI    GENSTAT2,USGETTXT   FLAGS ERREX TO CALL GETTXT                   
*                                                                               
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
*                                                                               
         MVC   GTINDX,GINDEX       MESSAGE INDEX                                
         MVC   GTMSGNO,GERROR      MESSAGE NUMBER                               
         MVC   GTMTYP,GMSGTYPE     MESSAGE TYPE                                 
         MVC   GTLTXT,GLTXT        LENGTH OF INSERTION TEXT                     
         MVC   GTATXT,GATXT        A(INSERTION TEXT)                            
*                                                                               
         CLC   GERROR,=H'60'       MESSAGE NUMBER <= 60 ?                       
         JH    *+8                 NO -- USE CONTROL SYSTEM MESSAGES            
         MVI   GTMSYS,X'FF'        USE GENERAL SYSTEM                           
*                                                                               
         DROP  RF                                                               
*                                                                               
CURSERRX DS    0H                                                               
         GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T40B00 - ADS CONTROLLER - SRDS ADS MAINT - VALSTYP'             
***********************************************************************         
*                                                                     *         
*        VALIDATE SRDS PUBLICATION TYPE                               *         
*                                                                     *         
*NTRY    R2==> SCREEN FIELD CONTAINING CODE                           *         
*              NEXT FIELD HOLDS DESCRIPTION (IF PROTECTED)            *         
*                                                                     *         
*EXIT          DESCRIPTION FIELD FILLED IN AND TRANSMITTED            *         
*        SRDTYP    CODE                                               *         
*        SRDTYPDS  DESCRIPTION                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VALSTYP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOL   WORKING STORAGE            
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
         USING SYSD,R9             ESTABLISH PROGRAM WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
*                                                                               
*        INIT SAVEAREAS                                                         
*                                                                               
         XC    SRDTYP,SRDTYP       SRDS TYPE CODE                               
         XC    SRDTYPDS,SRDTYPDS   SRDS TYPE DESCRIPTION                        
*                                                                               
         USING FLDHDRD,R2          ESTABLISH INPUT FIELD                        
*                                                                               
         CLI   FLDILEN,0           ENTRY REQUIRED                               
         JE    VSTPNOTE                                                         
*                                                                               
*        CHECK FOR HELP REQUEST                                                 
*                                                                               
         CLI   FLDDATA,C'+'        IF HELP REQUESTED                            
         BE    *+8                                                              
         CLI   FLDDATA,C'?'                                                     
         BE    *+8                                                              
         CLI   FLDDATA+1,C'?'                                                   
         BNE   VSTPHLPX                                                         
*                                                                               
         GOTO1 VPRHELP,DMCB,=AL2(PRQHMSTP),(R2),AHLPCBLK PUT OUT HELP           
*                                                                               
         DC    H'0'                SHOULD NEVER COME BACK                       
*                                                                               
VSTPHLPX DS    0H                                                               
*                                                                               
*        VALIDATE SRDS TYPE CODE                                                
*                                                                               
         L     R1,AVLPARMS                                                      
         USING VLPARMS,R1          ESTABLISH PRVAL PARAMETERS                   
*                                                                               
         GOTO1 VPRVAL,(R1),('VLPVALQ',=AL2(PRQSRDTP)),                 X        
               (FLDILEN,FLDDATA),AVLTAB,0,0,0                                   
*                                                                               
         CLI   VLPERR-VLPARMS(R1),0   CHECK FOR ERRORS                          
         JNE   VSTPNOTV                                                         
*                                                                               
         L     R5,AVLTAB           POINT TO RETURNED TABLE ENTRY                
         USING VLTABD,R5           ESTABLISH VALIDATION TABLE                   
*                                                                               
         OI    FLDIIND,FINPVAL     SRDS TYPE IS VALID                           
         MVC   SRDTYP,VLTICODE+1   SAVE SRDS PUBLICATION TYPE CODE              
         MVC   SRDTYPDS,VLTFULL    SAVE SRDS PUBLICATION TYPE DESC              
*                                                                               
*        DISPLAY DESCRIPTION                                                    
*                                                                               
         BRAS  RE,BUMP             BUMP TO NEXT FIELD ON SCREEN                 
*                                                                               
         TM    FLDATB,FATBPROT     SKIP IF NOT PROTECTED FIELD                  
         JNO   VSTPDSCX                                                         
*                                                                               
         MVC   FLDDATA(L'SRDTYPDS),SRDTYPDS DISPLAY DESCRIPTION                 
         OI    FLDOIND,FOUTTRN     FORCE TRANSMISSION                           
*                                                                               
VSTPDSCX DS    0H                                                               
*                                                                               
VALSTPX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
*        ERROR MESSAGES                                                         
*                                                                               
VSTPNOTV DS    0H                  SRDS TYPE NOT VALID                          
*                                                                               
         LHI   RF,PPESTPNV                                                      
         J     VSTPERR                                                          
*                                                                               
VSTPNOTE DS    0H                  SRDS TYPE REQUIRED                           
*                                                                               
         LHI   RF,PPESTPNE                                                      
         J     VSTPERR                                                          
*                                                                               
VSTPERR  DS    0H                                                               
*                                                                               
         STCM  RF,3,GERROR         SET ERROR MESSAGE CODE                       
*                                                                               
         GOTO1 VCURSERR                                                         
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T40B00 - ADS CONTROLLER - SRDS ADS MAINT - DISSTYP'             
***********************************************************************         
*                                                                     *         
*        DISPLAY  SRDS PUBLICATION TYPE                               *         
*                                                                     *         
*NTRY    R2==> SCREEN FIELD FOR        CODE                           *         
*              NEXT FIELD HOLDS DESCRIPTION (IF PROTECTED)            *         
*        SRDTYP    CODE TO BE DISPLAYED                               *         
*                                                                     *         
*EXIT          DESCRIPTION FIELD FILLED IN AND TRANSMITTED            *         
*        SRDTYP    CODE                                               *         
*        SRDTYPDS  DESCRIPTION                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DISSTYP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOL   WORKING STORAGE            
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
         USING SYSD,R9             ESTABLISH PROGRAM WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
*                                                                               
*        INIT SAVEAREAS                                                         
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           FIELD LENGTH                                 
         AHI   RF,-8               SUBTRACT HEADER LENGTH                       
*                                                                               
         TM    FLDATB,FATBXHDR     IF FIELD HAS EXTENDED HEADER                 
         BNO   *+8                                                              
         AHI   RF,-8                  SUBTRACT OFF ITS LENGTH                   
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                LOCATE WHERE WE ARE                          
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         XC    FLDDATA(0),FLDDATA  CLEAR FIELD                                  
*                                                                               
         MVC   FLDDATA(L'SRDTYP),SRDTYP   DISPLAY SRDS TYPE CODE                
         OI    FLDOIND,FOUTTRN     FORCE TRANSMISSION                           
         OI    FLDIIND,FINPVAL     INDICATE VALID ENTRY                         
*                                                                               
         XC    SRDTYPDS,SRDTYPDS   SRDS TYPE DESCRIPTION                        
*                                                                               
*        DISPLAY SRDS TYPE DESCRIPTION                                          
*                                                                               
         L     R1,AVLPARMS                                                      
         USING VLPARMS,R1          ESTABLISH PRVAL PARAMETERS                   
*                                                                               
         GOTO1 VPRVAL,(R1),('VLPVALQ',=AL2(PRQSRDTP)),                 X        
               (L'SRDTYP,SRDTYP),AVLTAB,0,0,0                                   
*                                                                               
         CLI   VLPERR-VLPARMS(R1),0   CHECK FOR ERRORS                          
         JE    *+6                                                              
         DC    H'0'                SHOULD NOT BE HERE                           
*                                                                               
         L     R5,AVLTAB           POINT TO RETURNED TABLE ENTRY                
         USING VLTABD,R5           ESTABLISH VALIDATION TABLE                   
*                                                                               
         MVC   SRDTYPDS,VLTFULL    SAVE SRDS PUBLICATION TYPE DESC              
*                                                                               
         BRAS  RE,BUMP             BUMP TO NEXT FIELD - DESCRIPTION             
*                                                                               
         TM    FLDATB,FATBPROT     SKIP IF NOT PROTECTED FIELD                  
         JNO   DSTPDSCX                                                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           FIELD LENGTH                                 
         AHI   RF,-8               SUBTRACT HEADER LENGTH                       
*                                                                               
         TM    FLDATB,FATBXHDR     IF FIELD HAS EXTENDED HEADER                 
         BNO   *+8                                                              
         AHI   RF,-8                  SUBTRACT OFF ITS LENGTH                   
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                LOCATE WHERE WE ARE                          
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   FLDDATA(0),VLTFULL  DISPLAY DESCRIPTION                          
         OI    FLDOIND,FOUTTRN     FORCE TRANSMISSION                           
*                                                                               
DSTPDSCX DS    0H                                                               
*                                                                               
DSPSTPX DS     0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T40B00 - ADS CONTROLLER - SRDS ADS MAINT - VALSTIT'             
***********************************************************************         
*                                                                     *         
*        VALIDATE SRDS PUBLICATION TITLE                              *         
*                                                                     *         
*NTRY    R2==> SCREEN FIELD CONTAINING TITLE                          *         
*                                                                     *         
*EXIT          DESCRIPTION FIELD FILLED IN AND TRANSMITTED            *         
*                                                                     *         
*        SRDTIT    TITLE                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VALSTIT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOL   WORKING STORAGE            
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
         USING SYSD,R9             ESTABLISH PROGRAM WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
*                                                                               
*        INIT SAVEAREAS                                                         
*                                                                               
         XC    SRDTIT,SRDTIT       SRDS TITLE                                   
         XC    SRDNUM,SRDNUM       SRDS NUMBER                                  
*                                                                               
         USING FLDHDRD,R2          ESTABLISH INPUT FIELD                        
*                                                                               
         CLI   FLDILEN,0           ENTRY OPTIONAL                               
         JE    VSTTOK                                                           
*                                                                               
*        CHECK FOR NAME SEARCH                                                  
*                                                                               
         CLI   FLDDATA,C'='        IF SEARCH REQUESTED                          
         JNE   VSTTSRCX                                                         
*                                                                               
         DC    H'0'                SHOULD NEVER COME BACK                       
*                                                                               
VSTTSRCX DS    0H                                                               
*                                                                               
         TM    FLDIIND,FINPNUM     IF VALID NUMERIC FIELD                       
         JNO   VSTTNUMN                                                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDILEN             GET FIELD LENGTH                          
         BCTR  RF,0                   DECREMENT FOR EXECUTE                     
         BASR  RE,0                   LOCATE WHERE WE ARE                       
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         PACK  DUB,FLDDATA            PACK SRDS NUMBER                          
         OI    DUB+7,X'0F'            FORCE SIGN                                
*                                                                               
         UNPK  SRDNUM,DUB             UNPACK SRDS NUMBER                        
*                                                                               
*        BUILD MASTER KEY                                                       
*                                                                               
         XC    KEY,KEY                INIT KEY                                  
         LA    R6,KEY                 ESTABLISH AS CONTROL PUB KEY              
         USING GPUBKEYD,R6                                                      
*                                                                               
         MVI   GPUBREC,GPUBRECQ       SET RECORD CODE                           
         MVI   GPUBTYP,GPUBTYPQ       SET RECORD TYPE                           
*                                                                               
         MVC   GPUBPUBT,SRDTYP        SET PUB TYPE                              
         MVC   GPUBPUB,SRDNUM         SET PUB NUMBER                            
         XC    GPUBAGY,GPUBAGY        SKIP AGENCY OVERRIDE                      
         MVI   GPUBSTYP,0             SET FOR HEADER RECORD                     
*                                                                               
         GOTO1 HIGH                   READ IN HEADER                            
*                                                                               
         CLC   GPUBKEYD,KEYSAVE       MUST FIND RECORD                          
         JNE   VSTTNOTF                                                         
*                                                                               
         GOTO1 GETREC                 READ IN RECORD                            
*                                                                               
         J     VSTTTIT                GO  FIND NAME                             
*                                                                               
VSTTNUMN DS    0H                                                               
*                                                                               
*        USE ENTERED TEXT WITH TEXT PASSIVE POINTERS                            
*                                                                               
         LA    R4,KEY              ESTABLISH NAME PASSIVE KEY                   
         USING GPUBNKYD,R4                                                      
         XC    KEY,KEY                                                          
*                                                                               
         MVI   GPUBNREC,GPUBNRCQ   SET RECORD CODE                              
         MVI   GPUBNTYP,GPUBNTYQ   SET RECORD TYPE                              
*                                                                               
         MVC   GPUBNPBT,SRDTYP     SET PUB TYPE                                 
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDILEN          GET INPUT LENGTH                             
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                FIND OUT WHERE WE ARE                        
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   GPUBNNAM(0),FLDDATA SET PARTIAL PUB NAME                         
*                                                                               
         GOTO1 HIGH                READ PASSIVE                                 
*                                                                               
         CLC   GPUBNKYD(GPUBNNAM-GPUBNKYD),KEYSAVE   MUST MATCH ON TYPE         
         JNE   VSTTNOTF                                                         
*                                                                               
         MVC   SRDNUM,GPUBNPUB     SAVE PUB NUMBER                              
*                                                                               
         GOTO1 GETREC              READ IN RECORD                               
*                                                                               
         J     VSTTTIT             GO GET TITLE                                 
*                                                                               
VSTTTIT  DS    0H                                                               
*                                                                               
         L     R4,AIO1             POINT TO FOUND RECORD                        
*                                                                               
         MVI   ELCODE,GPUBHEQU     SET FOR HEADER ELEMENT                       
         LR    R6,R4               POINT TO START OF RECORD                     
         BRAS  RE,GETEL            FIND HEADER ELEMENT                          
         JNE   VSTTNOTF            ELEMENT NOT FOUND                            
*                                                                               
         USING GPUBHEL,R6          ESTABLISH HEADER ELEMENT                     
*                                                                               
         MVC   SRDTIT,GPUBHALN     RETRIEVE PUB NAME                            
*                                                                               
         J     VALSTITX                                                         
*                                                                               
VSTTOK   DS    0H                                                               
*                                                                               
VALSTITX DS    0H                                                               
         XIT1                                                                   
*                                                                               
*        ERROR MESSAGES                                                         
*                                                                               
VSTTNOTF DS    0H                  SRDS PUB NOT FOUND                           
*                                                                               
         LHI   RF,PPESPBNF                                                      
         J     VSTTERR                                                          
*                                                                               
VSTTERR  DS    0H                                                               
*                                                                               
         STCM  RF,3,GERROR         SET ERROR MESSAGE CODE                       
*                                                                               
         GOTO1 VCURSERR                                                         
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T40B00 - ADS CONTROLLER - SRDS ADS MAINT - DISSTIT'             
***********************************************************************         
*                                                                     *         
*        DISPLAY  SRDS PUBLICATION TITLE                              *         
*                                                                     *         
*NTRY    R2==> SCREEN FIELD FOR        CODE                           *         
*        I/O1  HEADER RECORD                                          *         
*                                                                     *         
*EXIT          TITLE FIELD FILLED IN AND TRANSMITTED                  *         
*        SRDTIT    TITLE                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DISSTIT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOL   WORKING STORAGE            
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
         USING SYSD,R9             ESTABLISH PROGRAM WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
*                                                                               
*        INIT SAVEAREAS                                                         
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           FIELD LENGTH                                 
         AHI   RF,-8               SUBTRACT HEADER LENGTH                       
*                                                                               
         TM    FLDATB,FATBXHDR     IF FIELD HAS EXTENDED HEADER                 
         BNO   *+8                                                              
         AHI   RF,-8                  SUBTRACT OFF ITS LENGTH                   
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                LOCATE WHERE WE ARE                          
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         XC    FLDDATA(0),FLDDATA  CLEAR FIELD                                  
*                                                                               
         MVC   FLDDATA(L'SRDTIT),SRDTIT   DISPLAY SRDS TITLE                    
         OI    FLDOIND,FOUTTRN     FORCE TRANSMISSION                           
         OI    FLDIIND,FINPVAL     INDICATE VALID ENTRY                         
*                                                                               
DSPSTITX DS     0H                                                              
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'PPADS00 SRDS LIST SCREEN - HELPCHK'                             
***********************************************************************         
*                                                                     *         
*        CHECK TO SEE IF IN THE MIDDLE OF A HELP CALL                 *         
*        ALSO INITIALIZES HELP CONTROL BLOCK                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
HELPCHK  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOL   WORKING STORAGE            
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
         USING SYSD,R9             ESTABLISH PROGRAM WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
*                                                                               
         OC    TWAVPRNT,TWAVPRNT   SKIP IF OFF-LINE                             
         JNZ   HELPCHKX                                                         
*                                                                               
         LHI   R6,HELPCBLK-SYSD    ESTABLISH HELP CONTROL BLOCK                 
         LA    R6,SYSD(R6)                                                      
         USING HELPCB,R6                                                        
*                                                                               
         XC    HELPCB(HELPCBL),HELPCB  INIT HELP CONTROL BLOCK                  
*                                                                               
         LHI   RF,PRQHLPMN                                                      
         STCM  RF,3,HCBMTIC        USE PRINT HELP MENUS                         
*                                                                               
         MVI   HCBSCRN,X'EF'       SCREEN TO DISPLAY MENUS ON                   
         MVI   HCBPAGE,1           SAVE IN FIRST TWA PAGE                       
         MVC   HCBATWA,ATWA        SET SYSTEM ADDRESSES                         
         MVC   HCBACOM,ACOMFACS                                                 
         MVC   HCBATIOB,ATIOB      POINT TO TIOBD                               
         MVC   HCBASYRD,SYSRD                                                   
         MVC   HCBCTRY,CTRY        SET COUNTRY                                  
         MVC   HCBLANG,LANG        SET LANGUAGE                                 
         MVI   HCBSYST,HCBSYSPR    SET SYSTEM AS PRINT                          
         MVC   HCBATAB,ATIA        USE TIA FOR DDVAL TABLES                     
         LHI   RF,HELPSAVE-(CONHEADH-64)  DISPLACEMENT OF SAVEAREA              
         LA    RF,0(RF,RA)         HELP SAVE AREA                               
         ST    RF,HCBASAVE         PASS ADDRESS                                 
         MVI   HCBSEGS,4           PRVAL TAB AREA IS 4*256 LONG                 
         MVI   HCBQVAL,QPRVAL      PRVAL DDCOREQUS NUMBER                       
*                                                                               
         GOTO1 VPRHELP,DMCB,0,0,AHLPCBLK GO CHECK IF IN MIDDLE OF MENU          
*                                                                               
         TM    HCBRTRN,HCBRSELQ    IF SELECTION WAS MADE                        
         JNO   HELPCHKX                                                         
*                                                                               
         GOTO1 ERREX2                 EXIT FOR USER TO COMPLETE ENTRY           
*                                                                               
HELPCHKX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T40B00 - ADS CONTROLLER - SRDS ADS MAINT - VALSDAT'             
***********************************************************************         
*                                                                     *         
*        VALIDATE SRDS DATA TYPE                                      *         
*                                                                     *         
*NTRY    R2==> SCREEN FIELD CONTAINING CODE                           *         
*                                                                     *         
*EXIT          DESCRIPTION FIELD FILLED IN AND TRANSMITTED            *         
*        SRDDATA   DATA TYPE                                          *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VALSDAT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOL   WORKING STORAGE            
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
         USING SYSD,R9             ESTABLISH PROGRAM WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
*                                                                               
*        INIT SAVEAREAS                                                         
*                                                                               
         XC    SRDDATA,SRDDATA     SRDS DATA TYPE                               
*                                                                               
         USING FLDHDRD,R2          ESTABLISH INPUT FIELD                        
*                                                                               
         CLI   FLDILEN,0           ENTRY REQUIRED                               
         JE    VSRDNOTE                                                         
*                                                                               
*        CHECK FOR HELP REQUEST                                                 
*                                                                               
         CLI   FLDDATA,C'+'        IF HELP REQUESTED                            
         BE    *+8                                                              
         CLI   FLDDATA,C'?'                                                     
         BE    *+8                                                              
         CLI   FLDDATA+1,C'?'                                                   
         BNE   VSRDHLPX                                                         
*                                                                               
         GOTO1 VPRHELP,DMCB,=AL2(PRQHMSRD),(R2),AHLPCBLK PUT OUT HELP           
*                                                                               
         DC    H'0'                SHOULD NEVER COME BACK                       
*                                                                               
VSRDHLPX DS    0H                                                               
*                                                                               
*        VALIDATE SRDS DATA TYPE                                                
*                                                                               
         L     R1,AVLPARMS                                                      
         USING VLPARMS,R1          ESTABLISH PRVAL PARAMETERS                   
*                                                                               
         GOTO1 VPRVAL,(R1),('VLPVALQ',=AL2(PRQSRDAT)),                 X        
               (FLDILEN,FLDDATA),AVLTAB,0,0,0                                   
*                                                                               
         CLI   VLPERR-VLPARMS(R1),0   CHECK FOR ERRORS                          
         JNE   VSRDNOTV                                                         
*                                                                               
         L     R5,AVLTAB           POINT TO RETURNED TABLE ENTRY                
         USING VLTABD,R5           ESTABLISH VALIDATION TABLE                   
*                                                                               
         MVC   SRDDATA,VLTFULL     SAVE SRDS DATA TYPE                          
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           FIELD LENGTH                                 
         AHI   RF,-8               SUBTRACT HEADER LENGTH                       
*                                                                               
         TM    FLDATB,FATBXHDR     IF FIELD HAS EXTENDED HEADER                 
         BNO   *+8                                                              
         AHI   RF,-8                  SUBTRACT OFF ITS LENGTH                   
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                LOCATE WHERE WE ARE                          
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         XC    FLDDATA(0),FLDDATA  CLEAR FIELD                                  
*                                                                               
         BASR  RE,0                LOCATE WHERE WE ARE                          
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   FLDDATA(0),VLTFULL  DISPLAY SRDS DATA TYPE                       
*                                                                               
         OI    FLDOIND,FOUTTRN     FORCE TRANSMISSION                           
         OI    FLDIIND,FINPVAL     INDICATE VALID ENTRY                         
*                                                                               
VALSRDX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
*        ERROR MESSAGES                                                         
*                                                                               
VSRDNOTV DS    0H                  SRDS DATA TYPE NOT VALID                     
*                                                                               
         LHI   RF,PPESRDNV                                                      
         J     VSRDERR                                                          
*                                                                               
VSRDNOTE DS    0H                  SRDS DATA TYPE REQUIRED                      
*                                                                               
         LHI   RF,PPESRDNE                                                      
         J     VSRDERR                                                          
*                                                                               
VSRDERR  DS    0H                                                               
*                                                                               
         STCM  RF,3,GERROR         SET ERROR MESSAGE CODE                       
*                                                                               
         GOTO1 VCURSERR                                                         
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         TITLE 'T40B00 - ADS CONTROLLER - SRDS ADS MAINT - DISSDAT'             
***********************************************************************         
*                                                                     *         
*        DISPLAY  SRDS DATA TYPE                                      *         
*                                                                     *         
*NTRY    R2==> SCREEN FIELD FOR        CODE                           *         
*        SRDTDATA  CODE TO BE DISPLAYED                               *         
*                                                                     *         
*EXIT          DESCRIPTION FIELD FILLED IN AND TRANSMITTED            *         
*        SRDDATA   CODE                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DISSDAT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SPOOLD,R8           ESTABLISH SPOOL   WORKING STORAGE            
         USING GEND,RC             ESTABLISH GENCON  WORKING STORAGE            
         USING SYSD,R9             ESTABLISH PROGRAM WORKING STORAGE            
         USING CONHEADH-64,RA      ESTABLISH TWA                                
*                                                                               
*        INIT SAVEAREAS                                                         
*                                                                               
         USING FLDHDRD,R2          ESTABLISH SCREEN FIELD                       
*                                                                               
         SR    RF,RF                                                            
         IC    RF,FLDLEN           FIELD LENGTH                                 
         AHI   RF,-8               SUBTRACT HEADER LENGTH                       
*                                                                               
         TM    FLDATB,FATBXHDR     IF FIELD HAS EXTENDED HEADER                 
         BNO   *+8                                                              
         AHI   RF,-8                  SUBTRACT OFF ITS LENGTH                   
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         BASR  RE,0                LOCATE WHERE WE ARE                          
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         XC    FLDDATA(0),FLDDATA  CLEAR FIELD                                  
*                                                                               
         MVC   FLDDATA(L'SRDDATA),SRDDATA   DISPLAY SRDS DATA TYPE CODE         
         OI    FLDOIND,FOUTTRN     FORCE TRANSMISSION                           
         OI    FLDIIND,FINPVAL     INDICATE VALID ENTRY                         
*                                                                               
DISSDATX DS     0H                                                              
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP                                                                   
*                                                                               
         EJECT                                                                  
       ++INCLUDE PPADSWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTGENPUBS                                                      
*                                                                               
         TITLE 'T40B00 - ADS CONTROLLER - SRDS ADS MAINT - TWA'                 
***********************************************************************         
*                                                                     *         
*        TWA LAYOUT                                                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
       ++INCLUDE PPADSFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE PPADSE0D                                                       
*                                                                               
       ++INCLUDE DDGENTWA                                                       
*                                                                               
         ORG   CONHEADH-64+X'1800'                                              
HELPSAVE DS    XL512               HELP CONTROL BLOCK SAVE                      
*                                                                               
         TITLE 'T40B00 - ADS CONTROLLER - SRDS ADS MAINT - DSECTS'              
***********************************************************************         
*                                                                     *         
*        HIDDEN DSECTSSTEM DEPENDENT VALUES                           *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         PRINT ON                                                               
* PRGLOBEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE PRGLOBEQUS                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013PPADS00   05/01/02'                                      
         END                                                                    
