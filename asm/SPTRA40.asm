*          DATA SET SPTRA40    AT LEVEL 079 AS OF 02/17/16                      
*PHASE T21640A                                                                  
         TITLE 'T21640 - (SPOT) COMMERCIAL ASSIGN'                              
***********************************************************************         
*                                                                     *         
*  TO DO - VALIDATE LIST OF COMMLS IN OPTIONS AND UPDATE ALL          *         
*          SAVE PARTIAL?                                              *         
*          UPDATE AS ENTERED RATHER THAN NEED 'SAVE' ENTERED?         *         
*                                                                     *         
***********************************************************************         
***********************************************************************         
*                                                                     *         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)         *         
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER      *         
*                    (DDGENCON-T00A30)  - GETPTN RTN PATTERN TABLE    *         
*             AIO2 - COMMERCIAL RECORDS - APTN RTN PTTRNS, THEN TABLE *         
*                                                                     *         
*             AIO3 - TABLE OF COMMERCIALS - GETPTN RD COMMERCIALS     *         
*                                                                     *         
* REGISTER USAGE -                                                    *         
*        R0 - WORK REG                                                *         
*        R1 - WORK REG                                                *         
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR       *         
*        R3 - SPTABLE POINTER                                                   
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG                                                          
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 - SECOND BASE REG                                                   
*        R8 - POINTER TO SPOOLD                                                 
*        R9 - POINTER TO SYSD                                                   
*        RA - POINTER TO ATWA                                                   
*        RB - FIRST BASE                                                        
*        RC - POINTER TO GEND                                                   
*        RD - SAVE AREA POINTER                                                 
*        RE - GOTO1 REG                                                         
*        RF - GOTO1 REG                                                         
*                                                                               
*        PROGRAM LABELS MEANING:                                                
*        V PREFIX = VALIDATE                                                    
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                              
*        F PREFIX = FIND                                                        
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST                    
*                                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
* LEV 59 SMUR MAR12/00 ADD MORE CODE TO VALIDATE CML                  *         
* LEV 59 SMUR APR11/01 USE TRAFFIC OFFICE                             *         
* LEV 60 SMUR NOV16/01 BROADCAST BUSINESS APPROVAL OVERRRIDES ALL APP *         
* LEV 61 SMUR MAY09/02 FIX CML REFERENCE FOR STARCOM                  *         
* LEV 62 SMUR JUN03/02 FIX CML2/PRD2                                  *         
* LEV 63 BGRI OCT14/03 MAKE PROGTABL LARGER                           *         
* LEV 64 BGRI DEC05/03 ERR MSG FOR TOO MANY SPOTS                     *         
* LEV 65 BGRI JUL28/04 SOX                                            *         
* LEV 66 SMUR DEC13/04 ABILITY TO CLEAR ALL COMERCIALS                *         
*                   GIVE ERR MSG 'MUST USE SEED' IF P/B CML FOR 1 PRD *         
* LEV 68 SMUR DEC14/07 BYPASS TRAFFIC=NO BUYS                         *         
*        MHER SEP/08   PROCESS DAYPART W/IN ESTIMATE                  *         
* LEV 76 MNAS JAN22/13 MORE BANDS                                     *         
* LEV 77 SMUR JUL16/13 DELETE EMPTY X'18' ELEMS                       *         
* LEV 78 SMUR APR01/14 USE R1 NOT R0 IN EX INSTRUCTION                *         
* LEV 79 SMUR JAN06/16 NEW BAND CM FOR IHEART RADIO                   *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T21640   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1640**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTRRR                                                        
         LA    R3,TOPASVTB         START OF SAVED AREA                          
         LA    R3,SVSTART          START OF SAVED AREA                          
         AHI   R3,6144                                                          
         ST    R3,ASVSTOR                                                       
         LA    R3,SPTABLE                                                       
         L     R0,LSYSD                                                         
         AR    R0,R9                                                            
         LR    R1,R9                                                            
         AH    R1,=AL2(ENDSYSD-SYSD)                                            
         CR    R0,R1                                                            
         BNL   *+6                                                              
         DC    H'0'                USED MORE THAN SYSD                          
*                                                                               
* GENCON WILL SAVE STORAGE FROM SVSTART IN SYSD FOR 6144 BYTES                  
* THIS PROGRAM ALSO SAVES THE LST 6144 BYTES OF SYSD (SPTABLE)                  
* ASVSTOR MUST NOT SAVE ANY STORAGE PAST ENDSYSD                                
*                                                                               
         AHI   R1,-6144                                                         
         C     R1,ASVSTOR                                                       
         BNH   *+6                                                              
         DC    H'0'         GAP BETWEEN GENCON SV STOR AND THIS PROGRAM         
*                                  SAVED STORE                                  
         BNL   *+8                                                              
         ST    R1,ASVSTOR       DON'T DESTROY RD NEXT SAVED REGISTERS           
*                                                                               
         L     RE,AIO3             CLEAR COMML TABLE AREA                       
         LHI   RF,6000                                                          
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    GET ADDRESS OF TRPACK                  
         GOTO1 CALLOV,DMCB                                                      
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         CLI   TWAOFFC,C'*'        ALLOW DDS TERMINALS                          
         BE    INIT10                                                           
*                                                                               
* FIND DCB ADDRESS AND SEE IF READ ONLY                                         
*                                                                               
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT MEDIA SYSTEM                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DTFAD',=C'SPTDIR'                                
*                                                                               
         L     RE,12(R1)           GET ADDR OF DCB                              
*                                                                               
         TM    ISFOPEN-ISDTF(RE),ISFORO+ISFONOP  READ ONLY OR NOP               
         BNZ   WRITERR                                                          
*                                                                               
         GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO SPOT TRAFFIC SYSTEM           
*                                                                               
INIT10   CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*     TEST FOR ANY CHANGES IN KEY FIELDS                                        
                                                                                
VK       CLI   TRLSTPGM,X'40'      WAS THIS PROGRAM LAST ACTIVE                 
         BNE   VK10                  NO, FORCE REVALIDATE                       
         TM    TRAMEDH+4,X'20'     MEDIA CHANGED                                
         BZ    VK10                  YES REVALIDATE                             
         TM    TRACLTH+4,X'20'     CLIENT CHANGED                               
         BZ    VK10                  YES REVALIDATE                             
         TM    TRAPRDH+4,X'20'     PRODUCT CHANGED                              
         BZ    VK10                  YES REVALIDATE                             
         TM    TRAPTRH+4,X'20'     PARTNER CHANGED                              
         BZ    VK10                  YES REVALIDATE                             
         TM    TRAESTH+4,X'20'     ESTIMATE CHANGED                             
         BZ    VK10                  YES REVALIDATE                             
         TM    TRAPERH+4,X'20'     PERIOD CHANGED                               
         BZ    VK10                  YES REVALIDATE                             
         TM    TRASTAH+4,X'20'     MARKET/STATION CHANGED                       
         BZ    VK10                  YES REVALIDATE                             
         TM    TRAOPTH+4,X'20'     OPTIONS CHANGED                              
         BO    GEN10                 NO, DISPLAY NEXT/TOP LIST SPOTS            
         EJECT                                                                  
COPT     BRAS  RE,RDTWA            RESTORE SAVED SPOT TABLE                     
         LA    R2,TRAOPTH                                                       
         TM    TABLESW,TABLEBL     TABLE BUILT?                                 
         BO    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    COPT50               NONE                                        
*                                                                               
         BAS   RE,VOPT                                                          
*                                                                               
* CHECK FOR SPECIAL REQUESTS                                                    
*                                                                               
         TM    LOG,LOGPTTN+LOGCREF WERE PATTERNS SEEDED OR CLEARED              
         BNZ   DSPLY10                                                          
*                                                                               
         TM    TABLESW,ACMLPRG     WAS ASSIGN CML TO PROGRAM DONE?              
         BZ    *+12                                                             
         NI    TABLESW,X'FF'-ACMLPRG RESET                                      
         B     DSPLY10                                                          
*                                                                               
         TM    LOG,LOGSAVE         WAS SAVE REQUESTED                           
         BO    UPDATE                                                           
*                                                                               
         CLI   FTPROGLN,0          WAS SPECIFIC PROG REQUESTED                  
         BNE   DSPLY10              YES, START FROM START OF TABLE              
*                                                                               
         TM    LOG,LOGCLR          WAS CLEAR ASSIGNS REQUESTED                  
         BZ    GEN20                NO, EDIT REST OF SCREEN                     
*                                                                               
         NI    LOG,X'FF'-LOGCLR    RESET                                        
         LA    R3,SPTABLE                                                       
         XC    TRAOPT,TRAOPT                                                    
         OI    TRAOPTH+6,X'80'                                                  
         OI    TRAOPTH+4,X'20'    VALIDATED                                     
         B     DSPLY10                                                          
*                                                                               
COPT50   OI    TRAOPTH+4,X'20'    VALIDATED                                     
*                                                                               
         TM    LOG,LOGREQD         RETURN FROM PREV REQUEST                     
         BZ    GEN20               NO, EDIT ANY INPUT                           
         NI    LOG,X'FF'-LOGREQD                                                
         TM    LOG,LOGTCML+LOGTREF ANY CURR REQ                                 
         BNZ   TOPT10              GO HONOR THIS REQUEST                        
         LA    R3,SPTABLE                                                       
         A     R3,NXTASVTB                                                      
         B     DSPLY10             NO, EDIT ANY INPUT                           
         EJECT                                                                  
* VALIDATE KEY FIELDS                                                           
*                                                                               
VK10     DS   0H                                                                
         BAS   RE,CLRCLT           RESET OTHER KEY FLDS                         
*                                                                               
         BRAS  RE,SVTWA                                                         
*                                                                               
         BAS   RE,CLRSCR           CLEAR SCREEN                                 
*                                                                               
         LA    R2,TRAMEDH          MEDIA                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR                                                          
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'         SET ON VALIDATED                             
*                                                                               
         LA    R2,TRACLTH          CLIENT                                       
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         GOTO1 VALICLT                                                          
         BAS   RE,FPRO             GO GET PROFILE RECORD(S)                     
         OI    4(R2),X'20'         SET ON VALIDATED                             
*                                                                               
         LA    R2,TRAPRDH          PRODUCT                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR                                                          
         GOTO1 VALIPRD                                                          
         CLC   =C'POL',WORK                                                     
         BE    PRDINV                                                           
         CLI   WORK+4,0            VALID SPOT LEN                               
         BNE   VK16                YES                                          
         MVI   WORK+4,30           DEFAULT SPOT LENGTH IS 30 SEC                
         LA    R0,L'TRAPRD                                                      
         LA    R1,TRAPRD                                                        
         CLI   0(R1),C' '                                                       
         BNH   VK14                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,*-12                                                          
         B     PRDINV                                                           
VK14     MVC   0(3,R1),=C'-30'                                                  
         OI    TRAPRDH+6,X'80'    FORCE TRANSMIT                                
VK16     MVC   QPRD,WORK                                                        
         MVC   BPRD(2),WORK+3      GET BIN PROD AND SPOT LENGTH                 
         MVC   BPRD,WORK+3                                                      
         OI    4(R2),X'20'         SET ON VALIDATED                             
*                                                                               
         LA    R2,TRAPTRH          PARTNER                                      
         MVC   QPRD2,SPACES                                                     
         MVI   BPRD2,0                                                          
         MVI   BSLN2,0                                                          
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK28                                                             
         GOTO1 ANY                                                              
         CLC   =C'NONE',WORK       DON'T ALLOW ANY PIGGYBACK PRODS              
         BE    VK26                                                             
         GOTO1 VALIPRD                                                          
         CLC   =C'POL',WORK                                                     
         BE    PRDINV                                                           
*                                                                               
VK24     MVC   QPRD2,WORK                                                       
         MVC   BPRD2,WORK+3                                                     
         MVC   BSLN2,WORK+4                                                     
         B     VK28                                                             
VK26     MVI   BPRD2,255                                                        
*                                                                               
VK28     OI    4(R2),X'20'         SET ON VALIDATED                             
*                                                                               
* EDIT ESTIMATE *                                                               
*                                                                               
VK30     LA    R2,TRAESTH          EST NUMBER                                   
*                                                                               
         XC    SVESTAB,SVESTAB                                                  
         MVI   BEST,0                                                           
         CLI   5(R2),0                                                          
         BNE   VK34                                                             
         CLI   SVPROF+10,C'E'      COPY CODE = ESTIMATE                         
         BNE   VK40                                                             
         B     MISESTER                                                         
*                                                                               
VK34     GOTO1 ANY                                                              
         CLI   SVPROF+10,C'E'      COPY CODE = ESTIMATE                         
         BNE   *+14                 NEEDS EST                                   
         CLC   =C'NO ',WORK                                                     
         BE    MISESTER                                                         
*                                                                               
         GOTO1 VALINUM                                                          
*                                                                               
         MVC   BEST,ACTUAL         SET AS EST                                   
*                                                                               
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT SYSTEM                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),BEST                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE     SEE IF ESTIMATE FOUND                        
         BNE   BDESTPR                                                          
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING ESTHDRD,R6                                                       
*                                                                               
         GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
*                                                                               
         CLI   ECOPY,0             COPY CODE ILLEGAL                            
         BNE   ESTCPYER                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(0,ESTART),(3,SVGENST)                               
         GOTO1 (RF),(R1),(0,EEND),(3,SVGENEND)                                  
         DROP  R6                                                               
*                                                                               
VK40     LA    R2,TRASTAH          MARKET/STATION                               
         XC    BMKTSTA,BMKTSTA                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR             NO, ERROR                                    
         TM    4(R2),X'08'         IS THIS NUMERIC                              
         BO    VK44                                                             
         GOTO1 VALISTA                                                          
*                                                                               
         L     R6,AIO1                                                          
         USING STARECD,R6                                                       
         MVC   SVAFFIL,SNETWRK                                                  
         MVC   SVTYPE,STYPE                                                     
         MVC   TRAHDG+28(7),=C'PROGRAM'                                         
         XC    TRAHDG+29(6),SPACES MAKE IT LOWERCASE                            
         B     VK46                                                             
*                                                                               
VK44     GOTO1 VALIMKT                                                          
                                                                                
* CHANGE HEADING SINCE STATION WILL DISPLAY INSTEAD OF PROGRAM                  
                                                                                
         MVC   TRAHDG+28(7),=C'STATION'                                         
         XC    TRAHDG+29(6),SPACES MAKE IT LOWERCASE                            
*                                                                               
VK46     OI    TRAHDGH+6,X'80'                                                  
         OI    4(R2),X'20'         SET ON VALIDATED                             
*                                                                               
VK50     LA    R2,TRAPERH          PERIOD                                       
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR             NO, ERROR                                    
         BRAS  RE,VPER             VALIDATE PERIOD/FLIGHT                       
*                                                                               
         XC    FTPROGNM,FTPROGNM   INIT PROGRAM NAME FILTER                     
         MVI   FTPROGLN,0                                                       
*                                                                               
         LA    R2,TRAOPTH                                                       
         BAS   RE,VOPT                                                          
*                                                                               
* READ ESTIMATE HEADERS AND BUILD LIST OF ELIGIBLE                              
*                                                                               
         BRAS  RE,BLEST                                                         
         BE    VK60                                                             
         CLI   SVTBPR04,C'Y'       NO ESTIMATES ALLOWED?                        
         BNE   NOESTER                                                          
         MVI   SVESTAB,X'FF'       ALLOW ALL ESTIMATES                          
         MVC   SVESTAB+1(L'SVESTAB-1),SVESTAB                                   
*                                                                               
VK60     MVI   TRLSTPGM,X'40'      SET THIS PROGRAM LAST ACTIVE                 
         TM    TABLESW,TABLEBL     IS SPOT TABLE BUILT                          
         BZ    BUY10                NO, BUILD IT                                
         DC    H'0'                                                             
*                                                                               
CLRCLT   NTR1                                                                   
         LA    RE,TOPASVTB                                                      
         LA    RF,SPTABLE-TOPASVTB                                              
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         XC    AFPD(ELCDHI+1-AFPD),AFPD                                         
         LHI   R0,(ENDSYSD-SPTABLE)/L'SPTDATA                                   
         LA    R1,SPTABLE                                                       
         XC    0(L'SPTDATA,R1),0(R1)                                            
         LA    R1,L'SPTDATA(R1)                                                 
         BCT   R0,*-10                                                          
         XIT1                                                                   
         EJECT                                                                  
*================================================================               
* READ BUYS AND BUILD TABLE OF SPOTS                                            
*================================================================               
                                                                                
BUY10    XC    SPOTCT(4),SPOTCT    ZERO SPOT/UNCML CTS                          
*                                                                               
         BRAS  RE,BLACT            BUILD SPOT TABLE                             
*                                                                               
* FIND END OF SPTABLE *                                                         
*                                                                               
         MVC   TRAINFO(13),=C'TOTAL SPOTS ='                                    
         LH    R2,SPOTCT                                                        
         LA    R3,TRAINFO+14                                                    
         EDIT  (R2),(3,(R3)),ALIGN=LEFT,ZERO=NOBLANK                            
         OI    TRAINFOH+6,X'80'                                                 
*                                                                               
         OI    TRAPERH+4,X'20'     SET PERIOD VALID                             
         OI    TRAESTH+4,X'20'     SET ESTIMATE VALID                           
*                                                                               
         OI    TABLESW,TABLEBL     SPOT TABLE IS NOW BUILT                      
*                                                                               
* DISPLAY 1 SCREEN FULL OF SPOTS                                                
*                                                                               
         LA    R3,SPTABLE                                                       
         USING SPTABLED,R3                                                      
DSPLY10  BAS   RE,CLRSCRB          CLEAR BOTTOM OF SCREEN                       
         LR    R0,R3                                                            
         LA    R1,SPTABLE                                                       
         SR    R0,R1                                                            
         ST    R0,TOPASVTB         SAVE TOP OF SCREEN ENTRY ADDR                
         LA    R2,TRAEST1H                                                      
         MVC   TRAINFO+40(27),=CL27'SPOTS NNN TO NNN DISPLAYED'                 
         LA    R6,TRAINFO+46                                                    
         EDIT  (B1,SPTTBLN),(3,(R6))                                            
*                                                                               
DSPLY20  DS   0H                                                                
         CLI   FTPROGLN,0          FILTERING ON PROGRAM NAME                    
         BE    DSPLY21              NO                                          
         LLC   RF,SPTPRGPT         POINTER TO PROGRAM NAME TABLE                
         MHI   RF,L'PROGTABL       FIND THIS PROG NAME                          
         LA    RE,PROGTABL(RF)                                                  
*                                                                               
         LLC   R1,FTPROGLN         GET FILTER NAME LENGTH                       
         BCTR  R1,0                                                             
         EX    R1,DSPLYCLC                                                      
         BE    DSPLY21                                                          
         B     DSPLY22                                                          
*                                                                               
DSPLYCLC CLC   FTPROGNM(0),0(RE)                                                
*                                                                               
DSPLY21  DS   0H                                                                
         BAS   RE,BLIN             BUILD DISPLAY LINE                           
*                                                                               
         LA    R2,NEXTLINE(R2)                                                  
DSPLY22  DS   0H                                                                
         LA    R3,SPTNEXT                                                       
         OC    0(3,R3),0(R3)       TEST END OF TABLE                            
         BZ    DSPLY24              YES                                         
*                                                                               
         LA    R0,TRALAST                                                       
         CR    R2,R0               TEST END OF SCREEN                           
         BL    DSPLY20                                                          
*                                                                               
         LA    R0,SPTABLE          SAVE NEXT TOP OF SCREEN ADDR                 
         LR    R1,R3                                                            
         SR    R1,R0                                                            
         ST    R1,NXTASVTB                                                      
         B     DSPLY26                                                          
DSPLY24  SR    R0,R0                                                            
         ST    R0,NXTASVTB         SAVE TOP OF SCREEN ADDR                      
*                                                                               
DSPLY26  AHI   R3,-L'SPTDATA                                                    
*                                                                               
         LA    R6,TRAINFO+53                                                    
         EDIT  (B1,SPTTBLN),(3,(R6))                                            
         MVC   TRAINFO+20(9),=C'NO CMLS ='                                      
         LA    R3,TRAINFO+31                                                    
         EDIT  (B2,UNCMLCT),(3,(R3)),ALIGN=LEFT,ZERO=NOBLANK                    
         OI    TRAINFOH+6,X'80'                                                 
*                                                                               
         TM    LOG,LOGPTTN+LOGCREF SEEDED SPTS FROM PTTN/CLEARED REF            
         BNZ   COMXIT               YES                                         
*                                                                               
         L     R1,=A(NEXTMSG)                                                   
         OC    UNCMLCT,UNCMLCT                                                  
         BNZ   *+8                                                              
         L     R1,=A(DONEMSGS)                                                  
         LA    R2,TRAOPTH                                                       
         TM    TABLESW,X'60'       ANY CML MSGS                                 
         BZ    COMXIT              NO                                           
         NI    TABLESW,X'FF'-X'40' ONLY GIVE DIFFERENT MSG ONCE                 
         B     COMXIT                                                           
         EJECT                                                                  
* RESTORE SPTABLE AND PROCESS ANY OPERATOR ENTRIES                              
*                                                                               
GEN10    BRAS  RE,RDTWA                                                         
         TM    LOG,LOGREQD         COMPLETED SPECIAL REQUEST?                   
         BO    GEN15                                                            
         TM    TABLESW,ACMLPRG     WAS ASSIGN CML TO PROGRAM DONE?              
         BZ    GEN20                                                            
         NI    TABLESW,X'FF'-ACMLPRG RESET                                      
         B     *+8                                                              
GEN15    NI    LOG,X'FF'-LOGREQD   RESET                                        
         LA    R3,SPTABLE                                                       
         B     DSPLY10                                                          
*                                                                               
* FIND ANY ENTRIES FOR CML *                                                    
*                                                                               
GEN20    NI    TABLESW,X'FF'-REASGN                                             
         LA    R2,TRACML1H                                                      
         LA    R3,SPTABLE                                                       
         A     R3,TOPASVTB         GET 1ST ENTRY AT TOP OF CURR                 
         USING SPTABLED,R3                                                      
         XC    LASTCML,LASTCML                                                  
         XC    LASTCML1,LASTCML1                                                
         XC    LASTCML2,LASTCML2                                                
*                                                                               
GEN30    CLI   5(R2),0             ANY ENTRY                                    
         BE    GEN40                NO                                          
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BZ    GEN30A              NO                                           
         MVC   LASTCML1,8(R2)                                                   
         OC    LASTCML1,SPACES                                                  
         B     GEN40                                                            
*                                                                               
* VALIDATE THIS CMML *                                                          
*                                                                               
GEN30A   MVC   WORK(3),SPTPROD                                                  
         MVC   WORK+4(1),SPTSLN                                                 
         MVC   WORK+5(3),SPTPROD2                                               
         MVC   WORK+9(1),SPTSLN2                                                
*                                                                               
         MVC   LASTCML,LASTCML1                                                 
*                                                                               
         MVI   PRDFLD,1                                                         
         BRAS  RE,VCML             VALIDATE AND FIND CML SEQ                    
         BNE   GEN31                                                            
*                                   1 CML COVERED BOTH PRODS                    
*                                                                               
         XC    SPTPREF,SPTPREF     ZERO PATTERN REFERENCE (WITHOUT SUB)         
*                                                                               
         CLI   SVCMLSOL,0                                                       
         BE    GEN48                                                            
         CLI   SVCMLSOL,C'P'       IF PIGGYBACK COMML, ERROR                    
         BE    PGSOERR                                                          
         B     GEN48                                                            
*                                                                               
GEN31    MVC   LASTCML1,LASTCML                                                 
*                                                                               
         OC    SPTPROD2,SPTPROD2   IS THERE A P/B PROD                          
         BNZ   GEN31C                                                           
*                                                                               
         CLI   SVCMLSOL,0                                                       
         BE    GEN31E                                                           
         CLI   SVCMLSOL,C'P'       IF PIGGYBACK COMML, ERROR                    
         BE    PGSOERR                                                          
         B     GEN31E                                                           
*                                                                               
GEN31C   CLI   SVCMLSOL,0                                                       
         BE    GEN31E                                                           
         CLI   SVCMLSOL,C'S'       IF SOLO COMML, ERROR                         
         BE    PGSOERR                                                          
*                                                                               
GEN31E   OC    SPTCMLSQ,SPTCMLSQ   WAS THIS ASSIGNED BEFORE                     
         BNZ   GEN32                                                            
*                                                                               
         OC    CMLSEQCD,CMLSEQCD   IS IT NOW UNASSIGNED                         
         BZ    GEN34                YES                                         
*                                                                               
         LH    R1,UNCMLCT                                                       
         BCTR  R1,0                                                             
         STH   R1,UNCMLCT                                                       
         B     GEN34                                                            
*                                                                               
GEN32    OC    CMLSEQCD,CMLSEQCD   IS IT NOW UNASSIGNED                         
         BNZ   GEN34                NO, NOW ASSIGNED                            
         LH    R1,UNCMLCT                                                       
         LA    R1,1(R1)                                                         
         STH   R1,UNCMLCT                                                       
*                                                                               
         CLC   SPTCMLSQ,SPTCMLS2   1 COMML COVERING BOTH                        
         BNE   GEN34                                                            
         XC    SPTCMLS2,SPTCMLS2                                                
         LH    R1,UNCMLCT                                                       
         LA    R1,1(R1)                                                         
         STH   R1,UNCMLCT                                                       
*                                                                               
GEN34    MVC   SPTCMLSQ,CMLSEQCD                                                
         XC    SPTPREF,SPTPREF     ZERO PATTERN REFERENCE (WITHOUT SUB)         
         OI    4(R2),X'20'         SET ON VALIDATED                             
         B     GEN40                                                            
*                                                                               
GEN36    OC    SPTCMLSQ,SPTCMLSQ   ANY COMML ASSIGNED                           
         BZ    GEN40                NO                                          
         MVC   DUB(3),SPTPROD                                                   
         MVC   CMLSEQCD,SPTCMLSQ                                                
         MVC   CMLSEQC2,SPTCMLS2                                                
         LA    R5,BPRD                                                          
         BAS   RE,FCML             FIND COMML CODE                              
         MVC   LASTCML1,CMLCD                                                   
         MVC   LASTSEQ1,SPTCMLSQ                                                
*                                                                               
GEN40    LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   5(R2),0             ANY ENTRY                                    
         BE    GEN50                NO                                          
         TM    4(R2),X'20'         ALREADY VALIDATED                            
         BZ    GEN40A              NO                                           
         MVC   LASTCML2,8(R2)                                                   
         OC    LASTCML2,SPACES                                                  
         B     GEN46               YES                                          
*                                                                               
* VALIDATE THIS COMML *                                                         
*                                                                               
GEN40A   OC    SPTPROD2,SPTPROD2   IS THERE A P/B PROD                          
         BZ    NOPBERR                                                          
         MVC   WORK(3),SPTPROD2                                                 
         MVC   WORK+4(1),SPTSLN2                                                
         XC    WORK+5(5),WORK+5                                                 
*                                                                               
         MVC   LASTCML,LASTCML2                                                 
*                                                                               
         MVI   PRDFLD,2                                                         
         BRAS  RE,VCML             VALIDATE AND FIND CML SEQ                    
*                                                                               
         XC    SPTPREF,SPTPREF     ZERO PATTERN REFERENCE (WITHOUT SUB)         
         MVC   LASTCML2,LASTCML                                                 
*                                                                               
         CLI   SVCMLSOL,0                                                       
         BE    GEN41                                                            
         CLI   SVCMLSOL,C'S'       IF SOLO COMML, ERROR                         
         BE    PGSOERR                                                          
*                                                                               
GEN41    OC    SPTCMLS2,SPTCMLS2   WAS THIS ASSIGNED BEFORE                     
         BNZ   GEN42                                                            
         LH    R1,UNCMLCT                                                       
         BCTR  R1,0                                                             
         STH   R1,UNCMLCT                                                       
         B     GEN44                                                            
*                                                                               
GEN42    OC    CMLSEQCD,CMLSEQCD   IS IT NOW UNASSIGNED                         
         BNZ   GEN44                                                            
         LH    R1,UNCMLCT                                                       
         LA    R1,1(R1)                                                         
         STH   R1,UNCMLCT                                                       
GEN44    MVC   SPTCMLS2,CMLSEQCD                                                
         OI    4(R2),X'20'         SET ON VALIDATED                             
         B     GEN50                                                            
*                                                                               
GEN46    OC    SPTCMLS2,SPTCMLS2   ANY COMML ASSIGNED                           
         BZ    GEN50                NO                                          
*                                                                               
         OC    SPTPROD2,SPTPROD2   ANY PRD2                                     
         BZ    SEEDERR             ERROR, (MUST USE SEED)                       
*                                                                               
         MVC   DUB(3),SPTPROD2                                                  
         MVC   CMLSEQCD,SPTCMLS2                                                
         MVC   CMLSEQC2,SPTCMLSQ                                                
*                                                                               
         LA    R1,DUB                                                           
         BRAS  RE,FPRD             GET 1 BYTE PRD                               
         MVC   DUB+4(1),SPTSLN2                                                 
*                                                                               
         LA    R5,DUB+3            PRD2                                         
         BAS   RE,FCML             FIND COMML CODE                              
         MVC   LASTCML2,CMLCD                                                   
         MVC   LASTSEQ2,SPTCMLS2                                                
         B     GEN50                                                            
*                                                                               
GEN48    LLC   R0,0(R2)            BYPASS FIELD ALREADY VALIDATED               
         AR    R2,R0                                                            
*                                                                               
GEN50    LA    R3,SPTNEXT                                                       
         OC    0(3,R3),0(R3)       TEST END OF LIST                             
         BZ    GEN60                YES                                         
         CLI   FTPROGLN,0          FILTERING ON PROGRAM NAME                    
         BE    GEN54                NO                                          
         LLC   RF,SPTPRGPT         POINTER TO PROGRAM NAME TABLE                
         MHI   RF,L'PROGTABL       FIND THIS PROG NAME                          
         LA    RE,PROGTABL(RF)                                                  
*                                                                               
         LLC   R0,FTPROGLN         GET FILTER NAME LENGTH                       
*NOP     EX    R0,GENCLC                                                        
         LR    R1,R0                                                            
         EX    R1,GENCLC                                                        
         BE    GEN54                                                            
         B     GEN50                                                            
*                                                                               
GENCLC   CLC   FTPROGNM(0),0(RE)                                                
*                                                                               
GEN54    DS   0H                                                                
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LA    R0,TRALAST          END OF SCREEN                                
         CR    R2,R0               AT END                                       
         BNL   GEN60                YES                                         
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     GEN30                                                            
*                                                                               
* TEST FOR ANY SPECIAL REQUESTS                                                 
*                                                                               
GEN60    TM    LOG,LOGTCML+LOGTREF                                              
         BNZ   TOPT10              YES, TEST REQUEST                            
         L     R3,NXTASVTB                                                      
         LA    R3,SPTABLE(R3)                                                   
         B     DSPLY10             PROCESS NEXT SCREEN,  IF ANY                 
*                                                                               
TOPT10   TM    LOG,LOGTCML+LOGTREF                                              
         BZ    TOPT30                                                           
         CLC   UNCMLCT,SPOTCT      ALL SPOTS NOT ASSIGNED CMLS                  
         BE    TOTERR              NO                                           
         BAS   RE,CLRSCRB          CLEAR SCREEN                                 
         GOTO1 =A(TCML),RR=SPTRRR                                               
         NI    LOG,X'FF'-LOGTCML-LOGTREF                                        
*                                                                               
TOPTXIT  OI    LOG,LOGREQD                                                      
         L     R1,=A(REQMS)                                                     
         LA    R2,TRAOPTH                                                       
         XC    TRAOPT,TRAOPT                                                    
         OI    TRAOPTH+6,X'80'                                                  
         OI    TRAINFOH+6,X'80'                                                 
         B     COMXIT                                                           
TOPT30   DC    H'0'                                                             
         EJECT                                                                  
* BUILD DISPLAY LINE *                                                          
*                                                                               
         USING DSPLINED,R2                                                      
         USING SPTABLED,R3                                                      
         DS    0H                                                               
BLIN     NTR1                                                                   
         LLC   R0,SPTEST                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSPEST,DUB                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,SPTLINE                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSPLIN,DUB                                                       
*                                                                               
         GOTO1 UNDAY,DMCB,SPTDAY,DSPDAY                                         
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 UNTIME,(R1),SPTTIME,WORK                                         
         MVC   DSPTIME,WORK                                                     
*                                                                               
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT SYSTEM                        
*                                                                               
         MVC   KEY+14(4),SPTDSKAD                                               
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
*                                                                               
         USING BUYRECD,R4                                                       
         MVC   DSPPRG,BDPROGRM                                                  
         DROP  R4                                                               
*                                                                               
         OC    BSTA,BSTA           TEST MARKET REQUEST                          
         BNZ   BLIN04                                                           
         MVC   DSPPRG,SPACES                                                    
         XC    DUB,DUB                                                          
         MVC   DUB+2(3),SPTSTA                                                  
         GOTO1 MSUNPK,DMCB,(X'80',DUB),WORK,WORK+4                              
         MVC   DSPPRG(8),WORK+4                                                 
         CLI   DSPPRG,C'0'                                                      
         BL    *+8                                                              
         MVI   DSPPRG+4,C'/'                                                    
*                                                                               
BLIN04   MVC   DSPDPT,SPTDPT                                                    
*                                                                               
         OC    SPTPROD2,SPTPROD2                                                
         BZ    BLIN06                                                           
         MVC   DSPPTR,SPTPROD2                                                  
*                                                                               
         LA    R1,DSPPTR+3                                                      
         CLI   DSPPTR+2,C' '                                                    
         BH    *+6                                                              
         BCTR  R1,0                                                             
         MVI   0(R1),C'-'                                                       
         LLC   R0,SPTSLN2                                                       
         EDIT  (R0),(3,1(R1)),ALIGN=LEFT                                        
*                                                                               
BLIN06   GOTO1 DATCON,DMCB,(2,SPTFTD),(4,DSPDATE)                               
*                                                                               
         LR    R4,R2                                                            
         OI    6(R2),X'80'         SET XMT                                      
         LLC   R0,0(R2)            BUMP TO CML FIELD                            
         AR    R2,R0                                                            
         OC    SPTCMLSQ,SPTCMLSQ                                                
         BZ    BLIN10                                                           
         MVC   CMLSEQCD,SPTCMLSQ                                                
         MVC   CMLSEQC2,SPTCMLS2                                                
*                                                                               
         MVC   DUB(3),SPTPROD                                                   
         LA    R5,BPRD                                                          
         BAS   RE,FCML             FIND COMML CODE                              
*                                                                               
         GOTO1 DATCON,DMCB,(2,SPTFTD),(3,FULL)                                  
         OI    COMML,CMLAPRSW                                                   
*                                                                               
         BRAS  RE,VCMLAPR          CHK CML APPROVALS IF ANY                     
         NI    COMML,X'FF'-CMLAPRSW                                             
*                                                                               
         MVC   8(12,R2),CMLCD                                                   
         B     *+10                                                             
BLIN10   MVC   8(12,R2),=CL12'TBA '                                             
         OI    4(R2),X'20'         SET ON VALIDATED                             
         OI    6(R2),X'80'             AND XMT                                  
*                                                                               
         LLC   R0,0(R2)            BUMP TO CML FIELD                            
         AR    R2,R0                                                            
         OC    SPTCMLS2,SPTCMLS2                                                
         BZ    BLIN20                                                           
         OC    SPTPROD2,SPTPROD2   ANY PRD2                                     
         BZ    SEEDERR             ERROR (USE SPOT SEED)                        
*                                                                               
         MVC   CMLSEQCD,SPTCMLS2                                                
         MVC   CMLSEQC2,SPTCMLS2                                                
         MVC   DUB(3),SPTPROD2                                                  
*                                                                               
         LA    R1,DUB                                                           
         BRAS  RE,FPRD             GET 1 BYTE PRD                               
         MVC   DUB+4(1),SPTSLN2                                                 
*                                                                               
         LA    R5,DUB+3                                                         
         BAS   RE,FCML             FIND COMML CODE                              
*                                                                               
         OI    COMML,CMLAPRSW                                                   
         BRAS  RE,VCMLAPR          CHK CML APPROVALS IF ANY                     
         NI    COMML,X'FF'-CMLAPRSW                                             
*                                                                               
         MVC   8(12,R2),CMLCD                                                   
         OI    4(R2),X'20'         SET ON VALIDATED                             
BLIN20   DS    0H                                                               
         OI    6(R2),X'80'             AND XMT                                  
         B     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
*=======================================================                        
* SUBROUTINE FINDS COMMERCIAL CODE FROM COMML SEQ                               
* ON ENTRY R5 POINTS TO PRD(1)/SLN(1)                                           
*=======================================================                        
                                                                                
FCML     NTR1                                                                   
*                                                                               
         NI    COMML,X'FF'-FOUNDSW   INIT COMMERCIAL SWITCH                     
*                                                                               
         L     R2,AIO3                                                          
         LA    R3,4000                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R2,RE                                                            
*                                                                               
         L     R2,AIO3                                                          
         LA    R3,4000(R2)                                                      
         USING CMLTBLED,R2                                                      
*                                                                               
FCML10   OC    CMLTDATA,CMLTDATA                                                
         BZ    FCML30                                                           
         CLC   CMLSEQCD,CMLTSQNO                                                
         BE    FCML20                                                           
         LA    R2,CMLTNEXT                                                      
         CR    R2,R3                                                            
         BL    FCML10                                                           
         DC    H'0'                                                             
*                                                                               
FCML20   MVC   CMLCD,CMLTCML                                                    
         MVC   SVCMLSOL,CMLTSOLO                                                
         OI    COMML,FOUNDSW                                                    
*                                                                               
FCML30   XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLPID,=X'0AA1'                                                  
         MVC   CMLPAM(3),BAGYMD AND BCLT                                        
         MVC   CMLPSEQ+1(2),CMLSEQCD                                            
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
*        BE    *+6                                                              
*        DC    H'0'                                                             
         BNE   FCMLERR                                                          
*                                                                               
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVC   CMLCD(8),CMLKCML                                                 
         MVC   CMLCD+8(4),SPACES                                                
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,X'A0'                                                     
         BRAS  RE,GETEL                                                         
         BNE   FCML32                                                           
         USING CMLADIEL,R6                                                      
         MVC   CMLCD,CMLADID       SET CODE TO UNPACKED ADID                    
*                                                                               
FCML32   LR    R6,R4                                                            
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SVCMLRCL,CMLRCL     SAVE CML RECALL DATE                         
*                                                                               
         TM    COMML,FOUNDSW       IF CML IS ALREADY IN THE TABLE               
         BO    FCML50               THEN SEE THAT THE LENGTH IS VALID           
*                                                                               
         USING CMLDTAEL,R6                                                      
         MVC   CMLTSQNO,CMLSEQCD                                                
         MVC   CMLTCML,CMLCD                                                    
         MVC   CMLTLEN,CMLSLN                                                   
         MVC   CMLTPROD,DUB        SAVE ASSOCIATED PRODUCT                      
*                                                                               
         GOTO1 DATCON,DMCB,(3,CMLRLSE),(2,CMLTSTDT)                             
         GOTO1 (RF),(R1),(3,CMLRCL),(2,CMLTRCDT)                                
*                                                                               
         MVC   CMLTSOLO,CMLSOLO                                                 
         CLI   CMLTSOLO,C'S'                                                    
         BE    FCML40                                                           
         CLI   CMLTSOLO,C'P'                                                    
         BE    FCML40                                                           
         MVI   CMLTSOLO,0                                                       
*                                                                               
FCML40   MVC   SVCMLSOL,CMLTSOLO                                                
*                                                                               
FCML50   CLI   SVBPRD2,0          IS THERE A SECOND PROD                        
         BE    FCML52              NO                                           
         CLI   SVBSLN2,0          IS THERE A SECOND LEN                         
         BE    FCML54              NO, 1ST MUST COVER BOTH                      
*                                                                               
FCML52   CLC   1(1,R5),CMLSLN      IS THIS CML SAME SPOT LENGTH                 
         BE    FCML56               YES                                         
         CLC   CMLSEQCD,CMLSEQC2   SEE IF ONE CML COVERS BOTH                   
         BNE   FCMLERR              NO,ERROR                                    
         LLC   R0,1(R5)            SPOT LENGTH                                  
         AR    R0,R0               DOUBLE IT UP                                 
         CLM   R0,1,CMLSLN         SAME LENGTH ?                                
         BE    FCML56               YES                                         
         B     FCMLERR                                                          
*                                                                               
FCML54   LLC   R0,SVBSLN           GET TOTAL LEN                                
         LLC   R1,SVBSLN2                                                       
         AR    R1,R0                                                            
         CLM   R1,1,CMLSLN         THIS EQUAL BOTH                              
         BNE   FCMLERR                                                          
*                                                                               
         DROP  R6                                                               
*                                                                               
FCML56   DS    0H                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLPRDEL,R6                                                      
*                                                                               
         CLI   CMLPRDS,X'FF'        ALL PRDS                                    
         BE    EXIT                                                             
*                                                                               
         LLC   R0,1(R6)            ELEM LEN                                     
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
         LA    R1,2(R6)            FIRST PRD                                    
FCML60   CLC   0(1,R5),0(R1)       DO PRDS MATCH (BUY/CML)                      
         BE    EXIT                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,FCML60                                                        
FCMLERR  MVC   CMLCD,=CL12'REASSIGN'                                            
         OI    TABLESW,REASGN      REASSIGN CML                                 
         B     EXIT                                                             
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
*===============================================================                
*                              PROGRAM= FOR VK RTN                              
* SUBROUTINE VALIDATES OPTIONS CLEAR, SAVE, TOTAL, PATTERN                      
*                              CPAT=#, CLEAR=ALL                                
*===============================================================                
                                                                                
VOPT     NTR1                                                                   
*                                                                               
         NI    TRAMEDH+4,X'FF'-X'20'   SET OFF VALIDATED IF ERROR               
*                                                                               
         MVI   BYTE,0                                                           
*                                                                               
         NI    TABLESW,X'FF'-ACMLPRG  INIT ASGN CML TO A PROG FLAG              
*                                                                               
         XC    SVPROGNM,SVPROGNM   INIT PROGRAM NAME                            
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VOPTX                NO                                          
         CLI   8(R2),C'?'          HELP                                         
         BE    VOPTHLP              YES                                         
         CLI   5(R2),4                                                          
         BNH   VOPT002                                                          
         LA    R1,3                                                             
         B     VOPT004                                                          
VOPT002  LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
VOPT004  EX    R1,VOPTCLCH         HELP                                         
         BE    VOPTHLP                                                          
*                                                                               
         LA    R4,BLOCK+240        ADDRESS OF FIRST BLOCK                       
         GOTO1 SCANNER,DMCB,(18,TRAOPTH),(7,(R4))                               
*                                                                               
         CLI   DMCB+4,1            SEE IF SCANNER FOUND ANYTHING                
         BNE   SCANERR              NO, ERROR                                   
*                                                                               
VOPT010  LLC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,VOPTCLCS         CLEAR PROGRAM NAME                           
         BNE   VOPT010A                                                         
         MVI   FTPROGLN,0                                                       
         XC    FTPROGNM,FTPROGNM                                                
         B     VOPT010C                                                         
*                                                                               
VOPT010A DS    0H                                                               
         EX    R1,VOPTCLCR         FILTER ON PROGRAM NAME                       
         BNE   VOPT011                                                          
*                                                                               
*                                                                               
         CLI   1(R4),0             MUST BE SECOND ENTRY                         
         BE    MISSERR                                                          
         CLI   1(R4),18            MAX LENGTH                                   
         BH    PRGSIZER                                                         
         MVC   FTPROGNM,22(R4)     SAVE REQUESTED PROG NAME                     
         MVC   FTPROGLN,1(R4)      SAVE LENGTH                                  
*                                                                               
VOPT010C DS    0H                                                               
         XC    TRAOPT,TRAOPT                                                    
*                                                                               
         OI    TRAOPTH+6,X'80'                                                  
*                                                                               
         OI    TRAMEDH+4,X'20'     SET ON VALIDATED AGAIN                       
*                                                                               
         LA    R3,SPTABLE                                                       
*                                                                               
         XIT1  REGS=(R3)                                                        
*                                                                               
VOPT011  DS    0H                                                               
         EX    R1,VOPTCLCA         CLEAR (ALL ASSIGNMENTS)                      
         BNE   VOPT020                                                          
*                                                                               
         TM    TABLESW,TABLEBL     HAS TABLE BEEN BUILT?                        
         BZ    TABLDER              NO                                          
*                                                                               
* CLEAR ALL COMMERCIAL ASSIGNMENTS *                                            
*                                                                               
         CLI   1(R4),0                                                          
         BE    *+14                                                             
         CLC   =C'ALL',22(R4)                                                   
         BNE   VOPTHLP                                                          
*                                                                               
         LA    R3,SPTABLE                                                       
         USING SPTABLED,R3                                                      
         LH    R0,SPOTCT                                                        
VOPT012  CLI   SVTZPR02,C'Y'       BYPASS PREVIOUSLY ASSIGNED CMLS              
         BNE   VOPT013                                                          
         CLI   22(R4),C'A'         CLEAR 'ALL' REQUESTED                        
         BE    VOPT013                                                          
         TM    SPTFLAG,SPTPRVAS    WAS THIS PREVIOUSLY ASSIGNED                 
         BO    VOPT018              YES                                         
VOPT013  OC    SPTCMLSQ,SPTCMLSQ                                                
         BZ    VOPT014                                                          
         LH    R1,UNCMLCT                                                       
         LA    R1,1(R1)                                                         
         STH   R1,UNCMLCT                                                       
*                                                                               
VOPT014  OC    SPTCMLS2,SPTCMLS2                                                
         BZ    VOPT016                                                          
         LH    R1,UNCMLCT                                                       
         LA    R1,1(R1)                                                         
         STH   R1,UNCMLCT                                                       
VOPT016  XC    SPTCMLSQ(4),SPTCMLSQ                                             
         XC    SPTPREF,SPTPREF     ZERO PATTERN REFERENCE (WITHOUT SUB)         
*                                                                               
VOPT018  LA    R3,SPTNEXT                                                       
         BCT   R0,VOPT012                                                       
*                                                                               
         OI    LOG,LOGCLR          SET CLEAR ASSIGNS REQUESTED                  
         B     VOPTX                                                            
*                                                                               
VOPT020  EX    R1,VOPTCLCB         SAVE - SAVE ALL UPDATED SPOTS                
         BNE   VOPT030                                                          
*                                                                               
         TM    TABLESW,TABLEBL     HAS TABLE BEEN BUILT?                        
         BZ    TABLDER              NO                                          
*                                                                               
*                                                                               
         TM    SOXSW,SOXOKFLG      IF DDS, AND FACTEST, OKAY TO GO              
         BO    VOPT24                                                           
*                                                                               
         TM    SOXSW,SOXERFLG      IF RD ONLY/RD ONLY MODE/WRONG ADV            
         BZ    VOPT24                                                           
*                                                                               
         GOTO1 VSOXERR             FORCE ERROR MSG                              
*                                                                               
VOPT24   DS    0H                                                               
         OI    LOG,LOGSAVE         SET SAVE REQUESTED                           
         B     VOPTX                                                            
*                                                                               
VOPT030  EX    R1,VOPTCLCC         TOTAL - TOTAL ASSIGNED EACH COMML            
         BNE   VOPT034                                                          
*                                                                               
         TM    TABLESW,TABLEBL     HAS TABLE BEEN BUILT?                        
         BZ    TABLDER              NO                                          
*                                                                               
         OI    LOG,LOGTCML         TOTAL REQUEST                                
         B     VOPTX                                                            
*                                                                               
VOPT034  EX    R1,VOPTCLCE         TREF - TOTAL ASSIGNED EACH PATTN             
         BNE   VOPT040                                                          
*                                                                               
         TM    TABLESW,TABLEBL     HAS TABLE BEEN BUILT?                        
         BZ    TABLDER              NO                                          
*                                                                               
         OI    LOG,LOGTREF         TOTAL REQUEST                                
         B     VOPTX                                                            
*                                                                               
VOPT040  EX    R1,VOPTCLCP         USE PATTERNS TO ASSIGN COMMLS                
         BNE   VOPT050                                                          
*                                                                               
         TM    TABLESW,TABLEBL     HAS TABLE BEEN BUILT?                        
         BZ    TABLDER              NO                                          
*                                                                               
         BRAS  RE,GETPTN                                                        
*                                                                               
         BRAS  RE,SVTWA                                                         
*                                                                               
         XC    TRAOPT,TRAOPT                                                    
         OI    TRAOPTH+6,X'80'                                                  
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'PATMSG),PATMSG                                         
* COUNT NUMBER OF SPOTS IN TABLE                                                
         LA    R3,SPTABLE                                                       
         SR    R0,R0                                                            
*                                                                               
VOPT042  OC    0(3,R3),0(R3)                                                    
         BZ    VOPT044                                                          
         LA    R3,SPTNEXT                                                       
         BCT   R0,VOPT042                                                       
*                                                                               
VOPT044  LPR   R0,R0                                                            
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CONHEAD+9(3),DUB                                                 
         OI    CONHEADH+6,X'80'    FORCE WRITE                                  
*                                                                               
         OI    LOG,X'40'           SET ON SEEDED SPOT FROM PATTERNS             
*                                                                               
VOPTX    OI    4(R2),X'20'         SET ON VALIDATED                             
*                                                                               
         OI    TRAMEDH+4,X'20'     SET ON VALIDATED AGAIN                       
*                                                                               
         B     EXIT                                                             
*                                                                               
VOPT050  EX    R1,VOPTCLCD         CLEAR BY PATTERN REF #                       
         BNE   VOPT060                                                          
*                                                                               
         CLI   1(R4),0             MUST BE SECOND ENTRY                         
         BE    CLRPERR                                                          
         TM    3(R4),X'80'         AND MUST BE NUMERIC                          
         BZ    CLRPERR                                                          
*                                                                               
         TM    TABLESW,TABLEBL     HAS TABLE BEEN BUILT?                        
         BZ    TABLDER              NO                                          
*                                                                               
         SR    RF,RF                                                            
         LA    R3,SPTABLE                                                       
         USING SPTABLED,R3                                                      
         LH    R0,SPOTCT                                                        
VOPT052  CLC   SPTPREF,10(R4)                                                   
         BNE   VOPT058                                                          
         BCTR  RF,0                COUNT SPOTS CLEARED                          
         OC    SPTCMLSQ,SPTCMLSQ                                                
         BZ    VOPT054                                                          
         LH    R1,UNCMLCT                                                       
         LA    R1,1(R1)                                                         
         STH   R1,UNCMLCT                                                       
         B     VOPT056                                                          
*                                                                               
VOPT054  OC    SPTCMLS2,SPTCMLS2                                                
         BZ    VOPT056                                                          
         LH    R1,UNCMLCT                                                       
         LA    R1,1(R1)                                                         
         STH   R1,UNCMLCT                                                       
VOPT056  XC    SPTCMLSQ(4),SPTCMLSQ                                             
         XC    SPTPREF,SPTPREF                                                  
*                                                                               
VOPT058  LA    R3,SPTNEXT                                                       
         BCT   R0,VOPT052                                                       
         DROP  R3                                                               
*                                                                               
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(8),=C'* NOTE *'                                          
*                                                                               
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'         CLEAR NEGATIVE SIGN                          
         UNPK  CONHEAD+9(3),DUB                                                 
         MVC   CONHEAD+13(25),=C'SPOTS CLEARED FOR PAT REF'                     
         MVC   CONHEAD+39(4),22(R4)                                             
         LA    R2,TRAOPTH                                                       
         XC    TRAOPT,TRAOPT                                                    
         OI    TRAOPTH+6,X'80'                                                  
*                                                                               
         OI    LOG,LOGREQD+LOGCREF  CLEARED BYREF/RETURN FROM PREV REQ          
*                                                                               
         B     VOPTX                                                            
*                                                                               
* ASSIGN COMMERCIAL TO A PROGRAM ?                                              
*                                                                               
VOPT060  DS    0H                                                               
         CLI   0(R4),8             TEST FOR CMML OR ADID    L                   
         BL    VOPTHLP                                                          
*                                                                               
         TM    TABLESW,TABLEBL     HAS TABLE BEEN BUILT?                        
         BZ    TABLDER              NO                                          
*                                                                               
         TM    TABLESW,ACMLPRG     WAS ASGN CML REQUESTED BEFORE                
         BO    VOPTERR              YES, DUPLICATE REQUEST                      
*                                                                               
         MVI   BYTE,1                                                           
         MVC   SVACML,12(R4)       SAVE CML TO BE ASSIGNED                      
*                                                                               
         CLI   0(R4),9                                                          
         BL    VOPT61                                                           
         CLI   0(R4),12                                                         
         BH    VOPTERR                                                          
         GOTO1 VTRPACK,DMCB,(C'P',12(R4)),SVACML                                
         BNE   VOPTERR                                                          
*                                                                               
VOPT61   BAS   RE,FCMLQ            FIND CML SEQ #                               
*                                                                               
* GET PROGRAM ADJ CODE FOR THIS PROGRAM NAME                                    
*                                                                               
         LLC   R1,1(R4)            LEN OF 2ND HALF OF DIVIDED FIELD             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVPROGNM(0),22(R4)  SAVE PROGRAM NAME                            
         MVC   SVLEN,1(R4)         SAVE INPUT LENGTH                            
*                                                                               
         LA    R5,PROGTABL         PROGRAM NAME TABLE                           
         LA    RF,PROGNUM          NUMBER OF ENTRIES                            
*                                                                               
         LLC   R1,SVLEN            LENGTH OF PROG NAME                          
         BCTR  R1,0                                                             
VOPT065  EX    R1,VOPTCKPG                                                      
         BE    VOPT070                                                          
         LA    R5,L'PROGTABL(R5)    BUMP TO THE NEXT ENTRY                      
         OC    0(L'PROGTABL,R5),0(R5)  EMPTY ?                                  
         BZ    *+8                                                              
         BCT   RF,VOPT065                                                       
         B     VOPTPERR             PROGRAM ERROR                               
*                                                                               
VOPTCKPG CLC   SVPROGNM(0),0(R5)    IS THIS THE PROGRAM                         
*                                                                               
VOPT070  LA    R3,SPTABLE                                                       
         USING SPTABLED,R3                                                      
*                                                                               
         XC    ASGNCT,ASGNCT       CLEAR CML ASSIGNED COUNT                     
*                                                                               
VOPT075  ICM   R1,15,SPTPRGPT                                                   
         CR    R5,R1                                                            
         BE    VOPT080                                                          
         LA    R3,SPTNEXT                                                       
         OC    0(3,R3),0(R3)       END OF TABLE?                                
         BNZ   VOPT075                                                          
         B     VOPT120                                                          
*                                                                               
VOPT080  DS    0H                                                               
         XC    CMLSEQCD,CMLSEQCD                                                
         MVC   SVPRGADJ,SPTPROGT   SAVE PROG ADJ CODE                           
*                                                                               
         MVC   WORK(3),SPTPROD                                                  
         MVC   WORK+4(1),SPTSLN                                                 
         MVC   WORK+5(3),SPTPROD2                                               
         MVC   WORK+9(1),SPTSLN2                                                
*                                                                               
         MVI   PRDFLD,1                                                         
*                                                                               
         BRAS  RE,VCML             VALIDATE AND FIND CML SEQ                    
*                                                                               
         CLI   SVTZPR02,C'Y'       BYPASS PREVIOUSLY ASSIGNED CMLS              
         BNE   *+18                                                             
         OC    SPTCMLSQ,SPTCMLSQ   WAS CML ASGND BEFORE                         
         BNZ   VOPT090              YES, BYPASS                                 
         B     *+14                                                             
*                                                                               
         OC    SPTCMLSQ,SPTCMLSQ   WAS THIS ASSIGNED BEFORE                     
         BNZ   VOPT084              YES                                         
         OC    CMLSEQCD,CMLSEQCD   IS IT ASGND NOW                              
         BZ    VOPT084                                                          
*                                                                               
         LH    R1,UNCMLCT          DECREMENT UNASSIGNED CML COUNT               
         BCTR  R1,0                                                             
         STH   R1,UNCMLCT                                                       
*                                                                               
VOPT084  OC    CMLSEQCD,CMLSEQCD   NOT ASSIGNED?                                
         BZ    VOPT090              NO                                          
         MVC   SPTCMLSQ,CMLSEQCD                                                
         XC    SPTPREF,SPTPREF     ZERO PATTERN REFERENCE                       
*                                                                               
         LH    R1,ASGNCT           INCREMENT CML ASGN COUNT                     
         LA    R1,1(R1)                                                         
         STH   R1,ASGNCT                                                        
*                                                                               
VOPT090  OC    SPTPROD2,SPTPROD2   IF THERE IS NO P/B PROD                      
         BNZ   *+18                                                             
         OC    CMLSEQCD,CMLSEQCD   AND PRD1 IS NO MATCH                         
         BZ    BDPRDER1            THEN ERROR                                   
         B     VOPT110                                                          
*                                                                               
         MVC   WORK(3),SPTPROD2                                                 
         MVC   WORK+4(1),SPTSLN2                                                
         XC    WORK+5(5),WORK+5                                                 
*                                                                               
         MVI   PRDFLD,2                                                         
*                                                                               
         BRAS  RE,VCML             VALIDATE AND FIND CML SEQ                    
*                                                                               
         CLI   SVTZPR02,C'Y'       BYPASS PREVIOUSLY ASSIGNED CMLS              
         BNE   *+18                                                             
         OC    SPTCMLS2,SPTCMLS2   WAS CML ASGND BEFORE                         
         BNZ   VOPT110              YES, BYPASS                                 
         B     *+14                                                             
*                                                                               
         OC    SPTCMLS2,SPTCMLS2   WAS THIS ASSIGNED BEFORE                     
         BNZ   VOPT094              YES                                         
         CLI   PRDFLD,0            DID CML PRD MATCH?                           
         BE    VOPT094                                                          
*                                                                               
         LH    R1,UNCMLCT          DECREMENT UNASSIGNED CML COUNT               
         BCTR  R1,0                                                             
         STH   R1,UNCMLCT                                                       
*                                                                               
VOPT094  DS    0H                                                               
         CLI   PRDFLD,0            NO MATCH FOR FIELD 2?                        
         BE    VOPT110              NO, NO MATCH                                
         MVC   SPTCMLS2,CMLSEQCD                                                
         XC    SPTPREF,SPTPREF     ZERO PATTERN REFERENCE                       
*                                                                               
         LH    R1,ASGNCT           INCREMENT CML ASGN COUNT                     
         LA    R1,1(R1)                                                         
         STH   R1,ASGNCT                                                        
*                                                                               
VOPT110  LA    R3,SPTNEXT          GET NEXT SPOT                                
         OC    0(3,R3),0(R3)       END OF TABLE?                                
         BNZ   VOPT075                                                          
*                                                                               
VOPT120  DS    0H                                                               
         OC    ASGNCT,ASGNCT                                                    
         BZ    NOASGERR            ERROR, NOTHING WAS ASSIGNED                  
*                                                                               
         OI    TABLESW,ACMLPRG     ASSIGN CML TO A PROGRAM                      
         LA    R3,SPTABLE                                                       
         B     DSPLY10                                                          
*                                                                               
         DROP  R3                                                               
*                                                                               
VOPTCLCA CLC   8(0,R2),=C'CLEAR '                                               
VOPTCLCB CLC   8(0,R2),=C'SAVE '                                                
VOPTCLCC CLC   8(0,R2),=C'TOTAL '                                               
VOPTCLCD CLC   8(0,R2),=C'CREF '                                                
VOPTCLCE CLC   8(0,R2),=C'TREF '                                                
VOPTCLCP CLC   8(0,R2),=C'PATTERN '                                             
VOPTCLCR CLC   8(0,R2),=C'PROGRAM '                                             
VOPTCLCS CLC   8(0,R2),=C'CPROGRAM '                                            
VOPTCLCH CLC   8(0,R2),=C'HELP '                                                
PATMSG   DC    C'* NOTE * NNN SPOTS COVERED BY PATTERN COMMERCIALS *'           
         EJECT                                                                  
* READ CML RECORD TO GET CML SEQ NUMBER                                         
*                                                                               
FCMLQ    DS    0H                                                               
         NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CMLKEY,R6                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD AND BCLT                                        
         MVC   CMLKCML,SVACML                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         BNE   NOCMLER                                                          
*                                                                               
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         LR    R6,R4                                                            
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         MVC   SVCMLSEQ,CMLSEQ+1   SAVE COMML SEQ #                             
         XIT1                                                                   
*                                                                               
NOCMLER  MVI   ERROR,INVCOMM                                                    
         BRAS  RE,SVTWA                                                         
         NI    TRAMEDH+4,X'FF'-X'20' FORCE VALIDATION                           
         GOTO1 ERREX                                                            
         EJECT                                                                  
*===================================================                            
* FIND 1 CHAR BPRD CODE FROM 3 CHAR CODE                                        
* R1 POINTS TO 3 CHAR PRD CODE                                                  
*===================================================                            
                                                                                
FPRD     L     RF,ASVCLIST                                                      
*                                                                               
FPRD10   CLC   0(3,R1),0(RF)                                                    
         JE    FPRD20                                                           
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         JH    FPRD10                                                           
         DC    H'0'                                                             
FPRD20   MVC   3(1,R1),3(RF)                                                    
         BR    RE                                                               
         EJECT                                                                  
*=========================================================                      
* UPDATE BUY RECS FROM SPOT TABLE                                               
*=========================================================                      
                                                                                
UPDATE   NTR1                                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,TODAYP)                                     
                                                                                
* UPDATE BUY RECS FROM SPTABLE *                                                
                                                                                
         TM    TABLESW,X'20'+REASGN PREV BAD CML STILL HERE                     
         BNZ   NOSAVER              YES                                         
*                                                                               
         LA    R3,SPTABLE                                                       
         LH    R4,SPOTCT                                                        
         LR    R0,R4                                                            
         LR    R1,R3                                                            
*                                                                               
         USING SPTABLED,R1                                                      
UPD10    MVC   SPTSDT,SPTFTD      MOVE FTD FOR SORT                             
         LA    R1,L'SPTDATA(R1)                                                 
         BCT   R0,UPD10                                                         
         DROP  R1                                                               
         USING SPTABLED,R3                                                      
                                                                                
* SORT ON DISK ADDRESS, DATE, AND SPOT NUMBER                                   
                                                                                
         GOTO1 XSORT,DMCB,SPTABLE,(R4),L'SPTDATA,L'SPTUSORT,           C        
               SPTDSKAD-SPTDATA                                                 
*                                                                               
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT SYSTEM                        
*                                                                               
UPD20    MVC   KEY+14(4),SPTDSKAD                                               
         L     R6,AIO1                                                          
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),SYSFIL,KEY+14,(R6),     C        
               DMWORK                                                           
         NI    TABLESW,X'FF'-TBLEUPD UPDATE BUY REC NEEDED SW OFF               
*                                                                               
UPD30    XC    WORK,WORK                                                        
         MVC   WORK(3),SPTPROD                                                  
         MVC   WORK+4(1),SPTSLN                                                 
         MVC   WORK+5(3),SPTPROD2                                               
         MVC   WORK+9(1),SPTSLN2                                                
         LA    R1,WORK                                                          
         BRAS  RE,FPRD                                                          
         OC    WORK+5(3),WORK+5                                                 
         BZ    UPD34                                                            
         LA    R1,WORK+5                                                        
         BRAS  RE,FPRD                                                          
UPD34    MVC   WORK+10(2),SPTFTD                                                
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         L     R6,AIO1                                                          
         AH    R6,DATADISP                                                      
         XC    ELDATE,ELDATE                                                    
         EJECT                                                                  
* UPDATE ALL ELEMS FOR THIS BUY *                                               
*                                                                               
UPD40    BRAS  RE,BUYEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    6(R6),X'C0'                                                      
         BNZ   UPD40                                                            
         CLI   1(R6),10            UNALLOCATED                                  
         BNH   UPD40                                                            
         CLC   2(2,R6),ELDATE                                                   
         BE    UPD42                                                            
         MVI   SPOTNUMB,0                                                       
         MVC   ELDATE,2(R6)                                                     
UPD42    LLC   RE,SPOTNUMB                                                      
         LA    RE,1(RE)                                                         
         STC   RE,SPOTNUMB                                                      
         OC    WORK+5(3),WORK+5    IS THERE A PIGGYBACK PROD                    
         BZ    UPD44                                                            
         CLI   1(R6),18                                                         
         BL    UPD40                                                            
         CLC   WORK+3(2),10(R6)    SAME BPRD/SLN                                
         BNE   *+18                                                             
         CLC   WORK+8(2),14(R6)    SAME BPRD/SLN                                
         BNE   UPD40                                                            
         B     UPD45                                                            
*                                                                               
* CK IF PRODS WERE OUT OF ORDER *                                               
*                                                                               
         CLC   WORK+8(2),10(R6)    SAME BPRD/SLN                                
         BNE   UPD40                                                            
         CLC   WORK+3(2),14(R6)    SAME BPRD/SLN                                
         BNE   UPD40                                                            
         MVC   DUB(5),WORK                                                      
         MVC   WORK(5),WORK+5                                                   
         MVC   WORK+5(5),DUB                                                    
         MVC   SPTPROD,WORK                                                     
         MVC   SPTSLN,WORK+4                                                    
         MVC   SPTPROD2,WORK+5                                                  
         MVC   SPTSLN2,WORK+9                                                   
         MVC   DUB(2),SPTCMLSQ                                                  
         MVC   SPTCMLSQ,SPTCMLS2                                                
         MVC   SPTCMLS2,DUB                                                     
         B     UPD45                                                            
*                                                                               
UPD44    CLI   1(R6),14                                                         
         BH    UPD40                                                            
         CLC   WORK+3(2),10(R6)    SAME BPRD/SLN                                
         BNE   UPD40                                                            
UPD45    CLC   2(2,R6),WORK+10     SAME DATE                                    
         BNE   UPD40               NO                                           
         CLC   SPTSPTN,SPOTNUMB    SAME SPOT NUMBER                             
         BNE   UPD40               NO                                           
*                                                                               
         LR    R4,R6                                                            
*                                                                               
UPD46    LLC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),0             THIS END OF RECORD                           
         BE    UPD50                YES, CREATE NEW ELEM                        
*                                                                               
         CLI   0(R4),13            THIS ANOTHER BUY                             
         BNH   UPD50                YES                                         
*                                                                               
         CLI   0(R4),X'18'         THIS A CML ASSIGN ELEMENT                    
         BE    UPD48                YES, UPDATE IT IF NEEED                     
*                                                                               
         CLI   0(R4),X'1F'         THIS PAST SPOTS                              
         BH    UPD50                YES, CREATE NEW ELEM                        
         B     UPD46                                                            
*                                                                               
UPD48    CLC   TRACCSQ-TRACID(4,R4),SPTCMLSQ  SAME COMML(S)S                    
         BNE   UPD49                                                            
         CLC   SPTPREF,TRACREF-TRACID(R4)     SAME PATTERN REF #                
         BE    UPD60                                                            
*                                                                               
         TM    TRACREF-TRACID(R4),X'80' PRINTED ON INSTR                        
         BZ    UPD49                                                            
         MVC   HALF,TRACREF-TRACID(R4)   SAVE PATTERN REF #                     
         NI    HALF,X'7F'                                                       
         CLC   HALF,SPTPREF              SAME PATTERN REF #                     
         BE    UPD60                                                            
*                                                                               
UPD49    MVC   TRACCSQ-TRACID(4,R4),SPTCMLSQ                                    
*                                                                               
* NOTE THAT PRINTED FLAG IS TURNED OFF *                                        
*                                                                               
         MVC   TRACREF-TRACID(,R4),SPTPREF        SAVE PATTERN REF #            
         OI    TABLESW,TBLEUPD     SET UPDATE BUY REC NEEDED SW                 
*                                                                               
         OC    2(6,R4),2(R4)       EMPTY ELEM ?                                 
         BNZ   UPD60                                                            
*                                                                               
* DELETE EMPTY ELEM                                                             
         GOTO1 VRECUP,DMCB,AIO,(R4)   DELETE ELEMENT                            
         B     UPD60                                                            
*                                                                               
UPD50    DS   0H                                                                
         XC    ELEM,ELEM                                                        
         LA    R1,ELEM                                                          
         MVI   TRACID-TRACID(R1),X'18'    ELCODE                                
         MVI   TRACLN-TRACID(R1),TRACLEN  LENGTH                                
         MVC   TRACCSQ-TRACID(4,R1),SPTCMLSQ                                    
         MVC   TRACREF-TRACID(,R1),SPTPREF        SAVE PATERN REF #             
*                                                                               
         OC    2(6,R1),2(R1)       EMPTY ELEM ?                                 
         BZ    UPD60                YES, BYPASS                                 
*                                                                               
         L     R1,AIO1                                                          
         SR    RE,RE                                                            
         ICM   RE,3,13(R1)         RECORD LENGTH                                
         LLC   RF,ELEM+1           ELEMENT LENGTH                               
         AR    RE,RF                                                            
*        CH    RE,=H'1975'         OVER MAX LENGTH                              
         CHI   RE,3972             OVER MAX LENGTH                              
         BH    BUYSIZER            YES                                          
         L     R6,AIO1                                                          
         AH    R6,DATADISP                                                      
         GOTO1 VRECUP,DMCB,AIO1,ELEM,(R4)                                       
*                                                                               
         OI    TABLESW,TBLEUPD     SET UPDATE BUY REC NEEDED SW                 
*                                                                               
UPD60    LA    R3,SPTNEXT                                                       
         OC    0(3,R3),0(R3)       AT END OF SPOT TABLE                         
         BZ    UPD70                YES                                         
         CLC   SPTDSKAD,KEY+14     SAME REC                                     
         BE    UPD30                YES                                         
*                                                                               
UPD70    TM    TABLESW,TBLEUPD     IS UPDATE BUY REC NEEDED                     
         BZ    UPD74                                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'PUTREC',SYSFIL,KEY+14,AIO1,DMWORK                
*                                                                               
UPD74    OC    0(3,R3),0(R3)       AT END OF SPOT TABLE                         
         BNZ   UPD20                NO                                          
*                                                                               
         GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
*                                                                               
         XC    TRAOPT,TRAOPT                                                    
         OI    TRAOPTH+6,X'80'                                                  
         OI    TRAMEDH+1,X'01'     SET MODIFY BIT ON                            
         NI    TRAMEDH+4,X'FF'-X'20' SET OFF VALIDATED                          
         BAS   RE,CLRSCR                                                        
         LA    R2,TRAMEDH                                                       
         BAS   RE,CLRCLT           CLEAR SAVE AND VALIDATED                     
         L     R1,=A(SAVEDMSG)                                                  
         B     COMERR                                                           
         DROP  R3                                                               
         EJECT                                                                  
* GET PROFILE REC(S)                                                            
*                                                                               
         DS    0H                                                               
FPRO     NTR1                                                                   
*                                                                               
* READ T0 PROFILE *                                                             
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
* READ TZ PROFILE *                                                             
*                                                                               
         MVI   WORK+3,C'Z'                                                      
         GOTO1 (RF),(R1),,SVTZPROF                                              
*                                                                               
* READ TB PROFILE *                                                             
*                                                                               
         MVI   WORK+3,C'B'                                                      
         GOTO1 (RF),(R1),,ELEM                                                  
         MVC   SVTBPR04,ELEM+3                                                  
*                                                                               
* READ T2 PROFILE *                                                             
*                                                                               
         MVI   WORK+3,C'2'                                                      
         GOTO1 (RF),(R1),,ELEM                                                  
         MVC   SVT2PR9,ELEM+8      BRAND AGENCY                                 
*MNMB                                                                           
*                                                                               
* READ T3 PROFILE *                                                             
*                                                                               
         MVI   WORK+3,C'3'                                                      
         GOTO1 (RF),(R1),,ELEM                                                  
         MVC   SVT3PR06,ELEM+5      BRAND AGENCY                                
*MNMB                                                                           
         B     EXIT                                                             
         EJECT                                                                  
* CLEAR DISPLAY AREA OF SCREEN *                                                
*                                                                               
CLRSCR   OC    TRAINFO,TRAINFO                                                  
         BZ    CLRSCRB                                                          
         CLC   TRAINFO,SPACES                                                   
         BE    CLRSCRB                                                          
         XC    TRAINFO,TRAINFO                                                  
         OI    TRAINFOH+6,X'80'                                                 
CLRSCRB  LA    RF,TRAEST1H                                                      
         LA    R1,TRALINES                                                      
CLRSCR10 XC    8(L'TRAEST1,RF),8(RF)                                            
         OI    6(RF),X'80'                                                      
         LLC   R0,0(RF)                                                         
         AR    RF,R0                                                            
         XC    8(L'TRACML1,RF),8(RF)  CLEAR CML FIELD                           
         OI    6(RF),X'80'            AND XMT                                   
*                                                                               
         LLC   R0,0(RF)                                                         
         AR    RF,R0                                                            
         XC    8(L'TRACML2,RF),8(RF)                                            
         OI    6(RF),X'80'                                                      
*                                                                               
CLRSCR30 IC    R0,0(RF)                                                         
         AR    RF,R0                                                            
         BCT   R1,CLRSCR10                                                      
         BR    RE                                                               
         EJECT                                                                  
BUYEL    CLI   0(R6),0                                                          
         JNE   *+8                                                              
         LTR   RE,RE                                                            
         BR    RE                                                               
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   0(1,R6),ELCDLO                                                   
         JL    BUYEL                                                            
         CLC   0(1,R6),ELCDHI                                                   
         JH    BUYEL                                                            
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
*                                                                               
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
*        ERROR ROUTINES                                                         
*                                                                               
NOESTER  MVI   ERROR,NOESTS                                                     
         B     TRAPERR                                                          
BDESTPR  MVI   ERROR,BADESTS                                                    
         B     TRAPERR                                                          
DLRLENER MVI   ERROR,INVTXTLN     TOO LONG                                      
         B     TRAPERR                                                          
VTYPER   MVI   ERROR,INVTYPE       INVALID CMML TYPE-CTYPTAB                    
         B     TRAPERR                                                          
MISSCLT  LA    R2,TRACLTH                                                       
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
PRDERR   MVI   ERROR,NOPRDFND      NO SUCH PROD FOR CLT                         
         B     TRAPERR                                                          
PRDINV   MVI   ERROR,INVPRDCD      POL & AAA INVALID PROD                       
         B     TRAPERR                                                          
NUMERR   MVI   ERROR,NOTNUM                                                     
TRAPERR  NI    TRAMEDH+4,X'FF'-X'20' FORCE VALIDATION                           
         GOTO1 ERREX                                                            
         EJECT                                                                  
PIGERR   L     R1,=A(PIGERRMS)                                                  
         B     COMERRA             NOTHING TO SAVE                              
PRGSIZER L     R1,=A(PRGSIZMS)                                                  
         B     COMERRA             NOTHING TO SAVE                              
NOSAVER  L     R1,=A(NOSAVEMS)                                                  
         B     COMXIT                                                           
TOTERR   L     R1,=A(TOTERRMS)                                                  
         LA    R2,TRAOPTH                                                       
         NI    LOG,X'FF'-LOGTCML-LOGTREF                                        
         OI    LOG,LOGREQD                                                      
         B     COMXIT                                                           
*                                                                               
TABLDER  DS   0H                                                                
         L     R1,=A(TABLDMS)                                                   
         B     COMERR                                                           
*                                                                               
NOASGERR DS   0H                                                                
         L     R1,=A(NOASGMSG)                                                  
         B     COMERR                                                           
*                                                                               
BDPRDER1 DS   0H                                                                
         L     R1,=A(BPRDMSG1)                                                  
         B     COMERR                                                           
*                                                                               
SCANERR  DS   0H                                                                
         L     R1,=A(SCANMS)                                                    
         NI    TRAMEDH+4,X'FF'-X'20' FORCE VALIDATION                           
         B     COMERRA             TWA UPDATED, DON'T SAVE                      
*                                                                               
VOPTPERR DS   0H                                                                
         L     R1,=A(PROGMSG)                                                   
         B     COMERRA             TWA UPDATED, DON'T SAVE                      
*                                                                               
VOPTERR  DS   0H                                                                
         L     R1,=A(DUPRMSG)                                                   
         B     COMERRA             TWA UPDATED, DON'T SAVE                      
*                                                                               
VOPTHLP  DS   0H                                                                
         L     R1,=A(OPTHLPMS)                                                  
         B     COMERRA             TWA UPDATED, DON'T SAVE                      
*                                                                               
UNCMLER  L     R1,=A(UNCMLMS)                                                   
         LA    R2,TRACML1H                                                      
         B     COMERRA             TWA UPDATED, DON'T SAVE                      
NOPBERR  L     R1,=A(NOPBMS)                                                    
         B     COMERR                                                           
CLRPERR  L     R1,=A(CLRPERMS)                                                  
         B     COMERR                                                           
ESTCPYER L     R1,=A(ESTCPYMS)                                                  
         B     CMLERXT                                                          
MISESTER L     R1,=A(MISESTMS)                                                  
         B     CMLERXT                                                          
BUYSIZER L     R1,=A(BUYSIZMS)                                                  
         B     CMLERXT                                                          
SEEDERR  DS   0H                                                                
         L     R1,=A(SEEDMSG)                                                   
         B     COMERRA             TWA UPDATED, DON'T SAVE                      
*                                                                               
PGSOERR  MVC   GERROR,=Y(INVPGSO)  CML PIGGY/SOLO MUST MATCH USAGE              
         BRAS  RE,SVTWA                                                         
         GOTO1 VTRAERR                                                          
WRITERR  MVC   GERROR,=Y(SPTRDONL) SPOT FILES ARE READ ONLY                     
         LA    R2,TRAMEDH          MEDIA                                        
         GOTO1 VTRAERR                                                          
CMLERXIT LA    R2,TRAOPTH                                                       
         TM    TABLESW,X'80'      TABLE BUILT YET                               
         BO    COMERRA                                                          
CMLERXT  NI    TRAMEDH+4,X'FF'-X'20' FORCE REVALIDATION                         
         B     COMERRA                                                          
COMXIT   OI    TRAOPTH+1,X'01'      SET MODIFIED BIT                            
         OI    TRAOPTH+6,X'80'                                                  
         XC    TRAOPT,TRAOPT                                                    
         TM    LOG,LOGPTTN+LOGCREF SEEDED SPTS FROM PTTN/CLEARED REF            
         BNZ   COMXITX                                                          
*                                                                               
* NEED TO WRITE TWA2 WITH EXTENDED LIST BEFORE EXIT *                           
*                                                                               
COMERR   BRAS  RE,SVTWA                                                         
COMERRA  XC    CONHEAD,CONHEAD                                                  
         A     R1,SPTRRR                                                        
         BCTR  R1,0                                                             
         IC    RF,0(R1)                                                         
         EX    RF,COMERMVC                                                      
ERREXIT  GOTO1 ERREX2                                                           
COMERMVC MVC   CONHEAD(0),1(R1)                                                 
*                                                                               
*        SET OFF SPOTS SEEDED FROM PATTERNS/CLEARED BY PTTN REF                 
*                                                                               
COMXITX NI     LOG,X'FF'-LOGPTTN-LOGCREF                                        
         BRAS  RE,SVTWA                                                         
         LA    R2,TRAOPTH                                                       
         B     ERREXIT                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
ALPHATAB DC    C'ABCDEFGHIJKL'                                                  
HEXFF    DC    5X'FF'                                                           
CMLOPTMS DC    C'TOTAL/SAVE/RECML - ONLY VALID'                                 
         DC    AL1(L'NOPBMS-1)                                                  
NOPBMS   DC    C'* ERROR * NO PIGGYBACK PRODUCT, NO COMMERCIAL NEEDED *C        
               '                                                                
         DC    AL1(L'TABLDMS-1)                                                 
TABLDMS  DC    C'* ERROR * CAN NOT DO THIS UNTIL SPOTS ARE DISPLAYED *'         
         DC    AL1(L'BPRDMSG1-1)                                                
BPRDMSG1 DC    C'* ERROR * COMML AND SPOT PRODS DO NOT MATCH *'                 
         DC    AL1(L'NOASGMSG-1)                                                
NOASGMSG DC    C'* ERROR * NO COMMERCIALS WERE ASSIGNED *'                      
         DC    AL1(L'DITTOMS-1)                                                 
DITTOMS  DC    C'* ERROR * TO COPY PREV COMMERCIAL, USE '' OR " *'              
         DC    AL1(L'NOPRVMS-1)                                                 
NOPRVMS  DC    C'* ERROR * MUST HAVE PREVIOUS COMML TO COPY FROM *'             
         DC    AL1(L'CMLSLNMS-1)                                                
CMLSLNMS DC    C'* ERROR * SPOTS LEN AND COMML LEN DIFFERENT *'                 
         DC    AL1(L'CLRPERMS-1)                                                
CLRPERMS DC    C'* ERROR * ENTER CREF=NNN (PATTERN REF) *'                      
         DC    AL1(L'BDCMLDTM-1)                                                
BDCMLDTM DC    C'* ERROR * SPOT OUTSIDE COMML (MMMDD/YY-MMMDD/YY) *'            
         DC    AL1(L'ESTCPYMS-1)                                                
ESTCPYMS DC    C'* ERROR * EST HAS COPY CODE *'                                 
         DC    AL1(L'MISESTMS-1)                                                
MISESTMS DC    C'* ERROR * EST NEEDED FOR COPY CDE=EST *'                       
         DC    AL1(L'NOSAVEMS-1)                                                
NOSAVEMS DC    C'* ERROR * BAD CML (*) NOT REASSIGNED YET *'                    
         DC    AL1(L'PIGERRMS-1)                                                
PIGERRMS DC    C'* ERROR * CAN NOT HANDLE PIGGYBACK COMMERCIALS *'              
         DC    AL1(L'PRGSIZMS-1)                                                
PRGSIZMS DC    C'* ERROR * MAX LENGTH FOR PROGRAM IS 18 *'                      
         DC    AL1(L'REQMS-1)                                                   
REQMS    DC    C'REQUEST DISPLAYED, HIT ENTER TO RESUME ASSIGNING *'            
         DC    AL1(L'TOTERRMS-1)                                                
TOTERRMS DC    C'* ERROR * NO SPOTS ASSIGNED *'                                 
         DC    AL1(L'CMLNUMMS-1)                                                
CMLNUMMS DC    C'* ERROR * COMML XXXXXXXX CHAR 6-7 MUST BE NUMERIC *'           
         DC    AL1(L'UNCMLMS-1)                                                 
UNCMLMS  DC    C'* ERROR * NO SPOTS HAVE BEEN ASSIGNED *'                       
         DC    AL1(L'NOPSPTMS-1)                                                
NOPSPTMS  DC    C'* ERROR * NO SPOTS (TRUE-POL) SELECTED *'                     
         DC    AL1(L'NEXTMSG-1)                                                 
NEXTMSG  DC    C'ENTER CMLS, HIT ENTER FOR NEXT SCREEN'                         
         DC    AL1(L'DONEMSGS-1)                                                
DONEMSGS DC    C'ALL SPOTS DISPLAYED-TYPE SAVE TO UPDATE'                       
         DC    AL1(L'SAVEDMSG-1)                                                
SAVEDMSG DC    C'CML ASSIGNS SAVED, ENTER NEXT REQUEST'                         
         DC    AL1(L'BUYSIZMS-1)                                                
BUYSIZMS DC    C'BUY LINE HAS TOO MANY SPOTS - SPLIT TO 2 BUY LINES'            
         DC    AL1(L'OPTHLPMS-1)                                                
OPTHLPMS DC    C'* VALID OPTIONS=CLEAR/TOTAL/SAVE/PATTERN/TREF/CREF= *'         
         DC    AL1(L'SCANMS-1)                                                  
SCANMS   DC    C'* ERROR * FIELD TOO LARGE/ONLY REQUEST 1 FUNCTION *'           
         DC    AL1(L'DUPRMSG-1)                                                 
DUPRMSG  DC    C'* ERROR * ONE COMMERCIAL AT A TIME *'                          
         DC    AL1(L'PROGMSG-1)                                                 
PROGMSG  DC    C'* ERROR * NO SPOTS FOR THIS PROGRAM *'                         
         DC    AL1(L'SEEDMSG-1)                                                 
SEEDMSG  DC    C'* ERROR * MUST USE SPOT SEED'                                  
         DROP  R7                                                               
         LTORG                                                                  
         EJECT                                                                  
*==================================================                             
* READ THROUGH BUYS AND BUILD SPOT ACTIVITY LIST                                
*==================================================                             
                                                                                
BLACT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* CLEAR 1ST 2 WORK AREAS IN ACTIVITY LIST BUILD AREA *                          
*                                                                               
         XC    SPTABLE(L'SPTDATA*2),SPTABLE                                     
*                                                                               
         LA    RE,PROGTABL                                                      
         LA    RF,PROGNUM*L'PROGTABL                                            
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT SYSTEM                        
                                                                                
*                                                                               
         LA    R3,SPTABLE                                                       
         USING SPTABLED,R3                                                      
                                                                                
         XC    KEY,KEY             BUILD BUY KEY                                
         MVC   KEY(4),BAGYMD       A-M/CLT/PRD                                  
         MVC   KEY+4(5),BMKT       MKT/STA                                      
         MVC   KEY+9(1),BEST                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     BLA12                                                            
*                                                                               
BLA10    MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
* IF ASSIGNING CMMLS BY MARKET, BSTA WILL BE ZERO *                             
*                                                                               
BLA12    DS    0H                                                               
*MNMB                                                                           
         CLI   SVT3PR06,C'Y'                                                    
         BNE   BLA14                                                            
         XC    ELEM,ELEM                                                        
         XC    DUB,DUB                                                          
         GOTO1 MSUNPK,DMCB,(X'80',KEY+4),ELEM,DUB                               
         CLI   DUB+4,C'D'                                                       
         BE    BLA10                                                            
         CLI   DUB+4,C'S'                                                       
         BE    BLA10                                                            
         CLI   DUB+4,C'C'          CM FOR IHEART                                
         BE    BLA10                                                            
BLA14    DS    0H                                                               
*MNMB                                                                           
         OC    BSTA,BSTA           IS THIS BY MKT                               
         BNZ   BLA16                                                            
         CLC   KEY(6),KEYSAVE      A-M/C/P/MKT                                  
         BNE   BLA34                                                            
         B     BLA30                                                            
*                                                                               
BLA16    CLC   KEY(9),KEYSAVE      A-M/C/P/MKT/STA                              
         BNE   BLA34               DONE BUILD                                   
*                                                                               
BLA30    LLC   RE,KEY+9                                                         
         LA    RE,SVESTAB(RE)                                                   
         CLI   0(RE),0             TEST EST ACTIVE                              
         BNE   BLA36               YES                                          
         B     BLA10                                                            
         EJECT                                                                  
* SORT SPOTS IN DATE ORDER WITHIN STATION                                       
*                                                                               
BLA34    LH    R2,SPOTCT           GET NUMBER OF SPOTS                          
         LTR   R2,R2               IF NO SPOTS FOUND                            
         BZ    NOSPTER                                                          
*                                                                               
         CLI   SVTZPR03,C'Y'      SORT BY DATE/TIME                             
         BE    BLA35                YES                                         
*                                                                               
         GOTO1 XSORT,DMCB,SPTABLE,(R2),L'SPTDATA,L'SPTSORT,0                    
*                                                                               
* COUNT NUMBER OF SPOTS TO DISPLAY ON THIS SCREEN                               
*                                                                               
BLA35    DS    0H                                                               
         LA    RE,SPTABLE                                                       
         LA    RF,1                                                             
         STC   RF,SPTTBLN-SPTDATA(RE)                                           
         LA    RE,SPTNEXT-SPTDATA(RE)                                           
         LA    RF,1(RF)                                                         
         BCT   R2,*-12                                                          
*                                                                               
         CLI   SVTZPR03,C'Y'      SORT BY DATE/TIME                             
         BNE   BLA35X                                                           
*                                                                               
* SORT BY: PROD/LEN/PROD2/LEN2/FTD/TIME                                         
*                                                                               
         GOTO1 XSORT,DMCB,SPTABLE,(R2),L'SPTDATA,4,                    C        
               SPTPROD2-SPTDATA                                                 
         GOTO1 (RF),(R1),,,,4,SPTPROD-SPTDATA                                   
         GOTO1 (RF),(R1),,,,4,SPTTIME-SPTDATA                                   
         GOTO1 (RF),(R1),,,,2,SPTFTD-SPTDATA                                    
*                                                                               
BLA35X   GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
*                                                                               
         B     BLAX                                                             
         EJECT                                                                  
*======================================================                         
* GET BUY RECORD                                                                
*======================================================                         
                                                                                
BLA36    L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         GOTO1 GETREC                                                           
         USING BUYRECD,R4                                                       
         TM    15(R4),X'80'        TEST DELETED BUY                             
         BO    BLA10               YES - IGNORE                                 
*                                                                               
         CLI   FTPROGLN,0          FILTERING ON PROGRAM NAME                    
         BE    BLA38                                                            
         LLC   RF,FTPROGLN                                                      
         BCTR  RF,0                                                             
         EX    RF,CKPRGNM                                                       
         BNE   BLA10                                                            
         B     BLA38                                                            
CKPRGNM  CLC   FTPROGNM(0),BDPROGRM                                             
                                                                                
* CALCULATE DIFFERENCE BETWEEN FIRST/LAST DAYS OF ROTATOR *                     
                                                                                
BLA38    DS    0H                                                               
         LLC   R0,BDDAY                                                         
         SLL   R0,25                                                            
         LTR   R0,R0               REG GOES NEGATIVE WHEN BIT ARRIVES           
         BM    *+12                                                             
         SLL   R0,1                                                             
         B     *-10                                                             
*                                                                               
         SR    RE,RE               CLEAR COUNTER                                
         SLL   R0,1                                                             
         LTR   R0,R0               SHIFT TILL NO MORE BITS ON                   
         BZ    *+8                                                              
         BCT   RE,*-10                                                          
         LPR   RE,RE                                                            
         STH   RE,ROTDAYS          AND SAVE DAYS                                
*                                                                               
* LOOK FOR TRAFFIC=NO                                                           
*                                                                               
         MVI   ELCDLO,X'66'                                                     
         MVI   ELCDHI,X'66'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
BLA39    BRAS  RE,BUYEL                                                         
         BNE   BLA39X                                                           
         CLC   =C'TRAFFIC=NO',3(R6)                                             
         BE    BLA10               BYPASS THIS BUY                              
         B     BLA39                                                            
*                                                                               
BLA39X   MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         LA    R6,BDELEM                                                        
         SR    R5,R5               ZERO EQ ELEM CTR                             
*                                                                               
BLA40    BRAS  RE,BUYEL                                                         
         BNE   BLA10                                                            
         MVI   SVPRDFLG,0                                                       
         TM    6(R6),X'C0'         TEST MINUS OR MINUSED                        
         BNZ   BLA40                                                            
         CLI   1(R6),10            TEST UNALL                                   
         BNH   BLA40                                                            
         CLC   ELDATE,2(R6)        SAME DATE AS LAST                            
         BE    BLA44                YES                                         
         SR    R5,R5               ZERO EQ ELEM CTR                             
*                                                                               
BLA44    MVC   ELDATE,2(R6)        SAVE ELEM START DATE                         
         LA    R5,1(R5)            ADD TO ELEMENT CTR                           
         XC    SVPROD(10),SVPROD                                                
         MVC   SVBPRD(2),10(R6)                                                 
         LA    R1,SVBPRD                                                        
         BRAS  RE,FPROD                                                         
         MVC   SVPROD,0(RF)                                                     
*                                                                               
         CLI   1(R6),18            THIS PIGGYBACK PROD                          
         BNE   BLA46                                                            
         MVC   SVBPRD2(2),14(R6)                                                
         LA    R1,SVBPRD2                                                       
         BRAS  RE,FPROD                                                         
         MVC   SVPROD2,0(RF)                                                    
         MVI   SVPRDFLG,0                                                       
         CLC   SVPROD,SVPROD2      SEE IF PRODS IN ALPHA ORDER                  
         BL    BLA46                                                            
*                                                                               
* REVERSE PRODUCT ORDER                                                         
*                                                                               
         MVC   DUB(5),SVPROD                                                    
         MVC   SVPROD(5),SVPROD2                                                
         MVC   SVPROD2(5),DUB                                                   
         MVI   SVPRDFLG,1                                                       
*                                                                               
BLA46    CLC   BPRD,SVBPRD         THIS PROD                                    
         BNE   BLA40                NO                                          
         CLC   BSLN,SVBSLN         THIS SPOT LEN                                
         BNE   BLA40                NO                                          
         CLI   1(R6),18            THIS PIGGYBACK                               
         BE    *+24                 YES                                         
         CLI   BPRD2,0             LIMIT ON P/B'S                               
         BE    BLA48                NO                                          
         CLI   BPRD2,255           NO P/B'S AT ALL                              
         BE    BLA48                YES, OK                                     
         B     BLA40               BYPASS                                       
*                                                                               
         CLI   BPRD2,0             LIMIT ON P/B'S                               
         BE    BLA48                NO                                          
         CLI   BPRD2,255           NO P/B'S AT ALL                              
         BE    BLA40                YES, BYPASS                                 
         CLC   BPRD2,SVBPRD2                                                    
         BNE   BLA40                                                            
         CLI   BSLN2,0             LIMIT ON P/B'S SPOT LEN                      
         BE    BLA48                NO                                          
         CLC   BSLN2,SVBSLN2                                                    
         BNE   BLA40                                                            
*                                                                               
BLA48    MVC   ELDATEX,2(R6)       AND PRESET ELEM END DATE                     
*                                                                               
         CLC   ELDATE,PERENDP      TEST AFTER FLIGHT/TELECAST                   
         BH    BLA40                                                            
*                                                                               
         CLI   0(R6),11            TEST REGEL                                   
         BNE   BLA50                                                            
         GOTO1 DATCON,DMCB,(2,ELDATE),WORK                                      
         LH    R0,ROTDAYS                                                       
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,ELDATEX)                                   
*                                                                               
BLA50    CLC   ELDATEX,PERSTP      TEST BEFORE FLIGHT/TELECAST                  
         BL    BLA40                                                            
*                                                                               
         XC    SPTWORK,SPTWORK                                                  
         LA    R3,SPTWORK                                                       
         USING SPTABLED,R3                                                      
         MVC   SPTSTA,KEY+6                                                     
         MVC   SPTFTD,ELDATE                                                    
         MVC   SPTLTD,ELDATEX                                                   
         MVC   SPTDAY,BDDAY                                                     
         MVC   SPTTIME,BDTIMST                                                  
         STC   R5,SPTSPTN                                                       
         MVC   SPTEST,BUYKEST                                                   
*                                                                               
         LLC   R0,BUYKEY+10                                                     
         TM    BUYRCNTL,BUYRLN2    TEST 2-BYTE LINE NUMBERS                     
         BZ    *+8                                                              
         ICM   R0,3,BUYKEY+10                                                   
         STCM  R0,3,SPTLINE                                                     
*                                                                               
         MVC   SPTDSKAD,KEY+14                                                  
         MVC   SPTPROD,SVPROD                                                   
         MVC   SPTSLN,SVBSLN                                                    
         MVC   SPTDPT,BDDAYPT                                                   
         MVC   SPTPROGT,BDPROGT    PROGRAM ADJ. CODE                            
         CLI   1(R6),18            TEST PIGGYBACK                               
         BL    *+16                                                             
         MVC   SPTPROD2,SVPROD2                                                 
         MVC   SPTSLN2,SVBSLN2                                                  
                                                                                
* ADD PROGRAM NAME TO PROGRAM TABLE (IF NOT THERE ALREADY)                      
                                                                                
         LA    R1,PROGTABL                                                      
         SR    RE,RE                                                            
         LA    RF,L'PROGTABL*PROGNUM(R1) POINT TO END OF TABLE                  
*                                                                               
BLA51    OC    0(L'PROGTABL,R1),0(R1) ANY ENTRY?                                
         BZ    BLA51C              NO, ADD PROGRAM TO THE TABLE                 
         CLC   BDPROGRM,0(R1)      SAME?                                        
         BNE   *+12                                                             
         STC   RE,SPTPRGPT         POINTER INTO PROG TABLE                      
         B     BLA52               CONTINUE PROCESSING                          
*                                                                               
         LA    RE,1(RE)                                                         
         LA    R1,L'PROGTABL(R1)                                                
         CR    R1,RF               END OF TABLE?                                
         BL    *+6                                                              
         DC    H'0'                MAKE PROG TABLE BIGGER                       
         B     BLA51                                                            
*                                                                               
BLA51C   MVC   0(L'PROGTABL,R1),BDPROGRM                                        
         STC   RE,SPTPRGPT                                                      
*                                                                               
BLA52    DS   0H                                                                
         LR    RE,R6                                                            
         LLC   R0,1(R6)                                                         
         B     BLA54                                                            
*                                                                               
BLA53    IC    R0,1(RE)                                                         
*                                                                               
BLA54    AR    RE,R0                                                            
         CLI   0(RE),13            THIS ANOTHER SPOT                            
         BNH   BLA60                YES                                         
         CLI   0(RE),X'18'         THIS A CML ASSIGN ELEMENT                    
         BNE   BLA53                NO, LOOK FURTHUR                            
         CLI   1(RE),9             THIS A DEALER TAG?                           
         BE    DLRTAGER             YES                                         
         MVC   SPTCMLSQ(4),TRACCSQ-TRACID(RE)   SAVE BOTH COMML SEQ             
         MVC   SPTPREF,TRACREF-TRACID(RE)                                       
*                                                                               
         NI    SPTPREF,X'7F'       SET OFF PRINTED BIT                          
*                                                                               
         OC    SPTCMLSQ(4),SPTCMLSQ ARE COMMERCIALS ASSIGNED                    
         BZ    BLA56                                                            
         OI    SPTFLAG,SPTPRVAS    SET ON PREVIOUS CMML ASSIGN FLAG             
         OC    SPTPREF,SPTPREF     ASSIGNED FROM PATTERN                        
         BZ    BLA56                                                            
         OI    SPTFLAG,SPTSRCPT    SET ON SOURCE = PATTERN FLAG                 
*                                                                               
BLA56    CLI   SVPRDFLG,0          WERE PRODS SWAPPED TO ALPHA ORDER            
         BE    BLA60                NO                                          
         MVC   SPTCMLSQ,TRACCSQ2-TRACID(RE)                                     
         MVC   SPTCMLS2,TRACCSQ-TRACID(RE)                                      
*                                                                               
* TEST DATA IN TABLE ALREADY *                                                  
*                                                                               
BLA60    LA    R3,SPTABLE                                                       
*                                                                               
BLA64    OC    0(3,R3),0(R3)       END OF TABLE                                 
         BZ    BLA70                                                            
*                                                                               
         CLC   0(L'SPTDATA,R3),SPTWORK STA/FTD/LTD/#/DAY/TIME/EST/LIN           
         BNE   *+6                                                              
         DC    H'0'                                                             
         LA    R3,SPTNEXT                                                       
         B     BLA64                                                            
*                                                                               
BLA70    LA    RE,TOPASVTB         MAKE SURE NOT PAST END OF SAVED AREA         
         LA    RE,SPTABLE                                                       
         AHI   RE,ENDSYSD-SPTABLE                                               
         CR    R3,RE                                                            
         BH    SPTSIZER                                                         
*                                                                               
         LH    R1,SPOTCT           UPDATE TOTAL SPOTS COUNTER                   
         LA    R1,1(R1)                                                         
         CHI   R1,MAXSPOTS                                                      
         BH    MAXSPTER                                                         
         STH   R1,SPOTCT                                                        
*                                                                               
         MVC   SPTDATA,SPTWORK                                                  
         OC    SPTCMLSQ,SPTCMLSQ                                                
         BNZ   BLA74                                                            
         LH    R1,UNCMLCT                                                       
         LA    R1,1(R1)                                                         
         STH   R1,UNCMLCT                                                       
*                                                                               
BLA74    OC    SPTPROD2,SPTPROD2                                                
         BZ    BLA76                                                            
         OC    SPTCMLS2,SPTCMLS2                                                
         BNZ   BLA76                                                            
         LH    R1,UNCMLCT                                                       
         LA    R1,1(R1)                                                         
         STH   R1,UNCMLCT                                                       
*                                                                               
BLA76    XC    L'SPTDATA(L'SPTDATA,R3),L'SPTDATA(R3)                            
         B     BLA40                                                            
         DROP  R3,R4                                                            
*                                                                               
BLAX     XIT1                                                                   
                                                                                
*======================================================                         
* FIND 3 CHAR PROD CODE FROM 1 BINARY CODE                                      
* R1 POINTS TO 1-BYTE BINARY PRD                                                
* ON EXIT, RF POINTS TO 3 BYTE CHAR                                             
*======================================================                         
                                                                                
FPROD    L     RF,ASVCLIST                                                      
*                                                                               
FPROD10  CLC   0(1,R1),3(RF)                                                    
         BER   RE                                                               
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         JH    FPROD10                                                          
         DC    H'0'                                                             
*                                                                               
MAXSPTER L     R1,=A(MAXSPTMS)     TOO MANY SPOTS                               
         B     COMERRD             NOTHING TO SAVE                              
DLRTAGER L     R1,=A(DLRTAGMS)                                                  
         LA    R2,TRACLTH          CLIENT                                       
         B     COMERRD             NOTHING TO SAVE                              
SPTSIZER L     R1,=A(SPTSIZMS)                                                  
         B     COMERRD             NOTHING TO SAVE                              
NOSPTER  L     R1,=A(NOSPTMS)                                                   
         LA    R2,TRAMEDH                                                       
         NI    4(R2),X'FF'-X'20'   SET OFF VALIDATED                            
         TM    LOG,X'80'           WERE NON-POL BUYS FOUND                      
         BZ    COMERRC                                                          
         L     R1,=A(NOPSPTMS)                                                  
*                                                                               
* NEED TO WRITE TWA2 WITH EXTENDED LIST BEFORE EXIT *                           
*                                                                               
COMERRC  BRAS  RE,SVTWA                                                         
*                                                                               
COMERRD  NI    TRAMEDH+4,X'FF'-X'20'   SET OFF VALIDATED IF ERROR               
         XC    CONHEAD,CONHEAD                                                  
         A     R1,SPTRRR                                                        
         BCTR  R1,0                                                             
         IC    RF,0(R1)                                                         
         EX    RF,CMERMVC                                                       
         GOTO1 ERREX2                                                           
CMERMVC  MVC   CONHEAD(0),1(R1)                                                 
*                                                                               
         DC    AL1(L'DLRTAGMS-1)                                                
DLRTAGMS DC    C'* ERROR * SPOTS HAVE BEEN DEALER TAGGED *'                     
         DC    AL1(L'SPTSIZMS-1)                                                
SPTSIZMS DC    C'* ERROR * TOO MANY SPOTS, REDO WITH SMALLER PERIOD *'          
         DC    AL1(L'NOSPTMS-1)                                                 
NOSPTMS  DC    C'* ERROR * NO SPOTS SELECTED *'                                 
         DC    AL1(L'MAXSPTMS-1)                                                
MAXSPTMS DC    C'* ERROR * MORE THAN 215 SPOTS *'                               
         LTORG                                                                  
         EJECT                                                                  
*=======================================================                        
* SUBROUTINE VALIDATES COMMERCIAL ASSIGNMENT                                    
* ON ENTRY WORK PRD1(3)/BPRD1/SLN1                                              
*               PRD2(3)/BPRD2/SLN2                                              
*=======================================================                        
                                                                                
         USING SPTABLED,R3                                                      
VCML     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   BYTE,1              ASSIGN CML TO A PROGRAM?                     
         BNE   VCML00              YES                                          
         MVC   8(12,R2),SVACML                                                  
         B     VCML08                                                           
*                                                                               
* CHECK IF REQUEST TO COPY PREV COMML *                                         
*                                                                               
VCML00   CLI   5(R2),1             CK FOR ONLY 1 CHAR                           
         BNE   VCML01                                                           
         CLI   8(R2),C''''         IF 1, MUST BE ' OR                           
         BE    *+12                                                             
         CLI   8(R2),C'"'          "                                            
         JNE   DITTOERR                                                         
         OC    LASTCML,LASTCML     IS THERE A PREVIOUS COMML                    
         JZ    NOPRVCML                                                         
         CLC   =C'DELETE',LASTCML  WAS LAST A DELETE                            
         BE    VCML36                                                           
         MVC   8(12,R2),LASTCML                                                 
         MVI   5(R2),12                                                         
         B     VCML08                                                           
*                                                                               
VCML01   CLI   5(R2),3             IF 3, COULD BE TBA                           
         BNE   VCML02                                                           
         CLC   =C'TBA',8(R2)       IS THIS A TBA                                
         BE    VCML38                                                           
         MVI   ERROR,INVALID                                                    
         J     TRAPERR                                                          
*                                                                               
VCML02   CLI   5(R2),6                                                          
         BNE   *+14                                                             
         CLC   =C'DELETE',8(R2)                                                 
         BE    VCML36                                                           
*                                                                               
         CLI   5(R2),9             9 CHARS MAY BE #NNNNNNNNN                    
         BNE   VCML02A                                                          
         CLI   8(R2),C'#'                                                       
         BNE   VCML02A                                                          
         MVC   DUB,9(R2)                                                        
         MVC   8(8,R2),DUB         MOVE INPUT TO THE LEFT + X'00'               
         MVI   16(R2),0                                                         
         MVI   5(R2),8             SET NEW INPUT LENGTH                         
         OI    6(R2),X'80'         SEND IT BACK TO USER                         
         B     VCML08              AND IT'S AN ISCI!                            
*                                                                               
VCML02A  CLI   5(R2),8                                                          
         BH    VCML08              MORE THAN 8 IS ADID                          
         BE    VCML02B                                                          
         OC    LASTCML,LASTCML     IF NO PREVIOUS                               
         JZ    NOPRVCML                                                         
         B     VCML04              TREAT < 8 CHARS AS PARTIAL INPUT             
                                                                                
* WHEN EXACTLY 8 CHARS INPUT, ASSUME ISCI IF FIRST                              
* 4 CHARS ARE ALPHA, ELSE OVERWRITE LASTCML+4                                   
                                                                                
VCML02B  OC    LASTCML,LASTCML     IF NO PREVIOUS                               
         BZ    VCML08              THEN JUST VALIDATE IT                        
*                                                                               
         LA    R0,4                                                             
         LA    R1,8(R2)                                                         
*                                                                               
VCML03   CLI   0(R1),C'A'                                                       
         BL    VCML04                                                           
         CLI   0(R1),C'Z'                                                       
         BH    VCML04                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,VCML03                                                        
         B     VCML08                                                           
*                                                                               
* WHEN INPUT LESS THAN 8 CHARS, APPEND TO FIRST 4 OF LASTCML                    
*                                                                               
VCML04   MVC   LASTCML+4(8),SPACES                                              
         MVI   ERROR,INVALID                                                    
         CLI   5(R2),4                                                          
         JL    TRAPERR                                                          
*                                                                               
         LLC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   LASTCML+4(0),8(R2)                                               
*                                                                               
         MVC   8(12,R2),LASTCML                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R1,LASTCML+11                                                    
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         LA    R0,LASTCML-1                                                     
         SR    R1,R0                                                            
         STC   R1,5(R2)            SET INPUT LENGTH                             
*                                                                               
VCML08   CLC   =C'DELETE',8(R2)    IS THIS A DELETE                             
         BE    VCML36                                                           
         CLI   5(R2),8                                                          
         BL    CMLNERR                                                          
         CLI   5(R2),12                                                         
         BH    CMLNERR                                                          
*                                                                               
         LA    R1,WORK                                                          
         BRAS  RE,FPRD             GET 1 BYTE PRD CODE                          
*                                                                               
         OC    WORK+5(3),WORK+5    ANY P/B PROD                                 
         BZ    VCML10                                                           
         LA    R1,WORK+5                                                        
         BRAS  RE,FPRD                                                          
         EJECT                                                                  
* SEE IF COMML CODE IN TABLE ALREADY *                                          
*                                                                               
VCML10   MVC   LASTCML,8(R2)                                                    
         OC    LASTCML,SPACES                                                   
*                                                                               
         L     R4,AIO3                                                          
         LA    R0,4000(R4)                                                      
         USING CMLTBLED,R4                                                      
*                                                                               
VCML20   OC    CMLTDATA,CMLTDATA                                                
         BZ    VCML40                                                           
         CLC   8(12,R2),CMLTCML     SAME COMML CODE                             
         BE    VCML30                                                           
*                                                                               
VCML24   LA    R4,CMLTNEXT                                                      
         CR    R4,R0                                                            
         BL    VCML20                                                           
         DC    H'0'                                                             
                                                                                
* CK IF PROD, SLN, AND DATES MATCH, OR ARE COVERED *                            
                                                                                
VCML30   CLC   CMLTPROD,WORK                                                    
         BNE   VCML24                                                           
         CLC   SPTSLN,CMLTLEN                                                   
         BNE   CMLSLNER                                                         
         CLC   SPTFTD,CMLTSTDT                                                  
         BL    VCML32                                                           
         CLC   SPTFTD,CMLTRCDT                                                  
         BNH   VCML34                                                           
*                                                                               
VCML32   MVC   DUB,CMLTSTDT                                                     
         B     BADCMLER                                                         
*                                                                               
VCML34   MVC   CMLSEQCD,CMLTSQNO                                                
         MVC   SVCMLSOL,CMLTSOLO                                                
         MVC   LASTCML,8(R2)                                                    
         OC    CMLTSQNO,CMLTSQNO   MUST NOT BE ZERO                             
         BNZ   VCPX                                                             
         DC    H'0'                                                             
*                                                                               
VCML36   XC    LASTCML,LASTCML                                                  
         MVC   LASTCML(6),=C'DELETE'                                            
*                                                                               
VCML38   XC    CMLSEQCD,CMLSEQCD   SET TO NONE                                  
         MVI   SVCMLSOL,0                                                       
         CR    RB,RD               SET NE CC FOR RETURN                         
         B     VCPX                                                             
*                                                                               
* CMML CODE NOT IN TABLE, VALIDATE FROM FILE *                                  
*                                                                               
VCML40   XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CMLKEY,R6                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD AND BCLT                                        
         MVC   CMLKCML,8(R2)                                                    
*                                                                               
         CLI   5(R2),8                                                          
         BNH   VCML41                                                           
         MVC   CMLCD,SPACES                                                     
         LLC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CMLCD(0),8(R2)                                                   
         GOTO1 VTRPACK,DMCB,(C'P',CMLCD),CMLKCML                                
         BNE   INVCMLER                                                         
*                                                                               
VCML41   MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VCML41X                                                          
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         MVC   KEY(2),=X'0AC1'     MAYBE IT'S AN ADID                           
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   INVCMLER                                                         
*                                                                               
VCML41X  MVC   SVCML,CMLKCML       SAVE CML                                     
*                                                                               
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         LR    R6,R4                                                            
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         MVC   SVCMLRCL,CMLRCL     SAVE CML RECALL DATE                         
*                                                                               
         GOTO1 DATCON,DMCB,(2,SPTFTD),(3,FULL)                                  
         CLC   FULL(3),CMLRLSE                                                  
         BL    VCML42                                                           
         CLC   FULL(3),CMLRCL                                                   
         BNH   VCML44                                                           
*                                                                               
VCML42   MVC   DUB,CMLRLSE                                                      
         B     BADCMLER                                                         
*                                                                               
VCML44   CLC   CMLSLN,WORK+4       THIS CORRECT SPOT LEN                        
         BE    VCML50                                                           
*                                                                               
* CHECK IF 1 COMML COVERS BOTH PRODUCTS *                                       
*                                                                               
         OC    WORK+5(4),WORK+5    IS THERE A P/B PROD                          
         BZ    CMLSLNER             NO                                          
         CLI   PRDFLD,2            CAN'T BE FIELD 2                             
         BE    CMLSLNER             YES                                         
         LLC   RE,WORK+4                                                        
         LLC   RF,WORK+9                                                        
         AR    RE,RF                                                            
         CLM   RE,1,CMLSLN                                                      
         BNE   CMLSLNER                                                         
         LA    R1,WORK+3                                                        
         BAS   RE,VCP              VALIDATE COMML PROD                          
         BNE   CMLPRDBD                                                         
         LA    R1,WORK+8                                                        
         BAS   RE,VCP              VALIDATE COMML PROD                          
         BNE   CMLPRDBD                                                         
         OC    SPTCMLSQ,SPTCMLSQ                                                
         BNZ   VCML46                                                           
         LH    R1,UNCMLCT                                                       
         BCTR  R1,0                                                             
         STH   R1,UNCMLCT                                                       
*                                                                               
VCML46   OC    SPTCMLS2,SPTCMLS2                                                
         BNZ   VCML48                                                           
         LH    R1,UNCMLCT                                                       
         BCTR  R1,0                                                             
         STH   R1,UNCMLCT                                                       
*                                                                               
VCML48   MVC   SPTCMLSQ,CMLSEQ+1                                                
         MVC   SPTCMLS2,CMLSEQ+1                                                
         MVC   LASTCML1,8(R2)                                                   
         MVC   LASTCML2,8(R2)                                                   
         OI    4(R2),X'20'         VALIDATED                                    
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         MVC   8(8,R2),LASTCML1                                                 
         OI    6(R2),X'80'         XMT                                          
         OI    4(R2),X'20'         VALIDATED                                    
*                                                                               
* VALIDATE CML APPROVALS IF ANY                                                 
*                                                                               
         BRAS  RE,VCMLAPR          CHK CML APPROVALS IF ANY                     
*                                                                               
         CR    R0,R0               SET COND CODE                                
         B     VCPX                                                             
*                                                                               
* VALIDATE CML APPROVALS IF ANY                                                 
*                                                                               
VCML50   BRAS  RE,VCMLAPR          CHK CML APPROVALS IF ANY                     
*                                                                               
* BUILD NEW COMML ENTRY IN TABLE *                                              
*                                                                               
         LA    R1,WORK+3                                                        
         BAS   RE,VCP              VALIDATE COMML PROD                          
         BE    VCML55                                                           
*                                                                               
         CLI   BYTE,1              ASGN CML TO A PROG?                          
         BNE   CMLPRDBD             NO, ERROR                                   
         CLI   PRDFLD,2            IF FIELD 2                                   
         BNE   VCPX                                                             
         OC    CMLSEQCD,CMLSEQCD   AND FIELD 1 WAS NO MATCH                     
         BZ    CMLPRDBD            THEN ERR                                     
         MVI   PRDFLD,0            NO MATCH FOR FIELD 2                         
         B     VCPX                                                             
*                                                                               
VCML55   MVC   CMLTPROD,WORK+5     SAVE ASSOCIATED PRODUCT                      
         MVC   CMLTLEN,CMLSLN                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(3,CMLRLSE),(2,CMLTSTDT)                             
         GOTO1 (RF),(R1),(3,CMLRCL),(2,CMLTRCDT)                                
*                                                                               
         MVC   CMLTSQNO,CMLSEQ+1                                                
         MVC   CMLTSOLO,CMLSOLO                                                 
         MVC   CMLSEQCD,CMLTSQNO                                                
         MVC   SVCMLSOL,CMLTSOLO                                                
         MVC   LASTCML,8(R2)                                                    
         OC    CMLTSQNO,CMLTSQNO   MUST NOT BE ZERO                             
         BNZ   VCPX                                                             
         DC    H'0'                                                             
         EJECT                                                                  
* VALIDATE COMMERCIAL PRODUCT CODE *                                            
*                                                                               
VCP      NTR1                                                                   
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LLC   RE,1(R6)                                                         
         BCTR  RE,0                                                             
         BCTR  RE,0                                                             
         LA    RF,2(R6)                                                         
         CLI   2(R6),X'FF'         GOOD FOR ALL PRODS                           
         BE    VCP20                                                            
VCP10    CLC   0(1,R1),0(RF)                                                    
         BE    VCP20                                                            
         LA    RF,1(RF)                                                         
         BCT   RE,VCP10                                                         
         LTR   RB,RB                                                            
         B     VCPX                                                             
VCP20    DS    0H                                                               
*                                                                               
VCPX     XIT1                                                                   
*                                                                               
CMLPRDBD DS    0H                                                               
         CLI   BYTE,1              ASNG CML TO PROG                             
         BE    BADPRDER                                                         
         MVI   ERROR,CMLPRDER                                                   
         B     *+16                                                             
INVCMLER MVI   ERROR,INVCOMM                                                    
         B     *+8                                                              
CMLNERR  MVI   ERROR,INVCMMLN                                                   
*                                                                               
         BRAS  RE,SVTWA                                                         
         GOTO1 ERREX                                                            
*                                                                               
BADPRDER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BPRDMSG),BPRDMSG                                       
         BRAS  RE,SVTWA                                                         
         GOTO1 ERREX2                                                           
BPRDMSG  DC    C'* ERROR * COMML AND SPOT PRODS DO NOT MATCH *'                 
*                                                                               
BADCMLER LAY   R4,BDCMLDTM                                                      
         GOTO1 DATCON,DMCB,(3,DUB),(5,30(R4))                                   
         GOTO1 (RF),(R1),(3,DUB+3),(5,39(R4))                                   
         L     R1,=A(BDCMLDTM)                                                  
*                                                                               
         CLC   =X'FFFFFF',DUB+3    CK FOR UFN RECALL                            
         BNE   COMERRB                                                          
         MVC   39(11,R4),=CL11'UFN) *'                                          
         B     COMERRB                                                          
DITTOERR L     R1,=A(DITTOMS)                                                   
         B     COMERR1                                                          
NOPRVCML L     R1,=A(NOPRVMS)                                                   
         B     COMERR1                                                          
CMLSLNER L     R1,=A(CMLSLNMS)                                                  
COMERR1  BRAS  RE,SVTWA                                                         
COMERRB  XC    CONHEAD,CONHEAD                                                  
         A     R1,SPTRRR                                                        
         BCTR  R1,0                                                             
         IC    RF,0(R1)                                                         
         EX    RF,COMRRMVC                                                      
         GOTO1 ERREX2                                                           
COMRRMVC MVC   CONHEAD(0),1(R1)                                                 
         DROP  R3,R4                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* FURTHER VALIDATE CML (APPROVALS AND DATES IF ANY)                             
                                                                                
VCMLAPR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   ELCODE,X'90'        BROADCAST BUSINESS ELEMENT                   
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    VAPR10                                                           
*                                                                               
         CLI   SVT2PR9,C'Y'        IS THIS BRAND AGENCY                         
         BNE   VAPRXIT                                                          
         TM    COMML,CMLAPRSW      CHK APR ON PREV ASGND CML                    
         BZ    NOAIRERR                                                         
         OI    COMML,NOAIR         YES, NOT APPROVED TO AIR                     
         B     VAPRX                                                            
*                                                                               
         USING CMLBBEL,R6                                                       
*                                                                               
VAPR10   DS    0H                                                               
         CLI   CMLBBBAG,C'N'       IS BRAND AGY=N                               
         BE    VAPRXIT                                                          
         CLI   CMLBBBAG,C'Y'                                                    
         BE    *+12                                                             
         CLI   SVT2PR9,C'Y'        IS THIS BRAND AGENCY                         
         BNE   VAPRXIT                                                          
*                                                                               
         OC    CMLBBMXD,CMLBBMXD   ANY MAX USE DATE                             
         BZ    VAPR20                                                           
*                                                                               
         CLC   SVCMLRCL,CMLBBMXD   COMPARE RECALL DATE TO MAX USE DTE           
         BNH   VAPR20                                                           
         MVC   SVCMLRCL,CMLBBMXD   SAVE EARLIER DATE                            
*                                                                               
         CLC   SVCMLRCL,FULL       IS CML RECALL BEFORE FTD                     
         BNL   VAPR20                                                           
         MVC   DUB,SVCMLRCL                                                     
         TM    COMML,CMLAPRSW      CHK APR ON PREV ASGND CML                    
         BZ    MAXDTERR                                                         
         OI    COMML,MAXDTE        BAD DATES                                    
         B     VAPRX                                                            
*                                                                               
VAPR20   DS    0H                                                               
         CLI   CMLBBBAG,0          LEO CML?                                     
         BNE   *+12                                                             
         CLI   SVT2PR9,C'Y'        IS THIS BRAND AGENCY                         
         BE    *+12                                                             
         CLI   CMLBBBAG,C'Y'       LEO B. CML                                   
         BNE   VAPRXIT              NO, DONE                                    
*                                                                               
         OC    CMLBBCAD,CMLBBCAD   ANY CLIENT APPROVAL DATE?                    
         BNZ   VAPR22                                                           
         TM    COMML,CMLAPRSW      CHK APR ON PREV ASGND CML                    
         BZ    CADTERR              NO, ERORR                                   
         OI    COMML,CADTE         BAD DATES                                    
         B     VAPRX                                                            
*                                                                               
VAPR22   CLI   CMLBBAPR,C'Y'       BROADCAST BUSINESS APPROVAL?                 
         BE    VAPR24                                                           
         CLI   CMLATAIR,C'Y'       APPROVED TO AIR                              
         BE    VAPRXIT                                                          
         TM    COMML,CMLAPRSW      CHK APR ON PREV ASGND CML                    
         BZ    NOAIRERR            NOT APPROVED TO AIR                          
         OI    COMML,NOAIR                                                      
         B     VAPRX                                                            
*                                                                               
VAPR24   MVC   SVBBREF,CMLBBREF                                                 
         OC    CMLBBREF,CMLBBREF   ANY CML REFERENCE?                           
         BNZ   VAPR26                                                           
         TM    COMML,CMLAPRSW      CHK APR ON PREV ASGND CML                    
         BZ    NOAIRERR            NOT APPROVED TO AIR                          
         OI    COMML,NOAIR                                                      
         B     VAPRX                                                            
*                                                                               
* CHECK IF CML FROM REFERENCE FIELD IS APPROVED TO AIR                          
*                                                                               
VAPR26   XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CMLKEY,R6                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD AND BCLT                                        
         MVC   CMLKCML,SVBBREF                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VAPR28                                                           
         TM    COMML,CMLAPRSW      CHK APR ON PREV ASGND CML                    
         BZ    NOAIRERR            NOT APPROVED TO AIR                          
         OI    COMML,NOAIR                                                      
         B     VAPRX                                                            
*                                                                               
VAPR28   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'90'        BROADCAST BUSINESS ELEMENT                   
*                                                                               
         BRAS  RE,GETEL                                                         
         BNE   VAPR30                                                           
*                                                                               
         USING CMLBBEL,R6                                                       
*                                                                               
         CLI   CMLATAIR,C'Y'                                                    
         BE    VAPRX10                                                          
*                                                                               
VAPR30   TM    COMML,CMLAPRSW      CHK APR ON PREV ASGND CML                    
         BZ    NOAIRERR            NOT APPROVED TO AIR                          
         OI    COMML,NOAIR                                                      
VAPRX    TM    COMML,NOAIR+MAXDTE+CADTE                                         
         BZ    *+14                                                             
         MVC   CMLCD,=C'REASSIGN'                                               
         OI    TABLESW,REASGN      REASSIGN CML                                 
VAPRXIT  XIT1                                                                   
*                                                                               
* RESTORE ORIGINAL COMMERCIAL                                                   
*                                                                               
VAPRX10  XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD AND BCLT                                        
         MVC   CMLKCML,SVCML                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                BETTER BE THERE                              
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
*                                                                               
         B     VAPRXIT                                                          
*                                                                               
INVCML   MVI   ERROR,INVCOMM                                                    
         XC    DMCB(24),DMCB                                                    
         LA    R1,=C'DMWRT'                                                     
         ST    R1,DMCB                                                          
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    MOVE TERMINAL NUMBER                         
         MVI   DMCB+8,2            SET PAGE 2                                   
         GOTO1 DATAMGR,DMCB,,=C'TEMPSTR',,ASVSTOR                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ERREX                                                            
*                                                                               
CADTERR  MVC   GERROR,=Y(NOCLADTE) NO CLIENT APPROVAL DATE                      
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,9                                                           
         MVC   ELEM+1(8),SVCML                                                  
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST                                                     
         B     VTRAX                                                            
*                                                                               
NOAIRERR MVC   GERROR,=Y(NAPRTAIR) NOT APPORVED TO AIR                          
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,9                                                           
         MVC   ELEM+1(8),SVCML                                                  
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST                                                     
VTRAX    GOTO1 VTRAERR                                                          
*                                                                               
MAXDTERR L     R4,=A(MAXDTMSG)                                                  
         A     R4,SPTRRR                                                        
         GOTO1 DATCON,DMCB,(3,DUB),(5,30(R4))                                   
         GOTO1 (RF),(R1),(3,DUB+3),(5,39(R4))                                   
         L     R1,=A(MAXDTMSG)                                                  
*                                                                               
         CLC   =X'FFFFFF',DUB+3    CK FOR UFN RECALL                            
         BNE   *+10                                                             
         MVC   39(11,R4),=CL11'UFN) *'                                          
         XC    CONHEAD,CONHEAD                                                  
         A     R1,SPTRRR                                                        
         BCTR  R1,0                                                             
         IC    RF,0(R1)                                                         
         EX    RF,ERRORMVC                                                      
         GOTO1 ERREX2                                                           
ERRORMVC MVC   CONHEAD(0),1(R1)                                                 
*                                                                               
         DC    AL1(L'MAXDTMSG-1)                                                
MAXDTMSG DC    C'* ERROR * SPOT OUTSIDE COMML (MONDA/YR-MONDA/YR) *'            
*                                                                               
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* SUBROUTINE VALIDATES START/END DATES FOR PERIOD                               
*================================================================               
                                                                                
VPER     NTR1  BASE=*,LABEL=*                                                   
         XC    SVPERDTS,SVPERDTS                                                
         CLI   8(R2),C'?'          IF QUESTION MK, TELL MEL FLT DATES           
         BNE   VPER30                                                           
         CLI   SVPROF+10,C'E'      COPY CODE = EST                              
         BE    VPER26                                                           
         CLI   5(R2),1             SEE IF DATE ENTERED TOO                      
         BE    VPER04              NO                                           
         GOTO1 DATVAL,DMCB,9(R2),SVQSTART                                       
         L     R4,DMCB             GET LENGTH OF FIELD                          
         LTR   R4,R4                                                            
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,SVQSTART),(3,SVPERST)                             
         B     VPER06                                                           
VPER04   GOTO1 DATCON,DMCB,(5,0),(3,SVPERST)                                    
*                                                                               
VPER06   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A27'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEY+5(1),BPRD       TRY FOR PRODUCT SPECIFIC RECORD              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER10                                                           
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVI   KEY+5,0             CLEAR PRD TO TRY FOR CLT DATA                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
VPER10   CLC   KEY(6),KEYSAVE                                                   
         BNE   FLTRECER                                                         
         CLC   SVPERST,KEY+6       FIRST TLCST DATE TO RECORD END DATE          
         BNH   VPER12                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER10                                                           
         MVC   KEY,KEYSAVE         GET LAST DATE BEFORE TODAY                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
VPER12   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(5),=C'*END='                                             
         GOTO1 DATCON,DMCB,(3,KEY+6),(5,CONHEAD+5)                              
         LA    R3,4                                                             
         LA    R5,CONHEAD+14                                                    
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
VPER14   BRAS  RE,NEXTEL                                                        
         BNE   VPER20                                                           
         USING FLTDTAEL,R6                                                      
         GOTO1 DATCON,DMCB,(3,FLTSTART),(4,0(R5))                               
         MVI   5(R5),C'-'                                                       
         GOTO1 (RF),(R1),(3,FLTEND),(4,6(R5))                                   
         LA    R5,11(R5)                                                        
         BCT   R3,VPER14                                                        
VPER20   MVI   0(R5),C'*'                                                       
         B     VPERERR                                                          
*                                                                               
VPER26   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(8),=CL8'EST FROM'                                        
         GOTO1 DATCON,DMCB,(3,SVGENST),(5,CONHEAD+9)                            
         MVC   CONHEAD+18(2),=C'TO'                                             
         GOTO1 (RF),(R1),(3,SVGENEND),(4,CONHEAD+21)                            
         B     VPERERR                                                          
*                                                                               
VPER30   CLI   SVPROF+10,C'E'      COPY CODE = EST                              
         BNE   VPER32                                                           
         CLC   =C'ES',8(R2)        USE EST DATES                                
         BNE   VPER32                                                           
         GOTO1 DATCON,DMCB,(3,SVGENST),(5,TRAPER)                               
         MVI   TRAPER+8,C'-'                                                    
         GOTO1 (RF),(R1),(3,SVGENEND),(5,TRAPER+9)                              
         GOTO1 (RF),(R1),(3,SVGENST),SVQSTART                                   
         GOTO1 (RF),(R1),(3,SVGENEND),SVQEND                                    
         MVC   SVPERDTS,SVGENDTS                                                
         OI    TRAPERH+6,X'80'                                                  
         MVI   TRAPERH+5,17        RESET LENGTH                                 
         B     VPER60                                                           
*                                                                               
VPER32   LA    R5,8(R2)            START DATE                                   
         GOTO1 DATVAL,DMCB,(R5),SVQSTART                                        
         L     R4,DMCB             GET LENGTH OF FIELD                          
         LTR   R4,R4                                                            
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,SVQSTART),(3,SVPERST)                             
*                                                                               
         MVC   SVQEND,SVQSTART                                                  
         CLM   R4,1,5(R2)          ONLY 1 DATE ENTERED                          
         BE    VPER40              YES                                          
*                                                                               
         LA    R5,1(R4,R5)         POINT TO END DATE                            
         GOTO1 DATVAL,(R1),(R5),SVQEND                                          
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,SVQEND),(3,SVPEREND)                              
         CLC   SVPERST,SVPEREND                                                 
         BH    DATERR                                                           
*                                                                               
VPER40   CLI   SVPROF+10,C'E'      COPY CODE = EST                              
         BE    VPER50                                                           
         BAS   RE,FFLT                                                          
*                                                                               
         B     VPER60                                                           
*                                                                               
* PERIOD DATES SHOULD FALL ENTIRELY WITHIN THIS ESTIMATE *                      
*                                                                               
VPER50   CLC   SVPERST,SVGENEND    PER START AFTER EST END                      
         BH    ESTDTERR                                                         
         CLC   SVPERST,SVGENST     PER START BEFORE EST STR                     
         BL    ESTDTERR                                                         
*                                                                               
         OC    SVPEREND,SVPEREND   ANY END DATE ENTERED                         
         BNZ   VPER54                                                           
         MVC   SVPEREND,SVGENEND   USE EST END DATE                             
         B     VPER56                                                           
*                                                                               
* BOTH DATES GIVEN, END MUST MATCH ESTIMATE END *                               
*                                                                               
VPER54   CLC   SVPEREND,SVGENEND   LAST TLCST MUST BE EST END                   
         BH    ESTDTERR                                                         
*                                                                               
VPER56   GOTO1 DATCON,DMCB,(3,SVPERST),SVQSTART                                 
         GOTO1 (RF),(R1),(3,SVPEREND),SVQEND                                    
*                                                                               
* GET FLIGHT/ESTIMATE/TELECAST DATES IN 2 BYTE FORM *                           
*                                                                               
VPER60   GOTO1 DATCON,DMCB,(3,SVPERST),(2,PERSTP)                               
         GOTO1 (RF),(R1),(3,SVPEREND),(2,PERENDP)                               
*                                                                               
         GOTO1 (RF),(R1),(3,SVPERST),PERSTDT                                    
         GOTO1 (RF),(R1),(3,SVPEREND),PEREDDT                                   
         XIT1                                                                   
*                                                                               
ESTDTERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(37),=CL37'* ERROR * DATE(S) NOT IN EST PERIOD *'         
VPERERR  NI    TRAMEDH+4,X'FF'-X'20' FORCE VALIDATION                           
         GOTO1 ERREX2                                                           
FLTOVLER MVI   ERROR,FLTOVLAP                                                   
         B     TRAPERR1                                                         
FLTELER  MVI   ERROR,NOFLTEL                                                    
         B     TRAPERR1                                                         
DATERR   MVI   ERROR,INVDATE                                                    
         B     TRAPERR1                                                         
FLTRECER MVI   ERROR,NOFLTREC                                                   
TRAPERR1 NI    TRAMEDH+4,X'FF'-X'20' FORCE VALIDATION                           
         GOTO1 ERREX                                                            
         EJECT                                                                  
*================================================================               
* SUBROUTINE DETERMINES FLIGHT DATES FOR GIVEN TELECAST DATES                   
*================================================================               
                                                                                
         DS    0H                                                               
FFLT     NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A27'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEY+5(1),BPRD       TRY FOR PRODUCT SPECIFIC RECORD              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    CHKF2                                                            
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVI   KEY+5,0             CLEAR PRD TO TRY FOR CLT DATA                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
CHKF2    CLC   KEY(6),KEYSAVE                                                   
         BNE   FLTRECER                                                         
         CLC   SVPERST,KEY+6       FIRST TLCST DATE TO RECORD END DATE          
         BNH   CHKF4                                                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     CHKF2                                                            
*                                                                               
CHKF4    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
CHKF6    BRAS  RE,NEXTEL                                                        
         BNE   FLTELER                                                          
*                                                                               
         USING FLTDTAEL,R6                                                      
*                                                                               
         OC    SVPEREND,SVPEREND   TEST END DATE GIVEN                          
         BZ    CHKF10                                                           
*                                                                               
         CLC   SVPERST,FLTEND      FIRST TLCST AFTER FLIGHT END                 
         BH    CHKF6                                                            
         CLC   SVPEREND,FLTSTART    LAST TLCST BEFORE FLIGHT START              
         BL    CHKF6                                                            
         EJECT                                                                  
* TELECAST DATES SHOULD FALL ENTIRELY WITHIN THIS FLIGHT *                      
*                                                                               
         CLC   SVPEREND,FLTEND      LAST TLCST DATE TO FLT END                  
         BH    FLTOVLER                                                         
         CLC   SVPERST,FLTSTART                                                 
         BL    FLTOVLER                                                         
         MVC   SVGENDTS,FLTSTART   SAVE FLT START/END DATES                     
         CLC   SVPERDTS,FLTSTART   TEST FLIGHT = TLCST DATES                    
         BE    CHKF20                                                           
         MVC   SVGENDTS,SVPERDTS   FORCE FLIGHT DATES = TELECAST                
         B     CHKF20                                                           
*                                                                               
* ONLY ONE DATE GIVEN - MATCH FLIGHT START DATE *                               
*                                                                               
CHKF10   CLC   SVPERST,FLTSTART                                                 
         BNE   CHKF6                                                            
*                                                                               
         MVC   SVGENDTS,FLTSTART                                                
*                                                                               
         MVC   SVPEREND,SVGENEND                FORCE END DATE                  
         GOTO1 DATCON,DMCB,(3,SVPEREND),SVQEND  AND REQ END DATE                
*                                                                               
CHKF20   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
* SUBROUTINE TO READ AND FILTER ESTIMATE HEADERS ON REQUESTED DATES             
*====================================================================           
                                                                                
BLEST    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SVESTAB,SVESTAB                                                  
*                                                                               
         GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT SYSTEM                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),BEST                                                    
         CLI   KEY+7,0                                                          
         BNE   *+8                                                              
         MVI   KEY+7,1                                                          
*                                                                               
BLEST2   DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(7),KEYSAVE      SAME A-M/CLT/PRD                             
         BNE   BLEST10                                                          
         OC    KEY+8(5),KEY+8      TEST BILL                                    
         BNZ   BLEST4                                                           
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING ESTHDRD,R6                                                       
*                                                                               
         CLC   ESTART,SVQEND       EST START AFTER REQ END                      
         BH    BLEST4                                                           
         CLC   EEND,SVQSTART       EST END BEFORE REQ START                     
         BL    BLEST4                                                           
         LLC   RE,KEY+7                                                         
         LA    RE,SVESTAB(RE)                                                   
         MVC   0(1,RE),ECOPY       SET COPY CODE IN TABLE                       
         CLI   0(RE),0                                                          
         BNE   *+8                                                              
         MVI   0(RE),X'FF'                                                      
*                                                                               
BLEST4   MVC   KEY+8(5),=5X'FF'    FORCE NEXT EST                               
         CLI   BEST,0              TEST NO ESTIMATE REQUEST                     
         BE    BLEST2               YES - CONTINUE                              
         CLC   KEY+7(1),BEST       TEST PAST END OF SERIES OR ONE               
         BL    BLEST2               NO - CONTINUE                               
*                                                                               
* SET HI AND LOW EST NUMBERS FOR EST=NO *                                       
*                                                                               
BLEST10  GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
*                                                                               
         OC    SVESTAB,SVESTAB     TEST ANY DATA FOUND                          
         BZ    NEQXIT              NO - RETURN WITH CC SET                      
*                                                                               
         CLI   BEST,0              TEST EST=NO REQUEST                          
         BNE   EQXIT                                                            
*                                                                               
         LA    RE,SVESTAB                                                       
         CLI   0(RE),0                                                          
         BNE   *+12                                                             
         LA    RE,1(RE)                                                         
         B     *-12                                                             
         LA    R1,SVESTAB                                                       
         SR    RE,R1                                                            
         STC   RE,BEST                                                          
*                                                                               
         LA    RE,SVESTAB+255                                                   
         LA    R0,255                                                           
         CLI   0(RE),0                                                          
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         BCT   R0,*-10                                                          
         STC   R0,BESTEND                                                       
EQXIT    CR    RE,RE                                                            
         B     BLESTX                                                           
NEQXIT   CR    R7,RB                                                            
BLESTX   XIT1                                                                   
         LTORG                                                                  
         DROP  R6                                                               
         EJECT                                                                  
* SUBROUTINE PRINTS TOTAL SPOTS FOR EACH COMML *                                
* A TABLE IS BUILT IN BLOCK 10 BYTES PER ENTRY *                                
* 0-1 COMML SEQ, 8-9 BINARY CTR                *                                
* WHEN TABLE IS BUILT, COMML SEQ IS CONVERTED  *                                
* TO 8 BYTE COMML CODE                         *                                
*                                                                               
TCML     NTR1  BASE=*,LABEL=*                                                   
         LA    R3,SPTABLE                                                       
         USING SPTABLED,R3                                                      
         XC    BLOCK(250),BLOCK                                                 
         XC    BLOCK+250(170),BLOCK+250                                         
         LA    R5,BLOCK+420        HIGH LIMIT                                   
         SR    R7,R7                                                            
TCML10   OC    SPTCMLSQ,SPTCMLSQ   COMML ASSIGNED                               
         BZ    TCML30                                                           
*                                                                               
         TM    LOG,LOGTREF         THIS PATTERN REF TOTAL                       
         BZ    TCML14                                                           
         OC    SPTPREF,SPTPREF      WAS PATTERN ASSIGNED                        
         BZ    TCML30               NO                                          
*                                                                               
TCML14   LA    R4,BLOCK                                                         
*                                                                               
TCML20   OC    0(10,R4),0(R4)      EMPTY SLOT                                   
         BZ    TCML24                                                           
         TM    LOG,LOGTREF         THIS PATTERN REF TOTAL                       
         BZ    TCML21                                                           
         CLC   SPTPREF,0(R4)                                                    
         BNE   TCML22                                                           
         B     TCML26                                                           
TCML21   CLC   SPTCMLSQ,0(R4)                                                   
         BE    TCML26                                                           
TCML22   LA    R4,10(R4)                                                        
         CR    R4,R5               CK HIGH LIMIT                                
         BL    TCML20                                                           
         B     TCMLERR                                                          
TCML24   MVC   0(2,R4),SPTCMLSQ                                                 
         TM    LOG,LOGTREF         THIS PATTERN REF TOTAL                       
         BZ    *+10                                                             
         MVC   0(2,R4),SPTPREF                                                  
*                                                                               
         BCTR  R7,0                                                             
TCML26   LH    R1,8(R4)                                                         
         LA    R1,1(R1)                                                         
         STH   R1,8(R4)                                                         
*                                                                               
TCML30   OC    SPTCMLS2,SPTCMLS2   COMML ASSIGNED                               
         BZ    TCML50                                                           
         TM    LOG,LOGTREF         THIS PATTERN REF TOTAL                       
         BO    TCML50                                                           
         LA    R4,BLOCK                                                         
*                                                                               
TCML40   OC    0(10,R4),0(R4)       EMPTY SLOT                                  
         BZ    TCML44                                                           
         CLC   SPTCMLS2,0(R4)                                                   
         BE    TCML46                                                           
         LA    R4,10(R4)                                                        
         CR    R4,R5               CK HIGH LIMIT                                
         BL    TCML40                                                           
         B     TCMLERR                                                          
TCML44   MVC   0(2,R4),SPTCMLS2                                                 
         BCTR  R7,0                                                             
TCML46   LH    R1,8(R4)                                                         
         LA    R1,1(R1)                                                         
         STH   R1,8(R4)                                                         
TCML50   LA    R3,SPTNEXT                                                       
         OC    0(3,R3),0(R3)       END OF TABLE                                 
         BNZ   TCML10               NO                                          
         LCR   R7,R7                                                            
         BZ    TCMLZERO                                                         
*                                                                               
         TM    LOG,LOGTREF         THIS PATTERN REF TOTAL                       
         BO    *+8                                                              
         BAS   RE,TCVT             GO CONVERT SEQ TO COMML CODE                 
*                                                                               
         GOTO1 XSORT,DMCB,BLOCK,(R7),10,8,0                                     
         LA    R3,BLOCK                                                         
         SR    R4,R4                                                            
         OI    TRAEST1H+6,X'80'                                                 
TCML60   LA    R2,TRALINES-1                                                    
         LA    R6,TRAEST1(R4)                                                   
         LA    RF,TRAEST1H                                                      
         MVC   0(16,R6),=C'COMMERCIAL TOTALS'                                   
*                                                                               
         TM    LOG,LOGTREF         THIS PATTERN REF TOTAL                       
         BZ    *+10                                                             
         MVC   0(16,R6),=C'PATTRN REF TOTALS'                                   
*                                                                               
         LA    R6,NEXTLINE(R6)                                                  
TCML70   TM    LOG,LOGTREF         THIS PATTERN REF TOTAL                       
         BZ    TCML74                                                           
         MVC   0(4,R6),=C'REF='                                                 
         SR    R0,R0                                                            
         ICM   R0,3,0(R3)                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  4(3,R6),DUB                                                      
         B     TCML76                                                           
TCML74   MVC   0(8,R6),0(R3)                                                    
*                                                                               
TCML76   MVI   8(R6),C'='                                                       
         EDIT  (B2,8(R3)),(3,9(R6)),ALIGN=LEFT                                  
         OI    6(RF),X'80'                                                      
         LA    R3,10(R3)                                                        
         LA    R6,NEXTLINE(R6)                                                  
         LA    RF,NEXTLINE(RF)                                                  
         BCT   R7,TCML80                                                        
         MVC   TRAINFO+40(31),=C'COMML ASSIGNED TOTALS DISPLAYED'               
         TM    LOG,LOGTREF         THIS PATTERN REF TOTAL                       
         BZ    *+10                                                             
         MVC   TRAINFO+40(5),=C'PATTN'                                          
*                                                                               
TCMLX    XIT1                                                                   
TCML80   BCT   R2,TCML70                                                        
         LA    R4,14(R4)           NEXT COL                                     
         CHI   R4,42               CAN ONLY GO 3 COL                            
         BNH   TCML60                                                           
         DC    H'0'                                                             
*                                                                               
TCMLZERO XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TCMLZMS1),TCMLZMS1                                     
         TM    LOG,LOGTREF         THIS PATTERN REF TOTAL                       
         BO    TCMLERX2                                                         
         MVC   CONHEAD(L'TCMLZMS),TCMLZMS                                       
         B     TCMLERX2                                                         
TCMLERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TCMLMS),TCMLMS                                         
TCMLERX2 NI    LOG,X'FF'-LOGTCML-LOGTREF                                        
         OI    LOG,LOGREQD         SET REQUEST DONE (EVEN THOUGH ERROR)         
         XC    DMCB(24),DMCB                                                    
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    MOVE TERMINAL NUMBER                         
         MVI   DMCB+8,2            SET PAGE 2                                   
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,ASVSTOR                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    TRAOPTH+1,X'01'      SET MODIFIED BIT                            
         OI    TRAOPTH+6,X'80'                                                  
         XC    TRAOPT,TRAOPT                                                    
         LA    R2,TRAOPTH                                                       
         NI    TRAMEDH+4,X'FF'-X'20' FORCE VALIDATION                           
         GOTO1 ERREX2                                                           
TCMLMS   DC    C'* ERROR * MORE THAN 42 COMMERCIALS *'                          
TCMLZMS  DC    C'* ERROR * NO COMMLS ASIGNED *'                                 
TCMLZMS1 DC    C'* ERROR * NO PATTERNS USED *'                                  
         EJECT                                                                  
***************************************************                             
* SUBROUTINE FINDS COMMERCIAL CODE FROM COMML SEQ *                             
***************************************************                             
*                                                                               
         DS    0H                                                               
TCVT     NTR1                                                                   
         LA    R2,BLOCK                                                         
TCVT10   XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLPID,=X'0AA1'                                                  
         MVC   CMLPAM(3),BAGYMD AND BCLT                                        
         MVC   CMLPSEQ+1(2),0(R2)                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   0(8,R2),CMLKCML                                                  
         LA    R2,10(R2)                                                        
         OC    0(10,R2),0(R2)                                                   
         BNZ   TCVT10                                                           
         B     TCMLX                                                            
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*****************************************************************               
* SUBROUTINE TO READ PATTERNS, THEN ASSIGN COMMERCIALS TO SPOTS *               
* BUILDS PATTERN TABLE IN AIO3, THEN MOVES 'USED' PAT TO AIO2   *               
*****************************************************************               
*                                                                               
GETPTN   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R3,SPTABLE                                                       
         LA    R1,DPCTBLS          SAVE DONE COPY CODES (IF NEEDED)             
         XC    0(L'DPCTBLS,R1),0(R1) INIT COPY CODE TABLE                       
*                                                                               
         OC    BSTA,BSTA           WAS REQUEST STATION SPECIFIC                 
         BZ    MKTPATER             NO, CAN'T USE PATTERN OPTION                
*                                                                               
         L     R5,AIO2                                                          
         USING PTNLISTD,R5                                                      
         MVI   PATSEQ,0                                                         
*                                                                               
GETP10   XC    0(L'PTNLIST,R5),0(R5) CLEAR ONE ENTRY                            
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A22'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(4),BPRD       PRD/SLN  PTR/SLN                             
         MVC   KEY+9(1),SPTDPT     DAYPART                                      
*                                                                               
         MVC   0(1,R1),SPTDPT      SAVE IN DPC TABLE                            
*                                                                               
         CLI   SVPROF+10,C'D'      COPY CODE = DAYPART ?                        
         BE    GETP14                                                           
         MVC   KEY+9(1),SPTPROGT   PROGRAM ADJ CODE                             
*                                                                               
         MVC   0(1,R1),SPTPROGT    SAVE IN COPY CODE TABLE                      
*                                                                               
         CLI   SVPROF+10,C'A'      COPY CODE = PROG ADJ CODE?                   
         BE    GETP14                                                           
         MVC   KEY+9(1),BEST       ESTIMATE                                     
*                                                                               
GETP14   DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     GETP20                                                           
*                                                                               
GETP16   SR    RE,RE                                                            
         ICM   RE,7,KEY+10         GET REF/SUBL                                 
         SRL   RE,10               DROP SUBLINE                                 
         LA    RE,1(RE)            BUMP LINE NUMBER                             
         SLL   RE,10                                                            
         STCM  RE,7,KEY+10                                                      
         OC    KEY+10(3),KEY+10    TEST REACHED END                             
         BE    GETP30                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
GETP20   CLC   KEY(10),KEYSAVE                                                  
         BNE   GETP30                                                           
*                                                                               
         TM    KEY+13,X'03'        TEST INCOMPLETE OR BPAT                      
         BNZ   GETP16              YES - IGNORE                                 
*                                                                               
         L     R6,AIO1             SET I/O AREA ADDRESS                         
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         BAS   RE,BLDLIST          BUILD PATTERN TABLE ENTRY                    
         B     GETP16                                                           
*                                                                               
GETP30   DS    0H                                                               
         CLI   BEST,0              ANY EST EXPECTED                             
         BE    GETP40               NO                                          
         CLI   SVPROF+10,C'E'      THIS COPY CODE = EST                         
         BE    GETP40               MUST HAVE CODED PTTNS                       
         CLI   SVPROF+10,C'D'      COPY CODE = DAYPART ?                        
         BE    GETP40                                                           
         CLI   SVPROF+10,C'A'      COPY CODE = PROG ADJ CODE?                   
         BE    GETP40                                                           
*                                                                               
         CLI   KEYSAVE+PATKCODE-PATKEY,0 ALREADY DONE NON-COPY CODED            
         BE    GETP40                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(9),KEYSAVE                                                   
         B     GETP14                                                           
*                                                                               
GETP40   LA    R1,DPCTBLS          COPYCODE LIST                                
         LA    RF,L'DPCTBLS(R1)    EOL ADDRESS                                  
*                                                                               
         LA    R3,SPTNEXT                                                       
         OC    0(3,R3),0(R3)       END OF TABLE                                 
         BZ    GETP50                                                           
*                                                                               
         CLI   SVPROF+10,C'D'      COPY CODE = DAYPART ?                        
         BNE   GETP44                                                           
*                                                                               
GETP42   CLC   SPTDPT,0(R1)        DID WE DO THIS ONE YET?                      
         BE    GETP40               YES, SKIP                                   
         LA    R1,1(R1)                                                         
         CR    RF,R1               REACHED END OF TABLE?                        
         BH    *+6                                                              
         DC    H'0'                DAYPART TABLE TOO SMALL                      
         CLI   0(R1),0             EMPTY?                                       
         BNE   GETP42               NO                                          
*                                                                               
         MVC   0(1,R1),SPTDPT      ADD NEXT DPT TO LIST                         
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(9),KEYSAVE      RESTORE THE KEY                              
         MVC   KEY+9(1),SPTDPT     MOVE IN NEXT DAYPART                         
         B     GETP14                                                           
*                                                                               
GETP44   CLI   SVPROF+10,C'A'      COPY CODE = PROG ADJ CODE?                   
         BNE   GETP50                                                           
*                                                                               
GETP46   CLC   SPTPROGT,0(R1)      DID WE DO THIS ONE YET?                      
         BE    GETP40               YES, SKIP                                   
         LA    R1,1(R1)                                                         
         CR    RF,R1               REACHED END OF TABLE?                        
         BH    *+6                                                              
         DC    H'0'                DAYPART TABLE TOO SMALL                      
         CLI   0(R1),0             EMPTY?                                       
         BNE   GETP46                                                           
*                                                                               
         MVC   0(1,R1),SPTPROGT                                                 
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(9),KEYSAVE      RESTORE THE KEY                              
         MVC   KEY+9(1),SPTPROGT   PROGRAM ADJ CODE                             
         B     GETP14                                                           
*                                                                               
GETP50   BAS   RE,BLDDATE          BUILD PATTERN DATE LIST                      
*                                                                               
         L     RE,AIO3             CLEAR COMML TABLE AREA                       
         L     RF,SIZEIO                                                        
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         XIT1                                                                   
*                                                                               
         DROP  R5                                                               
         EJECT                                                                  
*=================================================================              
* SUBROUTINE TO BUILD A PATTERN LIST ENTRY FROM PATTERN RECORD                  
* ON ENTRY R5 POINTS TO NEXT PATTERN LIST SLOT AND PATSEQ                       
*                                  HAS NEXT SEQUENCE NUMBER                     
*=================================================================              
*                                                                               
BLDLIST  NTR1                                                                   
         XC    0(L'PTNLIST+1,R5),0(R5)    CLEAR 1 ENTRY + 1 BYTE                
         USING PTNLISTD,R5                                                      
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PATDTAEL,R6                                                      
*                                                                               
         XC    ELEM,ELEM                                                        
         CLC   SVPEREND,PATSTART   LAST TLCST BEFORE PATTERN START              
         BL    BLDLX                                                            
         CLC   SVPERST,PATEND      FIRST TLCST AFTER PATTERN END                
         BH    BLDLX                                                            
         CLI   1(R6),38            TEST EXTENDED ELEMENT                        
         BNH   BLDL02               NO                                          
         TM    PATSTAT,X'80'       TEST STATUS = DELETED                        
         BO    BLDLX                YES - IGNORE                                
*                                                                               
BLDL02   MVC   ELEM(6),PATSTART    SAVE DATES                                   
         MVC   ELEM+8(1),PATDPT                                                 
         MVC   ELEM+10(1),PATSTAT                                               
*                                                                               
* TEST X'30' ELEMENT FOR HIATUS *                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =C'HIATUS',2(R6)    IF HIATUS, BYPASS                            
         BE    BLDLX                                                            
*                                                                               
* TEST PATTERN APPLIES TO THIS STATION *                                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL            FIND LIST ELEMENT                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PATLSTEL,R6                                                      
*                                                                               
         CLI   2(R6),C'M'          TEST MARKET LIST                             
         BNE   BLDL03                                                           
         OC    PATLST,PATLST       TEST ALL MARKET PATTERN                      
         BNZ   BLDL03                                                           
         MVI   ELCODE,14           SET ALL MKT IND                              
         B     BLDL60                                                           
*                                                                               
BLDL03   LLC   R0,1(R6)                                                         
         SRDL  R0,32                                                            
         D     R0,=F'5'                                                         
         LR    R0,R1               SET FOR BCT                                  
         LA    R6,2(R6)                                                         
*                                                                               
         CLI   0(R6),C'G'          TEST MKT GROUP PATTERN                       
         BNE   BLDL10                                                           
         MVC   WORK(18),KEY                                                     
*                                                                               
BLDL04   GOTO1 VSWITCH,=C'SPT'     SWITCH TO SPOT SYSTEM                        
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(3),BAGYMD                                                  
         MVC   KEY+8(3),1(R6)                                                   
         MVC   KEY+11(2),BMKT                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    BLDL08                                                           
         CLC   KEY(5),KEYSAVE                                                   
         BNE   BLDL05                                                           
         OC    KEY+5(3),KEY+5      IS THIS A PRODUCT GROUP                      
         BZ    BLDL05                                                           
         MVC   KEYSAVE+5(3),KEY+5                                               
         MVC   KEY(13),KEYSAVE                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    BLDL08                                                           
*                                                                               
BLDL05   MVC   KEY(13),KEYSAVE                                                  
         XC    KEY+3(5),KEY+3      TRY NON-CLIENT SPECIFIC                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    BLDL08                                                           
         CLC   KEY(5),KEYSAVE                                                   
         BNE   BLDL06                                                           
         OC    KEY+5(3),KEY+5      IS THIS A PRODUCT GROUP                      
         BZ    BLDL06                                                           
         MVC   KEYSAVE+5(3),KEY+5                                               
         MVC   KEY(13),KEYSAVE                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    BLDL08                                                           
BLDL06   LA    R6,5(R6)                                                         
         BCT   R0,BLDL04                                                        
*                                                                               
         GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
*                                                                               
         MVC   KEY(18),WORK                                                     
         B     BLDLX                                                            
*                                                                               
BLDL08   MVI   ELCODE,12           SET MARKET GROUP ENTRY IND                   
*                                                                               
         GOTO1 VSWITCH,=C'STR'     SWITCH BACK TO TRAFFIC SYSTEM                
*                                                                               
         MVC   KEY(18),WORK                                                     
         B     BLDL60                                                           
*                                                                               
BLDL10   CLI   0(R6),C'M'          TEST MKT PATTERN                             
         BNE   BLDL20                                                           
*                                                                               
* PROCESS MARKET LIST *                                                         
*                                                                               
BLDL12   CLC   BMKT,4(R6)          MATCH MARKET CODES                           
         BE    BLDL14                                                           
         LA    R6,5(R6)                                                         
         BCT   R0,BLDL12                                                        
         B     BLDLX                                                            
BLDL14   MVI   ELCODE,10           SET MARKET ENTRY IND                         
         B     BLDL60                                                           
*                                                                               
* PROCESS AFFILIATE LIST *                                                      
*                                                                               
BLDL20   CLI   0(R6),C'A'          TEST AFFILIATE LIST                          
         BNE   BLDL30                                                           
BLDL22   CLC   SVAFFIL,1(R6)                                                    
         BE    BLDL24                                                           
         LA    R6,5(R6)                                                         
         BCT   R0,BLDL22                                                        
         B     BLDLX                                                            
BLDL24   MVI   ELCODE,8            SET AFFILIATE ENTRY IND                      
         B     BLDL60                                                           
*                                                                               
* PROCESS COMBINED MARKET/AFFILIATE LIST *                                      
*                                                                               
BLDL30   CLI   0(R6),C'C'          TEST COMBINED                                
         BNE   BLDL40                                                           
         LR    RE,R0                                                            
         LR    RF,R6                                                            
BLDL32   CLI   1(R6),0             THIS AN AFFILIATE                            
         BE    BLDL33               NO                                          
         CLC   SVAFFIL,1(R6)                                                    
         BE    BLDL34                                                           
BLDL33   LA    R6,5(R6)                                                         
         BCT   R0,BLDL32                                                        
         B     BLDLX                                                            
BLDL34   CLC   BMKT,4(RF)          MATCH MARKET CODES                           
         BE    BLDL36                                                           
         LA    RF,5(RF)                                                         
         BCT   RE,BLDL34                                                        
         B     BLDLX                                                            
BLDL36   MVI   ELCODE,6            SET COMBINED                                 
         B     BLDL60                                                           
*                                                                               
* PROCESS STATION TYPE *                                                        
*                                                                               
BLDL40   CLI   0(R6),C'T'          TEST STATION TYPE LIMIT                      
         BNE   BLDL50                                                           
BLDL42   CLC   SVTYPE,1(R6)                                                     
         BE    BLDL44                                                           
         LA    R6,5(R6)                                                         
         BCT   R0,BLDL42                                                        
         B     BLDLX                                                            
BLDL44   MVI   ELCODE,4            SET STATION TYPE ENTRY IND                   
         B     BLDL60                                                           
*                                                                               
BLDL50   CLI   0(R6),C'S'          TEST STATION LIST                            
         BE    *+6                                                              
         DC    H'0'                                                             
BLDL52   OC    1(2,R6),1(R6)       THIS A CABLE HEAD STA                        
         BNZ   BLDL53                                                           
         CLC   BSTA,3(R6)          THIS FOR THIS STATION                        
         BNE   BLDL53C                                                          
         B     BLDL54                                                           
*                                                                               
BLDL53   CLC   QSTA,1(R6)                                                       
         BE    BLDL54                                                           
BLDL53C  LA    R6,5(R6)                                                         
         BCT   R0,BLDL52                                                        
         B     BLDLX                                                            
BLDL54   MVI   ELCODE,2            SET STATION LIST ENTRY IND                   
*                                                                               
BLDL60   CLI   BEST,0              BY ESTIMATE                                  
         BE    BLDL70               NO                                          
         CLI   KEY+9,0             COPY CODED PATTN                             
         BNE   BLDL70               YES                                         
         LLC   RF,ELCODE                                                        
         LA    RF,14(RF)                                                        
         STC   RF,ELCODE                                                        
*                                                                               
* ADD ENTRY TO PATTERN LIST IF SPACE *                                          
*                                                                               
BLDL70   CLI   OFFLINE,C'Y'                                                     
         BE    BLDL76                                                           
*                                                                               
         L     R0,AIO3                                                          
         AHI   R0,-(L'PTNLIST+1+8)                                              
         CR    R5,R0                     TEST ROOM IN LIST                      
         BH    NOPTNRM                                                          
         XC    0(L'PTNLIST+1,R5),0(R5)   CLEAR 1 ENTRY + 1 BYTE                 
*                                                                               
BLDL76   LLC   RE,PATSEQ           BUMP SEQUENCE NUMBER                         
         LA    RE,1(RE)                                                         
         STC   RE,PATSEQ           AND SAVE                                     
         CLI   PATSEQ,X'FF'                                                     
         BL    *+6                                                              
         DC    H'0'                NO MORE PATTERNS                             
*                                                                               
         MVC   PTNLSEQ,PATSEQ      MOVE SEQUENCE NUMBER                         
         MVC   PTNLTYPE,ELCODE     SET ENTRY TYPE                               
         CLI   SVPROF+10,C'E'      TEST COPYCODE=EST                            
         BNE   BLDL76A                                                          
         MVC   PTNLDPT,ELEM+8                                                   
         CLI   PTNLDPT,0                                                        
         BNE   *+8                                                              
         MVI   PTNLDPT,X'FF'                                                    
*                                                                               
BLDL76A  LA    R1,KEY+5                                                         
         BRAS  RE,FPROD                                                         
         MVC   PTNLPRD,0(RF)           SAVE PROD                                
         MVC   PTNLSLN,KEY+6                SLN                                 
*                                                                               
         CLI   KEY+7,0                                                          
         BE    BLDL77                                                           
         LA    R1,KEY+7                                                         
         BRAS  RE,FPROD                                                         
         MVC   PTNLPRD2,0(RF)          SAVE PROD2                               
         MVC   PTNLSLN2,KEY+8               SLN2                                
*                                                                               
BLDL77   GOTO1 DATCON,DMCB,(3,ELEM),(2,PTNLSTR)                                 
*                                                                               
         CLC   =X'FFFFFF',PTNLEND  TEST PATTERN RUNS UFN                        
         BNE   *+12                                                             
         OI    PTNLFLAG,PTNLFUFN   SET FLAG                                     
         B     BLDL78                                                           
*                                                                               
         GOTO1 (RF),(R1),(3,ELEM+3),(2,PTNLEND)                                 
*                                                                               
BLDL78   MVC   PTNLREF,KEY+10          REF/SUBLINE                              
         MVC   PTNLDSKA,KEY+14         AND DISK ADDRESS                         
         MVC   PTNLCODE,KEY+9               COPY CODE                           
*                                                                               
         CLC   =X'FFFFFF',PTNLEND  TEST PATTERN RUNS UFN                        
         BNE   *+8                                                              
         OI    PTNLFLAG,PTNLFUFN   SET FLAG                                     
*                                                                               
BLDL80   LA    R5,PTNLNEXT           NEXT PATTERN LIST ENTRY                    
*                                                                               
BLDLX    XIT1  REGS=(R5)                                                        
         DROP  R5,R3                                                            
*                                                                               
NOPTNRM  DC    H'0'                                                             
         EJECT                                                                  
*================================================================               
* SUBROUTINE TO PROCESS PATTERN LIST DATA AND BUILD A TABLE                     
* OF PATTERNS THAT APPLY TO EACH DAY IN THE TELECAST PERIOD                     
* WORK    = START OF PERIOD, WORK+6  = END OF PERIOD DATE                       
* WORK+12 = PATTERN START,   WORK+18 = PATTERN END DATE                         
* WORK+24 = EFFECTIVE PATTN STR, WORK+30 = EFF PAT END                          
* AFTER EACH PATTERN IS SET FOR USE, IT IS SEEDED TO SPOTS                      
*================================================================               
                                                                                
BLDDATE  NTR1                                                                   
*                                                                               
         OC    ADTLIST,ADTLIST     TEST HAVE STORAGE FOR DTLIST                 
         BNZ   *+8                                                              
         BRAS  RE,GETSTOR                                                       
*                                                                               
         L     R5,AIO2                                                          
         USING PTNLISTD,R5                                                      
*                                                                               
         CLI   0(R5),0             TEST FOR NO PATTERN                          
         BE    NOPATERR             YES, ALL DONE                               
*                                                                               
         MVI   BYTE,0                                                           
         NI    TABLESW,X'FF'-SPTASGND  INIT SPOT ASSIGNED FLAG                  
         XC    DPTLIST,DPTLIST         CLEAR DPT BY EST TAB                     
*                                                                               
BLDDT2   CLI   SVPROF+10,C'E'      TEST COPY CODE = ESTIMATE                    
         BNE   BLDDT10                                                          
*                                                                               
         LA    RE,DPTLIST          BUILD A LIST OF ALL THE DPTS                 
         LA    RF,L'DPTLIST        FOUND IN PATTERNS                            
*                                                                               
BLDDT2A  CLC   0(1,RE),PTNLDPT     MATCH DPT                                    
         BE    BLDDT4                                                           
         CLI   0(RE),0             TEST EOL                                     
         BE    BLDDT4                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,BLDDT2A                                                       
         DC    H'0'                TOO MANY DIFFERENT DPTS                      
*                                                                               
BLDDT4   CLI   PTNLDPT,X'FF'       TEST THIS IS ALL DPT ENTRY                   
         BE    BLDDT6              YES - MAKE SURE IT'S FIRST IN LIST           
         MVC   0(1,RE),PTNLDPT                                                  
         B     BLDDT8                                                           
*                                                                               
BLDDT6   CLI   DPTLIST,X'FF'       TEST DPT=0 ENTRY YET                         
         BE    BLDDT8              YES - HAVE ONE                               
                                                                                
* MOVE ENTRIES DOWN 1 SO ALL DPT ENTRY IS FIRST                                 
                                                                                
         LM    R0,R1,DPTLIST                                                    
         MVI   DPTLIST,X'FF'                                                    
         STCM  R0,15,DPTLIST+1                                                  
         STCM  R1,14,DPTLIST+5                                                  
*                                                                               
BLDDT8   LA    R5,L'PTNLIST(R5)                                                 
         CLI   0(R5),0                                                          
         BNE   BLDDT2                                                           
*                                                                               
         MVI   DPTINDEX,0                                                       
*                                                                               
BLDDT10  CLI   SVPROF+10,C'D'      COPY CODE = DAYPART ?                        
         BE    *+12                                                             
         CLI   SVPROF+10,C'A'      PROG ADJ CODE?                               
         BNE   BLDDT16                                                          
*                                                                               
         LA    R1,DPCTBLS          COPY CODE LIST                               
         ST    R1,ANEXTCC          SAVE ADDRESS OF NEXT COPY CODE               
         B     BLDDT14                                                          
*                                                                               
BLDDT12  L     R1,ANEXTCC                                                       
         LA    R1,1(R1)                                                         
         ST    R1,ANEXTCC                                                       
*                                                                               
BLDDT14  LA    RF,DPCTBLS                                                       
         LA    RF,L'DPCTBLS(RF)                                                 
         CR    RF,R1                                                            
         BNL   *+6                                                              
         DC    H'0'                PROG ADJ TABLE TOO SMALL                     
*                                                                               
         CLI   0(R1),0             DONE ALL                                     
         BE    BLDDT100             YES                                         
*                                                                               
         MVC   BYTE,0(R1)          COPY CODE                                    
*                                                                               
BLDDT16  L     R0,ADTLIST                                                       
         L     R1,ADTLISTX                                                      
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   ELCODE,14                                                        
         CLI   BEST,0              ANY COPY EXPECTED                            
         BE    BLDDT20                                                          
         MVI   ELCODE,28                                                        
*                                                                               
BLDDT20  CLI   PTNLTYPE,0          TEST FOR EOL                                 
         BE    BLDDT30                                                          
         CLC   PTNLTYPE,ELCODE     ELSE MATCH CODES                             
         BNE   BLDDT26                                                          
*                                                                               
         CLI   BYTE,0              ANY COPY CODE?                               
         BE    *+14                                                             
         CLC   PTNLCODE,BYTE                                                    
         BNE   BLDDT26                                                          
*                                                                               
         CLI   DPTLIST,0           TEST DPTS BY EST ACTIVE                      
         BE    BLDDT22             NO                                           
         LLC   RE,DPTINDEX                                                      
         LA    RE,DPTLIST(RE)                                                   
         CLC   PTNLDPT,0(RE)       MATCH DPT                                    
         BNE   BLDDT26                                                          
                                                                                
* DETERMINE LIMITS OF PATTERN DATES VS TLCST DATES *                            
                                                                                
BLDDT22  LA    R0,PTNLSTR          POINT TO PATTERN START                       
         CLC   PERSTP,PTNLSTR      PERIOD START TO PATTERN START                
         BL    *+8                  LOW - USE PATTERN START DATE                
         LA    R0,PERSTP           ELSE POINT TO FIRST TLCST DATE               
         GOTO1 DATCON,DMCB,(2,(R0)),WORK+12                                     
*                                                                               
         LA    R0,PTNLEND                                                       
         CLC   PERENDP,PTNLEND     PERIOD END TO PATTERN END                    
         BH    *+8                                                              
         LA    R0,PERENDP                                                       
         GOTO1 (RF),(R1),(2,(R0)),WORK+18                                       
                                                                                
* GET DISPLACEMENT TO FIRST DATE IN DATELIST (FROM SVPERST) *                   
                                                                                
         GOTO1 PERVERT,(R1),PERSTDT,WORK+12                                     
         LH    R0,8(R1)            GIVES INCLUSIVE DAYS                         
         BCTR  R0,0                                                             
         MHI   R0,L'DPTLIST        X 8                                          
         L     R6,ADTLIST          POINT TO FIRST DATE                          
         AR    R6,R0                                                            
         LLC   R0,DPTINDEX                                                      
         AR    R6,R0                                                            
*                                                                               
BLDDT24  MVC   0(1,R6),PTNLSEQ     MOVE PATTERN SEQUENCE                        
         LA    R6,L'DPTLIST(R6)                                                 
         OI    TABLESW,SPTASGND    SPOT ASSIGNED                                
*                                                                               
         GOTO1 ADDAY,DMCB,WORK+12,WORK+12,1                                     
         CLC   WORK+12(6),WORK+18  TEST REACHED END LIMIT                       
         BNH   BLDDT24                                                          
*                                                                               
BLDDT26  LA    R5,L'PTNLIST(R5)   NEXT PATTERN LIST ENTRY                       
         B     BLDDT20                                                          
         EJECT                                                                  
*===============================================================                
* IF DAYPARTS ACTIVE, UPDATE DPTINDX AND PROCESS AGAIN                          
* WHEN THAT'S COMPLETE,                                                         
* DECREMENT VALUE IN ELCODE AND RE-PROCESS PATTERN LIST                         
*===============================================================                
                                                                                
BLDDT30  CLI   DPTLIST,0           TEST DPTS ACTIVE                             
         BE    BLDDT30X            NO                                           
         LLC   RE,DPTINDEX                                                      
         LA    RE,1(RE)                                                         
         STC   RE,DPTINDEX                                                      
         CLI   DPTINDEX,L'DPTLIST  MAX 8 DPTS                                   
         BE    BLDDT30X                                                         
         LA    RE,DPTLIST(RE)      POINT TO NEXT SLOT IN LIST                   
         CLI   0(RE),0                                                          
         BE    BLDDT30X            NO MORE DPTS                                 
         L     R5,AIO2             POINT TO FIRST PATTERN                       
         B     BLDDT20                                                          
*                                                                               
BLDDT30X MVI   DPTINDEX,0          RESET POINTERS                               
         L     R5,AIO2                                                          
         L     R6,ADTLIST                                                       
         LLC   R0,ELCODE                                                        
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
         STC   R0,ELCODE                                                        
         LTR   R0,R0                                                            
         BP    BLDDT20                                                          
                                                                                
* ALL LIST ELEMENTS PROCESSED - MAY NOT HAVE *                                  
* PATTERN FOR EVERY DAY IN TELECAST PERIOD   *                                  
                                                                                
         GOTO1 PERVERT,DMCB,PERSTDT,PEREDDT                                     
         LH    R0,8(R1)            NUMBER OF DAYS IN TELECAST PERIOD            
         CHI   R0,371                                                           
         BH    DATSPRER            DATE SPREAD ERROR                            
*                                                                               
         L     RE,ADTLIST                                                       
         MHI   R0,L'DPTLIST                                                     
         AR    RE,R0                                                            
         MVI   0(RE),X'FF'               SET EOL FLAGS FOR EACH DPT             
         MVC   1(L'DPTLIST-1,RE),0(RE)                                          
*                                                                               
         L     R6,ADTLIST          START OF LIST                                
         LH    R0,8(R1)            DAYS IN TLCST PERIOD                         
*                                                                               
BLDDT34  OC    0(L'DPTLIST,R6),0(R6)   TEST NON-ZERO ENTRY                      
         BNZ   BLDDT60                                                          
*                                                                               
BLDDT34X LA    R6,L'DPTLIST(R6)                                                 
         BCT   R0,BLDDT34                                                       
*                                                                               
BLDDT36  CLI   BYTE,0              DOING COPY CODES?                            
         BNE   BLDDT12             YES- PROCESS NEXT                            
         B     NOUPATER            YES - ERROR                                  
         EJECT                                                                  
*==================================================================             
* NOW BUILD LIST OF PATTERNS USED, THEN PROCESS AGAINST SPOTS                   
*==================================================================             
                                                                                
BLDDT60  ST    R6,AFPD             ADDRESS OF FIRST PATTERN DATE                
         STH   R0,AFPDCNT          SAVE DAY COUNT                               
                                                                                
* NOW FIND HOW FAR THIS PATTERN COVERS *                                        
                                                                                
         MVI   DPTINDEX,0          RESET                                        
*                                                                               
BLDDT62  LLC   RE,DPTINDEX                                                      
         AR    RE,R6                                                            
         CLC   0(1,RE),L'DPTLIST(RE)  SAME PATTERN FOR NEXT DAY                 
         BNE   BLDDT64                                                          
         LA    R6,L'DPTLIST(R6)                                                 
         BCT   R0,BLDDT62                                                       
         DC    H'0'                                                             
*                                                                               
BLDDT64  STH   R0,ALPDCNT          SAVE REMAINING COUNT                         
         ST    R6,ALPD             ADDRESS OF LAST PATTERN DATE                 
*                                                                               
         CLI   0(R6),0             THIS A TBA                                   
         BE    BLDDT94              YES, BYPASS                                 
                                                                                
* POINT TO PROPER PATTERN *                                                     
                                                                                
         LLC   R5,0(R6)                                                         
         BCTR  R5,0                                                             
         MHI   R5,PTNLNEXT-PTNLIST                                              
         A     R5,AIO2                                                          
                                                                                
* SET PATTERN DATES TO THOSE USED HERE *                                        
                                                                                
         L     RF,AFPD                                                          
         S     RF,ADTLIST                                                       
         SR    RE,RE                                                            
         LA    R0,L'DPTLIST                                                     
         DR    RE,R0               GIVES NUMBER OF DAYS IN R1                   
         LR    R0,RF                                                            
         GOTO1 ADDAY,DMCB,PERSTDT,WORK+24,(R0)                                  
*                                                                               
         L     RF,ALPD                                                          
         S     RF,ADTLIST                                                       
         SR    RE,RE                                                            
         LA    R0,L'DPTLIST                                                     
         DR    RE,R0                                                            
         LR    R0,RF                                                            
         GOTO1 ADDAY,(R1),,WORK+30,(R0)                                         
*                                                                               
         GOTO1 DATCON,(R1),(0,WORK+24),(2,PTNLSTR)                              
         GOTO1 (RF),(R1),(0,WORK+30),(2,PTNLEND)                                
*                                                                               
         MVC   KEY+14(4),PTNLDSKA                                               
         MVI   RDUPDATE,C'N'                                                    
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ADIDFLAG,C'N'                                                    
         TM    PATSTAT1-PATDTAEL(R6),PATSADID                                   
         BZ    *+8                                                              
         MVI   ADIDFLAG,C'Y'                                                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'        COMMERCIAL LIST ELEMENT                      
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,GCSQ             GET COMML SEQ #'S FOR EACH CMML              
         BZ    DELCMLER            ALL CMLS ARE DELETED ERROR                   
*                                                                               
         MVI   ELCODE,X'32'        ROTATION LIST ELEMENT                        
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LLC   R0,1(R6)                                                         
         LA    R1,2(R6)                                                         
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         STC   R0,ELEM                                                          
         LA    RE,ELEM+1                                                        
*                                                                               
BLDDT70  ICM   RF,1,0(R1)                                                       
         SLL   RF,28                                                            
         SRL   RF,28                                                            
         CLI   0(R1),C'J'                                                       
         BL    *+8                                                              
         LA    RF,9(RF)            J=10, K=11                                   
         BCTR  RF,0                                                             
         STC   RF,0(RE)                                                         
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,BLDDT70                                                       
*                                                                               
         LA    R4,SPTABLE                                                       
         USING SPTABLED,R4                                                      
*                                                                               
BLDDT72  LLC   R0,ELEM             ROTATION LENGTH                              
         LA    R1,ELEM+1                                                        
*                                                                               
BLDDT74  CLI   SVTZPR02,C'Y'       BYPASS PREVIOUSLY ASSIGNED CMLS              
         BNE   BLDDT76                                                          
         TM    SPTFLAG,SPTPRVAS    TEST PREVIOUSLY ASSIGNED                     
         BO    BLDDT90                                                          
*                                                                               
BLDDT76  CLC   PTNLSTR,SPTLTD      THIS SPOT FOR THIS PAT                       
         BH    BLDDT90              NO                                          
         CLC   PTNLEND,SPTFTD      THIS SPOT FOR THIS PAT                       
         BL    BLDDT90              NO                                          
*                                                                               
         CLC   PTNLPRD(8),SPTPROD  SAME PRD/SLN/PRD2/SLN2                       
         BNE   BLDDT90              NO                                          
*                                                                               
         CLI   SVPROF+10,C'D'      COPY CODE = DAYPART ?                        
         BNE   *+14                                                             
         CLC   PTNLCODE,SPTDPT                                                  
         BNE   BLDDT90              NO                                          
*                                                                               
         CLI   SVPROF+10,C'A'      PROG ADJ CODE?                               
         BNE   *+14                                                             
         CLC   PTNLCODE,SPTPROGT                                                
         BNE   BLDDT90              NO                                          
*                                                                               
         CLI   DPTLIST,0           TEST DPTS FOR COPYCODE=EST                   
         BE    BLDDT80             NO                                           
         LLC   RE,DPTINDEX                                                      
         LA    RE,DPTLIST(RE)                                                   
         CLI   0(RE),X'FF'         TEST ALL DPT ENTRY                           
         BE    BLDDT80                                                          
         CLC   SPTDPT,0(RE)        ELSE MATCH DAYPART                           
         BNE   BLDDT90                                                          
*                                                                               
BLDDT80  LLC   RE,0(R1)            ROT POINTER                                  
         SLL   RE,2                TIMES 4                                      
         LA    RF,WORK(RE)         THIS COMML                                   
         OC    0(4,RF),0(RF)       WERE THESE CMLS DELETED                      
         BZ    BLDDT84             YES                                          
*                                                                               
         TM    SPTFLAG,SPTPRVAS    TEST PREVIOUSLY ASSIGNED                     
         BO    BLDDT82             YES                                          
*                                                                               
         LH    RE,UNCMLCT                                                       
         BCTR  RE,0                                                             
         STH   RE,UNCMLCT                                                       
*                                                                               
         OC    2(2,RF),2(RF)       IS THERE A P/B COMML                         
         BZ    BLDDT82              NO                                          
*                                                                               
         LH    RE,UNCMLCT                                                       
         BCTR  RE,0                                                             
         STH   RE,UNCMLCT                                                       
*                                                                               
BLDDT82  MVC   SPTCMLSQ(4),0(RF)   MOVE IN COMML SEQ #'S                        
         SR    RE,RE                                                            
         ICM   RE,7,PTNLREF                                                     
         SRL   RE,10               DROP SUBLINE                                 
         X     RE,=XL4'00003FFF'    CONVERT TO POSITIVE NUMBERS                 
         STCM  RE,3,SPTPREF                                                     
*                                                                               
BLDDT84  LA    R1,1(R1)                                                         
         BCTR  R0,0                                                             
         OC    0(4,RF),0(RF)       WERE THESE CMLS DELETED                      
         BZ    BLDDT92              YES                                         
*                                                                               
BLDDT90  LA    R4,SPTNEXT                                                       
         OC    0(3,R4),0(R4)       END OF SPOT TABLE                            
         BZ    BLDDT94              YES                                         
*                                                                               
BLDDT92  LTR   R0,R0                                                            
         BNZ   BLDDT74                                                          
         B     BLDDT72                                                          
*                                                                               
BLDDT94  CLI   DPTLIST,0           TEST DPTS FOR COPYCODE=EST                   
         BE    BLDDT96             NO                                           
*                                                                               
         LLC   RE,DPTINDEX                                                      
         LA    RE,1(RE)                                                         
         STC   RE,DPTINDEX                                                      
         CLI   DPTINDEX,L'DPTLIST  TEST REACHED MAX                             
         BE    BLDDT96                                                          
         LLC   RE,DPTINDEX                                                      
         LA    RE,DPTLIST(RE)                                                   
         CLI   0(RE),0             TEST ANY MORE IN LIST                        
         BE    BLDDT96                                                          
*                                                                               
         L     R6,AFPD                                                          
         LH    R0,AFPDCNT          RESTORE POINTERS                             
         B     BLDDT62                                                          
*                                                                               
BLDDT96  L     R6,ALPD                                                          
         LH    R0,ALPDCNT                                                       
*                                                                               
         LA    R6,L'DPTLIST(R6)                                                 
         CLI   0(R6),X'FF'         END OF DATE TABLE                            
         BE    BLDDT98             YES                                          
         ST    R6,AFPD             ADDRESS OF FIRST PATTERN DATE                
         BCTR  R0,0                DECREMENT COUNT                              
         MVI   DPTINDEX,0          RESET INDEX TOO, STUPIDO                     
         B     BLDDT62                                                          
*                                                                               
BLDDT98  OI    LOG,LOGPTTN                                                      
         CLI   SVPROF+10,C'D'      COPY CODE = DAYPART ?                        
         BE    BLDDT12              YES, GO PROCESS NEXT DPC                    
         CLI   SVPROF+10,C'A'      PROG ADJ CODE?                               
         BE    BLDDT12              YES GO PROCESS NEXT ADJ CODE                
*                                                                               
BLDDT100 CLI   BYTE,0              DOING COPY CODES?                            
         BE    *+12                 NO                                          
         TM    TABLESW,SPTASGND                                                 
         BE    NOUPATER             NO, ERROR                                   
         XIT1                                                                   
         EJECT                                                                  
*====================================================================           
* READ ALL COMMLS IN A PATTERN, AND SAVE THEIR SEQ #'S IN WORK                  
*====================================================================           
                                                                                
GCSQ     NTR1        GET COMML SEQ #'S FOR EACH CMML                            
*                                                                               
         LA    R5,WORK                                                          
         XC    WORK,WORK                                                        
         LA    R2,2(R6)                                                         
         LLC   R3,1(R6)                                                         
         SRL   R3,4           DIVIDE BY 16-AND DROPS 2 FOR ELEM ID/LEN          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
*                                                                               
GCSQ10   CLC   =X'5C00',0(R2)     TEST  DELETED COMML                           
         BE    GCSQ20                                                           
*                                                                               
         MVC   CMLKID,=X'0A21'                                                  
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   *+10                                                             
         MVC   CMLKID,=X'0AC1'                                                  
*                                                                               
         MVC   CMLKAM(3),BAGYMD AND BCLT                                        
         MVC   CMLKCML,0(R2)                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         TM    CMLSTAT,X'80'       DELETED COMML                                
         BO    GCSQ20                                                           
         MVC   0(2,R5),CMLSEQ+1                                                 
*                                                                               
         OC    8(8,R2),8(R2)       IS THERE A P/B COMML                         
         BZ    GCSQ30               NO                                          
         CLC   =X'5C00',8(R2)     TEST  DELETED COMML                           
         BE    GCSQ20                                                           
         MVC   CMLKCML,8(R2)                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO3                                                          
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         TM    CMLSTAT,X'80'       DELETED COMML                                
         BO    GCSQ20                                                           
         MVC   2(2,R5),CMLSEQ+1                                                 
         B     GCSQ30                                                           
*                                                                               
GCSQ20   XC    0(4,R5),0(R5)                                                    
*                                                                               
GCSQ30   LA    R2,16(R2)                                                        
         LA    R5,4(R5)                                                         
         BCT   R3,GCSQ10                                                        
         MVC   AIO,AIO1                                                         
*                                                                               
         OC    WORK,WORK           WAS ANYTHING FOUND                           
*NOP     BNZ   *+6                                                              
*NOP     DC    H'0'                                                             
         XIT1                                                                   
         EJECT                                                                  
DELCMLER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DELCMLMS),DELCMLMS                                     
         L     RE,AIO1             GET PATTERN REF NUM                          
         ICM   R0,7,10(RE)                                                      
         SRDL  R0,10                                                            
         X     R0,=XL4'00003FFF'                                                
         EDIT  (R0),(5,DUB),ALIGN=LEFT                                          
         LA    R1,CONHEAD                                                       
         MVC   18(3,R1),DUB                                                     
         B     APTNERX                                                          
NOUPATER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOUPATMS),NOUPATMS                                     
         B     APTNERX                                                          
NOPATERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOPATMS),NOPATMS                                       
         B     APTNERX                                                          
MKTPATER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MKTPATMS),MKTPATMS                                     
         B     APTNERX                                                          
DATSPRER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'DATSPRMS),DATSPRMS                                     
APTNERX  NI    TRAMEDH+4,X'FF'-X'20' FORCE VALIDATION                           
         GOTO1 ERREX2                                                           
DATSPRMS DC    C'* ERROR * MAX 371 DAYS IN PERIOD COVERED *'                    
NOPATMS  DC    C'* ERROR * NO PATTERNS FOUND *'                                 
DELCMLMS DC    C'* ERROR * PAT REF=XXX ALL COMMERCIALS DELETED *'               
NOUPATMS DC    C'* ERROR * NO PATTERNS APPLY TO STATION IN PERIOD *'            
MKTPATMS DC    C'* ERROR * MUST BE STATION SPECIFIC FOR PATTERN *'              
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* ALLOCATE ADDITIONAL STORAGE IN T2168C                                         
*==============================================================                 
                                                                                
GETSTOR  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB,X'8C'          SET OVERLAY NUMBER                           
         GOTO1 CALLOV,DMCB,,ATWA                                                
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,0(R1)            GET PHASE ADDRESS                            
         LA    RE,16(RE)           CAN USE FROM +16                             
*                                                                               
         MVC   0(8,RE),=C'DATELIST'                                             
         LA    RE,8(RE)                                                         
         ST    RE,ADTLIST                                                       
         LA    RE,3200(RE)         ALLOW 1600 BYTES                             
         ST    RE,ADTLISTX                                                      
         J     EXIT                                                             
         LTORG                                                                  
*==============================================================                 
* RTN TO WRITE TWA2 WITH EXTENDED LIST BEFORE EXIT                              
*==============================================================                 
                                                                                
SVTWA    NTR1  BASE=*,LABEL=*                                                   
         LA    R1,=C'DMWRT'                                                     
         LA    R0,TEMPSTR                                                       
         J     COMTWA                                                           
                                                                                
* RESTORE SPTABLE *                                                             
                                                                                
RDTWA    NTR1  BASE=*,LABEL=*                                                   
         LA    R1,=C'DMREAD'                                                    
         LA    R0,TEMPSTR                                                       
*                                                                               
COMTWA   CLI   OFFLINE,C'Y'                                                     
         JE    EXIT                                                             
         XC    DMCB(24),DMCB                                                    
         ST    R1,DMCB                                                          
         ST    R0,DMCB+4                                                        
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    MOVE TERMINAL NUMBER                         
         MVI   DMCB+8,2            SET PAGE 2                                   
         GOTO1 DATAMGR,DMCB,,,,ASVSTOR                                          
         CLI   8(R1),0                                                          
         JE    EXIT                                                             
         DC    H'0'                                                             
*                                                                               
TEMPSTR  DC    C'TEMPSTR'                                                       
         LTORG                                                                  
         EJECT                                                                  
* INCLUDE SPGENCLT                                                              
* INCLUDE SPGENEST                                                              
* INCLUDE SPGENBUY                                                              
* INCLUDE SPGENSTA                                                              
* INCLUDE SPTRCMML                                                              
* INCLUDE SPTRCMML                                                              
* INCLUDE SPTRFLT                                                               
* INCLUDE SPTRPAT                                                               
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRFLT                                                        
       ++INCLUDE SPTRPAT                                                        
         TITLE 'T21640 - SPOT CMML ASSIGN - DSECTS'                             
*   INCLUDE DDSPOOLD                                                            
*   INCLUDE DDSPLWORKD                                                          
*   INCLUDE SPTRAFFD                                                            
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
         PRINT ON                                                               
       ++INCLUDE SPTRAD0D                                                       
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         EJECT                                                                  
GEND     DSECT                                                                  
         ORG   BLOCK                                                            
VTRPACK  DS    A                                                                
AFPD     DS    A                   A(FIRST DATE THIS PTTN)                      
ALPD     DS    A                   A(LAST DATE THIS PTTN)                       
ANEXTCC  DS    A                   ADDRESS OF NEXT COPY CODE                    
ADTLIST  DS    A                                                                
ADTLISTX DS    A                                                                
DPTLIST  DS    CL8                                                              
*                                                                               
AFPDCNT  DS    H                                                                
ALPDCNT  DS    H                                                                
DATECTR  DS    H                                                                
*                                                                               
* FROM BUY ELEMENT                                                              
*                                                                               
ELDATE   DS    H                                                                
ELDATEX  DS    H                                                                
ROTDAYS  DS    H                                                                
PATSEQ   DS    X'00'                                                            
ELCDLO   DS    XL1                                                              
ELCDHI   DS    XL1                                                              
SPOTNUMB DS    XL1                                                              
DPTINDEX DS    X                                                                
SPTWORK  DS    CL(L'SPTDATA)                                                    
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
         DS    0D                                                               
SVESTAB  DS    XL256                                                            
SPTRRR   DS    A                                                                
APTNSTRT DS    A                                                                
ASVSTOR  DS    A                                                                
TOPASVTB DS    A                   1ST TABLE ENTRY ON CURR SCREEN               
NXTASVTB DS    A                   1ST TABLE ENTRY ON NEXT SCREEN               
CURRCML  DS    H                                                                
SVTZPROF DS   0CL16                                                             
SVTZPR01 DS    C                   FLEXIBLE DATES - SPOT GEN                    
SVTZPR02 DS    C                   KEEP PREVIOUSLY ASSIGNED CMLS                
SVTZPR03 DS    C                   SORT BY DATE/TIME                            
         DS    CL14                                                             
SVTBPR04 DS    C                   TB PROFILE 4-ESTIMATES NOT REQUIRED          
SVT2PR9  DS    X                   T2 PROFILE 9-BRAND AGENCY                    
*MNMB                                                                           
SVT3PR06 DS    C                   EXCLUDE DV/SM FROM TRAFFIC                   
*MNMB                                                                           
SVAFFIL  DS    CL3                 AFFILIATE FROM STATION MASTER                
SVTYPE   DS    CL1                 TYPE FROM STATION MASTER                     
*                                                                               
* FROM VPER RTN - PERIOD FIELD IN HEADING ON SCREEN                             
*                                                                               
SVPERDTS DS    0XL6                                                             
SVPERST  DS    XL3                 START DATE FROM PERIOD HEADING               
SVPEREND DS    XL3                 END DATE FROM PERIOD HEADING                 
*                                                                               
* FROM FLIGHT OR ESTIMATE RECORD OR ENTERED TELECAST DATES                      
*                                                                               
PERSTP   DS    XL2                 FLIGHT/TELECAST START DATE                   
PERENDP  DS    XL2                 FLIGHT/TELECAST END DATE                     
*                                                                               
PERSTDT  DS    CL6                                                              
PEREDDT  DS    CL6                                                              
*                                                                               
SVGENDTS DS    0XL6                                                             
SVGENST  DS    XL3                 INSTRUCTION START DATE                       
SVGENEND DS    XL3                 INSTRUCTION END DATE                         
TODAYP   DS    XL2                 TODAY PACKED - USED IN UPD RTN               
SVBBREF  DS    CL8                 CML REFERENCE FOR STARCOM                    
*                                                                               
* KEEP ALL SVPROD TO SVBSLN2 TOGETHER AND IN ORDER *                            
*                                                                               
SVPROD   DS    CL3                                                              
SVBPRD   DS    XL1                                                              
SVBSLN   DS    XL1                                                              
SVPROD2  DS    CL3                                                              
SVBPRD2  DS    XL1                                                              
SVBSLN2  DS    XL1                                                              
SVPRDFLG DS    XL1                                                              
*                                                                               
COMML    DS    XL1                 COMMERCIAL SWITCH                            
FOUNDSW  EQU   X'80'               CML IS IN THE TABLE                          
NOAIR    EQU   X'40'               NOT APPROVED TO AIR                          
MAXDTE   EQU   X'20'               MAX DATE ERROR                               
CADTE    EQU   X'10'               CLIENT APPROVAL DATE ERROR                   
CMLAPRSW EQU   X'08'               JUST GET CML APPROVALS                       
*                                                                               
SPOTCT   DS    H                                                                
UNCMLCT  DS    H                   SPOTS WITHOUT CMLS                           
ASGNCT   DS    H                   CML ASGND CNT WHEN CML=PRG REQUESTED         
CMLSEQCD DS    XL2                                                              
CMLSEQC2 DS    XL2                                                              
CMLCD    DS    CL12                COMMERCIAL CODE USED FOR DISPLAY             
LASTCML  DS    CL12                COMMERCIAL CODE FOR VALIDATION               
LASTCML1 DS    CL12                LAST VALID PROD1 COMML CODE                  
SVACML   DS    CL12                SAVE CML TO BE ASSIGNED                      
SVCMLSEQ DS    CL2                      CML SEQ #                               
SVLEN    DS    CL1                      INPUT LENGTH                            
SVPROGNM DS    CL18                     PROGRAM NAME                            
SVPRGADJ DS    CL1                      PROGRAM ADJECENCY CODE                  
LASTSEQ1 DS    XL2                                                              
LASTCML2 DS    CL12                LAST VALID PROD2 COMML CODE                  
LASTSEQ2 DS    XL2                                                              
SVCMLSOL DS    CL1                 SAVED COMML PIGGYBACK/SOLO FIELD             
PRDFLD   DS    XL1                                                              
SVCML    DS    XL8                 SAVE COMMERCIAL                              
SVCMLRCL DS    XL3                 CML RECALL DATE                              
*                                                                               
TABLESW  DS    XL1                 80 = TABLE BUILT                             
TABLEBL  EQU   X'80'               80 = TABLE BUILT                             
TABLERE  EQU   X'40'               40 = SPOT CMLS PRE-ASSIGNED                  
TABLEPT  EQU   X'20'               20 = CML PREV ASSIGNED NOT IN PATTRN         
TBLEUPD  EQU   X'10'               UPDATE BUY REC NEEDED                        
REASGN   EQU   X'08'               REASSIGN CML                                 
ACMLPRG  EQU   X'04'               ASSIGN CML TO A PROGRAM                      
SPTASGND EQU   X'01'               AT LEAST ONE SPOT ASSIGNED                   
*                                                                               
LOG      DS    XL1                                                              
LOGNPOL  EQU   X'80'               NON-POOL BUYS READ IN BLACT RTN              
LOGPTTN  EQU   X'40'               PATTERN SEED REQUEST PROCESSED               
LOGTCML  EQU   X'20'               COMML TOTALS                                 
LOGTREF  EQU   X'10'               PAT REF TOTALS                               
LOGREQD  EQU   X'08'               SPEC REQUEST COMPLETE, DISPLAY SPOTS         
LOGSAVE  EQU   X'04'               SAVE REQUESTED                               
LOGCLR   EQU   X'02'               CLEAR ASSIGNS REQUESTED                      
LOGCREF  EQU   X'01'               CLEAR BY PTN REF REQUESTED                   
*                                                                               
FTPROGLN DS    XL1                           PROGRAM NAME LENGTH                
FTPROGNM DS    CL18                FILTER ON PROGRAM NAME                       
*                                                                               
DPCTBLS  DS    CL100               DAYPART COPY CODE TABLE                      
DPCTBLE  EQU   *                                                                
*                                                                               
PROGNUM  EQU   80                  80 PROGRAM NAME ENTRIES                      
PROGTABL DS    (PROGNUM)XL18       PROGRAM NAME TABLE                           
         DS    0D                                                               
SPTABLE  DS    215XL(L'SPTDATA)    SPOT TABLE BUILD AREA                        
*                                                                               
MAXSPOTS EQU   215                                                              
*                                                                               
* DO NOT USE ANY STORAGE BETWEEN SPTABLE AND ENDSYSD                            
*                                                                               
ENDSYSD  EQU   *                IF THIS ADDRESS >2FD0, PAST END OF SYSD         
*                                                                               
TRALINES EQU   (TRALAST-TRAEST1H)/(TRAEST2H-TRAEST1H)                           
NEXTLINE EQU   TRAEST2H-TRAEST1H                                                
         EJECT                                                                  
* DSECT FOR SPOT ACTIVITY LIST ENTRIES *                                        
*                                                                               
SPTABLED DSECT                                                                  
SPTDATA  DS    0XL41                                                            
SPTSORT  DS    0XL17                                                            
SPTSTA   DS    XL3                 STATION                                      
SPTFTD   DS    XL2                 SPOT FIRST TELECAST DATE                     
SPTPROD  DS    CL3                 PROD                                         
SPTSLN   DS    XL1                 SPOT LENGTH                                  
SPTPROD2 DS    CL3                 PARTNER PROD                                 
SPTSLN2  DS    XL1                 SPOT LENGTH                                  
SPTTIME  DS    XL4                 START/END TIMES                              
*                                                                               
SPTDAY   DS    XL1                 DAYS                                         
SPTTBLN  DS    XL1                 ASSIGNED SPOT TABLE NUMBER 1 ... N           
SPTUSORT DS    0XL7                                                             
SPTDSKAD DS    XL4                 BUY REC DISK ADDR                            
SPTSDT   DS    XL2                 FOR UPDATE SORT, FTD                         
SPTSPTN  DS    XL1                 SPOT NUMBER FOR = DATES                      
SPTDPT   DS    XL1                 DAY PART                                     
SPTEST   DS    XL1                 ESTIMATE                                     
SPTLINE  DS    XL2                 BUY LINE                                     
SPTCMLSQ DS    XL2                 CML SEQ NUMBER                               
SPTCMLS2 DS    XL2                 CML SEQ NUMBER                               
SPTFLAG  DS    XL1                 FLAG BYTE                                    
SPTPRTD  EQU   X'80'               COMMERCIAL 2 PREVIOUSLY ASSIGNED             
SPTPRVAS EQU   X'40'               COMMERCIAL PREVIOUSLY ASSIGNED               
SPTSRCPT EQU   X'20'               SOURCE WAS PATTERN                           
SPTPREF  DS    XL2                 PATTERN REFERENCE (WITHOUT SUBLINE)          
SPTLTD   DS    XL2                 SPOT LAST TELECAST DATE                      
SPTPROGT DS    XL1                 PROGRAM ADJACENCY CODE                       
SPTPRGPT DS    XL1                 POINTER TO PROGRAM NAME TABLE                
SPTNEXT  EQU   *                                                                
*                                                                               
* DSECT FOR COMML TABLE ENTRIES *                                               
*                                                                               
CMLTBLED DSECT                                                                  
CMLTDATA DS    0XL24                                                            
CMLTSQNO DS    XL2                 COMML SEQ NO                                 
CMLTCML  DS    CL12                COMML CODE                                   
CMLTLEN  DS    XL1                 CMML LENGTH                                  
CMLTPROD DS    CL3                 COMML PROD                                   
CMLTSTDT DS    XL2                 CMML RELEASE DATE                            
CMLTRCDT DS    XL2                 CMML RECALL DATE                             
CMLTSOLO DS    CL1                 PIGGYBACK/SOLO CODE                          
         DS    CL1                 SPARE                                        
CMLTNEXT EQU   *                                                                
*                                                                               
PTNLISTD DSECT                                                                  
*                                                                               
PTNLIST  DS    0XL32                                                            
PTNLTYPE DS    XL1                 10=ALL MKT  8=ONE MKT  7=MKT/AFFIL           
*                                   6=AFFIL    4=STA TYPE 2=STATION             
*                                  FF=TBA (NO PATTERN)                          
PTNLSEQ  DS    XL1                 PATTERN SEQ NUM (MIN 1)                      
PTNLPRD  DS    CL3                 PATTERN PRD                                  
PTNLSLN  DS    XL1                         SLN                                  
PTNLPRD2 DS    CL3                         PRD2                                 
PTNLSLN2 DS    XL1                         SLN2                                 
PTNLSTR  DS    XL2                 PATTERN START DATE                           
PTNLEND  DS    XL2                 PATTERN END DATE                             
PTNLREF  DS    XL3                 REF/SUBLINE                                  
PTNLDSKA DS    XL4                 DISK ADDRESS                                 
PTNLCODE DS    XL1                 COPY CODE                                    
PTNLFLAG DS    XL1                                                              
PTNLFUFN EQU   X'40'               PATTERN RUNS UFN                             
PTNLDPT  DS    CL1                                                              
PTNLSTIM DS    XL2                 START TIME                                   
PTNLETIM DS    XL2                                                              
         DS    CL4                 SPARE                                        
PTNLNEXT EQU   *                                                                
*                                                                               
* DSECT FOR DISPLAY LINE FOR SPOTS                                              
*                                                                               
DSPLINED DSECT                                                                  
         DS    CL8                 ALLOW FOR FLDHDR                             
DSPEST   DS    CL3                                                              
         DS    CL1                                                              
DSPLIN   DS    CL3                                                              
         DS    CL1                                                              
DSPDAY   DS    CL7                                                              
         DS    CL1                                                              
DSPTIME  DS    CL11                                                             
         DS    CL1                                                              
DSPPRG   DS    CL7                                                              
         DS    CL1                                                              
DSPDPT   DS    CL1                                                              
         DS    CL1                                                              
DSPPTR   DS    CL7                                                              
         DS    CL1                                                              
DSPDATE  DS    CL5                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'079SPTRA40   02/17/16'                                      
         END                                                                    
