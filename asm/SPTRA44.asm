*          DATA SET SPTRA44    AT LEVEL 096 AS OF 10/25/12                      
*PHASE T21644A                                                                  
         TITLE 'T21644 INSTRUCTION RECAP '                                      
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*                    IN AIO FROM HYPER CONTROLLER GEGENCON-T00A30               
*                    PATTERN RECS READ IN HERE                                  
*                                                                               
*             AIO2 - COMML RECS                                                 
*                                                                               
*             AIO3 - INSTRUCTION RECS READ IN HERE                              
*                                                                               
*             IPTABLE IN SYSD - TABLE OF INSTR LINES/PATTN KEYS                 
*                                                                               
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - WORK REG                                                          
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE                 
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 - UNUSED                                                            
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
*        FLAG VALUES                                                            
*               00 - FIRST TIME                                                 
*               01 - JUST THRU VALKEY, RESET AND START FROM SCRATCH             
*               02 - BEEN THRU LIST, AND LISTMON                                
*               04 - BEEN THRU DISPLAY KEY (SELECT)                             
*               08 - BEEN THRU DISPLAY REC (SELECT)                             
*                                                                               
***********************************************************************         
* NOTE - TO ALLOW MARKET IN STATION, MUST ADD STATION TO TABLE, TO              
*        DO ALL, MUST ALSO ADD MARKET TO FIND SCREEN TO SCREEN REC              
*        AND ELEM.                                                              
         TITLE 'T21644 INSTRUCTION RECAP '                                      
***********************************************************************         
*                                                                               
* LEV 75 BG OCT11/02 NEW INST RECAP RECS                              *         
* LEV 76 BG JAN20/04 SHOW NEW INSFLFAX FOR AMS GEN FAXED-SHOW @       *         
* LEV 77 BG MAR31/04 FIX NEW CABLE STATION MAX                        *         
* LEV 78 BG MAY11/04 FIX NEW CABLE STATION 'TINY BUG'                 *         
* LEV 89 SM DEC28/09 FIX BPAT CML DISPLAY SCREEN                      *         
* LEV 90 SM JAN13/10 CODE AROUND MSUNPK BUG FOR MEDIA N               *         
* LEV 91 MN MAR19/10 FIX BUG WHEN RE-ENTERING LIST FUNCT FROM PAT/DIS *         
* LEV 93 MN JAN11/10 STOP DUMPS WHEN 1)INST/DIS 2) CML RANGE ON PATTN *         
* LEV 95 MN OCT23/12 MORE BANDS AND ADD ESTIMATE NUMBER TO REPORT     *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T21644   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*21644**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA             BASE SCREEN FOR SYSTEM + THIS PROG           
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR44RR                                                      
*                                                                               
         MVC   TRACEIT+6(1),MODE                                                
         GOTO1 DATAMGR,DMCB,=C'DMTRACE',=C'DATA',TRACEIT                        
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    GET ADDRESS OF TRPACK                  
         GOTO1 CALLOV,DMCB                                                      
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,LISTRECS       PRINT OR DISPLAY RECAP ONLINE                
         BE    RP                                                               
         CLI   MODE,PRINTREP       PRINT RECAP OFFLINE                          
         BE    RPR                                                              
         CLI   MODE,DISPKEY        DISPLAY PAT KEY                              
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY PAT REC                              
         BE    DR                                                               
         CLI   MODE,PROCPFK        OR PAT CMMLS/COMMENTS                        
         BE    DK                                                               
         CLI   MODE,RECADD         ADD RECORD ILLEGAL                           
         BE    ADDMODE                                                          
         CLI   MODE,RECDEL         DELETE RECORD ILLEGAL                        
         BE    DELMODE                                                          
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    CHAMODE             NOT HERE                                     
EXIT     XIT1                                                                   
*                                                                               
TRACEIT  DC    X'07',C'MODE= '                                                  
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
*                                                                               
VK       CLI   ACTNUM,ACTADD       IF ACTION ADD, INVALID                       
         BE    ADDMODE                                                          
         CLI   ACTNUM,ACTDEL       IF ACTION DEL, INVALID                       
         BE    DELMODE                                                          
         CLI   ACTNUM,ACTCHA       IF ACTION CHA, INVALID                       
         BE    CHAMODE                                                          
*                                                                               
         LA    R2,CONACTH                                                       
         CLI   ACTNUM,ACTDIS                                                    
         BE    INVLERR                                                          
*                                                                               
         TM    TRRMEDH+4,X'20'                                                  
         BZ    VK10                                                             
         TM    TRRCLTH+4,X'20'                                                  
         BZ    VK10                                                             
         TM    TRRPRDH+4,X'20'                                                  
         BZ    VK10                                                             
         TM    TRRMKSTH+4,X'20'                                                 
         BZ    VK10                                                             
         TM    TRRPERH+4,X'20'                                                  
         BZ    VK10                                                             
         TM    TRRFTRH+4,X'20'                                                  
         BO    EXIT                                                             
*                                                                               
VK10     XC    SVLSTKEY,SVLSTKEY                                                
         MVI   MYSCREEN,0          SET TO START ON BASE SCREEN                  
         LA    R2,TRRMEDH          MEDIA                                        
*                                                                               
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'         SET ON VALIDATED BIT                         
*                                                                               
         LA    R2,TRRCLTH          CLIENT                                       
*                                                                               
         GOTO1 VALICLT                                                          
*                                                                               
* READ TS (SHIPPING) PROFILE *                                                  
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0TS'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,SVTSPROF,DATAMGR                               
*                                                                               
* READ T2 STRAFFIC PROFILE *                                                    
*                                                                               
         MVI   WORK+3,C'2'                                                      
*                                                                               
         GOTO1 (RF),(R1),,ELEM                                                  
*                                                                               
         MVC   SVT2PR04,ELEM+3     SAVE PRINT ROT PERCENTS                      
*                                                                               
         OI    4(R2),X'20'         SET ON VALIDATED BIT                         
*                                                                               
         LA    R2,TRRPRDH         PRODUCT                                       
         MVI   BPRD,0                                                           
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK12                NO - TREAT AS POL                            
         CLC   =C'POL',8(R2)       IS PRD POL                                   
         BE    VK26                                                             
         CLC   =C'ALL',8(R2)       IS PRD ALL (POL)                             
         BNE   VK24                                                             
VK12     MVC   8(3,R2),=C'POL'     MAKE ALL POL                                 
         B     VK26                                                             
*                                                                               
VK24     GOTO1 VALIPRD                                                          
         MVC   PROD,WORK                                                        
         MVC   BPRD,WORK+3         GET BIN PROD                                 
*                                                                               
VK26     OI    4(R2),X'20'         SET ON VALIDATED BIT                         
*                                                                               
         LA    R2,TRRMKSTH         MARKET/STATION                               
         XC    BMKTSTA,BMKTSTA                                                  
         XC    QSTA,QSTA                                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK40                NO                                           
         TM    4(R2),X'08'         IS IT NUMERIC (MKT NUM)                      
         BZ    VK32                                                             
         GOTO1 VALIMKT                                                          
         B     VK40                                                             
*                                                                               
VK32     GOTO1 VALISTA                                                          
         L     R6,AIO1                                                          
         USING STAREC,R6                                                        
         MVC   SVAFFIL,SNETWRK                                                  
*                                                                               
VK40     OI    4(R2),X'20'         SET ON VALIDATED BIT                         
*                                                                               
         LA    R2,TRRPERH          PERIOD                                       
*                                                                               
         BRAS  RE,VPER                                                          
*                                                                               
         OC    PEREND,PEREND       PERIOD                                       
         BNZ   VK44                                                             
         BRAS  RE,VFLT             GO VALIDATE END FLT DATE                     
*                                                                               
VK44     OI    4(R2),X'20'         SET ON VALIDATED BIT                         
*                                                                               
         LA    R2,TRRFTRH          FILTER                                       
*                                                                               
         BRAS  RE,VFTR                                                          
*                                                                               
* NOW BUILD KEY, 1ST FOR INSTR, THEN FOR PATTERN                                
*                                                                               
         MVI   CODE,0                                                           
         BRAS  RE,FRSTINS          FIND FIRST INSTR REC                         
         XC    STARTKEY,STARTKEY                                                
         MVC   STARTKEY,SVKEY                                                   
         L     R4,AIO3                                                          
         LA    R6,24(,R4)                                                       
         BRAS  RE,FINDEL1                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING INSDTAEL,R6                                                      
         LA    R2,INSPTTN          POINT TO PATTERN USED                        
         BAS   RE,BLDPAT           R2-PAT USED, R4-INSKEY, R6-INSDTAEL          
         OI    FLAG,X'01'          SET FIRST TIME FLAG                          
*                                                                               
         MVI   LASTSELN,0          RESET                                        
*                                                                               
* KEY NOW HAS THE 1ST PATTN REC                                                 
* SVKEY HAS THE ADDRESS OF THE 1ST INSTR REC                                    
*                                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
* FORMAT RECAP REPORT                                                           
*                                                                               
RP       TM    FLAG,X'01'          LAST FROM VALKEY                             
         BO    RP10                 YES, START OUT                              
*                                                                               
         TM    FLAG,X'0C'          LAST FROM DISP KEY, REC (SELECT)             
         BNZ   RP20                 YES, FIND SLOT TO START SCREEN              
*                                                                               
         B     RP30                 NO, RETURN FROM NEXT SCREEN                 
*                                                                               
RP10     NI    FLAG,X'F2'          SET OFF VALKEY, DISP KEY, REC BITS           
         L     R4,AIO3                                                          
         LA    R6,24(,R4)                                                       
         MVC   KEY(L'SVKEY),SVKEY                                               
         ST    R4,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         BRAS  RE,SRT              SORT BY PRD/SLN/PRD2/SLN2/FTD/LTD            
         BNE   RPL44                                                            
*                                                                               
         BRAS  RE,FINDEL1                                                       
         MVI   IPTABCT,0                                                        
         MVI   PATNELCT,1                                                       
         B     RPL                 GO DO ONLINE LIST                            
*                                                                               
* NOW HAVE TO FIND SLOT IN LIST (TABLE) WITH SELLISTN LINE NO.                  
* OR, IF AT END OF TABLE, GO FOR NEXT SCREEN, IF ANY                            
*                                                                               
RP20     LLC   R0,IPTABCT                                                       
         LLC   R1,LASTSELN                                                      
         CLI   LASTSELN,0                                                       
         BE    RP30                                                             
         MVC   MYBYTE,LASTSELN                                                  
         MVI   LASTSELN,0          THIS IS A BETTER TIME                        
*                                                                               
         LR    RF,R1                                                            
         LA    RF,1(,RF)           MAKE REL TO 1                                
         CR    R0,RF               IF EQUAL, LAST SEL WAS LAST LINE             
         BE    RP30                GO FOR END OF LIST                           
         MHI   R1,IPTABEND-IPTABLED  POINT TO TABLE ENTRY                       
         LA    R5,IPTABLE(R1)      START OF TABLE                               
         USING IPTABLED,R5                                                      
RP24     XC    HOLDKEY,HOLDKEY                                                  
         MVC   CODE,IPCODE                                                      
*                     BEFORE      R5 MUST PT TO IPTABLE TABLE ENTRY             
*------------------------------------------------------------------             
         OC    IPPDKAD2,IPPDKAD2                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   IPPDKAD2,SPACES         TEMP - LOOKING FOR BUG                   
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   TEMPDSK,IPPDKAD2                                                 
         XC    KEY,KEY                                                          
         MVC   KEY+14(4),IPPDKAD2                                               
         L     R4,AIO3                                                          
         ST    R4,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   KEY,0(R4)                                                        
*                                                                               
         BRAS  RE,GETMKT           SETS STAPRNT,MKTNAME, AFFIL                  
*                                                                               
         XC    SVLSTKEY,SVLSTKEY                                                
         MVC   SVLSTKEY(14),0(R4)                                               
                                                                                
         LLC   R1,MYBYTE                                                        
         MHI   R1,IPTABEND-IPTABLED  POINT TO TABLE ENTRY                       
         LA    R5,IPTABLE(R1)      START OF TABLE                               
         USING IPTABLED,R5                                                      
         LA    R1,IPTABLE                                                       
         MVC   0(IPTABEND-IPTABLED,R1),0(R5)                                    
         LR    R5,R1                                                            
         MVC   PATNELCT,IPIELCTR                                                
                                                                                
         LLC   R1,IPIELCTR                                                      
         L     R6,AIO3                                                          
         MVC   SVKEY,0(R6)                                                      
         MVI   ELCODE,X'10'                                                     
         LA    R6,24(R6)                                                        
         BRAS  RE,SRT                                                           
                                                                                
         BRAS  RE,FINDEL1                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         B     *+8                                                              
RP26     BRAS  RE,FINDEL                                                        
         BCT   R1,RP26                                                          
                                                                                
         USING INSDTAEL,R6                                                      
         LLC   R1,INSDTALN                                                      
         AHI   R1,-(INSPTTN-INSDTAEL) SUB OUT LEN                               
         SR    R0,R0                                                            
         D     R0,=F'7'                                                         
         LTR   R0,R0               SHOULD BE NO REMAINDER                       
         BZ    *+6                                                              
         DC    H'0'                                                             
         STC   R1,PATTNMAX         SAVE PTTN COUNT                              
         LLC   R1,IPIFDCTR                                                      
         STC   R1,PATNFDCT                                                      
         BCTR  R1,0                MAKE REL 0                                   
         MHI   R1,7                                                             
         LA    R2,INSPTTN(R1)                                                   
         L     R4,AIO3                                                          
*------------------------------------------------------------------             
*                     AFTER FINS                                                
*                                 R2 PTS TO INSPTTN                             
*                                 R4 TO KEY IN AIO3                             
*                                 R6 TO CURR ELEM                               
         MVI   IPTABCT,1                                                        
         CLC   PATTNMAX,PATNFDCT   AT END OF THIS INSTR                         
         BH    RPL                 NO, BUT FORMAT LINE                          
         NI    FLAG,X'F3'          SET OFF FLAG                                 
         B     RPL40               YES, GET NEXT                                
         DROP  R5                                                               
         EJECT                                                                  
* NOW HAVE TO FIND END OF TABLE, AND PICK UP WHERE IT LEFT OFF                  
*                                                                               
RP30     LLC   R1,IPTABCT          GET TABLE SIZE                               
         BCT   R1,RP32             STOP AT LAST ENTRY                           
*                                                                               
         BRAS  RE,FRSTINS          FIND FIRST INSTR REC                         
         L     R4,AIO3                                                          
         LA    R6,24(,R4)                                                       
         BRAS  RE,FINDEL1                                                       
         USING INSDTAEL,R6                                                      
         LA    R2,INSPTTN          POINT TO PATTERN USED                        
         BAS   RE,BLDPAT           R2-PAT USED, R4-INSKEY, R6-INSDTAEL          
*                                                                               
         OI    FLAG,X'01'          SET FLAG AS THOUGH KEY CHANGED               
         NI    TRRMEDH+4,X'FF'-X'20' SET OFF VALIDATED BIT                      
         B     EXIT                DONE                                         
RP32     MHI   R1,IPTABEND-IPTABLED  POINT TO TABLE ENTRY                       
         LA    R5,IPTABLE(R1)      START OF TABLE                               
         USING IPTABLED,R5                                                      
         LA    R1,IPTABLE                                                       
         MVC   0(IPTABEND-IPTABLED,R1),0(R5)                                    
         LR    R5,R1                                                            
         MVI   IPTABCT,1                                                        
         MVC   KEY(L'SVKEY),SVKEY                                               
         MVC   CODE,IPCODE                                                      
*                     BEFORE      R5 MUST PT TO IPTABLE TABLE ENTRY             
         BRAS  RE,FINS            FIND INSTR REC, ELEM, LINE                    
*                     AFTER FINS                                                
*                                 R2 PTS TO INSPTTN                             
*                                 R4 TO KEY IN AIO3                             
*                                 R6 TO CURR ELEM                               
*                                                                               
         BAS   RE,FLST             GO FORMAT START OF LIST                      
*                                                                               
         B     RPL20                                                            
         DROP  R5                                                               
         EJECT                                                                  
* FORMAT ONLINE RECAP                                                           
*                                                                               
RPL      BAS   RE,FLST             GO FORMAT START OF LIST                      
*                                                                               
         USING INSKEY,R4                                                        
         USING INSDTAEL,R6                                                      
*                                                                               
         TM    FLAG,X'0C'          THIS A RESTART FROM SELECT                   
         BO    RPL20               YES                                          
         LA    R2,INSPTTN          1ST PATTERN ENTRY                            
         LLC   R1,INSDTALN                                                      
         AHI   R1,-(INSPTTN-INSDTAEL) SUB OUT LEN                               
         SR    R0,R0                                                            
         D     R0,=F'7'                                                         
         LTR   R0,R0               SHOULD BE NO REMAINDER                       
         BZ    *+6                                                              
         DC    H'0'                                                             
         STC   R1,PATTNMAX         SAVE PTTN COUNT                              
         MVI   PATNFDCT,1          SET PATTERN COUNT USED TO 1                  
RPL10    MVI   RREF-1,C' '                                                      
*                                                                               
         CLC   PERSTART,INSLTD-INSPTTN(R2) TO LAST TEL DTE                      
         BH    RPL36               BYPASS                                       
         CLC   PEREND,INSFTD-INSPTTN(R2) TO 1ST TELECAST DATE                   
         BL    RPL36               BYPASS                                       
         LLC   R1,IPTABCT          GET TABLE CT                                 
         LA    R1,1(,R1)                                                        
         STC   R1,IPTABCT                                                       
*                                                                               
RPL20    NI    FLAG,X'F3'          RESET FLAG                                   
         GOTO1 DATCON,DMCB,(2,3(R2)),(5,RPER)                                   
         MVI   RPER+8,C'-'                                                      
         GOTO1 (RF),(R1),(2,5(R2)),(5,RPER+9)                                   
*                                                                               
         CLC   =X'FFFFFF',0(R2)    THIS A TBA REF                               
         BE    RPL22                                                            
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,7,0(R2)          GET PAT REF/SUB                              
         BZ    RPL24               ZERO PAT REF/SUB IS HIATUS                   
         SRL   R0,10                                                            
         X     R0,=X'00003FFF'                                                  
         EDIT  (R0),(5,RREF)                                                    
         BAS   RE,BLDPAT           R2-PAT USED, R4-INSKEY, R6-INSDTAEL          
         B     RPL26                                                            
*                                                                               
RPL22    MVC   RREF,=CL5'  TBA'                                                 
         B     RPL25                                                            
*                                                                               
RPL24    CLI   INSDTAEL,X'30'      SPOT ASSIGN                                  
         BNE   *+14                                                             
         MVC   RREF(5),=C' SPOT'                                                
         B     RPL25                                                            
*                                                                               
         MVC   RREF(5),=C' HIAT'                                                
RPL25    XC    KEY,KEY                                                          
         XC    DMDSKADD,DMDSKADD                                                
*                                                                               
RPL26    TM    INSFLAG,INSFLCOV+INSFLTWX COVER LETTER/TWX                       
         BZ    RPL28                                                            
         MVI   RSRCE,C'C'                                                       
         TM    INSFLAG,INSFLCOV    COVER LETTER                                 
         BO    RPL28                                                            
         MVI   RSRCE,C'T'                                                       
*                                                                               
RPL28    DS   0H                                                                
         TM    INSFLAG,INSFLCGR    THIS A CABLE GROUP STATION?                  
         BZ    RPL30                                                            
*                                                                               
         TM    INSFLAG,INSFLMAR    WAS AMS GEN RUN?                             
         BZ    RPL31                                                            
         MVI   RSRCE,C'A'                                                       
*                                                                               
         TM    INSFLAG,INSFLFAX    WAS IT FAXED?                                
         BZ    RPL31                                                            
*                                                                               
         MVI   RSRCE,C'@'                                                       
         B     RPL31                                                            
*                                                                               
RPL30    DS   0H                                                                
         TM    INSFLAG,INSFLCGR    THIS A CABLE GROUP STATION?                  
         BO    RPL31                                                            
*                                                                               
         TM    INSFLAG,INSFLMAR    WAS MKT GEN RUN?                             
         BZ    RPL31                                                            
         MVI   RSRCE,C'M'                                                       
*                                                                               
         TM    INSFLAG,INSFLFAX    WAS IT FAXED?                                
         BZ    RPL31                                                            
         MVI   RSRCE,C'@'                                                       
*                                                                               
RPL31    DS   0H                                                                
         L     R4,AIO3                                                          
         LLC   R5,IPTABCT          GET TABLE CT                                 
         BCTR  R5,0                                                             
         MHI   R5,IPTABEND-IPTABLED                                             
         LA    R5,IPTABLE(R5)                                                   
         USING IPTABLED,R5                                                      
*                                                                               
         MVC   IPSTA,INSKSTA       STATION NOW IN TABLE                         
         MVC   IPPPRDS,INSPRD1                                                  
         MVC   IPCODE,INSKCOPY                                                  
         MVC   IPPREF(7),0(R2)     REF, 1ST, LAST TELECAST DATES                
         MVC   IPPDKAD,KEY+14      SAVE DISK ADDR OF PATTERN REC                
         MVC   IPPDKAD2,TEMPDSK    SAVE DISK ADDR OF INSTR RECAP REC            
         MVC   IPIELCTR,PATNELCT   SAVE ELEM USED CT                            
         MVC   IPIFDCTR,PATNFDCT   SAVE PATTN USED CT                           
*                                                                               
         CLI   INSDTAEL,X'30'      SPOT                                         
         BNE   *+8                                                              
         OI    IPINSFLG,X'01'                                                   
         DROP  R5                                                               
*                                                                               
         TM    FILTER1,REVISION    SHOW REVISION                                
         BZ    RPL32                                                            
*                                                                               
         MVI   RREF-1,C' '                                                      
         EDIT  (B1,INSREV),(5,RREF),ALIGN=LEFT,ZERO=NOBLANK                     
*                                                                               
RPL32    MVC   SVLSTKEY,INSKEY     SAVE CURRENT KEY                             
         GOTO1 LISTMON             LET CONTROLLER BUILD SCREEN                  
*                                                                               
         LH    R1,RECCT            UPDATE                                       
         LA    R1,1(,R1)                 LINES                                  
         STH   R1,RECCT                       LISTED                            
*                                                                               
RPL36    CLC   PATTNMAX,PATNFDCT   AT END OF THIS INSTR                         
         BNH   RPL40               YES, GET NEXT INSTR                          
         LA    R2,7(,R2)                                                        
         LLC   R1,PATNFDCT                                                      
         LA    R1,1(,R1)                                                        
         STC   R1,PATNFDCT                                                      
         B     RPL10                                                            
*                                                                               
RPL40    BRAS  RE,FINDEL           SEE IF ANOTHER ELEM                          
         BNE   RPL42               NO                                           
         LLC   R1,PATNELCT                                                      
         LA    R1,1(,R1)                                                        
         STC   R1,PATNELCT                                                      
         B     RPL                                                              
*                                                                               
RPL42    MVC   KEY(L'SVKEY),SVKEY  KEY OF LAST INSTR                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),SVKEY       ALL SAME                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
RPL44    MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                 NOW GET NEXT REC                             
*                                                                               
         CLC   KEY(5),SVINSKEY     ID/A-M/CLT                                   
         BNE   RPL70                                                            
         MVC   TEMPDSK,KEY+14                                                   
*                                                                               
         CLC   =C'POL',TRRPRD      ALL/POL PRODS                                
         BE    *+14                                                             
         CLC   KEY+5(1),SVINSKEY+5                                              
         BNE   RPL70                                                            
*                                                                               
         OC    BMKTSTA,BMKTSTA                                                  
         BZ    RPL48                                                            
         TM    TRRMKSTH+4,X'08'    IS IT NUMERIC (MKT NUM)                      
         BO    RPL46                                                            
*                                                                               
         CLI   SVTSPRO1,C'Y'       USE GENERIC STATION FOR MARKET               
         BNE   RPL45                                                            
         CLC   KEY+8(3),SVINSKEY+8  STA ONLY                                    
         BE    RPL50                                                            
         B     RPL44                                                            
*                                                                               
RPL45    CLC   KEY+6(5),SVINSKEY+6  MKT AND STA                                 
         BE    RPL50                                                            
         CLC   =C'POL',TRRPRD      ** DO WE ALLOW THIS ONLINE **                
         BE    RPL44               ***************************                  
         B     RPL70                                                            
*                                                                               
RPL46    CLC   KEY+6(2),SVINSKEY+6  MKT ONLY                                    
         BE    RPL48                                                            
         CLC   =C'POL',TRRPRD      ** DO WE ALLOW THIS ONLINE **                
         BE    RPL44               ***************************                  
         B     RPL70                                                            
*                                                                               
RPL48    CLC   KEY+8(3),SVINSKEY+8  TEST SAME STATION                           
         BE    RPL50                                                            
*                                                                               
         BRAS  RE,GETMKT           SETS STAPRNT,MKTNAME, AFFIL                  
*                                                                               
         OC    AFFFTR,AFFFTR       FILTERING ON AFFILIATE                       
         BZ    RPL50                NO                                          
*                                                                               
         CLC   SVAFFIL,AFFFTR                                                   
         BNE   RPL44                                                            
*                                                                               
RPL50    MVC   SVKEY,KEY                                                        
         L     R4,AIO3                                                          
         ST    R4,AIO                                                           
         LA    R6,24(,R4)                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         BRAS  RE,SRT              SORT BY PRD/SLN/PRD2/SLN2/FTD/LTD            
         BNE   RPL44                NO DATA HERE                                
*                                                                               
         BRAS  RE,FINDEL1                                                       
         BNE   RPL42                                                            
         USING INSDTAEL,R6                                                      
         CLI   INSDTALN,INSLTD-INSDTAEL MUST HAVE AT LEAST 1 ENTRY              
         BH    *+6                                                              
         DC    H'0'                                                             
         MVI   PATNELCT,1          SET PATTERN ELEM COUNT USED TO 1             
         B     RPL                                                              
         DROP  R4,R6                                                            
*                                                                               
RPL60    OC    RECCT,RECCT         WERE ANY LINES LISTED                        
         BNZ   RPL70               YES                                          
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=CL30'* NOTE * NO RECORDS SELECTED *'                
         GOTO1 ERREX2                                                           
*                                                                               
* EXIT CLEARING TABLES AND SET OFF VALIDITY BITS *                              
*                                                                               
RPL70    NI    TRRMEDH+4,X'FF'-X'20'                                            
*                                                                               
         BRAS  RE,FRSTINS          FIND FIRST INSTR REC                         
         L     R4,AIO3                                                          
         LA    R6,24(,R4)                                                       
         BRAS  RE,FINDEL1                                                       
         USING INSDTAEL,R6                                                      
         LA    R2,INSPTTN          POINT TO PATTERN USED                        
         BAS   RE,BLDPAT           R2-PAT USED, R4-INSKEY, R6-INSDTAEL          
*                                                                               
         MVI   FLAG,0              RESET FLAG TO FIRST TIME                     
         B     EXIT                                                             
         EJECT                                                                  
*=========================================================                      
* FORMAT OFFLINE REPORT HERE                                                    
* IO1 USED FOR COMMERCIALS                                                      
* IO2 USED FOR STATION RECORDS/PATTERNS                                         
* IO3 USED FOR INSTRUCTION RECAP                                                
*=========================================================                      
                                                                                
RPR      LAY   R1,HEADING          HEADING LINE FOR REPORT                      
         ST    R1,SPECS            STORE FOR CONTROLLER                         
         LA    R1,HDHK             RECAP HEADLINES ROUTINE                      
         ST    R1,HEADHOOK         STORE FOR CONTROLLER                         
         GOTO1 DATCON,DMCB,(2,PERSTART),USERQSTR                                
         GOTO1 (RF),(R1),(2,PEREND),USERQEND                                    
*                                                                               
* AFTER VALKEY SVKEY HAS INSTRUCTION RECAP KEY *                                
*                                                                               
         MVC   KEY(L'SVKEY),SVKEY  MOVE INST RECAP KEY                          
                                                                                
         CLC   CONWHEN(3),=C'NOW'                                               
         BNE   *+16                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(L'STARTKEY),STARTKEY                                         
                                                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO3                                                         
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              GET INST RECAP RECORD                        
*                                                                               
         LA    R4,KEY                                                           
         USING INSKEY,R4                                                        
         MVC   P+132(25),SPACES                                                 
*                                                                               
         MVC   SVKCLT,INSKCLT                                                   
         MVC   SVKPRD,INSKPRD                                                   
         MVC   SVKMKT,INSKMKT                                                   
         MVC   SVKSTA,INSKSTA                                                   
*                                                                               
OL05     LA    R4,KEY                                                           
         USING INSKEY,R4                                                        
*MNMB                                                                           
         DROP  R6                                                               
         XC    DUB,DUB                                                          
         SR    R0,R0                                                            
         L     R4,AIO                                                           
         MVC   SVPECODE(1),INSKCOPY                                             
         CLI   INSKCOPY,0                                                       
         BE    OL06                                                             
         TM    INSFLAG,X'20'       COPY CODE = ESTIMATE                         
         BZ    OL06                                                             
         LLC   R0,INSKCOPY                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SVPECODE(3),DUB                                                  
         MVC   SVPECODE+3(1),INSKDPT                                            
OL06     DS    0H                                                               
         LA    R4,KEY                                                           
*MNMB                                                                           
         CLC   SVKCLT,INSKCLT                                                   
         BE    OL07                                                             
         MVC   SVKCLT,INSKCLT                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVC   BCLT,INSKCLT                  BCLT                               
*                                                                               
         BRAS  RE,FCLT                       CLTNM                              
         BNE   OLX                                                              
*                                                                               
OL07     CLC   SVKPRD,INSKPRD                                                   
         BE    OL10                                                             
         MVC   SVKPRD,INSKPRD                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVC   BPRD,INSKPRD                  BPRD                               
         LA    R3,SVPRD1                                                        
         MVC   SVPRD1,BPRD                                                      
         MVI   SVPRD1+1,X'00'                                                   
         BAS   RE,PPRD             PUTS 3 CHAR PROD CODE INTO FLD               
         LA    R3,FLD                                                           
         MVC   PROD,FLD                                                         
*                                                                               
OL10     CLC   SVKMKT,INSKMKT                                                   
         BE    OL12                                                             
         MVC   SVKMKT,INSKMKT                                                   
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         BRAS  RE,GETMKT           SETS STAPRNT,MKTNAME                         
*                                                                               
OL12     CLC   SVKSTA,INSKSTA                                                   
         BE    OL30                                                             
         MVC   SVKSTA,INSKSTA                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVC   QSTA,INSKSTA                                                     
*                                                                               
         BRAS  RE,GETMKT          SETS STAPRNT,MKTNAME                          
*                                                                               
*                                GETMKT ROUTINE GETS STAPRNT 1ST,THEN           
         DROP  R4                    - IF MKT = PREV IT SKIPS MKT RTN.          
*                                                                               
* CHECK PERIOD INPUT AGAINST 7 BYTE DATA IN INSDTAEL *                          
*                                                                               
OL30     L     R6,AIO3                                                          
         LA    R6,24(R6)                                                        
         USING INSDTAEL,R6                                                      
*                                                                               
         BRAS  RE,SRT              SORT BY PRD/SLN/PRD2/SLN2/FTD/LTD            
         BNE   OL95                                                             
*                                                                               
         BRAS  RE,FINDEL1                                                       
         BNE   OL95                                                             
         LA    R2,P                                                             
         USING PRTLINE,R2                                                       
*                                                                               
OL32     SR    R0,R0                                                            
         LLC   R1,1(R6)            LEN OF INSDTAEL                              
         AHI   R1,-(INSPTTN-INSDTAEL)                                           
         D     R0,=F'7'            VARIABLE NUM OF 9 BYTE DATA.                 
         LA    R5,INSPTTN                                                       
         B     OL36                                                             
*                                                                               
OL34     L     R5,INSFLDPT         GET ADDR OF LAST FLD                         
         LA    R5,7(R5)            R5 POINTS TO NEXT FLD                        
*                                                                               
OL36     ST    R5,INSFLDPT         SAVE                                         
         STC   R1,INSFLDCT         R1 HAS BCT LIMIT OF BYTE DATA                
         ST    R6,INSRCPEL         SAVE LAST ELEMENT ADDRESS                    
*                                                                               
* PERIODS INPUT                                                                 
                                                                                
         CLC   PERSTART,5(R5)                                                   
         BH    OL90                                                             
         CLC   PEREND,3(R5)                                                     
         BL    OL90                                                             
*                                                                               
*MNMB                                                                           
         MVC   PECODE,SVPECODE                                                  
*MNMB                                                                           
OL38     MVC   SVREFNUM,0(R5)                                                   
         GOTO1 DATCON,DMCB,(2,3(R5)),(5,PPERIOD)                                
         CLI   6(R5),X'FF'                                                      
         BE    OL42                                                             
         GOTO1 (RF),(R1),(2,5(R5)),(5,PPERIOD+9)                                
         MVI   PPERIOD+8,C'-'                                                   
         B     *+10                                                             
OL42     MVC   PPERIOD+10(3),=C'UFN'                                            
*                                                                               
         CLC   =X'FFFFFF',0(R5)    THIS A TBA REF                               
         BNE   *+14                                                             
         MVC   PREF,=C'TBA'                                                     
         B     OL43                                                             
*                                                                               
         ICM   R0,7,0(R5)          GET PAT REF/SUB                              
         BZ    OL43                IF PTN REF/SUB ZERO, HIATUS                  
         SRL   R0,10                                                            
         X     R0,=X'00003FFF'                                                  
         EDIT  (R0),(3,PREF)                                                    
*                                                                               
OL43     TM    INSFLAG,INSFLCOV+INSFLTWX COVER LETTER/TWX                       
         BZ    OL43C                                                            
         MVI   RSRCE,C'C'                                                       
         TM    INSFLAG,X'80'                                                    
         BO    *+8                                                              
         MVI   RSRCE,C'T'                                                       
*                                                                               
OL43C    DS   0H                                                                
         TM    INSFLAG,INSFLCGR    THIS A CABLE GROUP STATION?                  
         BZ    OL43E                                                            
*                                                                               
         TM    INSFLAG,INSFLMAR    WAS AMS GEN RUN?                             
         BZ    OL43E                                                            
         MVI   RSRCE,C'A'                                                       
*                                                                               
         TM    INSFLAG,INSFLFAX    WAS IT FAXED?                                
         BZ    OL43E                                                            
         MVI   RSRCE,C'@'                                                       
*                                                                               
OL43E    DS   0H                                                                
         GOTO1 DATCON,DMCB,(2,INSDATE),(5,PINSTDTE)   INSTRUCT DATE             
*                                                                               
         LR    R3,R2               SAVE P ADDRESS                               
         LA    R4,INSPRD1                                                       
OL44     L     R1,ASVCLIST                                                      
OL45     CLC   0(1,R4),3(R1)                                                    
         BE    OL47                                                             
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '          GET PRD CODE                                 
         BNL   OL45                                                             
         LA    R1,=C'???'                                                       
OL47     MVC   PPROD,0(R1)                                                      
         MVC   PROD,0(R1)                                                       
         EDIT  (B1,1(R4)),(3,PPLEN)                                             
*                                                                               
         BAS   RE,GETPRD           GO GET PRD NAME                              
         MVC   PPRDNM,SVPNAME                                                   
*                                                                               
* SET UP HERE TO READ SECOND PRD-LEN                                            
*                                                                               
         CLI   FRSTIME,C'N'                                                     
         BE    OL50                                                             
         MVI   FRSTIME,C'N'                                                     
         MVC   PRDNM,SVPNAME       SAVE PRIMARY PROD NAME                       
         LA    R4,2(R4)            INCREMENT TO PRT-LEN(INSPRD2)                
         CLI   0(R4),0                                                          
         BE    OL50                                                             
         LA    R2,132(R2)                                                       
         B     OL44                                                             
*                                                                               
OL50     LR    R2,R3               RESTORE P LINE                               
         MVC   PROD,PPROD          RESTORE PRD1                                 
         OC    0(3,R5),0(R5)       IF PTN REF/SUB ZERO, HIATUS                  
         BNZ   OL51                                                             
         CLI   INSDTAEL,X'30'      SPOT ASSIGN                                  
         BNE   *+14                                                             
         MVC   PCMLID(4),=C'SPOT'                                               
         B     OL90                                                             
         MVC   PCMLID(6),=C'HIATUS'                                             
         B     OL90                                                             
*                                                                               
OL51     CLC   =X'FFFFFF',0(R5)    THIS A TBA REF                               
         BNE   OL52                                                             
         MVC   PCMLID(3),=C'TBA'                                                
         B     OL90                                                             
         EJECT                                                                  
* GET PATTERN RECORD                                                            
*                                                                               
OL52     XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A22'                                                  
         MVC   KEY+2(3),BAGYMD AND BCLT                                         
         MVC   KEY+5(4),INSPRD1                                                 
         MVC   KEY+9(1),SVKEY+11   COPY CODE                                    
         MVC   KEY+10(3),SVREFNUM                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    OL54                                                             
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVC   KEY(13),KEYSAVE                                                  
         CLI   KEY+9,0             ANY COPY CODE                                
         BE    OL92                 NO                                          
         MVI   KEY+9,0             BLANK COPY CODE                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   OL92                                                             
*                                                                               
OL54     MVI   RDUPDATE,C'N'                                                    
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'        FIND STATUS ELEM                             
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PATDTAEL,R6                                                      
         MVI   ADIDFLAG,C'Y'                                                    
         TM    PATSTAT1,PATSADID   SET CMMLS ARE ADIDS                          
         BO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'31'                                                     
         BRAS  RE,GETEL                                                         
         BNE   OL56                                                             
         LA    R2,CONWHENH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(33),=CL33'* NOTE * NOT VALID FOR THEATRICAL'             
         GOTO1 ERREX2                                                           
*                                                                               
OL56     XC    ELEM,ELEM           FIND CMML PCT ELEMENT                        
         MVI   ELCODE,X'34'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    OL56X                                                            
*                                                                               
         MVI   ELCODE,X'36'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   OL58                                                             
*                                                                               
OL56X    MVC   ELEM,0(R6)          MOVE PCTEL TO ELEM                           
*                                                                               
OL58     LA    RE,ALPHATAB                                                      
         ST    RE,ALPHA                                                         
*                                                                               
         L     R6,AIO              PATTERN REC IS IN AIO1                       
         MVI   ELCODE,X'30'        GET CMML ELEM                                
         BRAS  RE,GETEL                                                         
         BNE   OL59                                                             
         LA    R3,2(R6)            POINT TO FIRST CMML PAIR                     
         B     OL59A                                                            
*                                                                               
OL59     L     R6,AIO                                                           
         MVI   ELCODE,X'31'        BPAT CMMLS                                   
         BRAS  RE,GETEL                                                         
         BNE   OL72                                                             
         LA    R3,3(R6)                                                         
*                                                                               
OL59A    LLC   R4,1(R6)                                                         
         SRL   R4,4                                                             
         B     OL60                                                             
ALPHATAB DC    C'ABCDEFGHIJKL'                                                  
*                                                                               
OL60     LA    R2,P                                                             
         USING PRTLINE,R2                                                       
*                                                                               
         L     RE,ALPHA                                                         
         MVC   PALPHA(1),0(RE)                                                  
*                                                                               
         BRAS  RE,GETPCT                                                        
*                                                                               
         MVC   PCMLID,0(R3)        FIRST CMML                                   
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   OL62                                                             
         GOTO1 VTRPACK,DMCB,(C'U',0(R3)),PCMLID                                 
*                                                                               
OL62     MVC   SVCML,0(R3)                                                      
         BRAS  RE,GETCMML                                                       
*                                                                               
         OC    8(8,R3),8(R3)       TEST P/B                                     
         BZ    OL66                NO                                           
         MVI   PCMLID-1,C'('                                                    
*                                                                               
         LA    RE,PCMLID+12                                                     
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVI   1(RE),C'-'                                                       
*                                                                               
         LA    R2,132(R2)          SET R2 TO P2                                 
         MVC   SVCML,8(R3)                                                      
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   OL64                                                             
         GOTO1 VTRPACK,DMCB,(C'U',8(R3)),PCMLID                                 
*                                                                               
OL64     LA    RE,PCMLID+12                                                     
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVI   1(RE),C')'                                                       
*                                                                               
         BAS   RE,GETCMML                                                       
*                                                                               
OL66     BAS   RE,PRINT                                                         
         LA    R2,P                                                             
*                                                                               
         L     RE,ALPHA            NEXT CMML CHAR                               
         LA    RE,1(RE)                                                         
         ST    RE,ALPHA                                                         
         LA    R3,16(R3)           NEXT PAIR IN ELEMENT                         
         BCT   R4,OL60                                                          
*                                                                               
OL68     MVI   ELCODE,X'32'                                                     
         L     R6,AIO2             PATTERN RECORD HERE                          
         BAS   RE,GETEL                                                         
         BNE   OL90                                                             
*                                                                               
         USING PATPTNEL,R6                                                      
OL70     MVC   PALPHA-9(8),=C'ROTATION'                                         
         LLC   R1,PATPTNLN                                                      
         AHI   R1,-2               GIVES NUMBER OF LETTERS                      
         LR    R0,R1               SAVE THIS NUMBER                             
         CHI   R1,40               TEST MORE THAN 40 LETTERS                    
         BNH   *+12                                                             
         LA    R1,1(R1)            ROUND UP FOR FIRST LINE                      
         SRL   R1,1                HALF GO ON THE FIRST LINE                    
         SR    R0,R1               GIVES NUMBER REMAINING                       
         BCTR  R1,0                SET FOR EX                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PALPHA(0),2(R6)                                                  
*                                                                               
         LA    R6,1(R1,R6)         POINT TO NEXT TO BE MOVED                    
         LTR   R1,R0               SEE IF ANY MORE TO MOVE                      
         BNP   OL70X               NO                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PALPHA+132(0),2(R6)                                              
*                                                                               
OL70X    BAS   RE,PRINT                                                         
         B     OL90                                                             
*                                                                               
OL72     MVC   PCMLID(25),=C'NO COMMERCIALS IN PATTERN'                         
*                                                                               
OL90     XC    SETEND,SETEND        CLEAR SWITCHES                              
         CLC   P(132),SPACES                                                    
         BE    OL92                                                             
*                                                                               
         BAS   RE,PRINT                                                         
         MVI   SPACING,2                                                        
         BAS   RE,PRINT                                                         
*                                                                               
OL92     L     R6,INSRCPEL         RESTORE LAST RECAP EL ADDR                   
         LLC   R1,INSFLDCT         R1 HAS BCT LIMIT OF BYTE DATA                
         BCT   R1,OL34                                                          
         BRAS  RE,FINDEL           GET NEXT ELEMENT ADDR IN R6                  
         ST    R6,INSRCPEL         SAVE NEW ELEMENT ADDRESS                     
         BE    OL32                BE FROM FINDEL ROUTINE                       
         EJECT                                                                  
                                                                                
OL95     MVI   RDUPDATE,C'N'                                                    
         L     R6,AIO3                                                          
         MVC   KEY(13),0(R6)                                                    
         GOTO1 HIGH                                                             
*                                                                               
OL100    DS    0H                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(3),SVINSKEY      ID/AM                                       
         BNE   OLX                                                              
                                                                                
         CLC   BCLT,KEY+3           HAS CLIENT CODE CHANGED                     
         BL    OLX                                                              
                                                                                
OL110    DS    0H                                                               
         OC    TRRPRD,TRRPRD                                                    
         BZ    OL120                                                            
         CLC   TRRPRD(3),=C'POL'                                                
         BE    OL120                                                            
         CLC   BPRD,KEY+5                                                       
         BNE   OL100                                                            
                                                                                
OL120    DS    0H                                                               
         OC    TRRMKST,TRRMKST                                                  
         BZ    OL140                                                            
         OC    BMKT,BMKT                                                        
         BZ    OL130                                                            
         CLC   BMKT,KEY+6                                                       
         BNE   OL100                                                            
                                                                                
OL130    DS    0H                                                               
         OC    BSTA,BSTA                                                        
         BZ    OL140                                                            
         CLC   BSTA,KEY+8                                                       
         BNE   OL100                                                            
                                                                                
OL140    DS    0H                                                               
         MVC   SVKEY,KEY           SAVE CURRENT KEY                             
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         B     OL05                                                             
                                                                                
OLX      NI    TRRMEDH+4,X'FF'-X'20' SET OFF VALIDATED BIT                      
         B     EXIT                                                             
         EJECT                                                                  
* DISPLAY KEY FOR PATTERN RECORD                                                
*                                                                               
DK       CLI   MODE,PROCPFK                                                     
         BNE   DK2                                                              
         OC    KEY+14(4),KEY+14   TEST HIATUS OR TBA                            
         BZ    DK6                YES - DO NOT ALLOW PFKEY                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC             GET RECORD FOR PROCPFK                        
         B     DK6                                                              
*                                                                               
* NEW RECORD ALWAYS STARTS ON PATTERN SCREEN                                    
*                                                                               
DK2      MVI   MYSCREEN,X'F3'                                                   
         BRAS  RE,GETSCR                                                        
*                                                                               
DK6      L     R4,AIO                                                           
         USING PATKEY,R4                                                        
         XC    WORK(L'TRAMED),WORK                                              
         MVC   WORK(L'QMED),QMED                                                
         CLC   TRAMED,WORK                                                      
         BE    *+14                                                             
         MVC   TRAMED,WORK                                                      
         OI    TRAMEDH+6,X'80'                                                  
*                                                                               
         XC    WORK(L'TRACLT),WORK                                              
         MVC   WORK(L'QCLT),QCLT                                                
         CLC   TRACLT,WORK                                                      
         BE    DK12                                                             
*                                                                               
DK10     MVC   TRACLT,WORK                                                      
         OI    TRACLTH+6,X'80'                                                  
*                                                                               
DK12     XC    TRAPRLN,TRAPRLN     CLEAR PRD                                    
         OI    TRAPRLNH+6,X'80'                                                 
         XC    TRAPTLN,TRAPTLN     PTNR                                         
         OI    TRAPTLNH+6,X'80'                                                 
         XC    TRACODE,TRACODE     CODE                                         
         OI    TRACODEH+6,X'80'                                                 
         XC    TRAREF,TRAREF       REF                                          
         OI    TRAREFH+6,X'80'                                                  
*                                                                               
         OC    KEY+14(4),KEY+14    THIS A HIATUS OR TBA PATTERN                 
         BZ    DKX                  YES                                         
*                                                                               
         LA    R3,PATKPRD          PROD AND SPOT LEN                            
         BAS   RE,PPRD             OUTPUT WILL BE IN FLD                        
         MVC   TRAPRLN,FLD                                                      
*                                                                               
         LA    R3,PATKPRD2         PROD PARTNER AND SPOT LEN                    
         BAS   RE,PPRD             OUTPUT WILL BE IN FLD                        
         MVC   TRAPTLN,FLD                                                      
         XC    WORK(L'TRACODE),WORK                                             
         MVC   WORK(L'PATKCODE),PATKCODE                                        
         CLI   PATKCODE,0                                                       
         BE    DK20                                                             
         LR    R6,R4                                                            
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
         CLI   PATDTALN,38         OLD PAT LEN                                  
         BE    DK20                                                             
         TM    PATSTAT,X'10'       COPY CODE = ESTIMATE                         
         BZ    DK20                                                             
         XC    WORK(L'TRACODE),WORK                                             
         EDIT  (B1,PATKCODE),(3,DMCB),ALIGN=LEFT                                
         XC    WORK(L'TRACODE),WORK                                             
         MVC   WORK(3),DMCB                                                     
         DROP  R6                                                               
*                                                                               
DK20     MVC   TRACODE,WORK                                                     
         ICM   R0,7,PATKREF                                                     
         SRL   R0,10                                                            
         X     R0,=X'00003FFF'                                                  
         STH   R0,BREF                                                          
         EDIT  (R0),(5,REF),ALIGN=LEFT                                          
         MVC   TRAREF,REF                                                       
         OI    FLAG,X'04'          SET FLAG ON FOR DISPLAY KEY                  
         MVC   HOLDKEY,KEY                                                      
*                                                                               
DKX      CLI   MODE,PROCPFK                                                     
         BNE   EXIT                                                             
         B     DR                                                               
         DROP  R4                                                               
         EJECT                                                                  
*===============================================================                
* DISPLAY PATTERN RECORD                                                        
* TABLE WAS COPIED FROM SPTRA03 WHERE CONREC WAS B/P                            
* HERE IT IS ALWAYS I                                                           
*===============================================================                
                                                                                
PFKEYTAB DC    X'C9F3025D'         PATT TO PATT/COMM (FROM T21603)              
         DC    X'C9F303C1'         PATT TO PATT/CMML                            
         DC    X'C95C02F3'         BPAT/CMML TO PATT                            
         DC    X'C95D02F3'         PATT/COMM TO PATT                            
         DC    X'C95D03C1'         PATT/COMM TO PATT/CMML                       
         DC    X'C9C1025D'         PATT/CMML TO PATT/COMM                       
         DC    X'C9C103F3'         PATT/CMML TO PATT                            
         DC    X'FF00'                                                          
*                                                                               
DR       CLI   PFKEY,0                                                          
         BE    DR08                                                             
         OC    KEY+14(4),KEY+14    TEST HIATUS/TBA                              
         BZ    DR04X               YES- IGNORE PFKEYS                           
*                                                                               
         LA    R4,PFKEYTAB         TABLE OF PFKEYS                              
*                                                                               
DR02     CLC   CONREC(1),0(R4)     MATCH RECORD TYPE                            
         BNE   DR04                                                             
         CLC   MYSCREEN,1(R4)      MATCH CURRENT SCREEN                         
         BNE   DR04                                                             
         CLC   PFKEY,2(R4)         MATCH PFKEY                                  
         BE    DR06                                                             
*                                                                               
DR04     LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   DR02                                                             
*                                                                               
DR04X    MVI   PFKEY,0             PFKEY IS INVALID - IGNORE IT                 
         MVI   GENPFOPT,C'Y'       SET TO STAY ON SELECTION                     
         B     DR08                                                             
*                                                                               
DR06     MVC   MYSCREEN,3(R4)      SET NEW SCREEN                               
*                                                                               
         CLI   MYSCREEN,X'C1'      TEST PATT/CMML SCREEN                        
         BNE   DR06X                                                            
         TM    KEY+13,X'01'        TEST BPAT RECORD                             
         BZ    *+8                                                              
         MVI   MYSCREEN,X'5C'      SET FOR BPAT/CMML SCREEN                     
*                                                                               
DR06X    BRAS  RE,GETSCR                                                        
         MVI   GENPFOPT,C'Y'       SET TO STAY ON THIS SELECTION                
*                                                                               
         BRAS  RE,SETFLDS                                                       
*                                                                               
DR08     L     R6,AIO                                                           
*                                                                               
         OC    KEY+14(4),KEY+14    TEST HIATUS OR TBA                           
         BZ    DR10                                                             
                                                                                
         CLI   MYSCREEN,X'5D'      TEST COMMENT DISPLAY                         
         BE    DR110                                                            
*                                                                               
         CLI   MYSCREEN,X'C1'      TEST CMML DISPLAY                            
         BE    *+12                                                             
         CLI   MYSCREEN,X'5C'      TEST BPAT CMML DISPLAY                       
         BNE   DR09                                                             
         BRAS  RE,DCMMLS                                                        
         B     DR120                                                            
                                                                                
* PRINT OUT PATTERN TEXT IF ANY                                                 
                                                                                
DR09     MVI   ELCODE,X'50'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DR09A                                                            
         MVC   TRATXT,2(R6)                                                     
         OI    TRATXTH+6,X'80'                                                  
*                                                                               
DR09A    L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE PATTERN DATA ELEMENT               
         USING PATDTAEL,R6                                                      
         XC    WORK(L'TRADESC),WORK                                             
         MVC   WORK(L'PATDESC),PATDESC                                          
         CLC   TRADESC,WORK                                                     
         BE    *+14                                                             
         MVC   TRADESC,WORK                                                     
         OI    TRADESCH+6,X'80'                                                 
*                                                                               
         LA    R2,TRASTIMH                                                      
         LA    R4,PATSTIM                                                       
         BAS   RE,GOUNTIME                                                      
*                                                                               
         LA    R2,TRAETIMH                                                      
         LA    R4,PATETIM                                                       
         BAS   RE,GOUNTIME                                                      
*                                                                               
         LA    R2,TRADLYH                                                       
         MVI   8(R2),C'N'                                                       
         TM    PATSTAT1,PATSDLY                                                 
         BZ    *+8                                                              
         MVI   8(R2),C'Y'                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,TRADPTH                                                       
         MVC   8(1,R2),PATDPT                                                   
         OI    6(R2),X'80'                                                      
         B     DR09X                                                            
*                                                                               
GOUNTIME NTR1                                                                   
         XC    8(5,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
         OC    0(2,R4),0(R4)       TEST NO TIME PRESENT                         
         JZ    EXIT                                                             
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),0(R4)                                                    
         XC    WORK,WORK                                                        
         GOTO1 UNTIME,DMCB,FULL,WORK                                            
         MVC   8(5,R2),WORK                                                     
         J     EXIT                                                             
*                                                                               
* DISPLAY INVERTED PROD/CMML FIELD                                              
*                                                                               
DR09X    MVI   TRAINVP,C'N'        DEFAULT                                      
         TM    PATSTAT,X'04'       IS INVERTED BIT ON?                          
         BZ    *+8                 NO                                           
         MVI   TRAINVP,C'Y'        YES                                          
         OI    TRAINVPH+6,X'80'    TRANSMIT                                     
*                                                                               
* FIND SAVED INSTR DATES, AND USE THEM IN PLACE OF PATTERN DATES                
*                                                                               
DR10     LLC   R1,SELLISTN         GET LINE NUMBER IN LIST                      
         STC   R1,LASTSELN           SAVE LINE NUMBER                           
*        LA    R1,1(,R1)                                                        
         MHI   R1,IPTABEND-IPTABLED  POINT TO TABLE ENTRY                       
         LA    R5,IPTABLE(R1)                                                   
         USING IPTABLED,R5                                                      
*                                                                               
* SAVE INSTR DATES                                                              
*                                                                               
DR12     DS   0H                                                                
         GOTO1 DATCON,DMCB,(2,INFTCDT),(3,PATSTART)                             
         GOTO1 (RF),(R1),(2,INLTCDT),(3,PATEND)                                 
*                                                                               
         BAS   RE,PPER                                                          
         CLC   TRAPER,WORK                                                      
         BE    *+14                                                             
         MVC   TRAPER,WORK                                                      
         OI    TRAPERH+6,X'80'                                                  
         OC    KEY+14(3),KEY+14    IF HIATUS REC, ZERO KEY                      
         BZ    DR122               YES, PRINT DATES AND HIATUS ONLY             
*                                                                               
         XC    TRAMS1,TRAMS1                                                    
         XC    TRAMS2,TRAMS2                                                    
         XC    TRAMS3,TRAMS3                                                    
         MVI   ELCODE,X'20'        MARKET/STATION LIST                          
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATLSTEL,R6                                                      
         LLC   R1,PATLSTLN                                                      
         AHI   R1,-3                                                            
         SR    R0,R0                                                            
         D     R0,=F'5'                                                         
         LTR   R0,R0                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    R4,R1                                                            
         LA    R5,PATLST                                                        
         LA    R2,TRAMS1H                                                       
         LA    R3,TRAMS1                                                        
*                                                                               
         OC    TRALTY,TRALTY                                                    
         BZ    *+14                                                             
         XC    TRALTY,TRALTY                                                    
         OI    TRALTYH+6,X'80'                                                  
*                                                                               
         CLI   PATLSTTY,C'T'       STATION TYPE                                 
         BNE   DR20                                                             
         MVC   TRAMS1(5),=CL5'TYPE='                                            
DR14     MVC   5(1,R3),0(R5)       MOVE IN STATION TYPE                         
         BCT   R4,DR16                                                          
         B     DR60                                                             
*                                                                               
DR16     MVI   6(R3),C','                                                       
         LA    R3,2(,R3)                                                        
         LA    R5,5(,R5)                                                        
         B     DR14                                                             
*                                                                               
DR20     CLI   PATLSTTY,C'A'       AFFILIATE                                    
         BNE   DR26                                                             
         MVC   TRAMS1(4),=C'AFF='                                               
*                                                                               
DR22     MVC   4(3,R3),0(R5)       MOVE IN AFFILIATE                            
         BCT   R4,DR24                                                          
         B     DR60                                                             
*                                                                               
DR24     MVI   7(R3),C','                                                       
         LA    R3,4(,R3)                                                        
         LA    R5,5(,R5)                                                        
         B     DR22                                                             
*                                                                               
DR26     CLI   PATLSTTY,C'M'                                                    
         BNE   DR28                                                             
         OC    PATLST(5),PATLST                                                 
         BNZ   DR28                                                             
         MVC   TRAMS1(3),=CL3'ALL'                                              
         B     DR60                                                             
*                                                                               
DR28     SR    R2,RA               SAVE ONLY DISPLACEMENT                       
         ST    R2,SVR2                                                          
         AR    R2,RA               RESTORE TO REAL ADDR                         
         LLC   R1,0(R2)                                                         
         LA    R3,0(R1,R2)         START OF NEXT FLD                            
         BCTR  R3,0                SUBTRACT 1 FOR END OF THIS FLD               
         LA    R2,8(,R2)                                                        
*                                                                               
         MVC   TRALTY(1),PATLSTTY                                               
         OI    TRALTYH+6,X'80'                                                  
*                                                                               
DR30     CLI   PATLSTTY,C'C'       COMBINED MARKET AFFILIATE                    
         BE    *+12                 YES                                         
         CLI   PATLSTTY,C'M'       MARKET                                       
         BNE   DR40                 NO, STATION                                 
*                                                                               
         CLI   0(R5),0             THIS AN AFFILIATE                            
         BNE   DR36                 YES                                         
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,3(R5)                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(5),DUB+5(3)                                                 
         CLI   WORK+1,C'0'                                                      
         BNE   DR34                                                             
         CLI   WORK+2,C'0'                                                      
         BNE   DR32                                                             
         MVC   0(2,R2),WORK+3                                                   
         LA    R2,2(R2)                                                         
         B     DR46                                                             
DR32     MVC   0(3,R2),WORK+2                                                   
         LA    R2,3(R2)                                                         
         B     DR46                                                             
*                                                                               
DR34     MVC   0(4,R2),WORK+1                                                   
         LA    R2,4(,R2)                                                        
         B     DR46                                                             
*                                                                               
DR36     MVC   0(3,R2),0(R5)       MOVE IN AFFILIATE                            
         LA    R2,3(,R2)                                                        
         B     DR46                                                             
*                                                                               
DR40     CLI   PATLSTTY,C'G'       MARKET GROUP                                 
         BE    DR44                                                             
         CLI   PATLSTTY,C'S'       STATION LIST                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         OC    0(2,R5),0(R5)       IS THIS A CABLE HEAD STATION                 
         BNZ   DR42                                                             
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'80',(R5)),WORK,WORK+4                             
*                                                                               
         MVC   0(8,R2),WORK+4                                                   
         MVI   4(R2),C'/'                                                       
         MVI   BYTE,C'/'           INDICATE CABLE                               
         LA    R2,7(,R2)                                                        
         CLI   0(R2),C' '                                                       
         BNH   DR46                                                             
         LA    R2,1(,R2)                                                        
         B     DR46                                                             
*                                                                               
DR42     MVC   0(4,R2),0(R5)       MOVE IN 1ST 4 LETTERS                        
         LA    R2,3(,R2)                                                        
         CLI   0(R2),C' '          SEE IF ONLY THREE LETTERS                    
         BNH   *+8                 YES                                          
         LA    R2,1(,R2)           BUMP OVER 4TH LETTER                         
         MVC   BYTE,4(R5)          SAVE MEDIA                                   
*                                                                               
         CLI   4(R5),C'T'          IF TV                                        
         BE    DR46                ONLY PRINT STATION LETTERS                   
*MNMB                                                                           
         CLI   4(R5),C'D'                                                       
         BNE   DR43                                                             
         MVI   0(R2),C'-'                                                       
         MVC   1(1,R2),4(R5)                                                    
         MVI   2(R2),C'V'                                                       
         LA    R2,3(,R2)                                                        
         B     DR46                                                             
*MNMB                                                                           
DR43     MVI   0(R2),C'-'                                                       
         MVC   1(1,R2),4(R5)                                                    
         MVI   2(R2),C'M'                                                       
         LA    R2,3(,R2)                                                        
         B     DR46                                                             
*                                                                               
DR44     MVC   0(1,R2),0(R5)       MOVE IN MARKET GROUP LETTER                  
         UNPK  DUB(5),1(3,R5)                                                   
         MVC   1(4,R2),DUB                                                      
         LA    R2,5(,R2)                                                        
*                                                                               
DR46     LA    R5,5(,R5)           POINT TO NEXT IN PATLST                      
         BCT   R4,DR48                                                          
         B     DR60                                                             
DR48     CR    R2,R3                                                            
         BNH   *+6                                                              
         DC    H'0'                                                             
         LR    R1,R3                                                            
         SR    R1,R2                                                            
         CLI   PATLSTTY,C'G'       MARKET GROUP                                 
         BNE   DR50                 NO                                          
         CHI   R1,7                NEED TO ALLOW 7 SPACES                       
         BNH   DR58                                                             
         B     DR56                                                             
*                                                                               
DR50     CLI   PATLSTTY,C'S'       STATION?                                     
         BNE   DR54                 NO                                          
         CLI   BYTE,C'/'           THIS A CABLE HEAD STATION                    
         BNE   DR52                                                             
         CHI   R1,8                NEED TO ALLOW 8 SPACES                       
         BNH   DR58                                                             
         B     DR56                                                             
*                                                                               
DR52     CLI   BYTE,C'T'           TV                                           
         BE    DR54                                                             
         CHI   R1,7                NEED TO ALLOW 7 SPACES                       
         BNH   DR58                                                             
         B     DR56                                                             
*                                                                               
DR54     CHI   R1,4                ONLY NEED 4                                  
         BNH   DR58                                                             
*                                                                               
DR56     MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         B     DR30                                                             
*                                                                               
DR58     L     R2,SVR2             GET FLD DISPLACEMENT                         
         AR    R2,RA               MAKE REAL ADDR                               
         LLC   R1,0(R2)            GET LENGTH OF THIS FLD                       
         AR    R2,R1               POINT TO NEXT                                
         LLC   R1,0(R2)            GET LENGTH OF THIS FLD                       
         AR    R2,R1               POINT TO NEXT                                
         B     DR28                                                             
*                                                                               
DR60     OI    TRAMS1H+6,X'80'                                                  
         OI    TRAMS2H+6,X'80'                                                  
         OI    TRAMS3H+6,X'80'                                                  
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'        FIND STATUS ELEM                             
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PATDTAEL,R6                                                      
         TM    PATSTAT1,PATSINC    TEST INCOMPLETE FLAG                         
         BO    DR120               YES - STOP NOW                               
*                                                                               
         MVI   ADIDFLAG,C'Y'                                                    
         TM    PATSTAT1,PATSADID   SET CMMLS ARE ADIDS                          
         BO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
*                                                                               
         L     R6,AIO              COMMERCIAL LIST ELEMENT                      
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BE    DR70                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'31'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
*=============================================================                  
* DISPLAY FIRST FOUR COMMERCIALS AND ++MORE IF MORE THAN 4                      
*=============================================================                  
                                                                                
         USING PATCMLEL,R6                                                      
DR70     LA    R2,TRACMLAH         1ST SCREEN FIELD                             
         LLC   R0,PATCMLLN         GET ELEM LEN                                 
         SRL   R0,4                DIV BY 16=NO OF CMML PRS (DROPS ODD)         
*                                                                               
         XC    TRACMOR,TRACMOR                                                  
         OI    TRACMORH+6,X'80'                                                 
         CHI   R0,4                                                             
         BNH   *+14                                                             
         LHI   R0,4                SET TO DISPLAY ONLY FIRST FOUR               
         MVC   TRACMOR,=CL10'++ MORE ++'                                        
*                                                                               
         LA    R5,PATCML           FIRST CMML                                   
         CLI   0(R6),X'31'                                                      
         BNE   *+8                                                              
         LA    R5,1(R5)                                                         
         LA    R3,4                MAX CMML PAIRS ON PTTN SCREEN                
         SR    R3,R0               # OF FLDS TO BLANK                           
*                                                                               
DR72     XC    8(25,R2),8(R2)                                                   
*                                                                               
         MVI   8(R2),C'*'                                                       
         CLC   =X'5C00',0(R5)                                                   
         BE    DR78                                                             
         MVC   8(8,R2),0(R5)                                                    
         OC    8(8,R5),8(R5)                                                    
         BZ    *+14                                                             
         MVI   16(R2),C'-'                                                      
         MVC   17(8,R2),8(R5)                                                   
*                                                                               
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   DR78                                                             
         XC    8(25,R2),8(R2)                                                   
         GOTO1 VTRPACK,DMCB,(C'U',0(R5)),8(R2)                                  
         OC    8(8,R5),8(R5)                                                    
         BZ    DR78                                                             
*                                                                               
         LA    R4,8(R2)                                                         
DR74     CLI   0(R4),C' '                                                       
         BNH   DR76                                                             
         LA    R4,1(R4)                                                         
         B     DR74                                                             
*                                                                               
DR76     MVI   0(R4),C'-'                                                       
         GOTO1 VTRPACK,DMCB,(C'U',8(R5)),1(R4)                                  
*                                                                               
DR78     OI    6(R2),X'80'                                                      
         LA    R5,16(R5)                                                        
         AHI   R2,TRACMLBH-TRACMLAH                                             
         BCT   R0,DR72                                                          
*                                                                               
         LTR   R3,R3               ANY FLDS TO BLANK                            
         BZ    DR90                NO                                           
*                                                                               
DR80     XC    WORK,WORK                                                        
         CLC   8(L'TRACMLA,R2),WORK                                             
         BE    *+14                                                             
         MVC   8(L'TRACMLA,R2),WORK                                             
         OI    6(R2),X'80'                                                      
         LLC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         LLC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         BCT   R3,DR80                                                          
*                                                                               
DR90     B     DR120                                                            
         EJECT                                                                  
*===========================================================                    
* DISPLAY COMMENTS                                                              
*===========================================================                    
                                                                                
DR110    L     R6,AIO                                                           
         LA    R3,4                MAX COMMENTS LINES                           
         LA    R2,PCMCMT1H                                                      
         OC    KEY+14(4),KEY+14    TEST HIATUS                                  
         BZ    DR118                                                            
         MVI   ELCODE,X'40'        COMMENT ELEMENT(S)                           
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
DR112    BRAS  RE,NEXTEL                                                        
         BNE   DR116                                                            
         USING PATCMTEL,R6                                                      
         LLC   R1,PATCMTLN         GET COMMENT ELEM LEN                         
         AHI   R1,-4               GET ACTUAL COMMENT LEN-1                     
         LLC   RF,0(R2)            FLD LEN                                      
         AHI   RF,-9                                                            
         EX    RF,DRXC             CLEAR FIELD BEFORE MOVE                      
         CR    RF,R1               INSURE                                       
         BNL   DR114               NO FLDH                                      
         LR    R1,RF               CLOBBER                                      
*                                                                               
DR114    EX    R1,DRMVCB                                                        
         OI    6(R2),X'80'                                                      
         LLC   R0,0(R2)            GET FLD LENGTH                               
         AR    R2,R0                                                            
         BCT   R3,DR112                                                         
*                                                                               
         BRAS  RE,NEXTEL           SEE IF 5TH COMMENT                           
         BNE   DR120               NO, OK                                       
         DC    H'0'                                                             
*                                                                               
DR116    LTR   R3,R3                                                            
         BZ    DR120                                                            
*                                                                               
DR118    LLC   RF,0(R2)            GET FLD LENGTH                               
         AHI   RF,-9               GET FLD LEN-1                                
         EX    RF,DRXC                                                          
         OI    6(R2),X'80'                                                      
         LA    R2,9(RF,R2)                                                      
         BCT   R3,DR118                                                         
         EJECT                                                                  
DR120    OI    FLAG,X'08'          SET FLAG ON FOR DISPLAY REC                  
         BRAS  RE,SETPFTXT                                                      
         J     EXIT                                                             
*                                                                               
DR122    DS    0H                                                               
         MVC   TRACMLA(10),=C'HIATUS/TBA'                                       
*                                                                               
DR124    OI    TRACMLAH+6,X'80'                                                 
         B     DR120                                                            
DRCLC    CLC   8(0,R5),WORK                                                     
DROC     OC    8(0,R2),8(R2)                                                    
DRXC     XC    8(0,R2),8(R2)                                                    
DRMVCA   MVC   8(0,R5),WORK                                                     
DRMVCB   MVC   8(0,R2),PATCMT-PATCMTEL(R6)                                      
DRMVCC   MVC   WORK(0),PATPTN-PATPTNEL(R6)                                      
         DROP  R6                                                               
         EJECT                                                                  
* FORMAT PART OF LISTAR LINE *                                                  
*                                                                               
FLST     NTR1                                                                   
         MVC   LISTAR,SPACES                                                    
         USING INSKEY,R4                                                        
         USING INSDTAEL,R6                                                      
*                                                                               
         MVC   RMKT,QMKT                                                        
         MVC   RSTA,STAPRNT                                                     
*                                                                               
         OC    STANET,STANET       THIS A CABLE HEAD STA                        
         BZ    *+10                                                             
         MVC   RSTA(8),STANET                                                   
*                                                                               
         MVC   RAFFIL,SVAFFIL                                                   
         LA    R3,INSPRD1                                                       
         BAS   RE,PPRD                                                          
         MVC   RPRDSLN,FLD                                                      
         LA    R3,INSPRD2                                                       
         BAS   RE,PPRD                                                          
         MVC   RPTRSLN,FLD                                                      
         MVC   RCODE(1),INSKCOPY                                                
         CLI   INSKCOPY,0                                                       
         BE    FLST10                                                           
         TM    INSFLAG,X'20'       COPY CODE = ESTIMATE                         
         BZ    FLST10                                                           
         LLC   R0,INSKCOPY                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RCODE(3),DUB                                                     
         MVC   RCODE+3(1),INSKDPT                                               
*                                                                               
FLST10   GOTO1 DATCON,DMCB,(2,INSDATE),(5,RINSDTE)                              
         B     EXIT                                                             
         DROP  R4,R6                                                            
         EJECT                                                                  
* PRINT PRODUCT CODE AND SPOT LEN                                               
*                                                                               
PPRD     NTR1                                                                   
         LA    R5,FLD              ADDRESS OF OUTPUT AREA                       
         MVC   FLD,SPACES                                                       
         CLI   0(R3),0             ANY PRODUCT CODE                             
         BE    EXIT                NO,DONE                                      
         L     R1,ASVCLIST         ADDRESS OF SAVED CLIST (VALICLT)             
PPRD10   CLC   0(1,R3),3(R1)                                                    
         BE    PPRD12                                                           
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BH    PPRD10                                                           
         LA    R1,=C'???'                                                       
PPRD12   MVC   0(3,R5),0(R1)                                                    
         CLI   1(R3),0             ANY SPOT LEN                                 
         BE    PPRD16              NO                                           
         LA    R5,2(,R5)                                                        
         CLI   0(R5),C' '                                                       
         BNH   PPRD14                                                           
         LA    R5,1(,R5)                                                        
PPRD14   MVI   0(R5),C'-'                                                       
         LA    R5,1(,R5)                                                        
         EDIT  (B1,1(R3)),(3,(R5)),ALIGN=LEFT                                   
PPRD16   B     EXIT                                                             
* 3                                                                             
* PRINT PERIOD INTO WORK                                                        
*                                                                               
         USING PATDTAEL,R6                                                      
PPER     NTR1                                                                   
         MVC   WORK,SPACES                                                      
         GOTO1 DATCON,DMCB,(3,PATSTART),(5,WORK)                                
         MVI   WORK+8,C'-'                                                      
         CLC   PATEND,=X'FFFFFF'                                                
         BNE   PPER10                                                           
         MVC   WORK+9(3),=C'UFN'                                                
         B     EXIT                                                             
PPER10   GOTO1 (RF),(R1),(3,PATEND),(5,WORK+9)                                  
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*===========================================================                    
* BUILD PATTERN KEY FROM INSTR ELEM DATA                                        
*===========================================================                    
                                                                                
         USING INSKEY,R4                                                        
         USING INSDTAEL,R6                                                      
BLDPAT   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R1,KEY              SET UP PAT KEY                               
         USING PATKEY,R1                                                        
         MVC   PATKID,=X'0A22'                                                  
         MVC   PATKAM(3),BAGYMD & BCLT                                          
         MVC   PATKCLT,BCLT                                                     
         MVC   PATKPRD(4),INSPRD1  PROD, SPOT LEN FOR PROD/PARTNER              
         MVC   PATKCODE,INSKCOPY                                                
         MVC   PATKREF,0(R2)                                                    
*                                                                               
         L     RF,AIO1                                                          
         MVC   0(13,RF),KEY        FORMAT AIO1 FOR NO READ                      
         CLC   =X'FFFFFF',PATKREF  THIS A TBA REF                               
         BE    EXIT                 YES                                         
*                                                                               
         OC    PATKREF,PATKREF     IS THIS ZERO REF (HIATUS)                    
         BZ    EXIT                 YES                                         
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                DIR READ FOR PTTN                            
         CLC   KEY(13),KEYSAVE     FIND THIS PAT                                
         BE    BLDPAT2             NO, ERROR                                    
*                                                                               
         MVC   KEY(13),KEYSAVE     RESTORE KEY                                  
         CLI   PATKCODE,0                                                       
         BE    MISSPAT                                                          
*                                                                               
         MVI   PATKCODE,0                                                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                DIR READ FOR PTTN                            
         CLC   KEY(13),KEYSAVE     FIND THIS PAT                                
         BNE   MISSPAT             NO, ERROR                                    
*                                                                               
BLDPAT2  MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC              READ PATTERN REC INTO AIO1                   
         B     EXIT                                                             
         DROP  R1,R4,R6                                                         
*                                                                               
PRINT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
         EJECT                                                                  
* READ PRD HEADER TO GET PRD NAME, EXIT WITH PRDNAME IN SVPNAME *               
*                                                                               
GETPRD   NTR1                                                                   
         L     R5,AIO2                                                          
         ST    R5,AIO                                                           
         USING PRDHDR,R5                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD        A-M/CLT                                   
         MVC   KEY+4(3),PROD                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT SYSTEM                     
         GOTO1 HIGH                                                             
*                                                                               
         MVC   SVPNAME(19),=C'* UNKNOWN PRODUCT *'                              
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETPRDX                                                          
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT SYSTEM                     
         GOTO1 GETREC                                                           
         MVC   SVPNAME,PNAME                                                    
*                                                                               
GETPRDX  XC    FILENAME,FILENAME                                                
         B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*==================================================================             
* GET CMML REC                                                                  
* PUT CMML NUM/TITLE/TYPE IN P1                                                 
* CMML MUST BE IN SVCML                                                         
*==================================================================             
                                                                                
         USING PRTLINE,R2                                                       
GETCMML  NTR1                                                                   
         CLC   =X'5C00',SVCML      DELETED CMML                                 
         BE    GETCDEL             YES                                          
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'      CMML REC                                    
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   *+10                                                             
         MVC   KEY(2),=X'0AC1'                                                  
         MVC   KEY+2(3),BAGYMD AND BCLT                                         
         MVC   KEY+5(8),SVCML                                                   
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
*                                                                               
         MVC   PCMLTTL,CMLTITLE                                                 
*                                                                               
         MVC   PPTYPE,CMLTYPE                                                   
*                                                                               
GETC30   TM    CMLSTAT,X'80'       DELETED                                      
         BZ    GETCX                                                            
         MVI   PCMLTTL-1,C'*'                                                   
*                                                                               
GETCX    J     EXIT                                                             
*                                                                               
GETCDEL MVC    PCMLID(8),=C'*DELETED' INDICATE DELETED CMML                     
         B     GETCX                                                            
         EJECT                                                                  
*===========================================================                    
* ON ALPHA CONTAINS POINTER TO ALPHATAB ENTRY                                   
* FORMAT PCT TO PCMLPCT                                                         
*===========================================================                    
                                                                                
GETPCT   NTR1                                                                   
         CLI   ELEM,0              TEST NO PCTS                                 
         BE    GETPCTX                                                          
         LA    R6,ELEM+2                                                        
*                                                                               
         L     RE,ALPHA                                                         
GETPCT2  CLC   0(1,RE),0(R6)       MATCH LETTER                                 
         BE    GETPCT4                                                          
         BL    GETPCTX             CMML MUST BE DELETED                         
         LA    R6,3(R6)                                                         
         CLI   0(R6),0                                                          
         BNE   GETPCT2                                                          
         B     GETPCTX                                                          
*                                                                               
GETPCT4  LA    RE,PCMLPCT                                                       
         MVI   0(RE),C'('                                                       
         LLC   R0,2(R6)                                                         
         EDIT  (R0),(3,1(RE)),ALIGN=LEFT                                        
         AR    RE,R0                                                            
         MVI   1(RE),C')'                                                       
*                                                                               
GETPCTX  J     EXIT                                                             
*                                                                               
         DROP  R2,R6                                                            
         EJECT                                                                  
*=================================================                              
* HEADHOOK ROUTINE FOR OFF-LINE REPORT                                          
*=================================================                              
                                                                                
         DS    0H                                                               
HDHK     MVC   H2+10(L'MEDNM),MEDNM                                             
         MVC   H4+10(L'QCLT),QCLT                                               
         MVC   H4+15(L'CLTNM),CLTNM                                             
         MVC   H5+10(L'PROD),PROD                                               
         MVC   H5+15(L'PRDNM),PRDNM                                             
         MVC   H6+10(L'QMKT),QMKT                                               
         MVC   H6+15(L'MKTNM),MKTNM                                             
         MVC   H6+86(L'STAPRNT),STAPRNT                                         
*                                                                               
         OC    STANET,STANET       THIS A CABLE HEAD STA                        
         BZ    *+10                                                             
         MVC   H6+86(L'STANET),STANET                                           
*                                                                               
         MVC   H7+86(L'SVTWIX),SVTWIX                                           
*                                                                               
         OC    AFFFTR,AFFFTR       FILTER ON AFFILIATE                          
         BZR   RE                   NO                                          
         MVC   H4+40(4),=C'AFF='                                                
         MVC   H4+44(3),AFFFTR                                                  
         BR    RE                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
FINDEL   CLI   0(R6),0                                                          
         JE    FINDELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
*                                                                               
FINDEL1  CLI   0(R6),X'10'                                                      
         JL    FINDEL                                                           
         CLI   0(R6),X'14'                                                      
         JE    FINDEL                                                           
         CLI   0(R6),X'30'                                                      
         JH    FINDEL                                                           
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
FINDELX  LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
MISSPAT  LA    R2,TRAMEDH                                                       
         L     R1,=A(NOPATMSG)                                                  
         A     R1,SPTR44RR                                                      
         MVC   CONHEAD,0(R1)                                                    
         MVC   CONHEAD+25(L'QCLT),QCLT                                          
         MVC   KEY(13),KEYSAVE     RESTORE KEY TRIED FOR                        
         LA    R3,KEY+PATKPRD-PATKEY                                            
         BAS   RE,PPRD                                                          
         MVC   CONHEAD+29(7),FLD                                                
         XC    CONHEAD+37(7),CONHEAD+37                                         
         OC    KEY+PATKPRD2-PATKEY(2),KEY+PATKPRD2-PATKEY                       
         BZ    MISSPATC                                                         
         LA    R3,KEY+PATKPRD2-PATKEY                                           
         BAS   RE,PPRD                                                          
         MVC   CONHEAD+37(7),FLD                                                
*                                                                               
MISSPATC CLI   KEY+PATKCODE-PATKEY,0                                            
         BNE   *+14                                                             
         XC    CONHEAD+45(4),CONHEAD+45                                         
         B     *+10                                                             
         MVC   CONHEAD+48(1),KEY+PATKCODE-PATKEY                                
         SR    R1,R1                                                            
         ICM   R1,7,KEY+PATKREF-PATKEY                                          
         SRL   R1,10                                                            
         X     R1,=X'00003FFF'                                                  
         STH   R1,BREF                                                          
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,BREF                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CONHEAD+54(4),DUB                                                
*                                                                               
         GOTO1 SQUASHER,DMCB,CONHEAD,60                                         
         B     EREXIT                                                           
*                                                                               
EREXIT   NI    TRRMEDH+4,X'FF'-X'20'    SET OFF VALIDATED                       
         GOTO1 ERREX2                                                           
         EJECT                                                                  
NOINSREC MVI   ERROR,NOINSTR                                                    
         LA    R2,TRAMEDH                                                       
         J     NOINSCOM                                                         
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         J     TRAPERR                                                          
*                                                                               
INVLERR  DS    0H                                                               
         MVI   ERROR,INVRCACT                                                   
         J     TRAPERR                                                          
*                                                                               
CHAMODE  DS    0H                  * CHANGE FUNCTION ILEGAL *                   
ADDMODE  DS    0H                  * ADD RECORD FUNCTION ILEGAL *               
DELMODE  MVI   ERROR,INVACT        * DELETE RECORD FUNCTION ILLEGAL *           
         LA    R2,CONACTH                                                       
         J     TRAPERR                                                          
*                                                                               
NOINSTER MVI   ERROR,NOTFOUND                                                   
*                                                                               
NOINSCOM NI    TRRMEDH+4,X'FF'-X'20'                                            
         NI    TRRCLTH+4,X'FF'-X'20'                                            
         NI    TRRPRDH+4,X'FF'-X'20'                                            
         NI    TRRMKSTH+4,X'FF'-X'20'                                           
         NI    TRRPERH+4,X'FF'-X'20'                                            
         NI    TRRFTRH+4,X'FF'-X'20'                                            
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
NOPATMSG DC    CL60'* ERR * NO PATTN REC-CLT XXX PRD-SLN PTR-SLN CC-X RC        
               EF XXXX'                                                         
         EJECT                                                                  
HEADING  SSPEC H1,1,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,33,C'I N S T R U C T I O N   R E C A P'                       
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,1,C'MEDIA'                                                    
         SSPEC H2,33,C'---------------------------------'                       
         SSPEC H2,75,AGYADD                                                     
         SSPEC H3,36,PERIOD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H4,75,RUN                                                        
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H5,75,REQUESTOR                                                  
         SSPEC H5,99,PAGE                                                       
         SSPEC H6,1,C'MARKET'                                                   
         SSPEC H6,75,C'STATION'                                                 
         SSPEC H9,3,C'INST'                                                     
         SSPEC H9,14,C'    PRODUCT NAME    '                                    
         SSPEC H10,3,C'DATE'                                                    
         SSPEC H9,10,C'PRD'                                                     
         SSPEC H10,10,C'COD'                                                    
         SSPEC H10,14,C'-------------------'                                    
         SSPEC H9,35,C'SLN'                                                     
         SSPEC H9,39,C'      PERIOD     '                                       
         SSPEC H10,39,C'-----------------'                                      
         SSPEC H9,58,C'REF'                                                     
*MNMB                                                                           
         SSPEC H9,64,C'CODE'                                                    
         SSPEC H10,64,C'----'                                                   
*MNMB                                                                           
*MNMB    SSPEC H9,64,C'-----------   COMMERCIAL  -------------'                 
         SSPEC H9,69,C'-----------   COMMERCIAL  -------------'                 
*MNMB    SSPEC H10,65,C' PCT'                                                   
         SSPEC H10,70,C' PCT'                                                   
*MNMB    SSPEC H10,72,C'CODE'                                                   
         SSPEC H10,77,C'CODE'                                                   
*MNMB    SSPEC H10,87,C'TITLE'                                                  
         SSPEC H10,92,C'TITLE'                                                  
         DC    X'00'               END MARKER FOR SSPEC                         
         DROP  R7                                                               
         EJECT                                                                  
         USING IPTABLED,R5                                                      
FINS     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   KEY(13),SVLSTKEY                                                 
         OC    SVLSTKEY,SVLSTKEY                                                
         BNZ   FINS02                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING INSKEY,R4                                                        
         MVC   INSKID,=X'0A24'                                                  
         MVC   INSKAM(3),BAGYMD BCLT                                            
         MVC   INSKPRD,IPPPRDS                                                  
         MVC   INSKMKT,BMKT                                                     
         MVC   INSKSTA,IPSTA                                                    
         MVC   INSKCOPY,IPCODE                                                  
*                                                                               
FINS02   MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                DIR READ FOR INSTR                           
         CLC   KEY(13),KEYSAVE     FIND IT                                      
         BE    FINS06                                                           
*                                                                               
         CLC   KEY(6),KEYSAVE      FIND IT                                      
         JNE   NOINSREC             NO                                          
*                                                                               
         CLI   SPOTCAN,C'C'        IF CANADIAN                                  
         JE    NOINSREC             NOT CABLE                                   
*                                                                               
         CLI   INSKSTA,CABLESTA    THIS A CABLE STATION                         
         JL    NOINSREC             NO                                          
         TM    INSKSTA+2,X'7F'                                                  
         JZ    NOINSREC             NO                                          
*                                                                               
         MVC   FULL,INSKSTA                                                     
         NI    FULL+2,X'80'                                                     
         CLC   IPSTA,FULL                                                       
         J     NOINSREC            NO, ISSUE ERROR MSG                          
*                                                                               
FINS06   L     R6,AIO3                                                          
         ST    R6,AIO                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,24(,R6)                                                       
         BRAS  RE,SRT              SORT ELEMS IN DATE ORDER                     
         MVC   SVKEY,KEY           SAVE INST KEY AND DISK ADDR                  
*                                                                               
         BRAS  RE,FINDEL1                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING INSDTAEL,R6                                                      
         CLI   IPIELCTR,1          USE 1ST ELEM                                 
         BE    FINS20              YES                                          
         LLC   R1,IPIELCTR         GET ELEM CTR                                 
         BCTR  R1,0                SUB 1 FOR ELEM ALREADY GETEL'ED              
*                                                                               
FINS10   BRAS  RE,FINDEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         BCT   R1,FINS10                                                        
*                                                                               
         USING INSDTAEL,R6                                                      
FINS20   LLC   R1,INSDTALN                                                      
         AHI   R1,-(INSPTTN-INSDTAEL) SUB OUT LEN                               
         SR    R0,R0                                                            
         D     R0,=F'7'                                                         
         LTR   R0,R0               SHOULD BE NO REMAINDER                       
         BZ    *+6                                                              
         DC    H'0'                                                             
         STC   R1,PATTNMAX         SAVE PTTN COUNT                              
         LLC   R1,IPIFDCTR                                                      
         STC   R1,PATNFDCT                                                      
         BCTR  R1,0                MAKE REL 0                                   
         MHI   R1,7                                                             
         LA    R2,INSPTTN(R1)                                                   
         L     R4,AIO3                                                          
         STM   R2,R6,DMCB                                                       
         XIT1  REGS=(R2,R6)                                                     
         LTORG                                                                  
         DROP  R4,R6                                                            
         EJECT                                                                  
*==========================================================                     
* SORT ELEMENTS IN INSTR RECAP REC IN FTD LTD ORDER                             
*        R2=COUNT OF ELEMS   R3=AIO1 PTR                                        
*==========================================================                     
                                                                                
SRT      NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,FINDEL1                                                       
         JNE   EXIT                                                             
*                                                                               
SRT06    SR    R2,R2                                                            
         L     R3,AIO1                                                          
*                                                                               
* BUILD SORT KEYS OF PRD1 SLN1 PRD2 SLN2 FTD LTD REL ADR PTR TO ELEM *          
*                                                                               
         USING INSDTAEL,R6                                                      
SRT10    MVC   0(4,R3),INSPRD1                                                  
         MVC   4(2,R3),INSFTD                                                   
         LLC   R1,INSDTALN         FIND LTD                                     
         LA    R1,0(R1,R6)                                                      
         AHI   R1,-2                                                            
         MVC   6(2,R3),0(R1)                                                    
         LR    RF,R6                                                            
         S     RF,AIO3                                                          
         STH   RF,8(R3)                                                         
*                                                                               
         LA    R2,1(,R2)                                                        
         LA    R3,10(,R3)                                                       
         BRAS  RE,FINDEL                                                        
         BE    SRT10                                                            
*                                                                               
         CHI   R2,1                                                             
         JE    EXIT                                                             
*                                                                               
* SORT KEYS OF PRD1 SLN1 PRD2 SLN2 FTD LTD REL ADR PTR TO ELEM *                
*                                                                               
         XC    DMCB(4),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QQSORT                                                    
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),AIO1,(R2),10,08,0                                      
*                                                                               
* BUILD REL ADR TABLE IN BLOCK *                                                
*                                                                               
         XC    BLOCK(240),BLOCK                                                 
         XC    BLOCK+240(240),BLOCK+240                                         
         LA    R1,BLOCK                                                         
         L     RE,AIO1                                                          
         LR    RF,R2                                                            
SRT20    MVC   0(2,R1),8(RE)                                                    
         LA    RE,10(,RE)                                                       
         LA    R1,2(,R1)                                                        
         BCT   RF,SRT20                                                         
         EJECT                                                                  
* BUILD NEW REC OF SORTED ELEMS *                                               
*                                                                               
         LA    R3,BLOCK                                                         
         L     RE,AIO1                                                          
         L     RF,AIO3                                                          
         MVC   0(24,RE),0(RF)                                                   
         MVC   13(2,RE),=H'24'                                                  
         LA    RE,24(,RE)                                                       
         LA    R4,24                                                            
SRT30    LH    RF,0(R3)                                                         
         A     RF,AIO3                                                          
         LLC   R1,1(RF)                                                         
         AR    R4,R1                                                            
         BCTR  R1,0                                                             
         EX    R1,SRTMVC                                                        
         LA    R3,2(,R3)                                                        
         LA    RE,1(R1,RE)                                                      
         BCT   R2,SRT30                                                         
         MVI   0(RE),0                                                          
         LA    R4,1(,R4)                                                        
*                                                                               
* MOVE SORTED RECORD BACK TO AIO3 *                                             
*                                                                               
         L     R0,AIO3                                                          
         L     RE,AIO1                                                          
         STCM  R4,3,13(RE)                                                      
         LR    RF,R4                                                            
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
*                                                                               
         CR    RB,RB               SET EQ CC                                    
         J     EXIT                                                             
SRTMVC   MVC   0(0,RE),0(RF)                                                    
         EJECT                                                                  
*=================================================================              
* DISPLAY CMMLS/PCTS/ROTATIONS FOR X'C1' PATT/CMML SCREEN                       
*=================================================================              
                                                                                
DCMMLS   NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'        FIND STATUS ELEM                             
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PATDTAEL,R6                                                      
         MVI   ADIDFLAG,C'Y'                                                    
         TM    PATSTAT1,PATSADID   SET CMMLS ARE ADIDS                          
         BO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
*                                                                               
         L     R6,AIO                                                           
         TM    15(R6),X'01'        TEST BPAT                                    
         BO    DBPAT                                                            
*                                                                               
         MVI   ELCODE,X'36'        LOOK FOR ABSURD ROT ELEM                     
         BRAS  RE,GETEL                                                         
         BE    DCM2                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'34'        LOOK FOR NORMAL ROT ELEM                     
         BRAS  RE,GETEL                                                         
         BE    DCM2                                                             
*                                                                               
         SR    R3,R3               INDICATE NO ROT ELEM                         
         B     DCM4                                                             
*                                                                               
DCM2     LA    R3,PATPCTLT-PATPCTEL(R6)  POINT TO FIRST PCT LETTER              
*                                                                               
DCM4     L     R6,AIO              COMMERCIAL LIST ELEMENT                      
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BE    DCM4X                                                            
*                                                                               
         USING PATCMLEL,R6                                                      
DCM4X    LA    R2,TCMCMLAH         1ST SCREEN FIELD                             
         LLC   R4,PATCMLLN                                                      
         SRL   R4,4                GIVES NUMBER OF CMMLS                        
         LA    R5,PATCML           FIRST CMML                                   
*                                                                               
DCM6     DS    0H                                                               
         ST    R4,SVR4TEMP         SAVE TO RESTORE LATER FOR BCT                
         XC    8(L'TCMCMLA,R2),8(R2)      CLEAR FIELD                           
         OI    6(R2),X'80'                AND XMT                               
         LLC   RE,0(R2)                                                         
         AR    RE,R2                                                            
         XC    8(L'TCMPCTA,RE),8(RE)    CLEAR PCT FIELD                         
         OI    6(RE),X'80'              AND XMT                                 
*                                                                               
DCM8     MVI   8(R2),C'*'                                                       
         CLC   =X'5C00',0(R5)        TEST DELETED                               
         BNE   DCM10                                                            
         AHI   R2,TCMCMLBH-TCMCMLAH  POINT TO NEXT CMML                         
         B     DCM14                                                            
*                                                                               
DCM10    MVC   8(8,R2),0(R5)       MOVE FIRST CMML                              
         OC    8(8,R5),8(R5)       TEST FOR SECOND CMML                         
         BZ    *+14                NO                                           
         MVI   16(R2),C'-'                                                      
         MVC   17(8,R2),8(R5)                                                   
*                                                                               
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   DCM10X                                                           
         GOTO1 VTRPACK,DMCB,(C'U',0(R5)),8(R2)                                  
         OC    8(8,R5),8(R5)                                                    
         BZ    DCM10X                                                           
*                                                                               
         LA    R4,8(R2)                                                         
DCM10A   CLI   0(R4),C' '                                                       
         BNH   DCM10B                                                           
         LA    R4,1(R4)                                                         
         B     DCM10A                                                           
*                                                                               
DCM10B   MVI   0(R4),C'-'                                                       
         GOTO1 VTRPACK,DMCB,(C'U',8(R5)),1(R4)                                  
*                                                                               
DCM10X   LLC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO PCT FIELD                           
         OI    6(R2),X'80'                                                      
*                                                                               
         LTR   R3,R3               TEST HAVE PCTS                               
         BZ    DCM12                                                            
         ICM   R0,3,1(R3)          GET PCT                                      
         EDIT  (R0),(3,8(R2)),0,ALIGN=LEFT                                      
*                                                                               
DCM12    LLC   R0,0(R2)            POINT TO NEXT LETTER                         
         AR    R2,R0                                                            
         LLC   R0,0(R2)            POINT TO NEXT CMML FIELD                     
         AR    R2,R0                                                            
*                                                                               
         LTR   R3,R3               TEST HAVE PCT ELEMENT                        
         BZ    *+8                                                              
         LA    R3,3(R3)                                                         
*                                                                               
DCM14    LA    R5,16(R5)                                                        
         L     R4,SVR4TEMP         SAVE TO RESTORE LATER FOR BCT                
         BCT   R4,DCM6                                                          
                                                                                
* CLEAR REMAINING CMMLS/PCTS                                                    
                                                                                
DCM16    XC    8(L'TCMCMLA,R2),8(R2)      CLEAR FIELD                           
         OI    6(R2),X'80'                AND XMT                               
         LLC   RE,0(R2)                                                         
         AR    RE,R2                                                            
         XC    8(L'TCMPCTA,RE),8(RE)    CLEAR PCT FIELD                         
         OI    6(RE),X'80'              AND XMT                                 
*                                                                               
         AHI   R2,TCMCMLBH-TCMCMLAH                                             
         LA    R0,TCMPCTOH         LAST PCT FIELD                               
         CR    R2,R0               TEST DONE ALL                                
         BL    DCM16                                                            
         EJECT                                                                  
*=======================================================                        
* DISPLAY LETTER ROTATION ELEMENT                                               
*=======================================================                        
                                                                                
DCM20    XC    TCMROT,TCMROT                                                    
         OI    TCMROTH+6,X'80'                                                  
         XC    TCMDROT,TCMDROT                                                  
         OI    TCMDROTH+6,X'80'                                                 
*                                                                               
         LA    R2,TCMDROTH         IF HAVE PCTS, SHOW DERIVED ROT               
         MVI   ELCODE,X'34'        CHECK FOR PCT ELEM                           
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+8                                                              
         LA    R2,TCMROTH          IF NO PCT, SHOW ROT                          
*                                                                               
         MVI   ELCODE,X'32'        FIND ROTATION ELEMENT                        
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    DCM24                                                            
         CLC   TCMCMLA(6),=CL6'HIATUS'                                          
         BE    DCMX                                                             
         DC    H'0'                                                             
*                                                                               
         USING PATPTNEL,R6                                                      
DCM24    LLC   R1,PATPTNLN                                                      
         AHI   R1,-3                                                            
         EX    R1,*+8              MOVE IN ROTATION                             
         B     *+10                                                             
         MVC   8(0,(R2)),PATPTN-PATPTNEL(R6)                                    
*                                                                               
DCMX     J     EXIT                                                             
         EJECT                                                                  
*===========================================================                    
* DISPLAY COMMERCIALS AND PERCENTAGES FOR BPAT RECORD                           
* BUILD ONE CONTINUOUS CMML ELEM IN AIO2+4000                                   
*===========================================================                    
                                                                                
DBPAT    DS    0H                                                               
         L     R6,AIO              COMMERCIAL LIST ELEMENT                      
         MVI   ELCODE,X'31'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DBPATX                                                           
*                                                                               
         L     R4,AIO2                                                          
         AHI   R4,4000                                                          
         ST    R4,FULL             SAVE BUILD AREA ADDRESS                      
*                                                                               
DBPAT2   LLC   RE,1(R6)            ELEM LEN                                     
         SHI   RE,4                MINUS ELCODE/LEN/NUMBER/EX MOVE              
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),3(R6)       SAVE OLD ELEM INFO                           
*                                                                               
         AR    R4,RE                                                            
         LA    R4,1(R4)                                                         
         XC    0(16,R4),0(R4)      CLEAR FOLLOWING                              
         BRAS  RE,NEXTEL                                                        
         BE    DBPAT2                                                           
*                                                                               
         L     R6,AIO              PERCENTAGE ELEMENT                           
         MVI   ELCODE,X'34'                                                     
         BRAS  RE,GETEL                                                         
         JNE   EXIT                                                             
*                                                                               
         LA    R3,2(R6)            R3 POINTS TO PCT                             
         L     R6,FULL             R6 POINTS TO CMML LIST                       
         LA    R2,PCOCMLAH         1ST SCREEN FIELD                             
*                                                                               
DBPAT10  CLI   0(R6),0             TEST NO MORE CMMLS                           
         BE    DBPAT20                                                          
*                                                                               
         MVC   8(8,R2),0(R6)       MOVE CMML                                    
         OI    6(R2),X'80'                                                      
*                                                                               
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   DBPAT11                                                          
*                                                                               
         CLC   =X'5C00',0(R6)      DELETED                                      
         BE    DBPAT11                                                          
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'U',0(R6)),8(R2)                                  
*                                                                               
DBPAT11  LLC   RE,0(R2)                                                         
         AR    R2,RE               PT TO % FIELD                                
         OI    6(R2),X'80'                                                      
*                                                                               
         CLI   0(R6),C'*'          TEST DELETED CMML                            
         BE    DBPAT12                                                          
*                                                                               
         LLC   R0,2(R3)                                                         
         EDIT  (R0),(3,8(R2)),ALIGN=LEFT                                        
*                                                                               
DBPAT12  LA    R3,3(R3)            BUMP IN PERCENTAGE ELEM                      
         LA    R6,16(R6)           NEXT CMML                                    
*                                                                               
         LLC   RE,0(R2)                                                         
         AR    R2,RE               PROTECTED FIELD                              
         LLC   RE,0(R2)                                                         
         AR    R2,RE               CML                                          
*                                                                               
         LA    R0,PCOPC24H                                                      
         CR    R2,R0                                                            
         BL    DBPAT10                                                          
*                                                                               
DBPAT20  LA    R0,PCOPC24H                                                      
         CR    R2,R0                                                            
         BNL   DBPATX              NO                                           
*                                                                               
         XC    8(8,R2),8(R2)       CLEAR CMML FIELD                             
         OI    6(R2),X'80'         XMT                                          
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         XC    8(2,R2),8(R2)       CLEAR PCT FIELD                              
         OI    6(R2),X'80'                                                      
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     DBPAT20                                                          
*                                                                               
DBPATX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*-----------------------------------------------------------------*             
CHKSCR   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,TRADCMLH                                                      
         LLC   R3,0(R2)                                                         
         AR    R2,R3                                                            
*        HERE IS WHERE I NEED TO START WITH ZERO INTENSITY FIELDS               
         LA    R4,38                                                            
CSCR010  OI    1(R2),X'0C'    LOW INTENSITY                                     
         OI    6(R2),X'80'    TRANSMIT                                          
         LLC   R3,0(R2)                                                         
         AR    R2,R3                                                            
         BCT   R4,CSCR010                                                       
         J     EXIT                                                             
*-----------------------------------------------------------------*             
GETSCR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   ACTEQU,ACTSEL                                                    
         MVC   BLOCK(TRAXXXXH-TRAKEYH),TRAKEYH   SAVE KEY FIELDS                
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(3),=X'D90216'                                             
         MVC   DMCB+7(1),MYSCREEN                                               
*                                                                               
         GOTO1 CALLOV,DMCB,TRAKEYH                                              
         CLI   4(R1),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TRAKEYH(TRAXXXXH-TRAKEYH),BLOCK    RESTORE KEY FIELDS            
*                                                                               
         LA    R1,TRAXXXXH         FIND EOS                                     
         SR    R0,R0                                                            
GETSCR2  IC    R0,0(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BNE   GETSCR2                                                          
         MVC   0(3,R1),=X'000101'  ERASE BEFORE/AFTER                           
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
SETFLDS  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,TRAXXXXH         DON'T CLEAR KEY FIELDS                       
*                                                                               
SETFLD2  OI    1(R2),X'20'         PROTECT FIELD                                
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         TM    1(R2),X'02'         TEST EXTENDED FLDHDR                         
         BZ    *+8                                                              
         LA    R2,8(R2)                                                         
         CLI   0(R2),0                                                          
         BNE   SETFLD2                                                          
         MVC   0(3,R2),=X'000101'  FORCE XMT ALL                                
*                                                                               
         OI    CONSERVH+1,X'01'    FORCE MODIFIED FOR PFKEYS                    
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
         DS    0H                                                               
TRAF3TAB DC    AL2(TRATXTH-T216FFD)                                             
         DC    AL2(TRADESCH-T216FFD)                                            
         DC    AL2(TRAINVPH-T216FFD)                                            
         DC    AL2(TRAPERH-T216FFD)                                             
         DC    AL2(TRALTYH-T216FFD)                                             
         DC    AL2(TRAMS1H-T216FFD)                                             
         DC    AL2(TRAMS2H-T216FFD)                                             
         DC    AL2(TRAMS3H-T216FFD)                                             
         DC    AL2(TRASTIMH-T216FFD)                                            
         DC    AL2(TRAETIMH-T216FFD)                                            
         DC    AL2(TRADPTH-T216FFD)                                             
         DC    X'FFFF'                                                          
*                                                                               
TRA5DTAB DC    AL2(PCMCMT1H-T216FFD)                                            
         DC    AL2(PCMCMT2H-T216FFD)                                            
         DC    AL2(PCMCMT3H-T216FFD)                                            
         DC    AL2(PCMCMT4H-T216FFD)                                            
         DC    X'FFFF'                                                          
*                                                                               
TRAC1TAB DC    AL2(TCMCMLAH-T216FFD)                                            
         DC    AL2(TCMPCTAH-T216FFD)                                            
         DC    AL2(TCMCMLBH-T216FFD)                                            
         DC    AL2(TCMPCTBH-T216FFD)                                            
         DC    AL2(TCMCMLCH-T216FFD)                                            
         DC    AL2(TCMPCTCH-T216FFD)                                            
         DC    AL2(TCMCMLDH-T216FFD)                                            
         DC    AL2(TCMPCTDH-T216FFD)                                            
         DC    AL2(TCMCMLEH-T216FFD)                                            
         DC    AL2(TCMPCTEH-T216FFD)                                            
         DC    AL2(TCMCMLFH-T216FFD)                                            
         DC    AL2(TCMPCTFH-T216FFD)                                            
         DC    AL2(TCMCMLGH-T216FFD)                                            
         DC    AL2(TCMPCTGH-T216FFD)                                            
         DC    AL2(TCMCMLHH-T216FFD)                                            
         DC    AL2(TCMPCTHH-T216FFD)                                            
         DC    AL2(TCMCMLIH-T216FFD)                                            
         DC    AL2(TCMPCTIH-T216FFD)                                            
         DC    AL2(TCMCMLJH-T216FFD)                                            
         DC    AL2(TCMPCTJH-T216FFD)                                            
         DC    AL2(TCMCMLKH-T216FFD)                                            
         DC    AL2(TCMPCTKH-T216FFD)                                            
         DC    AL2(TCMCMLLH-T216FFD)                                            
         DC    AL2(TCMPCTLH-T216FFD)                                            
         DC    AL2(TCMCMLMH-T216FFD)                                            
         DC    AL2(TCMPCTMH-T216FFD)                                            
         DC    AL2(TCMCMLNH-T216FFD)                                            
         DC    AL2(TCMPCTNH-T216FFD)                                            
         DC    AL2(TCMCMLOH-T216FFD)                                            
         DC    AL2(TCMPCTOH-T216FFD)                                            
         DC    AL2(TCMROTH-T216FFD)                                             
         DC    X'FFFF'                                                          
         EJECT                                                                  
SETPFTXT NTR1  BASE=*,LABEL=*                                                   
         NC    PFKCMT+3(7),=7X'BF' MAKE LOWERCASE AS NEEDED                     
         NC    PFKCML+3(4),=7X'BF'                                              
         NC    PF2PTN+3(6),=7X'BF'                                              
         NC    PF3PTN+3(6),=7X'BF'                                              
*                                                                               
         CLI   MYSCREEN,X'F3'      TEST PATTERN BASE                            
         BNE   SETPF2                                                           
         XC    TRAPFK,TRAPFK                                                    
         LA    R2,TRAPFK                                                        
         LR    R4,R2                                                            
         MVC   0(2,R4),=C'PF'                                                   
         LA    R4,2(R4)                                                         
*                                                                               
         MVC   0(L'PFKCMT,R4),PFKCMT    2=COMMENTS                              
         LA    R4,L'PFKCMT+1(R4)                                                
*                                                                               
         MVC   0(L'PFKCML,R4),PFKCML    3=CMMLS                                 
         LA    R4,L'PFKCML+1(R4)                                                
         B     SETPF10                                                          
*                                                                               
SETPF2   CLI   MYSCREEN,X'C1'      TEST PATTERN CMML                            
         BNE   SETPF4                                                           
         XC    TCMPFK,TCMPFK                                                    
         LA    R2,TCMPFK                                                        
         LR    R4,R2                                                            
         MVC   0(2,R4),=C'PF'                                                   
         LA    R4,2(R4)                                                         
         MVC   0(L'PFKCMT,R4),PFKCMT    2=COMMENTS                              
         LA    R4,L'PFKCMT+1(R4)                                                
         MVC   0(L'PF3PTN,R4),PF3PTN    3=PATTERN                               
         LA    R4,L'PF3PTN+1(R4)                                                
         B     SETPF10                                                          
*                                                                               
SETPF4   CLI   MYSCREEN,X'5D'      TEST PATTERN COMMENT                         
         BNE   SETPF10                                                          
         XC    PCMPFK,PCMPFK                                                    
         LA    R2,PCMPFK                                                        
         LR    R4,R2                                                            
         MVC   0(2,R4),=C'PF'                                                   
         LA    R4,2(R4)                                                         
         MVC   0(L'PF2PTN,R4),PF2PTN    2=PATTERN                               
         LA    R4,L'PF2PTN+1(R4)                                                
         MVC   0(L'PFKCML,R4),PFKCML    3=CMMLS                                 
         LA    R4,L'PFKCML+1(R4)                                                
         B     SETPF10                                                          
*                                                                               
SETPF10  OI    6(R2),X'80'         SET FIELD TO XMT                             
*                                                                               
SETPFX   J     EXIT                                                             
*                                                                               
PFKCMT   DC    C'2=COMMENTS'                                                    
PFKCML   DC    C'3=CMMLS'                                                       
PF2PTN   DC    C'2=PATTERN'                                                     
PF3PTN   DC    C'3=PATTERN'                                                     
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE FLIGHT REC FOR PERSTART TO BE A FLIGHT END DATE                      
*                                                                               
VFLT     NMOD1 0,**+VFL**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING FLTKEY,R4                                                        
         MVC   FLTKID,=X'0A27'                                                  
         MVC   FLTKAM(4),BAGYMD & BCLT & BPRD                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    VFLT10                                                           
         MVC   KEY,KEYSAVE                                                      
         MVI   FLTKPRD,0                                                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BNE   NOFLTFND                                                         
VFLT10   CLC   PERSTART,FLTKEDT                                                 
         BNH   VFLT14                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(6),KEYSAVE                                                   
         BNE   NOFLTDTE                                                         
         B     VFLT10                                                           
VFLT14   MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
VFLT20   BNE   NOFLTDTE                                                         
         CLC   PERSTART,2(R6)      COMPARE TO THIS START DATE                   
         BE    VFLT30              THIS IS IT                                   
         BL    NOFLTDTE            ERROR, NOT HERE                              
         BRAS  RE,NEXTEL                                                        
         B     VFLT20                                                           
         BNE   VFLT30                                                           
         DC    H'0'                                                             
VFLT30   MVC   PEREND,5(R6)        SAVE FLT END DATE                            
         XIT1                                                                   
*                                                                               
NOFLTFND XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOFLTMS),NOFLTMS                                       
         B     VFLTERX                                                          
*                                                                               
NOFLTDTE XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'NOFLTMS-6),NOFLTMS                                     
         MVC   CONHEAD+36(4),=C'DATE'                                           
*                                                                               
VFLTERX  NI    TRRMEDH+4,X'FF'-X'20'    SET OFF VALIDATED                       
         GOTO1 ERREX2                                                           
*                                                                               
NOFLTMS  DC    C'* ERROR * NO FLIGHT RECORD FOR THIS CLIENT'                    
         DROP  R4                                                               
         EJECT                                                                  
* VALIDATE PERIOD                                                               
*                                                                               
VPER     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    PEREND,PEREND                                                    
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VPER2               YES                                          
         GOTO1 DATCON,DMCB,(5,0),(X'20',DATE)  GET TODAY'S DATE                 
         GOTO1 DATCON,DMCB,DATE,(2,PEREND)     SET TODAY AS END DATE            
*                                                                               
         LA    R0,6                                                             
         LNR   R0,R0                                                            
         ST    R0,DMCB+8           SET TO START 6 MONTHS AGO                    
         GOTO1 ADDAY,DMCB,(C'M',DATE),DATE                                      
         MVC   DATE+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,DATE,(2,PERSTART)                                    
         GOTO1 DATCON,DMCB,(2,PERSTART),(8,8(R2))                               
         MVI   16(R2),C'-'                                                      
         GOTO1 (RF),(R1),(2,PEREND),(8,17(R2))                                  
         OI    6(R2),X'80'                                                      
         B     VPERX                                                            
*                                                                               
VPER2    LA    R3,8(R2)            START DATE                                   
         GOTO1 DATVAL,DMCB,(R3),DATE                                            
         ICM   R5,15,DMCB          GET LENGTH OF FIELD                          
         BZ    VPER20                                                           
         LA    R3,1(R5,R3)         POINT TO END DATE                            
         GOTO1 DATCON,(R1),(0,DATE),(2,PERSTART)                                
         CLM   R5,1,5(R2)          WAS ONLY 1 DATE ENTERED                      
         BE    VPERX               YES, ALL DONE                                
*                                                                               
VPER10   GOTO1 DATVAL,DMCB,(R3),DATE                                            
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,DATE),(2,PEREND)                                  
         CLC   PERSTART,PEREND                                                  
         BH    DATERR                                                           
         B     VPERX                                                            
*                                                                               
* INPUT WAS NOT MMMDD/YY - SEE IF THEY ENTERED MMM/YY OR JUST YY                
*                                                                               
VPER20   MVC   HALF,8(R2)                                                       
         CLI   5(R2),2             2 CHARS IS JUST YEAR                         
         BE    VPER22                                                           
         MVC   HALF,10(R2)                                                      
         CLI   5(R2),4             4 CHARS MUST HAVE CC                         
         BNE   VPER25                                                           
*                                                                               
VPER22   TM    4(R2),X'08'         TEST NUMERIC                                 
         BZ    DATERR                                                           
*                                                                               
VPER24   MVC   8(17,R2),=C'JAN01/XX-DEC31/XX'                                   
         MVC   14(2,R2),HALF                                                    
         MVC   23(2,R2),HALF                                                    
         MVI   5(R2),17                                                         
         OI    6(R2),X'80'         SEND DATES BACK TO USER                      
         B     VPER2                                                            
*                                                                               
* CHECK FOR MMM/YY(-MMM/YY)                                                     
*                                                                               
VPER25   LA    R3,8(R2)            START DATE                                   
         GOTO1 DATVAL,DMCB,(2,(R3)),DATE                                        
         ICM   R5,15,DMCB          GET LENGTH OF FIELD                          
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,DATE),(2,PERSTART)                                
*                                                                               
         LA    R3,1(R5,R3)         POINT TO END DATE                            
         CLM   R5,1,5(R2)          WAS ONLY 1 DATE ENTERED                      
         BE    VPER29              YES                                          
*                                                                               
VPER27   GOTO1 DATVAL,DMCB,(2,(R3)),DATE    VALIDATE FOR MMM/YY                 
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
*                                                                               
VPER29   GOTO1 ADDAY,(R1),DATE,(X'80',DUB),1    GET MONTH END DATE              
         GOTO1 DATCON,(R1),DUB,(2,PEREND)                                       
*                                                                               
VPERX    XIT1                                                                   
*                                                                               
MISSERRP MVI   ERROR,MISSING                                                    
         B     *+8                                                              
DATERR   MVI   ERROR,INVDATE                                                    
         GOTO1 ERREX                                                            
         EJECT                                                                  
* VALIDATE FILTER ROUTINES                                                      
*                                                                               
VFTR     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    FILTERS,FILTERS                                                  
         MVI   FILTER1,0                                                        
         CLI   5(R2),0                                                          
         BE    VFTRX                                                            
         CLI   8(R2),C'?'          HELP                                         
         BE    VFTRH                                                            
*                                                                               
         LA    R4,BLOCK+240        ADDRESS OF FIRST BLOCK                       
         GOTO1 SCANNER,DMCB,(R2),(7,(R4))                                       
         LLC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSERRA             NO, ERROR                                   
VFTR10   LLC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,VFTRCLCA         THIS AN AFFILIATE                            
         BNE   VFTR20                                                           
         CLI   1(R4),0             MUST HAVE ENTRY                              
         BE    AFFERR                                                           
         MVC   AFFFTR,22(R4)                                                    
*                                                                               
VFTR20   LLC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,VFTRCLCR         SHOW REVISION                                
         BNE   VFTRH                                                            
         CLI   1(RA),C'*'          THIS A DDS TERM                              
         BNE   VFTRH                                                            
         OI    FILTER1,REVISION                                                 
*                                                                               
VFTRNXT  LA    R4,32(,R4)          POINT TO NEXT BLOCK                          
         BCT   R3,VFTR10           FOR NUMBER OF BLOCKS FOUND                   
*                                                                               
VFTRX    OI    4(R2),X'20'         SET ON VALIDATED BIT                         
*                                                                               
         XIT1                                                                   
VFTRCLCA CLC   12(0,R4),=C'AFFIL ' AFFILIATE FILTER                             
VFTRCLCR CLC   12(0,R4),=C'REVISION'  SHOW REVISION                             
*                                                                               
MISSERRA MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
VFTRH    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(22),=C'VALID FILTER IS AFFIL='                           
         B     EREXIT2                                                          
AFFERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(39),=C'* ERROR * ENTER 3 CHARACTER AFFILIATE *'          
EREXIT2  GOTO1 ERREX2                                                           
         EJECT                                                                  
*========================================================                       
* GETS INST REC ONLY FOR VALIKEY                                                
*========================================================                       
                                                                                
FRSTINS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    TEMPDSK,TEMPDSK                                                  
         XC    FLD,FLD                                                          
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING INSKEY,R4                                                        
         MVC   INSKID,=X'0A24'                                                  
         MVC   INSKAM(4),BAGYMD    A-M/CLT/PRD                                  
*                                                                               
         OC    BSTA,BSTA           WAS STATION ENTERED                          
         BZ    *+12                                                             
         CLI   SVTSPRO1,C'Y'       USE GENERIC STATION FOR MARKET               
         BE    *+10                                                             
         MVC   INSKMKT,BMKT                                                     
*                                                                               
         MVC   INSKSTA,BSTA                                                     
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
FRST10   CLC   KEY(5),KEYSAVE      ID/AM/CLT                                    
         JNE   NOINSRE                                                          
*                                                                               
         CLC   =C'POL',TRRPRD      WAS PROD ALL OR POL                          
         BE    FRST12                                                           
         CLC   INSKPRD,BPRD        SEE IF THIS IS SEL PRD                       
         JNE   NOINSRE                                                          
*                                                                               
FRST12   OC    BSTA,BSTA           IS STATION ZERO                              
         BZ    FRST16               YES, USE MARKET ONLY                        
*                                                                               
         CLI   SVTSPRO1,C'Y'       USE GENERIC STATION FOR MARKET               
         BNE   FRST14                                                           
*                                                                               
         CLC   BSTA,INSKSTA        STA ONLY                                     
         BE    FRST30                                                           
*                                                                               
         CLI   SPOTCAN,C'C'        IF CANADIAN                                  
         BE    FRST20               NOT CABLE                                   
*                                                                               
*                    X'E8' NOW                                                  
         CLI   INSKSTA,CABLESTA    THIS A CABLE STATION                         
         BL    FRST20              READ THRU ALL DATA                           
*                                                                               
         TM    BSTA+2,X'7F'        THIS A SPECIFIC CABLE STATION REQ            
         BNZ   FRST20                                                           
*                                                                               
         MVC   FULL,INSKSTA                                                     
         NI    FULL+2,X'80'                                                     
         CLC   BSTA,FULL                                                        
         BE    FRST30                                                           
         B     FRST20              READ THRU ALL DATA                           
*                                                                               
FRST14   CLC   BMKTSTA,INSKMKT     MKT+STA                                      
         BL    FRST20                                                           
         BE    FRST30                                                           
         CLC   =C'POL',TRRPRD      WAS PROD ALL OR POL                          
         BE    FRST20                                                           
         B     NOINSRE                                                          
FRST16   OC    BMKT,BMKT           TEST NO MKT ENTERED                          
         BZ    FRST30                                                           
         CLC   INSKMKT,KEYSAVE+6   MKT ONLY                                     
         BL    FRST20                                                           
         BE    FRST30                                                           
         CLC   =C'POL',TRRPRD      WAS PRD ALL OR POL                           
         BNE   NOINSRE                                                          
*                                                                               
FRST20   MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     FRST10                                                           
*                                                                               
FRST30   MVC   AIO,AIO3                                                         
         MVC   TEMPDSK,KEY+14                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         LA    R6,24(,R6)                                                       
         BRAS  RE,FINDEL1                                                       
         BNE   FRST20                                                           
*                                                                               
         MVC   SVKEY,KEY                                                        
         MVC   SVINSKEY,KEY                                                     
*                                                                               
         CLC   =C'POL',TRRPRD                                                   
         BNE   FRST36                                                           
         L     R1,ASVCLIST                                                      
FRST32   CLC   INSKPRD,3(R1)                                                    
         BE    FRST34                                                           
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BH    FRST32                                                           
         LA    R1,=C'???'                                                       
*                                                                               
FRST34   MVC   BPRD,INSKPRD                                                     
         MVC   PROD,0(R1)                                                       
         BAS   RE,FPRDHD           FPRDHD EXITS WITH PROD IN SVPNAME            
         MVC   PRDNM,SVPNAME                                                    
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                RESTORE DIRECTORY!                           
*                                                                               
FRST36   LA    R2,TRRMKSTH                                                      
         OC    BSTA,BSTA           TEST NO STATION                              
         BZ    *+12                YES- GO READ IT                              
         TM    4(R2),X'08'         IS IT NUMERIC (MKT NUM)                      
         BZ    FRST38                                                           
*                                                                               
         BRAS  RE,GETMKT           SETS STAPRNT,MKTNAME, AFFIL                  
*                                                                               
         OC    AFFFTR,AFFFTR       FILTERING ON AFFILIATE                       
         BZ    FRST38               NO                                          
*                                                                               
         CLC   SVAFFIL,AFFFTR                                                   
         BNE   FRST20                                                           
*                                                                               
FRST38   XIT1                                                                   
*                                                                               
* READ PRD HEADER TO GET PRD NAME, EXIT WITH PRDNAME IN SVPNAME *               
*                                                                               
FPRDHD   NTR1                                                                   
         XC    SVPNAME,SVPNAME                                                  
         L     R5,AIO2                                                          
         ST    R5,AIO                                                           
         USING PRDHDR,R5                                                        
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD & BCLT                                           
         MVC   KEY+4(3),PROD                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'  SWITCH TO SPOT SYSTEM                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+14                                                             
         MVC   SVPNAME(15),=C'** NOT AVAIL **'                                  
         B     FPRDHDX                                                          
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL'  SWITCH TO SPOT SYSTEM                     
         GOTO1 GETREC                                                           
         MVC   SVPNAME,PNAME                                                    
*                                                                               
FPRDHDX  XC    FILENAME,FILENAME                                                
         XIT1                                                                   
         DROP  R5                                                               
NOINSRE  MVI   ERROR,NOINSTR                                                    
         LA    R2,TRAMEDH                                                       
         NI    TRRMEDH+4,X'FF'-X'20'                                            
         NI    TRRCLTH+4,X'FF'-X'20'                                            
         NI    TRRPRDH+4,X'FF'-X'20'                                            
         NI    TRRMKSTH+4,X'FF'-X'20'                                           
         NI    TRRPERH+4,X'FF'-X'20'                                            
         NI    TRRFTRH+4,X'FF'-X'20'                                            
*                                                                               
         GOTO1 ERREX                                                            
         EJECT                                                                  
* FORMAT PERCENT ELEMENT *                                                      
*                                                                               
PCT      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING PATPCTEL,R6                                                      
         SR    R0,R0                                                            
         LLC   R1,PATPCTLN                                                      
         D     R0,=F'3'                                                         
         CHI   R0,2                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R5,R1                                                            
         SR    R1,R1                                                            
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R3,PATPCTLT                                                      
         B     PCT20                                                            
PCT10    MVI   0(R2),C','                                                       
         LA    R1,1(,R1)                                                        
         LA    R2,1(,R2)                                                        
PCT20    MVC   0(1,R2),0(R3)                                                    
         MVI   1(R2),C'='                                                       
*        EDIT  (B2,1(R3)),(2,2(R2))                                             
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  HALF,DUB                                                         
*                                                                               
         CLI   HALF,C'0'                                                        
         BE    PCT30                                                            
         MVC   2(1,R2),HALF                                                     
         LA    R2,1(,R2)                                                        
PCT30    MVC   2(1,R2),HALF+1                                                   
         LA    R1,4(,R1)                                                        
         LA    R1,4(,R1)                                                        
         LA    R2,3(,R2)                                                        
         LA    R3,3(,R3)                                                        
         BCT   R5,PCT10                                                         
         XIT1                                                                   
         DROP  R6,RB,RC                                                         
         EJECT                                                                  
* READ MKT REC FOR MKT NAME/THIS ROUTINE ALSO GIVES STAPRNT(STATION)            
*                                                                               
GETMKT   NMOD1 0,**+GMK**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         XC    INSTKEY,INSTKEY                                                  
         LA    R4,KEY                                                           
         USING INSKEY,R4                                                        
*                                                                               
         OC    INSKSTA,INSKSTA                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'80',INSKMKT),WORK,WORK+4                          
         MVC   STACL5,WORK+4                                                    
         CLI   STACL5+4,C' '                                                    
         BNE   *+10                                                             
         MVC   STACL5+4(1),QMED                                                 
*                                                                               
         CLI   STACL5+4,C'/'                                                    
         BNE   *+8                                                              
         MVI   STACL5+4,C'T'                                                    
*                                                                               
         MVC   STAPRNT,SPACES      * FORMAT STATION FOR PRINTING                
         MVC   STAPRNT(4),STACL5                                                
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),STACL5+4                                                 
         CLI   STACL5+4,C'L'       LOW POWER TV STATION                         
         BE    GM36                                                             
         MVI   3(RE),C'V'                                                       
         CLI   QMED,C'T'                                                        
         BE    GM36                                                             
         MVI   3(RE),C'M'                                                       
         CLI   QMED,C'R'                                                        
         BE    GM36                                                             
         MVI   3(RE),C' '                                                       
*                                                                               
GM36     XC    STANET,STANET                                                    
         CLC   WORK+9(3),SPACES                                                 
         BE    GM38                                                             
         MVC   STANET,WORK+4                                                    
         MVI   STANET+4,C'/'                                                    
*                                                                               
GM38     CLC   QMKT,WORK           TEST SAME MARKET AS PREV                     
         BE    GM40                                                             
         MVC   INSTKEY,KEY                                                      
         MVC   QMKT,WORK           SAVE MARKET                                  
*                                                                               
         MVI   KEY,C'0'            * READ MARKET RECORD *                       
         MVC   KEY+1(14),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         L     R3,AIO2                                                          
         ST    R3,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO                      
*                                                                               
         MVC   MKTNM,=CL24'**** UNKNOWN ****'                                   
         CLC   KEY(8),0(R3)                                                     
         BNE   GM40                                                             
         MVC   MKTNM,MKTNAME-MKTRECD(R3)                                        
*                                                                               
* READ STATION ADDRESS REC TO GET SNETWRK INTO SVAFFIL *                        
*                                                                               
GM40     OC    BSTA,BSTA           STATION SPECIFIC REQUEST                     
         BNZ   GMX                                                              
*                                                                               
         CLC   SVSTACL5,STACL5                                                  
         BE    GMX                                                              
         OC    INSTKEY,INSTKEY     KEY ALREADY SAVED                            
         BNZ   *+10                 YES                                         
         MVC   INSTKEY,KEY                                                      
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(5),STACL5                                                  
         MVC   KEY+7(2),AGENCY                                                  
*                                                                               
         CLI   SVTSPRO1,C'Y'       USE GENERIC STATION                          
         BE    *+10                                                             
         MVC   KEY+9(3),QCLT                                                    
*                                                                               
         L     R3,AIO2                                                          
         ST    R3,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         USING STAREC,R3                                                        
         MVC   SVAFFIL,SNETWRK                                                  
*                                                                               
GMX      MVC   SVSTACL5,STACL5                                                  
         OC    INSTKEY,INSTKEY                                                  
         BZ    GMX2                                                             
*                                                                               
         MVC   AIO,AIO3          * RESTORE KEY,DSK ADDRS,AIO *                  
         MVC   KEY,INSTKEY                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH              TO RESTORE PROPER REC FOR READ SEQ             
GMX2     XIT1                                                                   
         LTORG                                                                  
         DROP  R3,R4,RB,RC                                                      
         EJECT                                                                  
* FIND CLIENT HEADER AND SAVE CLIST                                             
*                                                                               
FCLT     NMOD1 0,**+FCL**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
* SAVE CURRENT RECORD                                                           
*                                                                               
         L     R0,AIO3                                                          
         L     RE,AIO1                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
*                                                                               
         MVC   SVKEY,KEY                                                        
*                                                                               
         GOTO1 CLUNPK,DMCB,BCLT,QCLT                                            
*                                                                               
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         GOTO1 VALICLT                                                          
*                                                                               
         L     R0,AIO1             MOVE BACK THE ORIGINAL RECORD                
         L     RE,AIO3                                                          
         SR    RF,RF                                                            
         ICM   RF,3,13(RE)                                                      
         LR    R1,RF                                                            
         MVCL  (R0),(RE)                                                        
*                                                                               
* READ TS (SHIPPING) PROFILE *                                                  
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0TS'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,SVTSPROF,DATAMGR                               
*                                                                               
* READ T2 STRAFFIC PROFILE *                                                    
*                                                                               
         MVI   WORK+3,C'2'                                                      
*                                                                               
         GOTO1 (RF),(R1),,ELEM                                                  
*                                                                               
         MVC   SVT2PR04,ELEM+3     SAVE PRINT ROT PERCENTS                      
*                                                                               
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY AND DISK ADDR                    
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         CR    RB,RB                                                            
         B     FCLTX                                                            
*                                                                               
FCLTNE   LTR   RB,RB                                                            
FCLTX    XIT1                                                                   
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
       ++INCLUDE SPTRINST                                                       
         EJECT                                                                  
       ++INCLUDE SPTRPAT                                                        
         EJECT                                                                  
       ++INCLUDE SPTRFLT                                                        
         EJECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
* INCLUDED DSECTS                                                               
* INCLUDE DDSPOOLD                                                              
* INCLUDE DDSPLWORKD                                                            
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
*                                                                               
       ++INCLUDE SPTRAFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAD4D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAF3D                                                       
         ORG   TRAKEYH                                                          
       ++INCLUDE SPTRA5DD                                                       
         ORG   TRAKEYH                                                          
       ++INCLUDE SPTRAC1D                                                       
         ORG   TRAKEYH                                                          
       ++INCLUDE SPTRA5CD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
* INCLUDED DSECTS                                                               
* INCLUDE SPTRAWORKD                                                            
         PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
*                                                                               
MAXCMLS  EQU   15                                                               
         DS    0D                                                               
IPTABLE  DS    CL400               TABLE OF INTR LINE TO PATTN REC              
SVR4TEMP DS    F                                                                
*                                  15 X 24 + 10 SPARE                           
FLDH     DS    CL8                                                              
FLD      DS    CL32                                                             
SPTR44RR DS    F                                                                
VTRPACK  DS    A                                                                
SVR2     DS    F                                                                
BREF     DS    H                   REFERENCE NUMBER-BINARY                      
RECCT    DS    H                   CT OF LINES LISTED FOR NO SEL MSG            
REF      DS    CL5                                                              
CODE     DS    CL1                                                              
SVTWIX   DS    CL21                                                             
DATE     DS    CL6                                                              
PATNELCT DS    XL1                 CT OF ELEMENT ENTRIES USED                   
PATNFDCT DS    XL1                 CT OF PATTN ENTRIES USED - THIS ELEM         
PATTNMAX DS    XL1                 CT OF PATTN ENTRIES IN THIS INSTR            
IPTABCT  DS    XL1                 TABLE SIZE                                   
LASTSELN DS    XL1                 LINE NUMBER IN LIST OF LAST SELECTED         
FLAG     DS    XL1                                                              
*                   00 - FIRST TIME                                             
*                   01 - JUST THRU VALKEY, RESET AND START FROM SCRATCH         
*                   02 - BEEN THRU LIST, AND LISTMON                            
*                   04 - BEEN THRU DISPLAY KEY (SELECT)                         
*                   08 - BEEN THRU DISPLAY REC (SELECT)                         
FILTER1  DS    XL1                                                              
REVISION EQU   X'80'               SHOW REVISION, DDS ONLY                      
*                                                                               
         DS    0F                                                               
HOLDKEY  DS    CL24                                                             
PROD     DS    CL3                                                              
PROD2    DS    CL3                                                              
PERSTART DS    XL2                                                              
PEREND   DS    XL2                                                              
*                                                                               
* DSECT SPECIFIC TO OFFLINE RECAP *                                             
*                                                                               
OFFLND   DS    0CL92                                                            
INSRCPEL DS    A                   A(LAST RECAP ELEMENT)                        
INSFLDPT DS    F                   PTR TO LAST FLD WITHIN INST X'10'            
INSFLDCT DS    C                   CT OF FLDS WITHIN INSTR ELEM                 
SVREFNUM DS    XL3                                                              
SVKCLT   DS    CL2                                                              
SVKPRD   DS    CL1                                                              
SVKMKT   DS    CL2                                                              
SVKSTA   DS    CL3                                                              
SVINSKEY DS    CL11                ID/A-M/CLT/PRD/MKT/STA(NO COPY/RUN)          
SVPNAME  DS    CL20                                                             
SVCML    DS    CL8                                                              
SVPRD1   DS    CL1                 PRD                                          
*MNMB                                                                           
SVPECODE DS    CL4                                                              
*MNMB                                                                           
TEMPDSK  DS    XL4                                                              
MYBYTE   DS    CL1                                                              
         DS    CL1                  -LEN                                        
*                                                                               
SVTSPROF DS    CL16                                                             
SVTSPRO1 EQU   SVTSPROF+0          USE GENERIC STATION FOR MARKET               
SVTSPRO2 EQU   SVTSPROF+1          PAGE BREAK BY STATION AFFILIATE              
SVTSPRO3 EQU   SVTSPROF+2          FORCE 1 COMMERCIAL PER LIST                  
SVTSPRO4 EQU   SVTSPROF+3          SUPPRESS COMMERCIAL TYPE                     
SVTSPRO5 EQU   SVTSPROF+4          SUPPRESS COMMERCIAL COUNTS                   
*                                                                               
SVT2PR04 DS    CL1                 PRINT PERCENT ROT                            
*                                                                               
INSTKEY  DS    CL17                                                             
STARTKEY DS    CL17                                                             
*                                                                               
FILTERS  DS    0CL3                                                             
AFFFTR   DS    CL3                                                              
*                                                                               
SETEND   DS   0CL3                                                              
CLSTEND  DS    CL1                 SWITCHES 1                                   
RLSTEND  DS    CL1                          2                                   
FRSTIME  DS    CL1                          3                                   
*                                                                               
PRTSW    DS    CL1                          4                                   
*                                                                               
ALPHA    DS    A                                                                
STACL5   DS    CL5                                                              
SVSTACL5 DS    CL5                                                              
SVAFFIL  DS    CL3                                                              
         EJECT                                                                  
* TABLE - INSTRUC/PATTN REC POINTERS                                            
IPTABLED DSECT                                                                  
IPSTA    DS    XL3   3             STATION                                      
IPPPRDS  DS    XL4   7             PRD, SLN, PRD2, SLN2                         
IPCODE   DS    CL1   8             COPY CODE FROM INSTR                         
IPPREF   DS    XL3  11             PAT REF/SUBLINE                              
INFTCDT  DS    XL2  13             1ST TELECAST (OR PER) DATE FROM INST         
INLTCDT  DS    XL2  15             LST TELECAST (OR PER) DATE FROM INST         
IPPDKAD  DS    XL4  19             DISK ADDR OF PATTERN REC                     
IPIELCTR DS    XL1  20             INSTR ELEMENT PTR                            
IPIFDCTR DS    XL1  21             INSTR FIELD (WITHIN ELEMENT) PTR             
IPINSFLG DS    XL1  22             80 - COVER LETTER INST                       
*                                  40 - TWX INSTR RUN                           
*                                  20 - COPY CODE = ESTIMATE                    
*                                  02 - DEALER INSTR RECAP                      
*                                  01 - SPOT INSTR RECAP                        
IPPDKAD2 DS    XL4  26             DISK ADDR OF INSTR RECAP REC                 
IPTABEND EQU   *                                                                
*                                                                               
PRTLINE  DSECT                                                                  
PINSTDTE DS    CL8                                                              
         DS    CL1                                                              
PPROD    DS    CL3                                                              
         DS    C                                                                
PPRDNM   DS    CL20                                                             
         DS    CL1                                                              
PPLEN    DS    CL3                                                              
         DS    CL1                                                              
PPERIOD  DS    CL17                                                             
         DS    CL2                                                              
PREF     DS    CL3                                                              
         DS    CL3                                                              
*MNMB                                                                           
PECODE   DS    CL4                                                              
         DS    CL1                                                              
*MNMB                                                                           
PALPHA   DS    CL1                                                              
         DS    CL1                                                              
PCMLPCT  DS    CL5                                                              
         DS    CL1                                                              
PCMLID   DS    CL12                                                             
         DS    CL3                                                              
PCMLTTL  DS    CL15                                                             
         DS    CL2                                                              
PPTYPE   DS    CL3                                                              
         DS    CL1                                                              
*                                                                               
GEND     DSECT                     RECAP LINE                                   
         ORG   LISTAR                                                           
RMKT     DS    CL4                                                              
         DS    CL1                                                              
RSTA     DS    CL7                                                              
         DS    CL2                                                              
RAFFIL   DS    CL3                                                              
         DS    CL1                                                              
RPRDSLN  DS    CL7                                                              
         DS    CL1                                                              
RPTRSLN  DS    CL7                                                              
         DS    CL1                                                              
RCODE    DS    CL4                                                              
         DS    CL1                                                              
RINSDTE  DS    CL9                                                              
         DS    CL1                                                              
RPER     DS    CL17                                                             
         DS    CL1                                                              
RREF     DS    CL5                                                              
         DS    CL1                                                              
RSRCE    DS    CL1                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'096SPTRA44   10/25/12'                                      
         END                                                                    
