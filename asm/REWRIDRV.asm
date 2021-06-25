*          DATA SET REWRIDRV   AT LEVEL 074 AS OF 01/14/13                      
*          DATA SET REWRIDRV   AT LEVEL 071 AS OF 02/23/98                      
*PHASE T82113C                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'REDRIVER - T82113 - SYSTEM DRIVER FOR REP'                      
***********************************************************************         
*                                                                     *         
* REGISTER USAGE -                                                    *         
*        R0 - WORK REG                                                *         
*        R1 - WORK REG                                                *         
*        R2 - WORK REG                                                *         
*        R3 - WORK REG                                                *         
*        R4 - WORK REG & GETEL PTR                                    *         
*        R5 - WORK REG                                                *         
*        R6 - BASE REG                                                *         
*        R7 - BASE REG                                                *         
*        R8 - BASE REG                                                *         
*        R9 - POINTER TO SUBSYSD                                      *         
*        RA - POINTER TO GLOBALD                                      *         
*        RB - FIRST BASE                                              *         
*        RC - POINTER TO GEND                                         *         
*        RD - SAVE AREA POINTER                                       *         
*        RE - GOTO1 REG                                               *         
*        RF - GOTO1 REG                                               *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
*  LEV 81    NOV12/90 DON'T CK PERIOD DATE FOR SALES ALL ACT TOTALS   *         
*  LEV 82    NOV14/90 DON'T ALLOW PERIOD OVERRIDE FOR AACT TOTALS     *         
*  LEV 83    DEC06/90 SET PAPER WORK RECS REQD FOR ACTIVITY TOTALS    *         
*  LEV 84    AUG23/91 FIX NULL TOTAL PRINT LINES WITH AGENCY          *         
*  LEV 85    MAY27/93 ADD AGYALPHA - SORT ON AGY NAME (20 CHAR)       *         
*                     DOUBLE COUNTS FOR COMBINED STATIONS             *         
*  LEV 86    SEP13/94 ADD ADVERTISER NAME AND POINT PERSON KEYWORDS   *         
*  LEV 87    DEC22/94 INCREASE STATION OWNER TABLE SIZE               *         
*                                                                     *         
*  NOV10/95 (BU ) --- CHANGE REGENALL -> REGENALL1 FOR 2K CONTRACTS   *         
*                                                                     *         
*  APR26/96 (BU ) --- INCREASE STATION OWNER TABLE SIZE               *         
*                                                                     *         
*                     ***  END TOMBSTONE  ***                         *         
***********************************************************************         
REDRIVER CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**REDV**,R8,R7,R6                                              
         L     RA,0(R1)                                                         
         USING GLOBALD,RA                                                       
         L     RC,GLAWORKD                                                      
         USING GEND,RC                                                          
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
                                                                                
         CLI   GLHOOK,GLRESOLV                                                  
         BNE   REP20                                                            
         BAS   RE,SYSRES           EITHER RESOLVING ADDRESSES                   
         B     XIT                                                              
         SPACE                                                                  
REP20    CLI   GLHOOK,GLROUT                                                    
         BNE   REP40                                                            
         BAS   RE,SYSEXEC          OR EXECUTING ROUTINES                        
         SPACE                                                                  
REP40    B     XIT                                                              
         EJECT                                                                  
*              RESOLVING ROUTINE ADDRESSES                                      
         SPACE 3                                                                
SYSRES   NTR1                                                                   
         LA    R1,ROUTLIST                                                      
         SPACE                                                                  
SYSRES10 CLC   0(8,R1),GLLABEL                                                  
         BE    SYSRES20                                                         
         LA    R1,12(R1)                                                        
         CLI   0(R1),X'FF'                                                      
         BNE   SYSRES10                                                         
         B     XIT                                                              
         SPACE                                                                  
SYSRES20 MVC   GLAROUT,8(R1)       RETURN ADDRESS                               
         SPACE                                                                  
* NOW CHECK FOR COUNTS DATE SETUPS *                                            
         SPACE                                                                  
         CLC   PWCACT(3),0(R1)     THIS A PAPER WRK CT ACTIVE CONTRACTS         
         BE    SYSRES36                                                         
         CLC   PWCCCT(3),0(R1)     THIS A PAPER WRK CT CONT ACT CONTS           
         BE    SYSRES36                                                         
         CLC   PWCSCT(3),0(R1)     THIS A PAPER WRK CT SALES ACT CONTS          
         BE    SYSRES36                                                         
         SPACE                                                                  
         CLC   PWCTACT(3),0(R1)    THIS A PAPER WRK CT TOTAL ACTIVITY           
         BE    SYSRES36                                                         
         CLC   PWCTCCT(3),0(R1)    THIS A PAPER WRK CT TOT CONT ACT             
         BE    SYSRES36                                                         
         CLC   PWCTSCT(3),0(R1)    THIS A PAPER WRK CT TOT SALES ACT            
         BE    SYSRES36                                                         
         SPACE                                                                  
         CLC   PWCTYP1+1(7),1(R1)  THIS A PAPER WORK COUNT WEEK                 
         BNE   SYSRES24                                                         
         SPACE                                                                  
         BAS   RE,CALWEND          CALC WEEK END                                
         SPACE                                                                  
         B     SYSRES36                                                         
         SPACE                                                                  
SYSRES24 CLC   PWCTYP2+1(7),1(R1)  THIS A PAPER WORK COUNT MONTH                
         BNE   SYSRES26                                                         
         SPACE                                                                  
         BAS   RE,CALMEND          CALC MONTH END                               
         SPACE                                                                  
         B     SYSRES30                                                         
         SPACE                                                                  
SYSRES26 CLC   PWCTYP3+1(7),1(R1)  THIS A PAPER WORK COUNT QUARTER              
         BNE   SYSRES40                                                         
         SPACE                                                                  
         BAS   RE,CALQEND          CALC QUARTER END                             
         SPACE 2                                                                
* FORCE START ACTIVITY DATE TO BROADCAST MONTH START                            
         SPACE                                                                  
SYSRES30 GOTO1 QGTBROAD,DMCB,(1,REQASTR),WORK,GETDAY,ADDAY                      
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                ERROR                                        
         SPACE                                                                  
         CLC   REQASTR,WORK        START OK                                     
         BE    SYSRES34                                                         
         BH    *+6                                                              
         DC    H'0'                                                             
         MVC   REQAASTR,WORK                                                    
         SPACE                                                                  
SYSRES34 CLI   GLLABEL,C'S'        SALES                                        
         BNE   *+12                                                             
         OI    REQFLAGS,REQPWCSA   SET ON SALES PWC                             
         B     SYSRES36                                                         
         SPACE                                                                  
         CLI   GLLABEL,C'C'        CONTRACT                                     
         BNE   *+12                                                             
         OI    REQFLAGS,REQPWCCO   SET ON CONTRACT PWC                          
         B     SYSRES36                                                         
         DC    H'0'                                                             
         SPACE                                                                  
SYSRES36 OI    REQFLAGS,REQPWCR    SET ON READ PAPER WORK CT RECS               
         B     XIT                                                              
         SPACE                                                                  
SYSRES40 TM    REQTABLE,REQOFRG    FILTERING ON REGION?                         
         BO    *+14                                                             
         CLC   RRGIN,0(R1)         THIS A REGION REQUEST                        
         BNE   SYSRES50                                                         
         OI    REQTABLE,REQOFRG    SET ON READ OFFICE RECS - REGION TAB         
         LA    RE,OFFREGTB                                                      
         ST    RE,REREGTAB                                                      
         LA    RF,L'OFFREGTB/4                                                  
         STC   RF,REREGTAB                                                      
         CH    RF,=H'255'                                                       
         BNH   SYSRES50                                                         
         DC    H'0'                                                             
         SPACE                                                                  
SYSRES50 TM    REQTABLE,REQCTCL    SET ON READ CATEGORY RECS-CLASS TAB          
         BO    *+14                                                             
         CLC   RCLIN,0(R1)         THIS A CLASS REQUEST                         
         BNE   SYSRES60                                                         
         OI    REQTABLE,REQCTCL    SET ON READ CATEGORY RECS-CLASS TAB          
         LA    RE,CATCLSTB                                                      
         ST    RE,RECLSTAB                                                      
         LA    RF,L'CATCLSTB/4                                                  
         STC   RF,RECLSTAB                                                      
         CH    RF,=H'255'                                                       
         BNH   SYSRES60                                                         
         DC    H'0'                                                             
         SPACE                                                                  
SYSRES60 TM    REQTABLE,REQSTOW    FILTERING ON STATION OWNER                   
         BO    SYSRES64                                                         
         CLC   RRKIN,0(R1)         THIS A STATION RANK REQUEST                  
         BE    SYSRES64                                                         
         CLC   RTVIN,0(R1)         THIS A STATION TVB REQUEST                   
         BE    SYSRES64                                                         
         CLC   RMCDIN,0(R1)        THIS A STATION MARKET CODE REQUEST           
         BE    SYSRES64                                                         
         CLC   ROWIN,0(R1)         THIS A STATION OWNER REQUEST                 
         BNE   SYSRES70                                                         
SYSRES64 OI    REQTABLE,REQSTOW    SET ON READ STATION RECS-OWNER               
         LA    RE,STAOWNTB                                                      
         ST    RE,REOWNTAB                                                      
         LA    RF,L'STAOWNTB/(L'RESTA+L'REOWN+L'RERNK+L'RETVB)                  
         SRL   RF,3                DIVIDE BY 8                                  
         STC   RF,REOWNTAB                                                      
         CH    RF,=H'255'                                                       
         BNH   SYSRES70                                                         
         DC    H'0'                                                             
SYSRES70 TM    REQTABLE,REQSPPP    SET ON READ POINT PERSON-SAL TAB             
         BO    *+14                                                             
         CLC   RPPIN,0(R1)         THIS A POINT PERSON REQUEST                  
         BNE   XIT                                                              
         OI    REQTABLE,REQSPPP    SET ON READ POINT PERSON-SAL TAB             
         B     XIT                                                              
         EJECT                                                                  
*              EXECUTING ROUTINES (ROWS)                                        
         SPACE 3                                                                
SYSEXEC  NTR1                                                                   
         MVC   WORK,MYSPACES       PRESET WORK AREAS                            
         ZAP   DUB,=P'0'                                                        
         L     RF,GLAROUT                                                       
         L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         SPACE                                                                  
         CLI   GLMODE,GLOUTPUT                                                  
         BE    SYSOUT                                                           
         L     R1,GLADTENT                                                      
         USING DRIND,R1                                                         
         MVC   MYILEN,DRINLEN      SAVE INPUT LEN & TYPE                        
         MVC   MYITYPE,DRINTYPE                                                 
         ZIC   R3,DRINLEN          PICK UP INPUT LENGTH-1 INTO R3               
         BCTR  R3,0                                                             
         L     R4,REAREC           R4=A(RECORD)                                 
         MVI   ELTYPE,0                                                         
         STCM  RF,15,FULL          SAVE RF BRANCH ADDR                          
         BAS   RE,COLFILT                                                       
         BNE   XIT                                                              
         L     RF,FULL             RESTORE BRANCH ADDR                          
         BR    RF                                                               
         DROP  R1                                                               
         SPACE                                                                  
SYSOUT   MVC   OUTAREA,MYSPACES    PRESET SOME FIELDS FOR OUTPUT                
         L     R1,GLADTENT                                                      
         USING DROD,R1                                                          
         CLI   DROLTYP,C'N'        NO PRINT - NOT INTERESTED                    
         BE    XIT                                                              
         MVC   MYPOSO,DROPOS                                                    
         MVC   MYOLEN,DROLEN                                                    
         DROP  R1                                                               
         BR    RF                                                               
         EJECT                                                                  
*              GENERAL ROUTINES                                                 
         SPACE 3                                                                
RPIN     MVC   0(L'REREP,R2),REREP    REP                                       
         B     XIT                                                              
         SPACE                                                                  
RGIN     MVC   0(L'REREG,R2),REREG    REGION                                    
         B     XIT                                                              
         SPACE                                                                  
OFIN     MVC   0(L'REOFF,R2),REOFF     OFFICE                                   
         B     XIT                                                              
         SPACE                                                                  
GSIN     MVC   0(L'REGRS,R2),REGRS     GRP + SUB                                
         B     XIT                                                              
         SPACE                                                                  
GPIN     MVC   0(L'REGRP,R2),REGRP     GRP ONLY                                 
         B     XIT                                                              
         SPACE                                                                  
STIN     MVC   0(L'RESTA,R2),RESTA     STATION                                  
         B     XIT                                                              
         SPACE                                                                  
SLIN     MVC   0(L'RESAL,R2),RESAL     SALESPERSON                              
         B     XIT                                                              
         SPACE                                                                  
DTIN     MVC   0(L'REDVT,R2),REDVT     DIV + TEAM                               
         B     XIT                                                              
         SPACE                                                                  
ADIN     MVC   0(L'READV,R2),READV     ADVERTISER                               
         B     XIT                                                              
         SPACE                                                                  
ADAIN    MVC   NEEDSVRD,REREAD     SAVE                                         
         MVC   NEEDSVKY,REKEY                                                   
         MVI   REREAD,RADVKTYQ     ADVERTISER ALPHA                             
         SPACE                                                                  
         LA    R4,L'READV-1                                                     
         LA    R5,L'RADVNAME-1                                                  
         XC    NEEDKEY,NEEDKEY                                                  
         MVC   NEEDKEY(L'READV),READV                                           
         MVC   RENAME,MYSPACES                                                  
         SPACE                                                                  
         BAS   RE,NEEDNAM                                                       
         SPACE                                                                  
         OC    RENAME,MYSPACES                                                  
         MVC   0(L'RADVNAME,R2),RENAME       ADVERTISER NAME                    
         SPACE                                                                  
         XC    NEEDSVKY,NEEDSVKY             RESET                              
         B     XIT                                                              
         SPACE                                                                  
AGIN     MVC   0(L'REAGY,R2),REAGY           AGENCY                             
         MVC   L'REAGY(L'REAOF,R2),REAOF     AGENCY OFFICE                      
         OC    L'REAGY(L'REAOF,R2),MYSPACES                                     
         B     XIT                                                              
         SPACE                                                                  
AGAIN    MVC   NEEDSVRD,REREAD     SAVE                                         
         MVC   NEEDSVKY,REKEY                                                   
         MVI   REREAD,RAGYKTYQ     AGENCY ALPHA                                 
         SPACE                                                                  
         OC    REAOF,MYSPACES                                                   
         LA    R4,L'REAGY+L'REAOF-1                                             
         LA    R5,L'RAGYNAM1-1                                                  
         XC    NEEDKEY,NEEDKEY                                                  
         MVC   NEEDKEY(L'REAGY),REAGY                                           
         MVC   NEEDKEY+L'REAGY(L'REAOF),REAOF                                   
         MVC   RENAME,MYSPACES                                                  
         SPACE                                                                  
         BAS   RE,NEEDNAM                                                       
         SPACE                                                                  
         OC    RENAME,MYSPACES                                                  
         MVC   0(L'RAGYNAM1,R2),RENAME       AGENCY NAME                        
         SPACE                                                                  
         XC    NEEDSVKY,NEEDSVKY             RESET                              
         B     XIT                                                              
         SPACE                                                                  
CLIN     MVC   0(L'RECLS,R2),RECLS     CLASS                                    
         B     XIT                                                              
         SPACE                                                                  
CAIN     MVC   0(L'RECTG,R2),RECTG     CATEGORY                                 
         B     XIT                                                              
         SPACE                                                                  
PRIN     MVC   0(L'REPRD,R2),REPRD     PRODUCT CODE                             
         CLC   REPRD,MYSPACES          PRODUCT CODE PRESENT                     
         BE    PRIN10                                                           
         OC    REPRD,REPRD             PRODUCT CODE PRESENT                     
         BNZ   XIT                                                              
PRIN10   MVC   0(L'REPRDN,R2),REPRDN   PRODUCT NAME                             
         B     XIT                                                              
         SPACE                                                                  
CTIN     MVC   0(L'RECTY,R2),RECTY     CONTRACT TYPE                            
         B     XIT                                                              
         SPACE                                                                  
RKIN     MVC   0(L'RERNK,R2),RERNK     STATION RANK                             
         B     XIT                                                              
         SPACE                                                                  
TVIN     MVC   0(L'RETVB,R2),RETVB     STATION TVB                              
         B     XIT                                                              
         SPACE                                                                  
SYIN     MVC   0(L'RESTAT,R2),RESTAT   STATION TYPE                             
         B     XIT                                                              
         SPACE                                                                  
OWIN     MVC   0(L'REOWN,R2),REOWN     STATION OWNER                            
         B     XIT                                                              
         SPACE                                                                  
DEMIN    MVC   0(L'REDEMO,R2),REDEMO   DEMO                                     
         B     XIT                                                              
         SPACE                                                                  
BOKIN    MVC   0(L'REBOOK3,R2),REBOOK3  BOOK FROM X'12' ELEM                    
         MVI   3(R2),X'FF'              AND FLAG IT                             
         CLI   1(R2),0                  IS THERE DATA                           
         BNE   XIT                                                              
         MVC   0(L'REBOOK6,R2),REBOOK6  NO/USE X'10' ELEM                       
*        GOTO1 =V(PRNTBL),DMCB,=C'BOOKIN',REBOOK,C'DUMP',3,=C'1D'               
         B     XIT                                                              
         SPACE                                                                  
PTPIN    MVC   0(L'REPTP,R2),REPTP     POINT PERSON                             
         B     XIT                                                              
         SPACE                                                                  
DPTIN    MVC   0(L'REDPT,R2),REDPT     DAYPART                                  
         B     XIT                                                              
         SPACE                                                                  
SRVCIN   MVC   0(L'RESRVC,R2),RESRVC   RATING SERVICE                           
         B     XIT                                                              
         SPACE                                                                  
MKTIN    MVC   0(L'REMKT,R2),REMKT     GEOGRAPHIC SURVEY                        
         B     XIT                                                              
         SPACE                                                                  
MCDIN    MVC   0(L'REMCD,R2),REMCD     MARKET CODE                              
         B     XIT                                                              
         SPACE                                                                  
* TOTAL COUNT OF CONTRACTS IN REQUEST PERIOD *                                  
         SPACE                                                                  
TCT      DS    0H                  IF OUTSIDE DATES, DON'T COUNT                
         CLC   REQPSTRB,RECONEND                                                
         BH    XIT                                                              
         CLC   REQPENDB,RECONSTR                                                
         BL    XIT                                                              
         OC    REQADDST,REQADDST   FILTER ON CONT ADD DATE?                     
         BZ    TCT05                                                            
         CLC   RECADT,REQADDST     START                                        
         BL    XIT                                                              
         CLC   RECADT,REQADDED     END                                          
         BH    XIT                                                              
TCT05    MVI   3(R2),1                                                          
         SPACE                                                                  
         CLI   RESTA+4,C'C'        THIS A COMBINED STATION                      
         BNE   XIT                                                              
         MVI   3(R2),2                                                          
         B     XIT                                                              
                                                                                
* TOTAL COUNT OF DARE AND NON-DARE CONTRACTS *                                  
         SPACE                                                                  
DXT      CLC   REQPSTRB,RECONEND   IF OUTSIDE DATES, DON'T COUNT                
         BH    XIT                                                              
         CLC   REQPENDB,RECONSTR                                                
         BL    XIT                                                              
         CLI   GLARGS,C'X'         XCLUDE DARE CONTRACTS                        
         BNE   DXT02                                                            
         L     R4,REAREC           YES                                          
         MVI   ELCODE,X'1D'        IF DARE                                      
         BAS   RE,GETEL                                                         
         BE    XIT                 XCLUDE IT                                    
*                                                                               
DXT02    DS    0H                                                               
**       L     R4,REAREC           IF FORECAST                                  
**       MVI   ELCODE,X'23'                                                     
**       BAS   RE,GETEL                                                         
**       BE    XIT                 XCLUDE                                       
* IF CONTRACT BUCKETS, INCLUDE IT                                               
         L     R4,REAREC           IF NO BUCKETS, THUS PENDING                  
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   XIT                    XCLUDE                                    
         DROP  R4                                                               
*                                                                               
         OC    REQADDST,REQADDST   FILTER ON CONT ADD DATE?                     
         BZ    DXT05                                                            
         CLC   RECADT,REQADDST     START                                        
         BL    XIT                                                              
         CLC   RECADT,REQADDED     END                                          
         BH    XIT                                                              
DXT05    MVI   3(R2),1                                                          
         SPACE                                                                  
         CLI   RESTA+4,C'C'        THIS A COMBINED STATION                      
         BNE   XIT                                                              
         MVI   3(R2),2                                                          
         B     XIT                                                              
                                                                                
* DARE CONTRACTS                                                                
DCT      CLC   REQPSTRB,RECONEND   IF OUTSIDE DATES, DON'T COUNT                
         BH    XIT                                                              
         CLC   REQPENDB,RECONSTR                                                
         BL    XIT                                                              
         L     R4,REAREC                                                        
         MVI   ELCODE,X'1D'        DO WE HAVE A DARE ELEM ?                     
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         SPACE                                                                  
         OC    REQADDST,REQADDST   FILTER ON CONT ADD DATE?                     
         BZ    DCT20                                                            
         CLC   RECADT,REQADDST     START                                        
         BL    XIT                                                              
         CLC   RECADT,REQADDED     END                                          
         BH    XIT                                                              
         SPACE                                                                  
DCT20    MVI   3(R2),1                                                          
         SPACE                                                                  
         CLI   RESTA+4,C'C'        THIS A COMBINED STATION                      
         BNE   XIT                                                              
         MVI   3(R2),2                                                          
         B     XIT                                                              
         EJECT                                                                  
* COUNT CONTRACTS WITH ACTIVITY IN ACTIVITY PERIOD (CONTRACT & SALES)           
         SPACE                                                                  
ACT      ICM   R6,15,REAPWC        GET PWC CT REC ADDR                          
         BZ    XIT                                                              
         SPACE                                                                  
         CLC   REQAAEND,REPWCWDT   ACT END TO CONTRACT START                    
         BL    ACTX                                                             
         CLC   REQAASTR,REPCENDT   ACT STR TO CON OR BUY END                    
         BNH   ACT10                                                            
         CLC   REQAASTR,REPSENDT   ACT STR TO SALES END                         
         BH    ACTX                                                             
         SPACE                                                                  
ACT10    SR    R3,R3                                                            
         ZIC   R4,REQAAWKS         GET WKS IN REQUEST PERIOD                    
         SPACE                                                                  
         CLC   REPWCWDT,REQAASTR   PWC WK OF DATE TO PERIOD START               
         BE    ACT26                 ALL DONE                                   
         BH    ACT24                                                            
         SPACE                                                                  
* FIND # WKS BETWEEN ELEM START AND PERIOD START                                
         SPACE                                                                  
         GOTO1 PERVERT,DMCB,REPWCWDT,REQAASTR                                   
         SPACE                                                                  
         IC    R3,DMCB+13          GET WKS PAST START OF ELEM                   
         B     ACT26                                                            
         SPACE                                                                  
* FIND # WKS BETWEEN PERIOD START AND ELEM START                                
         SPACE                                                                  
ACT24    GOTO1 PERVERT,DMCB,REQAASTR,REPWCWDT                                   
         SPACE                                                                  
         ZIC   RF,DMCB+13                                                       
         CR    R4,RF               IS DIFF MORE THAN WKS IN PERIOD              
         BNH   ACTX                                                             
         SR    R4,RF                                                            
         SPACE                                                                  
* NOW LOOK FOR ANY ACTIVITY *                                                   
         SPACE                                                                  
ACT26    LA    R6,38(,R6)                                                       
         CLI   0(R6),01                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,1(R6)                                                         
         AR    R6,R1               START OF NEXT ELEM                           
         CLI   0(R6),02                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
ACT30    LA    RE,2(R3,R6)         START INTO ELEM IF NEEDED                    
         LR    RF,R4                                                            
         ZIC   R1,1(R6)                                                         
         AR    R1,R6               START OF NEXT ELEM                           
         SPACE                                                                  
ACT34    CR    R1,RE               CK IF AT END OF ELEM                         
         BNH   ACT36                                                            
         CLI   0(RE),0             ANY ACTIVITY                                 
         BNE   ACT40                YES                                         
         LA    RE,1(,RE)                                                        
         BCT   RF,ACT34                                                         
         SPACE                                                                  
ACT36    CLI   0(R1),04            THIS LAST ELEM TO CK                         
         BH    ACTX                                                             
         LR    R6,R1                                                            
         B     ACT30                                                            
         SPACE                                                                  
ACT40    MVI   3(R2),1                                                          
         SPACE                                                                  
         CLI   RESTA+4,C'C'        THIS A COMBINED STATION                      
         BNE   ACTX                                                             
         MVI   3(R2),2                                                          
         SPACE                                                                  
ACTX     B     XIT                                                              
         EJECT                                                                  
* COUNT CONTRACTS WITH ACTIVITY IN ACTIVITY PERIOD (CONTRACT)                   
         SPACE                                                                  
CCT      ICM   R6,15,REAPWC        GET PWC CT REC ADDR                          
         BZ    XIT                                                              
         SPACE                                                                  
         CLI   REQAPER,C'Y'                                                     
         BE    *+14                                                             
         SR    R3,R3               START AT CONTRACT START                      
         LA    R4,255              GO FOR MAX                                   
         B     CCT26                                                            
         SPACE                                                                  
         CLC   REQAAEND,REPWCWDT   ACT END TO CONTRACT START                    
         BL    CCTX                                                             
         CLC   REQAASTR,REPCENDT   ACT STR TO CON OR BUY END                    
         BNH   CCT10                                                            
         CLC   REQAASTR,REPSENDT   ACT STR TO SALES END                         
         BH    CCTX                                                             
         SPACE                                                                  
CCT10    SR    R3,R3                                                            
         ZIC   R4,REQAAWKS         GET WKS IN REQUEST PERIOD                    
         SPACE                                                                  
         CLC   REPWCWDT,REQAASTR   PWC WK OF DATE TO PERIOD START               
         BE    CCT26                 ALL DONE                                   
         BH    CCT24                                                            
         SPACE                                                                  
* FIND # WKS BETWEEN ELEM START AND PERIOD START                                
         SPACE                                                                  
         GOTO1 PERVERT,DMCB,REPWCWDT,REQAASTR                                   
         SPACE                                                                  
         IC    R3,DMCB+13          GET WKS PAST START OF ELEM                   
         B     CCT26                                                            
         SPACE                                                                  
* FIND # WKS BETWEEN PERIOD START AND ELEM START                                
         SPACE                                                                  
CCT24    GOTO1 PERVERT,DMCB,REQAASTR,REPWCWDT                                   
         SPACE                                                                  
         ZIC   RF,DMCB+13                                                       
         CR    R4,RF               IS DIFF MORE THAN WKS IN PERIOD              
         BNH   CCTX                                                             
         SR    R4,RF                                                            
         SPACE                                                                  
* NOW LOOK FOR ANY ACTIVITY *                                                   
         SPACE                                                                  
CCT26    LA    R6,38(,R6)                                                       
         CLI   0(R6),01                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,1(R6)                                                         
         AR    R6,R1               START OF NEXT ELEM                           
         CLI   0(R6),02                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
CCT30    LA    RE,2(R3,R6)         START INTO ELEM IF NEEDED                    
         LR    RF,R4                                                            
         ZIC   R1,1(R6)                                                         
         AR    R1,R6               START OF NEXT ELEM                           
         SPACE                                                                  
CCT34    CR    R1,RE               CK IF AT END OF ELEM                         
         BNH   CCT36                                                            
         CLI   0(RE),0             ANY ACTIVITY                                 
         BNE   CCT40                YES                                         
         LA    RE,1(,RE)                                                        
         BCT   RF,CCT34                                                         
         SPACE                                                                  
CCT36    CLI   0(R1),03            THIS LAST ELEM TO CK                         
         BH    CCTX                                                             
         LR    R6,R1                                                            
         B     CCT30                                                            
         SPACE                                                                  
CCT40    MVI   3(R2),1                                                          
         SPACE                                                                  
         CLI   RESTA+4,C'C'        THIS A COMBINED STATION                      
         BNE   CCTX                                                             
         MVI   3(R2),2                                                          
         SPACE                                                                  
CCTX     B     XIT                                                              
         EJECT                                                                  
* COUNT CONTRACTS WITH SALES ACTIVITY IN ACTIVITY PERIOD                        
         SPACE                                                                  
SCT      ICM   R6,15,REAPWC        GET PWC CT REC ADDR                          
         BZ    XIT                                                              
         SPACE                                                                  
         CLI   REQAPER,C'Y'                                                     
         BE    *+14                                                             
         SR    R3,R3               START AT CONTRACT START                      
         LA    R4,255              GO FOR MAX                                   
         B     SCT26                                                            
         SPACE                                                                  
         CLC   REQAAEND,REPWCWDT   ACT END TO CONTRACT START                    
         BL    SCTX                                                             
         CLC   REQAASTR,REPCENDT   ACT STR TO CON OR BUY END                    
         BNH   SCT10                                                            
         CLC   REQAASTR,REPSENDT   ACT STR TO SALES END                         
         BH    SCTX                                                             
         SPACE                                                                  
SCT10    SR    R3,R3                                                            
         ZIC   R4,REQAAWKS         GET WKS IN REQUEST PERIOD                    
         SPACE                                                                  
         CLC   REPWCWDT,REQAASTR   PWC WK OF DATE TO PERIOD START               
         BE    SCT26                 ALL DONE                                   
         BH    SCT24                                                            
         SPACE                                                                  
* FIND # WKS BETWEEN ELEM START AND PERIOD START                                
         SPACE                                                                  
         GOTO1 PERVERT,DMCB,REPWCWDT,REQAASTR                                   
         SPACE                                                                  
         IC    R3,DMCB+13          GET WKS PAST START OF ELEM                   
         B     SCT26                                                            
         SPACE                                                                  
* FIND # WKS BETWEEN PERIOD START AND ELEM START                                
         SPACE                                                                  
SCT24    GOTO1 PERVERT,DMCB,REQAASTR,REPWCWDT                                   
         SPACE                                                                  
         ZIC   RF,DMCB+13                                                       
         CR    R4,RF               IS DIFF MORE THAN WKS IN PERIOD              
         BNH   SCTX                                                             
         SR    R4,RF                                                            
         SPACE                                                                  
* NOW LOOK FOR ANY ACTIVITY *                                                   
         SPACE                                                                  
SCT26    LA    R6,38(,R6)                                                       
         CLI   0(R6),01                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,1(R6)                                                         
         AR    R6,R1               START OF NEXT ELEM                           
         CLI   0(R6),02                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,1(R6)                                                         
         AR    R6,R1               START OF NEXT ELEM                           
         CLI   0(R6),03                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,1(R6)                                                         
         AR    R6,R1               START OF NEXT ELEM                           
         CLI   0(R6),04                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
SCT30    LA    RE,2(R3,R6)         START INTO ELEM IF NEEDED                    
         LR    RF,R4                                                            
         ZIC   R1,1(R6)                                                         
         AR    R1,R6               START OF NEXT ELEM                           
         SPACE                                                                  
SCT34    CR    R1,RE               CK IF AT END OF ELEM                         
         BNH   SCTX                                                             
         CLI   0(RE),0             ANY ACTIVITY                                 
         BNE   SCT40                YES                                         
         LA    RE,1(,RE)                                                        
         BCT   RF,SCT34                                                         
         B     SCTX                                                             
         SPACE                                                                  
SCT40    MVI   3(R2),1                                                          
         SPACE                                                                  
         CLI   RESTA+4,C'C'        THIS A COMBINED STATION                      
         BNE   SCTX                                                             
         MVI   3(R2),2                                                          
         SPACE                                                                  
SCTX     B     XIT                                                              
         EJECT                                                                  
* COUNT ALL SALES & CONTRACT ACTIVITY IN ACTIVITY PERIOD *                      
         SPACE                                                                  
AATOT    ICM   R6,15,REAPWC        GET PWC CT REC ADDR                          
         BZ    XIT                                                              
         SPACE                                                                  
         CLC   REQAAEND,REPWCWDT   ACT END TO CONTRACT START                    
         BL    AATOTX                                                           
         CLC   REQAASTR,REPCENDT   ACT STR TO CON OR BUY END                    
         BNH   AATOT10                                                          
         CLC   REQAASTR,REPSENDT   ACT STR TO SALES END                         
         BH    AATOTX                                                           
         SPACE                                                                  
AATOT10  SR    R3,R3                                                            
         ZIC   R4,REQAAWKS         GET WKS IN REQUEST PERIOD                    
         SPACE                                                                  
         CLC   REPWCWDT,REQAASTR   PWC WK OF DATE TO PERIOD START               
         BE    AATOT26               ALL DONE                                   
         BH    AATOT24                                                          
         SPACE                                                                  
* FIND # WKS BETWEEN ELEM START AND ACTIVITY START                              
         SPACE                                                                  
         GOTO1 PERVERT,DMCB,REPWCWDT,REQAASTR                                   
         SPACE                                                                  
         IC    R3,DMCB+13          GET WKS PAST START OF ELEM                   
         B     AATOT26                                                          
         SPACE                                                                  
* FIND # WKS BETWEEN ACTIVITY START AND ELEM START                              
         SPACE                                                                  
AATOT24  GOTO1 PERVERT,DMCB,REQAASTR,REPWCWDT                                   
         SPACE                                                                  
         ZIC   RF,DMCB+13                                                       
         CR    R4,RF               IS DIFF MORE THAN WKS IN ACTIVITY            
         BNH   AATOTX                                                           
         SR    R4,RF                                                            
         SPACE                                                                  
* NOW LOOK FOR ANY ACTIVITY *                                                   
         SPACE                                                                  
AATOT26  LA    R6,38(,R6)                                                       
         CLI   0(R6),01                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,1(R6)                                                         
         AR    R6,R1               START OF NEXT ELEM                           
         CLI   0(R6),02                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
AATOT30  LA    RE,2(R3,R6)         START INTO ELEM IF NEEDED                    
         LR    RF,R4                                                            
         ZIC   R1,1(R6)                                                         
         AR    R1,R6               START OF NEXT ELEM                           
         EJECT                                                                  
* ADD ALL ACTIVITY                                                              
         SPACE                                                                  
AATOT32  CR    R1,RE               CK IF AT END OF ELEM                         
         BNH   AATOT36                                                          
         SPACE                                                                  
         CLI   0(RE),0             ANY ACTIVITY                                 
         BE    AATOT34              NO                                          
         SPACE                                                                  
         ST    R1,DUB                                                           
         ZIC   R1,0(RE)                                                         
         SPACE                                                                  
         CLI   RESTA+4,C'C'        THIS A COMBINED STATION                      
         BNE   AATOT33                                                          
         SLL   R1,1                TIMES 2                                      
         SPACE                                                                  
AATOT33  A     R1,0(,R2)                                                        
         ST    R1,0(,R2)                                                        
         L     R1,DUB                                                           
AATOT34  LA    RE,1(,RE)                                                        
         BCT   RF,AATOT32                                                       
         SPACE                                                                  
AATOT36  CLI   0(R1),04            THIS LAST ELEM TO CK                         
         BH    AATOTX                                                           
         LR    R6,R1                                                            
         B     AATOT30                                                          
         SPACE                                                                  
AATOTX   B     XIT                                                              
         EJECT                                                                  
* COUNT ALL CONTRACT ACTIVITY IN ACTIVITY PERIOD *                              
         SPACE                                                                  
CATOT    ICM   R6,15,REAPWC        GET PWC CT REC ADDR                          
         BZ    XIT                                                              
         SPACE                                                                  
         CLC   REQAAEND,REPWCWDT   ACT END TO CONTRACT START                    
         BL    CATOTX                                                           
         CLC   REQAASTR,REPCENDT   ACT STR TO CON OR BUY END                    
         BNH   CATOT10                                                          
         SPACE                                                                  
CATOT10  SR    R3,R3                                                            
         ZIC   R4,REQAAWKS         GET WKS IN REQUEST PERIOD                    
         SPACE                                                                  
         CLC   REPWCWDT,REQAASTR   PWC WK OF DATE TO ACTIVITY START             
         BE    CATOT26               ALL DONE                                   
         BH    CATOT24                                                          
         SPACE                                                                  
* FIND # WKS BETWEEN ELEM START AND ACTIVITY START                              
         SPACE                                                                  
         GOTO1 PERVERT,DMCB,REPWCWDT,REQAASTR                                   
         SPACE                                                                  
         IC    R3,DMCB+13          GET WKS PAST START OF ELEM                   
         B     CATOT26                                                          
         SPACE                                                                  
* FIND # WKS BETWEEN ACTIVITY START AND ELEM START                              
         SPACE                                                                  
CATOT24  GOTO1 PERVERT,DMCB,REQAASTR,REPWCWDT                                   
         SPACE                                                                  
         ZIC   RF,DMCB+13                                                       
         CR    R4,RF               IS DIFF MORE THAN WKS IN PERIOD              
         BNH   CATOTX                                                           
         SR    R4,RF                                                            
         SPACE                                                                  
* NOW LOOK FOR ANY ACTIVITY *                                                   
         SPACE                                                                  
CATOT26  LA    R6,38(,R6)                                                       
         CLI   0(R6),01                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,1(R6)                                                         
         AR    R6,R1               START OF NEXT ELEM                           
         CLI   0(R6),02                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
CATOT30  LA    RE,2(R3,R6)         START INTO ELEM IF NEEDED                    
         LR    RF,R4                                                            
         ZIC   R1,1(R6)                                                         
         AR    R1,R6               START OF NEXT ELEM                           
         EJECT                                                                  
* ADD ALL ACTIVITY                                                              
         SPACE                                                                  
CATOT32  CR    R1,RE               CK IF AT END OF ELEM                         
         BNH   CATOT36                                                          
         SPACE                                                                  
         CLI   0(RE),0             ANY ACTIVITY                                 
         BE    CATOT34              NO                                          
         SPACE                                                                  
         ST    R1,DUB                                                           
         ZIC   R1,0(RE)                                                         
         SPACE                                                                  
         CLI   RESTA+4,C'C'        THIS A COMBINED STATION                      
         BNE   CATOT33                                                          
         SLL   R1,1                TIMES 2                                      
         SPACE                                                                  
CATOT33  A     R1,0(,R2)                                                        
         ST    R1,0(,R2)                                                        
         L     R1,DUB                                                           
CATOT34  LA    RE,1(,RE)                                                        
         BCT   RF,CATOT32                                                       
         SPACE                                                                  
CATOT36  CLI   0(R1),03            THIS LAST ELEM TO CK                         
         BH    CATOTX                                                           
         LR    R6,R1                                                            
         B     CATOT30                                                          
         SPACE                                                                  
CATOTX   B     XIT                                                              
         EJECT                                                                  
* COUNT ALL SALES ACTIVITY IN ACTIVITY PERIOD *                                 
         SPACE                                                                  
SATOT    ICM   R6,15,REAPWC        GET PWC CT REC ADDR                          
         BZ    XIT                                                              
         SPACE                                                                  
         CLC   REQAAEND,REPWCWDT   ACT END TO CONTRACT START                    
         BL    SATOTX                                                           
         CLC   REQAASTR,REPCENDT   ACT STR TO CON OR BUY END                    
         BNH   SATOT10                                                          
         CLC   REQAASTR,REPSENDT   ACT STR TO SALES END                         
         BH    SATOTX                                                           
         SPACE                                                                  
SATOT10  SR    R3,R3                                                            
         ZIC   R4,REQAAWKS         GET WKS IN REQUEST PERIOD                    
         SPACE                                                                  
         CLC   REPWCWDT,REQAASTR   PWC WK OF DATE TO ACTIVITY START             
         BE    SATOT26               ALL DONE                                   
         BH    SATOT24                                                          
         SPACE                                                                  
* FIND # WKS BETWEEN ELEM START AND ACTIVITY START                              
         SPACE                                                                  
         GOTO1 PERVERT,DMCB,REPWCWDT,REQAASTR                                   
         SPACE                                                                  
         IC    R3,DMCB+13          GET WKS PAST START OF ELEM                   
         B     SATOT26                                                          
         SPACE                                                                  
* FIND # WKS BETWEEN ACTIVITY START AND ELEM START                              
         SPACE                                                                  
SATOT24  GOTO1 PERVERT,DMCB,REQAASTR,REPWCWDT                                   
         SPACE                                                                  
         ZIC   RF,DMCB+13                                                       
         CR    R4,RF               IS DIFF MORE THAN WKS IN PERIOD              
         BNH   SATOTX                                                           
         SR    R4,RF                                                            
         SPACE                                                                  
SATOT26  LA    R6,38(,R6)          SCAN DOWN ELEMENTS                           
         CLI   0(R6),01                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,1(R6)                                                         
         AR    R6,R1               START OF NEXT ELEM                           
         CLI   0(R6),02                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,1(R6)                                                         
         AR    R6,R1               START OF NEXT ELEM                           
         CLI   0(R6),03                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,1(R6)                                                         
         AR    R6,R1               START OF NEXT ELEM                           
         CLI   0(R6),04                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
SATOT30  LA    RE,2(R3,R6)         START INTO ELEM IF NEEDED                    
         LR    RF,R4                                                            
         ZIC   R1,1(R6)                                                         
         AR    R1,R6               START OF NEXT ELEM                           
         SPACE                                                                  
* ADD ALL ACTIVITY                                                              
         SPACE                                                                  
SATOT32  CR    R1,RE               CK IF AT END OF ELEM                         
         BNH   SATOTX                                                           
         SPACE                                                                  
         CLI   0(RE),0             ANY ACTIVITY                                 
         BE    SATOT34              NO                                          
         SPACE                                                                  
         ST    R1,DUB                                                           
         ZIC   R1,0(RE)                                                         
         SPACE                                                                  
         CLI   RESTA+4,C'C'        THIS A COMBINED STATION                      
         BNE   SATOT33                                                          
         SLL   R1,1                TIMES 2                                      
         SPACE                                                                  
SATOT33  A     R1,0(,R2)                                                        
         ST    R1,0(,R2)                                                        
         L     R1,DUB                                                           
SATOT34  LA    RE,1(,RE)                                                        
         BCT   RF,SATOT32                                                       
         SPACE                                                                  
SATOTX   B     XIT                                                              
         EJECT                                                                  
* BUY ACTIVITY WEEK RTN - NOTE - WILL DO CONTRACT ALSO *                        
         SPACE                                                                  
CAWKIN   SR    R3,R3                                                            
         B     CACOM                                                            
         SPACE                                                                  
* BUY ACTIVITY MONTH RTN - NOTE - WILL DO CONTRACT ALSO *                       
         SPACE                                                                  
CAMNIN   LA    R3,1                                                             
         B     CACOM                                                            
         SPACE                                                                  
* BUY ACTIVITY QUARTER RTN - NOTE - WILL DO CONTRACT ALSO *                     
         SPACE                                                                  
CAQTIN   LA    R3,2                                                             
         SPACE                                                                  
CACOM    MVI   ELCODE,02                                                        
         B     AWKIN                                                            
         SPACE                                                                  
* SALES ACTIVITY WEEK RTN *                                                     
         SPACE                                                                  
SAWKIN   SR    R3,R3                                                            
         B     SACOM                                                            
         SPACE                                                                  
* SALES ACTIVITY MONTH RTN *                                                    
         SPACE                                                                  
SAMNIN   LA    R3,1                                                             
         B     SACOM                                                            
         SPACE                                                                  
* SALES ACTIVITY QUARTER RTN *                                                  
         SPACE                                                                  
SAQTIN   LA    R3,2                                                             
         SPACE                                                                  
SACOM    MVI   ELCODE,04                                                        
         EJECT                                                                  
* COMMON COUNT ACTIVITY ROUTINES *                                              
         SPACE                                                                  
AWKIN    OC    REAPWC,REAPWC       WAS PWC REC FOUND                            
         BZ    XIT                                                              
         OC    REPWCWDT,REPWCWDT   WEEK OF DATE FOUND YET                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLI   GLARGS,X'01'                                                     
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLI   GLARGS,X'0C'                                                     
         BNH   *+6                                                              
         DC    H'0'                                                             
         ZIC   R4,GLARGS                                                        
         SPACE                                                                  
* NOW GET STARTING WEEK DATE AND WEEKS IN PERIOD *                              
         SPACE                                                                  
         LTR   R3,R3               WEEK                                         
         BNZ   AWKIN10                                                          
         MVI   REPWKCT,1           SET 1 WEEK                                   
         BCTR  R4,0                                                             
         LTR   R4,R4                                                            
         BZ    *+8                                                              
         MH    R4,=H'7'                                                         
         GOTO1 ADDAY,DMCB,REQASTR,REPPERST,(R4)                                 
         MVC   REPPERND,REPPERST                                                
         B     AWKIN20                                                          
         SPACE                                                                  
AWKIN10  BCTR  R3,0                MONTH                                        
         LTR   R3,R3                                                            
         BNZ   AWKIN14                                                          
         SPACE                                                                  
         BAS   RE,CALWM            CALC WKS IN MONTH                            
         B     AWKIN20                                                          
         SPACE                                                                  
AWKIN14  BCTR  R3,0                QUARTER                                      
         LTR   R3,R3                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,CALWQ            CALC WKS IN QUARTER                          
         SPACE                                                                  
AWKIN20  CLC   REPWCWDT,REPPERND   IF PWC WK OF DATE AFTER END                  
         BH    XIT                  IGNORE IT                                   
         LA    R1,REPCENDT         CONTRACT END                                 
         CLI   ELCODE,04           THIS SALES COUNTS                            
         BNE   *+8                                                              
         LA    R1,REPSENDT         SALES END                                    
         SPACE                                                                  
         CLC   0(6,R1),REPPERST    COUNTS END BEFORE THIS PERIOD START          
         BL    XIT                                                              
         SPACE                                                                  
         MVC   DATADISP,=H'38'     SET FOR ROI                                  
         SPACE                                                                  
         MVI   REPWKST,0                                                        
         CLC   REPWCWDT,REPPERST   PWC WK OF DATE TO PERIOD START               
         BE    AWKIN30               NO DIFF                                    
         BH    AWKIN24                                                          
         SPACE                                                                  
* FIND # WKS BETWEEN ELEM START AND PERIOD START                                
         SPACE                                                                  
         GOTO1 PERVERT,DMCB,REPWCWDT,REPPERST                                   
         MVC   REPWKST,DMCB+13                                                  
         B     AWKIN30                                                          
         SPACE                                                                  
* FIND # WKS BETWEEN PERIOD START AND ELEM START                                
         SPACE                                                                  
AWKIN24  GOTO1 PERVERT,DMCB,REPPERST,REPWCWDT                                   
         ZIC   RE,REPWKCT                                                       
         ZIC   RF,DMCB+13                                                       
         CR    RE,RF               MORE WEEKS TO CT THAN DIFF                   
         BH    *+6                                                              
         DC    H'0'                                                             
         SR    RE,RF                                                            
         STC   RE,REPWKCT                                                       
         SPACE                                                                  
AWKIN30  L     R4,REAPWC                                                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R5,R5                                                            
         IC    R5,1(,R4)           GET ELEM LEN                                 
         BCTR  R5,0                 LESS                                        
         BCTR  R5,0                  TWO FOR CODE/LEN                           
         SPACE                                                                  
         SR    R1,R1                                                            
         IC    R1,REPWKST          GET WEEKS DIFF                               
         CR    R5,R1               IS THIS WEEK IN ELEM                         
         BNL   *+6                  YES                                         
         DC    H'0'                                                             
         ICM   R0,15,0(R2)                                                      
         ZIC   R3,REPWKCT          GET NUMBER OF WEEKS IN PERIOD                
         SPACE                                                                  
AWKIN50  CR    R5,R1               IS THIS WEEK IN ELEM                         
         BNH   AWKIN60              NO                                          
         SPACE                                                                  
         LA    RF,2(R1,R4)                                                      
         ZIC   RE,0(RF)                                                         
         CLI   RESTA+4,C'C'        THIS A COMBINED STATION                      
         BNE   AWKIN56                                                          
         SLL   RE,1                TIMES 2                                      
AWKIN56  AR    R0,RE                                                            
         LA    R1,1(,R1)                                                        
         SPACE                                                                  
         BCT   R3,AWKIN50          LOOP FOR WEEKS NEEDED                        
         SPACE                                                                  
AWKIN60  STCM  R0,15,0(R2)         SAVE WEEK CT                                 
         SPACE                                                                  
AWKIN70  CLI   ELCODE,02           BUY CTS                                      
         BNE   AWKINX                                                           
         MVI   ELCODE,03           ADD CONTRACT CTS TOO                         
         B     AWKIN30                                                          
         SPACE                                                                  
AWKINX   MVC   DATADISP,=H'34'     SET BACK TO REP                              
         B     XIT                                                              
         EJECT                                                                  
* CALCULATE WEEKS IN THIS MONTH                                                 
         SPACE                                                                  
CALWM    NTR1                                                                   
         MVC   WORK(6),REQASTR                                                  
CALWM10  GOTO1 QGTBROAD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                       
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                ERROR                                        
         SPACE                                                                  
         MVC   REPPERST(12),WORK+6  SAVE START/END DATES OF MONTH               
         SPACE                                                                  
         MVC   REPWKCT,DMCB         SAVE WEEKS IN THIS MONTH                    
         SPACE                                                                  
         GOTO1 ADDAY,DMCB,WORK+12,WORK,F'1'                                     
         BCT   R4,CALWM10                                                       
         XIT1                                                                   
         SPACE 2                                                                
* CALCULATE WEEKS IN THIS QUARTER                                               
         SPACE                                                                  
CALWQ    NTR1                                                                   
         MVC   WORK(6),REQASTR                                                  
CALWQ10  LA    R0,3                                                             
         SR    R3,R3                                                            
         MVC   REPPERST,WORK       SAVE START DATE OF MONTH                     
         SPACE                                                                  
* FORCE TO KNOWN QUARTER *                                                      
         SPACE                                                                  
         CLC   REPPERST+2(2),=C'10'                                             
         BL    CALWQ14                                                          
         SPACE                                                                  
CALWQ14  GOTO1 QGTBROAD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                       
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                ERROR                                        
         SPACE                                                                  
         ZIC   RE,DMCB                                                          
         AR    R3,RE                                                            
         MVC   REPPERND,WORK+12    SAVE END DATE OF MONTH                       
         SPACE                                                                  
         GOTO1 ADDAY,DMCB,WORK+12,WORK,F'1'                                     
         BCT   R0,CALWQ14                                                       
         SPACE                                                                  
         BCT   R4,CALWQ10                                                       
         SPACE                                                                  
         STC   R3,REPWKCT                                                       
         SPACE                                                                  
         XIT1                                                                   
         EJECT                                                                  
* CALC END DATE FOR THIS WEEK *                                                 
         SPACE                                                                  
CALWEND  NTR1                                                                   
         ZIC   R2,GLARGS                                                        
         BCTR  R2,0                                                             
         LTR   R2,R2                                                            
         BZ    CALWEN10                                                         
         MH    R2,=H'7'                                                         
CALWEN10 AH    R2,=H'6'                                                         
         B     CALALLX                                                          
         SPACE                                                                  
* CALC END DATE FOR THIS MONTH *                                                
         SPACE                                                                  
CALMEND  NTR1                                                                   
         SR    R2,R2                                                            
         ZIC   R0,GLARGS                                                        
         MVC   WORK(6),REQASTR                                                  
CALMEN10 GOTO1 QGTBROAD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                       
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                ERROR                                        
         SPACE                                                                  
         ZIC   RE,DMCB             GET WEEKS IN THIS MONTH                      
         AR    R2,RE                                                            
         GOTO1 ADDAY,(R1),WORK+12,WORK,F'1'                                     
         BCT   R0,CALMEN10                                                      
         MH    R2,=H'7'                                                         
         BCTR  R2,0                                                             
         B     CALALLX                                                          
         EJECT                                                                  
* CALC END DATE FOR THIS QUARTER *                                              
         SPACE                                                                  
CALQEND  NTR1                                                                   
         SPACE                                                                  
* 1ST BUILD QTRTABL IF NOT THERE                                                
         SPACE                                                                  
         OC    QTRTABL,QTRTABL     INITIALIZED YET                              
         BNZ   CALQEN30             YES                                         
         SPACE                                                                  
         LA    R2,24               GO BACK 2 YRS                                
         MVC   WORK(6),REQASTR                                                  
         SPACE                                                                  
         GOTO1 QGTBROAD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                       
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                ERROR                                        
         SPACE                                                                  
         GOTO1 ADDAY,DMCB,WORK+6,WORK,F'15'                                     
         CLC   =C'10',WORK+2                                                    
         BH    *+14                                                             
         MVC   WORK+2(2),=C'10'                                                 
         B     CALQEN10                                                         
         CLC   =C'07',WORK+2                                                    
         BH    *+14                                                             
         MVC   WORK+2(2),=C'07'                                                 
         B     CALQEN10                                                         
         CLC   =C'04',WORK+2                                                    
         BH    *+14                                                             
         MVC   WORK+2(2),=C'04'                                                 
         B     CALQEN10                                                         
         MVC   WORK+2(2),=C'01'                                                 
         SPACE                                                                  
CALQEN10 GOTO1 QGTBROAD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                       
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                ERROR                                        
         SPACE                                                                  
         GOTO1 ADDAY,DMCB,WORK+6,WORK,F'-1'                                     
         BCT   R2,CALQEN10                                                      
         SPACE                                                                  
         LA    R2,16                                                            
         LA    R3,QTRTABL                                                       
         SPACE                                                                  
CALQEN20 LA    R4,3                                                             
         SPACE                                                                  
         MVC   0(6,R3),WORK                                                     
         LA    R3,6(,R3)                                                        
         SPACE                                                                  
CALQEN24 GOTO1 QGTBROAD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                       
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                ERROR                                        
         SPACE                                                                  
         GOTO1 ADDAY,DMCB,WORK+12,WORK,F'-1'                                    
         BCT   R4,CALQEN24                                                      
         SPACE                                                                  
         BCT   R2,CALQEN20                                                      
         SPACE                                                                  
CALQEN30 SR    R2,R2                                                            
         ZIC   R0,GLARGS                                                        
         MVC   WORK(6),REQASTR                                                  
         SPACE                                                                  
CALQEN40 LA    R3,3                3 MONTHS PER QTR                             
         SPACE                                                                  
CALQEN44 GOTO1 QGTBROAD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                       
         SPACE                                                                  
         CLI   DMCB,X'FF'          ERROR                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,DMCB                                                          
         AR    R2,RE                                                            
         GOTO1 ADDAY,DMCB,WORK+12,WORK,F'1'                                     
         BCT   R3,CALQEN44                                                      
         SPACE                                                                  
         BCT   R0,CALQEN40                                                      
         MH    R2,=H'7'                                                         
         BCTR  R2,0                                                             
         SPACE                                                                  
CALALLX  GOTO1 ADDAY,DMCB,REQASTR,WORK,(R2)                                     
         CLC   REQAAEND,WORK                                                    
         BNL   XIT                                                              
         MVC   REQAAEND,WORK                                                    
         B     XIT                                                              
         EJECT                                                                  
* OUTPUT ROUTINES                                                               
         SPACE                                                                  
RPOUT    MVI   REREAD,RREPKTYQ                                                  
         MVC   REREP,0(R2)                                                      
         LA    R4,L'REREP-1                                                     
         LA    R5,L'RREPSHRT-1                                                  
         B     COMOUT                                                           
         SPACE                                                                  
RGOUT    MVI   REREAD,RREGKTYQ                                                  
         MVC   REREG,0(R2)                                                      
         LA    R4,L'REREG-1                                                     
         LA    R5,L'RREGNAME-1                                                  
         XC    NEEDKEY,NEEDKEY                                                  
         MVC   NEEDKEY(L'REREG),REREG                                           
         B     COMOUT                                                           
         SPACE                                                                  
OFOUT    MVI   REREAD,ROFFKTYQ     OFFICE                                       
         MVC   REOFF,0(R2)                                                      
         LA    R4,L'REOFF-1                                                     
         LA    R5,L'ROFFNAME-1                                                  
         XC    NEEDKEY,NEEDKEY                                                  
         MVC   NEEDKEY(L'REOFF),REOFF                                           
         B     COMOUT                                                           
         SPACE                                                                  
GSOUT    MVI   REREAD,RGRPKTYQ     GRP + SUB                                    
         MVC   REGRS,0(R2)                                                      
GSOUT10  LA    R4,L'REGRS-1                                                     
         LA    R5,L'RGRPNAME+L'RGRPSBNM  TOTAL IS SUM OF BOTH +1                
         XC    NEEDKEY,NEEDKEY                                                  
         MVC   NEEDKEY(L'REGRS),REGRS                                           
         B     COMOUT                                                           
         SPACE                                                                  
GPOUT    MVI   REREAD,RGRPKTYQ     GRP ONLY                                     
         MVC   REGRP,0(R2)                                                      
         MVI   RESUB,0             SIGNAL THAT THIS IS GRP ONLY                 
         B     GSOUT10                                                          
         SPACE                                                                  
STOUT    MVC   CODEAREA(4),0(R2)      STATION                                   
         LA    R1,CODEAREA+4                                                    
         CLI   CODEAREA+3,C' '     DON'T LEAVE A BLANK                          
         BH    *+6                                                              
         BCTR  R1,0                                                             
         SPACE                                                                  
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),4(R2)                                                    
         CLI   4(R2),C' '          BLANK IS TV                                  
         BNE   STOUT06                                                          
         MVI   1(R1),C'T'                                                       
         MVI   2(R1),C'V'                                                       
         B     STOUT10                                                          
         SPACE                                                                  
STOUT06  MVI   2(R1),C'M'                                                       
         CLI   4(R2),C'A'                                                       
         BE    STOUT10                                                          
         CLI   4(R2),C'F'                                                       
         BE    STOUT10                                                          
         MVI   2(R1),C' '                                                       
         SPACE                                                                  
STOUT10  MVI   REREAD,RSTAKTYQ     STATION                                      
         MVC   RESTA,0(R2)                                                      
         LA    R4,L'RESTA-1                                                     
         LA    R5,L'RSTAMKT-1                                                   
         XC    NEEDKEY,NEEDKEY                                                  
         MVC   NEEDKEY(L'RESTA),RESTA                                           
         B     COMOUT                                                           
         SPACE                                                                  
SLOUT    MVI   REREAD,RSALKTYQ     SALESPERSON                                  
         MVC   RESAL,0(R2)                                                      
         LA    R4,L'RESAL-1                                                     
         LA    R5,L'RSALNAME-1                                                  
         XC    NEEDKEY,NEEDKEY                                                  
         MVC   NEEDKEY(L'RESAL),RESAL                                           
         B     COMOUT                                                           
         SPACE                                                                  
DTOUT    MVI   REREAD,RTEMKTYQ     DIV + TEAM                                   
         MVC   REDVT,0(R2)                                                      
         LA    R4,L'REDVT-1                                                     
         LA    R5,L'RTEMDVNM+L'RTEMNAME-1                                       
         XC    NEEDKEY,NEEDKEY                                                  
         MVC   NEEDKEY(L'REDVT),REDVT                                           
         B     COMOUT                                                           
         SPACE                                                                  
ADOUT    MVI   REREAD,RADVKTYQ     ADVERTISER                                   
         MVC   READV,0(R2)                                                      
         LA    R4,L'READV-1                                                     
         LA    R5,L'RADVNAME-1                                                  
         XC    NEEDKEY,NEEDKEY                                                  
         MVC   NEEDKEY(L'READV),READV                                           
         B     COMOUT                                                           
         SPACE                                                                  
ADAOUT   DS   0H                                                                
         MVC   LABLAREA(8),=C'ADVALPHA'                                         
         MVC   CODEAREA,MYSPACES                                                
         MVC   NAMEAREA(L'RADVNAME),0(R2)                                       
         B     GENOUT                                                           
         SPACE                                                                  
AGOUT    MVI   REREAD,RAGYKTYQ     AGENCY                                       
         MVC   REAGY,0(R2)                                                      
         MVC   REAOF,L'REAGY(R2)                                                
         OC    REAOF,MYSPACES                                                   
         LA    R4,L'REAGY+L'REAOF-1                                             
         LA    R5,L'RAGYNAM1-1                                                  
         XC    NEEDKEY,NEEDKEY                                                  
         MVC   NEEDKEY(L'REAGY),REAGY                                           
         MVC   NEEDKEY+L'REAGY(L'REAOF),REAOF                                   
         B     COMOUT                                                           
         SPACE                                                                  
AGAOUT   DS   0H                                                                
         MVC   LABLAREA(5),=C'SHORT'                                            
         MVC   CODEAREA,MYSPACES                                                
         MVC   NAMEAREA(L'RAGYNAM1),0(R2)                                       
         B     GENOUT                                                           
         SPACE                                                                  
CLOUT    MVI   REREAD,RCLSKTYQ     CLASS                                        
         MVC   RECLS,0(R2)                                                      
         LA    R4,L'RECLS-1                                                     
         LA    R5,L'RCLSNAME-1                                                  
         XC    NEEDKEY,NEEDKEY                                                  
         MVC   NEEDKEY(L'RECLS),RECLS                                           
         B     COMOUT                                                           
         SPACE                                                                  
CAOUT    MVI   REREAD,RCTGKTYQ     CATEGORY                                     
         MVC   RECTG,0(R2)                                                      
         LA    R4,L'RECTG-1                                                     
         LA    R5,L'RCTGNAME-1                                                  
         XC    NEEDKEY,NEEDKEY                                                  
         MVC   NEEDKEY(L'RECTG),RECTG                                           
         B     COMOUT                                                           
         SPACE                                                                  
* PRODUCT - 1ST SEE IF USING PRODUCT NAME OR CODE *                             
         SPACE                                                                  
PROUT    MVI   REREAD,RPRDKTYQ     PRODUCT                                      
         LA    R4,L'REPRD-1                                                     
         LA    R5,L'RPRDNAME-1                                                  
         SPACE                                                                  
* SEE IF THIS IS PRODUCT CODE OR NAME *                                         
         SPACE                                                                  
         OC    L'REPRD(L'REPRDN-L'REPRD,R2),L'REPRD(R2)                         
         BZ    PROUT10                                                          
         CLC   L'REPRD(L'REPRDN-L'REPRD,R2),MYSPACES                            
         BE    PROUT10                                                          
         MVC   REPRDN(20),0(R2)                                                 
         MVC   RENAME,MYSPACES                                                  
         MVC   RENAME(20),0(R2)                                                 
         XC    0(L'REPRDN,R2),0(R2)                                             
         LA    R5,L'REPRDN-1                                                    
         B     COMOUT02                                                         
         SPACE                                                                  
PROUT10  MVC   REPRD,0(R2)                                                      
         XC    NEEDKEY,NEEDKEY                                                  
         MVC   NEEDKEY(L'READV),READV                                           
         MVC   NEEDKEY+L'READV(L'REPRD),REPRD                                   
         B     COMOUT                                                           
         SPACE                                                                  
CTOUT    MVI   REREAD,RCTYKTYQ     CONTRACT TYPE                                
         MVC   RECTY,0(R2)                                                      
         LA    R4,L'RECTY-1                                                     
         LA    R5,L'RCTYDESC-1                                                  
         XC    NEEDKEY,NEEDKEY                                                  
         MVC   NEEDKEY(L'RECTY),RECTY                                           
         B     COMOUT                                                           
         SPACE                                                                  
RKOUT    DS    0H                  STATION RANK                                 
         MVC   RERNK,0(R2)                                                      
         MVC   LABLAREA(4),=C'RANK'                                             
         MVC   CODEAREA(L'RERNK),RERNK                                          
         B     GENBOTH                                                          
         SPACE                                                                  
TVOUT    DS    0H                  STATION TVB                                  
         MVC   RETVB,0(R2)                                                      
         MVC   LABLAREA(3),=C'TVB'                                              
         MVC   CODEAREA(L'RETVB),RETVB                                          
         SPACE                                                                  
         L     R1,REATVBLS                                                      
TVOUT10  CLC   RETVB,0(R1)                                                      
         BE    TVOUT20                                                          
         LA    R1,20(,R1)                                                       
         CLI   0(R1),X'FF'                                                      
         BNE   TVOUT10                                                          
         LA    R1,=CL20'  UNKNOWN'                                              
         SPACE                                                                  
TVOUT20  MVC   NAMEAREA(18),2(R1)                                               
         B     GENBOTH                                                          
         SPACE                                                                  
OWOUT    MVI   REREAD,ROWNKTYQ     STATION OWNER                                
         MVC   REOWN,0(R2)                                                      
         LA    R4,L'REOWN-1                                                     
         LA    R5,L'ROWNNAME-1                                                  
         XC    NEEDKEY,NEEDKEY                                                  
         MVC   NEEDKEY(L'REOWN),REOWN                                           
         B     COMOUT                                                           
         SPACE                                                                  
BOKOUT   CLI   1(R2),0             IS THERE A BOOK                              
         BE    XIT                 NO                                           
         MVC   LABLAREA(4),=C'BOOK'                                             
         CLI   3(R2),X'FF'                  IS IT FROM X'12' ELEM               
         BE    BOKOUT5                      YES                                 
         MVC   CODEAREA(L'REBOOK6),0(R2)    NO/IT'S FROM X'10' ELEM             
         B     GENBOTH                                                          
*                                                                               
BOKOUT5  DS    0H                     IT'S FROM X'12' ELEMENT                   
         MVC   DUB(2),1(R2)           BOOK=SPACE,YEAR,MONTH                     
         MVI   DUB+2,1                                                          
         GOTO1 DATCON,DMCB,(3,DUB),(6,CODEAREA)                                 
         B     GENBOTH                                                          
         SPACE                                                                  
DEMOUT   MVC   LABLAREA(4),=C'DEMO' BOOK                                        
         MVC   WORK(3),0(R2)                                                    
*                                                                               
         OC    DEMOCON,DEMOCON     DO WE HAVE ADDRESS                           
         BNZ   DMO5                                                             
         GOTO1 CALLOV,DMCB,0,X'D9000AE0'  A(DEMOCON)                            
         L     RF,DMCB                                                          
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,DEMOCON                                                       
DMO5     XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'R'                                                    
         MVI   WORK+3,0                                                         
         CLI   WORK+1,C'T'         DEMOCON NEEDS I FOR REP SYSTEM               
         BNE   *+8                                                              
         MVI   WORK+1,C'I'                                                      
         GOTO1 DEMOCON,DMCB,(0,WORK),(2,CODEAREA),(0,DBLOCK),0                  
         B     GENBOTH                                                          
*                                                                               
         EJECT                                                                  
*                                                                               
* - POINT PERSON                                                                
PTPOUT   CLI   0(R2),C'?'          UNKNOWN                                      
         BNE   PTP05                                                            
         MVC   LABLAREA(3),=C'PTP'                                              
         MVC   CODEAREA(3),=C'???'                                              
         MVC   NAMEAREA(7),=C'UNKNOWN'                                          
         B     GENBOTH                                                          
PTP05    MVI   REREAD,RPOPKTYQ                                                  
         MVC   REPTP,0(R2)                                                      
         LA    R4,L'REPTP-1                                                     
         LA    R5,L'RPTPNAME-1                                                  
         XC    NEEDKEY,NEEDKEY                                                  
         MVC   NEEDKEY(L'REPTP),REPTP                                           
         B     COMOUT                                                           
         SPACE                                                                  
DPTOUT   MVC   LABLAREA(3),=C'DPT'          DAYPART                             
         MVC   CODEAREA(L'REDPT),0(R2)                                          
         B     GENBOTH                                                          
         SPACE                                                                  
SRVCOUT  MVC   LABLAREA(5),=C'RSRVC'        RATING SERVICE                      
         MVC   CODEAREA(L'RESRVC),0(R2)                                         
         B     GENBOTH                                                          
         SPACE                                                                  
MKTOUT   MVC   LABLAREA(10),=C'GEO SURVEY'   GEOGRAPHIC SURVEY                  
         MVC   CODEAREA(3),0(R2)            ONLY USING CL3                      
         B     GENBOTH                                                          
         SPACE                                                                  
MCDOUT   MVC   LABLAREA(8),=C'MKT CODE'      MARKET CODE                        
         MVC   CODEAREA(L'REMCD),0(R2)                                          
         B     GENBOTH                                                          
         EJECT                                                                  
* COMMON OUTPUT ROUTINE FOR ALL NAMES *                                         
         SPACE                                                                  
COMOUT   MVC   RENAME,MYSPACES                                                  
         SPACE                                                                  
         CLI   GLARGS,C'C'         CODE ONLY                                    
         BE    COMOUT02             YES, DON'T GET NAME                         
         SPACE                                                                  
         BAS   RE,NEEDNAM                                                       
         SPACE                                                                  
COMOUT02 OC    RENAME,MYSPACES                                                  
         SPACE                                                                  
         CLI   MYLTYP,C'H'         THIS A HEADING                               
         BNE   COMOUT10                                                         
         LA    R1,HDGTBL                                                        
COMOUT04 CLI   0(R1),X'FF'         END OF TABLE                                 
         BE    COMOUT10                                                         
         CLC   REREAD,0(R1)                                                     
         BE    COMOUT06                                                         
         LA    R1,9(,R1)                                                        
         B     COMOUT04                                                         
COMOUT06 MVC   LABLAREA(8),1(R1)                                                
         SPACE                                                                  
COMOUT10 EX    R5,COMMVCA                                                       
         SPACE                                                                  
         EX    R4,COMOC                                                         
         BZ    COMOUTX                                                          
         EX    R4,COMCLC                                                        
         BE    COMOUTX                                                          
         SPACE                                                                  
         CLI   REREAD,RPRDKTYQ     PRODUCT                                      
         BNE   COMOUT20                                                         
         OC    0(L'REPRDN,R2),0(R2)   IS THERE A CODE                           
         BZ    GENBOTH                                                          
         SPACE                                                                  
COMOUT20 CLI   REREAD,RSTAKTYQ     STATION ALREADY FORMATED                     
         BE    GENBOTH                                                          
         SPACE                                                                  
         CLI   REREAD,RGRPKTYQ     GRP ONLY                                     
         BNE   COMOUT24                                                         
         CLI   RESUB,0             SIGNAL THAT THIS IS GRP ONLY                 
         BNE   COMOUT24                                                         
         BCTR  R4,0                                                             
         SPACE                                                                  
COMOUT24 LA    R1,CODEAREA                                                      
         SPACE                                                                  
         MVI   0(R1),C'('                                                       
         EX    R4,COMMVCB                                                       
         LA    R1,1(R1,R4)                                                      
COMOUT26 CLI   0(R1),C' '                                                       
         BH    COMOUT30                                                         
         BCTR  R1,0                                                             
         BCT   R4,COMOUT26                                                      
         SPACE                                                                  
COMOUT30 MVI   1(R1),C')'                                                       
         SPACE                                                                  
COMOUTX  B     GENBOTH                                                          
         SPACE                                                                  
COMMVCA  MVC   NAMEAREA(0),RENAME                                               
COMMVCB  MVC   1(0,R1),0(R2)                                                    
COMOC    OC    0(0,R2),0(R2)                                                    
COMCLC   CLC   0(0,R2),MYSPACES                                                 
         EJECT                                                                  
WKHDOT   DS    0H                  WEEKLY ACTIVITY CT HEADLINE                  
         SPACE                                                                  
         MVC   WORK(6),REQASTR                                                  
         ICM   R0,1,GLARGS                                                      
         BCTR  R0,0                                                             
         LTR   R0,R0                                                            
         BZ    WKHDOT10                                                         
         MH    R0,=H'7'                                                         
         GOTO1 ADDAY,DMCB,REQASTR,WORK,(R0)                                     
         SPACE                                                                  
WKHDOT10 GOTO1 DATCON,DMCB,WORK,(4,387(R3))                                     
         MVI   392(R3),C'-'                                                     
         GOTO1 ADDAY,(R1),WORK,WORK,F'6'                                        
         GOTO1 DATCON,(R1),WORK,(4,588(R3))                                     
         B     XIT                                                              
         SPACE 3                                                                
MOHDOT   DS    0H                  MONTHLY ACTIVITY CT HEADLINE                 
         ICM   R0,1,GLARGS                                                      
         MVC   WORK(6),REQASTR                                                  
MOHDOT10 GOTO1 QGTBROAD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                       
         SPACE                                                                  
         CLI   DMCB,X'FF'          ERROR                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,DMCB                                                          
         AR    R2,RE                                                            
         GOTO1 ADDAY,(R1),WORK+12,WORK,F'1'                                     
         BCT   R0,MOHDOT10                                                      
         SPACE                                                                  
MOHDOT20 GOTO1 DATCON,DMCB,WORK+6,(4,387(R3))                                   
         MVI   392(R3),C'-'                                                     
         GOTO1 (RF),(R1),WORK+12,(4,588(R3))                                    
         B     XIT                                                              
         SPACE 3                                                                
QTHDOT   DS    0H                  QUARTERLY ACTIVITY CT HEADLINE               
         ICM   R0,1,GLARGS                                                      
         MVC   WORK(6),REQASTR                                                  
         SPACE                                                                  
QTHDOT10 LA    R2,3                3 MONTHS PER QTR                             
         MVC   WORK+20(6),WORK                                                  
         SPACE                                                                  
QTHDOT14 GOTO1 QGTBROAD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                       
         SPACE                                                                  
         CLI   DMCB,X'FF'          ERROR                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   WORK+26(6),WORK+12                                               
         GOTO1 ADDAY,(R1),WORK+12,WORK,F'1'                                     
         BCT   R2,QTHDOT14                                                      
         SPACE                                                                  
         BCT   R0,QTHDOT10                                                      
         SPACE                                                                  
         MVC   WORK+6(12),WORK+20                                               
         B     MOHDOT20                                                         
FILTIN   DC    H'0'                                                             
FILTOUT  DC    H'0'                                                             
         DC    H'0'                                                             
         SPACE 3                                                                
OUTNAME  MVC   LABLAREA(5),=C'SHORT'                                            
         MVC   NAMEAREA,0(R2)                                                   
         B     GENOUT                                                           
         SPACE                                                                  
INADD    MVI   ELCODE,01           ADDRESSES                                    
         BAS   RE,GETEL                                                         
         MVC   0(120,R2),MYSPACES                                               
         BNE   XIT                                                              
*        USING TAADD,R4                                                         
*        ZIC   R1,TAADLNES                                                      
         MH    R1,=H'30'                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     XIT                                                              
*        MVC   0(0,R2),TAADADD                                                  
         SPACE                                                                  
INSHORT  DS    0H                  SHORT NAME                                   
*        MVI   ELCODE,TASNELQ      SHORT NAME                                   
         BAS   RE,GETEL                                                         
         MVC   0(16,R2),MYSPACES                                                
         BNE   XIT                                                              
*        USING TASND,R4                                                         
*        ZIC   R1,TASNLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     XIT                                                              
*        MVC   0(0,R2),TASNAME                                                  
         SPACE                                                                  
OUTSHORT MVC   LABLAREA(5),=C'SHORT'                                            
         MVC   NAMEAREA(16),0(R2)                                               
         B     GENOUT                                                           
         EJECT                                                                  
*              GENERAL INPUT HANDLER                                            
         SPACE 3                                                                
*              INPUT               R2=A(WHERE DATA IS TO GO)                    
*                                  R3=L'INPUT                                   
*                                  R1=A(ELEMENT DISPLACEMENT TABLE)             
*                                  ELCODE SET                                   
*                                  R4=A(RECORD)                                 
         SPACE                                                                  
GENIN    OC    ATHISEL,ATHISEL     IF AN ELEMENT IS BEING PASSED                
         BZ    GENIN1                                                           
         CLI   MYITYPE+1,C'+'         AND FIELD IS ADDITIVE                     
         BNE   GENIN1                                                           
         L     R4,ATHISEL          ONLY PROCESS IF WE ARE NOW                   
         CLC   0(1,R4),ELCODE      ADDRESSING THE REQUIRED ELEMENT              
         BNE   XIT                                                              
         B     GENIN4                                                           
         SPACE                                                                  
GENIN1   BAS   RE,GETEL                                                         
         B     *+8                                                              
         SPACE                                                                  
GENIN2   BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         CLI   ELTYPE,0            IF ELTYPE IS SET                             
         BE    GENIN4                                                           
         CLC   ELTYPE,2(R4)           CHECK AGAINST FIRST DATA BYTE             
         BNE   GENIN2                                                           
         SPACE                                                                  
GENIN4   ZIC   RF,GLARGS           USE ARG1 TO GET TO DATA ELEMENT              
         BCTR  RF,0                                                             
         SLL   RF,1                                                             
         AR    R1,RF                                                            
         ZIC   RE,0(R1)            RE=DISP INTO ELEMENT                         
         AR    RE,R4                                                            
         ST    RE,AGINDATA                                                      
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RE)       MOVE OUT BASIC DATA                          
         CLI   1(R1),0             SECOND BYTE IS ADDITIONAL ROUTINE#           
         BE    XIT                                                              
         SPACE                                                                  
         L     RE,=A(ADDROUT)      ADDITIONAL ROUTINE NEEDED                    
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),MYSPACES    SO CLEAR TO SPACES                           
         SPACE                                                                  
GENIN12  CLC   0(1,RE),ELCODE      FIRST BYTE IS ELCODE                         
         BNE   GENIN14                                                          
         CLC   1(1,RE),1(R1)       SECOND BYTE IS ROUTINE NUMBER                
         BE    GENIN16                                                          
         SPACE                                                                  
GENIN14  LA    RE,8(RE)                                                         
         CLI   0(RE),X'FF'                                                      
         BE    XIT                                                              
         B     GENIN12                                                          
         SPACE                                                                  
GENIN16  L     RF,4(RE)            PICK UP EXTRA ROUTINE                        
         L     R1,AGINDATA         AND ADDRESS OF DATA                          
         BR    RF                                                               
         EJECT                                                                  
*              DATE ROUTINES                                                    
         SPACE 3                                                                
SETDATE  NTR1                                                                   
         MVC   WORK(3),RECADT                                                   
         B     XIT                                                              
         SPACE                                                                  
DTE2OUT  DS    0H                  COMPRESSED DATE                              
         GOTO1 DATCON,DMCB,(2,(R2)),(8,(R3))                                    
         B     XIT                                                              
         SPACE                                                                  
ALLDOUT  OC    0(3,R2),0(R2)                                                    
         BZ    XIT                                                              
         GOTO1 DATCON,DMCB,(1,0(R2)),(8,CODEAREA)                               
         B     GENOUT                                                           
         EJECT                                                                  
*              COMPUTE ROUTINES                                                 
         SPACE 3                                                                
ICOMPUTE ZAP   0(8,R2),=P'0'       ZERO OUT                                     
         B     XIT                                                              
         SPACE                                                                  
OCOMPUTE L     R1,GLATHID                                                       
         USING GLINTD,R1                                                        
         CLC   GLLEVEL,GLDETLEV                                                 
         BE    OCOMPOK                 ONLY PRINT WHEN DETAIL                   
         TM    GLARGS+11,X'80'     NO TOTAL TO PRINT                            
         BO    XIT                                                              
OCOMPOK  MVI   GLHOOK,GLEDIT       COMPUTE                                      
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*              DATE ROUTINES                                                    
         SPACE 3                                                                
MONIN    BAS   RE,SETDATE          MONTH                                        
         MVC   0(2,R2),WORK                                                     
         CLI   GLARGS+1,2          ARG2:2=MONTH ONLY                            
         BL    XIT                                                              
         MVC   0(1,R2),WORK+1                                                   
         B     XIT                                                              
         SPACE                                                                  
MONOUT   MVC   LABLAREA(5),=C'MONTH'                                            
         CLI   GLARGS,2                                                         
         BNE   MONOUT2             OPTION JUST TO DO MONTH                      
         MVI   DUB,1                                                            
         MVC   DUB+1(1),0(R2)                                                   
         MVI   DUB+2,1                                                          
         GOTO1 DATCON,DMCB,(1,DUB),(9,WORK)                                     
         MVC   CODEAREA(3),WORK                                                 
         B     GENOUT                                                           
         SPACE                                                                  
MONOUT2  MVC   DUB(2),0(R2)                                                     
         MVI   DUB+2,1                                                          
         GOTO1 DATCON,DMCB,(1,DUB),(9,WORK)                                     
         CLI   WORK+3,C'/'                                                      
         BNE   *+10                                                             
         MVC   WORK+3(2),WORK+4                                                 
         MVC   CODEAREA(5),WORK                                                 
         B     GENOUT                                                           
         EJECT                                                                  
*              DATE ROUTINES - QUARTER, CYCLES AND YEARS                        
         SPACE 3                                                                
QUARTIN  BAS   RE,SETDATE                                                       
         MVC   0(1,R2),WORK                                                     
         ZIC   R1,WORK+1           GET QUARTER FROM MONTH                       
         CH    R1,=H'10'           CONVERT PWOS 10-12                           
         BL    *+8                                                              
         SH    R1,=H'6'            X'10' BECOMES X'0A'                          
         BCTR  R1,0                                                             
         SR    R0,R0                                                            
         D     R0,=F'3'                                                         
         STC   R1,1(R2)                                                         
         B     XIT                                                              
         SPACE                                                                  
QUARTOUT MVC   LABLAREA(7),=C'QUARTER'                                          
         MVC   CODEAREA,MYSPACES                                                
         UNPK  WORK(3),0(2,R2)                                                  
         MVC   CODEAREA+5(2),WORK                                               
         ZIC   R1,1(R2)                                                         
         SLL   R1,2                                                             
         LA    R1,QUARTLST(R1)                                                  
         MVC   CODEAREA(4),0(R1)                                                
         B     GENOUT                                                           
         SPACE                                                                  
QUARTLST DC    C'1ST.2ND.3RD.4TH.   '                                           
         SPACE                                                                  
         EJECT                                                                  
*              TESTING GLARGS - RETURNING OR ON TO GENOUT BELOW                 
         SPACE 3                                                                
*              ARGUMENT 1          N=NAME                                       
*                                  C=CODE                                       
*                                  B=BOTH                                       
         SPACE                                                                  
GENBOTH  CLI   GLARGS,C'N'         NAME ONLY?                                   
         BNE   GENBOTH2                                                         
         MVC   CODEAREA,MYSPACES   CLEAR OUT CODE                               
         B     GENOUT                                                           
         SPACE                                                                  
GENBOTH2 CLI   GLARGS,C'C'         CODE ONLY?                                   
         BNE   GENOUT               MUST BE BOTH                                
         SPACE                                                                  
         MVC   NAMEAREA,MYSPACES   AND MOVE OUT NAME                            
         EJECT                                                                  
*              SHARED OUTPUT ROUTINE                                            
         SPACE 3                                                                
*              AT THIS STAGE...    LABLAREA HAS PREFIX                          
*                                  CODEAREA HAS CODE                            
*                                  NAMEAREA HAS NAME                            
         SPACE                                                                  
GENOUT   TM    GLINDS,X'40'        TOTALS ARE DEALT WITH BELOW                  
         BO    TOTOUT                                                           
         CLI   MYLTYP,C'H'         FOR HEADLINES, MOVE OUT THE LOT              
         BNE   GENOUT2                                                          
         CLI   MYCOL,99            IF ON THE LEFT HAND SIDE                     
         BL    GENOUT1                                                          
         GOTO1 SQUASHER,DMCB,OUTAREA,68                                         
         MVC   0(33,R3),OUTAREA    ELSE SQUASHER AND SHOW SOME                  
         B     XIT                                                              
         SPACE                                                                  
GENOUT1  MVC   0(L'OUTAREA,R3),OUTAREA                                          
         B     XIT                                                              
         SPACE                                                                  
GENOUT2  CLI   MYLTYP,C'M'         FOR MIDLINES, SQUASH FIRST                   
         BNE   GENOUT4                                                          
         GOTO1 SQUASHER,DMCB,CODENNAM,52                                        
         MVC   0(L'CODENNAM,R3),CODENNAM                                        
         B     XIT                                                              
         SPACE                                                                  
GENOUT4  SR    R4,R4               ANY CODE TO OUTPUT                           
         CLC   CODEAREA,MYSPACES                                                
         BE    GENOUT10                                                         
         LA    R4,12                                                            
         LA    R1,CODEAREA+11                                                   
         SPACE                                                                  
GENOUT6  CLI   0(R1),C' '          FIGURE OUT L'CODE                            
         BH    GENOUT8                                                          
         BCTR  R1,0                                                             
         BCT   R4,GENOUT6                                                       
         SPACE                                                                  
GENOUT8  BCTR  R4,0                (R4=L'CODE-1)                                
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),CODEAREA    MOVE OUT THE CODE                            
         LA    R4,2(R4)            (NOW HAS LENGTH+1)                           
         AR    R3,R4                                                            
         SPACE                                                                  
GENOUT10 LA    R1,NAMEAREA         SET UP TO CHOP THE NAME                      
         ST    R1,DMCB             A(INPUT)                                     
         MVI   DMCB,36             L'INPUT                                      
         ST    R3,DMCB+4           A(OUTPUT)                                    
         ZIC   R1,MYOLEN                                                        
         SR    R1,R4                                                            
         BNP   XIT                                                              
         STC   R1,DMCB+4                                                        
         LA    R1,4                MAX N'LINES                                  
         TM    GLDOWNLD,X'80'                                                   
         BNO   *+8                                                              
         LA    R1,1                ONLY 1 FOR DOWNLOADING                       
         ST    R1,DMCB+8                                                        
         MVI   DMCB+8,198          PRINT LINES ARE 198 APART                    
         GOTO1 CHOPPER,DMCB                                                     
         B     XIT                                                              
         EJECT                                                                  
*              SHARED TOTAL ROUTINE                                             
         SPACE 3                                                                
*              AT THIS STAGE...    LABLAREA HAS PREFIX                          
*                                  CODEAREA HAS CODE                            
*                                  NAMEAREA HAS NAME                            
*              INPUT               ROW1WIDE ROWWIDTH                            
         SPACE                                                                  
TOTOUT   ZIC   R1,GLRECNO          PICK UP PRESENT RECORD NUMBER                
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LA    R1,GLAINTD(R1)      GET TO DETAILS FOR THIS REPORT               
         L     R1,0(R1)                                                         
         USING GLINTD,R1                                                        
         LH    R4,GLPDISP          NOW PICK UP PRINT DISPLACEMENT               
         A     R4,GLAINTP1         AND GET ACTUAL PRINT ADDRESS                 
         LA    R4,1(R4)                                                         
         DROP  R1                                                               
         SPACE                                                                  
         ZIC   R1,ROWWIDTH                                                      
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         EX    R1,CLEARP1                                                       
         EX    R1,CLEARP2                                                       
         EX    R1,CLEARP3                                                       
         CLI   ROWWIDTH,16         IF THERE IS ENOUGH ROOM                      
         BL    TOTOUT2                                                          
         MVC   BLOCK(80),MYSPACES                                               
         MVC   BLOCK(11),=C'*TOTALS FOR'                                        
         MVC   BLOCK+12(65),OUTAREA                                             
         GOTO1 SQUASHER,DMCB,BLOCK,80                                           
         LA    R1,4                                                             
         ST    R1,DMCB+8                                                        
         MVI   DMCB+8,198                                                       
         ZIC   R3,ROWWIDTH                                                      
         BCTR  R3,0                                                             
         GOTO1 CHOPPER,DMCB,(80,BLOCK),((R3),0(R4))                             
         B     TOTOUTX                                                          
         SPACE                                                                  
CLEARP1  MVC   000(0,R4),MYSPACES                                               
CLEARP2  MVC   198(0,R4),MYSPACES                                               
CLEARP3  MVC   396(0,R4),MYSPACES                                               
         SPACE                                                                  
TOTOUT2  L     R1,GLATOUT          PICK UP A(OUT ELEMENT)                       
         USING DROD,R1                                                          
         ZIC   R4,DROLEN           AND USE THE OUT WIDTH                        
         CLI   DROLTYP,C'P'        IF FIELD IS IN THE PRINT LINE                
         BE    TOTOUT4                                                          
         DROP  R1                                                               
         ZIC   R4,ROW1WIDE                                                      
         BCTR  R4,0                                                             
         SPACE                                                                  
TOTOUT4  CH    R4,=H'3'                                                         
         BL    TOTOUTX                                                          
         MVC   0(3,R3),=C'ALL'                                                  
         CH    R4,=H'5'                                                         
         BL    TOTOUTX                                                          
         MVC   0(5,R3),=C'*ALL*'                                                
         CH    R4,=H'8'                                                         
         BL    TOTOUTX                                                          
         MVC   0(8,R3),=C'*TOTALS*'                                             
         SPACE                                                                  
TOTOUTX  MVC   OUTAREA,MYSPACES                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO FILTER COLUMNS                                       
         SPACE 3                                                                
*              GLARGS              7-9  UP TO 3 FILTER SLOT NUMBERS             
*                                  EACH SLOT NUMBER REFERS TO A 16              
*                                  BYTE ENTRY, ADDRESSED BY ACOLFILT            
         SPACE                                                                  
*                                  EACH ENTRY IS....                            
*                                     1. DISPLACEMENT INTO RECODES              
*                                     2. LENGTH OF CODE                         
*                                     3. SPARE                                  
*                                     4. - FOR NEGATIVE FILTERS                 
*                                  5-16. FILTER EXPRESSION                      
         SPACE                                                                  
*                                  10    DATE TYPE                              
*                                  11-13 START DATE                             
*                                  14-16 END DATE                               
         SPACE                                                                  
COLFILT  NTR1                                                                   
         LA    RE,GLARGS+6                                                      
         LA    R0,3                                                             
         SPACE                                                                  
CFILTNXT CLI   0(RE),0                                                          
         BE    CFILT8                                                           
         ZIC   R1,0(RE)            (SLOT NUMBER)                                
         BCTR  R1,0                                                             
         SLL   R1,4                (16 BYTES PER SLOT ENTRY)                    
         A     R1,ACOLFILT         DISPLACE INTO COLUMN FILTER BUFFER           
         ZIC   R3,0(R1)            (DISPLACEMENT INTO RECODES)                  
         LA    R3,RECODES(R3)      NOW POINTS TO SPECIFIC CODE                  
         LA    R4,4(R1)            R4=A(FILTER EXPRESSION)                      
         ZIC   R5,1(R1)            R5=LENGTH OF CODE                            
         SPACE                                                                  
CFILT2   CLI   0(R4),X'41'         MUST BE SIGNIFICANT                          
         BL    CFILT4                                                           
         CLI   0(R4),C'*'          * IS WILD-CARD                               
         BE    CFILT4                                                           
         CLC   0(1,R4),0(R3)       ELSE MATCH ON THIS                           
         BNE   CFILTNO                                                          
         SPACE                                                                  
CFILT4   LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,CFILT2                                                        
         SPACE                                                                  
         CLI   3(R1),C'-'          PASSED FILTER                                
         BNE   CFILT6                     UNLESS IT WAS NEGATIVE                
         B     NOGOOD                                                           
         SPACE                                                                  
CFILTNO  CLI   3(R1),C'-'          FAILED FILTER                                
         BE    CFILT6                     OK IF IT WAS NEGATIVE                 
         B     NOGOOD                                                           
         SPACE                                                                  
CFILT6   LA    RE,1(RE)            ON TO THE NEXT ARGUMENT!                     
         BCT   R0,CFILTNXT                                                      
         EJECT                                                                  
*              DATE FILTERS                                                     
         SPACE 3                                                                
*                                        ARGS 10 IS DATE TYPE                   
CFILT8   OC    GLARGS+10(6),GLARGS+10    ARGS 11-16 ARE DATES                   
         BZ    ITSFINE                                                          
         IC    R0,GLARGS           (BORROW GLARGS FOR SETDATE)                  
         MVC   GLARGS(1),GLARGS+9  (PASSING DATE TYPE TO SETDATE)               
         BAS   RE,SETDATE          (WORK NOW HAS RELEVANT YMD PWOS)             
         STC   R0,GLARGS                                                        
         SPACE                                                                  
         LA    R1,1                ARE WE FILTERING ON YM                       
         CLI   GLARGS+12,0                                                      
         BNE   *+8                                                              
         LA    R1,2                                 OR YMD                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),GLARGS+10                                                
         BL    NOGOOD             MUST NOT BE BEFORE START                      
         BE    CFILT10                                                          
         OC    GLARGS+13(3),GLARGS+13  AFTER START OK IF END DATE               
         BZ    NOGOOD                                    IN FILTER              
         SPACE                                                                  
CFILT10  OC    GLARGS+13(3),GLARGS+13  NO END DATE?                             
         BZ    ITSFINE                                                          
         LA    R1,1               IS END DATE YM                                
         CLI   GLARGS+15,0                                                      
         BNE   *+8                                                              
         LA    R1,2                        OR YMD                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),GLARGS+13                                                
         BH    NOGOOD              CAN'T BE AFTER END DATE                      
         B     ITSFINE                                                          
         EJECT                                                                  
*              PERIOD IN HEADINGS                                               
         SPACE 3                                                                
*              ARGUMENTS           1   DATE TYPE                                
*                                  2-4 START YMD (PWOS)                         
*                                  5-7 END YMD (PWOS)                           
         SPACE                                                                  
PERPOP   MVC   WORK,MYSPACES        PERIOD TO HEADLINES                         
         GOTO1 DATCON,DMCB,(1,GLARGS+1),(8,WORK)                                
         LA    R2,WORK+3                                                        
         CLC   0(2,R2),=C'00'                                                   
         BE    *+8                                                              
         LA    R2,2(R2)                                                         
         MVC   0(3,R2),MYSPACES                                                 
         SPACE                                                                  
*                                   NOW HAVE MMM OR MMMDD                       
         OC    GLARGS+4(3),GLARGS+4 ANY END DATE?                               
         BZ    PERPOP2                                                          
         MVI   0(R2),C'-'                                                       
         GOTO1 DATCON,DMCB,(1,GLARGS+4),(8,1(R2))                               
         LA    R2,3(R2)                                                         
         CLC   0(2,R2),=C'00'                                                   
         BE    *+8                                                              
         LA    R2,2(R2)                                                         
         MVC   0(3,R2),MYSPACES     NOW HAVE MMM-MMM OR MMMDD-MMMDD             
         SPACE                                                                  
PERPOP2  L     R4,GLADTENT                                                      
         USING DRHDD,R4                                                         
         ZIC   R2,DRHDWDTH                                                      
         GOTO1 CHOPPER,DMCB,(11,WORK),((R2),(R3)),(198,2)                       
         GOTO1 CENTER,DMCB,(R3),(R2)                                            
         B     XIT                                                              
         EJECT                                                                  
* SEE IF REQUESTED RECORD IS IN SOME TABLE *                                    
         SPACE                                                                  
NEEDNAM  NTR1                                                                   
         L     R2,=A(NEEDTAB)                                                   
         SPACE                                                                  
NREC10   CLI   0(R2),X'FF'         NO TABLE FOR THIS REQUEST                    
         BE    NREC16                                                           
         CLC   0(1,R2),REREAD      MATCH ON FIRST BYTE OF KEY                   
         BE    NREC20                                                           
         LA    R2,4(R2)                                                         
         B     NREC10                                                           
         SPACE                                                                  
NREC16   MVI   NEEDHIT,C'N'        SET NO SAVE FOR THIS TYPE                    
         B     NREC36                                                           
         SPACE                                                                  
NREC20   CLI   REREAD,RPRDKTYQ     FOR PRODUCT CODE, DIFF LEN                   
         BNE   NREC24                                                           
         LA    R4,L'READV+L'REPRD-1                                             
         SPACE                                                                  
NREC24   L     R2,0(,R2)           NOW R2 HAS A(BUFFER)                         
*                                  BYTES  1-4 N'ENTRIES                         
*                                  BYTES  5-8 L'ENTRY                           
*                                  BYTES  9-12 NUMBER OF LAST ENTRY             
         MVI   NEEDHIT,C'Y'                                                     
         LA    RF,2(R4,R5)         GET ENTRY LENGTH                             
         C     RF,4(,R2)           THIS SAME ENTRY LEN AS REQUEST               
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         BCTR  RF,0                                                             
         LA    R3,12(,R2)          BYTES 13+  THE BUFFER!                       
         L     R0,0(,R2)                                                        
         SPACE                                                                  
NREC30   EX    R4,NRECCLC          IS MY RECORD IN THE BUFFER?                  
         BE    NREC50                                                           
         A     R3,4(R2)                                                         
         BCT   R0,NREC30                                                        
         SPACE                                                                  
NREC36   GOTO1 REWRIIO,DMCB,REWRIIOD                                            
         SPACE                                                                  
         CLI   REERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   NEEDHIT,C'Y'        IS THERE A BUFFER FOR THIS?                  
         BNE   NREC38                                                           
         SPACE                                                                  
         L     R1,8(,R2)           NO - PICK UP N'LAST ENTRY                    
         LA    R1,1(,R1)                ROUND ROBIN                             
         C     R1,0(,R2)           HAVE WE GOT TO THE END OF BUFFER?            
         BNH   *+8                                                              
         LA    R1,1                YES, SO GO BACK TO THE BEGINNING             
         ST    R1,8(,R2)                                                        
         BCTR  R1,0                                                             
         M     R0,4(R2)            DISPLACE INTO THE BUFFER                     
         LA    R3,12(R1,R2)                                                     
         EX    R4,NRECMVCA         SAVE KEY INFO                                
         LA    R3,1(R3,R4)                                                      
         EX    R5,NRECMVCB          AND NAME                                    
NREC38   CLI   REREAD,RAGYKTYQ     AGENCY ALPHA                                 
         BE    NREC39                                                           
         CLI   REREAD,RADVKTYQ     ADVERTISER ALPHA                             
         BNE   NREC40                                                           
NREC39   DS    0H                                                               
         OC    NEEDSVKY,NEEDSVKY   IS SYSIO READING RECORDS                     
         BZ    NREC40                                                           
         MVC   KEY,NEEDSVKY                                                     
         GOTO1 HIGH                REREAD TO ESTABLISH SEQUENCE                 
         SPACE                                                                  
         MVC   REREAD,NEEDSVRD                                                  
         MVC   REKEY,NEEDSVKY                                                   
         SPACE                                                                  
NREC40   DS    0H                                                               
         B     ITSFINE                                                          
         SPACE                                                                  
NREC50   LA    R3,1(R3,R4)                                                      
         EX    R5,NRECMVCC         SAVE NAME FROM TABLE                         
         B     ITSFINE                                                          
         SPACE                                                                  
NRECCLC  CLC   NEEDKEY(0),0(R3)    IS MY RECORD IN THE BUFFER?                  
NRECMVCA MVC   0(0,R3),NEEDKEY                                                  
NRECMVCB MVC   0(0,R3),RENAME                                                   
NRECMVCC MVC   RENAME(0),0(R3)                                                  
         SPACE                                                                  
NEEDKEY  DC    XL27'00'                                                         
NEEDNAME DS    CL36                                                             
NEEDHIT  DS    CL1                                                              
NEEDSVKY DC    XL32'00'                                                         
NEEDSVRD DC    X'00'                                                            
         DS    0H                                                               
         EJECT                                                                  
*              ODDMENTS                                                         
         SPACE 3                                                                
         SPACE                                                                  
ITSFINE  SR    R1,R1                                                            
         B     *+8                                                              
         SPACE                                                                  
NOGOOD   LA    R1,1                                                             
         LTR   R1,R1                                                            
         SPACE                                                                  
XIT      XIT1                                                                   
         SPACE                                                                  
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
*              LTORG ETC                                                        
         SPACE 3                                                                
MYSPACES DC    CL198' '                                                         
MYMONTHS DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
AGINDATA DS    A                                                                
ELTYPE   DC    X'00'                                                            
SAVEEL   DC    X'00'                                                            
QTRTABL  DC    16XL6'00'           TABLE OF QTR STR DTS LAST/NEXT 2 YRS         
         SPACE                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE ADDRESS LOOK-UP TABLE                                    
         SPACE 3                                                                
ROUTLIST DS    0F                                                               
         DC    C'OUTNAME ',A(OUTNAME)     GENERAL                               
         DC    C'INADD   ',A(INADD)                                             
         DC    C'INSHORT ',A(INSHORT)                                           
         DC    C'OUTSHORT',A(OUTSHORT)                                          
         SPACE                                                                  
         DC    C'RPIN    ',A(RPIN)        REP                                   
         DC    C'RPOUT   ',A(RPOUT)                                             
RRGIN    DC    C'RGIN    ',A(RGIN)        REGION                                
         DC    C'RGOUT   ',A(RGOUT)                                             
         DC    C'OFIN    ',A(OFIN)        OFFICE                                
         DC    C'OFOUT   ',A(OFOUT)                                             
         DC    C'GSIN    ',A(GSIN)        GROUP/SUB                             
         DC    C'GSOUT   ',A(GSOUT)                                             
         DC    C'GPIN    ',A(GPIN)        GROUP                                 
         DC    C'GPOUT   ',A(GPOUT)                                             
         DC    C'STIN    ',A(STIN)        STATION                               
         DC    C'STOUT   ',A(STOUT)                                             
         DC    C'SLIN    ',A(SLIN)        SALESPERSON                           
         DC    C'SLOUT   ',A(SLOUT)                                             
         DC    C'DTIN    ',A(DTIN)        DIV + TEAM                            
         DC    C'DTOUT   ',A(DTOUT)                                             
         DC    C'ADAIN   ',A(ADAIN)       ADVERTISER ALPHA NAME                 
         DC    C'ADAOUT  ',A(ADAOUT)                                            
         DC    C'ADIN    ',A(ADIN)        ADVERTISER                            
         DC    C'ADOUT   ',A(ADOUT)                                             
         DC    C'AGAIN   ',A(AGAIN)       AGENCY ALPHA NAME                     
         DC    C'AGAOUT  ',A(AGAOUT)                                            
         DC    C'AGIN    ',A(AGIN)        AGENCY CODE                           
         DC    C'AGOUT   ',A(AGOUT)                                             
RCLIN    DC    C'CLIN    ',A(CLIN)        CLASS                                 
         DC    C'CLOUT   ',A(CLOUT)                                             
         DC    C'CAIN    ',A(CAIN)        CATEGORY                              
         DC    C'CAOUT   ',A(CAOUT)                                             
         DC    C'PRIN    ',A(PRIN)        PRODUCT                               
         DC    C'PROUT   ',A(PROUT)                                             
         DC    C'CTIN    ',A(CTIN)        CONTRACT TYPE                         
         DC    C'CTOUT   ',A(CTOUT)                                             
RRKIN    DC    C'RKIN    ',A(RKIN)        STATION RANK                          
         DC    C'RKOUT   ',A(RKOUT)                                             
RTVIN    DC    C'TVIN    ',A(TVIN)        STATION TVB                           
         DC    C'TVOUT   ',A(TVOUT)                                             
         DC    C'BKIN    ',A(RKIN)        BOOK                                  
         DC    C'BKOUT   ',A(RKOUT)                                             
ROWIN    DC    C'OWIN    ',A(OWIN)        STATION OWNER                         
         DC    C'OWOUT   ',A(OWOUT)                                             
         DC    C'DEMIN   ',A(DEMIN)       DEMOS                                 
         DC    C'DEMOUT  ',A(DEMOUT)                                            
         DC    C'BOKIN   ',A(BOKIN)       BOOKS                                 
         DC    C'BOKOUT  ',A(BOKOUT)                                            
RPPIN    DC    C'PTPIN   ',A(PTPIN)       POINT PERSON                          
         DC    C'PTPOUT  ',A(PTPOUT)                                            
         DC    C'DPTIN   ',A(DPTIN)       DAYPART                               
         DC    C'DPTOUT  ',A(DPTOUT)                                            
         DC    C'SRVCIN  ',A(SRVCIN)      RATING SERVIC                         
         DC    C'SRVCOUT ',A(SRVCOUT)                                           
         DC    C'MKTIN   ',A(MKTIN)       GEOGRAPHIC SURVEY                     
         DC    C'MKTOUT  ',A(MKTOUT)                                            
RMCDIN   DC    C'MCDIN   ',A(MCDIN)       MARKET CODE                           
         DC    C'MCDOUT  ',A(MCDOUT)                                            
         SPACE                                                                  
         DC    C'FILTIN  ',A(FILTIN)      OTHER SYSIO CODES                     
         DC    C'FILTOUT ',A(FILTOUT)                                           
         SPACE                                                                  
         DC    C'ICOMPUTE',A(ICOMPUTE)    COMPUTE RTNS                          
         DC    C'OCOMPUTE',A(OCOMPUTE)                                          
         SPACE                                                                  
         DC    C'MONIN   ',A(MONIN)    DATE RELATED                             
         DC    C'MONOUT  ',A(MONOUT)                                            
         DC    C'QUARTIN ',A(QUARTIN)                                           
         DC    C'QUARTOUT',A(QUARTOUT)                                          
         SPACE                                                                  
PWCTYP1  DC    C'SAWKIN  ',A(SAWKIN)   MISCELLANEOUS                            
PWCTYP2  DC    C'SAMNIN  ',A(SAMNIN)                                            
PWCTYP3  DC    C'SAQTIN  ',A(SAQTIN)                                            
         DC    C'CAWKIN  ',A(CAWKIN)                                            
         DC    C'CAMNIN  ',A(CAMNIN)                                            
         DC    C'CAQTIN  ',A(CAQTIN)                                            
         DC    C'WKHDOT  ',A(WKHDOT)  WEEKLY HEADINGS                           
         DC    C'MOHDOT  ',A(MOHDOT)  MONTHLY                                   
         DC    C'QTHDOT  ',A(QTHDOT)  QUARTERLY                                 
         SPACE                                                                  
         DC    C'TCT     ',A(TCT)  TOTAL CONTRACTS IN REQ PERIOD                
         DC    C'DCT     ',A(DCT)  TOTAL DARE CONTRACTS IN REQ PERIOD           
         DC    C'DXT     ',A(DXT)  TOTAL DARE + NON DARE CONTRACTS              
PWCACT   DC    C'ACT     ',A(ACT)  PWC ACTIVE CT                                
PWCCCT   DC    C'CCT     ',A(CCT)  PWC ACTIVE CONTRACT CT                       
PWCSCT   DC    C'SCT     ',A(SCT)  PWC ACTIVE SALES CT                          
         SPACE                                                                  
PWCTACT  DC    C'AATOT   ',A(AATOT)  ALL PWC ACTIVITY CTS FOR PERIOD            
PWCTCCT  DC    C'CATOT   ',A(CATOT)  ALL PWC ACTIVITY CONTRACT CTS              
PWCTSCT  DC    C'SATOT   ',A(SATOT)  ALL PWC ACTIVITY SALES CTS                 
         SPACE                                                                  
         DC    X'FF'                                                            
         EJECT                                                                  
* DESCRIPTORS FOR HEADINGS FOR DATA TYPES                                       
         SPACE                                                                  
HDGTBL   DC    AL1(RREPKTYQ),CL8'REP    '                                       
         DC    AL1(RREGKTYQ),CL8'REGION '                                       
         DC    AL1(ROFFKTYQ),CL8'OFFICE '                                       
         DC    AL1(RGRPKTYQ),CL8'GROUP  '                                       
         DC    AL1(RSTAKTYQ),CL8'STATION'                                       
         DC    AL1(RSALKTYQ),CL8'SAL    '                                       
         DC    AL1(RTEMKTYQ),CL8'DIV    '                                       
         DC    AL1(RADVKTYQ),CL8'ADV    '                                       
         DC    AL1(RAGYKTYQ),CL8'AGY    '                                       
         DC    AL1(RCLSKTYQ),CL8'CLA    '                                       
         DC    AL1(RCTGKTYQ),CL8'CAT    '                                       
         DC    AL1(RPRDKTYQ),CL8'PRO    '                                       
         DC    AL1(RCTYKTYQ),CL8'CONTYPE'                                       
         DC    AL1(ROWNKTYQ),CL8'OWN    '                                       
         DC    AL1(RPOPKTYQ),CL8'PTP    '                                       
         DC    X'FF'                                                            
         SPACE                                                                  
* FOLLOWING TABLES ARE BUILT IN REWRIIO IF REQUIRED *                           
         SPACE                                                                  
* OFFICE REGION TABLE - ONLY BUILT IF REGION PART OF REPORT *                   
         SPACE                                                                  
OFFREGTB DS    CL400     ALLOWS FOR 100 ENTRIES - 2 OFFICE, 2 REGION            
         SPACE                                                                  
* CATEGORY CLASS TABLE - ONLY BUILT IF CLASS PART OF REPORT *                   
         SPACE                                                                  
CATCLSTB DS    CL400     ALLOWS FOR 100 ENTRIES - 2 CAT, 2 CLASS                
         SPACE                                                                  
* STATION OWNER TABLE - ONLY BUILT IF OWNER PART OF REPORT *                    
         SPACE                                                                  
STAOWNTB DS    CL22440   COVERED BY STABLD IN REWRIWORKD                        
*                                                                               
STAONENT EQU   (L'STAOWNTB/STDLENE)   NUMBER OF ENTRIES                         
         SPACE                                                                  
*              ADDITIONAL INPUT ROUTINE TABLE                                   
         SPACE 3                                                                
ADDROUT  DS    0D                                                               
         DC    X'FF'                                                            
         SPACE                                                                  
         EJECT                                                                  
*              RECORD SAVE BUFFER AREAS                                         
         SPACE 3                                                                
NEEDTAB  DS    0F                                                               
         DC    AL1(RSTAKTYQ),AL3(STBUFF)                                        
         DC    AL1(RREGKTYQ),AL3(RGBUFF)                                        
         DC    AL1(ROFFKTYQ),AL3(OFBUFF)                                        
         DC    AL1(RTEMKTYQ),AL3(TMBUFF)                                        
         DC    AL1(RSALKTYQ),AL3(SLBUFF)                                        
         DC    AL1(RGRPKTYQ),AL3(GSBUFF)                                        
         DC    AL1(RADVKTYQ),AL3(ADBUFF)                                        
         DC    AL1(RPRDKTYQ),AL3(PRBUFF)                                        
         DC    AL1(RAGYKTYQ),AL3(AGBUFF)                                        
         DC    AL1(RCLSKTYQ),AL3(CLBUFF)                                        
         DC    AL1(RCTGKTYQ),AL3(CGBUFF)                                        
         DC    AL1(ROWNKTYQ),AL3(OWBUFF)                                        
         DC    AL1(RCTYKTYQ),AL3(CTBUFF)                                        
         DC    AL1(RPOPKTYQ),AL3(PPBUFF)                                        
         SPACE                                                                  
         DC    X'FF'                                                            
         SPACE                                                                  
         DS    0D                                                               
         DC    C'**STAT**'                                                      
STBUFF   DC    F'500'                     NO. BUFFERS                           
         DC    A(L'RSTAKSTA+L'RSTAMKT)    ENTRY SIZE                            
         DC    F'0'                                                             
         DC    (500*(L'RSTAKSTA+L'RSTAMKT))X'00'                                
         SPACE                                                                  
         DS    0D                                                               
         DC    C'*REGION*'                                                      
RGBUFF   DC    F'20'                                                            
         DC    A(L'RREGKREG+L'RREGNAME)                                         
         DC    F'0'                                                             
         DC    (20*(L'RREGKREG+L'RREGNAME))X'00'                                
         SPACE                                                                  
         DS    0D                                                               
         DC    C'*OFFICE*'                                                      
OFBUFF   DC    F'50'                                                            
         DC    A(L'ROFFKOFF+L'ROFFNAME)                                         
         DC    F'0'                                                             
         DC    (50*(L'ROFFKOFF+L'ROFFNAME))X'00'                                
         SPACE                                                                  
         DS    0D                                                               
         DC    C'*DIVTEM*'                                                      
TMBUFF   DC    F'100'                                                           
         DC    A(L'RTEMKTEM+L'RTEMDVNM+L'RTEMNAME)                              
         DC    F'0'                                                             
         DC    (100*(L'RTEMKTEM+L'RTEMDVNM+L'RTEMNAME))X'00'                    
         SPACE                                                                  
         DS    0D                                                               
         DC    C'*SALES**'                                                      
SLBUFF   DC    F'100'                                                           
         DC    A(L'RSALKSAL+L'RSALNAME)                                         
         DC    F'0'                                                             
         DC    (100*(L'RSALKSAL+L'RSALNAME))X'00'                               
         SPACE                                                                  
         DS    0D                                                               
         DC    C'*GRPSB**'                                                      
GSBUFF   DC    F'100'                                                           
         DC    A(L'RGRPKGRP+L'RGRPNAME+L'RGRPSBNM+1)                            
         DC    F'0'                                                             
         DC    (100*(L'RGRPKGRP+L'RGRPNAME+L'RGRPSBNM+1))X'00'                  
         SPACE                                                                  
         DS    0D                                                               
         DC    C'*ADVERT*'                                                      
ADBUFF   DC    F'100'                                                           
         DC    A(L'RADVKADV+L'RADVNAME)                                         
         DC    F'0'                                                             
         DC    (100*(L'RADVKADV+L'RADVNAME))X'00'                               
         SPACE                                                                  
         DS    0D                                                               
         DC    C'*PRDUCT*'                                                      
PRBUFF   DC    F'100'                                                           
         DC    A(L'RPRDKADV+L'RPRDKPRD+L'RPRDNAME)                              
         DC    F'0'                                                             
         DC    (100*(L'RPRDKADV+L'RPRDKPRD+L'RPRDNAME))X'00'                    
         SPACE                                                                  
         DS    0D                                                               
         DC    C'*AGENCY*'                                                      
AGBUFF   DC    F'100'                                                           
         DC    A(L'RAGYKAGY+L'RAGYKAOF+L'RAGYNAM1)                              
         DC    F'0'                                                             
         DC    (100*(L'RAGYKAGY+L'RAGYKAOF+L'RAGYNAM1))X'00'                    
         SPACE                                                                  
         DS    0D                                                               
         DC    C'*CLASS**'                                                      
CLBUFF   DC    F'100'                                                           
         DC    A(L'RCLSKCLS+L'RCLSNAME)                                         
         DC    F'0'                                                             
         DC    (100*(L'RCLSKCLS+L'RCLSNAME))X'00'                               
         SPACE                                                                  
         DS    0D                                                               
         DC    C'*CATEGY*'                                                      
CGBUFF   DC    F'100'                                                           
         DC    A(L'RCTGKCTG+L'RCTGNAME)                                         
         DC    F'0'                                                             
         DC    (100*(L'RCTGKCTG+L'RCTGNAME))X'00'                               
         SPACE                                                                  
         DS    0D                                                               
         DC    C'*OWNER**'                                                      
OWBUFF   DC    F'50'                                                            
         DC    A(L'ROWNKOWN+L'ROWNNAME)                                         
         DC    F'0'                                                             
         DC    (50*(L'ROWNKOWN+L'ROWNNAME))X'00'                                
         SPACE                                                                  
         DS    0D                                                               
         DC    C'*CONTYP*'                                                      
CTBUFF   DC    F'100'                                                           
         DC    A(L'RCTYKCTY+L'RCTYDESC)                                         
         DC    F'0'                                                             
         DC    (100*(L'RCTYKCTY+L'RCTYDESC))X'00'                               
         SPACE                                                                  
         DS    0D                                                               
         DC    C'*PTPERS*'                                                      
PPBUFF   DC    F'100'                                                           
         DC    A(L'RPTPKREC+L'RPTPNAME)                                         
         DC    F'0'                                                             
         DC    (100*(L'RPTPKREC+L'RPTPNAME))X'00'                               
         SPACE                                                                  
         SPACE                                                                  
*        INCLUDE DRGLOBAL                                                       
*        INCLUDE DRIVETABLE                                                     
*        INCLUDE DRINTRECD2                                                     
*        INCLUDE DDSPLWORKD                                                     
*        INCLUDE REGENALL1                                                      
*        INCLUDE REGENPWC                                                       
*        INCLUDE REWRIWORKD                                                     
*        PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DRINTRECD2                                                     
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE REGENALL1                                                      
       ++INCLUDE REGENPWC                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE REWRIWORKD                                                     
       ++INCLUDE REWRISTBD                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'074REWRIDRV  01/14/13'                                      
         END                                                                    
