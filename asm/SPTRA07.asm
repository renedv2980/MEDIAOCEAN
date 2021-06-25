*          DATA SET SPTRA07    AT LEVEL 131 AS OF 08/30/16                      
*PHASE T21607C                                                                  
*INCLUDE KHDUMMY                                                                
                                                                                
*>>>>>>>>>>>>>>                                                                 
* WATCH OUT - THIS ++INCLUDES SPTRAINSTC                                        
*>>>>>>>>>>>>>>                                                                 
                                                                                
         TITLE 'T21607 - AUTO COMMERCIAL INSTRUCTIONS'                          
**********************************************************************          
*                                                                    *          
* AIO USAGE - AIO1 VALI RTNS IN CTLR                                 *          
*             AIO2 USED IN BLDACT RTN TO UPDATE BUY ACTIVITY RECS    *          
*                  USED FOR STATION RECORD IF DAYPART EQUIVALENCY    *          
*                  CODE USED WITH PATTERN TYPE OR AFFIL              *          
*             AIO3 ACTIVITY TABLES                                   *          
*                                                                    *          
**********************************************************************          
*                                                                    *          
* LEV 46 BGRI JUN07/00 FIX LOCKET BUG                     F A        *          
* LEV 47 BGRI JAN03/01 ADD CATCHIOS                                  *          
* LEV 48 BGRI FEB23/01 FORCE ALL NUMERIC STATIONS TO AMS             *          
* LEV 49 BGRI FEB26/01 BYPASS LOCAL CABLE 7000-7500 NOT AMS          *          
* LEV 50 BGRI MAY22/01 CHANGE DUMMY                                  *          
* LEV 51 BGRI AUG01/01 FIX TRAFAX CHANGED TO 'C'                     *          
* LEV 52 SMUR AUG07/01 ADD OPTION ISCII                              *          
* LEV 53 BGRI MAR11/02 ADD OPTION FOR SPOT SEED - SVT2PR11           *          
* LEV 54 SMUR APR02/02 MOVE DSECTS AROUND/ FIX PAT SIZE ERROR        *          
* LEV 55 SMUR APR24/02 MOVE VALILOC FOR LOCK BY EST                  *          
* LEV 57 SMUR JUL30/02 UNLOCK WHEN EXITING WITH ERROR                *          
* LEV 58 BGRI SEP10/02 ADD SORT OPTION BY DATE                       *          
* LEV 59 SMUR SEP25/02 GLOBAL SELECT ALL (S+)                        *          
* LEV 60 SMUR OCT04/02 SHOW E-MAIL ADDRESS FROM CONTACT RECORD       *          
* LEV 61 SMUR JAN07/03 NULL PAD 0A36 INSTEAD OF SPACES               *          
* LEV 62 SMUR JAN08/03 FIX BAD BRANCH IN CONTACT RECORD LOGIC        *          
* LEV 63 BGRI MAR28/03 FIX CON= BLANK - MOVES JUNK                   *          
* LEV 64 SMUR MAY29/03 FIX FOR COMBINE CABLE STATION                 *          
* LEV 65 BGRI AUG20/03 ANY BUYS IN PERIOD FORCES ENTIRE PERIOD       *          
* LEV 66 SMUR DEC09/03 USE TRAFFIC OFFICE FOR PROFILE                *          
* LEV 67 BGRI DEC09/03 USE TRAFFIC OFFICE (SVCLTOFF)                 *          
* LEV 68 SMUR FEB10/04 2 CHARACTER MARKET GROUP                      *          
* LEV 74 BGRI MAR31/04 FIX NEW CABLE STATION MAX                     *          
* LEV 75 BGRI MAY14/04 CK FOR DUPLICATE MARKET GROUP ENTRY           *          
* LEV 77 ???? ??                                                     *          
* LEV 78 ???  ??       ATTEMPT TO FIX PRODUCT = ALL                  *          
* LEV 78 ???  ??       ATTEMPT TO FIX PRODUCT = ALL                  *          
* LEV 80 BGRI JUL19/04 TEST FOR READ-ONLY - FORCE OPTION TEST        *          
* LEV 78 SMUR SEP14/04 A QUICK FIX TO STOP DUMPS (BOB WILL REVISIT)  *          
* LEV 79 SMUR FEB27/06 SAVE CORRECT KEY WHEN AT THE END OF SCREEN    *          
* LEV 80 SMUR MAR16/06 FIX FAX FOR OV(NIGHT) WHEN READING TW PROFILE *          
* LEV 81 SMUR MAY01/06 GIVE ERROR MESSAGE WHEN FAX AND NOPRT         *          
* LEV100 SMUR DEC11/07 BYPASS TRAFFIC=NO BUYS, FIX EOF CHECK         *          
* LEV105 MNAS AUG13/08 ARCHIVING NOW REPORTS                         *          
* LEV107 SMUR SEP03/08 NOP BUY END DATE CHECK TO FIX MISSING STATION *          
* LEV111 SMUR DEC10/08 OPEN BPAT FOR AGY FR                          *          
* LEV115 MNAS JUL07/10 OPEN BPAT FOR AGENCY ALPHA H7                 *          
* LEV116 SMUR AUG06/10 BUG FIX DO NOT UNLOCK WHEN AMS GEN TO FOLLOW. *          
* LEV117 SMUR FEB25/11 FIX DATE VALID WHEN ONLY START DATE ENTERED   *          
* LEV118 MNAS MAR03/11 HONOR TW PROFILE FOR ID FDMFDA WHEN COMING    *          
*             FROM ANOTHER SCREEN - ELSE VALUES GET CARRIED OVER     *          
* LEV120 SMUR NOV07/11 OPEN BPAT FOR AGENCY ALPHA OO                 *          
* LEV121 MNAS FEB28/12 FIX FAX=C ISSUE WITH PQ ENTRY (NOT AWX)       *          
*                      REPRINT ISSUE READING PATTERN W/INVERTED PRDS *          
*                      CL0357187N - ERROR READING MARKET GROUPS      *          
* LEV122 SMUR APR30/12 INIT SEED FLAG TO FIX SEED REQUEST            *          
* LEV124 SMUR JUN22/12 FIX RERUN AND REPRINT OPTIONS                 *          
* LEV125 MNAS JAN22/13 MORE BANDS                                    *          
* LEV126 SMUR JAN28/14 PROCESS MARKET 0000 FOR MEDIA N REQUESTS      *          
* LEV127 MHER MAY16/14 ALLOW POL INST FOR LIMITED PRDS IN SVPOLQ     *          
*                      AND REMOVE NON-POL BUY PROCESSING             *          
* LEV129 SMUR MAR15/16 NEW BAND FOR IHEART RADIO CM                             
**********************************************************************          
         EJECT                                                                  
T21607   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NMOD1 0,T21607**,RA,RR=R8                                              
         ST    R8,RELO                                                          
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R8,SPTR07RR                                                      
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R3,ATWA                                                          
         USING T216FFD,R3                                                       
*                                                                               
         L     RE,=V(DUMMY)                                                     
         A     RE,RELO                                                          
         ST    RE,SPED0607                                                      
*                                                                               
         L     R7,ASVTABLE                                                      
         USING SVTABLED,R7                                                      
*                                                                               
         LA    R2,TRACOPYH         SET ADDRESS OF COPIES PRINTED FIELD          
         ST    R2,SCRCOPY                                                       
         LA    R1,L'TRACOPY                                                     
         STC   R1,SCRCOPY                                                       
         XC    TRACOPY,TRACOPY     CLEAR ANY LEFTOVER MESSAGES                  
         NI    1(R2),X'FF'-X'08'   UNSET HIGH INTENSITY                         
         OI    6(R2),X'80'         AND XMT                                      
*                                                                               
         LA    R2,TRASEL1H         SET ADDRESS OF FIRST SELECT FIELD            
         ST    R2,NEXTSELF                                                      
*                                                                               
         OI    TRATABH+1,X'01'     SET MODIFIED BIT                             
         OI    TRATABH+6,X'80'     FORCE XMT OF TAB FIELD                       
         B     GEN0                                                             
*                                                                               
RELO     DS    A                                                                
*                                                                               
EQXIT    CR    RE,RE                                                            
         B     *+6                                                              
NEQXIT   LTR   RE,RE                                                            
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*=================================================================              
* TEST FOR ANY CHANGES IN HEADLINE FIELDS                                       
*=================================================================              
                                                                                
GEN0     BRAS  RE,GETGLOB          CHECK FOR XFR GLOBALS                        
*                                                                               
GEN0X    TM    TRAMEDH+4,X'20'                                                  
         BZ    GEN10                                                            
         TM    TRAMEDH+4,X'80'     TEST INPUT THIS TIME                         
         BO    GEN10                                                            
         TM    TRACLTH+4,X'20'                                                  
         BZ    GEN10                                                            
         TM    TRACLTH+4,X'80'     TEST INPUT THIS TIME                         
         BO    GEN10                                                            
         TM    TRAPRDH+4,X'20'                                                  
         BZ    GEN10                                                            
         TM    TRAPRDH+4,X'80'     TEST INPUT THIS TIME                         
         BO    GEN10                                                            
         TM    TRAPTRH+4,X'20'                                                  
         BZ    GEN10                                                            
         TM    TRAPTRH+4,X'80'     TEST INPUT THIS TIME                         
         BO    GEN10                                                            
         TM    TRAESTH+4,X'20'                                                  
         BZ    GEN10                                                            
         TM    TRAESTH+4,X'80'     TEST INPUT THIS TIME                         
         BO    GEN10                                                            
         TM    TRAPERH+4,X'20'                                                  
         BZ    GEN10                                                            
         TM    TRAPERH+4,X'80'     TEST INPUT THIS TIME                         
         BO    GEN10                                                            
         TM    TRACONTH+4,X'20'                                                 
         BZ    GEN10                                                            
         TM    TRACONTH+4,X'80'    TEST INPUT THIS TIME                         
         BO    GEN10                                                            
         TM    TRAOPTH+4,X'20'                                                  
         BZ    GEN10                                                            
         TM    TRAOPTH+4,X'80'     TEST INPUT THIS TIME                         
         BO    GEN10                                                            
         TM    TRAFAXH+4,X'20'                                                  
         BZ    GEN10                                                            
         TM    TRAFAXH+4,X'80'     TEST INPUT THIS TIME                         
         BO    GEN10                                                            
*                                                                               
         TM    CONWHENH+4,X'20'    HAS WHEN CHANGED                             
         BZ    GEN10                                                            
                                                                                
*============================================                                   
* RELEVANT HEADLINE FIELDS UNCHANGED                                            
* CHECK FOR AMS/GEN PFKEY OR SELECT FIELDS                                      
*============================================                                   
                                                                                
         CLI   OFFLINE,C'Y'        TEST ONLINE                                  
         BE    GEN2                NO                                           
         CLI   SVAMSAUT,C'Y'       TEST AMS/GEN PENDING                         
         BNE   GEN2                                                             
         CLI   PFKEY,2                                                          
         BNE   GEN2                                                             
         OI    GENSTAT7,GES7BACK   SET FLAG TO RETURN TO 00                     
         NI    TRAMEDH+4,X'DF'     FORCE VALKEY NEXT TIME                       
         B     EXIT                                                             
                                                                                
GEN2     LA    R2,TRASEL1H                                                      
*                                                                               
GEN2A    TM    1(R2),X'20'                                                      
         BO    GEN2X                                                            
*                                                                               
         CLC   8(2,R2),SPACES                                                   
         BH    *+12                                                             
         TM    SVOPT3,OPTSALL                                                   
         BO    GEN200                                                           
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         CLI   8(R2),C'S'                                                       
         BNE   GEN2F                                                            
*                                                                               
         CLI   9(R2),C'+'          GLOBAL SELECT                                
         BE    *+12                                                             
         NI    SVOPT3,X'FF'-OPTSALL                                             
         B     GEN200                                                           
*                                                                               
         OI    SVOPT3,OPTSALL      TURN ON SELECT ALL                           
         B     GEN200              GO GENERATE INSTRUCTIONS                     
*                                                                               
GEN2F    CLI   8(R2),C' '                                                       
         BNE   GEN2X                                                            
*                                                                               
         CLI   9(R2),C'S'                                                       
         BNE   *+12                                                             
         NI    SVOPT3,X'FF'-OPTSALL                                             
         B     GEN200              GO GENERATE INSTRUCTIONS                     
*                                                                               
         CLI   9(R2),C' '                                                       
         BNE   *+12                                                             
         TM    SVOPT3,OPTSALL      SELECT ALL?                                  
         BO    GEN200                                                           
*                                                                               
GEN2X    LA    R2,NEXTLINE(R2)                                                  
         CLI   0(R2),9                                                          
         BH    GEN2A                                                            
*                                                                               
         NI    SVOPT3,X'FF'-OPTSALL                                             
*                                                                               
* NO FIELDS SELECTED - CONTINUE DISPLAY *                                       
*                                                                               
         BAS   RE,CLRSCR                                                        
         L     R7,ASVTABLE                                                      
         XC    0(L'SVTBDATA,R7),0(R7)                                           
         LA    R7,SVTBNEXT                                                      
         OC    SVTBLINE,SVTBLINE                                                
         BNZ   *-16                                                             
         L     R7,ASVTABLE                                                      
         B     GEN100                                                           
         EJECT                                                                  
GEN10    DS    0H                                                               
         LA    R2,TRAMEDH                                                       
         TM    4(R2),X'20'                                                      
         BO    GEN12                                                            
*                                                                               
* VALIDATE MEDIA *                                                              
*                                                                               
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
GEN12    LA    R2,TRACLTH                                                       
         BAS   RE,CLRCLT                                                        
*                                                                               
         CLI   SVXFROV,0           TEST XFRCTL ACTIVE                           
         JE    GEN14               NO                                           
         MVI   ERROPT,C'Y'         TELL GENCON TO RETURN ON ERROR               
         GOTO1 VALICLT                                                          
         CLI   ERROR,0                                                          
         JNE   GENERRV                                                          
*                                                                               
         L     RF,SYSPARMS                                                      
         L     RF,16(RF)                                                        
         ICM   RF,15,CGLOBBER-COMFACSD(RF)                                      
*  PASS CLTHDR KEY BACK IN GLOBAL                                               
         GOTO1 (RF),DMCB,=C'PUTD',KEY,18,GLVPRKEY                               
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         J     GEN15                                                            
*                                                                               
GEN14    MVI   ERROPT,C'N'         VALIDATE CLIENT                              
         GOTO1 VALICLT                                                          
*                                                                               
* READ T0 PROFILE *                                                             
*                                                                               
GEN15    XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         L     R1,AIO                                                           
         MVC   WORK+11(1),SVCLTOFF                                              
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
         MVI   WORK+3,C'3'         READ T3 PROF                                 
         GOTO1 (RF),(R1),,SVT3PROF                                              
                                                                                
* READ T2 PROFILE INTO SVT1PROF AND SAVE SOME OF THE FIELDS!                    
                                                                                
         MVI   WORK+3,C'2'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
         MVC   SVT2PR01(2),SVT1PROF                                             
         MVC   SVT2PR04(2),SVT1PROF+3                                           
         MVC   SVT2PR10(4),SVT1PROF+9  SAVE 10/11/12/13                         
         MVC   SVT2PR14(1),SVT1PROF+13                                          
         MVC   SVT2PR16(1),SVT1PROF+15  RMEM                                    
*                                                                               
         CLI   SVT2PR01,0          IF NOT BY MKT GROUP                          
         BE    GEN18                                                            
         CLI   SVT2PR01,C'0'                                                    
         BE    GEN18                                                            
         CLI   SVT2PR01,C'*'       OR MORE THAN ONE MKT GROUP                   
         BNE   *+12                                                             
         MVI   SVT2PR14,0                                                       
         B     GEN18                                                            
*                                                                               
         CLI   SVT2PR14,C'Z'       SEE IF 2 CHAR MKT GROUP                      
         BH    GEN18                                                            
         CLI   SVT2PR14,C'A'                                                    
         BL    GEN18                                                            
*                                                                               
         MVC   DUB(1),SVT2PR01                                                  
         MVC   DUB+1(1),SVT2PR14                                                
*                                                                               
* CONVERT 2 CHAR MARKET GROUP TO 1 CHAR                                         
*                                                                               
         L     RE,=A(SPMGRTAB)                                                  
         A     RE,RELO                                                          
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
*                                                                               
GEN16    CLC   DUB(2),0(RE)        IS THIS IT                                   
         BE    GEN16C                                                           
         LA    RE,3(RE)                                                         
         BCT   RF,GEN16                                                         
*                                                                               
         MVC   ERRTEXT(2),DUB                                                   
         MVC   GERROR,=Y(MGNF)                                                  
         GOTO1 VTRAERR                                                          
*                                                                               
GEN16C   MVC   SVT2PR01,2(RE)      MOVE HEX VALUE FROM TABLE                    
         MVI   SVT2PR14,X'FF'      CONVERTED MKT GROUP                          
*                                                                               
* READ TB PROFILE AND ONLY SAVE PROFILE 4                                       
*                                                                               
GEN18    MVI   WORK+3,C'B'                                                      
         GOTO1 GETPROF,DMCB,WORK,SVT1PROF,DATAMGR                               
*NOP     GOTO1 (RF),(R1),,SVT1PROF                                              
         MVC   SVTBPR04,SVT1PROF+3                                              
*                                                                               
* READ T1 PROFILE *                                                             
*                                                                               
         MVI   WORK+3,C'1'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
*                                                                               
         TM    WHEN,X'20'          THIS SOON REQUEST                            
         BZ    *+12                 NO                                          
         CLI   SVT1PR8,C'Y'        AUTO PIGGY BACK ON                           
         BE    AUTPIGER             YES                                         
*                                                                               
* READ TW PROFILE *                                                             
*                                                                               
         MVI   WORK+3,C'W'                                                      
         GOTO1 (RF),(R1),,ELEM                                                  
         MVC   SVTWPROF,ELEM                                                    
*                                                                               
         CLI   SVTWPR1,0                                                        
         BNE   *+8                                                              
         MVI   SVTWPR1,C'N'        SET TO NO FAX                                
*                                                                               
         CLI   SVPROF+1,C'T'       FOR TWX FOLLOW-UP                            
         BNE   *+8                                                              
         MVI   SVPROF+1,C'A'       FORCE AUTO GEN FOLLOW-UP                     
*                                                                               
GEN20    OI    4(R2),X'20'                                                      
         EJECT                                                                  
* EDIT OPTIONS BEFORE PRODUCT & ESTIMATE                                        
*                                        NO ESTIMATE FOR TRAFFIC BUYS *         
*                                        EXCEPT COPY CODE = ESTIMATE  *         
*                                                                               
         LA    R2,TRAOPTH                                                       
*                                                                               
         BAS   RE,CLROPT                                                        
                                                                                
* VALIDATE PRODUCT *                                                            
                                                                                
         LA    R2,TRAPRDH                                                       
         TM    4(R2),X'20'                                                      
         BO    GEN22                                                            
*                                                                               
         MVI   BPRD,0                                                           
         XC    QPRD,QPRD                                                        
         XC    SVPOLQ,SVPOLQ       CLEAR POL PRDLIST                            
         BAS   RE,CLREST                                                        
         CLI   SVPROF15,C'Y'       USE TRAFFIC, NOT SPOT BUYS                   
         BE    GEN22                PROD ALL NOT ALLOWED                        
         CLI   SVT1PR16,C'Y'       SPECIFIC PRODUCT REQUIRED                    
         BE    GEN22                PROD ALL NOT ALLOWED                        
         CLC   =C'ALL',TRAPRD                                                   
         BNE   GEN22                                                            
         CLI   SVT3PROF+3,C'Y'                                                  
         BE    GEN22A                                                           
         B     GEN23A                                                           
*                                                                               
GEN22    NI    SECFLAG,X'FF'-PRDTHTR INIT THEATRICAL PRODUCT                    
         GOTO1 VALIPRD             VALIDATE PRODUCT                             
*                                                                               
         CLI   SVT3PROF+3,C'Y'                                                  
         BNE   GEN22C                                                           
         CLC   =C'ALL',WORK                                                     
         BE    GEN22A                                                           
         CLC   =C'POL',WORK                                                     
         BNE   GEN22C                                                           
GEN22A   MVC   GERROR,=Y(INVAMSGN)                                              
         GOTO1 VTRAERR                                                          
*                                                                               
GEN22C   CLI   SVPROF13,C'Y'       USE PRODUCT EQUIVALENCY                      
         BNE   *+8                  NO                                          
         BAS   RE,PEQ                                                           
*                                                                               
         CLI   SVT1PR16,C'Y'       SPECIFIC PRODUCT REQUIRED                    
         BNE   GEN23                NO                                          
*                                                                               
         CLC   =C'ALL',WORK                                                     
         BE    SPCPRDER                                                         
         CLC   =C'POL',WORK                                                     
         BE    SPCPRDER                                                         
*                                                                               
GEN23    MVC   BPRD,WORK+3                                                      
         MVC   QPRD,WORK                                                        
         MVC   PRDNM,WORK+5                                                     
*                                                                               
GEN23A   OI    4(R2),X'20'         SET PRD VALID                                
*                                                                               
         CLI   BPRD,X'FF'          TEST POL REQUEST                             
         JNE   GEN24                                                            
         CLI   TRAPTRH+5,0         TEST INPUT IN PARTNER FIELD                  
         JE    GEN24               NO                                           
*                                                                               
         LA    R2,TRAPTRH                                                       
         BRAS  RE,VPOLLST          VALIDATE POL PRDLIST                         
         OI    4(R2),X'20'         SET PARTNER VALIDATED!                       
*                                                                               
* CLEAR ALL OPTIONS AND FIELDS PASSED TO AMS/GEN                                
*                                                                               
GEN24    MVI   SVOPT,0                                                          
         MVI   SVOPT2,0                                                         
         MVI   SVAMSAUT,0                                                       
                                                                                
         MVI   SEEDFLG,0           INIT SEED REQ GENERATED                      
*                                                                               
         CLC   TWAORIG,=X'335D'    TEST GROUPM                                  
         BNE   GEN24A                                                           
         LA    R2,TRAFAXH                                                       
         CLI   TRLSTPGM,X'04'                                                   
         BNE   GEN24C                                                           
*                                                                               
GEN24A   LA    R2,TRAFAXH                                                       
         CLI   5(R2),0                                                          
         BNE   GEN24E                                                           
GEN24C   MVC   8(1,R2),SVTWPR1                                                  
         MVI   5(R2),1             SET INPUT LENGTH                             
         OI    6(R2),X'80'                                                      
         MVI   TRLSTPGM,X'04'                                                   
         B     GEN25                                                            
*                                                                               
GEN24E   CLC   =C'TWXAUTO',CONREC                                               
         BE    GEN25                                                            
*                                                                               
         MVC   SVTWPROF(1),8(R2)                                                
         CLI   8(R2),C'C'          COPY ONLY                                    
         BE    GEN24G                                                           
         CLI   8(R2),C'N'          NO FAX                                       
         BE    GEN25                                                            
         CLI   8(R2),C'Y'          FAX ONLY                                     
         BE    GEN24G                                                           
         CLI   8(R2),C'2'          FAX AND COPY                                 
         BE    GEN24G                                                           
         MVC   GERROR,=Y(BADFAX)                                                
         GOTO1 VTRAERR                                                          
*                                                                               
GEN24G   MVI   SVT1PR9,C'N'        FOR FAX, FORCE TO PRINT                      
*                                                                               
GEN25    CLC   =C'TWXAUTO',CONREC                                               
         BNE   GEN25E                                                           
         CLI   8(R2),C'2'          REQ COPY?                                    
         BNE   GEN25E                                                           
         MVI   8(R2),C'Y'                                                       
         OI    6(R2),X'80'                                                      
         MVI   SVTWPR1,C'Y'                                                     
*                                                                               
GEN25E   OI    4(R2),X'20'                                                      
*                                                                               
         EJECT                                                                  
* VALIDATE PARTNER *                                                            
*                                                                               
GEN26    LA    R2,TRAPTRH                                                       
         TM    4(R2),X'20'                                                      
         BO    GEN28                                                            
         CLI   BPRD,X'FF'          TEST POL REQ                                 
         BE    GEN28               YES - DON'T VALIDATE HERE!                   
         MVI   BPRD2,0                                                          
         XC    QPRD2,QPRD2                                                      
         XC    PRD2NM,PRD2NM                                                    
         CLI   5(R2),0                                                          
         BE    GEN28                                                            
         CLC   =C'NONE',TRAPTR                                                  
         BNE   *+12                                                             
         MVI   BPRD2,X'FF'                                                      
         B     GEN28                                                            
*                                                                               
         GOTO1 VALIPRD             VALIDATE PRODUCT                             
*                                                                               
         CLC   =C'ALL',WORK                                                     
         BE    *+14                                                             
         CLC   =C'POL',WORK                                                     
         BNE   *+12                                                             
         MVI   ERROR,INVPROD                                                    
         J     GENERRV                                                          
*                                                                               
         CLC   =C'SJ',AGENCY       SJR                                          
         BE    GEN26C                                                           
         CLC   =C'FR',AGENCY       AGY FR                                       
         BE    GEN26C                                                           
         CLC   =C'H7',AGENCY       AGY FR                                       
         BE    GEN26C                                                           
         CLC   =C'OO',AGENCY       AGY OO                                       
         BE    GEN26C                                                           
         CLC   =C'M2',AGENCY       IF AGENCY MEDIACOM                           
         BNE   GEN26D                                                           
*                                                                               
GEN26C   TM    SECFLAG,PRDTHTR     THEATRICAL PRODUCT                           
         BZ    GEN26F                                                           
*                                                                               
GEN26D   MVC   GERROR,=Y(NOPBTHTR)  NO P/B FOR THEATRICAL PRODUCTS              
         GOTO1 VTRAERR                                                          
*                                                                               
GEN26F   CLI   SVPROF13,C'Y'       USE PRODUCT EQUIVALENCY                      
         BNE   *+8                  NO                                          
         BAS   RE,PEQ                                                           
*                                                                               
         MVC   BPRD2,WORK+3                                                     
         MVC   QPRD2,WORK                                                       
         MVC   PRD2NM,WORK+5                                                    
*                                                                               
GEN28    OI    4(R2),X'20'         SET PRD VALID                                
         EJECT                                                                  
* EDIT ESTIMATE *                                                               
*                                                                               
         LA    R2,TRAESTH                                                       
*                                                                               
         TM    4(R2),X'20'                                                      
         BO    GEN35                                                            
         CLI   SVT3PROF+3,C'Y'                                                  
         BNE   GEN29C                                                           
         CLC   =C'NO',TRAEST                                                    
         BNE   GEN29C                                                           
         MVC   GERROR,=Y(INVAMSGN)                                              
         GOTO1 VTRAERR                                                          
GEN29C   DS    0H                                                               
         BAS   RE,CLRPER                                                        
*                                                                               
         XC    SVESTAB,SVESTAB     CLEAR SVESTAB                                
*                                                                               
         MVI   QBEST,0                                                          
         MVI   QBESTEND,0                                                       
         GOTO1 ANY                                                              
*                                                                               
         CLC   =C'NO ',WORK                                                     
         BNE   GEN30                                                            
         CLI   SVPROF11,C'E'       COPY CODE = ESTIMATE                         
         BE    CCESTER                                                          
         B     GEN35                                                            
*                                                                               
GEN30    XC    BLOCK(32),BLOCK                                                  
         GOTO1 SCANNER,DMCB,(R2),(2,BLOCK),C',=,-'                              
*                                                                               
         LA    R4,BLOCK                                                         
         MVI   ERROR,MISSING                                                    
         CLI   0(R4),0                                                          
         JE    GENERRV                                                          
         MVC   QBEST,7(R4)         SET AS START EST                             
         MVC   QBESTEND,7(R4)      AND AS END                                   
         CLC   6(2,R4),=H'255'                                                  
         JH    GENERRV                                                          
         OC    6(2,R4),6(R4)                                                    
         JZ    GENERRV                                                          
*                                                                               
         CLI   1(R4),0             TEST SECOND ESTIMATE GIVEN                   
         BE    GEN32               NO                                           
*                                                                               
         CLI   SVPROF11,C'E'       COPY CODE = ESTIMATE                         
         BE    CCESTER                                                          
*                                                                               
         MVI   ERROR,BADESTS                                                    
         MVC   QBESTEND,11(R4)                                                  
         CLC   10(2,R4),=H'255'                                                 
         JH    GENERRV                                                          
         OC    10(2,R4),10(R4)                                                  
         JZ    GENERRV                                                          
*                                                                               
         CLC   QBEST,QBESTEND                                                   
         JH    GENERRV                                                          
         B     GEN35                                                            
*                                                                               
GEN32    DS   0H                                                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+4(3),QPRD                                                    
         MVC   KEY+7(1),QBEST                                                   
*                                                                               
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA                       
         GOTO1 HIGH                                                             
*                                                                               
         MVI   ERROR,NOESTS                                                     
         CLC   KEY(13),KEYSAVE     SEE IF ESTIMATE FOUND                        
         JNE   GENERRV                                                          
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'SPTFIL' SWITCH TO SPOT MEDIA                       
         GOTO1 GETREC                                                           
*                                                                               
         USING ESTHDRD,R6                                                       
         CLI   SVPROF11,C'E'       COPY CODE = ESTIMATE                         
         BE    *+12                                                             
         CLI   SVPROF15,C'Y'       USE TRAFFIC, NOT SPOT BUYS                   
         BE    GEN34                                                            
         CLI   ECOPY,0             COPY CODE ILLEGAL                            
         BE    GEN34                                                            
         MVC   GERROR,=Y(CCESTERQ)                                              
         GOTO1 VTRAERR                                                          
*                                                                               
GEN34    GOTO1 DATCON,DMCB,(0,ESTART),(2,SVFLTST)                               
         GOTO1 (RF),(R1),(0,EEND),(2,SVFLTEND)                                  
         MVC   QESTDESC,EDESC                                                   
         MVC   OWRSDAY,EOWSDAY                                                  
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
GEN35    DS   0H                                                                
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLI   OFFLINE,C'Y'        NOT IF OFFLINE                               
         BE    GEN36                                                            
         TM    WHEN,X'18'          OR IF OVERNIGHT PROCESSING                   
         BNZ   GEN36                                                            
*                                                                               
* USE LOCKET TO SEE IF LOCKED OUT ON ANY RECORDS *                              
*                                                                               
         MVI   DUB,X'E3'                     T=TEST                             
         MVC   DUB+1(7),=X'030A220A240A25'   03=3 ENTRIES                       
*                                            0A22, 0A24, 0A25 RECS              
         GOTO1 VALILOC,0                                                        
*                                                                               
         CLI   SVTWPR1,C'N'                                                     
         BE    GEN36                                                            
*                                                                               
         CLI   SVTWPR1,C'C'                                                     
         BE    GEN36                                                            
         MVC   REMUSER,=C'AWX'                                                  
         MVI   SPOOLKEY+PLCLASS-PQPLD,C'G'                                      
         OC    SPOOLKEY+QLTYP1-PQPLD(1),SVFAXARC                                
*                                                                               
         ICM   RE,15,TWAMASTC                                                   
         BZ    GEN36                                                            
         MVI   MCREQREP-MASTD(RE),C'N' SUPPRESS REQUEST PAGE                    
*                                                                               
***** MOVED HERE ****** DON'T KNOW IF ITS A GOOD IDEA *******                   
*                                                                               
GEN36    DS   0H                                                                
         LA    R2,TRAOPTH                                                       
*                                                                               
         BAS   RE,CLROPT                                                        
*                                                                               
         BRAS  RE,VOPT                                                          
         CLC   =C'SJ',AGENCY       SJR                                          
         BE    GEN37                                                            
         CLC   =C'FR',AGENCY       AGY FR                                       
         BE    GEN37                                                            
         CLC   =C'H7',AGENCY       AGY H7                                       
         BE    GEN37                                                            
         CLC   =C'OO',AGENCY       AGY OO                                       
         BE    GEN37                                                            
         CLC   =C'M2',AGENCY       IF AGENCY MEDIACOM                           
         BNE   GEN37F                                                           
*                                                                               
GEN37    TM    SECFLAG,PRDTHTR     THEATRICAL PRODUCT                           
         BZ    GEN37C                                                           
         TM    SVOPT3,OPTBPAT      MUST USE BPAT RECORDS                        
         BO    GEN37F                                                           
         MVC   GERROR,=Y(OBPATHTR)  RUN WITH OPTION BPAT FOR THTR PRD           
         GOTO1 VTRAERR                                                          
*                                                                               
GEN37C   TM    SVOPT3,OPTBPAT      MUST USE PAT FOR NON THTR PROD               
         BZ    GEN37F                                                           
         MVC   GERROR,=Y(NOBPATMS) BPAT IS NOT VALID FOR NON-THTR               
         GOTO1 VTRAERR                                                          
*                                                                               
GEN37F   CLI   SVTWPR1,C'N'        CAN NOT FAX                                  
         BE    GEN39                                                            
         CLI   SVT1PR9,C'Y'        PROFILE TO NOPRT                             
         BNE   GEN38                                                            
*                                                                               
         NI    TRAOPTH+4,X'FF'-X'20'                                            
         LA    R2,TRAFAXH                                                       
         MVC   GERROR,=Y(DOFAXNO)  CANNOT FAX WITH NOPRT                        
         GOTO1 VTRAERR                                                          
*                                                                               
GEN38    TM    SVOPT,OPTTEST       IF TEST RUN                                  
         BZ    GEN39                                                            
         CLI   SVTWPR1,C'N'        CAN NOT FAX                                  
         BE    GEN39                                                            
*                                                                               
         NI    TRAOPTH+4,X'FF'-X'20'                                            
         LA    R2,TRAFAXH                                                       
         MVC   GERROR,=Y(DOFAXNO2)                                              
         GOTO1 VTRAERR                                                          
*                                                                               
***************************************************************                 
*                                                                               
* EDIT PERIOD *                                                                 
*                                                                               
GEN39    LA    R2,TRAPERH                                                       
*                                                                               
         BAS   RE,CLRPER                                                        
*                                                                               
         BAS   RE,VPER                                                          
*                                                                               
         CLI   SVAMSOPT,C'N'       TEST AMSGEN=NO                               
         JE    GEN40                                                            
         CLI   SVT2PR16,C'Y'       TEST AUTO/AMS GEN                            
         JNE   GEN40                                                            
         CLI   SVPROF11,C'E'       TEST COPYCODE=EST                            
         JNE   GEN39A                                                           
* IF CC EQ EST, MUST ENTER EST                                                  
         CLI   QBEST,0             TEST RUNNING BY EST                          
         JNE   GEN40               YES                                          
         MVC   GERROR,=Y(CCESTERQ)                                              
         GOTO1 VTRAERR                                                          
* IF CC NEQ EST, MUST NOT ENTER EST                                             
GEN39A   CLI   QBEST,0                                                          
         JE    GEN40                                                            
         MVC   GERROR,=Y(ESTNECC)                                               
         GOTO1 VTRAERR                                                          
*                                                                               
* GO READ ESTIMATE HEADERS AS REQUIRED *                                        
*                                                                               
GEN40    MVI   ERROR,0                                                          
         BAS   RE,BLDEST                                                        
*                                                                               
         BZ    *+8                                                              
         MVI   ERROR,NOESTS                                                     
*                                                                               
         CLI   ERROR,0                                                          
         JNE   GENERRV                                                          
*                                                                               
         OI    TRAPERH+4,X'20'     SET PERIOD VALID                             
         OI    TRAESTH+4,X'20'     SET ESTIMATE VALID                           
         OI    CONWHENH+4,X'20'    SET WHEN TO CATCH CHANGES                    
*                                                                               
GEN50    LA    R2,TRASHIPH         POINT TO SHIPPING OPTION                     
         CLI   5(R2),0             TEST INPUT                                   
         BE    GEN52               NO                                           
         CLI   8(R2),C'Y'                                                       
         BE    GEN54                                                            
         CLI   8(R2),C'N'                                                       
         BE    GEN54                                                            
         CLI   8(R2),C'A'                                                       
         BE    GEN54                                                            
         MVI   ERROR,INVALID                                                    
         J     GENERRV                                                          
*                                                                               
GEN52    CLI   SVPROF6,0           IS THERE ANY PROFILE OPTION                  
         BE    GEN54                NO                                          
         MVC   TRASHIP(1),SVPROF6  MOVE PROFILE OPTION                          
         OI    6(R2),X'80'                                                      
         MVI   5(R2),1                                                          
*                                                                               
GEN54    LA    R2,CONOUTH          POINT TO OUTPUT TYPE                         
         CLI   5(R2),0             TEST DATA PRESENT                            
         BNE   GEN55               YES                                          
         CLI   RECNUM,47           TWX INSTR                                    
         BE    GEN56                                                            
*                                                                               
         CLI   SVPROF8,C'Y'        TEST AUTO MAILER OUTPUT                      
         BNE   GEN56                                                            
         MVC   8(3,R2),=C'MLR'                                                  
         OI    6(R2),X'80'                                                      
         MVI   5(R2),3                                                          
         MVC   TWAOUT(3),=C'MLR'   FIX UP FOR GENCON                            
         MVC   TWAOUT+3(3),SPACES                                               
         B     GEN56                                                            
GEN55    MVI   ERROR,INVALID                                                    
         CLI   RECNUM,47           TWX INSTR                                    
         JE    GENERRV                                                          
*                                                                               
GEN56    LA    R2,TRACONTH         CONTACT IS REQUIRED BUT NOT EDITED           
         XC    QUESTOR,QUESTOR                                                  
         XC    CONTEL,CONTEL                                                    
         XC    CONFAX,CONFAX                                                    
         XC    CONEMAIL,CONEMAIL                                                
         GOTO1 ANY                                                              
         MVC   QUESTOR(L'TRACONT),TRACONT                                       
         MVC   SVQUESTR,TRACONT                                                 
         CLC   =C'CON=',WORK       THIS AGY CONTACT KEY                         
         BNE   GEN85                                                            
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CNTKEY,R4                                                        
         MVC   CNTKID,=X'0A36'                                                  
         MVC   CNTKAM(3),BAGYMD                                                 
*                                                                               
         MVI   ERROR,MISSING                                                    
         CLI   5(R2),4             MUST BE MORE THAN 4 CHARACTERS               
         JNH   GENERRV                                                          
         MVI   ERROR,0                                                          
*                                                                               
         CLI   5(R2),12            NO MORE THAN 8 CHARS ALLOWED                 
         BNH   *+12                                                             
         LA    R1,7                                                             
         B     GEN56X                                                           
*                                                                               
         ZIC   R1,5(R2)            INPUT LENGTH                                 
         AHI   R1,-5               MINUS 4 (CON=) 1 (FOR EX)                    
*                                                                               
GEN56X   EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CNTKNAME(0),12(R2)                                               
*                                                                               
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    GEN58                                                            
         MVC   KEY,KEYSAVE                                                      
         XC    CNTKCLT,CNTKCLT                                                  
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    GEN58                                                            
         MVI   ERROR,NOTFOUND                                                   
         J     GENERRV                                                          
         DROP  R4                                                               
*                                                                               
GEN58    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CNTDTAEL,R6                                                      
         MVC   QUESTOR,CNTNAME                                                  
         MVC   CONTEL,CNTTEL                                                    
*                                                                               
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   GEN60                                                            
         USING CNTFAXEL,R6                                                      
         MVC   CONFAX,CNTFTEL                                                   
GEN60    DS    0H                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GEN85                                                            
*                                                                               
         USING CNTEMLEL,R6                                                      
         MVC   CONEMAIL,CNTEMLAD                                                
*                                                                               
GEN85    OI    TRACONTH+4,X'20'                                                 
*                                                                               
         B     GEN100                                                           
         DROP  R6                                                               
         EJECT                                                                  
* HEADLINES VALIDATED - GO BUILD DISPLAY SCREEN *                               
*                                                                               
GEN100   CLC   =C'SUB',SVXFRMOD                                                 
         BE    GEN200                                                           
         CLI   MODE,PRINTREP                                                    
         BE    GEN100X                                                          
         TM    WHEN,X'18'          TEST DDS OR OV                               
         BZ    EXIT                 NO                                          
*                                                                               
* CHECK OFFLINE REQUESTS TO HAVE BUYS IN ADDITION TO NOW PROCESSING *           
*                                                                               
GEN100X  OI    TRAOPTH+4,X'20'     SET OPTIONS VALIDATED                        
         MVI   SVBIGSW,C'N'        RESET SWITCH                                 
*                                                                               
* GET FLIGHT DATES IN 2 BYTE FORM *                                             
*                                                                               
*        GOTO1 DATCON,DMCB,(3,SVGENST),(2,GENSTP)                               
*        GOTO1 (RF),(R1),(3,SVGENEND),(2,GENENDP)                               
*                                                                               
GEN102   DS    0H                                                               
         CLI   OFFLINE,C'Y'        TEST OFFLINE PROCESSING                      
         JE    GEN150                                                           
*                                                                               
         OC    SVKEY,SVKEY         TEST CONTINUATION                            
         BNZ   GEN108                                                           
*                                                                               
         CLC   =C'ALL',TRAPRD                                                   
         BNE   GEN106                                                           
*                                                                               
GEN104   BAS   RE,NEXTPRD                                                       
         BNE   GEN110                                                           
*                                                                               
GEN106   MVC   SVKEY(3),BAGYMD     A-M/CLT                                      
         MVC   SVKEY+3(1),BPRD                                                  
         MVC   SVKEY+4(5),BMKT                                                  
*                                                                               
         CLI   SVPROF13,C'Y'       USING PRODUCT EQUIV                          
         BNE   GEN108               NO                                          
         MVI   SVKEY+3,X'FF'       READ POL PTRS (TRAFFIC TOO)                  
*                                                                               
* ONLINE PROCEESING FOR OFFLINE REQUESTS WILL STOP AT 1ST BUY *                 
                                                                                
GEN108   BRAS  RE,BLDACT                                                        
                                                                                
         LA    R2,TRASEL1H                                                      
         MVC   CONHEAD(L'GENMSG),GENMSG                                         
*                                                                               
         L     R7,ASVTABLE                                                      
                                                                                
         OC    SVTBLINE,SVTBLINE   TEST MORE DATA                               
         BNZ   GEN120              YES - EXIT                                   
*                                                                               
         CLC   =C'ALL',TRAPRD      TEST ALL PRD REQ                             
         BE    GEN104                                                           
                                                                                
* NO MORE DATA FOR THIS REQUEST *                                               
*                                                                               
GEN110   MVC   CONHEAD(L'GENXMSG),GENXMSG                                       
*                                                                               
         CLI   SVAMSAUT,C'Y'       TEST AMS/GEN PENDING                         
         BE    *+12                YES                                          
         NI    TRAMEDH+4,X'DF'     ELSE FORCE RE-VALIDATION                     
         B     GEN111                                                           
*                                                                               
         CLI   OFFLINE,C'Y'        TEST OFFLINE                                 
         BE    GEN111                                                           
         TM    WHEN,X'18'          TEST WHEN OR OVERNIGHT                       
         BNZ   GEN111                                                           
         MVC   TRACOPY+12(36),=C'* AMS/GEN PENDING - HIT PF2 TO RUN *'          
         OI    TRACOPYH+6,X'80'    SET TO XMT                                   
         OI    TRACOPYH+1,X'08'    SET HIGH INTENSITY                           
*                                                                               
GEN111   TM    SVOPT2,OP2ISACT     ANY BUYS FOR PERIOD                          
         BO    GEN120              YES                                          
*                                                                               
         MVC   GERROR,=Y(NOBUYSFD)  NO BUYS FOUND                               
         MVC   ERRTEXT(4),=C'BUYS'                                              
         TM    SVOPT3,OPT3REPT                                                  
         BZ    GENDONE                                                          
         MVC   ERRTEXT(4),=C'INST'                                              
         GOTO1 VTRAERR                                                          
*                                                                               
GEN120   TM    WHEN,X'18'          THIS A DDS OR OV REQUEST                     
         BNZ   EXIT                                                             
*                                                                               
GENDONE  CLI   SVXFROV,0           IF GOT HERE BY XFRCTL                        
         JNE   EXIT                THEN JUST EXIT!                              
         GOTO1 ERREX2                                                           
*                                                                               
GENMSG   DC    C'* DATA DISPLAYED - ENTER SELECT CODES *'                       
GENXMSG  DC    C'* PROCESSING COMPLETED - ENTER NEXT REQUEST *'                 
         EJECT                                                                  
* PROCESS OFF-LINE INSTRUCTIONS *                                               
*                                                                               
GEN150   CLC   =C'ALL',TRAPRD                                                   
         BNE   GEN158                                                           
*                                                                               
         BAS   RE,NEXTPRD                                                       
         BZ    GEN180                                                           
*                                                                               
GEN158   OC    SVKEY,SVKEY         TEST CONTINUE PREVIOUS                       
         BNZ   GEN158X                                                          
         MVC   SVKEY(4),BAGYMD     A-M/CLT/PRD                                  
         MVC   SVKEY+4(5),BMKTSTA                                               
*                                                                               
GEN158X  L     R7,ASVTABLE                                                      
         XC    0(L'SVTBDATA,R7),0(R7)                                           
         LA    R7,L'SVTBDATA(R7)                                                
         OC    0(4,R7),0(R7)       TEST TO CLEAR MORE                           
         BNZ   *-16                                                             
*                                                                               
         L     RE,ASHPLIST                                                      
         XC    0(256,RE),0(RE)     CLEAR TOP OF SHIP LIST                       
*                                                                               
GEN159   DS   0H                                                                
         BRAS  RE,BLDACT                                                        
*                                                                               
         L     R7,ASVTABLE                                                      
GEN159A  OC    SVTBLINE,SVTBLINE   TEST DATA PRESENT                            
         BZ    GEN159C                                                          
*                                                                               
* BUILD SVPRD ENTRIES FOR ALL ACTIVE PRODUCTS *                                 
*                                                                               
         LA    R4,SVTBPRD                                                       
         BAS   RE,GNSVPR                                                        
         CLI   SVTBPRD2,0                                                       
         BE    *+12                                                             
         LA    R4,SVTBPRD2                                                      
         BAS   RE,GNSVPR                                                        
         LA    R7,SVTBNEXT                                                      
         B     GEN159A                                                          
*                                                                               
GEN159C  L     R7,ASVTABLE                                                      
         OC    SVTBLINE,SVTBLINE   TEST DATA PRESENT                            
         BNZ   GEN159D                                                          
         CLI   TRAWRKRH+5,0        TEST PROCESSING VIA WKFILE                   
         BE    GEN162              NO - DONE                                    
         B     GEN160              GO SEE IF MORE DATA                          
*                                                                               
GEN159D  MVI   ERROPT,C'Y'         SET TO RETURN ON ERROR                       
         GOTO1 VGETPTNS                                                         
*                                                                               
         CLI   ERROR,0                                                          
         BNE   GEN160                                                           
*                                                                               
         MVI   MLRYORN,C'N'                                                     
         CLC   =C'MLR',CONOUT                                                   
         BNE   *+8                                                              
         MVI   MLRYORN,C'Y'                                                     
*                                                                               
         MVC   SHIPYORN,TRASHIP    MOVE SHIPPING OPTION                         
*                                                                               
         MVC   QUWHEN,CONWHEN      FOR AUTO PTN RECAP                           
*                                                                               
         GOTO1 VPRT,DMCB,(RC)                                                   
         BAS   RE,ERRTEST                                                       
*                                                                               
         TM    SVTBIND2,SVTBICGR   THIS A CABLE STATION WITH GRP CDE            
         BZ    GEN160                                                           
*                                                                               
         CLI   INSPRTSW,C'N'       DID WE PRINT INST                            
         BE    GEN160              NO                                           
*                                                                               
         TM    SVOPT,OPTTEST       TEST OPTION TEST                             
         BO    GEN160              YES - NEVER DO AMS/GEN                       
*                                                                               
         CLI   SVAMSOPT,C'Y'       TEST TO OVERRIDE PROFILE                     
         BE    GEN159X                                                          
*                                                                               
         CLI   SVAMSOPT,C'N'       TEST DO NOT DO AMS/GEN                       
         BE    GEN160                                                           
*                                                                               
         CLI   SVT2PR16,C'Y'       TEST AUTO AMS IF NEEDED                      
         BNE   GEN160                                                           
*                                                                               
GEN159X  MVI   SVAMSAUT,C'Y'                                                    
*                                                                               
GEN160   CLI   SVKEY,X'FF'         TEST NO MORE BUYS THIS A-M/CLT/PRD           
         BE    GEN162                                                           
         OC    SVKEY,SVKEY         TEST TO CONTINUE                             
         BNZ   GEN158X                                                          
*                                                                               
GEN162   CLC   =C'ALL',TRAPRD                                                   
         BE    GEN150                                                           
*                                                                               
GEN180   CLI   TRAWRKRH+5,0        TEST OPTICA                                  
         BNE   GEN182              YES - ALWAYS PRINT COPY                      
         CLI   SVTWPR1,C'2'        THIS FAX AND COPY REQUEST                    
         BNE   GEN190               NO                                          
         TM    WHEN,X'20'          THIS A SOON RUN                              
         BZ    GEN182                                                           
         BAS   RE,GENOV                                                         
         B     GEN190                                                           
*                                                                               
GEN182   GOTO1 VPCOPY              PRINT COPY OF FAXED INSTRUCTIONS             
*                                                                               
GEN190   CLI   OFFLINE,C'Y'        TEST ONLINE                                  
         BNE   EXIT                YES - EXIT TILL HIT PFKEY                    
         CLI   SVAMSAUT,C'Y'       TEST NEED AUTO AMS                           
         BNE   EXIT                NO                                           
*                                                                               
         BRAS  RE,AMSREQ           GO ADD REQ FOR STEP2 TO WKFILE               
         MVI   SVAMSAUT,C'X'       THEN RESET FLAG                              
         B     EXIT                                                             
         EJECT                                                                  
*==================================================================             
* GENERATE OVERNIGHT FOLLOW-UP REQUEST                                          
*==================================================================             
                                                                                
GENOV    NTR1                                                                   
         XC    REQHDR,REQHDR                                                    
         MVC   REQUEST,SPACES                                                   
         MVI   REQHDR+15,X'01'     GENERATE LINKED REQUESTS                     
         MVC   REQUEST(2),=C'TA'                                                
         MVC   REQUEST+2(2),AGENCY                                              
*                                                                               
         MVC   CONWHEN(2),=C'OV'                                                
         MVC   CONWHEN+2(4),CONWHEN+4                                           
         MVI   CONWHEN+6,0                                                      
         MVI   CONWHEN+7,0                                                      
         ZIC   RF,CONWHENH+5                                                    
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         STC   RF,CONWHENH+5                                                    
*                                                                               
         L     R1,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(R1)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     RE,DMCB             FAFACTS                                      
         L     R0,FASIN-FACTSD(,RE)                                             
*                                                                               
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  REQUEST+5(6),DUB                                                 
*                                                                               
         IC    R0,TRAFAX           SAVE THE ORIGINAL FAX VALUE                  
         MVI   TRAFAX,C'C'         SET TO COPY                                  
*                                                                               
         XC    WORK,WORK           NEED TO PASS A SPOOK                         
         GOTO1 REQTWA,DMCB,(0,ATWA),REQHDR,DATAMGR,RCCOMFAC,WORK                
         STC   R0,TRAFAX           RESTORE ORIGINAL FAX VALUE                   
*                                                                               
         MVC   WORK(4),CONWHEN+2        SAVE ,REQ                               
         MVC   CONWHEN(4),=C'SOON'      RESET TO SOON                           
         MVC   CONWHEN+4(4),WORK        RESTORE ,REQ                            
         ZIC   RF,CONWHENH+5                                                    
         LA    RF,2(,RF)                                                        
         STC   RF,CONWHENH+5                                                    
         B     EXIT                                                             
         EJECT                                                                  
*====================================================================           
* USER HAS SELECTED INSTRUCTIONS                                                
* IF XFRCTL MODE=SUB, SVTBLINE HAS X'0122FF',C'S' FOR SELECTED LINES            
*====================================================================           
                                                                                
*                                                                               
GEN200   CLI   SVXFROV,7                                                        
         JNE   GEN220                                                           
         CLC   =C'SUB',SVXFRMOD                                                 
         JNE   *+2                                                              
         BRAS  RE,ADDWRKR          GO ADD WORKER FILE AND OFFLINE REQ           
         J     EXIT                                                             
*                                                                               
GEN220   CLI   SVBIGSW,C'Y'        TEST SVTABLE SAVED ON DISK                   
         BNE   GEN222                                                           
*                                                                               
         XC    DMCB(24),DMCB                                                    
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    MOVE TERMINAL NUMBER                         
         MVI   DMCB+8,2            SET PAGE 2                                   
         GOTO1 DATAMGR,DMCB,=C'DMRDIR',=C'TEMPSTR',,ASVTABLE                    
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
* FIND THE SELECTED SVTABLE ENTRY *                                             
*                                                                               
GEN222   L     R0,SVTBLINE         GET REL LINE DSPL                            
         A     R0,ATWA             RELOCATE                                     
         CR    R2,R0                                                            
         BE    GEN224                                                           
         LA    R7,L'SVTBDATA(R7)                                                
         OC    SVTBLINE,SVTBLINE                                                
         BNZ   GEN222                                                           
         DC    H'0'                                                             
*                                                                               
GEN224   ST    R7,ASVTABLE         SET AS START OF TABLE ADDRESS                
*                                                                               
* NEED TO REREAD MARKET RECORD FOR FULL NAME *                                  
*                                                                               
         USING DSPLINED,R2                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         SR    R0,R0                                                            
         ICM   R0,3,SVTBMKST                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+2(4),DUB                                                     
         MVC   KEY+6(2),AGENCY                                                  
         MVC   AIO,AIO1                                                         
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO                      
*                                                                               
         L     R6,AIO                                                           
         USING MKTRECD,R6                                                       
         CLC   KEY(8),0(R6)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*****                                                                           
*****  NEED TEST FOR LIMIT ACCESS HERE *****                                    
*****                                                                           
         MVC   MKTNM,MKTNAME                                                    
         DROP  R6                                                               
         EJECT                                                                  
* BUILD SVPRD ENTRIES FOR ALL ACTIVE PRODUCTS *                                 
*                                                                               
GEN240   LA    R4,SVTBPRD                                                       
         BAS   RE,GNSVPR                                                        
         LA    R4,SVTBPRD2                                                      
         CLI   0(R4),0                                                          
         BE    *+8                                                              
         BAS   RE,GNSVPR                                                        
*                                                                               
         OC    SVTBNXLN(4),SVTBNXLN  TEST MORE ENTRIES                          
         BZ    GEN250                                                           
         CLC   SVTBMKST,SVTBNXMS     NEXT ENTRY SAME MARKET STA                 
         BNE   GEN250                                                           
         CLI   BPRD,X'FF'            TEST POL                                   
         BE    *+14                  YES                                        
         CLC   SVTBPRD,SVTBNXPR      NO-TEST SAME PRD                           
         BNE   GEN250                NO - STOP                                  
         LA    R7,L'SVTBDATA(R7)                                                
         B     GEN240                                                           
*                                                                               
GEN250   LA    R7,L'SVTBDATA(R7)   POINT TO NEXT ENTRY                          
*                                                                               
         L     R0,SVTBLINE         SAVE LINE ADDRESS                            
         XC    SVTBLINE,SVTBLINE   SET EOT FLAG                                 
*                                                                               
* READ PATTERN RECORDS *                                                        
*                                                                               
         MVI   ERROPT,C'Y'         SET TO RETURN ERRORS                         
         GOTO1 VGETPTNS                                                         
         ST    R0,SVTBLINE                                                      
         BAS   RE,ERRTEST                                                       
*                                                                               
* PRINT INSTRUCTIONS *                                                          
*                                                                               
         MVI   MLRYORN,C'N'                                                     
         CLC   =C'MLR',CONOUT                                                   
         BNE   *+8                                                              
         MVI   MLRYORN,C'Y'                                                     
*                                                                               
         MVC   SHIPYORN,TRASHIP    MOVE SHIPPING OPTION                         
*                                                                               
         MVI   ERROPT,C'Y'         SET TO RETURN ERRORS                         
         L     R0,SVTBLINE         SAVE LINE ADDRESS                            
         XC    SVTBLINE,SVTBLINE   SET EOT FLAG                                 
         GOTO1 VPRT,DMCB,(RC)                                                   
         ST    R0,SVTBLINE         RESTORE                                      
*                                                                               
         BAS   RE,ERRTEST                                                       
*                                                                               
         CLC   SVXFRMOD,=C'SUB'    TEST XFRCTL SUBMIT                           
         JNE   GEN260                                                           
* R7 POINTS TO NEXT SVTABLE ENTRY                                               
         OC    0(4,R7),0(R7)       TEST MORE DATA                               
         JNZ   GEN224              YES - GO PROCESS                             
*                                                                               
         LA    R4,ELEM             WHEN NO MORE, POINT R4 ANYWHERE!             
         J     GEN262                                                           
         EJECT                                                                  
* DISPLAY SPOOL ID FOR USER *                                                   
*                                                                               
GEN260   L     R7,ASVTABLE         POINT BACK TO START OF TABLE                 
         L     R4,SVTBLINE         POINT SELECT FIELD DSPL                      
         A     R4,ATWA             RELOCATE                                     
         ZIC   R0,0(R4)                                                         
         AR    R4,R0               POINT TO DISPLAY FIELD                       
         OI    6(R4),X'80'         SET XMT                                      
         LA    R4,DSPMKT-DSPLINED(R4)                                           
         MVC   0(17,R4),SPACES     BLANK MARKET NAME                            
*                                                                               
GEN262   TM    SVTBIND2,SVTBICGR   THIS A CABLE STATION WITH GRP CDE            
         BZ    GEN266                                                           
*                                                                               
         CLI   INSPRTSW,C'N'       DID WE PRINT INST                            
         BE    GEN268              NO                                           
*                                                                               
         TM    SVOPT,OPTTEST       TEST OPTION TEST                             
         BO    GEN268              YES - NEVER DO AMS/GEN                       
*                                                                               
         CLI   SVAMSOPT,C'Y'       TEST TO OVERRIDE PROFILE                     
         BE    GEN264                                                           
*                                                                               
         CLI   SVAMSOPT,C'N'       TEST DO NOT DO AMS/GEN                       
         BE    GEN268                                                           
*                                                                               
         CLI   SVT2PR16,C'Y'       ELSE TEST AMS PROFILE SETTING                
         BNE   GEN268                                                           
*                                                                               
GEN264   MVI   SVAMSAUT,C'Y'                                                    
         B     GEN268                                                           
*                                                                               
GEN266   CLI   RECNUM,47           TWX INSTR                                    
         BE    GEN270                                                           
         CLI   SVT1PR9,C'Y'        COVER LETTER INSTR                           
         BNE   GEN270                                                           
*                                                                               
GEN268   MVC   0(9,R4),=C'PROCESSED'                                            
         B     GEN274                                                           
*                                                                               
GEN270   MVC   0(3,R4),=C'ID='                                                  
         MVC   3(3,R4),REMUSER                                                  
*                                                                               
         CLI   SVTWPR1,C'N'        FAXING THIS                                  
         BE    GEN272               NO                                          
         CLI   SVTWPR1,C'C'        COPIES ONLY                                  
         BE    GEN272               YES                                         
         MVC   3(3,R4),=C'AWX'                                                  
*                                                                               
GEN272   CLI   RECNUM,47           TWX INSTR                                    
         BNE   *+10                                                             
         MVC   3(3,R4),=C'TWX'                                                  
*                                                                               
         MVI   6(R4),C','                                                       
         SR    R0,R0                                                            
         ICM   R0,3,SPOOLRPN                                                    
         EDIT  (R0),(5,7(R4)),ALIGN=LEFT                                        
*                                                                               
GEN274   CLI   INSPRTSW,C'N'       DID WE GENERATE INSTRUCTIONS                 
         BNE   GEN276                                                           
         MVC   0(17,R4),=CL17'NO DATA GENERATED'                                
*                                                                               
* MODIFY SELECT FIELD *                                                         
*                                                                               
GEN276   L     RE,ASVTABLE                                                      
         CLI   SVTBLINE-SVTBDATA(RE),X'FF'                                      
         JE    GEN310                                                           
         L     RE,SVTBLINE-SVTBDATA(RE)  GET REL LINE DSPL                      
         A     RE,ATWA                   GET ACTUAL ADDRESS                     
         OI    1(RE),X'20'                                                      
         XC    8(L'TRASEL1,RE),8(RE)                                            
         MVI   8(RE),C'*'                                                       
         OI    6(RE),X'80'                                                      
         EJECT                                                                  
* LOOK FOR MORE SELECTED ENTRIES *                                              
*                                                                               
GEN300   ICM   R2,15,SVTBLINE      START AT NEXT LINE                           
         BZ    GEN310                                                           
         A     R2,ATWA                                                          
*                                                                               
GEN301   TM    1(R2),X'20'                                                      
         BO    GEN308                                                           
*                                                                               
         CLI   5(R2),0                                                          
         BNE   GEN302                                                           
*                                                                               
         TM    SVOPT3,OPTSALL                                                   
         BO    GEN302                                                           
*                                                                               
         CLI   SVTWPR1,C'2'        THIS FAX AND COPY REQUEST                    
         BNE   GEN302                                                           
         TM    WHEN,X'20'          THIS A SOON RUN                              
         BO    GEN302                                                           
*                                                                               
         GOTO1 VPCOPY              PRINT COPY OF FAXED INSTRUCTIONS             
*                                                                               
GEN302   CLC   8(2,R2),SPACES                                                   
         BH    *+12                                                             
         TM    SVOPT3,OPTSALL                                                   
         BO    GEN222                                                           
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
GEN304   CLI   8(R2),C'S'                                                       
         BNE   GEN306                                                           
*                                                                               
         CLI   9(R2),C'+'          GLOBAL SELECT                                
         BE    *+12                                                             
         NI    SVOPT3,X'FF'-OPTSALL                                             
         B     GEN222                                                           
*                                                                               
         OI    SVOPT3,OPTSALL      TURN ON SELECT ALL                           
         B     GEN222              GO GENERATE INSTRUCTIONS                     
*                                                                               
GEN306   CLI   8(R2),C' '                                                       
         BNE   GEN308                                                           
*                                                                               
         CLI   9(R2),C'S'                                                       
         BNE   *+12                                                             
         NI    SVOPT3,X'FF'-OPTSALL                                             
         B     GEN222              GO GENERATE INSTRUCTIONS                     
*                                                                               
         CLI   9(R2),C' '                                                       
         BNE   *+12                                                             
         TM    SVOPT3,OPTSALL      SELECT ALL?                                  
         BO    GEN222                                                           
*                                                                               
GEN308   LA    R2,NEXTLINE(R2)                                                  
         CLI   0(R2),9                                                          
         BH    GEN301                                                           
*                                                                               
         NI    SVOPT3,X'FF'-OPTSALL                                             
*                                                                               
* ALL DATA PROCESSED *                                                          
*                                                                               
GEN310   CLI   SVTWPR1,C'2'        THIS FAXING INSTRUCTIONS                     
         BNE   GEN312                                                           
*                                                                               
         GOTO1 VPCOPY              PRINT COPY OF FAXED INSTRUCTIONS             
*                                                                               
GEN312   MVC   CONHEAD(29),=C'HIT ENTER TO CONTINUE DISPLAY'                    
         CLI   SVAMSAUT,C'Y'       TEST NEED AMS/GEN                            
         BNE   GEN314              NO                                           
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    GEN314                                                           
         TM    WHEN,X'18'          TEST WHEN OR OVERNIGHT                       
         BNZ   GEN314                                                           
         MVC   TRACOPY+12(36),=C'* AMS/GEN PENDING - HIT PF2 TO RUN *'          
         OI    TRACOPYH+6,X'80'    SET TO XMT                                   
         OI    TRACOPYH+1,X'08'    SET HIGH INTENSITY                           
*                                                                               
GEN314   OC    SVKEY,SVKEY                                                      
         BZ    GEN110                                                           
         B     ERRTEST5                                                         
*                                                                               
ERRTEST  MVI   ERROPT,0            RESET RETURN FLAG                            
         CLI   ERROR,0                                                          
         BER   RE                                                               
*                                                                               
         CLI   SVTWPR1,C'2'        THIS FAX AND COPY REQUEST                    
         BNE   ERRTEST2                                                         
         TM    WHEN,X'20'          THIS A SOON REQUEST                          
         BO    ERRTEST2                                                         
*                                                                               
         GOTO1 VPCOPY              PRINT COPY OF FAXED INSTRUCTIONS             
*                                                                               
ERRTEST2 DS    0H                                                               
         TM    WHEN,X'20'          THIS SOON REQUEST                            
         BZ    ERRTEST3             NO                                          
         CLI   OFFLINE,C'Y'        AND OFFLINE                                  
         BNE   ERRTEST3             NO                                          
                                                                                
* UNLOCK USING LOCKET *                                                         
                                                                                
         CLI   SVAMSAUT,C'Y'       TEST AMS GEN TO FOLLOW                       
         BE    ERRTEST3            IF YES, DO NOT UNLOCK NOW!                   
         CLI   SVAMSAUT,C'X'       TEST AMS REQ GENERATED                       
         BE    ERRTEST3            IF YES, DO NOT UNLOCK NOW!                   
         MVI   DUB,X'E4'                     U=UNLOCK                           
         MVC   DUB+1(7),=X'030A220A240A25' 03=3 ENTRIES                         
*                                       0A22, 0A24, 0A25 RECS                   
         GOTO1 VALILOC,0                                                        
*                                                                               
ERRTEST3 L     RF,ERREX                                                         
         CLI   ERROR,X'FF'                                                      
         BNE   *+8                                                              
ERRTEST5 L     RF,ERREX2                                                        
         BASR  RE,RF               NO RETURN HERE                               
         DC    H'0'                IN CASE IT COMES BACK                        
         EJECT                                                                  
*****************************************************                           
* SUBROUTINE TO READ AND FILTER ESTIMATE HEADERS ON *                           
* REQUESTED DATES.                                  *                           
*****************************************************                           
*                                                                               
BLDEST   NTR1                                                                   
*                                                                               
         XC    SVESTAB,SVESTAB     CLEAR SVESTAB                                
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD     A-M/CLT                                      
*                                                                               
         MVC   KEY+4(3),QPRD                                                    
*                                                                               
         MVC   KEY+7(1),QBEST                                                   
         CLI   KEY+7,0                                                          
         BNE   *+8                                                              
         MVI   KEY+7,1                                                          
*                                                                               
BLDEST10 MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA                       
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(7),KEYSAVE      SAME A-M/CLT/PRD                             
         BNE   BLDEST30                                                         
*                                                                               
         OC    KEY+8(5),KEY+8      TEST BILL                                    
         BNZ   BLDEST20                                                         
*                                                                               
         CLI   QBEST,0             ANY REQUESTED ESTIMATES                      
         BE    BLDEST14             NO                                          
         CLC   QBEST,KEY+7         IN REQUESTED EST RANGE                       
         BL    BLDEST20             NO                                          
         CLC   QBESTEND,KEY+7      IN REQUESTED EST RANGE                       
         BH    BLDEST20             NO                                          
*                                                                               
BLDEST14 L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'SPTFIL' SWITCH TO SPOT MEDIA                       
         GOTO1 GETREC                                                           
*                                                                               
         USING ESTHDRD,R6                                                       
*                                                                               
         CLC   ESTART,SVQEND       EST START AFTER REQ END                      
         BH    BLDEST20                                                         
         CLC   EEND,SVQSTART       EST END BEFORE REQ START                     
         BL    BLDEST20                                                         
*                                                                               
         ZIC   RE,KEY+7                                                         
         LA    RE,SVESTAB(RE)                                                   
         MVC   0(1,RE),ECOPY       SET COPY CODE IN TABLE                       
         CLI   0(RE),0                                                          
         BNE   *+12                                                             
         MVI   0(RE),X'FF'                                                      
         B     BLDEST20                                                         
*                                                                               
         CLI   SVPROF11,C'Y'      COPY CODE BY DAYPART                          
         BE    ESTCPYER                                                         
         CLI   SVPROF11,C'D'      COPY CODE BY DAYPART                          
         BE    ESTCPYER                                                         
*                                                                               
BLDEST20 MVC   KEY+8(5),HEXFF      FORCE NEXT EST                               
         CLI   QBEST,0             TEST NO ESTIMATE REQUEST                     
         BE    BLDEST10             YES - CONTINUE                              
         CLC   KEY+7(1),QBESTEND   TEST PAST END OF SERIES OR ONE               
         BL    BLDEST10             NO - CONTINUE                               
*                                                                               
* SET HI AND LOW EST NUMBERS FOR EST=NO *                                       
*                                                                               
BLDEST30 DS   0H                                                                
         XC    FILENAME,FILENAME                                                
*                                                                               
         OC    SVESTAB,SVESTAB     TEST ANY DATA FOUND                          
         BZ    BLDEST40             NO - SET ALL ACTIVE                         
*                                                                               
         MVC   BEST(2),QBEST       SET ACTUAL ESTIMATE NUMBERS                  
*                                                                               
         CLI   QBEST,0             TEST EST=NO REQUEST                          
         BNE   EQXIT                NO, SPECIFIC EST                            
*                                                                               
         LA    RE,SVESTAB                                                       
         CLI   0(RE),0                                                          
         BNE   *+12                                                             
         LA    RE,1(,RE)                                                        
         B     *-12                                                             
*                                                                               
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
*                                                                               
         STC   R0,BESTEND                                                       
*                                                                               
         B     EQXIT                                                            
*                                                                               
BLDEST40 CLI   SVTBPR04,C'Y'       TBUY ACCEPT NO ESTIMATES                     
         BNE   NEQXIT                                                           
*                                                                               
         MVI   SVESTAB,X'FF'       SET ALL ESTIMATES ACTIVE                     
         MVC   SVESTAB+1(255),SVESTAB                                           
         MVI   BEST,1                                                           
         MVI   BESTEND,255                                                      
         B     EQXIT                                                            
         EJECT                                                                  
* SEE PRODUCT IS EQUIVALENT *                                                   
*                                                                               
PEQ      NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PEQKEY,R6                                                        
         MVC   PEQPID,=X'0AB7'                                                  
         MVC   PEQPAM(3),BAGYMD                                                 
         MVC   PEQPEPRD,WORK                                                    
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      IS THIS AN EQUIVALENT PRODUCT                
         BNE   EXIT                 NO                                          
         MVC   GERROR,=Y(PRDSREQV)                                              
         MVC   ERRTEXT(3),WORK                                                  
         MVC   ERRTEXT+4(3),=C'AND'                                             
         MVC   ERRTEXT+8(3),PEQPBPRD                                            
         GOTO1 VTRAERR                                                          
         DROP  R6                                                               
         EJECT                                                                  
* VALIDATE PERIOD                                                               
*                                                                               
VPER     NTR1                                                                   
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         CLI   8(R2),C'?'          IF QUESTION MK, TELL MEL FLT DATES           
         BNE   VPER30                                                           
*                                                                               
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BE    VPER26                                                           
*                                                                               
         CLI   5(R2),1             SEE IF DATE ENTERED TOO                      
         BE    VPER10              NO                                           
*                                                                               
         GOTO1 DATVAL,DMCB,9(R2),SVQSTART                                       
         L     R4,DMCB             GET LENGTH OF FIELD                          
         MVI   ERROR,INVDATE                                                    
         LTR   R4,R4                                                            
         JZ    GENERRV                                                          
         GOTO1 DATCON,(R1),(0,SVQSTART),(2,SVTBSTR)                             
         B     VPER12                                                           
*                                                                               
VPER10   GOTO1 DATCON,DMCB,(5,0),(2,SVTBSTR)                                    
*                                                                               
VPER12   DS   0H                                                                
         GOTO1 DATCON,(R1),(2,SVTBSTR),(3,DUB)                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A27'                                                  
         MVC   KEY+2(4),BAGYMD     BCLT, BPRD (TRY PRD SPECIFIC REC)            
         MVI   ERROR,NOFLTREC                                                   
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER14                                                           
         MVC   KEY,KEYSAVE         RESTORE ORIGINAL KEY                         
         MVI   KEY+5,0             CLEAR PRD TO TRY FOR CLT DATA                
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(6),KEYSAVE                                                   
         JNE   GENERRV                                                          
VPER14   CLC   DUB(3),KEY+6        FIRST TLCST DATE TO RECORD END DATE          
         BNH   VPER16                                                           
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 SEQ                                                              
         CLC   KEY(6),KEYSAVE                                                   
         BE    VPER14                                                           
         MVC   KEY,KEYSAVE         GET LAST DATE BEFORE TODAY                   
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
*                                                                               
         MVC   DUB(1),KEY+6        CHANGE REQ DATE TO YEAR OF LAST FLT          
*                                                                               
VPER16   MVC   GERROR,=Y(FLTERR1)     *END=                                     
         GOTO1 DATCON,DMCB,(3,KEY+6),(5,ERRTEXT)                                
*                                                                               
**NOP    LA    R4,4                                                             
         LA    R4,2                ERRTEXT SUPPORTS 36 CHAR                     
         LA    R5,ERRTEXT+14                                                    
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
VPER20   BRAS  RE,NEXTEL                                                        
         BNE   VPER24                                                           
*                                                                               
         USING FLTDTAEL,R6                                                      
         CLC   DUB(3),FLTEND                                                    
         BNH   VPER22                                                           
         CLC   DUB(3),FLTSTART                                                  
         BH    VPER20                                                           
*                                                                               
VPER22   GOTO1 DATCON,DMCB,(3,FLTSTART),(4,0(R5))                               
         MVI   5(R5),C'-'                                                       
         GOTO1 (RF),(R1),(3,FLTEND),(4,6(R5))                                   
         LA    R5,11(,R5)                                                       
         BCT   R4,VPER20                                                        
*                                                                               
VPER24   GOTO1 VTRAERR                                                          
*                                                                               
VPER26   MVC   GERROR,=Y(FLTERR2)  EST DATES --                                 
         LA    R5,ERRTEXT                                                       
         GOTO1 DATCON,DMCB,(2,SVFLTST),(5,(R5))                                 
         LA    R5,ERRTEXT+12                                                    
         GOTO1 (RF),(R1),(2,SVFLTEND),(4,(R5))                                  
         GOTO1 VTRAERR                                                          
*                                                                               
VPER30   CLI   5(R2),8             TEST ONE DATE ENTERED                        
         BH    VPER34               NO, 2 DATES                                 
*                                                                               
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   VPER32                                                           
         CLC   =C'ES',8(R2)        USE EST DATES                                
         BNE   VPER32                                                           
         MVC   SVTBDTS,SVFLTST                                                  
         GOTO1 DATCON,DMCB,(2,SVFLTST),(5,TRAPER)                               
         MVI   TRAPER+8,C'-'                                                    
         GOTO1 (RF),(R1),(2,SVFLTEND),(5,TRAPER+9)                              
         GOTO1 (RF),(R1),(2,SVFLTST),SVQSTART                                   
         GOTO1 (RF),(R1),(2,SVFLTEND),SVQEND                                    
         MVC   SVGENDTS,SVFLTDTS                                                
         OI    TRAPERH+6,X'80'                                                  
         MVI   TRAPERH+5,17        RESET LENGTH                                 
         B     VPERX                                                            
*                                                                               
* ACCEPT ONE DATE AS FLIGHT START DATE *                                        
*                                                                               
VPER32   DS    0H                                                               
         GOTO1 DATVAL,DMCB,8(R2),SVQSTART                                       
         L     R4,DMCB                                                          
         MVI   ERROR,INVDATE                                                    
         LTR   R4,R4                                                            
         JZ    GENERRV                                                          
                                                                                
         GOTO1 VALIDATE,DMCB,SVQSTART                                           
         GOTO1 DATCON,(R1),SVQSTART,(2,SVTBSTR)                                 
         XC    SVTBEND,SVTBEND                                                  
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BE    VPER40                                                           
         GOTO1 VCHKFLT             FILLS IN SVQEND/SVTBEND                      
         B     VPER50                                                           
*                                                                               
* VALIDATE TWO DATES *                                                          
*                                                                               
VPER34   DS    0H                                                               
         GOTO1 VALPER,DMCB,SVQSTART                                             
*                                                                               
         GOTO1 DATCON,DMCB,SVQSTART,(2,SVTBSTR)                                 
         GOTO1 (RF),(R1),SVQEND,(2,SVTBEND)                                     
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BE    VPER40                                                           
*                                                                               
         GOTO1 VCHKFLT                                                          
*                                                                               
         MVI   ERROR,NOTFLTDT                                                   
         CLC   SVTBEND,SVGENEND                                                 
         JNE   GENERRV                                                          
         B     VPER50                                                           
*                                                                               
* PERIOD DATES SHOULD FALL ENTIRELY WITHIN THIS ESTIMATE *                      
*                                                                               
VPER40   CLC   SVTBSTR,SVFLTEND    PER START AFTER EST END                      
         BH    ESTDTERR                                                         
         CLC   SVTBSTR,SVFLTST     PER START BEFORE EST STR                     
         BL    ESTDTERR                                                         
*                                                                               
         OC    SVTBEND,SVTBEND     ANY END DATE ENTERED                         
         BNZ   VPER44                                                           
         MVC   SVTBEND,SVFLTEND    USE EST END DATE                             
         B     VPER46                                                           
*                                                                               
* BOTH DATES GIVEN, END MUST MATCH ESTIMATE END *                               
*                                                                               
VPER44   CLC   SVTBEND,SVFLTEND    LAST TLCST MUST BE EST END                   
         BNE   ESTEDTER                                                         
*                                                                               
VPER46   GOTO1 DATCON,DMCB,(2,SVFLTST),SVQSTART                                 
         GOTO1 (RF),(R1),(2,SVFLTEND),SVQEND                                    
         MVC   SVGENDTS,SVTBSTR                                                 
*                                                                               
VPER50   GOTO1 DATCON,DMCB,(2,SVTBSTR),(5,TRAPER)                               
         MVI   TRAPER+8,C'-'                                                    
         GOTO1 (RF),(R1),(2,SVTBEND),(5,TRAPER+9)                               
         OI    TRAPERH+6,X'80'                                                  
         MVI   TRAPERH+5,17        RESET LENGTH                                 
*                                                                               
VPERX    CLC   AGENCY,=C'FC'       ONLY FOOTE CONE                              
         BNE   EXIT                                                             
         CLC   SVGENEND,=X'5B061E'                                              
         BNH   EXIT                                                             
         MVI   ERROR,INVACT                                                     
         LA    R2,CONRECH                                                       
         J     GENERRV                                                          
*                                                                               
ESTEDTER MVC   GERROR,=Y(ESTDTER1) END DATE NOT EST END                         
         GOTO1 VTRAERR                                                          
*                                                                               
ESTDTERR MVC   GERROR,=Y(ESTDTER2) DATES NOT IN EST PERIOD                      
         GOTO1 VTRAERR                                                          
         EJECT                                                                  
*                                                                               
ESTCPYER MVC   GERROR,=Y(ESTCCERR)                                              
         LLC   R0,KEY+7                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ERRTEXT(3),DUB                                                   
         GOTO1 VTRAERR                                                          
*                                                                               
SPCPRDER MVC   GERROR,=Y(POLNOTOK)   MUST RUN FOR ONE PRD                       
         GOTO1 VTRAERR                                                          
*                                                                               
AUTPIGER MVC   GERROR,=Y(NOAUTPIG)   CANNOT RUN AUTO P/B SOON                   
         GOTO1 VTRAERR                                                          
*                                                                               
CCESTER  MVC   GERROR,=Y(ONECCEST)   REQUEST ONLY ONE EST FOR CC=EST            
         GOTO1 VTRAERR                                                          
         EJECT                                                                  
CLRCLT   NI    TRACLTH+4,X'DF'                                                  
*                                                                               
         MVI   AUTOQSW,0                                                        
         MVI   UPDSW,0                                                          
*                                                                               
         L     R4,ASVPRDS                                                       
         L     R5,APTNSTRT                                                      
         SR    R5,R4                                                            
         LA    R0,256                                                           
*                                                                               
CLRCLT2  CR    R5,R0                                                            
         BL    CLRCLT4                                                          
         XC    0(256,R4),0(R4)                                                  
         AR    R4,R0                                                            
         SR    R5,R0                                                            
         BZ    CLRPRD                                                           
         B     CLRCLT2                                                          
CLRCLTXC XC    0(0,R4),0(R4) *EXECUTED*                                         
CLRCLT4  BCTR  R5,0                                                             
         EX    R5,CLRCLTXC                                                      
*                                                                               
CLRPRD   NI    TRAPRDH+4,X'DF'                                                  
CLREST   NI    TRAESTH+4,X'DF'                                                  
CLRPER   NI    TRAPERH+4,X'DF'                                                  
*                                                                               
* CLEAR SVTABLE *                                                               
*                                                                               
CLROPT   L     R7,ASVTABLE                                                      
         LA    R0,16               NUMBER OF ENTRIES                            
         XC    0(L'SVTBDATA,R7),0(R7)                                           
         LA    R7,L'SVTBDATA(R7)                                                
         BCT   R0,*-10                                                          
         L     R7,ASVTABLE         POINT TO START OF TABLE                      
*                                                                               
         XC    SVKEY,SVKEY         CLEAR CONTINUATION KEY                       
*                                                                               
* CLEAR DISPLAY AREA OF SCREEN *                                                
*                                                                               
CLRSCR   LA    R4,TRASEL1H                                                      
*                                                                               
CLRSCR2  OI    1(R4),X'20'         SET PROT IN SELECT FIELD                     
         XC    8(L'TRASEL1,R4),8(R4)       CLEAR SELECT FIELD                   
         OI    6(R4),X'80'         AND XMT                                      
*                                                                               
         ZIC   R0,0(R4)                                                         
         AR    R4,R0                                                            
         OC    8(L'TRADSP1,R4),8(R4)                                            
         BZ    CLRSCR4                                                          
         CLC   8(L'TRADSP1,R4),SPACES                                           
         BE    CLRSCR4                                                          
         XC    8(L'TRADSP1,R4),8(R4)                                            
         OI    6(R4),X'80'                                                      
*                                                                               
CLRSCR4  IC    R0,0(R4)                                                         
         AR    R4,R0                                                            
         CLI   0(R4),9                                                          
         BH    CLRSCR2                                                          
         OC    TRACOPY,TRACOPY                                                  
         BZR   RE                                                               
         XC    TRACOPY,TRACOPY                                                  
         OI    TRACOPYH+6,X'80'                                                 
         BR    RE                                                               
         EJECT                                                                  
**************************************************************                  
* SUBROUTINE TO FIND NEXT ACTIVE PRODUCT FOR PRD=ALL REQUEST *                  
**************************************************************                  
*                                                                               
NEXTPRD  NTR1                                                                   
*                                                                               
NXTPRD10 L     R1,ASVCLIST                                                      
         CLI   BPRD,0              TEST FIRST TIME                              
         BE    NXTPRD60                                                         
*                                                                               
NXTPRD20 CLC   BPRD,3(R1)          FIND LAST PRD PROCESSED                      
         BE    NXTPRD40                                                         
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BH    NXTPRD20                                                         
         DC    H'0'                                                             
*                                                                               
NXTPRD40 LA    R1,4(R1)            POINT TO NEXT PRD                            
*                                                                               
NXTPRD60 CLC   =C'AAA',0(R1)                                                    
         BE    NXTPRD40                                                         
         CLC   =C'POL',0(R1)                                                    
         BE    NXTPRD40                                                         
         CLI   0(R1),C' '                                                       
         BH    NXTPRD64                                                         
*        BL    NEQXIT                                                           
         MVI   BPRD,0                                                           
         B     NEQXIT                                                           
*                                                                               
NXTPRD64 DS    0H                                                               
         CLI   SVPROF13,C'Y'       THIS A PROD EQUIVALENCE CLIENT               
         BNE   NXTPRD70             NO                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PEQKEY,R4                                                        
         MVC   PEQPID,=X'0AB7'                                                  
         MVC   PEQPAM(3),BAGYMD                                                 
         MVC   PEQPEPRD,0(R1)                                                   
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      THIS AN EQUIVALENT PRODUCT                   
         BE    NXTPRD40             YES, BYPASS                                 
*                                                                               
NXTPRD70 MVC   QPRD,0(R1)          SET AS REQUESTED PRODUCT                     
         MVC   BPRD,3(R1)                                                       
*                                                                               
         BAS   RE,GETPRDNM         GET PRODUCT NAME                             
         BNE   NXTPRD40                                                         
*                                                                               
         BAS   RE,BLDEST                                                        
         BNZ   NXTPRD10                                                         
         B     EQXIT                                                            
         DROP  R4                                                               
         EJECT                                                                  
* SUBROUTINE TO READ PRODUCT HEADER AND EXTRACT PRD NAME *                      
*                                                                               
GETPRDNM NTR1                                                                   
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+4(3),QPRD                                                    
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA                       
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NEQXIT                                                           
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'SPTFIL' SWITCH TO SPOT MEDIA                       
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
*                                                                               
         USING PRDHDRD,R6                                                       
*                                                                               
         MVC   PRDNM,PNAME                                                      
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*******************************************                                     
* SUBROUTINE TO READ PRODUCT HEADERS AND  *                                     
*      SAVE NAMES IN BUFFER               *                                     
*******************************************                                     
*                                                                               
GNSVPR   NTR1                                                                   
         L     R5,ASVPRDS                                                       
         USING SVPRDD,R5                                                        
*                                                                               
GNSVPR2  CLI   SVPRDCD,0                                                        
         BE    GNSVPR4                                                          
         CLC   SVPRDCD,0(R4)       MATCH                                        
         BE    EXIT                                                             
         LA    R5,L'SVPRDDTA(R5)                                                
         B     GNSVPR2                                                          
*                                                                               
* READ PRDHDR *                                                                 
*                                                                               
GNSVPR4  XC    KEY,KEY                                                          
*                                                                               
*                                                                               
         MVC   KEY+1(3),BAGYMD     A-M/CLT                                      
*                                                                               
         MVC   BYTE,0(R4)                                                       
         BAS   RE,GNGETPRD                                                      
         MVC   KEY+4(3),0(R1)                                                   
*                                                                               
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'SPTFIL' SWITCH TO SPOT MEDIA                       
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING PRDHDRD,R6                                                       
*                                                                               
         MVC   SVPRDCD,0(R4)                                                    
         MVC   SVPRDEBC,KEY+4                                                   
         MVC   SVPRDNM,PNAME                                                    
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         B     EXIT                                                             
*                                                                               
GNGETPRD L     R1,ASVCLIST                                                      
*                                                                               
GNGETPR2 CLC   BYTE,3(R1)                                                       
         BER   RE                                                               
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BH    GNGETPR2                                                         
         DC    H'0'                                                             
HEXFF    DC    6X'FF'                                                           
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB,RA,RC                                                         
         EJECT                                                                  
* VALIDATE OPTIONS                                                              
*                                                                               
* VALID OPTIONS ARE                                                             
*        SVOPT                                                                  
*        OPTTEST       'TEST'  (X80)                                            
*           RERUN      'RERUN' (X40)                                            
*           REV        'REV'   (X20) (REVISIONS ONLY)                           
*           NEW        'NEW'   (X10) (ONLY STA WITH NO INST)                    
*           DEL        'DEL'   (X08) (DELETE CMML ONLY)                         
*           1MKT               (X02) (ONE MKT REQUEST)                          
*           1STA               (X01) (ONE STA REQUEST)                          
*        SVOPT2                                                                 
*        OP2NOADR    'NOADDR'  (X80) IGNORE MISSING STA ADDR                    
*           TRBUY    'TRAFFIC' (X40) USE TRAFFIC BUYS, NOT SPOT BUYS            
*           SPCMT    'CMT'     (X20) USE SVPROF16 SPEC CMT, NOT 12              
*           TBA      'TBA      (X10) PRINT SPOTS WITHOUT PTTN AS TBA            
*           ISACT              (X08) INTERNAL BLDACT RTN SWITCH -               
*                                    THERE WERE BUYS FOUND                      
*           NOAGY    'NOAGY'   (X04) DON'T PRINT AGENCY NAME/ADDRESS            
*           TCOPY    'FOLOW'   (X02) FOLLOW UP OF TWX INSTRUCTION SENT          
*           PRT      'PRT'     (X01) PRINT INSTR (OVERRIDE SVT1PR9)             
*                    'NOPRT'         DON'T PRINT INSTRUCTIONS                   
*        SVOPT3                                                                 
*        OPTAPAT       'APAT'  (X80) (BYPASS AUTO PB PATTERN ERRORS)            
*                                                                               
         USING GEND,RC                                                          
VOPT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   SVAMSOPT,C'N'       CLEAR !                                      
         CLI   SVT2PR16,C'Y'       DEFAULT TO AUTO/AMS                          
         JNE   *+8                                                              
         MVI   SVAMSOPT,C'Y'                                                    
*                                                                               
         CLI   8(R2),C'?'         QUESTION MARK HELP                            
         BE    VOPTHELP                                                         
         XC    BMKT(5),BMKT        CLEAR BMKT/BSTA                              
         XC    BLOCK+64(256),BLOCK+64                                           
         GOTO1 SCANNER,DMCB,(R2),(7,BLOCK+64)                                   
         MVI   SVOPT,0                                                          
         MVI   SVOPT2,0                                                         
         XC    SVOPTDTA,SVOPTDTA                                                
         LA    R4,BLOCK+64                                                      
         MVC   BYTE,DMCB+4         SAVE FIELD COUNT                             
*                                                                               
VOPT10   CLI   0(R4),0             TEST FOR MORE DATA                           
         BNE   VOPT20                                                           
         LA    R0,BLOCK+64                                                      
         CR    R0,R4                                                            
         BE    *+12                                                             
         CLI   BYTE,0                                                           
         BNE   EXTCOMER                                                         
*                                                                               
         TM    SOXSW,SOXOKFLG      IF DDS, AND FACTEST, OKAY TO GO              
         BO    VOPT16                                                           
*                                                                               
         TM    SOXSW,SOXERFLG      IF RD ONLY/RD ONLY MODE/WRONG ADV            
         BZ    VOPT16                                                           
*                                                                               
         TM    SVOPT,OPTTEST                                                    
         BO    VOPT16                                                           
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
VOPT16   DS    0H                                                               
         CLI   SVPROF16,0          ANY SPECIAL COMMENT REQUIRED                 
         BE    VOPT18               NO                                          
         CLI   SVPROF16,C'0'       ANY SPECIAL COMMENT REQUIRED                 
         BE    VOPT18               NO                                          
         CLI   SVPROF16,C'N'       ANY SPECIAL COMMENT REQUIRED                 
         BE    VOPT18               NO                                          
         TM    SVOPT2,OP2SPCMT     WAS CMT= ENTERED                             
         BZ    CMTENTER             NO                                          
*                                                                               
VOPT18   CLI   TRAMED,C'N'                                                      
         BNE   VOPTX                                                            
*                                                                               
         TM    SVOPT3,OPT3MKL      MARKET LIST NAME ENTERED?                    
         BO    VOPTX                YES, DONE                                   
*                                                                               
         OC    OPTMGRP,OPTMGRP     MGROUP REQ                                   
         BNZ   VOPTX                YES, DONE                                   
*                                                                               
         OC    BMKTSTA,BMKTSTA     WAS MARKET STATION REQ?                      
         BNZ   VOPTX                                                            
*                                                                               
         TM    SVOPT,OPT1MKT       MKT REQ ?                                    
         BO    VOPTX                YES , DONE                                  
*                                                                               
         TM    SVOPT,OPT1STA                                                    
         BO    VOPTX                YES , DONE                                  
*                                                                               
* PROCESS MKT=0000                                                              
         MVC   0(4,R4),=X'030440A0' FAKE SCANNER RETURNED VALUES                
         MVC   22(4,R4),=C'0000'    PROCESS MARKET 0000                         
         MVI   BYTE,X'FF'                                                       
         B     OPTMKT                                                           
*                                                                               
VOPTX    XIT1                                                                   
*                                                                               
VOPT20   LA    R0,OPTABLEL                                                      
         LA    R1,OPTABLE                                                       
         ZIC   RE,0(R4)                                                         
         BCTR  RE,0                                                             
VOPT24   EX    RE,VOPTCLC                                                       
         BE    VOPT26                                                           
         LA    R1,12(,R1)                                                       
         BCT   R0,VOPT24                                                        
         B     VOPT90                                                           
VOPT26   SR    RF,RF                                                            
         ICM   RF,3,10(R1)                                                      
         AR    RF,RB                                                            
         BR    RF                                                               
VOPTCLC  CLC   0(0,R1),12(R4)                                                   
*                                                                               
* OPTION AFILIATE - ONLY STATIONS WITH THIS AFFILIATE *                         
*                                                                               
OPTAFIL  MVC   SVOPTAFF,22(R4)                                                  
         B     VOPT80                                                           
*                                                                               
* OPTION TO OVERRIDE AMS PROFILE SETTING                                        
*                                                                               
OPTAMS   CLI   22(R4),C'Y'                                                      
         BNE   OPTAMS2                                                          
         MVI   SVAMSOPT,C'Y'                                                    
         B     VOPT80                                                           
OPTAMS2  CLI   22(R4),C'N'                                                      
         BNE   AMSERR                                                           
         MVI   SVAMSOPT,C'N'                                                    
         B     VOPT80                                                           
*                                                                               
* OPTION BPAT - READ BPAT RECORD ONLY (MEDIACOM FEATURE) *                      
*                                                                               
OPTBPTN  OI    SVOPT3,OPTBPAT                                                   
         MVI   SVT1PR9,C'Y'        SET PROFILE TO NOPRT                         
         MVI   SVT1PR12,C'Y'      SET DON'T PRINT PATTERN ROTATION              
         B     VOPT80                                                           
*                                                                               
* OPTION APAT - BYPASS ANY AUTO PIGGYBACK PATTERN ERRORS *                      
*                                                                               
OPTAPTN  OI    SVOPT3,OPTAPAT                                                   
         B     VOPT80                                                           
*                                                                               
* OPTION MGROUP - FILTER BY MARKET GROUP                                        
*                                                                               
OPTMGR   DS    0H                                                               
*                                                                               
         TM    SVOPT3,OPT3MKL      MARKET LIST NAME ENTERED?                    
         BO    MKTMKLER             YES, ERROR                                  
*                                                                               
         OC    OPTMGRP,OPTMGRP     CHECK FOR DUPLICATE MGROUP REQ               
         BNZ   DUPMGERR            DUPLICATE MGROUP ERR                         
*                                                                               
         CLI   SVT2PR01,C'*'       MORE THAN ONE MARKET GROUP                   
         BNE   OPTMGR03             NO                                          
         CLI   SVT2PR14,X'FF'      CONVERTED MKT GROUP = '*'                    
         BE    OPTMGR3F            YES, MKT GROUP 'BJ' (SPMGRTAB)               
*                                                                               
         CLI   22(R4),C'A'                                                      
         BL    OPTMGRER                                                         
         CLI   22(R4),C'Z'                                                      
         BH    OPTMGRER                                                         
*                                                                               
         CLI   23(R4),C'A'          SEE IF 2 CHAR MKT GROUP                     
         BL    OPTMGR02                                                         
         CLI   23(R4),C'Z'                                                      
         BH    OPTMGR02                                                         
*                                                                               
* CONVERT 2 CHAR MARKET GROUP TO 1 CHAR                                         
*                                                                               
         L     RE,=A(SPMGRTAB)                                                  
         A     RE,SPTR07RR                                                      
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
*                                                                               
OPTMGR1  CLC   22(2,R4),0(RE)      IS THIS IT                                   
         BE    OPTMGR1C                                                         
         LA    RE,3(RE)                                                         
         BCT   RF,OPTMGR1                                                       
*                                                                               
         B     OPTMGRER                                                         
*                                                                               
OPTMGR1C MVC   SVT2PR01,2(RE)      MOVE HEX VALUE FROM TABLE                    
         B     *+10                                                             
OPTMGR02 MVC   SVT2PR01,22(R4)     MARKET GROUP                                 
*                                                                               
         CLI   1(R4),1                                                          
         BL    MGRENTER                                                         
         CLI   1(R4),6                                                          
         BH    MGRENTER                                                         
         ZIC   RF,1(R4)                                                         
         BCTR  RF,0                                                             
         LA    R1,23(,R4)                                                       
         CLI   23(R4),C'Z'                                                      
         BH    OPTMGR05                                                         
         LA    R1,1(R1)            ITS 2 CHAR MKT GROUP BUMP 1                  
         BCTR  RF,0                                                             
         B     OPTMGR05                                                         
*                                                                               
OPTMGR03 CLI   SVT2PR14,X'FF'      CONVERTED 2 CHAR MKT GROUP CODE              
         BE    OPTMGR3F                                                         
*                                                                               
         CLI   SVT2PR01,C'A'                                                    
         BL    OPTMGRER                                                         
         CLI   SVT2PR01,C'Z'                                                    
         BH    OPTMGRER                                                         
*                                                                               
OPTMGR3F CLI   1(R4),1                                                          
         BL    MGRENTER                                                         
         CLI   1(R4),4                                                          
         BH    MGRENTER                                                         
         ZIC   RF,1(R4)                                                         
         LA    R1,22(,R4)                                                       
         CLI   23(R4),C'Z'         SEE IF 2 CHAR MKT GROUP                      
         BH    OPTMGR05                                                         
         LA    R1,1(R1)            BUMP IN 2 CHAR MKT GROUP                     
         BCTR  RF,0                                                             
*                                                                               
OPTMGR05 LA    RE,DUB                                                           
         MVI   DUB,C'0'                                                         
         MVC   DUB+1(4),DUB                                                     
OPTMGR10 CLI   0(R1),C'0'                                                       
         BL    MGRENTER                                                         
         CLI   0(R1),C'9'                                                       
         BH    MGRENTER                                                         
         MVC   0(1,RE),0(R1)                                                    
         LA    R1,1(,R1)                                                        
         LA    RE,1(,RE)                                                        
         BCT   RF,OPTMGR10                                                      
*                                                                               
         PACK  WORK(3),DUB(5)                                                   
         MVC   OPTMGRP(2),WORK                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(3),BAGYMD    & BCLT                                        
         MVC   KEY+8(1),SVT2PR01                                                
         MVC   KEY+9(2),OPTMGRP                                                 
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA                       
         GOTO1 HIGH                                                             
         OC    KEY+5(3),KEY+5      FIND PRODUCT GROUP                           
         BZ    OPTMGR30                                                         
         CLC   KEY(5),KEYSAVE                                                   
         BE    OPTMGR20                                                         
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLIENT                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE                                                   
         BNE   NOMGRPER                                                         
*                                                                               
OPTMGR20 CLC   KEY+8(1),SVT2PR01                                                
         BNE   OPTMGR24                                                         
         CLC   KEY+9(2),OPTMGRP                                                 
         BE    OPTMGR30                                                         
OPTMGR24 MVC   KEY+8(1),SVT2PR01                                                
         MVC   KEY+9(2),OPTMGRP                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
OPTMGR30 CLC   KEY(11),KEYSAVE     FIND A MARKET GROUP                          
         BE    OPTMGR40                                                         
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLIENT                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
OPTMGR40 CLC   KEY(11),KEYSAVE     FIND A MARKET GROUP                          
         BNE   NOMGRPER                                                         
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVC   BMKT,KEY+11                                                      
         B     VOPT80                                                           
*                                                                               
* RERUN - RECREATE ALL INSTRUCTIONS AS OF THIS DATE *                           
*                                                                               
OPTRERN  MVI   ERROR,BADKEYWD                                                   
         TM    SVOPT,X'38'                                                      
         JNZ   GENERRV                                                          
         TM    SVOPT3,OPT3REPT                                                  
         JNZ   GENERRV                                                          
         OI    SVOPT,OPTRERUN                                                   
         MVI   ERROR,NODATE        MUST SPECIFY ORIGINAL INST DATE              
         CLI   1(R4),0                                                          
         JE    GENERRV                                                          
         GOTO1 DATVAL,DMCB,22(R4),DUB                                           
         MVI   ERROR,INVDATE                                                    
         OC    0(4,R1),0(R1)                                                    
         JZ    GENERRV                                                          
         GOTO1 DATCON,DMCB,DUB,(2,SVOPTDTE)                                     
         B     VOPT80                                                           
*                                                                               
* REPRINT - REPRINT INSTRUCTIONS AS THEY WERE LAST PRINTED *                    
*           MAY HAVE OPTIONAL DATE                                              
*                                                                               
OPTREPT  MVI   ERROR,BADKEYWD                                                   
         TM    SVOPT,OPTRERUN+OPTREV+OPTNEW                                     
         JNZ   GENERRV                                                          
         MVI   SVT1PR9,C'N'                                                     
         OI    SVOPT3,OPT3REPT                                                  
         CLI   1(R4),0                                                          
         BE    VOPT80                                                           
         GOTO1 DATVAL,DMCB,22(R4),DUB                                           
         MVI   ERROR,INVDATE                                                    
         OC    0(4,R1),0(R1)                                                    
         JZ    GENERRV                                                          
         GOTO1 DATCON,DMCB,DUB,(2,SVOPTDTE)                                     
         B     VOPT80                                                           
*                                                                               
* OPTION NOROT - DO NOT PRINT PATTERN ROTATION *                                
*                                                                               
OPTNOROT MVI   SVT1PR12,C'Y'      SET DON'T PRINT PATTERN ROTATION              
         B     VOPT80                                                           
*                                                                               
* OPTION NO AGENCY - BYPASS PRINTING AGENCY NAME & ADDRESS *                    
*                                                                               
OPTNOAGY OI    SVOPT2,OP2NOAGY    SET DON'T PRINT AGENCY NAME/ADDRESS           
         B     VOPT80                                                           
*                                                                               
* OPTION NO PRINT - BYPASS PRINTING INSTRUCTIONS *                              
*                                                                               
OPTNOPRT CLI   RECNUM,47           TWX INSTR                                    
         BE    VOPT90               INVALID FOR TWX                             
*                                                                               
         MVI   SVT1PR9,C'Y'        MANUALLY SET PROFILE TO NOPRT                
         B     VOPT80                                                           
*                                                                               
* OPTION REV - ONLY PRINT REVISED INSTRUCTIONS *                                
*                                                                               
OPTRVN   MVI   ERROR,BADKEYWD                                                   
         TM    SVOPT,X'58'                                                      
         JNZ   GENERRV                                                          
         TM    SVOPT3,OPT3REPT                                                  
         JNZ   GENERRV                                                          
         OI    SVOPT,OPTREV                                                     
         B     VOPT80                                                           
*                                                                               
* OPTION REV - PRINT PATTERN ROTATION ON INSTRUCTIONS *                         
*                                                                               
OPTROT   MVI   SVT1PR12,C'N'      SET PRINT PATTERN ROTATION                    
         B     VOPT80                                                           
*                                                                               
* SPOT - USE SOURCE OF MEDIA BUYS, NOT TRAFFIC BUYS *                           
*                                                                               
OPTSPT   NI    SVOPT2,X'FF'-OP2TRBUY                                            
         MVI   SVPROF15,C'N'       SET PROFILE OPTION ALSO                      
         B     VOPT80                                                           
*                                                                               
* TEST - DO NOT UPDATE FILES, ONLY PRINT INSTRUCTIONS *                         
*                                                                               
OPTEST   CLI   RECNUM,47           TWX INSTR                                    
         BE    VOPT90                                                           
         OI    SVOPT,OPTTEST                                                    
         B     VOPT80                                                           
*                                                                               
* GENERATE INSTRUCTIONS FOR ALL MARKETS IN A MARKET LIST                        
*                                                                               
OPTMLST  DS    0H                                                               
         OC    BMKTSTA,BMKTSTA     WAS MARKET STATION REQ?                      
         BNZ   MKTMKLER             YES, ERROR                                  
         TM    SVOPT,OPT1MKT       MKT REQ ?                                    
         BO    MKTMKLER             YES ,ERROR                                  
         OC    OPTMGRP,OPTMGRP     TEST SINGLE MKT GROUP REQUEST                
         BNZ   MKTMKLER                                                         
*                                                                               
         CLI   1(R4),0             WAS MARKET LIST NAME ENTERED                 
         BE    MKTL0ERR             NO                                          
*                                                                               
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING MKLKEY,R1                                                        
         MVC   MKLKID,=XL2'0A38'                                                
         MVC   MKLKAM,BAGYMD                                                    
         MVC   MKLKCLT,BCLT        MOVE IN CLIENT                               
         MVC   MKLKBPRD,BPRD            PRODUCT CODE                            
         MVC   MKLKBEST,QBEST           ESTIMATE CODE                           
         ZIC   RF,1(R4)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   MKLKLNAM(0),22(R4)  MARKET LIST NAME                             
         MVC   SVMKTLST,MKLKLNAM                                                
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(12),KEYSAVE     MARKET LIST FOUND ?                          
         BE    OPTML20              YES                                         
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         MVI   MKLKBEST,0          CLEAR ESTIMATE CODE                          
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE     MARKET LIST FOUND ?                          
         BE    OPTML20              YES                                         
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         MVI   MKLKBPRD,0          CLEAR PRODUCT CODE                           
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE     MARKET LIST FOUND ?                          
         BNE   NOMKLERR             NO                                          
*                                                                               
OPTML20  DS    0H                                                               
         XC    SVBMKT,SVBMKT                                                    
         OI    SVOPT3,OPT3MKL         DO MARKET LIST                            
         NI    SVOPT3,X'FF'-OPT3MKLR  INIT MARKET LIST RECORD READ              
         B     VOPT80                                                           
*                                                                               
* OPTION DEL(ETE) COMMERCIAL FROM SHIPPING ELEMENT *                            
*                                                                               
OPTDL    MVI   ERROR,BADKEYWD                                                   
         TM    SVOPT,X'70'                                                      
         JNZ   GENERRV                                                          
         MVI   ERROR,NOCOMM                                                     
         CLI   1(R4),0                                                          
         JE    GENERRV                                                          
         MVC   SVOPTCML,22(R4)     SAVE COMMERCIAL ID                           
*                                                                               
         MVI   ERROR,INVCOMM                                                    
         CLI   1(R4),8                                                          
         JNE   GENERRV                                                          
*                                                                               
* TRY TO READ COMMERCIAL REC *                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(8),22(R4)                                                  
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   GENERRV                                                          
         B     VOPT80                                                           
*                                                                               
OPTNW    MVI   ERROR,BADKEYWD                                                   
         TM    SVOPT,X'68'                                                      
         JNZ   GENERRV                                                          
         TM    SVOPT3,OPT3REPT                                                  
         JNZ   GENERRV                                                          
         CLI   1(R4),0                                                          
         JNE   GENERRV                                                          
         OI    SVOPT,OPTNEW                                                     
         B     VOPT80                                                           
*                                                                               
* PRINT ALL 'TO BE ANNOUNCED COMMERCIALS' - NO PATTERN COVERED *                
*                                                                               
OPTTBA   OI    SVOPT2,OP2TBA                                                    
         B     VOPT80                                                           
*                                                                               
* MKT - RUN INSTRUCTIONS FOR MARKET NNNN OR START AT MARKET NNNN *              
*                                                                               
OPTMKT   OI    SVOPT,OPT1MKT                                                    
*                                                                               
OPTSMKT  MVI   ERROR,BADKEYWD                                                   
         OC    BMKTSTA,BMKTSTA     TEST PREVIOUS MKT OR STA INPUT               
         JNZ   GENERRV                                                          
*                                                                               
         TM    SVOPT3,OPT3MKL      MARKET LIST NAME ENTERED?                    
         BO    MKTMKLER             YES, ERROR                                  
*                                                                               
* CONSTRUCT FLDHDR FOR VALIMKT *                                                
*                                                                               
         XC    ELEM,ELEM                                                        
         ZIC   RE,1(R4)                                                         
         STC   RE,ELEM+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+8(0),22(R4) *EXECUTED*                                      
         LA    RE,9(RE)                                                         
         STC   RE,ELEM                                                          
         PACK  ELEM+4(1),3(1,R4)   INVERT VALIDITY BYTE HALVES                  
*                                                                               
         MVI   ERROPT,C'Y'         SET 'RETURN ON ERROR'                        
         LA    R2,ELEM             POINT TO FLDHDR                              
*                                                                               
         GOTO1 VALIMKT                                                          
*                                                                               
         MVI   ERROPT,C'N'         RESET                                        
         LA    R2,TRAOPTH          RESTORE R2                                   
         CLI   ERROR,0                                                          
         JNE   GENERRV                                                          
         CLI   BYTE,X'FF'          PROCESSING MKT=0000                          
         BE    VOPTX               YES, DONE                                    
         B     VOPT80                                                           
*                                                                               
* ONLY RUN INSTRUCTIONS FOR 1 STATION, OR START AT STATION XXXX *               
*                                                                               
OPTSTA   OI    SVOPT,OPT1STA                                                    
*                                                                               
OPTSSTA  MVI   ERROR,BADKEYWD                                                   
         CLI   OFFLINE,C'Y'                                                     
         BE    *+14                                                             
         OC    BMKTSTA,BMKTSTA     TEST PREVIOUS MKT OR STA INPUT               
         JNZ   GENERRV                                                          
*                                                                               
* CONSTRUCT FLDHDR FOR VALISTA *                                                
*                                                                               
         XC    ELEM,ELEM                                                        
         ZIC   RE,1(R4)                                                         
         STC   RE,ELEM+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+8(0),22(R4) *EXECUTED*                                      
         LA    RE,9(RE)                                                         
         STC   RE,ELEM                                                          
         PACK  ELEM+4(1),3(1,R4)   INVERT VALIDITY BYTE HALVES                  
*                                                                               
         MVI   ERROPT,C'Y'         SET 'RETURN ON ERROR'                        
         LA    R2,ELEM             POINT TO FLDHDR                              
         GOTO1 VALISTA                                                          
*                                                                               
         MVI   ERROPT,C'N'         RESET                                        
         LA    R2,TRAOPTH          RESTORE R2                                   
         CLI   ERROR,0                                                          
         JNE   GENERRV                                                          
         B     VOPT80                                                           
*                                                                               
* NOADDR - PROCESS STATIONS WITH NO STATION ADDRESS *                           
*                                                                               
OPTNOAD  OI    SVOPT2,OP2NOADR                                                  
         B     VOPT80                                                           
*                                                                               
* TRAFFIC - USE SOURCE OF TRAFFIC BUYS, NOT MEDIA BUYS *                        
*                                                                               
OPTRAF   OI    SVOPT2,OP2TRBUY                                                  
         MVI   SVPROF15,C'Y'       SET PROFILE OPTION ALSO                      
         B     VOPT80                                                           
*                                                                               
* ENCLOSED - SHOW SHIPPING MESSAGE AS COMMERCIALS ENCLOSED *                    
*                                                                               
OPTENCL  MVI   SVT1PR6,C'Y'                                                     
         B     VOPT80                                                           
*                                                                               
OPTISCI  DS    0H                                                               
         CLI   TWAOFFC,C'*'        ONLY DDS TERMINALS                           
         BNE   VOPTHELP                                                         
         MVI   SVT1PR7,C'N'        PRINT CML ISCII NOT CLT CML #                
         B     VOPT80                                                           
*                                                                               
* CMT - GET LEADING SPECIAL TEXT KEY CHARACTER *                                
*                                                                               
OPTCMT   CLI   SVPROF16,C'N'                                                    
         BE    VOPT90                                                           
         CLI   SVPROF16,C'0'      DEFAULT                                       
         BE    VOPT90                                                           
         CLI   1(R4),1            ONLY 1 CHAR ALLOWED                           
         BNE   SPECMTER                                                         
         CLI   SVPROF16,C'A'       ALL SPEC CMT CODES ALLOWED                   
         BE    OPTCMT10                                                         
         CLC   22(1,R4),SVPROF12                                                
         BE    OPTCMT20                                                         
         CLC   22(1,R4),SVPROF16                                                
         BNE   SPECMTER                                                         
*                                                                               
OPTCMT10 LA    R0,7                6 SPEC TEXT & N (NULL SPEC TEXT)             
         LA    R1,=C'$&&@#/*N'                                                  
OPTCMT14 CLC   22(1,R4),0(R1)                                                   
         BE    OPTCMT16                                                         
         LA    R1,1(,R1)                                                        
         BCT   R0,OPTCMT14                                                      
         B     CMTCDER                                                          
OPTCMT16 MVC   SVPROF16(1),0(R1)                                                
         B     *+10                                                             
OPTCMT20 MVC   SVPROF16(1),22(R4)  SAVE SPECIAL COMMENT CODE                    
         OI    SVOPT2,OP2SPCMT                                                  
         B     VOPT80                                                           
*                                                                               
* PRT - PRINT INSTRUCTIONS - OVERRIDES T1 PROFILE 9 - COVER LETTERS *           
*                                                                               
OPTPRT   MVI   SVT1PR9,C'N'       COVER LETTER INSTR                            
         B     VOPT80                                                           
*                                                                               
* LEN - ONLY RUN INSTRUCTIONS FOR SPOT LENGTH NNN *                             
*                                                                               
* CONSTRUCT FLDHDR FOR VALISLN *                                                
*                                                                               
OPTLEN   XC    ELEM,ELEM                                                        
*                                                                               
         MVI   ERROR,MISSING                                                    
         SR    RE,RE                                                            
         ICM   RE,1,1(R4)                                                       
         JZ    GENERRV             IF NO DATA, BAD MOVE                         
         STC   RE,ELEM+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+8(0),22(R4) *EXECUTED*                                      
         LA    RE,9(RE)                                                         
         STC   RE,ELEM                                                          
         PACK  ELEM+4(1),3(1,R4)   INVERT VALIDITY BYTE HALVES                  
*                                                                               
         MVI   ERROPT,C'Y'         SET 'RETURN ON ERROR'                        
         LA    R2,ELEM             POINT TO FLDHDR                              
         GOTO1 VALISLN                                                          
*                                                                               
         MVI   ERROPT,C'N'         RESET                                        
         LA    R2,TRAOPTH          RESTORE R2                                   
         CLI   ERROR,0                                                          
         JNE   GENERRV                                                          
         MVC   SVOPTLEN,WORK+4                                                  
         B     VOPT80                                                           
*                                                                               
* # - MEANS THIS IS A FOLLOW UP REQUEST FROM TWX FOR AN EXACT COPY *            
*                                                                               
OPTLB    TM    WHEN,X'38'          MUST BE OFFLINE                              
         BZ    VOPT90                                                           
         CLI   12(R4),C'#'         FOLLOW UP FOR TWX                            
         BNE   VOPT90                                                           
         CLI   SVTWPR1,C'Y'        MUST HAVE TWX ON                             
         BNE   VOPT90                                                           
*                                                                               
         OI    SVOPT2,OP2TCOPY     COPY OPTION FOR TWX                          
*                                                                               
         MVI   TRAFAX,C'C'                                                      
         OI    TRAFAXH+6,X'80'                                                  
         MVI   SVTWPR1,C'C'                                                     
         B     VOPT80                                                           
*                                                                               
VOPT80   LA    R4,32(R4)                                                        
         LLC   R0,BYTE                                                          
         BCTR  R0,0                                                             
         STC   R0,BYTE                                                          
         B     VOPT10                                                           
*                                                                               
VOPT90   MVC   GERROR,=Y(BADINPQ)  UNKNOWN INPUT FIELD                          
         MVC   ERRTEXT(8),12(R4)                                                
         GOTO1 VTRAERR                                                          
                                                                                
*===============================================================                
* GENERRV IS A HOLDOVER FROM PUTTING MESSAGES OUT IN CONHEAD                    
* ERROR CONTAINS A 1-BYTE ERROR CODE THAT IS NOW MOVED TO GERROR                
* AND HANDLED LIKE A 2-BYTE ERROR                                               
*===============================================================                
                                                                                
GENERRV  LLC   R0,ERROR                                                         
         STH   R0,GERROR                                                        
*                                                                               
         TM    WHEN,X'20'          THIS SOON REQUEST                            
         JZ    GENERRV2             NO                                          
         CLI   OFFLINE,C'Y'        AND OFFLINE                                  
         JNE   GENERRV2             NO                                          
*                                                                               
* UNLOCK USING LOCKET *                                                         
*                                                                               
         MVI   DUB,X'E4'                     U=UNLOCK                           
         MVC   DUB+1(7),=X'030A220A240A25' 03=3 ENTRIES                         
*                                       0A22, 0A24, 0A25 RECS                   
         GOTO1 VALILOC,0                                                        
*                                                                               
GENERRV2 GOTO1 VTRAERR                                                          
*                                                                               
CMTCDER  MVC   GERROR,=Y(BADCMT1)  CMT= * && ...                                
         GOTO1 VTRAERR                                                          
*                                                                               
SPECMTER MVC   GERROR,=Y(BADCMT2)  CMT= * OR SPCL CHAR                          
         MVC   ERRTEXT(1),SVPROF16                                              
         GOTO1 VTRAERR                                                          
*                                                                               
CMTENTER MVC   GERROR,=Y(BADCMT3)  CMT= REQUIRED                                
         GOTO1 VTRAERR                                                          
*                                                                               
EXTCOMER MVC   GERROR,=Y(EXTCOMMA) FIELD WITH NO INPUT                          
         GOTO1 VTRAERR                                                          
*                                                                               
NOMKLERR MVC   GERROR,=Y(NOMKLERQ)  NO MARKET LIST FOUND                        
         GOTO1 VTRAERR                                                          
*                                                                               
MKTL0ERR MVC   GERROR,=Y(MKTLNAMQ)  ENTER MARKET LIST NAME                      
         GOTO1 VTRAERR                                                          
*                                                                               
MKTMKLER MVC   GERROR,=Y(MKTOPTQ)   RUN EITHER MLIST OR MKT/MGROUP              
         GOTO1 VTRAERR                                                          
*                                                                               
AMSERR   MVC   GERROR,=Y(AMSERRQ)   ENTER AMS=Y OR AMS=N                        
         GOTO1 VTRAERR                                                          
*                                                                               
DUPMGERR MVC   GERROR,=Y(ONLY1MGR)  REQUEST 1 MARKET GROUP AT A TIME            
         GOTO1 VTRAERR                                                          
*                                                                               
OPTMGRER MVC   GERROR,=Y(MGPROFER)  MUST HAVE T2 PROFILE 1 SET                  
         GOTO1 VTRAERR                                                          
*                                                                               
NOMGRPER MVC   GERROR,=Y(MGNF)       NO MARKET GROUP FOUND                      
         GOTO1 VTRAERR                                                          
*                                                                               
MGRENTER MVC   GERROR,=Y(NTRMGRAS)  ENTER MGR AS NNNN                           
         GOTO1 VTRAERR                                                          
*                                                                               
VOPTHELP MVC   GERROR,=Y(OPTHELPQ)                                              
         GOTO1 VTRAERR                                                          
         DROP  RB,RC                                                            
OPTABLE  DC    CL10'#         ',AL2(OPTLB-VOPT)                                 
         DC    CL10'AFFILIATE ',AL2(OPTAFIL-VOPT)                               
         DC    CL10'AMS       ',AL2(OPTAMS-VOPT)                                
         DC    CL10'APAT      ',AL2(OPTAPTN-VOPT)                               
         DC    CL10'BPAT      ',AL2(OPTBPTN-VOPT)                               
         DC    CL10'CMT       ',AL2(OPTCMT-VOPT)                                
         DC    CL10'DEL       ',AL2(OPTDL-VOPT)                                 
         DC    CL10'ENCLOSED  ',AL2(OPTENCL-VOPT)                               
         DC    CL10'ISCII     ',AL2(OPTISCI-VOPT) PRINT ISCII-DDS ONLY          
         DC    CL10'LEN       ',AL2(OPTLEN-VOPT)                                
         DC    CL10'MGROUP    ',AL2(OPTMGR-VOPT)                                
         DC    CL10'MKT       ',AL2(OPTMKT-VOPT)                                
         DC    CL10'MKT>      ',AL2(OPTSMKT-VOPT)                               
         DC    CL10'MLIST     ',AL2(OPTMLST-VOPT)                               
         DC    CL10'NEW       ',AL2(OPTNW-VOPT)                                 
         DC    CL10'NOADDR    ',AL2(OPTNOAD-VOPT)                               
         DC    CL10'NOAGY     ',AL2(OPTNOAGY-VOPT)                              
         DC    CL10'NOPRT     ',AL2(OPTNOPRT-VOPT)                              
         DC    CL10'NOROT     ',AL2(OPTNOROT-VOPT)                              
         DC    CL10'PRT       ',AL2(OPTPRT-VOPT)                                
         DC    CL10'REPRINT   ',AL2(OPTREPT-VOPT)                               
         DC    CL10'RERUN     ',AL2(OPTRERN-VOPT)                               
         DC    CL10'REV       ',AL2(OPTRVN-VOPT)                                
         DC    CL10'ROT       ',AL2(OPTROT-VOPT)                                
         DC    CL10'SPOT      ',AL2(OPTSPT-VOPT)                                
         DC    CL10'STA       ',AL2(OPTSTA-VOPT)                                
         DC    CL10'STA>      ',AL2(OPTSSTA-VOPT)                               
         DC    CL10'TBA       ',AL2(OPTTBA-VOPT)                                
         DC    CL10'TEST      ',AL2(OPTEST-VOPT)                                
         DC    CL10'TRAFFIC   ',AL2(OPTRAF-VOPT)                                
OPTABLEL EQU   (*-OPTABLE)/12                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*======================================================================         
* READ THROUGH BUYS AND BUILD ACTIVITY LIST                                     
* IF THERE IS A WORKER FILE NUMBER IN TRAWRKR, OPEN THAT WORKER FILE            
* AND ONLY PROCESS STATIONS/PRODUCTS THAT APPEAR IN THAT FILE                   
*======================================================================         
*                                                                               
BLDACT   NMOD1 0,*BLDACT*,RA                                                    
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,TODAYP)                                     
*                                                                               
* CLEAR EQUIVALENT PRODUCT AREA *                                               
*                                                                               
         L     R0,ADTLIST                                                       
         L     R1,ADTLISTX                                                      
         SR    R1,R0                                                            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
* 001-230 115 PRS (EQUIV/BASE PRODS) 203-232 SAVED PROD EQUIV TABLE             
* 233-256 24 SAVED BUY KEY                                                      
*                                                                               
* CLEAR ACTIVITY LIST BUILD AREA *                                              
*                                                                               
BLD01    L     R4,AIO3                                                          
         LHI   R5,1000                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R4,RE                                                            
*                                                                               
BLD01A   CLI   SVPOLQ,0            TEST POL PRDLIST                             
         BE    *+8                 NO                                           
         MVI   SVPOLFLG,1          SET FIRST PASS OF POL PRDLIST                
*                                                                               
         TM    SVOPT,OPTRERUN     THIS A RERERUN                                
         BO    BLDINS                                                           
*                                                                               
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BO    BLDINS                                                           
*                                                                               
         USING ACLDATA,R4                                                       
         CLI   SVPROF15,C'Y'       USE TRAFFIC, NOT SPOT BUYS                   
         BE    BLDTR                                                            
*                                                                               
         MVC   SYSDIR(3),=C'SPT'   SWITCH TO SPOT MEDIA                         
         MVC   SYSFIL(3),=C'SPT'                                                
         XC    KEY,KEY                                                          
         MVC   KEY(13),SVKEY       SET INITIAL KEY                              
*                                                                               
         OC    SVKEY,SVKEY         TEST CONTINUATION                            
         BZ    BLD02               NO                                           
         CLC   SVKEY(3),BAGYMD     TEST SAME A-M/CLT                            
         BNE   BLD122                                                           
         CLI   SVKEY+3,X'FF'       TEST USING POL KEYS                          
         BE    BLD02                                                            
         CLC   SVKEY+3(1),BPRD     TEST SAME PRD                                
         BNE   BLD122                                                           
*                                                                               
BLD02    MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
*                                                                               
         MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   I WANT CONTROL BACK                          
         GOTO1 CATCHIOS            DON'T TASKNEXT OUT!                          
         CLI   ERROR,0             ANY ERROR?                                   
         BE    BLD02A                                                           
*                                                                               
BLDACTER GOTO1 ERREX                                                            
*                                                                               
BLD02A   DS    0H                                                               
         CLC   KEY(3),BAGYMD       TEST SAME A-M/CLT                            
         BNE   BLD92               DONE                                         
*                                                                               
         CLC   KEY(9),KEYSAVE      A-M/C/P/MKT/STA                              
         BE    BLD03                                                            
*                                                                               
         OC    KEYSAVE+4(9),KEYSAVE+4   THIS STARTING KEY                       
         BZ    BLD03                     YES, CONTINUE                          
*                                                                               
         CLI   SVT2PR05,C'Y'       COMBINE CABLE NETWORKS                       
         BNE   BLD92                NO                                          
*                                                                               
*                    X'E8' NOW                                                  
         CLI   KEY+6,CABLESTA      THIS A CABLE STATION                         
         BL    BLD92                NO                                          
*                                                                               
         CLC   KEY(8),KEYSAVE                                                   
         BNE   BLD92                                                            
         MVC   HALF(1),KEY+8                                                    
         MVC   HALF+1(1),KEYSAVE+8                                              
         NI    HALF,X'80'                                                       
         NI    HALF+1,X'80'                                                     
         CLC   HALF(1),HALF+1                                                   
         BE    BLD03                                                            
         B     BLD92                                                            
         EJECT                                                                  
* IF FILTERING ON MARKET GROUP, CHECK IT OUT *                                  
*                                                                               
BLD03    DS    0H                                                               
*MNMB                                                                           
         CLI   SVT3PROF+5,C'Y'                                                  
         BNE   BLD03A                                                           
         XC    ELEM,ELEM                                                        
         XC    DUB,DUB                                                          
         GOTO1 MSUNPK,DMCB,(X'80',KEY+4),ELEM,DUB                               
         CLI   DUB+4,C'D'                                                       
         BE    BLD90                                                            
         CLI   DUB+4,C'S'                                                       
         BE    BLD90                                                            
         CLI   DUB+4,C'C'                                                       
         BE    BLD90                                                            
BLD03A   DS    0H                                                               
*MNMB                                                                           
         OC    OPTMGRP,OPTMGRP     TEST SINGLE MKT GROUP REQUEST                
         BZ    BLD03C                                                           
         CLC   KEY+4(2),BMKT                                                    
         BE    BLD03C                                                           
*                                                                               
         BRAS  RE,RMGB             GO GET NEXT MARKET IN GROUP                  
         BNE   BLD122                                                           
*                                                                               
BLD03C   TM    SVOPT3,OPT3MKL      MARKET LIST NAME ENTERED?                    
         BZ    BLD03D                                                           
         CLC   KEY+4(2),BMKT                                                    
         BE    BLD03D                                                           
*                                                                               
         TM    SVOPT3,OPT3MKLR     WAS MARKET LIST RECORD READ                  
         BO    BLD03CA                                                          
         BRAS  RE,RMKTL            READ MARKET LIST RECORD                      
*                                                                               
BLD03CA  BRAS  RE,GETMKTL                                                       
         BNE   BLD122                                                           
*                                                                               
BLD03D   TM    SVOPT,OPT1MKT       TEST SINGLE MKT REQUEST                      
         BZ    BLD03E                                                           
         CLC   KEY+4(2),BMKT                                                    
         BNE   BLD122                                                           
*                                                                               
BLD03E   TM    SVOPT,OPT1STA       TEST SINGLE STATION REQUEST                  
         BZ    BLD04                                                            
*                                                                               
         CLC   KEY+6(3),BSTA                                                    
         BE    BLD04                                                            
*                                                                               
         CLI   SVT2PR05,C'Y'       COMBINE CABLE NETWORKS                       
         BNE   BLD122                                                           
*                                                                               
*                    X'E8' NOW                                                  
         CLI   KEY+6,CABLESTA      THIS A CABLE STATION                         
         BL    BLD122                                                           
*                                                                               
         CLC   KEY+6(2),BSTA                                                    
         BNE   BLD122                                                           
         MVC   HALF(1),KEY+8                                                    
         NI    HALF,X'80'                                                       
         CLC   HALF(1),BSTA+2                                                   
         BNE   BLD122                                                           
*                                                                               
BLD04    ZIC   RE,KEY+9                                                         
         LA    RE,SVESTAB(RE)                                                   
         CLI   0(RE),0             TEST EST ACTIVE                              
         BNE   BLD05                YES                                         
*                                                                               
         XC    KEY+10(3),KEY+10                                                 
         CLC   KEY+9(1),BEST       TEST LOWER THAN START EST                    
         BH    *+14                NO                                           
         MVC   KEY+9(1),BEST       FORCE LOW EST                                
         B     BLD02                                                            
         CLC   KEY+9(1),BESTEND    TEST HIGHER THAN END EST                     
         BL    *+8                                                              
         MVI   KEY+9,X'FF'         SET TO FORCE NEW STA                         
*                                                                               
         MVC   KEY+10(3),=3X'FF'                                                
         B     BLD02                                                            
*                                                                               
BLD05    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         EJECT                                                                  
* GET DAYPART EQUIVALENCY CODE IF PROFILE ON AND NONE IN FOR ESTIMATE *         
*                                                                               
         CLI   SVPROF11,C'A'       TEST COPY CODE = ADJACENCY                   
         BE    BLD10                                                            
         CLI   SVPROF11,C'D'       TEST COPY CODE = DAYPART                     
         BE    BLD10                                                            
         CLI   SVPROF11,C'Y'       TEST COPY CODE = DAYPART                     
         BNE   BLD15                                                            
BLD10    CLC   EQVEST,KEY+9        SAME EST                                     
         BE    BLD15                                                            
         MVC   TRBUYKEY,KEY                                                     
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING DPEKEY,R1                                                        
         MVC   DPEKID,=X'0A34'                                                  
         MVC   DPEKAM(3),BAGYMD AND BCLT                                        
         MVC   DPEKEST,TRBUYKEY+9                                               
         DROP  R1                                                               
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'TRFDIR'                                            
         GOTO1 HIGH                                                             
         MVI   EQVEST,0                                                         
         XC    EQVTAB,EQVTAB                                                    
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BLD14                                                            
         MVI   ELCODE,X'10'                                                     
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'TRFFIL'                                            
         GOTO1 GETREC                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DPEDTAEL,R6                                                      
         LA    R1,EQVTAB                                                        
BLD12    MVC   0(2,R1),DPEDPCDE                                                 
         LA    R1,2(,R1)                                                        
         BRAS  RE,NEXTEL                                                        
         BE    BLD12                                                            
BLD14    MVC   EQVEST,TRBUYKEY+9                                                
         XC    FILENAME,FILENAME                                                
         DROP  R6                                                               
         MVC   KEY,TRBUYKEY                                                     
*                                                                               
         XC    TRBUYKEY,TRBUYKEY                                                
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
* NOW GET BUY RECORD *                                                          
*                                                                               
BLD15    OI    DMINBTS,X'08'       PASS DELETED RECS                            
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'F7'       SET OFF PASS DELETED RECS                    
*                                                                               
         MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   I WANT CONTROL BACK                          
         GOTO1 CATCHIOS            DON'T TASKNEXT OUT!                          
         CLI   ERROR,0             ANY ERROR?                                   
         BNE   BLDACTER                                                         
*                                                                               
         L     R7,AIO                                                           
         USING BUYRECD,R7                                                       
         TM    15(R7),X'80'        TEST DELETED                                 
         BO    BLD90               YES - IGNORE                                 
         CLC   KEY+4(2),4(R7)     TEST SAME MARKET                              
         BNE   BLD90                NO - SPILL                                  
*                                                                               
         MVC   BUYDATE,BDCHG                                                    
*                                                                               
* LOOK FOR TRAFFIC=NO                                                           
*                                                                               
         MVI   ELCDLO,X'66'                                                     
         MVI   ELCDHI,X'66'                                                     
         LA    R6,BDELEM                                                        
*                                                                               
BLD16    BAS   RE,BUYEL                                                         
         BNE   BLD16X                                                           
         CLC   =C'TRAFFIC=NO',3(R6)                                             
         BE    BLD90               BYPASS THIS BUY                              
         B     BLD16                                                            
*                                                                               
* FILTER ON SPOT LENGTH *                                                       
*                                                                               
BLD16X   CLI   SVOPTLEN,0          SPOT LENGTH ENTERED                          
         BE    *+14                                                             
         CLC   BDSEC,SVOPTLEN                                                   
         BNE   BLD90                                                            
*                                                                               
* CALCULATE DIFFERENCE BETWEEN FIRST/LAST DAYS OF ROTATOR *                     
*                                                                               
         XC    ROTDAYS,ROTDAYS                                                  
         ZIC   R0,BDDAY                                                         
         SLL   R0,25                                                            
*                                                                               
         CLI   OWRSDAY,0                                                        
         BE    BLD17                                                            
         ZIC   RF,OWRSDAY                                                       
*                                                                               
         LTR   R0,R0                                                            
         BNM   *+8                                                              
         O     R0,=X'00800000'                                                  
         SLL   R0,1                                                             
         BCT   RF,*-14                                                          
*                                                                               
BLD17    LTR   R0,R0               REG GOES NEGATIVE WHEN BIT ARRIVES           
         BZ    *+16                                                             
         BM    *+12                                                             
         SLL   R0,1                                                             
         B     *-14                                                             
*                                                                               
         SR    RE,RE               CLEAR COUNTER                                
         SLL   R0,1                                                             
         LTR   R0,R0               SHIFT TILL NO MORE BITS ON                   
         BZ    *+8                                                              
         BCT   RE,*-10                                                          
         LPR   RE,RE                                                            
         STH   RE,ROTDAYS          AND SAVE DAYS                                
         EJECT                                                                  
*                                                                               
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         LA    R6,BDELEM                                                        
*                                                                               
BLD20    BAS   RE,BUYEL                                                         
         BNE   BLD90                                                            
         CLI   1(R6),10            TEST UNALL                                   
         BNH   BLD20                                                            
         TM    6(R6),X'C0'         TEST MINUS OR MINUSED                        
         BNZ   BLD20                                                            
         MVC   ELDATE,2(R6)        SAVE ELEM START DATE                         
         MVC   ELDATEX,2(R6)       AND PRESET ELEM END DATE                     
*                                                                               
         CLI   SVT1PR1,0           TEST ADJUST ROTATOR DAYS                     
         BE    BLD22               NO                                           
         CLC   SVT1PR1(1),BDDAY    TEST DAYS MATCH                              
         BNE   BLD22                                                            
*                                                                               
* BACK UP ONE DAY *                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(2,ELDATE),WORK                                      
         ZIC   R0,SVT1PR2          GET NUMBER OF DAYS TO BACK UP                
         LNR   R0,R0                                                            
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,ELDATE)                                    
*                                                                               
BLD22    CLC   ELDATE,SVGENEND     TEST AFTER FLIGHT                            
         BH    BLD20                                                            
*                                                                               
         CLI   0(R6),11            TEST REGEL                                   
         BNE   BLD24                                                            
         GOTO1 DATCON,DMCB,(2,ELDATEX),WORK                                     
         LH    R0,ROTDAYS                                                       
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,ELDATEX)                                   
*                                                                               
BLD24    DS   0H                                                                
         CLI   SVT1PR13,C'Y'       USE FLT/EST DATES, NOT FTD/LTD               
         BNE   BLD24C                                                           
*                                                                               
         CLI   SVT2PR13,C'Y'       ANY BUY FORCES FULL PERIOD                   
         BNE   BLD24C                                                           
*                                                                               
         CLC   ELDATEX,SVFLTST     TEST ENDS BEFORE FLIGHT STARTS               
         BL    BLD20                                                            
*                                                                               
         CLC   ELDATE,SVFLTEND     TEST STARTS AFTER FLIGHT ENDS                
         BH    BLD20                                                            
         B     BLD24E                                                           
*                                                                               
BLD24C   DS   0H                                                                
         CLC   ELDATEX,SVGENST     TEST ENDS BEFORE FLIGHT STARTS               
         BL    BLD20                                                            
*                                                                               
BLD24E   DS   0H                                                                
         XC    ACLWORK,ACLWORK                                                  
         LA    R4,ACLWORK                                                       
         USING ACLDATA,R4                                                       
*                                                                               
         BRAS  RE,CPY              GO SET COPY CODE IF ANY                      
         EJECT                                                                  
BLD24X   MVC   ACLSLN,11(R6)       SLN                                          
         MVC   BYTE,10(R6)                                                      
         BAS   RE,GETPRD                                                        
         MVC   ACLEBCP1,0(R1)      EBCDIC PRD                                   
         MVC   ACLPRD,BYTE         MAY BE EQUIVALENT PROD                       
*                                                                               
         CLI   1(R6),18            TEST PIGGYBACK                               
         BNL   BLD24Y               YES                                         
         CLI   BPRD2,X'FF'         TEST PTR = NONE                              
         BE    BLD26                                                            
         CLI   BPRD2,0             TEST PTR REQUIRED                            
         BNE   BLD20                YES, BYPASS                                 
         B     BLD26                                                            
*                                                                               
BLD24Y   CLI   BPRD2,X'FF'         TEST PTR = NONE                              
         BE    BLD20                BYPASS                                      
         MVC   ACLSLN2,15(R6)      SLN2                                         
         MVC   BYTE,14(R6)                                                      
         BAS   RE,GETPRD                                                        
         MVC   ACLEBCP2,0(R1)                                                   
         MVC   ACLPRD2,BYTE         MAY BE EQUIVALENT PROD                      
*                                                                               
* MUST BE IN ALPHA SEQ *                                                        
*                                                                               
         CLC   ACLEBCP1,ACLEBCP2                                                
         BL    BLD25                                                            
*                                                                               
* REVERSE THE ENTRIES *                                                         
*                                                                               
         XC    ACLPRD(2),ACLPRD2                                                
         XC    ACLPRD2(2),ACLPRD                                                
         XC    ACLPRD(2),ACLPRD2                                                
         XC    ACLEBCP1,ACLEBCP2                                                
         XC    ACLEBCP2,ACLEBCP1                                                
         XC    ACLEBCP1,ACLEBCP2                                                
*                                                                               
BLD25    CLI   BPRD2,0             LIMIT ON PTR                                 
         BE    BLD26                NO                                          
         CLC   BPRD2,ACLPRD2                                                    
         BNE   BLD20                                                            
*                                                                               
* TEST DATA IN TABLE ALREADY *                                                  
*                                                                               
BLD26    CLI   BPRD,X'FF'          TEST POL REQ                                 
         BE    BLD26A                                                           
         CLC   ACLPRD,BPRD         ELSE MUST MATCH PRD 1                        
         BNE   BLD20                                                            
         B     BLD26X                                                           
*                                                                               
BLD26A   CLI   SVPOLQ,0            TEST ANY POL LIST                            
         JE    BLD26X              NO                                           
                                                                                
*==============================================================                 
* IF POL LIST ENTERED, FOR PASS 1 MATCH A PRD IN THE LIST                       
* WHEN ONE IS FOUND, DO PASS 2 AND DO ALL PRDS FOR THIS STATION                 
*==============================================================                 
                                                                                
         CLI   SVPOLFLG,1          TEST FIRST PASS                              
         JNE   BLD26X              NO - PROCESS ALL PRDS                        
*                                                                               
         LA    R1,SVPOLQ                                                        
         LA    R0,3                                                             
*                                                                               
BLD26B   CLC   ACLEBCP1,0(R1)      MATCH PRD1                                   
         JE    BLD26C                                                           
         CLI   ACLEBCP2,0          TEST THERE IS A PRD 2                        
         JE    BLD26D              NO                                           
         CLC   ACLEBCP2,0(R1)      MATCH PRD2                                   
         JNE   BLD26D                                                           
*                                                                               
BLD26C   MVI   SVPOLFLG,2          SET TO DO ALL PRDS ON PASS2                  
         MVC   KEY,SVKEY           START READING BUYS AGAIN                     
         XC    ACLDATA,ACLDATA     AND CLEAR CURRENT ENTRY                      
         J     BLD02                                                            
*                                                                               
BLD26D   LA    R1,3(R1)                                                         
         CLI   0(R1),0                                                          
         JNE   BLD26B                                                           
         XC    ACLDATA,ACLDATA     PRD NOT IN LIST                              
         J     BLD20                                                            
*                                                                               
BLD26X   L     R4,AIO3                                                          
*                                                                               
BLD28    CLI   ACLPRD,0                                                         
         BE    BLD30                                                            
         CLC   ACLPRD(5),ACLWORK+6  PRD/SLN/PRD2/SLN2/COPY                      
         BE    BLD32                                                            
         LA    R4,L'ACLDATA(R4)                                                 
         B     BLD28                                                            
*                                                                               
BLD30    L     RE,AIO3                                                          
         LA    RE,1500(,RE)                                                     
         SH    RE,=AL2(L'ACLDATA)                                               
         CR    R4,RE                                                            
         BNH   *+6                                                              
         DC    H'0'                                                             
         MVC   ACLEBCP1(11),ACLWORK                                             
         MVC   ACLFTD,ELDATE                                                    
         MVC   ACLLTD,ELDATEX                                                   
*                                                                               
BLD32    DS   0H                                                                
         GOTO1 DATCON,DMCB,(3,BUYDATE),(2,HALF)                                 
         CLC   TODAYP,HALF         IF BUY ACTIVE TODAY                          
         BNE   *+8                                                              
         OI    ACLFLG,ACLFTDAY     SET BUY ACTIVE TODAY                         
*                                                                               
         CLC   ACLFTD,ELDATE                                                    
         BL    *+10                                                             
         MVC   ACLFTD,ELDATE                                                    
*                                                                               
         CLC   ACLFTD,SVGENST      TEST PRIOR TO FLIGHT START                   
         BH    *+10                                                             
         MVC   ACLFTD,SVGENST                                                   
*                                                                               
         CLC   ACLLTD,ELDATEX                                                   
         BH    *+10                                                             
         MVC   ACLLTD,ELDATEX                                                   
*                                                                               
         CLC   ACLLTD,SVGENEND     TEST AFTER FLIGHT END                        
         BL    *+10                                                             
         MVC   ACLLTD,SVGENEND                                                  
*                                                                               
         B     BLD20                                                            
         EJECT                                                                  
BLD90    MVC   KEYSAVE,KEY         SAVE BUY DATA                                
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 SEQ                                                              
         B     BLD02A                                                           
         EJECT                                                                  
* CHANGE OF STATION *                                                           
*                                                                               
BLD92    MVC   SVKEY,KEY           SAVE KEY THAT GAVE BREAK                     
*                                                                               
BLD92A   L     R2,NEXTSELF                                                      
         CLI   0(R2),9             TEST REACHED EOS                             
         BH    BLD92C                                                           
         TM    SVOPT3,OPT3MKLR                                                  
         BZ    BLD99                                                            
         NI    SVOPT3,X'FF'-OPT3MKLR  OFF MARKET LIST READ                      
         B     BLD99               STOP NOW                                     
*                                                                               
BLD92C   L     R4,AIO3                                                          
         CLI   ACLPRD,0            TEST ACTIVITY IN TABLE                       
         BE    BLD120X             NO - TEST TO CONTINUE                        
*                                                                               
* COUNT LINES NEEDED FOR DISPLAY                                                
*                                                                               
BLD92X   OI    SVOPT2,OP2ISACT     SET ON ACTIVITY SW                           
         SR    R0,R0                                                            
         CLI   ACLPRD,0                                                         
         BE    *+12                                                             
         LA    R4,L'ACLDATA(R4)                                                 
         BCT   R0,*-12                                                          
         LPR   R0,R0                                                            
         CLI   OFFLINE,C'Y'                                                     
         BE    BLD94                                                            
*                                                                               
         TM    WHEN,X'18'          THIS A DDS OR OV REQUEST                     
         BNZ   BLDX                 YES, QUIT                                   
         CLI   SVXFROV,7           TEST CALL BY SPOT/LNK                        
         BE    BLD94                                                            
*                                                                               
         CH    R0,=AL2(TRALINES)   TEST DATA FITS ON ONE SCREEN                 
         BNH   BLD94               YES                                          
         LA    RE,TRASEL1H         DATA WONT FIT ON ONE SCREEN                  
         C     RE,NEXTSELF         SO TEST WE ARE ON DISPLAY LINE 1             
         BNE   BLD99               IF NOT - STOP NOW                            
         MVI   SVBIGSW,C'Y'        ELSE SET FLAG FOR BIG INST                   
*                                                                               
* SAVE BUY KEY FOR SVTABLE *                                                    
*                                                                               
BLD94    MVC   TRBUYKEY,BUYRECD                                                 
*                                                                               
* BLANK COPY CODES WITHOUT PATTERNS IF COPY CODE = DAYPART *                    
*                                                                               
BLD95    CLI   SVT1PR8,C'Y'        TEST AUTO P/B PATTN                          
         BE    BLD96                                                            
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BO    BLD96                                                            
         CLI   SVPROF11,C'A'       TEST ADJACENCY=COPY CODE                     
         BE    BLD95C                                                           
         CLI   SVPROF11,C'D'       TEST DPT=COPY CODE                           
         BE    BLD95C                                                           
         CLI   SVPROF11,C'P'       TEST DPT=1ST CHAR OF PROGRAM CODE            
         BE    BLD95C                                                           
         CLI   SVPROF11,C'Y'       TEST DPT=COPY CODE                           
         BNE   BLD96                                                            
*                                                                               
* CHECK WHICH DAYPARTS HAVE PATTERNS *                                          
*                                                                               
BLD95C   BRAS  RE,CHKPTN                                                        
         MVC   BUYKEY,TRBUYKEY     RESTORE BUYKEY                               
         EJECT                                                                  
* SORT BY COPY CODE/SLN2/SLN1/EBCDIC PRD *                                      
*                                                                               
BLD96    L     R4,AIO3                                                          
         SR    R0,R0                                                            
*                                                                               
         CLI   0(R4),0                                                          
         BE    *+12                                                             
         LA    R4,L'ACLDATA(R4)                                                 
         BCT   R0,*-12                                                          
*                                                                               
         LPR   R0,R0                                                            
         GOTO1 XSORT,DMCB,AIO3,(R0),L'ACLDATA,1,10  COPY CODE                   
         GOTO1 (RF),(R1),,,,,9                      SLN2                        
         GOTO1 (RF),(R1),,,,,7                      SLN1                        
         GOTO1 (RF),(R1),,,,6,0                     ALPHA PROD1 & 2             
*                                                                               
         CLI   SVT2PR12,C'Y'       SORT BY DATE HIGH                            
         BNE   BLD97                                                            
*                                                                               
         GOTO1 (RF),(R1),,,,4,11                   DATE                         
*                                                                               
* NOW TEST IF DATA WILL FIT ON REMAINING SCREEN LINES *                         
*                                                                               
BLD97    L     R2,NEXTSELF                                                      
         L     R4,AIO3                                                          
*                                                                               
         CLI   SVBIGSW,C'Y'        TEST KNOW IT WONT FIT                        
         BE    BLD100                                                           
         CLI   OFFLINE,C'Y'        OR IF WE CARE                                
         BE    BLD100                                                           
         CLI   SVXFROV,7           TEST CALLED BY SPOT/LINK                     
         BE    BLD100                                                           
*                                                                               
BLD98    LA    R4,L'ACLDATA(R4)                                                 
         CLI   ACLPRD,0                                                         
         BE    BLD100              YES                                          
         LA    R2,NEXTLINE(R2)                                                  
         CLI   0(R2),9                                                          
         BH    BLD98                                                            
*                                                                               
* WILL NOT FIT - TERMINATE NOW *                                                
*                                                                               
BLD99    XC    SVKEY,SVKEY                                                      
*                                                                               
         L     R4,AIO3                                                          
         CLI   ACLPRD,0            TEST ACTIVITY IN TABLE                       
         BNE   *+14                 YES                                         
         MVC   SVKEY(9),KEYSAVE    ELSE USE SVKEY                               
         B     *+10                                                             
         MVC   SVKEY(9),BUYRECD    SET SVKEY TO REPROCESS                       
         MVC   SVKEY+3(1),BPRD     FIX PROD                                     
*                                                                               
         CLI   SVT2PR05,C'Y'       COMBINE CABLE NETWORKS                       
         BNE   BLDX                                                             
*                    X'E8' NOW                                                  
         CLI   SVKEY+6,CABLESTA    THIS A CABLE STATION                         
         BL    BLDX                 NO                                          
*                                                                               
         NI    SVKEY+8,X'80'                                                    
*                                                                               
         B     BLDX                                                             
         EJECT                                                                  
*================================================================               
* BUILD SVTABLE ENTRIES FROM ACTIVITY LIST                                      
* FOR OPTICA, SVTABLE STARTS AT +24 SO ROOM FOR KEY                             
* IF RUNNING AN OPTICA REQUEST, ONLY ADD ENTRIES FOR                            
* LINES IN WORKER FILE OF SELECTED ENTRIES IN SVWRKREC  MHER 7/2015             
*================================================================               
                                                                                
BLD100   DS    0H                                                               
         L     R4,AIO3                                                          
         USING ACLDATA,R4                                                       
*                                                                               
         L     R7,ASVTABLE                                                      
         USING SVTABLED,R7                                                      
*                                                                               
BLD100M  OC    SVTBLINE,SVTBLINE   FIND LAST ENTRY                              
         BZ    *+12                                                             
         LA    R7,SVTBNEXT                                                      
         B     BLD100M                                                          
*                                                                               
         L     R4,AIO3                                                          
         USING ACLDATA,R4                                                       
*                                                                               
         L     R2,NEXTSELF                                                      
         NI    1(R2),X'DF'         SET UNP                                      
         OI    6(R2),X'80'         SET XMT                                      
*                                                                               
BLD100N  MVC   SVTBMKST,TRBUYKEY+4   MOVE SAVED MKT/STA                         
*                                                                               
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BO    BLD100S                                                          
*                                                                               
         CLI   SVT2PR05,C'Y'       COMBINE CABLE NETWORKS                       
         BNE   BLD100S                                                          
*                                                                               
*                    X'E8' NOW                                                  
         CLI   SVTBMKST+2,CABLESTA THIS A CABLE STATION                         
         BL    BLD100S              NO                                          
         NI    SVTBMKST+4,X'80'                                                 
*                                                                               
BLD100S  DS   0H                                                                
         XC    WORK,WORK                                                        
         XC    STANET,STANET                                                    
         GOTO1 MSUNPK,DMCB,(X'80',SVTBMKST),WORK,WORK+4                         
         MVC   SVTBSTA,WORK+4                                                   
         CLI   SVTBSTA+4,C' '                                                   
         BNE   *+8                                                              
         MVI   SVTBSTA+4,C'T'                                                   
*                                                                               
         CLC   WORK+9(3),SPACES                                                 
         BE    BLD101                                                           
         MVC   STANET(4),WORK+4                                                 
         MVI   STANET+4,C'/'                                                    
         MVC   STANET+5(3),WORK+9                                               
         OI    SVTBIND2,SVTBICAB   SET ON CABLE HEAD STATION                    
*                                                                               
* READ STATION MASTER RECORD FOR AFFL/TYPE *                                    
*                                                                               
BLD101   XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),TRAMED                                                  
         MVC   KEY+2(5),SVTBSTA                                                 
         MVC   KEY+7(2),AGENCY                                                  
         MVC   KEY+9(3),QCLT                                                    
         MVC   AIO,AIO1                                                         
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
*                                                                               
         L     R6,AIO                                                           
         USING STARECD,R6                                                       
*                                                                               
         OC    SVOPTAFF,SVOPTAFF   IS THERE AN AFFILIATE FILTER                 
         BNZ   *+28                 NO                                          
         MVC   SVTBAFFL,SCANNTWK                                                
         CLI   SPOTCAN,C'C'        CANADIAN AGCY?                               
         BE    BLD101A                                                          
         MVC   SVTBAFFL,SNETWRK                                                 
         B     BLD101A                                                          
*                                                                               
         CLI   SPOTCAN,C'C'        CANADIAN AGCY?                               
         BNE   *+24                                                             
         CLC   SVOPTAFF,SCANNTWK   THIS REQUESTED AFFILIATE                     
         BNE   BLD120V              NO                                          
         MVC   SVTBAFFL,SCANNTWK                                                
         B     BLD101A                                                          
         CLC   SVOPTAFF,SNETWRK    THIS REQUESTED AFFILIATE                     
         BNE   BLD120V              NO                                          
         MVC   SVTBAFFL,SNETWRK                                                 
*                                                                               
BLD101A  DS    0H                  CANADIAN AGCY?                               
         MVC   SVTBTYPE,STYPE                                                   
         MVC   QMKT,SMKT           THIS FORCES STATION MARKET                   
*                                                                               
         CLI   SVTBSTA,C'0'        THIS A CABLE STATION                         
         BL    BLD101C              NO                                          
*                                                                               
         CLC   =C'7000',SVTBSTA    THIS PART OF USER CODED STATIONS             
         BH    *+14                 NO                                          
*                                                                               
         CLC   =C'7500',SVTBSTA    THIS PART OF USER CODED STATIONS             
         BNL   BLD101C              YES                                         
*                                                                               
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BO    BLD120V              YES, NO REPRINT OF AMS GROUP CODE           
*                                                                               
         OI    SVTBIND2,SVTBICGR     THIS IS A CABLE GROUP STATION              
         XC    SVTBAFFL,SVTBAFFL                                                
         MVI   SVTBTYPE,0                                                       
         DROP  R6                                                               
*                                                                               
* FORMAT STATION FOR PRINTING *                                                 
*                                                                               
BLD101C  DS    0H                                                               
         MVC   STAPRNT,SPACES                                                   
         MVC   STAPRNT(4),SVTBSTA                                               
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),SVTBSTA+4                                                
         MVI   3(RE),C'V'                                                       
         CLI   QMED,C'T'                                                        
         BE    BLD102                                                           
         MVI   3(RE),C'M'                                                       
         CLI   QMED,C'R'                                                        
         BE    BLD102                                                           
         MVI   3(RE),C' '                                                       
*                                                                               
BLD102   DS    0H                                                               
*                                                                               
* READ MARKET RECORD *                                                          
*                                                                               
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         GOTO1 (RF),(R1)                                                        
*                                                                               
         L     R6,AIO                                                           
         USING MKTRECD,R6                                                       
*                                                                               
         MVC   MKTNM,MKTNAME                                                    
         DROP  R6                                                               
*                                                                               
BLD104   MVC   SVTBPRD(4),ACLPRD   PRD/SLN/PRD2/SLN2                            
*                                                                               
         CLI   TRAWRKRH+5,0        ANY WORKER FILENUM                           
         JE    BLD104X             NO                                           
*                                                                               
         BRAS  RE,OPENWRKF         GET FIRST WORKER FILE ENTRY                  
*                                                                               
BLD104A  CLI   SVRUNALL,C'Y'       TEST PROCESS ALL ENTRIES                     
         JE    BLD104X             YES                                          
*                                                                               
* ELSE MATCH STATION/PRODUCT (NOT SLN/NOT PRD2/NOT SLN2)                        
*                                                                               
         LA    RE,SVWRKREC+4+SVTBSTA-SVTABLED                                   
         CLC   SVTBSTA(6),0(RE)    MATCH STATION/PRODUCT                        
         JE    BLD104X                                                          
         GOTO1 DATAMGR,DMCB,=C'READ',=C'WRKFILE',SVINDEX,SVWRKREC,     X        
               AWKBUFF                                                          
         CLI   8(R1),0                                                          
         JE    BLD104A               MORE RECORDS - GO COMPARE AGAIN            
         XC    SVTBDATA,SVTBDATA     ELSE CLEAR ENTRY                           
*                                                                               
         LR    R0,R7               SAVE CURRENT SVTABLE ENTRY                   
BLD104B  OC    SVTBNEXT,SVTBNEXT   IS THERE ANOTHER ENTRY                       
         JZ    BLD104C                                                          
         MVC   SVTBDATA,SVTBNEXT   MOVE TO EMPTY SLOT                           
         LA    R7,L'SVTBDATA(R7)   POINT TO NEXT ENTRY                          
         J     BLD104B                                                          
*                                                                               
BLD104C  LR    R7,R0               RESTORE SVTABLE POINTER                      
         J     BLD120                                                           
*                                                                               
BLD104X  MVC   SVTBDTS,ACLFTD                                                   
*                                                                               
         CLI   SVT1PR13,C'Y'       USE FLT/EST DATES FOR ACTIVITY               
         BNE   *+10                 NO                                          
         MVC   SVTBDTS,SVGENDTS    USE INSTR PERIOD AS FTD/LTD                  
*                                                                               
         MVC   SVTBCOPY,ACLCOPY                                                 
*                                                                               
         L     R0,=F'-1'           SET DUMMY LINE ADDRESS FOR OFFLINE           
         ST    R0,SVTBLINE                                                      
         CLI   SVXFROV,7           TEST CALL FROM SPOT/LINK                     
         JE    BLD110                                                           
         CLI   OFFLINE,C'Y'                                                     
         BE    BLD110                                                           
         CLI   SVBIGSW,C'Y'        TEST EXTENDED SVTABLE                        
         BNE   *+12                                                             
         CLI   NEXTSELF,0          TEST BEYOND USER SCREEN                      
         BNE   BLD110              YES - SKIP DISPLAY                           
*                                                                               
         L     R2,NEXTSELF         POINT TO SELECT FIELD                        
         S     R2,ATWA                                                          
         ST    R2,SVTBLINE         SET REL LINE DSPL                            
         A     R2,ATWA                                                          
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO DISPLAY AREA                        
*                                                                               
         MVC   DSPSTA,STAPRNT      MOVE STATION TO DISPLAY LINE                 
         OC    STANET,STANET                                                    
         BZ    BLD105                                                           
         MVC   DSPSTA(8),STANET                                                 
*                                                                               
BLD105   MVC   DSPMKT,MKTNM                                                     
*                                                                               
         MVC   STAPRNT,SPACES      NAMES APPEAR ONCE ONLY                       
         MVC   STANET,SPACES                                                    
         MVC   MKTNM,SPACES                                                     
*                                                                               
         USING DSPLINED,R2                                                      
*                                                                               
* BUILD DISPLAY LINE *                                                          
*                                                                               
         MVC   DSPPRD(3),ACLEBCP1                                               
         LA    R1,DSPPRD+2                                                      
         ZIC   R0,SVTBSLN                                                       
         BAS   RE,FMTSLN                                                        
*                                                                               
         MVC   DSPPRD2(3),ACLEBCP2                                              
         LA    R1,DSPPRD2+2                                                     
         ZIC   R0,SVTBSLN2                                                      
         BAS   RE,FMTSLN                                                        
*                                                                               
         LA    R1,SVTBSTR                                                       
         BAS   RE,FMTDT                                                         
         MVC   DSPFTD,WORK                                                      
*                                                                               
         LA    R1,SVTBEND                                                       
         BAS   RE,FMTDT                                                         
         MVC   DSPLTD,WORK                                                      
*                                                                               
         MVC   DSPCOPY,SVTBCOPY                                                 
         CLI   SVPROF11,C'E'       TEST COPY CODE = EST                         
         BNE   BLD106                                                           
         ZIC   R0,ACLCOPY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DSPCOPY-1(3),DUB                                                 
*                                                                               
BLD106   OI    6(R2),X'80'         SET XMT                                      
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         ST    R2,NEXTSELF         SET NEXT LINE ADDRESS                        
*                                                                               
         CLI   SVBIGSW,C'Y'        TEST EXTENDED LIST                           
         BNE   BLD110              NO                                           
         CLI   0(R2),9             TEST REACHED LAST LINE YET                   
         BH    BLD110              NO                                           
         SR    R2,R0               BACK UP TO PREVIOUS FIELD                    
         MVC   8(L'TRADSP1,R2),SPACES                                           
         MVC   8(41,R2),=C'** ADDITIONAL DATA CANNOT BE DISPLAYED **'           
         MVI   NEXTSELF,X'FF'      SET FLAG FOR BEYOND SCREEN                   
*                                                                               
BLD110   LA    R4,L'ACLDATA(R4)                                                 
         CLI   ACLPRD,0            TEST MORE DATA                               
         BE    BLD120              NO                                           
         CLI   0(R2),9             TEST REACHED EOS                             
         BH    BLD114               NO                                          
         TM    SVOPT3,OPT3MKLR                                                  
         BZ    *+8                                                              
         NI    SVOPT3,X'FF'-OPT3MKLR  OFF MARKET LIST READ                      
*                                                                               
         CLI   SVXFROV,7           TEST XFRCTL                                  
         JE    BLD114                                                           
         CLI   SVBIGSW,C'Y'        TEST KNOW IT WON'T FIT                       
         BE    *+6                                                              
         DC    H'0'                REALLY SHOULDN'T GET HERE                    
*                                                                               
* MORE DATA *                                                                   
*                                                                               
BLD114   MVC   SVTBNEXT,SVTBDATA   COPY CURRENT DATA TO NEXT                    
         LA    R7,SVTBNEXT                                                      
         C     R7,ASVTABLX                                                      
         BL    BLD104                                                           
*                                                                               
BLD118   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD+10(30),=C'TOO MUCH DATA-REQUEST OFF-LINE'                
         MVI   SVBIGSW,0                                                        
         MVC   CONHEAD(9),=C'* ERROR *'                                         
         GOTO1 ERREX2                                                           
*                                                                               
* NO MORE DATA - FORCE NEXT STA AND CONTINUE *                                  
*                                                                               
BLD120   CLI   OFFLINE,C'Y'                                                     
         BE    BLD120T             PROCESS ONE STATION AT A TIME                
         CLI   SVBIGSW,C'Y'        TEST EXTENDED SCREEN                         
         BNE   BLD120X                                                          
         CLI   SVXFROV,7           TEST CALL BY SPOT/LINK                       
         JE    BLD120X                                                          
*                                                                               
* NEED TO WRITE TWA2 WITH EXTENDED LIST BEFORE EXIT *                           
*                                                                               
         XC    DMCB(24),DMCB                                                    
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    SET TERMINAL NUMBER                          
         MVI   DMCB+8,2            SET PAGE                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,ASVTABLE                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLD120T  XC    FILENAME,FILENAME   SWITCH BACK TO STRAFFIC                      
*                                                                               
         CLC   SVKEY(4),BAGYMD     TEST SAME A-M/C/P                            
         BE    BLDX                YES - LEAVE SVKEY SET                        
*                                                                               
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BO    *+12                 YES, CK POL                                 
         CLI   SVPROF15,C'Y'       THIS TRAFFIC BUYS                            
         BNE   BLD120U                                                          
         CLI   BPRD,X'FF'          THIS TRAFFIC POL INSTR                       
         BNE   BLD120U                                                          
         CLC   SVKEY(3),BAGYMD     TEST SAME A-M/CLT                            
         BE    BLDX                                                             
*                                                                               
BLD120U  MVI   SVKEY,X'FF'         ELSE SET NO CONTINUATION                     
         B     BLDX                                                             
*                                                                               
BLD120V  XC    SVTBDATA,SVTBDATA                                                
         OI    1(R2),X'20'         SET PROTECTED AGAIN                          
         NI    6(R2),X'FF'-X'80'   SET OFF XMT                                  
*                                                                               
BLD120X  CLC   SVKEY(3),BAGYMD     TEST SAME A-M/CLT                            
         BNE   BLD121               NO - QUIT                                   
*                                                                               
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BO    BLD01                YES - CONTINUE                              
         CLI   SVPROF13,C'Y'       RUNNING PROD EQUIV                           
         BE    BLD01                YES - CONTINUE                              
*                                                                               
         CLC   SVKEY+3(1),BPRD     TEST SAME PRODUCT                            
         BE    BLD01                YES - CONTINUE                              
*                                                                               
BLD121   CLI   SVPROF15,C'Y'       THIS TRAFFIC BUYS                            
         BE    *+12                                                             
         TM    SVOPT3,OPT3REPT     THIS A REPRINT                               
         BZ    BLD122                                                           
*                                                                               
         CLI   BPRD,X'FF'          THIS TRAFFIC POL INSTR                       
         BNE   BLD122                                                           
         CLC   SVKEY(3),BAGYMD     TEST SAME A-M/CLT                            
         BE    BLD01                                                            
*                                                                               
BLD122   XC    SVTBNEXT,SVTBNEXT   CLEAR NEXT SVTB ENTRY                        
         MVI   SVKEY,X'FF'         SET NO CONTINUATION                          
         J     BLDX                                                             
         EJECT                                                                  
*                                                                               
BLDX     MVC   SYSDIR(3),=C'TRF'                                                
         MVC   SYSFIL(3),=C'TRF'                                                
*                                                                               
         CLI   TRAWRKRH+5,0        ANY WORKER FILENUM                           
         JE    BLDX2               NO                                           
*                                                                               
         OC    AWKBUFF,AWKBUFF     DID WE READ IT YET?                          
         JNZ   *+8                 YES                                          
         BRAS  RE,OPENWRKF         IF NOT, GET UNIQID NOW!                      
*                                                                               
BLDX2    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* READ TRAFFIC BUYS                                                             
*                                                                               
BLDTR    XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A32'     SET INITIAL KEY                              
         MVC   KEY+2(9),SVKEY       A-M/CLT/PRD                                 
*                                                                               
         CLI   SVPROF13,C'Y'       PROD EQUIV CLT                               
         BE    *+12                                                             
         CLI   BPRD,X'FF'          POL REQ                                      
         BNE   BLDTR01                                                          
         OI    KEY+1,X'80'                                                      
         MVI   KEY+5,X'FF'                                                      
         CLI   SVPROF13,C'Y'       PROD EQUIV CLT                               
         BNE   BLDTR01                                                          
         CLI   SVKEY+3,X'FF'                                                    
         BE    BLDTR01                                                          
         MVC   KEY+11(1),SVKEY+3    PRD                                         
*                                                                               
BLDTR01  OC    SVKEY,SVKEY         TEST CONTINUATION                            
         BZ    BLDTR02              NO                                          
         CLC   SVKEY(4),BAGYMD     TEST SAME A-M/CLT/PRD                        
         BE    BLDTR02                                                          
         CLI   SVPROF13,C'Y'       PROD EQUIV CLT                               
         BE    *+12                                                             
         CLI   BPRD,X'FF'          THIS TRAFFIC POL INSTR                       
         BNE   BLD122                                                           
         CLC   SVKEY(3),BAGYMD     TEST SAME A-M/CLT                            
         BNE   BLD122                                                           
*                                                                               
BLDTR02  MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
*                                                                               
         L     R7,AIO1                                                          
         LA    R7,2(,R7)           FOR FAKE BUYKEY                              
*                                                                               
* IF POL OR EQUIV PRD, PRD WILL BE FF IN PASSIVE KEY, REAL PROD  *              
*                                                                               
BLDTR04  CLC   KEY(11),KEYSAVE     A-M/C/PRD/MKT/STA                            
         BE    BLDTR08                                                          
*                                                                               
         CLI   SVT2PR05,C'Y'       COMBINE CABLE NETWORKS                       
         BNE   BLDTR05                                                          
*                                                                               
*        TRBUYKEY HAS BEEN CLEARED - IT IS ZEROS                                
*        CLI   TRBUYKEY+8,CABLESTA THIS A CABLE STATION                         
         CLI   KEY+8,CABLESTA THIS A CABLE STATION                              
         BL    BLDTR05              NO                                          
*                                                                               
         CLC   KEY(10),KEYSAVE     A-M/C/PRD/MKT/STA                            
         BNE   BLDTR05                                                          
         MVC   HALF(1),KEY+10                                                   
         MVC   HALF+1(1),KEYSAVE+10                                             
         NI    HALF,X'80'                                                       
         NI    HALF+1,X'80'                                                     
         CLC   HALF(1),HALF+1                                                   
         BE    BLDTR08                                                          
*                                                                               
BLDTR05  CLC   KEY(5),KEYSAVE      REC TYPE A-M OR CLT CHANGE                   
         BE    BLDTR06              NO                                          
         MVI   KEY+2,X'FF'         FORCE BREAK IN CASE ONLY TYPE CHANGE         
*                                                                               
BLDTR06  MVC   KEYSAVE(9),KEYSAVE+2    A/M, BCLT, PRD, MKT/STA                  
         MVC   SVKEY(9),KEY+2                                                   
*                                                                               
         CLI   SVPROF13,C'Y'       PROD EQUIV CLT                               
         BE    *+12                                                             
         CLI   BPRD,X'FF'          POL REQ                                      
         BNE   BLD92A                                                           
         MVC   KEYSAVE+3(1),KEYSAVE+11 BPRD                                     
         MVC   SVKEY+3(1),KEY+11                                                
         B     BLD92A                                                           
         EJECT                                                                  
* IF FILTERING ON MARKET GROUP, CHECK IT OUT *                                  
*                                                                               
BLDTR08  OC    OPTMGRP,OPTMGRP     TEST SINGLE MKT GROUP REQUEST                
         BZ    BLDTR08C                                                         
         CLC   KEY+6(2),BMKT                                                    
         BE    BLDTR08C                                                         
*                                                                               
         BRAS  RE,RMGB              O GET NEXT MARKET IN GROUP                  
*TEMP    BAS   RE,RMG              GO GET NEXT MARKET IN GROUP                  
         BNE   BLD122                                                           
*                                                                               
BLDTR08C TM    SVOPT3,OPT3MKL      MARKET LIST NAME ENTERED?                    
         BZ    BLDTR08D                                                         
         CLC   KEY+6(2),BMKT                                                    
         BE    BLDTR08D                                                         
*                                                                               
         TM    SVOPT3,OPT3MKLR     WAS MARKET LIST RECORD READ                  
         BO    BLDTR8C                                                          
         BRAS  RE,RMKTL            READ MARKET LIST RECORD                      
BLDTR8C  BRAS  RE,GETMKTL                                                       
         BNE   BLD122                                                           
*                                                                               
BLDTR08D TM    SVOPT,OPT1MKT       TEST SINGLE MKT REQUEST                      
         BZ    *+14                                                             
         CLC   KEY+6(2),BMKT                                                    
         BNE   BLD122                                                           
*                                                                               
         TM    SVOPT,OPT1STA       TEST SINGLE STATION REQUEST                  
         BZ    BLDTR08G                                                         
*                                                                               
         CLC   KEY+8(3),BSTA                                                    
         BE    BLDTR08G                                                         
*                                                                               
         CLI   SVT2PR05,C'Y'       COMBINE CABLE NETWORKS                       
         BNE   BLD122                                                           
*                                                                               
*                    X'E8' NOW                                                  
         CLI   KEY+8,CABLESTA      THIS A CABLE STATION                         
         BL    BLD122                                                           
*                                                                               
         CLC   KEY+8(2),BSTA                                                    
         BNE   BLD122                                                           
         MVC   HALF(1),KEY+10                                                   
         NI    HALF,X'80'                                                       
         CLC   HALF(1),BSTA+2                                                   
         BNE   BLD122                                                           
*                                                                               
BLDTR08G L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
* SEARCH ELEMENTS FOR BUYS IN INSTR PERIOD *                                    
*                                                                               
BLDTR10  BRAS  RE,NEXTEL                                                        
         BNE   BLDTR40                                                          
         USING TBYDTAEL,R6                                                      
         CLC   TBYCODE,QBEST       CK EQUAL EST (QBEST=0 FOR NO EST)            
         BE    *+12                                                             
*                                                                               
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BE    BLDTR10                                                          
*                                                                               
         CLI   SVOPTLEN,0          FILTER ON SPOT LEN                           
         BE    BLDTR12                                                          
         ZIC   RE,TBYSLN                                                        
         ZIC   RF,TBYSLN2                                                       
         AR    RE,RF                                                            
         CLM   RE,1,SVOPTLEN                                                    
         BNE   BLDTR10                                                          
*                                                                               
BLDTR12  GOTO1 DATCON,DMCB,(3,TBYSTART),(2,WORK)                                
         GOTO1 (RF),(R1),(3,TBYEND),(2,WORK+2)                                  
         CLC   WORK(2),SVGENEND    START AFTER FLIGHT END                       
         BH    BLDTR10                                                          
         CLC   WORK+2(2),SVGENST   END BEFORE FLIGHT START                      
         BL    BLDTR10                                                          
         EJECT                                                                  
* LIMIT BUY DATES TO INSTR DATES *                                              
*                                                                               
         CLC   WORK(2),SVGENST     IF BUY DATE BEFORE INSTR START               
         BNL   *+10                                                             
         MVC   WORK(2),SVGENST     FORCE TO INSTR START DATE                    
*                                                                               
         CLC   WORK+2(2),SVGENEND  IF BUY DATE AFTER INSTR END                  
         BNH   *+10                                                             
         MVC   WORK+2(2),SVGENEND  FORCE TO INSTR END DATE                      
*                                                                               
* BUILD ACTIVITY TABLE ENTRY FOR BUY *                                          
*                                                                               
         XC    ACLWORK,ACLWORK                                                  
         LA    R4,ACLWORK                                                       
         MVC   BYTE,KEY+TBYKPRD-TBYKEY                                          
         CLI   BYTE,X'FF'                                                       
         BNE   *+10                                                             
         MVC   BYTE,KEY+TBYPPRD-TBYKEY                                          
         BAS   RE,GETPRD                                                        
         MVC   ACLPRD,BYTE                                                      
         MVC   ACLSLN,TBYSLN                                                    
         MVC   ACLEBCP1,0(R1)                                                   
         CLI   BPRD,X'FF'          PROD POL                                     
         BE    *+14                                                             
         CLC   BYTE,BPRD                                                        
         BNE   BLDTR10                                                          
*                                                                               
         CLI   BPRD2,0             ANY PTR SELECTION                            
         BE    BLDTR16                                                          
         CLI   BPRD2,X'FF'         PTR SELECTION = NONE                         
         BNE   BLDTR14                                                          
         CLI   TBYPRD2,0           NO PTR PROD                                  
         BNE   BLDTR10                                                          
         B     BLDTR18                                                          
*                                                                               
BLDTR14  CLI   TBYPRD2,0           NO PTR PROD                                  
         BE    BLDTR10                                                          
         B     *+12                                                             
*                                                                               
BLDTR16  CLI   TBYPRD2,0                                                        
         BE    BLDTR18                                                          
         MVC   BYTE,TBYPRD2                                                     
         BAS   RE,GETPRD                                                        
         CLI   BPRD2,0             ANY PTR SELECTION                            
         BE    *+14                                                             
         CLC   BPRD2,BYTE                                                       
         BNE   BLDTR10                                                          
         MVC   ACLPRD2,BYTE                                                     
         MVC   ACLSLN2,TBYSLN2                                                  
         MVC   ACLEBCP2,0(R1)                                                   
         CLC   ACLEBCP1,ACLEBCP2                                                
         BL    BLDTR18                                                          
*                                                                               
* REVERSE THE ENTRIES *                                                         
*                                                                               
         XC    ACLPRD(2),ACLPRD2                                                
         XC    ACLPRD2(2),ACLPRD                                                
         XC    ACLPRD(2),ACLPRD2                                                
         XC    ACLEBCP1,ACLEBCP2                                                
         XC    ACLEBCP2,ACLEBCP1                                                
         XC    ACLEBCP1,ACLEBCP2                                                
*                                                                               
BLDTR18  MVC   ACLCOPY,TBYCODE                                                  
*                                                                               
         MVC   ACLFTD(4),WORK                                                   
*                                                                               
         L     R4,AIO3                                                          
*                                                                               
BLDTR20  CLI   ACLPRD,0                                                         
         BE    BLDTR28                                                          
         CLC   ACLPRD(5),ACLWORK+6    PRD/SLN/PRD2/SLN2/COPY                    
         BNE   BLDTR26                                                          
*                                                                               
* COMBINE DATES FOR EQUAL ENTRIES *                                             
*                                                                               
         CLC   ACLFTD,ACLWORK+ACLFTD-ACLDATA                                    
         BL    *+10                                                             
         MVC   ACLFTD,ACLWORK+ACLFTD-ACLDATA                                    
*                                                                               
         CLC   ACLLTD,ACLWORK+ACLLTD-ACLDATA                                    
         BH    *+10                IF LOW,                                      
         MVC   ACLLTD,ACLWORK+ACLLTD-ACLDATA                                    
*                                                                               
         CLC   ACLFTD,SVGENST      TEST PRIOR TO FLIGHT START                   
         BH    *+10                                                             
         MVC   ACLFTD,SVGENST      FORCE LAST TLCST = FLT START                 
*                                                                               
         CLC   ACLLTD,SVGENEND     CHECK AFTER FLIGHT END                       
         BNH   *+10                IF HIGH,                                     
         MVC   ACLLTD,SVGENEND     FORCE LAST TLCST = FLT END                   
         B     BLDTR10                                                          
BLDTR26  LA    R4,L'ACLDATA(R4)                                                 
         L     R0,AIO3                                                          
         A     R0,=F'2000'                                                      
         CR    R0,R4                                                            
         BH    BLDTR20                                                          
         DC    H'0'                EXCEEDED TABLE SIZE                          
BLDTR28  MVC   0(L'ACLDATA,R4),ACLWORK                                          
         B     BLDTR10                                                          
*                                                                               
BLDTR40  MVC   KEYSAVE,KEY         SAVE BUY DATA                                
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 SEQ                                                              
         B     BLDTR04                                                          
         EJECT                                                                  
* ON ENTRY R1 POINTS TO END OF PRD, R0 HAS SLN *                                
*                                                                               
FMTSLN   LTR   R0,R0                                                            
         BZR   RE                                                               
         CLI   0(R1),C' '                                                       
         BNE   *+6                                                              
         BCTR  R1,0                                                             
         MVI   1(R1),C'-'                                                       
         EDIT  (R0),(3,2(R1)),ALIGN=LEFT                                        
         BR    RE                                                               
*                                                                               
FMTDT    NTR1                                                                   
         ST    R1,DMCB                                                          
         MVI   DMCB,2                                                           
         GOTO1 DATCON,DMCB,,(8,WORK)                                            
         XIT1                                                                   
         EJECT                                                                  
* CHECK OUT MARKET GROUP RECS TO GET NEXT MARKET *                              
*                                                                               
RMG      NTR1                                                                   
*                                                                               
RMG10    MVC   ELEM(18),KEY                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(3),BAGYMD & BCLT                                           
         MVC   KEY+5(3),OPTPGRP                                                 
         MVC   KEY+8(1),SVT2PR01                                                
         MVC   KEY+9(2),OPTMGRP                                                 
         MVC   KEY+11(2),ELEM+6    THIS MARKET                                  
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     FIND MARKET GROUP                            
         BE    RMG20                YES                                         
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLIENT                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     AT END OF MARKET GROUP                       
         BNE   RMG40                YES                                         
*                                                                               
RMG20    MVC   ELEM+18(18),KEY     SAVE MARKET GROUP                            
         MVC   KEY(18),ELEM                                                     
         CLC   KEY+6(2),ELEM+18+11 SAME MARKET                                  
         BE    RMG30                YES, PROCESS IT                             
         MVC   KEY+6(2),ELEM+18+11                                              
         XC    KEY+8(3),KEY+8      ZERO STATION                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE      SEE IF SAME A-M/C/P                          
         BNE   RMGX                                                             
         CLC   KEY(8),KEYSAVE      SEE IF FOUND THIS MARKET                     
         BNE   RMG10                                                            
         MVC   KEYSAVE,KEY                                                      
         MVC   BMKT,KEY+6                                                       
         B     RMGX                                                             
*                                                                               
RMG30    GOTO1 HIGH                                                             
*                                                                               
RMG40    MVC   BMKT,KEY+6                                                       
         CR    RB,RB                                                            
RMGX     XIT1                                                                   
         EJECT                                                                  
* FIND PRODUCT IN SVCLIST *                                                     
*                                                                               
GETPRD   L     R1,ASVCLIST                                                      
*                                                                               
GETPRD10 CLC   BYTE,3(R1)                                                       
         BE    GETPRD20                                                         
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BH    GETPRD10                                                         
         DC    H'0'                                                             
GETPRD20 CLI   SVPROF13,C'Y'       PROD EQIVALENCY ACTIVE                       
         BNER  RE                   NO                                          
*                                                                               
* NOW LOOK FOR EQUIVALENT PRODUCT CODE *                                        
*                                                                               
         L     RF,ADTLIST                                                       
GETPRD30 CLC   BYTE,0(RF)                                                       
         BE    GETPRD34                                                         
         CLI   0(RF),0                                                          
         BE    GETPRD40                                                         
         LA    RF,2(,RF)                                                        
         B     GETPRD30                                                         
*                                                                               
* NOW LOOK UP EQUIVALENT PRODUCT *                                              
*                                                                               
GETPRD34 CLC   BYTE,1(RF)          IS THIS A BASE                               
         BER   RE                   YES, DONE                                   
         MVC   BYTE,1(RF)          SAVE EQUIVALENT                              
         L     R1,ASVCLIST                                                      
*                                                                               
GETPRD36 CLC   BYTE,3(R1)                                                       
         BER   RE                                                               
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BH    GETPRD36                                                         
         DC    H'0'                                                             
*                                                                               
GETPRD40 NTR1                                                                   
         L     RE,ADTLIST                                                       
         MVC   232(24,RE),KEY                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PEQKEY,R4                                                        
         MVC   PEQPID,=X'0AB7'                                                  
         MVC   PEQPAM(3),BAGYMD                                                 
         MVC   PEQPEPRD,0(R1)                                                   
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'TRFDIR'                                            
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      THIS AN EQUIVALENT PRODUCT                   
         BNE   GETPRD60             NO                                          
         EJECT                                                                  
* SEE IF PRODUCT IS ACTIVE *                                                    
*                                                                               
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'TRFFIL'                                            
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PEQDTAEL,R6                                                      
GETPRD42 CLC   PEQPROD,0(R1)                                                    
         BE    GETPRD44                                                         
         BRAS  RE,NEXTEL                                                        
         BE    GETPRD42                                                         
         DC    H'0'                                                             
GETPRD44 MVC   AIO,AIO1                                                         
         DROP  R6                                                               
*                                                                               
* PUT EQUIVALENT AND BASE IN TABLE *                                            
*                                                                               
GETPRD46 LA    RE,115                                                           
         L     RF,ADTLIST                                                       
GETPRD50 CLI   0(RF),0                                                          
         BE    GETPRD54                                                         
         LA    RF,2(,RF)                                                        
         BCT   RE,GETPRD50                                                      
         DC    H'0'                                                             
GETPRD54 MVC   0(1,RF),BYTE        SAVE EQUIV BPRD                              
         L     R1,ASVCLIST                                                      
GETPRD56 CLC   PEQPBPRD,0(R1)                                                   
         BE    GETPRD58                                                         
         LA    R1,4(,R1)                                                        
         CLI   0(R1),C' '                                                       
         BH    GETPRD56                                                         
         DC    H'0'                                                             
GETPRD58 MVC   BYTE,3(R1)          SAVE BASE BPRD                               
         MVC   1(1,RF),BYTE                                                     
         B     GETPRD70                                                         
*                                                                               
GETPRD60 LA    RE,100                                                           
         L     RF,ADTLIST                                                       
GETPRD64 CLI   0(RF),0                                                          
         BE    GETPRD66                                                         
         LA    RF,2(,RF)                                                        
         BCT   RE,GETPRD64                                                      
         DC    H'0'                                                             
GETPRD66 MVC   0(1,RF),BYTE        SAVE BASE BPRD                               
         MVC   1(1,RF),BYTE                                                     
*                                                                               
GETPRD70 L     RE,ADTLIST                                                       
         MVC   KEY(24),232(RE)                                                  
*                                                                               
         XC    FILENAME,FILENAME                                                
*                                                                               
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
*                                                                               
         XIT1  REGS=(R1)                                                        
         DROP  R4                                                               
         EJECT                                                                  
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
BUYEL    CLI   0(R6),0                                                          
         JE    BUYELX                                                           
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
BUYELX   LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
* READ INSTRUCTION RECAP RECS HERE FOR OPTION REPRINT OR RERUN                  
*                                                                               
* FOR THIS OPTION, THERE ARE NO UPDATES, AND THE SOURCE                         
* IS INSTR RECAPS, NOT MEDIA OR TRAFFIC BUYS.                                   
*                                                                               
*                                                                               
BLDINS   DS    0H                                                               
         MVI   SVT1PR9,C'N'        SET PROFILE TO PRT, FOR OPT REPRINT          
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A24'                                                  
         MVC   KEY+2(9),SVKEY       A-M/CLT/PRD/MKT/STA                         
*                                                                               
         CLI   BPRD,X'FF'          THIS PROD POL                                
         BE    BLDI300              YES                                         
*                                                                               
*                                                                               
         OC    SVKEY,SVKEY         TEST CONTINUATION                            
         BZ    BLDI050              NO                                          
         CLC   SVKEY(4),BAGYMD     TEST SAME A-M/CLT/PRD                        
         BE    BLDI050                                                          
         CLI   SVPROF13,C'Y'       PROD EQUIV CLT                               
         BE    *+12                                                             
         CLI   BPRD,X'FF'          THIS TRAFFIC POL INSTR                       
         BNE   BLD122                                                           
         CLC   SVKEY(3),BAGYMD     TEST SAME A-M/CLT                            
         BNE   BLD122                                                           
*                                                                               
         CLC   SVKEY(6),BAGYMD     TEST SAME A-M/CLT                            
         BNE   BLD122                                                           
*                                                                               
BLDI050  MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
*                                                                               
         L     R7,AIO1                                                          
         LA    R7,2(,R7)           FOR FAKE BUYKEY                              
*                                                                               
         CLC   KEY(5),KEYSAVE      TYPE/A-M/CLT                                 
         BNE   BLD122                                                           
*                                                                               
         CLC   KEY+5(1),BPRD                                                    
         BNE   BLDI200                                                          
*                                                                               
BLDI100  CLC   KEY(11),KEYSAVE                                                  
         BE    BLDI120                                                          
*                                                                               
         OC    KEYSAVE+6(7),KEYSAVE+6 THIS STARTING KEY                         
         BZ    BLDI120                                                          
*                                                                               
         CLC   KEY(5),KEYSAVE      REC TYPE A-M OR CLT CHANGE                   
         BE    BLDI114              NO                                          
         MVI   KEY+2,X'FF'         FORCE BREAK IN CASE ONLY TYPE CHANGE         
*                                                                               
BLDI114  MVC   KEYSAVE(9),KEYSAVE+2    A/M, BCLT, PRD, MKT/STA                  
         MVC   SVKEY(9),KEY+2                                                   
         B     BLD92A                                                           
         EJECT                                                                  
* IF ESTIMATE NEEDED, CHECK IT OUT *                                            
*                                                                               
BLDI120  CLI   QBEST,0                                                          
         BE    BLDI124                                                          
*                                                                               
         CLC   KEY+11(1),QBEST                                                  
         BNE   BLDI200                                                          
*                                                                               
* IF FILTERING ON MARKET GROUP, CHECK IT OUT *                                  
*                                                                               
BLDI124  OC    OPTMGRP,OPTMGRP     TEST SINGLE MKT GROUP REQUEST                
         BZ    BLDI128                                                          
         CLC   KEY+6(2),BMKT                                                    
         BE    BLDI128                                                          
*                                                                               
         BRAS  RE,RMGB             GO GET NEXT MARKET IN GROUP                  
*TEMP    BAS   RE,RMG              GO GET NEXT MARKET IN GROUP                  
         BNE   BLD122                                                           
*                                                                               
BLDI128  TM    SVOPT3,OPT3MKL      MARKET LIST NAME ENTERED?                    
         BZ    BLDI130                                                          
         CLC   KEY+6(2),BMKT                                                    
         BE    BLDI130                                                          
*                                                                               
         TM    SVOPT3,OPT3MKLR     WAS MARKET LIST RECORD READ                  
         BO    BLDI129                                                          
         BRAS  RE,RMKTL            READ MARKET LIST RECORD                      
BLDI129  BRAS  RE,GETMKTL                                                       
         BNE   BLD122                                                           
*                                                                               
BLDI130  TM    SVOPT,OPT1MKT       TEST SINGLE MKT REQUEST                      
         BZ    *+14                                                             
         CLC   KEY+6(2),BMKT                                                    
         BNE   BLD122                                                           
*                                                                               
         TM    SVOPT,OPT1STA       TEST SINGLE STATION REQUEST                  
         BZ    BLDI134                                                          
*                                                                               
         CLC   KEY+8(3),BSTA                                                    
         BNE   BLD122                                                           
*                                                                               
BLDI134  L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         USING INSDTAEL,R6                                                      
         BNE   BLDI200                                                          
*                                                                               
BLDI150  OC    SVOPTDTE,SVOPTDTE                                                
         BZ    BLDI160                                                          
         CLC   SVOPTDTE,INSDATE                                                 
         BNE   BLDI190                                                          
*                                                                               
*LDI160  CLC   INSFTD,SVGENEND                                                  
*        BH    BLDI190                                                          
*        CLC   INSLTD,SVGENST                                                   
*NOP     BL    BLDI190                                                          
*                                                                               
* COMPARE START AND END PERIOD FOR PATTERN DATES TO INSTRUCTION DATES           
BLDI160  CLC   INSPERST,SVGENEND                                                
         BH    BLDI190                                                          
         CLC   INSPERND,SVGENST                                                 
         BL    BLDI190                                                          
*                                                                               
         CLI   SVOPTLEN,0          FILTER ON SPOT LEN                           
         BE    BLDI161                                                          
         ZIC   RE,INSSLN1                                                       
         ZIC   RF,INSSLN2                                                       
         AR    RE,RF                                                            
         CLM   RE,1,SVOPTLEN                                                    
         BNE   BLDI190                                                          
*                                                                               
BLDI161  CLI   BPRD,X'FF'          POL REQ                                      
         BE    BLDI164                                                          
         CLC   INSPRD1,BPRD                                                     
         BNE   BLDI190                                                          
*                                                                               
         CLI   BPRD2,0             ANY PTR SELECTION                            
         BE    BLDI164                                                          
         CLI   BPRD2,X'FF'         PTR SELECTION = NONE                         
         BNE   BLDI162                                                          
         CLI   INSPRD2,0           NO PTR PROD                                  
         BNE   BLDI190                                                          
         B     BLDI164                                                          
BLDI162  CLC   INSPRD2,BPRD2                                                    
         BNE   BLDI190                                                          
*                                                                               
* COMPARE START/END DATES FOR THIS PATTERN TO INSTR START/END DATES             
BLDI164  LLC   R1,INSDTALN         GET ELEMENT LENGTH                           
         AHI   R1,-(INSPTTN-INSDTAEL)                                           
         SR    R0,R0                                                            
         D     R0,=F'7'                                                         
         LTR   R0,R0                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    R0,R1               SET FOR BCT ON NUM OF ENTRIES                
         LA    R1,INSPTTN                                                       
*                                                                               
BLDI164B CLC   3(2,R1),SVGENEND  1ST TLCST TO PERIOD END                        
         BH    BLDI164C                                                         
         CLC   5(2,R1),SVGENST   LAST TLCST TO PERIOD STR                       
         BNL   BLDI165                                                          
*                                                                               
BLDI164C LA    R1,7(R1)            GET NEXT SUBELEM                             
         BCT   R0,BLDI164B                                                      
         B     BLDI190             GET NEXT ELEM                                
*                                                                               
BLDI165  ST    R1,FULL             SAVE ADDR OF THIS SUBELEM                    
         STC   R0,CNTR              AND COUNTER                                 
*                                                                               
* BUILD ACTIVITY TABLE ENTRY FOR STATION LIST *                                 
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    *+12                                                             
         TM    WHEN,X'18'          THIS A DDS OR OV REQUEST                     
         BNZ   BLD92X               YES, QUIT                                   
*                                                                               
         XC    ACLWORK,ACLWORK                                                  
         LA    R4,ACLWORK                                                       
         USING ACLDATA,R4                                                       
*                                                                               
         MVC   ACLFTD,3(R1)        INSFTD                                       
         MVC   ACLLTD,5(R1)        INSLTD                                       
*                                                                               
         MVC   BYTE,INSPRD1                                                     
         BAS   RE,GETPRD                                                        
         MVC   ACLPRD,BYTE                                                      
         MVC   ACLSLN,INSSLN1                                                   
         MVC   ACLEBCP1,0(R1)                                                   
         MVC   SVINSDT,0(R1)                                                    
*                                                                               
         CLI   INSPRD2,0                                                        
         BE    BLDI166                                                          
         MVC   BYTE,INSPRD2                                                     
         BAS   RE,GETPRD                                                        
         MVC   ACLPRD2,BYTE                                                     
         MVC   ACLSLN2,INSSLN2                                                  
         MVC   ACLEBCP2,0(R1)                                                   
*                                                                               
*LDI166  DS   0H                                                                
*NOP     MVC   ACLFTD,INSFTD                                                    
*        ZIC   RE,1(R6)                                                         
*        LA    R5,0(R6,RE)                                                      
*        AHI   R5,-2                                                            
*                                                                               
*******  MVC   ACLLTD,0(R5)                                                     
*                                                                               
BLDI166  MVC   ACLCOPY,KEY+11                                                   
*                                                                               
         L     R4,AIO3                                                          
         LR    R0,R4                                                            
         AH    R0,=H'1500'                                                      
BLDI170  CLI   ACLPRD,0                                                         
         BE    BLDI180                                                          
*                                                                               
W        USING ACLDATA,ACLWORK                                                  
         CLC   ACLPRD(5),W.ACLPRD  SAME PRDS/LENS/EST                           
         BNE   BLDI174                                                          
*                                                                               
         CLC   ACLLTD,W.ACLLTD     EXPAND DATES                                 
         BNL   *+10                                                             
         MVC   ACLLTD,W.ACLLTD                                                  
*                                                                               
         CLC   ACLFLG,W.ACLFLG                                                  
         BNH   *+10                                                             
         MVC   ACLFLG,W.ACLFLG                                                  
         B     BLDI184                                                          
*                                                                               
         DROP  W                                                                
*                                                                               
BLDI174  LA    R4,L'ACLDATA(,R4)                                                
         CR    R0,R4                                                            
         BH    BLDI170                                                          
         DC    H'0'                EXCEEDED TABLE SIZE                          
BLDI180  MVC   0(L'ACLDATA,R4),ACLWORK                                          
*                                                                               
BLDI184  L     R1,FULL             ADDR OF LAST SUBELEM                         
         LLC   R0,CNTR              AND COUNTER                                 
         LA    R1,7(R1)            GET NEXT SUBELEM                             
         BCT   R0,BLDI164B                                                      
*                                                                               
BLDI190  BRAS  RE,NEXTEL           GET NEXT ELEM                                
         BE    BLDI150                                                          
*                                                                               
BLDI200  MVC   KEYSAVE,KEY         SAVE BUY DATA                                
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 SEQ                                                              
         B     BLDI100                                                          
         EJECT                                                                  
* READ ALL MARKET/STATIONS, STORING MKT/STA/QPRD/BPRD WITH TSAROFF *            
*                                                                               
BLDI300  CLI   OFFLINE,C'Y'                                                     
         BE    *+12                                                             
         TM    WHEN,X'38'          THIS A SOON, DDS, OR OV REQUEST              
         BNZ   BLD92X               YES, QUIT                                   
*                                                                               
         CLC   SVKEY(3),BAGYMD     TEST SAME A-M/CLT                            
         BNE   BLD122                                                           
*                                                                               
         OC    ATSAR,ATSAR         THIS FIRST TIME THRU                         
         BNZ   BLDI600              NO, GET FROM LIST                           
*                                                                               
* INITIALIZE TSAROFF                                                            
*                                                                               
         MVI   *,0                                                              
*                                                                               
         LA    R7,TSARBLK                                                       
         LR    RE,R7                                                            
         LA    RF,TSARDL2                                                       
         XCEF                                                                   
         USING TSARD,R7                                                         
         L     R0,=A(21*5000)                                                   
         ST    R0,TSAREC                                                        
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ST    R1,TSABUF                                                        
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSKEYL,5+L'ACLDATA                                               
         MVI   TSRECL+1,5+L'ACLDATA                                             
*                                                                               
* TSAROFF CALLOV HERE                                                           
*                                                                               
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A7D')                                 
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ATSAR                                                         
*                                                                               
         MVI   TSOFFACT,TSAINI      SET 1ST TSAROFF FOR INIT                    
         GOTO1 ATSAR,(R7)                                                       
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   KEY+5,0                                                          
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
*                                                                               
BLDI400  CLC   KEY(5),KEYSAVE      TYPE/A-M/CLT                                 
         BNE   BLDI600                                                          
*                                                                               
         CLC   KEY+11(1),QBEST                                                  
         BNE   BLDI500                                                          
*                                                                               
* IF FILTERING ON MARKET GROUP, CHECK IT OUT *                                  
*                                                                               
         OC    OPTMGRP,OPTMGRP     TEST SINGLE MKT GROUP REQUEST                
         BZ    BLDI420                                                          
         CLC   KEY+6(2),BMKT                                                    
         BE    BLDI420                                                          
*                                                                               
         BRAS  RE,RMGB             GO GET NEXT MARKET IN GROUP                  
*TEMP    BAS   RE,RMG              GO GET NEXT MARKET IN GROUP                  
         BNE   BLDI500                                                          
*                                                                               
BLDI420  TM    SVOPT3,OPT3MKL      MARKET LIST NAME ENTERED?                    
         BZ    BLDI430                                                          
         CLC   KEY+6(2),BMKT                                                    
         BE    BLDI430                                                          
*                                                                               
         TM    SVOPT3,OPT3MKLR     WAS MARKET LIST RECORD READ                  
         BO    BLDI425                                                          
         BRAS  RE,RMKTL            READ MARKET LIST RECORD                      
BLDI425  BRAS  RE,GETMKTL                                                       
         BNE   BLDI500                                                          
*                                                                               
BLDI430  TM    SVOPT,OPT1MKT       TEST SINGLE MKT REQUEST                      
         BZ    *+14                                                             
         CLC   KEY+6(2),BMKT                                                    
         BNE   BLDI500                                                          
*                                                                               
         TM    SVOPT,OPT1STA       TEST SINGLE STATION REQUEST                  
         BZ    BLDI434                                                          
*                                                                               
         CLC   KEY+8(3),BSTA                                                    
         BNE   BLDI500                                                          
*                                                                               
BLDI434  L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         USING INSDTAEL,R6                                                      
         BNE   BLDI500                                                          
*                                                                               
BLDI450  OC    SVOPTDTE,SVOPTDTE                                                
         BZ    BLDI460                                                          
         CLC   SVOPTDTE,INSDATE                                                 
         BNE   BLDI490                                                          
*                                                                               
*LDI460  CLC   INSFTD,SVGENEND                                                  
*        BH    BLDI490                                                          
*        CLC   INSLTD,SVGENST                                                   
*NOP     BL    BLDI490                                                          
*                                                                               
* COMPARE START AND END PERIOD FOR PATTERN DATES TO INSTRUCTION DATES           
BLDI460  CLC   INSPERST,SVGENEND                                                
         BH    BLDI490                                                          
         CLC   INSPERND,SVGENST                                                 
         BL    BLDI490                                                          
*                                                                               
* COMPARE START/END DATES FOR THIS PATTERN TO INSTR START/END DATES             
BLDI464  LLC   R1,INSDTALN         GET ELEMENT LENGTH                           
         AHI   R1,-(INSPTTN-INSDTAEL)                                           
         SR    R0,R0                                                            
         D     R0,=F'7'                                                         
         LTR   R0,R0                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LR    R0,R1               SET FOR BCT ON NUM OF ENTRIES                
         LA    R1,INSPTTN                                                       
*                                                                               
BLDI464B CLC   3(2,R1),SVGENEND  1ST TLCST TO PERIOD END                        
         BH    BLDI464C                                                         
         CLC   5(2,R1),SVGENST   LAST TLCST TO PERIOD STR                       
         BNL   BLDI465                                                          
*                                                                               
BLDI464C LA    R1,7(R1)            GET NEXT SUBELEM                             
         BCT   R0,BLDI464B                                                      
         B     BLDI490             GET NEXT ELEM                                
*                                                                               
BLDI465  ST    R1,FULL             SAVE ADDR OF THIS SUBELEM                    
         STC   R0,CNTR              AND COUNTER                                 
*                                                                               
* BUILD ACTIVITY TABLE ENTRY FOR STATION LIST *                                 
*                                                                               
         XC    ACLWORK,ACLWORK                                                  
         LA    R4,ACLWORK                                                       
*                                                                               
         MVC   ACLFTD,3(R1)        INSFTD                                       
         MVC   ACLLTD,5(R1)        INSLTD                                       
*                                                                               
         MVC   BYTE,INSPRD1                                                     
         BAS   RE,GETPRD                                                        
         MVC   ACLPRD,BYTE                                                      
         MVC   ACLSLN,INSSLN1                                                   
         MVC   ACLEBCP1,0(R1)                                                   
         MVC   SVINSDT,0(R1)                                                    
*                                                                               
         CLI   INSPRD2,0                                                        
         BE    BLDI466                                                          
         MVC   BYTE,INSPRD2                                                     
         BAS   RE,GETPRD                                                        
         MVC   ACLPRD2,BYTE                                                     
         MVC   ACLSLN2,INSSLN2                                                  
         MVC   ACLEBCP2,0(R1)                                                   
*                                                                               
*LDI466  DS   0H                                                                
*        MVC   ACLFTD,INSFTD                                                    
*        ZIC   RE,1(R6)                                                         
*        LA    R5,0(R6,RE)                                                      
*        SH    R5,=H'3'                                                         
*                                                                               
*                                                                               
*NOP     MVC   ACLLTD,0(R5)                                                     
*                                                                               
BLDI466  MVC   ACLCOPY,KEY+11                                                   
*                                                                               
         XC    ELEM(128),ELEM                                                   
         MVC   ELEM(5),KEY+6                                                    
         MVC   ELEM+5(L'ACLWORK),ACLWORK                                        
         LA    R0,ELEM                                                          
         ST    R0,TSAREC                                                        
         OC    ELEM(5),ELEM                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVI   TSOFFACT,TSAADD                                                  
         GOTO1 ATSAR,(R7)                                                       
         CLI   TSERRS,0            ANY ERRORS                                   
         BE    *+6                                                              
         DC    H'0'                EXCEEDED TABLE SIZE                          
         DROP  R7                                                               
*                                                                               
         L     R1,FULL             ADDR OF LAST SUBELEM                         
         LLC   R0,CNTR              AND COUNTER                                 
         LA    R1,7(R1)            GET NEXT SUBELEM                             
         BCT   R0,BLDI464B                                                      
*                                                                               
BLDI490  BRAS  RE,NEXTEL                                                        
         BE    BLDI450                                                          
*                                                                               
BLDI500  MVC   KEYSAVE,KEY         SAVE BUY DATA                                
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 SEQ                                                              
         B     BLDI400                                                          
*                                                                               
* NOW READ BACK ENTRIES IN MARKET/STATION/QPROD ORDER *                         
*                                                                               
BLDI600  DS    0H                                                               
         L     R4,AIO3                                                          
         XC    ELEM(128),ELEM                                                   
         MVC   ELEM(5),SVKEY+4                                                  
*                                                                               
         LA    R7,TSARBLK                                                       
         USING TSARD,R7                                                         
         MVI   TSOFFACT,TSARDH                                                  
         GOTO1 ATSAR,(R7)                                                       
         TM    TSERRS,TSEEOF       EOF?                                         
         BO    BLDI650                                                          
         TM    TSERRS,TSERNF       REC NOT FOUND                                
         BO    BLDI616                                                          
         CLI   TSERRS,0            ANY ERRORS                                   
         BE    BLDI616                                                          
         DC    H'0'                EXCEEDED TABLE SIZE                          
*                                                                               
BLDI610  XC    0(L'ACLWORK,R4),0(R4)                                            
*                                                                               
         MVI   TSOFFACT,TSANXT                                                  
*                                                                               
         GOTO1 ATSAR,(R7)                                                       
         TM    TSERRS,TSEEOF       EOF?                                         
         BO    BLDI650                                                          
         CLI   TSERRS,0            ANY ERRORS                                   
         BE    BLDI616                                                          
         DC    H'0'                EXCEEDED TABLE SIZE                          
         DROP  R7                                                               
BLDI616  OC    SVKEY+4(5),SVKEY+4                                               
         BNZ   *+10                                                             
         MVC   SVKEY+4(5),ELEM                                                  
*                                                                               
         CLC   SVKEY+4(5),ELEM                                                  
         BNE   BLDI680                                                          
*                                                                               
         MVC   0(L'ACLDATA,R4),ELEM+5                                           
         LA    R4,L'ACLDATA(,R4)                                                
         B     BLDI610                                                          
*                                                                               
BLDI650  MVI   SVKEY,X'FF'                                                      
*                                                                               
* GIVE BACK MEMORY USED *                                                       
*                                                                               
         USING TSARD,R7                                                         
         MVI   *,0                                                              
         L     R1,TSABUF                                                        
         SR    R0,R0                                                            
         ICM   R0,7,TSAREC                                                      
         FREEMAIN RC,A=(1),LV=(0)                                               
*                                                                               
BLDI680  L     R7,AIO1             FOR FAKE BUY KEY                             
         XC    0(13,R7),0(R7)                                                   
         MVC   0(3,R7),BAGYMD & BCLT                                            
         MVC   4(5,R7),SVKEY+4                                                  
*                                                                               
         MVC   SVKEY+4(5),ELEM                                                  
         MVC   KEYSAVE+4(5),ELEM                                                
         B     BLD92A                                                           
TSARBLK  DS    CL(TSARDL2)                                                      
CNTR     DS    XL1                 SUBELEM COUNTER                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* FIND COPY CODE IF ANY, 1ST FROM EST, WHICH OVERRIDES ALL ELSE                 
* THEN FROM BUY IF DPT=COPY CODE PROFILE IS ON                                  
* THEN CHANGE FROM EQUIVALENCY TABLE IF PROFILE ON AND TABLE ON FILE            
*                                                                               
*                                                                               
CPY      NMOD1 0,**CPY*                                                         
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         USING ACLDATA,R4                                                       
         USING BUYRECD,R7                                                       
         ZIC   RF,BUYKEST                                                       
         CLI   SVPROF11,C'E'      COPY CODE = ESTIMATE                          
         BE    CPY40                                                            
         LA    RF,SVESTAB(RF)                                                   
*                                                                               
         CLI   SVPROF11,C'P'      USING 1ST CHAR OF PROGRAM CODE                
         BE    CPY24                YES, ALL OKAY                               
*                                                                               
         CLI   0(RF),X'FF'        COPY CODE FROM ESTIMATE                       
         BE    CPY14               NO                                           
         MVC   ACLCOPY,0(RF)                                                    
         CLI   OFFLINE,C'Y'                                                     
         BE    CPYX                                                             
         CLI   SVPROF11,C'D'      ERROR IF DPT=COPY CODE ON                     
         BE    CPY10                                                            
         CLI   SVPROF11,C'Y'      ERROR IF DPT=COPY CODE ON                     
         BE    CPY10                                                            
*                                                                               
CPY10    CLC   EQVEST,BUYKEY+9    EQUIV TABLE FOR THIS EST                      
         BNE   CPYX                                                             
         B     CPYESTER                                                         
*                                                                               
CPY14    CLI   SVPROF11,C'A'       TEST ADJACENCY CODE                          
         BE    CPY16                                                            
         CLI   SVPROF11,C'D'       TEST DPT=COPY CODE                           
         BE    CPY16                                                            
         CLI   SVPROF11,C'Y'       TEST DPT=COPY CODE                           
         BNE   CPYX                                                             
*                                                                               
CPY16    MVC   ACLCOPY,BDDAYPT                                                  
*                                                                               
         CLI   SVPROF11,C'A'       USE ADJACENCY CODE                           
         BNE   CPY30                                                            
         CLI   BDPROGT,C'9'                                                     
         BH    ADJCDER                                                          
         BE    CPY20                                                            
         CLI   BDPROGT,C'0'                                                     
         BNH   CPY20                                                            
         CLI   BDPROGT,C'Z'                                                     
         BH    ADJCDER                                                          
         BE    CPY20                                                            
         CLI   BDPROGT,C'A'                                                     
         BL    ADJCDER                                                          
CPY20    MVC   ACLCOPY,BDPROGT     SAVE PROGRAM ADJACENCY CODE                  
         B     CPY30                                                            
*                                                                               
CPY24    MVC   ACLCOPY,BDPROGRM    SAVE PROGRAM NAME CODE                       
         B     CPYX                                                             
*                                                                               
CPY30    CLC   EQVEST,BUYKEY+9     IS THERE A TABLE FOR THIS EST                
         BNE   CPYX                                                             
         LA    R0,14                                                            
         LA    R1,EQVTAB                                                        
CPY34    CLC   ACLCOPY,0(R1)       TEST DPT IN EQUIV LIST                       
         BE    CPY36                                                            
         LA    R1,2(R1)                                                         
         BCT   R0,CPY34                                                         
         B     CPYX                                                             
CPY36    MVC   ACLCOPY,1(R1)       MOVE IN EQUIV COPY CODE                      
         B     CPYX                                                             
CPY40    MVC   ACLCOPY,BUYKEST     MOVE IN EST AS COPY CODE                     
*                                                                               
CPYX     XIT1                                                                   
*                                                                               
CPYESTER MVC   GERROR,=Y(BADCCEST) COPY CODED EST XXX HAS EQUIV TABLE           
         LLC   R0,BUYKEY+9                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ERRTEXT(3),DUB                                                   
         GOTO1 VTRAERR                                                          
*                                                                               
ADJCDER  MVC   GERROR,=Y(BADADJCD)  ADJACENCY CD NOT A-Z OR 0-9                 
         GOTO1 VTRAERR                                                          
*                                                                               
         DROP  RB,RC,R4,R7                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* READ MARKET LIST RECORD & SAVE IT IN THE LAST 120 BYTES OF ELEM               
*                                                                               
RMKTL    NMOD1 0,*RMKT*                                                         
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         MVC   SYSDIR(3),=C'TRF'                                                
         MVC   SYSFIL(3),=C'TRF'                                                
*                                                                               
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING MKLKEY,R1                                                        
         MVC   MKLKID,=XL2'0A38'                                                
         MVC   MKLKAM,BAGYMD                                                    
         MVC   MKLKCLT,BCLT        MOVE IN CLIENT                               
         MVC   MKLKBPRD,BPRD            PRODUCT CODE                            
         MVC   MKLKBEST,QBEST           ESTIMATE CODE                           
         MVC   MKLKLNAM,SVMKTLST        MARKET LIST NAME                        
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(12),KEYSAVE     MARKET LIST FOUND ?                          
         BE    RMKTL20              YES                                         
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         MVI   MKLKBEST,0          CLEAR ESTIMATE CODE                          
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE     MARKET LIST FOUND ?                          
         BE    RMKTL20              YES                                         
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         MVI   MKLKBPRD,0          CLEAR PRODUCT CODE                           
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE     MARKET LIST FOUND ?                          
         BE    *+6                                                              
         DC    H'0'                BUG, VOPT SHOULD HAVE STOPPED THIS           
*                                                                               
         DROP  R1                                                               
*                                                                               
RMKTL20  MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    SVBMKT,SVBMKT       ARE WE IN THE MIDS                           
         BZ    *+10                                                             
         MVC   BMKT,SVBMKT         MOVE IN LAST MARKET PROCESSED                
*                                                                               
         XC    ELEM+130(120),ELEM+120 CLEAR MARKET LIST AREA                    
         LA    R1,ELEM+130                                                      
*                                                                               
RMKTL30  MVC   0(2,R1),2(R6)       SAVE MARKET IN ELEM                          
         LA    R1,2(R1)                                                         
         BRAS  RE,NEXTEL                                                        
         BE    RMKTL30                                                          
*                                                                               
         OI    SVOPT3,OPT3MKLR     MARKET LIST RECORD READ                      
*                                                                               
         MVC   SYSDIR(3),=C'SPT'                                                
         MVC   SYSFIL(3),=C'SPT'                                                
*                                                                               
         XIT1                                                                   
         DROP  RB,RC                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* GET NEXT MARKET FROM THE MARKET LIST                                          
*                                                                               
GETMKTL  NMOD1 0,*GETM*                                                         
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
GETMKT05 LA    R1,ELEM+130                                                      
*                                                                               
GETMKT10 CLC   0(2,R1),BMKT                                                     
         BH    GETMKT20                                                         
         LA    R1,2(R1)                                                         
         OC    0(2,R1),0(R1)                                                    
         BZ    GETMKT25                                                         
         B     GETMKT10                                                         
*                                                                               
GETMKT20 MVC   BMKT,0(R1)          SAVE NEXT MARKET CODE                        
         MVC   KEY(4),SVKEY                                                     
         MVC   KEY+4(2),0(R1)                                                   
         XC    KEY+6(3),KEY+6                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE      A/M/C/P                                      
         BNE   GETMKT25                                                         
         CLC   KEY(6),KEYSAVE      THIS MARKET?                                 
         BNE   GETMKT05                                                         
         MVC   SVBMKT,BMKT                                                      
         MVC   KEYSAVE,KEY                                                      
         CR    RB,RB                                                            
         B     GETMKTX                                                          
*                                                                               
GETMKT25 XC    BMKT,BMKT                                                        
         OI    SVOPT3,OPT3MKLX                                                  
         CR    RB,RC                                                            
GETMKTX  XIT1                                                                   
         DROP  RB,RC                                                            
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
* CHECK OUT MARKET GROUP RECS TO GET NEXT MARKET - SPOT BUYS *                  
*                                                                               
RMGB     NMOD1 0,*RMGB*,RA                                                      
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
RMGB10   MVC   ELEM(18),KEY                                                     
         CLI   SVPROF15,C'Y'       USE TRAFFIC, NOT SPOT BUYS                   
         BE    *+14                                                             
         MVC   ELEM+40(2),KEY+4    SAVE MARKET                                  
         B     *+10                                                             
         MVC   ELEM+40(2),KEY+6    SAVE MARKET                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(3),BAGYMD & BCLT                                           
         MVC   KEY+5(3),OPTPGRP                                                 
         MVC   KEY+8(1),SVT2PR01                                                
         MVC   KEY+9(2),OPTMGRP                                                 
         MVC   KEY+11(2),ELEM+40   THIS MARKET                                  
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     FIND MARKET GROUP                            
         BE    RMGB20               YES                                         
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLIENT                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     AT END OF MARKET GROUP                       
         BNE   RMGBX                YES                                         
*                                                                               
RMGB20   MVC   ELEM+18(18),KEY     SAVE MARKET GROUP                            
         MVC   KEY(18),ELEM                                                     
*                                                                               
         CLI   SVPROF15,C'Y'       USE TRAFFIC, NOT SPOT BUYS                   
         BE    RMGB25                                                           
*                                                                               
         CLC   KEY+4(2),ELEM+18+11 SAME MARKET                                  
         BNE   *+14                                                             
         MVC   BMKT,KEY+4                                                       
         B     RMGB30               YES, PROCESS IT                             
         MVC   KEY+4(2),ELEM+18+11                                              
         XC    KEY+6(3),KEY+6      ZERO STATION                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE      SEE IF SAME A-M/C/P                          
         BNE   RMGBX                                                            
         CLC   KEY(6),KEYSAVE      SEE IF FOUND THIS MARKET                     
         BNE   RMGB10                                                           
         MVC   KEYSAVE,KEY                                                      
         MVC   BMKT,KEY+4                                                       
         B     RMGB40                                                           
*                                                                               
RMGB25   CLC   KEY+6(2),ELEM+18+11 SAME MARKET                                  
         BNE   *+14                                                             
         MVC   BMKT,KEY+6                                                       
         B     RMGB30               YES, PROCESS IT                             
         MVC   KEY+6(2),ELEM+18+11                                              
         XC    KEY+8(3),KEY+8      ZERO STATION                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE      SEE IF SAME A-M/C/P                          
         BNE   RMGBX                                                            
         CLC   KEY(8),KEYSAVE      SEE IF FOUND THIS MARKET                     
         BNE   RMGB10                                                           
         MVC   KEYSAVE,KEY                                                      
         MVC   BMKT,KEY+6                                                       
         B     RMGB40                                                           
*                                                                               
RMGB30   GOTO1 HIGH                                                             
*                                                                               
RMGB40   DS    0H                                                               
         B     TEMP10                                                           
****                                                                            
         MVC   P+2(5),=C'WORX='                                                 
         GOTO1 HEXOUT,DMCB,KEY,P+7,18                                           
         MVC   P+50(4),=C'KEY='                                                 
         GOTO1 HEXOUT,DMCB,WORK,P+54,18                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
TEMP10   XC    FILENAME,FILENAME                                                
         CR    RB,RB                                                            
         XIT1                                                                   
RMGBX    DS   0H                                                                
         B     TEMP20                                                           
****                                                                            
         MVC   P+2(5),=C'WORX='                                                 
         GOTO1 HEXOUT,DMCB,KEY,P+7,18                                           
         MVC   P+50(4),=C'KEY='                                                 
         GOTO1 HEXOUT,DMCB,WORK,P+54,18                                         
         GOTO1 SPOOL,DMCB,(R8)                                                  
TEMP20   XC    FILENAME,FILENAME                                                
         CR    RB,RC                                                            
         XIT1                                                                   
*                                                                               
         DROP  RB,RA,RC                                                         
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
**********************************************************************          
* SUBROUTINE USED ONLY FOR OPTION TO TREAT DAYPARTS AS COPY CODES    *          
* PATTERN RECORDS ARE READ TO DETERMINE IF ANY EXIST FOR THIS MARKET *          
* OR STATION. IF THERE ARE NONE, THE COPY CODE IS RESET, AND AT THE  *          
* END, ENTRIES WITH DUPLICATE COPY CODES ARE MERGED                  *          
**********************************************************************          
*                                                                               
CHKPTN   NMOD1 0,**CKPT**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         MVC   WORK+20(13),KEY                                                  
         MVC   WORK+33(13),KEYSAVE                                              
*                                                                               
         L     R4,AIO3             GET ACLTAB ADDRESS                           
         USING ACLD,R4                                                          
*                                                                               
CPTN02   CLI   ACLCOPY,0           TEST DEFAULT COPY CODE                       
         BE    CPTN50              YES - IGNORE                                 
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A22'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(5),ACLPRD     PRD/SLN/PRD2/SLN2/COPY                       
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'TRFDIR'                                            
         GOTO1 HIGH                                                             
         B     CPTN42                                                           
*                                                                               
CPTN04   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'TRFFIL'                                            
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'        GET PATTERN DATA ELEMENT                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PATDTAEL,R6                                                      
*                                                                               
         GOTO1 DATCON,DMCB,(2,ACLFTD),(3,DUB)                                   
         GOTO1 (RF),(R1),(2,ACLLTD),(3,DUB+3)                                   
*                                                                               
CPTN10   CLC   DUB+3(3),PATSTART   LAST TLCST BEFOR PATTERN START               
         BL    CPTN40                                                           
         CLC   DUB(3),PATEND       FIRST TLCST AFTER PATTERN END                
         BH    CPTN40                                                           
         CLI   1(R6),38            TEST EXTENDED ELEMENT                        
         BNH   *+12                                                             
         TM    PATSTAT,X'80'       TEST STATUS=DELETED                          
         BO    CPTN40                                                           
*                                                                               
* NOW TEST IF THIS PATTERN APPLIES TO THIS MARKET OR STATION *                  
*                                                                               
         MVI   ELCODE,X'20'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PATLSTEL,R6                                                      
*                                                                               
         CLI   2(R6),C'M'          TEST MARKET LIST                             
         BNE   CPTN12                                                           
         OC    PATLST,PATLST       TEST ALL MKT PATTERN                         
         BE    CPTN50              YES                                          
*                                                                               
* SET TO PROCESS MARKET OR STATION LIST *                                       
*                                                                               
CPTN12   ZIC   R0,1(R6)                                                         
         SRDL  R0,32                                                            
         D     R0,=F'5'                                                         
         LR    R0,R1                                                            
         LA    R6,2(R6)                                                         
         CLI   0(R6),C'M'          TEST MKT PATTERN                             
         BNE   CPTN16                                                           
*                                                                               
CPTN14   CLC   TRBUYKEY+4(2),4(R6)    MATCH MKT CODES                           
         BE    CPTN50                                                           
         LA    R6,5(R6)                                                         
         BCT   R0,CPTN14                                                        
         B     CPTN40                                                           
*                                                                               
CPTN16   CLI   0(R6),C'S'          TEST STATION LIST                            
         BNE   CPTN20                                                           
*                                                                               
         GOTO1 MSUNPK,DMCB,TRBUYKEY+4,DUB,WORK+2                                
*                                                                               
CPTN18   CLC   WORK+2(5),1(R6)                                                  
         BE    CPTN50                                                           
         CLI   QMED,C'T'                                                        
         BNE   *+14                                                             
         CLC   WORK+2(4),1(R6)                                                  
         BE    CPTN50                                                           
         LA    R6,5(R6)                                                         
         BCT   R0,CPTN18                                                        
         B     CPTN40                                                           
*                                                                               
* MUST BE AFFILIATE OR STATION TYPE                                             
*                                                                               
* READ STATION MASTER RECORD FOR AFFL/TYPE *                                    
*                                                                               
CPTN20   MVI   WORK,C'0'                                                        
         MVC   WORK+1(14),WORK                                                  
         MVI   WORK,C'S'                                                        
         MVC   WORK+1(1),TRAMED                                                 
         GOTO1 MSUNPK,DMCB,TRBUYKEY+4,DUB,WORK+18                               
         MVC   WORK+2(5),WORK+18                                                
         CLI   WORK+2+4,C' '                                                    
         BNE   *+8                                                              
         MVI   WORK+2+4,C'T'                                                    
         MVC   WORK+7(2),AGENCY                                                 
         MVC   WORK+9(3),QCLT                                                   
         L     R2,AIO2                                                          
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',WORK,(R2)                    
         CLC   WORK(15),0(R2)                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING STARECD,R2                                                       
*                                                                               
         CLI   0(R6),C'A'          AFFILIATE                                    
         BNE   CPTN30                                                           
*                                                                               
         CLC   1(3,R6),SNETWRK                                                  
         BE    CPTN50                                                           
         B     CPTN40                                                           
CPTN30   CLI   0(R6),C'T'          TYPE                                         
         BNE   CPTN40                                                           
*                                                                               
         CLC   1(1,R6),STYPE                                                    
         BE    CPTN50                                                           
         DROP  R2                                                               
*                                                                               
CPTN40   DS    0H                  NO MATCH - CONTINUE READING                  
         SR    RE,RE                                                            
         ICM   RE,7,KEY+10         GET REF/SUBL                                 
         SRL   RE,10               DROP SUBLINE                                 
         LA    RE,1(RE)            BUMP LINE NUM                                
         SLL   RE,10                                                            
         STCM  RE,7,KEY+10                                                      
         OC    KEY+10(3),KEY+10    TEST REACHED END                             
         BE    CPTN44                                                           
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'TRFDIR'                                            
         GOTO1 HIGH                                                             
*                                                                               
CPTN42   CLC   KEY(10),KEYSAVE                                                  
         BE    CPTN04                                                           
*                                                                               
CPTN44   DS   0H                                                                
         CLI   SVPROF11,C'P'      USING 1ST CHAR OF PROGRAM CODE                
         BE    CPTN50               DO NOT BLANK                                
*                                                                               
         MVI   ACLCOPY,0           NO MATCH FOUND - RESET COPY CODE             
*                                                                               
CPTN50   LA    R4,L'ACLDATA(R4)    NEXT ENTRY                                   
         CLI   0(R4),0             TEST EOL                                     
         BNE   CPTN02                                                           
*                                                                               
* NOW ELIMINATE DUPLICATE ENTRIES FROM LIST *                                   
*                                                                               
         L     R4,AIO3                                                          
         LR    R5,R4                                                            
*                                                                               
CPTN52   LA    R5,L'ACLDATA(R5)    POINT TO NEXT ENTRY                          
         CLI   0(R5),0             IS IT THERE                                  
         BE    CPTN54              NO                                           
         CLI   0(R5),X'FF'         WAS IT PROCESSED BEFORE                      
         BE    CPTN52                                                           
*                                                                               
         CLC   ACLPRD(5),ACLPRD-ACLDATA(R5) SAME PRD/SLN/PRD2/SLN2/COPY         
         BNE   CPTN52                                                           
*                                                                               
         CLC   ACLFTD,ACLFTD-ACLDATA(R5) COMPARE FIRST TLCST DATES              
         BL    *+10                                                             
         MVC   ACLFTD,ACLFTD-ACLDATA(R5)                                        
*                                                                               
         CLC   ACLLTD,ACLLTD-ACLDATA(R5) COMPARE LAST TLCST DATES               
         BH    *+10                                                             
         MVC   ACLLTD,ACLLTD-ACLDATA(R5)                                        
*                                                                               
         MVI   0(R5),X'FF'         SET FLAG TO IGNORE ENTRY                     
         B     CPTN52                                                           
*                                                                               
CPTN54   LA    R4,L'ACLDATA(R4)    POINT TO NEXT ENTRY                          
         CLI   0(R4),X'FF'         TEST ALREADY PROCESSED                       
         BE    CPTN54                                                           
         CLI   0(R4),0             TEST E-O-T                                   
         BE    CPTN56                                                           
         LR    R5,R4               SET STARTING POINT                           
         B     CPTN52                                                           
*                                                                               
* SORT THE TABLE TO ELIMINATE X'FF' ENTRIES *                                   
*                                                                               
CPTN56   SR    R0,R0               SET TO COUNT THE ENTRIES                     
*                                                                               
         L     R4,AIO3                                                          
         CLI   0(R4),0                                                          
         BE    *+12                                                             
         LA    R4,L'ACLDATA(R4)                                                 
         BCT   R0,*-12                                                          
*                                                                               
         LPR   R0,R0                                                            
         GOTO1 XSORT,DMCB,AIO3,(R0),L'ACLDATA,6,0                               
*                                                                               
         L     R4,AIO3                                                          
CPTN62   CLI   0(R4),0             TEST E-O-T                                   
         BE    EXIT1                                                            
         CLI   0(R4),X'FF'                                                      
         BE    CPTN64                                                           
         LA    R4,L'ACLDATA(R4)                                                 
         B     CPTN62                                                           
*                                                                               
CPTN64   XC    0(L'ACLDATA,R4),0(R4)  CLEAR AN ENTRY                            
*                                                                               
EXIT1    MVC   KEY(13),WORK+20                                                  
         MVC   KEYSAVE(13),WORK+33                                              
         XC    FILENAME,FILENAME                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* VALIDATE LIST OF PRODUCTS FOR POL REQUEST                                     
* R2 POINTS TO INPUT FLDHDR                                                     
*==============================================================                 
                                                                                
VPOLLST  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    BLOCK(100),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(3,BLOCK)                                      
*                                                                               
         LA    R4,BLOCK                                                         
         LA    R5,SVPOLQ                                                        
*                                                                               
VPOLLST2 CLI   0(R4),1             MUST BE 2 OR 3 CHARS                         
         JNH   VPOLERR1                                                         
         CLI   0(R4),3                                                          
         JH    VPOLERR1                                                         
         CLC   =C'AAA',0(R4)                                                    
         JE    VPOLERR1                                                         
         CLC   =C'POL',0(R4)                                                    
         JE    VPOLERR1                                                         
         CLI   1(R4),0             NO SECONDARY FIELD                           
         JNE   VPOLERR1                                                         
         MVC   0(3,R5),12(R4)                                                   
*                                                                               
         LA    R4,32(R4)                                                        
         LA    R5,3(R5)                                                         
         CLI   0(R4),0                                                          
         JNE   VPOLLST2                                                         
                                                                                
* NOW VALIDATE AGAINST CLIST PRDS                                               
                                                                                
         LA    R4,SVPOLQ                                                        
         LA    R0,3                                                             
*                                                                               
VPOLLST4 CLI   0(R4),0             TEST ANOTHER PRD IN LIST                     
         JE    VPOLLSTX            NO                                           
*                                                                               
         L     RE,ASVNCLST                                                      
*                                                                               
VPOLLST6 CLC   0(3,R4),0(RE)       IF PRDS MATCH,                               
         JE    VPOLLST8            FINE                                         
*                                                                               
         LA    RE,4(RE)            NEXT CLIST ENTRY                             
         CLI   0(RE),C'A'                                                       
         JL    VPOLERR                                                          
         J     VPOLLST6                                                         
*                                                                               
VPOLLST8 LA    R4,3(R4)            NEXT PRD                                     
         JCT   R0,VPOLLST4                                                      
*                                                                               
VPOLLSTX XIT1                                                                   
*                                                                               
VPOLERR1 LA    R4,12(R4)           POINT TO PRD CODE                            
*                                                                               
VPOLERR  MVC   GERROR,=Y(POLPRDER)  XXX NOT A VALID PRODUCT'                    
         MVC   ERRTEXT(3),0(R4)                                                 
         GOTO1 VTRAERR                                                          
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* ADD AMS/GEN SOON REQUEST TO FACWRK FILE AT END OF STEP1                       
* FORMAT IS                                                                     
*      GENQID   DS  CL1'G'                                                      
*               DS  CL24           SPACE FOR CTFILE KEY                         
*      GENQLEN  DS  XL2    +25                                                  
*      GENQSTAT DS  XL1    +26                                                  
*               DS  CL5    +27                                                  
*      GENQSPOK DS  XL64   +32                                                  
*      GENQHDR  DS  XL26   +96                                                  
*      GENQREQ  DS  0CL80  +122  (ONE OR MORE REQUESTS)                         
*================================================================               
                                                                                
AMSREQ   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO3                                                          
         USING GENQREC,R6                                                       
*                                                                               
         LR    R0,R6                                                            
         LHI   R1,512                                                           
         SR    RE,RE                                                            
         LA    RF,X'40'            SET TO BLANK FILL                            
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   GENQTYPE,C'8'                                                    
         MVI   GENQSBTY,C'*'                                                    
         LHI   R0,GENQRECL                                                      
         STCM  R0,3,GENQLEN                                                     
*                                                                               
         LA    R4,GENQHDR          POINT TO REQUEST HEADER                      
         USING REQOFFC,R4                                                       
         MVC   REQOUT,TWAOUT                                                    
         MVC   REQDEST,TWADEST                                                  
         DROP  R4                                                               
*                                                                               
         LA    R4,GENQREQ1         REQ CARD 1                                   
         MVC   0(2,R4),=C'CJ'                                                   
         MVC   2(2,R4),AGENCY                                                   
         MVC   5(6,R4),=C'123456'                                               
         LA    R4,12(R4)                                                        
*                                                                               
         MVC   0(7,R4),=C'0203AMS'                                              
         LA    R0,8                                                             
         CLI   TRAWRKRH+5,0        TEST OPTICA                                  
         JE    *+14                                                             
         MVC   0(10,R4),=C'0206AMSPDF'                                          
         LA    R0,11                                                            
         AR    R4,R0                                                            
*                                                                               
         MVC   0(7,R4),=C'0303GEN'                                              
         LA    R4,8(R4)                                                         
*                                                                               
         MVC   0(2,R4),=C'05'                                                   
         LA    R1,CONWHEN                                                       
         LHI   R0,L'CONWHEN                                                     
         BAS   RE,BACKUP                                                        
*                                                                               
         MVC   0(2,R4),=C'09'                                                   
         LA    R1,TRAMED                                                        
         LHI   R0,L'TRAMED                                                      
         BAS   RE,BACKUP                                                        
*                                                                               
         MVC   0(2,R4),=C'10'                                                   
         LA    R1,TRACLT                                                        
         LHI   R0,L'TRACLT                                                      
         BAS   RE,BACKUP                                                        
*                                                                               
         MVC   0(2,R4),=C'11'                                                   
         LA    R1,TRAPRD                                                        
         LHI   R0,L'TRAPRD                                                      
         BAS   RE,BACKUP                                                        
*                                                                               
         MVC   0(2,R4),=C'12'                                                   
         LA    R1,TRAPTR                                                        
         LHI   R0,L'TRAPTR                                                      
         BAS   RE,BACKUP                                                        
*                                                                               
         MVC   0(2,R4),=C'13'                                                   
         LA    R1,TRAEST                                                        
         LHI   R0,L'TRAEST                                                      
         BAS   RE,BACKUP                                                        
*                                                                               
         MVC   GENQREQ2(12),GENQREQ1                                            
         LA    R4,GENQREQ2+12                                                   
                                                                                
         MVC   0(2,R4),=C'14'                                                   
         LA    R1,TRAPER                                                        
         LHI   R0,L'TRAPER                                                      
         BAS   RE,BACKUP                                                        
*                                                                               
         MVC   0(2,R4),=C'17'                                                   
         LA    R1,TRACONT                                                       
         LHI   R0,L'TRACONT                                                     
         BAS   RE,BACKUP                                                        
*                                                                               
* DO THIS SO ALWAYS HAVE 3 REQUEST CARDS WITH DATA                              
*                                                                               
         MVC   GENQREQ3(12),GENQREQ1   REQUEST CARD 3                           
         LA    R4,GENQREQ3+12                                                   
*                                                                               
         MVC   0(4,R4),=C'1801'                                                 
         MVC   4(1,R4),TRAFAX                                                   
         CLI   4(R4),C'C'          C ISN'T VALID IN 36                          
         BNE   *+8                                                              
         MVI   4(R4),C'N'                                                       
         LA    R4,6(R4)                                                         
*                                                                               
         MVC   0(2,R4),=C'19'                                                   
         LA    R1,TRAOPT                                                        
         LHI   R0,L'TRAOPT                                                      
         BAS   RE,BACKUP                                                        
         BNZ   *+10                                                             
         MVC   0(4,R4),SPACES      IF NO DATA, OVERWRITE FIELD DATA             
*                                                                               
         CLI   TRAWRKRH+5,0        TEST OPTICA                                  
         BE    AMSREQ10                                                         
         MVC   0(2,R4),=C'20'      PASS WORKER FILE NUMBER                      
         LA    R1,TRAWRKR                                                       
         LHI   R0,L'TRAWRKR                                                     
         BAS   RE,BACKUP                                                        
*                                                                               
AMSREQ10 BCTR  R4,0                                                             
         MVC   0(2,R4),=C'* '      SET E-O-R                                    
         B     AMSADD                                                           
*                                                                               
BACKUP   ST    R1,FULL             SAVE FIELD START                             
         AR    R1,R0               POINT TO END OF FIELD                        
         BCTR  R1,0                BACK UP TO LAST CHAR                         
*                                                                               
BACKUP2  CLI   0(R1),C' '                                                       
         BH    BACKUP4                                                          
         BCTR  R1,0                                                             
         BCT   R0,BACKUP2                                                       
         LTR   R0,R0               RETURN WITH CC EQ IF NO DATA                 
         BR    RE                                                               
*                                                                               
BACKUP4  CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  2(2,R4),DUB                                                      
*                                                                               
         L     R1,FULL             POINT TO DATA                                
         LR    RF,R0                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R4),0(R1)                                                    
*                                                                               
         LA    R4,6(R4,RF)         POINT TO NEXT OUTPUT POSN                    
         LTR   RB,RB               SET CC NEQ                                   
         BR    RE                                                               
         EJECT                                                                  
*===================================================================            
* ADD AMS/GEN SOON REQUEST AS AN 8* RECORD TO FACWK FILE                        
* SRUPD00 WILL THEN ADD A SOON REQUEST FOR THIS REPORT                          
*===================================================================            
                                                                                
*>>>>>>>>>> REMEMBER TO CHANGE OUTPUT LENGTH ON TR07AMS FILE !!!!               
*>>>>>>>>>> REMEMBER TO CHANGE OUTPUT LENGTH ON TR07AMS FILE !!!!               
*>>>>>>>>>> REMEMBER TO CHANGE OUTPUT LENGTH ON TR07AMS FILE !!!!               
*>>>>>>>>>> REMEMBER TO CHANGE OUTPUT LENGTH ON TR07AMS FILE !!!!               
*>>>>>>>>>> REMEMBER TO CHANGE OUTPUT LENGTH ON TR07AMS FILE !!!!               
*>>>>>>>>>> REMEMBER TO CHANGE OUTPUT LENGTH ON TR07AMS FILE !!!!               
                                                                                
AMSADD   DS    0H                                                               
*                                                                               
         TM    SVOPT,OPTTEST       SINCE TEST RUNS DON'T UPDATE FILE            
         JO    EXIT                GET OUT NOW!                                 
*                                                                               
         TM    WHEN,X'20'          THIS A SOON RUN                              
         BZ    AMSADD10            NO-ADD REQUEST TO TEMP FILE                  
*                                                                               
         MVI   GENQHDR+15,X'21'    SET FLAG FOR 3 REQUEST CARDS!                
*                                                                               
         L     RF,TWAMASTC                                                      
         USING MASTD,RF                                                         
         MVC   GENQRFH,MCRFHDR     MOVE RFHDR                                   
*                                                                               
         LA    R4,GENQSPOK         SET SPOOK FOR SOON PROCESSING                
         USING SPOOK,R4                                                         
*                                                                               
         MVC   SPOOKUID,MCORIGID                                                
         MVC   SPOOKDES,MCDESTID                                                
*                                                                               
         OI    SPOOKTY,X'01'       SET TO RUN IN CLASS G                        
*                                                                               
         LR    RE,RF                                                            
         AHI   RE,MCTNUM-MASTD                                                  
         OC    0(2,RE),0(RE)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   SPOOKTID,0(RE)      SET TNUM FROM LOGO2= CARD                    
         DROP  RF                                                               
*                                                                               
         MVC   SPOOKSEN,SYSPHASE+1                                              
         MVC   SPOOKERN,GETMSYS                                                 
         MVC   SPOOKAGY,TWAAGY                                                  
         L     RE,SYSPARMS                                                      
         MVC   SPOOKAGX,0(RE)                                                   
         MVC   SPOOKDID,REMUSER                                                 
         CLI   SVTWPR1,C'Y'        TEST FAXING                                  
         BE    *+12                                                             
         CLI   SVTWPR1,C'2'                                                     
         BNE   AMSADD2                                                          
         MVC   SPOOKDID,=C'MWX'    MAKE THE USER A FAXNAME                      
         OC    SPOOKTY1,SVFAXAR2                                                
*                                                                               
AMSADD2  MVC   SPOOKSYS,RCPROG                                                  
         MVC   SPOOKEOD,=C'CJ'                                                  
         MVC   SPOOKJCL,=C'TJ'                                                  
         MVC   SPOOKPR1,REQPRI1                                                 
         MVC   SPOOKPR2,REQPRI2                                                 
         MVC   SPOOKSML,REQSML                                                  
         MVI   SPOOKWEN,2                                                       
         MVC   SPOOKXT,=C'XT='                                                  
         MVC   SPOOKTY,REQRTYP                                                  
*                                                                               
         MVI   SPOOKTY1,0                                                       
         CLI   SVTWPR1,C'2'                                                     
         BNE   *+14                                                             
         OC    SPOOKTY1,SVFAXAR2                                                
         B     AMSADD3                                                          
                                                                                
         TM    WHEN,X'40'                                                       
         BZ    *+10                                                             
         MVC   SPOOKTY1,SVQLTYP2                                                
AMSADD3  DS    0H                                                               
         CLI   SPOOKDID+2,C' '                                                  
         BNE   AMSADD4                                                          
         MVI   SPOOKDID+2,C'*'                                                  
         CLI   SPOOKDID+1,C' '                                                  
         BNE   AMSADD4                                                          
         MVI   SPOOKDID+1,C'*'                                                  
*                                                                               
AMSADD4  GOTO1 DATAMGR,DMCB,=C'DMADD',=C'CTFILE',,AIO3                          
         J     EXIT                                                             
         EJECT                                                                  
*=================================================================              
* FOR OFFLINE PROCESSING, ADD REQUEST TO REQFILE                                
* NOTE OPEN/CLOSE ARE DONE IN SPTRA04                                           
*=================================================================              
                                                                                
AMSADD10 LA    R0,GENQREQ1                                                      
         L     R1,ATR07AMS                                                      
         PUT   (1),(0)                                                          
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* OPEN A WORKER FILE USING T2168C AS A BUFFER                                   
* AND WRITE THE SELECTED SVTABLE ROWS TO IT                                     
*==============================================================                 
                                                                                
ADDWRKR  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R0,ADTLIST                                                       
         ST    R0,AWKBUFF          SET DTLIST AS WKBUFF ADDRESS                 
*                                                                               
         LA    R4,ELEM                                                          
         USING WLHDRD,R4                                                        
         XC    ELEM,ELEM                 CLEAR AN I/O AREA                      
*                                                                               
         MVC   ELEM(8),=C'*SOFSOF*'                                             
*                                                                               
         L     RE,ATWA                                                          
         MVC   WLUSRID,TWAORIG-T216FFD(RE)   MOVE USERID                        
*                                                                               
         MVC   WLDESC,=CL16'AUTO GEN PDF'                                       
         MVC   WLSYSPRG,=C'SAI'          SET REPORT ID                          
         MVI   WLSUBPRG,C'S'             SET SUB PROGRAM NUMBER                 
         MVI   WLCLASS,C'T'              SET REPORT CLASS                       
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(1,DUB) GET PWOS DATE                          
         MVC   WLDAY,DUB+2                                                      
*                                                                               
         MVI   WLTYPE,0                                                         
         MVC   SVINDEX,WLKEY                                                    
                                                                                
         GOTO1 DATAMGR,DMCB,=C'DMPRINT ',=C'WRKFILE',SVINDEX,(R4),     X        
               AWKBUFF                                                          
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         MVC   SVWRKFNO,WLREPRNO         SAVE REPORT SEQNUM                     
                                                                                
* GET RUN=ALL OPTION FROM GLOBALS                                               
                                                                                
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'GETD',ELEM,24,GLVSPTRF                              
         LA    RE,ELEM                                                          
         USING GLVTRFD,RE                                                       
         MVC   SVRUNALL,TRFDOALL         SAVE RUN=ALL OPTION                    
         MVC   DUB,TRFSVKEY              SAVE SAVREC SEQ/DATE                   
         DROP  RE                                                               
                                                                                
* GET UNIQUE ID FROM GLOBALS                                                    
                                                                                
         LA    R4,ELEM                                                          
         XC    0(255,R4),0(R4)           MOVE UNIQUE ID                         
         LA    R0,L'SVUNIQID                                                    
         STH   R0,0(R4)                                                         
         GOTO1 (RF),DMCB,=C'GETD',4(R4),60,GLVBUY2                              
                                                                                
* PUT UNIQUE ID TO WRKFILE                                                      
                                                                                
         GOTO1 DATAMGR,DMCB,=C'DMPRINT ',=C'WRKFILE',SVINDEX,(R4),     X        
               AWKBUFF                                                          
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         DROP  R4                                                               
                                                                                
*==============================================================                 
* READ SVTABLE FROM TRFFIL 0A7F RECORD INTO TIA AND                             
* MOVE SELECTED ENTRIES TO WORKER FILE                                          
*==============================================================                 
                                                                                
         CLI   SVRUNALL,C'Y'                                                    
         JNE   ADDWRK2             IF RUN=ALL, NO SELECTED ENTRIES              
         MVC   ELEM(7),=C'RUN=ALL'                                              
         GOTO1 DATAMGR,DMCB,=C'DMPRINT ',=C'WRKFILE',SVINDEX,ELEM,     X        
               AWKBUFF                                                          
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         J     ADDWRK8                                                          
*                                                                               
ADDWRK2  LA    R4,KEY                                                           
         USING XSAVKEY,R4                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   XSAVTYPE,XSAVTYPQ   X'0A'                                        
         MVI   XSAVSBTY,XSAVSBTQ   X'7F'                                        
         MVC   XSAVSEQ(5),DUB      MOVE SEQ/DATE                                
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(XSAVAGMD-XSAVKEY),KEYSAVE   SAME SEQNUM/DATE                 
         JNE   *+2                                                              
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'TRFFIL',KEY+14,ATIA,DMWORK            
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
                                                                                
* NOW WRITE SELECTED ENTRIES TO WORKER FILE                                     
                                                                                
         L     RE,ASVTABLE                                                      
         L     R7,ATIA                                                          
         USING SVTABLED,R7                                                      
         LA    R7,24(R7)           FIRST ENTRY AT +24                           
         XC    ELEM,ELEM           CLEAR RECORD AREA                            
                                                                                
ADDWRK4  CLI   SVTBLINE+3,C'S'                                                  
         JNE   ADDWRK6                                                          
*                                                                               
         MVI   SVTBLINE,X'FF'      OVERWRITE ELEMENT CODE/LEN                   
         MVI   SVTBLINE+1,X'FF'                                                 
         MVI   SVTBLINE+3,X'FF'        SET BACK TO X'FFFFFFFF'                  
         LA    R0,L'SVTBDATA                                                    
         STCM  R0,3,ELEM                                                        
         MVC   ELEM+4(L'SVTBDATA),0(R7)  MOVE RECORD                            
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMPRINT ',=C'WRKFILE',SVINDEX,ELEM,     X        
               AWKBUFF                                                          
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
ADDWRK6  LA    R7,L'SVTBDATA(R7)                                                
         OC    SVTBLINE,SVTBLINE       TEST FOR MORE ENTRIES                    
         JNZ   ADDWRK4                                                          
*                                                                               
ADDWRK8  LA    R4,ELEM                                                          
         USING WLHDRD,R4                                                        
         XC    ELEM,ELEM                 CLEAR AN I/O AREA                      
         MVC   WLSOFLAB,=C'*EOFEOF*'                                            
*                                                                               
         GOTO1 DATAMGR,DMCB                                                     
                                                                                
*==========================================================                     
* UNPROTECT ALL SELECT FIELDS SO WRKR FILENUM IS FIELD 32                       
*==========================================================                     
                                                                                
         LA    R2,TRASEL1H                                                      
*                                                                               
ADDWRK10 NI    1(R2),X'FF'-X'20'   SET UNPROTECTED                              
         AHI   R2,TRASEL2H-TRASEL1H                                             
         CLI   0(R2),9                                                          
         BNE   ADDWRK10                                                         
                                                                                
* PUT WRKR FILENUM IN REQUEST                                                   
                                                                                
         XC    TRAWRKR,TRAWRKR     CLEAR ONSCREEN WORKER FILENUM                
         SR    R0,R0                                                            
         ICM   R0,3,SVWRKFNO                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TRAWRKR(5),DUB                                                   
         MVI   TRAWRKRH+5,5        SET INPUT LENGTH                             
*                                                                               
         MVC   CONREC(8),=C'AUTOPDF '                                           
         MVI   CONRECH+5,7                                                      
*                                                                               
         MVC   CONOUT(3),=C'PDF'   SET FLAG TO RUN IN CLASS G                   
*                                                                               
         XC    CONWHEN,CONWHEN                                                  
         MVC   CONWHEN(5),=C'SOON,'                                             
         MVC   CONWHEN+5(3),OPTICWHO  SET REQUESTOR INITIALS                    
         MVI   CONWHENH+5,8        SET INPUT LENGTH                             
         MVI   TWAWHEN,TWW$SOON    SET UPDATIVE SOON REQUEST                    
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* OPEN WORKER FILE WITH FILE NUMBER IN TRAWRKR                                  
* PASSED BY A PDF REQUEST FOR INSTRUCTIONS                                      
* IF NO SELECT RECORDS FOLLOW UNIQUE ID REC, THEN RUN ALL STATIONS              
*=================================================================              
                                                                                
OPENWRKF NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    AWKBUFF,AWKBUFF     TEST ALREADY ALLOCATED                       
         JZ    OPENWRK0                                                         
* DO INDEX CALL TO REPROCESS WKFILE                                             
         XC    ELEM,ELEM                                                        
         MVC   ELEM(10),SVINDEX                                                 
         GOTO1 DATAMGR,DMCB,=C'INDEX   ',=C'WRKFILE',ELEM,ELEM,        X        
               AWKBUFF                                                          
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         J     OPENWRK6                                                         
*                                                                               
OPENWRK0 L     R0,=A(14*1024)      GET A 14K WKFILE BUFFER                      
         GETMAIN RU,LV=(0),LOC=(24)                                             
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         ST    R1,AWKBUFF                                                       
*                                                                               
OPENWRK2 LLC   RE,TRAWRKRH+5                                                    
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     *+10                                                             
         PACK  DUB,TRAWRKR(0)                                                   
*                                                                               
         CVB   R0,DUB                                                           
         STH   R0,HALF             SAVE WKFILE NUMBER                           
*                                                                               
         LA    R4,ELEM             BUILD WKFILE KEY                             
         USING WLINDEX,R4                                                       
         XC    ELEM,ELEM                                                        
*                                                                               
         L     RE,ATWA                                                          
         MVC   WLUSRID,TWAORIG-T216FFD(RE)   MOVE USERID                        
*                                                                               
         MVC   WLSYSPRG,=C'SAI'          SET REPORT ID                          
         MVI   WLSUBPRG,C'S'             SET SUB PROGRAM NUMBER                 
         MVI   WLCLASS,C'T'              SET REPORT CLASS                       
         MVI   WLTYPE,0                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(1,DUB) GET PWOS DATE                          
         MVC   WLDAY,DUB+2                                                      
         MVC   WLFILENO,HALF             SET REPORT NUMBER                      
                                                                                
OPENWRK4 DS    0H                                                               
         GOTO1 DATAMGR,DMCB,=C'INDEX   ',=C'WRKFILE',ELEM,ELEM,        X        
               AWKBUFF                                                          
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
*                                                                               
         CLC   HALF,WLFILENO       TEST RIGHT FILE                              
         JNE   OPENWRK4                                                         
*                                                                               
OPENWRK6 MVC   SVINDEX,ELEM        SAVE INDEX/FILENUM/WKFILE                    
                                                                                
* FIRST RECORD IS UNIQUE IDENTIFIER FOR PDF PROCESSING                          
                                                                                
         GOTO1 DATAMGR,DMCB,=C'READ    ',=C'WRKFILE ',SVINDEX,ELEM,    X        
               AWKBUFF                                                          
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         MVC   SVUNIQID,ELEM+4                                                  
                                                                                
* NOW READ FIRST SELECT ENTRY (IF ANY)                                          
                                                                                
         GOTO1 (RF),(R1),,,,SVWRKREC   READ FIRST SELECT ENTRY                  
         TM    8(R1),X'80'         TEST EOF                                     
         JO    OPENWRK8                                                         
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         CLC   SVWRKREC+4(7),=C'RUN=ALL'                                        
         JNE   OPENWRKX                                                         
*                                                                               
OPENWRK8 MVI   SVRUNALL,C'Y'       SET TO RUN ALL                               
*                                                                               
OPENWRKX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* CHECK FOR XFRCTL FROM SPOT/LINK                                               
* IF SO, RELOCATE SVTABLE ADDRESS TO ATIA                                       
* SINCE SPOT/LINK IS STATELESS, ALWAYS READ HEADLINE GLOBALS                    
*=================================================================              
                                                                                
GETGLOB  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OI    TRAWRKRH+1,X'20'    PROTECT WORKER FILE FIELD                    
*                                                                               
         L     RF,SYSPARMS                                                      
         L     RF,16(RF)                                                        
         ST    RF,ACOMFACS                                                      
         ICM   RF,15,CGLOBBER-COMFACSD(RF)  EXIT IF OFFLINE                     
         JZ    GETGLOBX                                                         
*                                                                               
         GOTO1 (RF),DMCB,=C'GETD',ELEM,24,GLVXCTL                               
         CLI   8(R1),0                                                          
         BNE   GETGLOBX                                                         
*                                                                               
         GOTO1 (RF),(R1),=C'DELE'  DELETE ELEMENT                               
*                                                                               
         LA    RE,ELEM                                                          
         USING GLVXFRSY,RE                                                      
         CLC   =C'SPOLIN',GLVXFRSY TEST CALL FROM SPOT/LINK                     
         JNE   GETGLOBX                                                         
         DROP  RE                                                               
*                                                                               
         MVI   SVXFROV,7           SET AUTO/GEN ACTIVE                          
         OI    GENSTAT7,GES7PDF    REQUEST RETURN HOOK ON ERROR                 
         NI    TRAWRKRH+1,X'FF'-X'20'  UNPROTECT WORKER FILE FIELD              
                                                                                
*===========================================================                    
* READ GLVSPTRF TO FIND OUT IF THIS IS BLD OR RUN MODE                          
* FORMAT OF GLOBAL IS C'BLD'/C'RUN',FAXOPT,SHIPOPT                              
*===========================================================                    
                                                                                
         GOTO1 (RF),DMCB,=C'GETD',ELEM,24,GLVSPTRF                              
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
*                                                                               
         LA    RE,ELEM                                                          
         USING GLVTRFD,RE                                                       
         MVC   SVXFRMOD,TRFACT     SAVE BLD/SUB MODE                            
         MVC   TRAFAX,TRFFAX                                                    
         MVC   TRASHIP(1),TRFSHIP                                               
         MVC   OPTICWHO,TRFWHO     SAVE REQUESTOR INITIALS                      
         DROP  RE                                                               
                                                                                
* FOR BLD MODE RELOCATE SVTABLE TO TIA                                          
                                                                                
         CLC   =C'BLD',SVXFRMOD                                                 
         JNE   GETGLOB2                                                         
         L     RE,ATIA                                                          
         XC    0(24,RE),0(RE)                                                   
         LA    RE,24(RE)           START AT +24 SO ROOM FOR KEY                 
         ST    RE,ASVTABLE                                                      
*                                                                               
         AHI   RE,TWAMAX                                                        
         ST    RE,ASVTABLX                                                      
*                                                                               
         L     R4,ASVTABLE         AND CLEAR STORAGE                            
         LHI   R5,TWAMAX                                                        
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  R4,R0                                                            
*                                                                               
* GLOBBER CALLS FOR MEDIA AND CLIENT ALREADY DONE IN SPTRA00                    
*                                                                               
GETGLOB2 LA    R2,TRAFAXH          FIELD MOVED ABOVE FROM GLVSPFMT              
         LA    R1,L'TRAFAX                                                      
         BAS   RE,SETIN                                                         
*                                                                               
         LA    R2,TRASHIPH                                                      
         LA    R1,L'TRASHIP                                                     
         BAS   RE,SETIN                                                         
         GOTO1 (RF),DMCB,=C'GETF',TRAPRDH,,GLVSPPRD                             
         LA    R2,TRAPRDH                                                       
         LA    R1,L'TRAPRD                                                      
         BAS   RE,SETIN                                                         
*                                                                               
         GOTO1 (RF),DMCB,=C'GETF',TRAPTRH,,GLVSPPR2                             
         LA    R2,TRAPTRH                                                       
         LA    R1,L'TRAPTR                                                      
         BAS   RE,SETIN                                                         
*                                                                               
         GOTO1 (RF),DMCB,=C'GETF',TRAESTH,,GLVSPEST                             
         LA    R2,TRAESTH                                                       
         LA    R1,L'TRAEST                                                      
         BAS   RE,SETIN                                                         
*                                                                               
         GOTO1 (RF),DMCB,=C'GETF',TRAPERH,,GLVSPPER                             
         LA    R2,TRAPERH                                                       
         LA    R1,L'TRAPER                                                      
         BAS   RE,SETIN                                                         
*                                                                               
         GOTO1 (RF),DMCB,=C'GETF',TRAOPTH,,GLVBUY1                              
         LA    R2,TRAOPTH                                                       
         LA    R1,L'TRAOPT                                                      
         BAS   RE,SETIN                                                         
*                                                                               
         GOTO1 (RF),DMCB,=C'GETF',TRACONTH,,GLVSPREQ    REQUESTOR               
         LA    R2,TRACONTH                                                      
         LA    R1,L'TRACONT                                                     
         BAS   RE,SETIN                                                         
*                                                                               
GETGLOBX J     EXIT                                                             
*                                                                               
SETIN    LA    R4,8(R1,R2)         POINT BEYOND INPUT FIELD                     
         BCTR  R4,0                BACK UP TO LAST INPUT CHAR                   
*                                                                               
SETIN2   CLI   0(R4),C' '                                                       
         JH    SETIN4                                                           
         BCTR  R4,0                                                             
         JCT   R1,SETIN2                                                        
*                                                                               
SETIN4   STC   R1,5(R2)                                                         
         NI    4(R2),X'FF'-X'20'   UNSET PREV VALID                             
         BR    RE                                                               
         EJECT                                                                  
       ++INCLUDE SPMGRTAB                                                       
                                                                                
* NOTE THIS DSECT ALSO INCLUDED IN SPLNK10                                      
                                                                                
GENQREC  DSECT                     DSECT FOR WKFILE REQUEST REC                 
*                                                                               
GENQTYPE DS  CL1'8'                                                             
GENQSBTY DS  CL1'*'                                                             
         DS  CL23           SPACE FOR CTFILE KEY                                
GENQLEN  DS  XL2      +25                                                       
GENQSTAT DS  XL1      +26                                                       
         DS  CL1      +27                                                       
GENQRFH  DS  XL20     +28   RFHDR                                               
GENQSPOK DS  XL64     +48   SPOOK                                               
*                                                                               
GENQHDR  DS  XL26     +112  REQUEST HEADER                                      
*                                                                               
GENQREQ1 DS  CL80     +138  (ONE OR MORE REQUESTS)                              
GENQREQ2 DS  CL80     +198                                                      
GENQREQ3 DS  CL80     +258                                                      
GENQRECL EQU *-GENQREC                                                          
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDREPMASTD                                                     
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPTRMKL                                                        
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE SPTRKEYS                                                       
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAF7D                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPTRAWORKD                                                     
         EJECT                                                                  
       ++INCLUDE SPTRAINSTC                                                     
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE DDREQHDR                                                       
       ++INCLUDE DDSPOOK                                                        
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDGLVSPTRF                                                     
       ++INCLUDE DDGLOBEQUS                                                     
         EJECT                                                                  
TRALINES EQU   (TRALAST-TRASEL1H)/(TRASEL2H-TRASEL1H)                           
NEXTLINE EQU   TRASEL2H-TRASEL1H                                                
*                                                                               
DSPLINED DSECT                                                                  
         DS    CL8                 ALLOW FOR FLDHDR                             
DSPSTA   DS    CL7                                                              
         DS    CL2                                                              
DSPMKT   DS    CL17                                                             
         DS    CL1                                                              
DSPPRD   DS    CL7                                                              
         DS    CL1                                                              
DSPPRD2  DS    CL7                                                              
         DS    CL1                                                              
DSPFTD   DS    CL8                                                              
         DS    CL3                                                              
DSPLTD   DS    CL8                                                              
         DS    CL4                                                              
DSPCOPY  DS    CL1                                                              
         DS    CL1                                                              
DSPLMTS  DS    CL3                                                              
         EJECT                                                                  
       ++INCLUDE DMWRKFL                                                        
       ++INCLUDE FASSB                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'131SPTRA07   08/30/16'                                      
         END                                                                    
