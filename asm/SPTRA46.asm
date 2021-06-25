*          DATA SET SPTRA46    AT LEVEL 017 AS OF 02/26/19                      
*PHASE T21646C                                                                  
*INCLUDE BINSR31                                                                
*INCLUDE PQPROF                                                                 
         TITLE 'T21646 - TRAFFIC SHIPPING ORDERS'                               
                                                                                
***********************************************************************         
*                                                                     *         
*        MEMORY USAGE                                                 *         
*   DYNAWORK (5000) USED TO BUILD STATION LISTS ONLINE                *         
*   KHDUMMY (20000) OFFLINE                                           *         
*          ASTLIST = ADDRESS OF START OF LIST                         *         
*          ASTLISTX = ADDRESS OF END OF LIST-24                       *         
*          EACH ENTRY                                                 *         
*              0 - 3 = POINTER TO COMMERCIAL LIST                     *         
*              4 - 5 = MARKET                                         *         
*              6 - 8 = STATION                                        *         
*              9 -11 = FIRST TELECAST DATE                            *         
*                                                                     *         
*   AIO2           USED TO BUILD COMMERCIAL LISTS ONLINE              *         
*   KHDUMMY+20000(60000) OFFLINE                                      *         
*          ACMLST = ADDRESS OF START OF LIST                          *         
*          ACMLSTX = ADDRESS OF END OF LIST-26                        *         
*          EACH COMMERCIAL LIST IS                                    *         
*          0 - 1 LIST SIZE                                            *         
*          2 - 4 STATION AFFILIATE ON TS PROFILE 2 OPTION                       
*          N  20 BYTE ENTRIES                                         *         
*              0-15 CMML/PIGGYBACK CML ENTRIES.                       *         
*             16-19 CMML TYPE                                         *         
*          EACH LIST IS BUILT AT END OF EXISTING LISTS,               *         
*          AND THEN COMPARED TO ALL EXISTING LISTS, AND IF EQUAL      *         
*          STLACML (IN STATION LIST) IS CHANGED TO POINT              *         
*          TO EXISTING LIST                                           *         
*                                                                     *         
*   T2168C IS USED FOR CML SHIP SUMMARY ONLINE                        *         
*   KHDUMMY+60000(20000) OFFLINE                                      *         
*          ASHPSUM = ADDRESS OF START OF LIST                         *         
*          ASHPSUMX = ADDRESS OF END OF LIST                          *         
*           EACH ENTRY IS 48 BYTES LONG                               *         
*               0 -  7 = CML                                          *         
*               8 - 15 = PIGGY BACK CML                               *         
*              16 - 19 = CMML TYPE                                    *         
*              20 - 21 = COUNT OF CMLS TO SHIP EACH STATION           *         
*              22 - 23 = COUNT OF CMLS TO SHIP FOR ALL STATIONS       *         
*              24 - 47 = AD-ID CODE & P/B AD-ID                       *         
*        CTAB IS DSECT FOR THIS TABLE                                 *         
*                                                                     *         
*   AIO3 IS USED TO READ COMMLS WHILE READING SHIP RECAPS             *         
*   AIO3 IS USED AT PRINT TIME TO BUILD LISTS OF STATIONS             *         
*          EACH ENTRY IS DESCRIBED BY STALISTD DSECT                  *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
*        REGISTER USAGE                                               *         
*                                                                     *         
*         R0 - WORK REG                                               *         
*         R1 - WORK REG                                               *         
*         R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR      *         
*         R3 - POINTER TO ATWA AND POINTER TO STATION LIST            *         
*         R4 - WORK REG                                               *         
*         R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE      *         
*         R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM  *         
*              FOR DSECT IN VALREC                                    *         
*         R7 - SECOND BASE REG                                        *         
*         R8 - POINTER TO SPOOLD                                      *         
*         R9 - POINTER TO SYSD                                        *         
*         RA - SECOND BASE                                            *         
*         RB - FIRST BASE                                             *         
*         RC - POINTER TO GEND                                        *         
*         RD - SAVE AREA POINTER                                      *         
*         RE - GOTO1 REG                                              *         
*         RF - GOTO1 REG                                              *         
*                                                                     *         
***********************************************************************         
                                                                                
***********************************************************************         
*                                                                     *         
* LEV 32 BGRI FEB06/02 FIX DUP COMMLS WITHOUT P/B                     *         
* LEV 34 BGRI MAR01/02 FIX WHEN,5 FOR FRED                            *         
* LEV 36 SMUR JAN07/03 EX MOVE OF CON= TO GET RID OF SPACES           *         
* LEV 37 BGRI JAN14/03 CHANGE SVSPARE TO SVSPAREX                     *         
* LEV 38 BGRI JUN29/04 AD-ID                                          *         
* LEV 39 BGRI JUL27/04 SOX                                            *         
* LEV 40 BGRI FEB02/05 FIX T1 PROFILE 7 - IF B PRINT BOTH             *         
* LEV 41 SMUR MAR23/06 USE DEFAULT TIME FOR PQ RETAINS                *         
* LEV 42 MHER MAR06/07 CHANGE SVCLIST TO ASVCLIST (RECLAIM STORAGE)   *         
* LEV 43 MNAS APR08/07 CHANGE VALILOC CALLS TO INCLUDE PARAM1 OF 0    *         
* LEV 44 SMUR DEC07/07 LANDSCAPE                                      *         
* LEV 45 MHER MAY30/08 REFORMAT REPORT LAYOUT                         *         
* LEV 02 MNAS AUG13/08 ARCHIVING NOW REPORTS                          *         
* LEV 04 SMUR DEC17/08 FIX PRINTING HEADLINES ON 1ST PAGE OF AGY COPY *         
* LEV 11 MNAS MAR03/11 HONOR TW PROFILE FOR ID FDMFDA WHEN COMING    *          
*             FROM ANOTHER SCREEN - ELSE VALUES GET CARRIED OVER     *          
* LEV 12 SMUR AUG19/13 OPTICA                                        *          
* LEV 13 SMUR OCT23/13 SEND CALL LETTER WITH OUT "-TV" TO OPTICA     *          
*        SMUR DEC02/13 CHANGE Q FROM PROD TO ADV FOR OPTICA          *          
*        SMUR APR23/14 ADD PRODUCTION SERVICES REQUIRED FOR OPTICA   *          
*                      MOVE TABLES FOR OPTICA TO HIGH CORE           *          
*        SMUR JUN03/14 ADD COMMENTS ENTERED ON SCREEN TO OPTICA      *          
*        SMUR SEP11/14 FIX MISSING 8 CHAR ISCII IN XML               *          
*        SMUR NOV21/14 ADD YNG FOR OPTICA                            *          
*       SMUR FEB06/15 SEND CALL LETTERS FOR RADIO AS WABCA FOR WABC-AM          
*        SMUR MAR13/15 ADD PRD CODE TO XML ORDER ITEM DUE TO SPEC CHG*          
*        SMUR MAR23/15 SHOW 3 CHAR STATIONS AS XIR X (FOR XIR-X)     *          
*        SMUR JUN05/15 NO MQ MESSAGES FOR OPTICA UAT CLIENT          *          
*        SMUR JUL24/15 ADD SPG FOR OPTICA                            *          
* LEV 15 SMUR SEP20/16 DISABLE XML TO OPTICA                         *          
* SPEC-27177  AUG23/18 FIX ZERO CML ENTRY IN TABLE, DO NOT OVERRIDE  *          
*                      FAX OPTION WHEN SOON/OV, A BUG @LEV 11        *          
* SPEC-25370  SMUR NOV05/18 SHOW HD IN SHIPPING SUMMARY (19.1)       *          
**********************************************************************          
         EJECT                                                                  
T21646   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NMOD1 0,T21646,R7,RA,RR=R2                                             
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,SPTR46RR                                                      
*                                                                               
         L     RF,=V(PQPROF)                                                    
         AR    RF,R2                                                            
         ST    RF,PQPROF                                                        
*                                                                               
         L     R3,ATWA                                                          
         USING T216FFD,R3                                                       
*                                                                               
         L     RE,ASECBLK                                                       
         USING SECD,RE                                                          
         MVC   SVSECPID,SECPID     PID                                          
         MVC   SVSECFNM,SECFNAME   FIST AND                                     
         MVC   SVSECLNM,SECLNAME   LAST NAME                                    
         DROP  RE                                                               
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   TESTMOD2                                                         
*                                                                               
* MUST OPEN PRINT QUE MANUALLY TO ALLOW FOR 36/12HR RETENTION *                 
*                                                                               
         MVI   PQSW,1                                                           
                                                                                
         MVI   SVQLTYP1,0                                                       
         MVI   SVFAXARC,0                                                       
         XC    WORK,WORK                                                        
         XC    PROFKEY,PROFKEY                                                  
         MVI   PROFKEY,C'S'                                                     
         MVC   PROFKEY+1(2),=C'TO'                                              
         MVC   PROFKEY+3(2),TWAORIG                                             
         GOTO1 PQPROF,DMCB,(X'80',PROFKEY),(0,WORK),ACOMFACS                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,WORK                                                          
         USING REMOTED,R4                                                       
         MVC   SVQLTYP1,REMOTTY1                                                
                                                                                
         CLI   REMOTSUB,C'#'                                                    
         BNE   GEN12                                                            
         CLI   REMOTSUB+1,C'N'                                                  
         BE    GEN12                                                            
         CLI   REMOTSUB+1,C'A'                                                  
         BNE   GEN12                                                            
         OI    SVFAXARC,REMOTARQ                                                
GEN12    OI    SVFAXARC,REMOTAEQ                                                
         DROP  R4                                                               
*                                                                               
* PROCESS VALKEY ONLY IF ON-LINE REQUEST FOR OFF-LINE PROCESSING *              
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         JE    EXIT                                                             
*                                                                               
         TM    WHEN,X'30'          TEST OVERNT/SOON                             
         BNZ   GEN00               YES - PROCESS                                
*                                                                               
TESTMOD2 DS    0H                                                               
         CLI   OFFLINE,C'Y'        IF OFFLINE                                   
         BNE   TESTMOD5                                                         
         CLI   SVTWPR05,C'N'       AND FAXING                                   
         BE    TESTMOD5                                                         
         MVI   SPOOLKEY+PLCLASS-PQPLD,C'G'                                      
         OC    SPOOLKEY+QLTYP1-PQPLD(1),SVFAXARC                                
*                                                                               
         ICM   RE,15,TWAMASTC                                                   
         BZ    TESTMOD5                                                         
         MVI   MCREQREP-MASTD(RE),C'N' THEN SUPPRESS REQUEST PAGE               
*                                                                               
TESTMOD5 CLI   MODE,PRINTREP                                                    
         JNE   EXIT                                                             
*                                                                               
         MVC   PAGE,=H'1'          RESET PAGE FOR EACH REQUEST                  
         B     GEN00                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         EJECT                                                                  
GEN00    GOTO1 DATCON,DMCB,(5,0),(3,TODAY)                                      
*                                                                               
         BAS   RE,INIT             INIT STUFF                                   
*                                                                               
         XC    TRAPD,TRAPD         CLEAR PRINT COPY ID FIELD                    
         OI    TRAPDH+6,X'80'      FORCE TRANSMIT                               
*                                                                               
         LA    R2,TRAMEDH                                                       
*                                                                               
* VALIDATE MEDIA *                                                              
*                                                                               
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
* VALIDATE CLIENT *                                                             
*                                                                               
         LA    R2,TRACLTH                                                       
         GOTO1 VALICLT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
         XC    WORK,WORK           READ T0 PROFILE                              
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
         MVI   WORK+3,C'1'         READ T1 PROFILE                              
         GOTO1 (RF),(R1),,SVT1PROF                                              
*                                                                               
         MVI   WORK+3,C'S'         READ TS PROFILE                              
         GOTO1 (RF),(R1),,SVTSPROF                                              
*                                                                               
         MVI   WORK+3,C'W'         READ TW PROFILE                              
         GOTO1 (RF),(R1),,SVTWPROF                                              
         CLI   SVTWPR05,0                                                       
         BNE   *+8                                                              
         MVI   SVTWPR05,C'N'      MUST SET DEFAULT VALUE = N                    
*                                                                               
         LA    R2,TRAFAXH          WAS FAX ENTERED                              
         CLC   =C'NOW',CONWHEN     IF NOT "NOW"                                 
         BNE   GEN04               BYPASS                                       
         CLC   TWAORIG,=X'335D'                                                 
         BNE   GEN04                                                            
         CLI   TRLSTPGM,X'46'                                                   
         BNE   GEN05                                                            
*                                                                               
GEN04    CLI   5(R2),0                                                          
         BNE   GEN07                                                            
GEN05    MVC   8(1,R2),SVTWPR05                                                 
         MVI   5(R2),1             SET ADJUSTED LENGTH                          
         OI    6(R2),X'80'         FORCE TRANSMIT                               
*                                                                               
         MVI   TRLSTPGM,X'46'                                                   
         B     GEN10                                                            
*                                                                               
GEN07    CLI   8(R2),C'Y'                                                       
         BE    *+20                                                             
         CLI   8(R2),C'N'                                                       
         BE    *+12                                                             
         CLI   8(R2),C'2'                                                       
         BNE   FAXERR                                                           
         MVC   SVTWPR05,8(R2)                                                   
*                                                                               
* VALIDATE PRODUCT (OPTIONAL FIELD) *                                           
*                                                                               
GEN10    LA    R2,TRAPRDH                                                       
         MVC   REQPRD,SPACES                                                    
         MVI   BPRD,0                                                           
         CLI   5(R2),0                                                          
         BNE   GEN20                                                            
         CLI   SVPROF2,C'A'      PRODUCT REQUIRED AND CAN BE 'ALL'              
         BE    PRDREQER                                                         
         CLI   SVT1PR16,C'Y'       SPECIFIC PRODUCT REQUIRED                    
         BE    PRDREQER                                                         
         CLI   SVPROF2,C'Y'        PRODUCT REQUIRED                             
         BNE   GEN26                                                            
         B     PRDREQER                                                         
*                                                                               
GEN20    CLC   =C'POL',8(R2)                                                    
         BE    POLPRERR                                                         
         CLC   =C'ALL',8(R2)                                                    
         BE    GEN22                                                            
         GOTO1 VALIPRD                                                          
         MVC   QPRD,WORK                                                        
         MVC   REQPRD,WORK                                                      
         MVC   BPRD,WORK+3                                                      
         MVC   PRDNM,WORK+5                                                     
         OC    PRDNM,SPACES        NEEDED IF FAXING                             
         B     GEN26                                                            
*                                                                               
GEN22    CLI   SVPROF2,C'A'         SEE IF PRODUCT 'ALL' ALLOWED                
         BNE   POLPRERR                                                         
         CLI   SVT1PR16,C'Y'       SPECIFIC PRODUCT REQUIRED                    
         BNE   POLPRERR                                                         
         MVC   REQPRD,=C'ALL'                                                   
*                                                                               
* SEE IF LOCKET HAS THIS LOCKED OUT *                                           
*                                                                               
GEN26    DS   0H                                                                
         CLI  OFFLINE,C'Y'         IF OFFLINE                                   
         BE   GEN28                 DON'T BOTHER                                
*                                                                               
*        SKIP VALILOC CALL WHEN IN TEST MODE                                    
         TM    SVOPTSW,OPTTEST     TEST ON                                      
         BO    GEN28                                                            
*                                                                               
         MVC   DUB,=X'E3010A2500000000' T=TEST, 01=1 ENTRY, 0A25 RECS           
         GOTO1 VALILOC,0                                                        
*                                                                               
GEN28    LA    R2,TRAPTRH          PARTNER CODE                                 
         XC    QPRD2,QPRD2                                                      
         MVI   BPRD2,0                                                          
         XC    PRD2NM,PRD2NM                                                    
         CLI   5(R2),0                                                          
         BE    GEN30                                                            
         CLC   =C'NONE',8(R2)                                                   
         BNE   *+12                                                             
         MVI   BPRD2,X'FF'                                                      
         B     GEN30                                                            
*                                                                               
         CLC   TRAPRD(3),=C'ALL'  NO PARTNER ALLOWED FOR 'ALL' PRDS             
         BE    INVPRDER                                                         
*                                                                               
         GOTO1 VALIPRD                                                          
*                                                                               
         CLC   =C'POL',WORK        PRODUCT POL INVALID                          
         BE    INVPRDER                                                         
         MVC   QPRD2,WORK           SAVE EBCIC PRODUCT                          
         MVC   BPRD2,WORK+3         SAVE BINARY PRODUCT                         
         MVC   PRD2NM,WORK+5        SAVE PRODUCT NAME                           
*                                                                               
* CONTACT IS REQUIRED BUT NOT VALIDATED (UNLESS CON=) *                         
GEN30    LA    R2,TRACONH                                                       
*                                                                               
*************************************************                               
* TEST FOR CODE TO DELETE ALL SHIPPING ELEMENTS *                               
*************************************************                               
*                                                                               
         CLC   =C'MELDEL',TRACON                                                
         BC    0,DEL                                                            
*                                                                               
         BRAS  RE,VALCONT          GO CHECK OUT CONTACT                         
*                                                                               
* VALID OPTIONS ARE TEST, RERUN, CML/COM/CMML, CLASS, MKT, FAX, T/A *           
*                                                                               
         BRAS  RE,VOPT                                                          
*                                                                               
* VALIDATE HOUSE ENTRY IF ANY *                                                 
*                                                                               
         BRAS  RE,RPHSE                                                         
*                                                                               
         OC    SVOPTICA,SVOPTICA   OPTICA ?                                     
*NOP>>>  BNZ   GEN32                                                            
         B     GEN40                                                            
         CLI   TRAPRIH+5,0                                                      
         BE    GEN40                                                            
         B     PRIERR                                                           
*                                                                               
GEN32    CLI   TRAPRIH+5,0                                                      
         BNE   GEN34                                                            
         MVI   SVPRI,C'N'          DEFAULT TO NEXTDAY DELIVERY                  
         MVI   TRAPRI,C'3'                                                      
         OI    TRAPRIH+6,X'80'     TRANSMIT                                     
         B     GEN36               MUST ENTER PRIORITY                          
*                                                                               
GEN34    MVC   SVPRI,TRAPRI        PRESET PRIORITY TO 1 HR                      
         CLI   TRAPRI,C'1'                                                      
         BNE   GEN34C                                                           
         CLC   SVOPTICA,=C'YNG'    NO 1 HOUR SERVICE FOR YNG                    
         BE    PRIERR                                                           
         CLC   SVOPTICA,=C'SPG'    OR SPG                                       
         BE    PRIERR                                                           
         B     GEN36                                                            
                                                                                
GEN34C   MVI   SVPRI,C'4'          FOUR HOUR                                    
         CLI   TRAPRI,C'2'                                                      
         BE    GEN36                                                            
         MVI   SVPRI,C'N'          PRESET OPTICA DELIVERY TO NEXTDAY            
         CLI   TRAPRI,C'3'                                                      
         BE    GEN36                                                            
         LA    R2,TRAPRIH                                                       
         MVI   SVPRI,0                                                          
         MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
*                                                                               
GEN36    CLI   TRAPSRH+5,0         PRODUCTION SERVICES REQUIRED                 
         BNE   GEN38                                                            
         MVC   SVPSR,=C'false'     DEFAULT                                      
         MVI   TRAPSR,C'N'                                                      
         OI    TRAPRIH+6,X'80'     TRANSMIT                                     
         B     GEN40                                                            
*                                                                               
GEN38    MVC   SVPSR,=C'true '                                                  
         CLI   TRAPSR,C'Y'         VALID ENTRIES Y/N                            
         BE    GEN40                                                            
         MVC   SVPSR,=C'false'                                                  
         CLI   TRAPSR,C'N'                                                      
         BE    GEN40                                                            
         LA    R2,TRAPSRH                                                       
         MVI   SVPSR,0                                                          
         MVI   ERROR,INVALID                                                    
         GOTO1 ERREX                                                            
*                                                                               
GEN40    TM    WHEN,X'10'          TEST OVERNT                                  
         BZ    GEN50                NO - PROCESS                                
         CLI   SVTWPR05,C'N'       THIS A FAX REQUEST                           
         BNE   FAXOFFER             YES, FAX OFFLINE ERROR                      
*                                                                               
* CHECK IF MANUAL PURCHASE ORDER ENTERED *                                      
*                                                                               
GEN50    MVC   SVPONUM,SPACES                                                   
         LA    R2,TRAPOH                                                        
         CLI   5(R2),0             ANY ENTRY                                    
         BE    GEN70                NO                                          
         CLI   SVPROF9,C'Y'       AUTO GEN OF PURCHASE ORDER                    
         BE    AUTOPOER                                                         
*                                                                               
         LLC   R1,TRAPOH+5                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVPONUM(0),TRAPO    SAVE PO NUMBER                               
*                                                                               
         B     GEN70                                                            
*                                                                               
FAXERR   MVC   GERROR,=Y(BADFAX)                                                
         GOTO1 VTRAERR                                                          
*                                                                               
PRIERR   LA    R2,TRAPRIH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(36),=CL36'* OPTICA ONLY FIELD *'                         
         GOTO1 ERREX2                                                           
*                                                                               
*================================================================               
* SPECIAL CODE TO FIX BAD RECORDS - REQUIRES PATCH TO REACH                     
*================================================================               
*                                                                               
DEL      XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A25'                                                  
         MVC   KEY+2(3),BAGYMD                                                  
         GOTO1 HIGH                                                             
         B     DEL4                                                             
*                                                                               
DEL2     GOTO1 SEQ                                                              
*                                                                               
DEL4     CLC   KEY(5),KEYSAVE                                                   
         BNE   DELX                                                             
         OC    KEY+10(3),KEY+10    TEST SEQ NUM 0                               
         BNZ   DEL2                                                             
         OI    KEY+13,X'80'                                                     
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTDIR',KEY,KEY                        
         B     DEL2                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         GOTO1 PUTREC                                                           
         B     DEL2                                                             
*                                                                               
DELX     DS    0H                                                               
         MVC   CONHEAD(31),=C'** SHIPPING ELEMENTS DELETED **'                  
         GOTO1 ERREX2                                                           
         EJECT                                                                  
*                                                                               
* USE LOCKET TO LOCK ALL 0A25 (SHIPPING RECAP) RECS THIS MEDIA/CLIENT *         
*                                                                               
GEN70    CLI  OFFLINE,C'Y'         IF OFFLINE                                   
         BE   GEN100                DON'T BOTHER                                
*                                                                               
         TM    WHEN,X'20'           IF SOON                                     
         BZ    GEN100                THEN LOCK                                  
*                                                                               
         TM    SVOPTSW,OPTTEST     TEST ON                                      
         BO    GEN80                                                            
*                                                                               
         MVI   TWAWHEN,5           FRED'S NEW UPDATIVE SOON'S                   
*                                                                               
         MVC   DUB,=X'D3010A2500000000' L=LOCK 01=1 ENTRY, 0A25 RECS            
         GOTO1 VALILOC,0                                                        
*                                                                               
GEN80    DS    0H                                                               
         CLI   SVTWPR05,C'N'       FAXING W/AGY COPY                            
         BE    GEN100               NO                                          
         MVC   REMUSER,=C'SHP'                                                  
*                                                                               
* STORAGE TABLE ALLOCATION                                                      
*                                                                               
GEN100   CLI   MODE,PRINTREP                                                    
         JNE   EXIT                                                             
*                                                                               
         CLC   TRAPRD(3),=C'ALL'   SEE IF DOING ALL PRODUCT REQ                 
         BNE   GEN102                                                           
         BAS   RE,NEXTPRD                                                       
         BNE   PRT220             LAST PRODUCT                                  
*                                                                               
GEN102   MVI   TABLSIZE,0          SET TABLES EXCEEDED SW OFF                   
         LA    R0,BLOCK                                                         
         LHI   R1,480                                                           
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    SVSTCMLT,SVSTCMLT   CLEAR STATION CML TYPE                       
         XC    SVCMLTYP,SVCMLTYP   AND COMMERCIAL TYPE                          
*                                                                               
         L     RE,AIO2                                                          
         ST    RE,ACMLST           FILM LIST                                    
         L     RE,AIO3                                                          
         AHI   RE,-24                                                           
         ST    RE,ACMLSTX                                                       
*                                                                               
         LAY   RE,DYNAWORK                                                      
         ST    RE,ASTLIST          STATION LIST                                 
         AHI   RE,5000                                                          
         LR    R0,RE                                                            
         AHI   R0,-24                                                           
         ST    R0,ASTLISTX                                                      
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'  GET TRPACK ADDRESS                       
         GOTO1 CALLOV,DMCB,0                                                    
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    GEN110                                                           
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVI   DMCB,X'8C'          SET OVERLAY NUMBER                           
         GOTO1 CALLOV,DMCB,,ATWA                                                
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RE,0(R1)                                                         
         L     R0,8(RE)            PHASE LEN                                    
         LA    RE,16(RE)            DO NOT OVERWRITE OVLY/LEN                   
         MVC   0(8,RE),=CL8'*SHPSMST'                                           
         LA    RE,8(RE)                                                         
         ST    RE,ASHPSUM                                                       
         AR    RE,R0                                                            
         AHI   RE,-L'CTABENT                                                    
         ST    RE,ASHPSUMX         END OF SHIPPING SUMMARY                      
         B     GEN120                                                           
         EJECT                                                                  
*=========================================================                      
* ALLOCATE STORAGE FOR OFFLINE PROCESSING                                       
*=========================================================                      
                                                                                
GEN110   L     RE,VADUMMY                                                       
*                                                                               
         MVC   0(8,RE),=C'*STALST*'                                             
         LA    RE,8(,RE)                                                        
         ST    RE,ASTLIST          STATION LIST START                           
         A     RE,=F'120000'                                                    
         LR    R0,RE                                                            
         AHI   R0,-24                                                           
         ST    R0,ASTLISTX         STATION LIST END                             
*                                                                               
         MVC   0(8,RE),=C'*CMLLST*'                                             
         LA    RE,8(RE)                                                         
         ST    RE,ACMLST           SET FILM LIST START                          
         A     RE,=F'80000'                                                     
         AHI   RE,-(L'CMLCMMLS+8)                                               
         ST    RE,ACMLSTX          SET FILM LIST END                            
*                                                                               
         AHI   RE,(L'CMLCMMLS+8)                                                
         MVC   0(8,RE),=C'*SHPSUM*'                                             
         LA    RE,8(RE)                                                         
         ST    RE,ASHPSUM          START OF SHIPPING SUMMARY                    
*                                                                               
         A     RE,=F'40000'                                                     
         AHI   RE,-L'CTABENT+4                                                  
         ST    RE,ASHPSUMX         CML SHIPPING SUMMARY END                     
*                                                                               
         EJECT                                                                  
*========================================================                       
* CLEAR ALL LIST AREAS                                                          
*========================================================                       
                                                                                
GEN120   LM    RE,RF,ASTLIST       CLEAR STATION LIST                           
         SR    RF,RE                                                            
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LM    RE,RF,ACMLST        CLEAR CML LISTS                              
         SR    RF,RE                                                            
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LM    RE,RF,ASHPSUM       CLEAR SHIP SUM                               
         SR    RF,RE                                                            
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         LA    RE,AMSGCTAB                                                      
         LA    RF,L'AMSGCTAB                                                    
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         CLI   PQSW,1              TEST PRTQUE OPEN                             
         BNE   GEN130              YES                                          
*                                                                               
         CLI   SVTWPR05,C'Y'       IF ONLY FAXING                               
         BE    GEN130              SKIP THIS PQ OPEN                            
         CLI   SVTWPR05,C'2'       IF FAX WITH AGY COPY                         
         BNE   *+12                                                             
         TM    WHEN,X'20'          AND SOON                                     
         BO    GEN130              SKIP THIS PQ OPEN                            
*                                                                               
         LA    R1,ELEM                                                          
         ST    R1,SPOOLQLK                                                      
         XC    ELEM(128),ELEM                                                   
         USING PQPLD,R1                                                         
         MVI   QLEXTRA,X'FF'       NEW PARM LIST                                
*                                                                               
         TM    WHEN,X'40'          SET ARCHIVING TYPE FOR NONFAX                
         BZ    *+10                                                             
         MVC   QLTYP1,SVQLTYP1                                                  
*                                                                               
         OI    SPOOLIND,X'40'      USER VALUES PRESENT                          
         GOTO1 OPENPQ                                                           
*                                                                               
GEN130   XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A25'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/C                                        
         MVC   KEY+5(3),SVOPTMKT                                                
*                                                                               
GEN140   DS   0H                                                                
         TM    SVOPTSW,OPTTEST     TEST ON                                      
         BZ    *+8                                                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(5),KEYSAVE      TY/A-M/CLT                                   
         BNE   NODATA                                                           
         MVC   KEYSAVE,KEY         FORCE EQUAL                                  
*                                                                               
         LA    R0,L'STLDATA                                                     
         L     R2,ASTLIST                                                       
         LR    R1,R2                                                            
         XC    0(L'STLDATA,R1),0(R1)                                            
         AR    R1,R0                                                            
         C     R1,ASTLISTX                                                      
         BL    *-12                                                             
         SR    R1,R0                                                            
         ST    R1,ASTLISTX                                                      
*                                                                               
         USING STALISTD,R2                                                      
*                                                                               
         L     R4,ACMLST                                                        
         BAS   RE,SETSTA                                                        
         BNE   PRINTCON                                                         
         B     GEN154                                                           
*                                                                               
GEN150   DS   0H                                                                
         TM    SVOPTSW,OPTTEST     TEST ON                                      
         BZ    *+8                                                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
         OI    GENSTAT1,CATCHIOR   SET USER WANTS TO REGAIN CONTROL             
         MVI   ERROR,0                                                          
         GOTO1 CATCHIOS                                                         
         CLI   ERROR,0             WE OVER 90 PERCENT                           
         BNE   IOSERR               YES                                         
*                                                                               
GEN154   OC    KEY+10(3),KEY+10    TEST CMML SEQ REC                            
         BZ    GEN150              YES - IGNORE                                 
*                                                                               
         OC    SVOPTMKT,SVOPTMKT   FILTERING ON MARKET                          
         BZ    GEN156                                                           
         CLC   KEY+5(2),SVOPTMKT                                                
         BE    GEN156                                                           
         BRAS  RE,ENDSTA                                                        
         L     R4,NEXTADDR         PICK UP RESET LIST POINTER                   
         B     PRINT                                                            
*                                                                               
GEN156   CLC   KEY(10),KEYSAVE     TY/A-M/C/MKT/STA                             
         BE    GEN160                                                           
*                                                                               
         BRAS  RE,ENDSTA                                                        
         L     R4,NEXTADDR         PICK UP RESET LIST POINTER                   
*                                                                               
         CLC   KEY(5),KEYSAVE      TYPE/A-M/CLT                                 
         BNE   PRINT                                                            
*                                                                               
         MVC   KEYSAVE,KEY         FORCE KEYS EQUAL                             
*                                                                               
         OC    STLDATA,STLDATA     TEST PREVIOUS ENTRY ERASED                   
         BZ    *+8                 YES - USE IT AGAIN                           
         LA    R2,L'STLDATA(R2)                                                 
*                                                                               
         BAS   RE,SETSTA                                                        
         BNE   PRINTCON                                                         
*                                                                               
GEN160   OC    SVOPTAFF,SVOPTAFF                                                
         BZ    GEN164                                                           
         CLC   STLAFF,SVOPTAFF                                                  
         BNE   GEN150                                                           
         CLI   STLAMSGC,0          IS THIS AN AMS WITH GROUP CODE               
         BNE   GEN150                                                           
*                                                                               
GEN164   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         TM    SVOPTSW,OPTTEST     TEST ON                                      
         BZ    *+8                                                              
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         OI    GENSTAT1,CATCHIOR   SET USER WANTS TO REGAIN CONTROL             
         MVI   ERROR,0                                                          
         GOTO1 CATCHIOS                                                         
         CLI   ERROR,0             WE OVER 90 PERCENT                           
         BNE   IOSERR               YES                                         
*                                                                               
         MVI   BYTE,0              RESET UPDATE SWITCH                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GEN150                                                           
         B     *+12                                                             
*                                                                               
GEN170   MVI   ELCODE,X'10'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   GEN220                                                           
*                                                                               
         USING SHPDTAEL,R6                                                      
*                                                                               
         CLI   SHPPIG,0            TEST PASSIVE PIGGYBACK                       
         BNE   GEN170                                                           
         TM    SHPNOSHP,X'80'      TEST NO-SHIP REQUEST                         
         BO    GEN170                                                           
*                                                                               
         TM    SVOPTSW,OPTDATE     AS OF DATE RUN                               
         BZ    *+14                                                             
         CLC   SHPQDATE,SVOPTDTE   TEST INSTR DATE PRIOR TO THIS DATE           
         BL    GEN170              YES - IGNORE                                 
*                                                                               
         CLC   SHPLTD,TODAY        IF LTD NOT NOW OR FUTURE                     
         BL    GEN170              BYPASS                                       
*                                                                               
         OC    SHPSHPDT,SHPSHPDT   TEST SHIPPED                                 
         BZ    GEN180                                                           
*                                                                               
         TM    SVOPTSW,OPTRERUN    TEST RERUN                                   
         BZ    GEN170                                                           
         CLC   SHPSHPDT,SVOPTDTE   TEST SHIPPED PRIOR TO RERUN DATE             
         BL    GEN170              YES - IGNORE                                 
*                                                                               
GEN180   OC    SVCMLADI,SVCMLADI                                                
         BZ    GEN185                                                           
         TM    SHPNOSHP,SHPISADI                                                
         BZ    GEN170                                                           
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'U',SHPCMML),SVADIWRK                             
         BNE   GEN183                                                           
         ZIC   R1,SVADILEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SVCMLADI(0),SVADIWRK                                             
         BE    GEN190                                                           
*                                                                               
GEN183   GOTO1 VTRPACK,DMCB,(C'U',SHPCMML2),SVADIWRK                            
         BNE   GEN170                                                           
         ZIC   R1,SVADILEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SVCMLADI(0),SVADIWRK                                             
         BE    GEN190                                                           
         B     GEN170                                                           
*                                                                               
GEN185   CLI   SVCMLLN,0           TEST FILTER ACTIVE                           
         BE    GEN190                                                           
         LLC   RE,SVCMLLN                                                       
         BCTR  RE,0                                                             
         EX    RE,TESTCML1                                                      
         BE    GEN190                                                           
         EX    RE,TESTCML2                                                      
         BE    GEN190                                                           
         B     GEN170                                                           
*                                                                               
TESTCML1 CLC   SVOPTCML(0),SHPCMML *EXECUTED*                                   
TESTCML2 CLC   SVOPTCML(0),SHPCMML2 *EXECUTED*                                  
         EJECT                                                                  
GEN190   BRAS  RE,CML              GO CHECK COMERCIALS                          
         BNE   GEN170               NEXT ELEM                                   
*                                                                               
         CLC   SHPFTD,STLFTD                                                    
         BNL   *+10                                                             
         MVC   STLFTD,SHPFTD       SAVE LOWEST FIRST TLCST DATE                 
*                                                                               
         C     R4,ACMLSTX                                                       
         BL    GEN200                                                           
*                                                                               
* NO MORE ROOM                                                                  
*                                                                               
         L     RE,STLACML          GET CMML LIST ADDRESS                        
         XC    0(2,RE),0(RE)       CLEAR LENGTH FIELD                           
         XC    STLDATA,STLDATA     CLEAR CURRENT STATION ENTRY                  
         OI    TABLSIZE,CMLSIZER   SET CML LISTS TABLE TOO SMALL SW             
         CLI   OFFLINE,C'Y'                                                     
         BE    PRINTCON                                                         
         MVC   TABSIZMS+9(3),=C'CML'                                            
         B     TABSIZER                                                         
*                                                                               
         USING SHPDTAEL,R6                                                      
GEN200   MVI   BYTE,C'Y'           SET RECORD UPDATED SWITCH                    
*                                                                               
         LR    RE,R4                                                            
         AHI   RE,-(L'CMLCMMLS)                                                 
         CLC   0(16,RE),2(R6)      THIS EQUAL TO PREVIOUS?                      
         BE    GEN212               YES, NO ADD TO TABLE                        
*                                                                               
         MVC   0(16,R4),2(R6)                                                   
*                                                                               
         MVC   16(4,R4),SVCMLTYP                                                
*                                                                               
         CLI   SVT1PR11,C'Y'       USE STATION CML TYP IF ANY                   
         BNE   GEN210                                                           
         OC    SVSTCMLT,SVSTCMLT   ANY CML TYPE FOR THIS STATION                
         BZ    GEN210                                                           
         MVC   16(4,R4),SVSTCMLT   CMML TYPE FROM STATION ADDR                  
*                                                                               
GEN210   LA    R4,L'CMLCMMLS(,R4)                                               
*                                                                               
* SET SHIP DATE IN ELEMENT *                                                    
*                                                                               
GEN212   DS   0H                                                                
         MVC   SHPSHPDT,TODAY                                                   
*                                                                               
         CLI   SVTSPR03,C'Y'       SHIP EACH COMML SEPARATELY                   
         BNE   GEN170                                                           
*                                                                               
         MVC   ELEM(L'STLDATA),STLDATA  SAVE STATION ENTRY                      
*                                                                               
         BRAS  RE,ENDSTA                                                        
         L     R4,NEXTADDR                                                      
*                                                                               
         OC    STLDATA,STLDATA     WAS ENTRY USED                               
         BNZ   GEN214                                                           
         MVC   STLDATA,ELEM        RESTORE STATION ENTRY                        
         B     GEN216                                                           
*                                                                               
GEN214   LA    R2,STLNEXT                                                       
         C     R2,ASTLISTX         CK IF OUT OF ROOM                            
         BNL   GEN218               GO TO NORMAL ERROR                          
         MVC   STLDATA,ELEM        SET NEW STATION ENTRY                        
GEN216   ST    R4,STLACML                                                       
         XC    0(5,R4),0(R4)                                                    
         LA    R4,CMLCMMLS-CMLLISTD(,R4)                                        
         MVC   STLFTD,=3X'FF'                                                   
         B     GEN170                                                           
GEN218   BAS   RE,SETSTA                                                        
         BNE   PRINTCON                                                         
         DC    H'0'                                                             
*                                                                               
* TEST RECORD UPDATED AND WRITE IF NECESSARY *                                  
*                                                                               
GEN220   TM    SVOPTSW,OPTTEST                                                  
         BO    GEN230                                                           
         TM    SVOPTSW,OPTCPY2                                                  
         BO    GEN230                                                           
         CLI   BYTE,0                                                           
         BE    GEN230                                                           
         TM    SVOPTSW,OPTRERUN    TEST RERUN                                   
         BO    GEN230              DO NOT MARK FILE AGAIN                       
         GOTO1 PUTREC                                                           
*                                                                               
GEN230   B     GEN150                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
*==================================                                             
* INIT STUFF                                                                    
*==================================                                             
*                                                                               
INIT     NTR1                                                                   
*                                                                               
         L     RE,=A(COMSUBS)      COMMON AREA                                  
         ST    RE,ACOMSUB                                                       
*                                                                               
         L     RF,=A(BLDESHP)                                                   
         ST    RF,ABLDESHP                                                      
*                                                                               
         L     RF,=A(MAKEFILE)                                                  
         ST    RF,AMKFILE                                                       
*                                                                               
         L     RF,=A(MAKEXML)                                                   
         ST    RF,AMKXML                                                        
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'  GET TRPACK ADDRESS                       
         GOTO1 CALLOV,DMCB,0                                                    
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
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
         BL    NEQXIT                                                           
*                                                                               
*                                                                               
NXTPRD70 MVC   QPRD,0(R1)          SET AS REQUESTED PRODUCT                     
         MVC   BPRD,3(R1)                                                       
*                                                                               
         LA    R3,QPRD                                                          
         LA    R4,PRDNM                                                         
         BAS   RE,GETPRDNM         GET PRODUCT NAME                             
         BNE   NXTPRD40                                                         
         B     EQXIT                                                            
         EJECT                                                                  
* SUBROUTINE TO READ PRODUCT HEADER AND EXTRACT PRD NAME *                      
*                                                                               
GETPRDNM NTR1                                                                   
*                                                                               
         BAS   RE,INITSPT                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+4(3),0(R3)      QPRD                                         
*                                                                               
         TM    SVOPTSW,OPTTEST     TEST ON                                      
         BZ    *+8                                                              
GETP10   MVI   RDUPDATE,C'N'                                                    
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NEQXIT                                                           
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         TM    SVOPTSW,OPTTEST     TEST ON                                      
         BZ    *+8                                                              
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         USING PRDHDRD,R6                                                       
*                                                                               
         CLC   =C'POL',KEY+4       IS THIS PRD POL?                             
         BE    GETP20                                                           
*                                                                               
         MVC   0(L'PRDNM,R4),PNAME                                              
         OC    0(L'PRDNM,R4),SPACES        NEEDED IF FAXING                     
*                                                                               
GETP20   MVC   TALID,PTALAGY               TALENT ID                            
         OC    TALID,TALID                                                      
         BNZ   EQXIT                                                            
*                                                                               
         CLC   =C'POL',KEY+4       DID PRD POL?                                 
         BE    EQXIT                                                            
*                                                                               
         MVC   KEY+4(3),=C'POL'    CHECK TALENT ID FOR POL                      
         B     GETP10                                                           
*                                                                               
*SM                                                                             
EQXIT    CR    RE,RE                       RETURN WITH EQUAL                    
         BE    *+6                                                              
NEQXIT   LTR   RE,RE                                                            
         BAS   RE,INITTRF                                                       
         XIT1                                                                   
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
INITSPT  MVI   SYSDIR,C'S'                                                      
         MVI   SYSDIR+1,C'P'                                                    
         MVI   SYSDIR+2,C'T'                                                    
         MVC   SYSFIL(3),SYSDIR                                                 
         BR    RE                                                               
*                                                                               
INITTRF  MVI   SYSDIR,C'T'                                                      
         MVI   SYSDIR+1,C'R'                                                    
         MVI   SYSDIR+2,C'F'                                                    
         MVC   SYSFIL(3),SYSDIR                                                 
         BR    RE                                                               
*==================================================                             
* INITIALIZE STATION LIST ENTRY AND                                             
* GET STATION CML TYPE (IF ANY)                                                 
*==================================================                             
                                                                                
SETSTA   NTR1                                                                   
         C     R2,ASTLISTX         TEST ROOM FOR MORE STATIONS                  
         BNL   SETSTA30                                                         
         ST    R4,STLACML                                                       
*                                  POINT TO FIRST COMMERCIAL SLOT               
         XC    0(5,R4),0(R4)                                                    
         LA    R4,CMLCMMLS-CMLLISTD(,R4)                                        
         MVC   STLMKT(5),KEY+5     MKT/STA                                      
         MVC   STLFTD,=3X'FF'                                                   
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'80',STLMKT),QMKT,DUB                              
*                                                                               
         MVC   QSTA,DUB                                                         
         CLI   QSTA+4,C' '                                                      
         BNE   *+8                                                              
         MVI   QSTA+4,C'T'                                                      
*                                                                               
         XC    SVSTCMLT,SVSTCMLT                                                
         XC    SVKEY,SVKEY                                                      
*                                                                               
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING STARECD,R6                                                       
         MVC   STAKID,=X'0A28'                                                  
         MVC   STAKAM,BAGYMD                                                    
         MVC   STAKSTA,QSTA                                                     
         DROP  R6                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   SETSTA10                                                         
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'20'        AMS DATA ELEM                                
         BRAS  RE,GETEL                                                         
         BNE   SETSTA06            ELEM NOT FOUND                               
         USING STAAMSEL,R6                                                      
         OC    STAAMSGC,STAAMSGC   CHECK IF GROUP CODE                          
         BZ    SETSTA06             NO                                          
         CLC   STAAMSGC,SPACES     CHECK IF GROUP CODE                          
         BE    SETSTA06             NO                                          
*                                                                               
         LA    R0,AMSGCLEN                                                      
         LA    R1,AMSGCTAB                                                      
         LA    RF,1                                                             
SETSTA02 OC    0(L'STAAMSGC,R1),0(R1)                                           
         BZ    SETSTA03                                                         
         CLC   STAAMSGC,0(R1)                                                   
         BE    SETSTA04                                                         
         LA    R1,L'STAAMSGC(,R1)                                               
         LA    RF,1(,RF)                                                        
         BCT   R0,SETSTA02                                                      
         DC    H'0'                                                             
SETSTA03 MVC   0(L'STAAMSGC,R1),STAAMSGC                                        
*                                                                               
SETSTA04 STC   RF,STLAMSGC                                                      
*                                                                               
SETSTA06 L     R6,AIO1                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING STADTAEL,R6                                                      
         CLI   STADTALN,119        OLD ELEM                                     
         BE    SETSTA10             NO TYPE THEN                                
*                                                                               
         CLI   SVTSPR04,C'Y'       SUPPRESS COMMERCIAL TYPE                     
         BE    SETSTA10                                                         
*                                                                               
         CLI   SVT1PR11,C'Y'       USE STATION CML TYP IF ANY                   
         BNE   SETSTA10                                                         
*                                                                               
         MVC   SVSTCMLT,STACMLT    SAVE STATION COMMERCIAL TYPE                 
*                                                                               
SETSTA10 CLI   SVTSPR01,C'Y'       USE GENERIC STATION FOR MARKET               
         BE    SETSTA12                                                         
         CLI   SVTSPR02,C'Y'       PAGE BREAK BY STATION AFFILIATE              
         BNE   SETSTA18                                                         
*                                                                               
* READ STATION MASTER RECORD FOR AFFL/TYPE *                                    
*                                                                               
SETSTA12 OC    SVKEY,SVKEY                                                      
         BNZ   *+10                                                             
         MVC   SVKEY,KEY                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),TRAMED                                                  
         MVC   KEY+2(5),QSTA                                                    
         MVC   KEY+7(2),AGENCY                                                  
*                                                                               
         CLI   SVTSPR01,C'Y'       USE GENERIC STATION FOR MARKET               
         BE    *+10                                                             
         MVC   KEY+9(3),QCLT                                                    
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'STATION',KEY,AIO                      
*                                                                               
         L     R6,AIO                                                           
         USING STAMASD,R6                                                       
*                                                                               
         CLI   SVTSPR01,C'Y'       USE GENERIC STATION FOR MARKET               
         BNE   SETSTA14                                                         
         PACK  DUB,SMKT                                                         
         CVB   R0,DUB                                                           
         STCM  R0,3,STLMKT                                                      
*                                                                               
SETSTA14 CLI   SVTSPR02,C'Y'       PAGE BREAK BY STATION AFFILIATE              
         BNE   SETSTA18                                                         
*                                                                               
         MVC   STLAFF,SNETWRK                                                   
*                                                                               
SETSTA18 OC    SVKEY,SVKEY                                                      
         BZ    SETSTA20                                                         
         MVC   KEY(13),SVKEY                                                    
*                                                                               
         TM    SVOPTSW,OPTTEST     TEST ON                                      
         BZ    *+8                                                              
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BE    SETSTA20                                                         
         DC    H'0'                                                             
SETSTA20 CR    RB,RB                                                            
*                                                                               
SETSTAX  XIT1  REGS=(R4)                                                        
         DROP  R6                                                               
*                                                                               
SETSTA30 OI    TABLSIZE,STASIZER   SET STATION TABLE TOO SMALL SW               
         MVC   TABSIZMS+9(3),=C'STA'                                            
         CLI   OFFLINE,C'Y'                                                     
         BNE   TABSIZER                                                         
         CR    RB,RD                                                            
         B     SETSTAX                                                          
         EJECT                                                                  
*                                                                               
*====================================================                           
* ROUTINES TO PRINT THE SHIPPING ORDER                                          
*====================================================                           
                                                                                
PRINTCON XC    SVKEY,SVKEY                                                      
         MVC   SVKEY(10),KEY        SAVE KEY THRU STATION                       
         B     PRT02                                                            
*                                                                               
PRINT    XC    SVKEY,SVKEY         SET TO NOT CONTINUE                          
*                                                                               
*DELIVERY PRIORITY 1=1, 2=4, 3=N                                                
PRT02    MVI   SVPRI,C'N'          PRESET OPTICA DELIVERY TO NEXTDAY            
         CLI   TRAPRI,C'3'                                                      
         BE    PRT02C                                                           
         MVC   SVPRI,TRAPRI                                                     
         CLI   TRAPRI,C'1'         ONE HOUR (RUSH)                              
         BE    PRT02C                                                           
         MVI   SVPRI,C'4'          FOUR HOUR                                    
*                                                                               
* SEE IF ANY COMMENTS 1-4                                                       
PRT02C   LA    R4,SVCMT            SAVE COMMENTS AREA                           
         LA    R2,TRACMT1H                                                      
         LA    R5,4                                                             
         LR    RE,R2                                                            
         LR    RF,R5                                                            
*                                                                               
PRT02E   CLI   5(RE),0             ANY DATA                                     
         BNE   PRT02F               YES                                         
         LLC   R0,0(RE)                                                         
         AR    RE,R0                                                            
         IC    R0,0(RE)                                                         
         AR    RE,R0                                                            
         BCT   RF,PRT02E                                                        
         B     PRT03               NO ENTRIES, BYPASS                           
*                                                                               
PRT02F   CLI   5(R2),0             ANY DATA                                     
         BE    PRT02G               NO                                          
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,PRTMVC                                                        
*                                                                               
         AR    R4,R1               BUMP PAST END OF COMMENT-1                   
         MVI   1(R4),X'40'         SPACE BETWEEN EACH COMMENT LINE              
         LA    R4,2(R4)                                                         
*                                                                               
PRT02G   LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R5,PRT02F                                                        
         B     PRT03                                                            
PRTMVC   MVC   0(0,R4),8(R2)                                                    
*                                                                               
*<<<<<<<<<<<<                                                                   
PRT03    MVC   SVPSR,=C'false'     PRESET TO DEFAULT PROD SERVICES REQ          
         CLI   TRAPSR,C'N'                                                      
         BE    PRT03B                                                           
         MVC   SVPSR,=C'true '                                                  
*                                                                               
* SORT INTO SEQUENCE BY LIST/MKT/STA *                                          
*                                                                               
*                                                                               
PRT03B   SR    R0,R0                                                            
         L     R2,ASTLIST                                                       
         OC    0(4,R2),0(R2)                                                    
         BZ    *+12                                                             
         LA    R2,L'STLDATA(R2)                                                 
         BCT   R0,*-14                                                          
         LPR   R0,R0                                                            
         BZ    PRT100                                                           
         GOTO1 XSORT,DMCB,ASTLIST,(R0),L'STLDATA,L'STLDATA,0                    
         EJECT                                                                  
*                                                                               
*      SEE IF FAXING SHIPPING ORDERS TO WESTERN UNION                           
*      IF SO - MUST USE GLASS G AND SEND SPECIAL PRINT LINE FIRST               
*                                                                               
         TM    SVOPTSW1,OPTHDCPY                                                
         BO    PRT06                                                            
         CLI   SVTWPR05,C'2'                                                    
         BNE   *+12                                                             
         TM    WHEN,X'20'          IF SOON, WITH AGY COPY                       
         BZ    *+12                                                             
         OI    SVOPTSW,OPTCPY2     DO NOT MARK THE FILE YET                     
         B     *+12                AND 1ST DO FAX                               
         CLI   SVTWPR05,C'Y'                                                    
         BNE   PRT06                                                            
         CLI   PQSW,1                                                           
         BNE   PRT06                                                            
*                                                                               
PRT03C   CLI   OFFLINE,C'Y'                                                     
         BNE   PRT03E                                                           
*                                                                               
         ICM   RE,15,TWAMASTC                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     RF,MCVREMOT-MASTD(RE)                                            
         USING REMOTED,RF                                                       
         MVC   REMOTSYS,MCS2OSYS-MASTD(RE)                                      
         MVI   REMOTCPY,1                                                       
         MVI   REMOTCLS,C'G'                                                    
         MVC   REMOTJID,=C'SHP'                                                 
         OC    REMOTTY1,SVFAXARC                                                
         MVC   REMOTDST,TWAORIG                                                 
*                                                                               
         OC    TWADEST,TWADEST                                                  
         BZ    *+10                                                             
         MVC   REMOTDST,TWADEST                                                 
         DROP  RF                                                               
*                                                                               
PRT03E   LA    R1,ELEM                                                          
         ST    R1,SPOOLQLK                                                      
*                                                                               
         XC    ELEM(128),ELEM                                                   
         USING PQPLD,R1                                                         
         MVC   QLDESC(1),QMED                                                   
         MVC   QLDESC+3(3),QCLT                                                 
         MVC   QLDESC+4(3),REQPRD                                               
         MVI   PLCLASS,C'G'        WESTERN UNION                                
         MVI   QLCLASS,C'G'        ALSO                                         
         OC    QLTYP1,SVFAXARC                                                  
         MVC   REMUSER,=C'SHP'                                                  
*                                                                               
*        MVC   QLRETNL,=H'36'                                                   
*        MVC   QLRETND,=H'12'                                                   
         MVI   QLEXTRA,X'FF'       NEW PARM LIST                                
         OI    SPOOLIND,X'40'      USER VALUES PRESENT                          
         GOTO1 OPENPQ                                                           
         MVI   PQSW,2             SO I WON'T REOPEN FOR NEXT PRODUCT            
*                                 OF A PRD=ALL REQUEST                          
*                                                                               
* SET UP PRINTING ADDRESSABILITY (AFTER PRTQUE OPEN) *                          
*                                                                               
         MVI   MAXLINES,40         SET PAGE SIZE                                
*                                  RESET IN HEADHOOK                            
*                                  AFTER 1ST PAGE IS FAXED                      
*                                                                               
*  WESTERN UNION GETS A PRINT LINE WITH SOME FIXED INFO, AND EITHER THE         
*   TWX NUMBER AND ANSWER BACK CODE, OR A TWX STATION ID                        
*                                                                               
         MVI   FORCEHED,C'N'       PREVENT HEADLINES                            
         MVI   LINE,2                                                           
         MVC   EDIORIG,AGYORIG                                                  
         MVC   EDIHDR,=C'*HDR*'                                                 
*                                                                               
         MVC   EDIDESID(4),=C'FAXW'                                             
*                                                                               
PRT04    MVC   EDIDESID+5(19),PRDHFAX      PRODUCTION HOUSE FAX NUMBER          
         MVC   EDIFDEST(16),PRDHFAX                                             
*                                                                               
PRT05    MVI   EDIWIDE,C'L'                                                     
         MVI   EDIPAGE,C'P'                                                     
*                                                                               
         MVC   EDIBILL(1),QMED    MEDIA                                         
         MVC   EDIBILL+1(3),QCLT    CLIENT                                      
         MVC   EDIBILL+4(3),REQPRD  PRODUCT                                     
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)    SEND SPECIAL PRINT LINE                       
*                                                                               
         MVC   EDIDDSID(14),=C'++DDS SPSRATRN'                                  
         MVC   EDISTSMD(4),QMED   MEDIA & CLIENT                                
         MVC   EDISTSPR,QPRD      PRODUCT                                       
         MVC   EDISTSP2,QPRD2     PARTNER                                       
         MVC   EDISTSCT,QUESTOR   CONTACT                                       
         MVC   EDISTSHS,HOUSECD                                                 
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)    SEND SPECIAL PRINT LINE                       
         EJECT                                                                  
*==========================================================                     
* INITIALIZE FOR HEADLINES FOR SHIPPING ORDERS                                  
*==========================================================                     
                                                                                
PRT06    MVI   FORCEHED,C'Y'                                                    
         LM    RE,RF,=A(HDSPECS,HDHK)                                           
         A     RE,SPTR46RR                                                      
         ST    RE,SPECS                                                         
         A     RF,SPTR46RR                                                      
         ST    RF,HEADHOOK                                                      
         MVI   RCSUBPRG,1                                                       
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
*                                                                               
         LA    R0,14                                                            
         LA    R1,H1                                                            
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
PRT10    L     R2,ASTLIST                                                       
         USING STALISTD,R2                                                      
*                                                                               
         DROP  R3                                                               
PRT12    L     R3,STLACML                                                       
         SR    R4,R4                                                            
         ICM   R4,3,0(R3)          GET LIST LENGTH                              
*                                                                               
         MVC   SVSTAFF,CMLAFF-CMLLISTD(R3)                                      
*                                                                               
         AHI   R4,-(CMLCMMLS-CMLLISTD)                                          
*                                                                               
         MVI   ONECMML,0                                                        
         CHI   R4,(L'CMLCMMLS)     TEST ONLY ONE ENTRY IN LIST                  
         BNE   *+8                                                              
         MVI   ONECMML,X'80'       SET FLAG                                     
*                                                                               
         LA    R3,CMLCMMLS-CMLLISTD(,R3)  POINT TO FIRST ENTRY                  
                                                                                
         BRAS  RE,RPHSE            GET HOUSE RECORD FOR COMMENTS                
         GOTO1 SPOOL,DMCB,(R8)     PRINT HEADLINES NOW!                         
*                                                                               
*SM                                                                             
         OC    SVOPTICA,SVOPTICA                                                
         BZ    PRT14                                                            
*                                                                               
* GET ENVIRONMENT (A)DV, (T)ST, (C)SC                                           
*                                                                               
         TM    WHEN,X'40'          NOW                                          
         BO    PRT14               NO OPTICA FOR "NOW"                          
*                                                                               
         CLI   ENVIRNMT,0          DID WE GET THE ENVIRONMENT ?                 
         BNE   PRT13                 YES                                        
*                                                                               
         BAS   RE,GENVRNMT         GET ENVIRONMENT                              
*                                                                               
PRT13    BAS   RE,GUEMAIL          GET USER EMAIL                               
*                                                                               
* ADD HEADER RECORD -AGY ALPHA, PID, FIRST/LAST NAME,EMAIL ADDR, CAD            
         GOTO1 ABLDESHP,DMCB,('ESAHED',AGENCY),SVSECPID,SVSECFNM,      X        
               SVSECLNM,SVEMAIL,SVOPTICA                                        
*                                      MEDIA  DELAY  ENVIRONMENT                
         GOTO1 ABLDESHP,DMCB,('ESAHED',QMED),SVDELAY,ENVIRNMT                   
*                                                                               
* ADD AGENCY RECORD - USER ID, USER NAME                                        
         GOTO1 ABLDESHP,DMCB,('ESAAGY',AGYORIG),USERNAME                        
*                                                                               
* ADD ADVERTISER RECORD            CLT CODE, CLIENT NAME                        
         GOTO1 ABLDESHP,DMCB,('ESAADV',QCLT),CLTNM                              
*                                                                               
* ADD ORDER RECORD - PRIORITY, PO NUMBER, PROD SERVICES, PRD                    
         GOTO1 ABLDESHP,DMCB,('ESAORD',SVPRI),SVPONUM,SVCMT,SVPSR,     X        
               REQPRD                                                           
*                                                                               
E        USING GETCMMLD,ELEM                                                    
*                                                                               
PRT14    BAS   RE,GETCMML                                                       
*                                                                               
         CLC   =C'CF6',16(R3)      TEST TYPE = CF6                              
         BE    PRT16                                                            
         CLC   =C'CF8',16(R3)      OR CF8                                       
         BNE   PRT18                                                            
*                                                                               
PRT16    TM    ONECMML,X'80'       TEST ONE CMML LIST                           
         BZ    *+8                                                              
         OI    ONECMML,X'81'       SET FLAG TO SHIP TWO COPIES                  
                                                                                
*=============================================================                  
* NOTE THAT AIO1+4000 IS USED LOCALLY HERE AS A PRINT BUFFER                    
*=============================================================                  
                                                                                
MYP      USING P,R5                                                             
*                                                                               
PRT18    L     R5,AIO1                                                          
         LA    R5,4000(R5)                                                      
         ST    R5,FIRSTLIN         SAVE FIRST PRINT LINE ADDRESS                
         LR    R0,R5               CLEAR THE STORAGE                            
         LA    R1,2000                                                          
         SR    RE,RE                                                            
         LA    RF,X'40'            FILL WITH SPACES                             
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLI   SVTSPR05,C'Y'       SUPPRESS COMMERCIAL COUNTS                   
         BE    PRT20                                                            
*                                                                               
         MVI   MYP.PCOMMLCT+3,C'1'   SET TO SHIP ONE PRINT                      
         TM    ONECMML,X'81'         TEST SHIP TWO COPIES                       
         BNO   *+8                                                              
         MVI   MYP.PCOMMLCT+3,C'2'   SET TO SHIP TWO PRINTS                     
*                                                                               
PRT20    MVC   WORK,SPACES                                                      
         MVC   WORK(8),0(R3)       8 CHAR COMML                                 
*                                                                               
         CLI   E.GETCMADI,C' '     TEST AD-ID PRESENT                           
         BNH   *+10                                                             
         MVC   WORK(12),E.GETCMADI                                              
*                                                                               
PRT22    MVC   MYP.PCOMML,WORK      ASSUME NOT A P/B                            
         MVC   MYP.PTYPE,E.GETCMTYP TYPE                                        
*                                                                               
         MVC   MYP.PTITLE1,E.GETCMT1 TITLE                                      
         MVC   MYP.PTITLE2,E.GETCMT2                                            
         MVC   MYP.PTITLE3,E.GETCMT3                                            
         LA    R5,132(R5)          NEXT PRINT LINE                              
*                                                                               
         CLI   E.GETCMCLT,C' '     TEST FOR CLIENT CMML                         
         BNH   PRT24                                                            
         MVC   MYP.PTITLE1(6),=C'CLT# ='                                        
         MVC   MYP.PTITLE1+7(20),E.GETCMCLT                                     
         LA    R5,132(R5)                                                       
*                                                                               
PRT24    OC    8(8,R3),8(R3)       TEST FOR PIGGYBACK                           
         BZ    *+8                                                              
         LA    R5,132(R5)          SKIP A LINE                                  
*                                                                               
         CLI   E.GETCMHDF,C' '                                                  
         BNH   PRT26                                                            
         MVC   MYP.POTHER(12),E.GETCMHDF                                        
         MVC   MYP.PTYPE(4),=C'HDEF'                                            
         MVC   MYP.PTITLE1(15),=C'-SAME AS ABOVE-'                              
         LA    R5,132(R5)                                                       
*                                                                               
PRT26    CLI   E.GETCMCTR,C' '                                                  
         BNH   PRT30                                                            
         MVC   MYP.POTHER(12),E.GETCMCTR                                        
*                                                                               
         LR    R0,R3               PRESERVE TABLE POINTER                       
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'P',E.GETCMCTR),DUB                               
*NOP     MVC   DUB,E.GETCMCTR                                                   
         LA    R3,DUB                                                           
         BAS   RE,GETCMML          GET CENTERCUT CMML                           
         LR    R3,R0               RESTORE TABLE POINTER                        
*                                                                               
         MVC   MYP.PTYPE(4),=C'CNTR'                                            
         MVC   MYP.PTITLE1,E.GETCMT1                                            
         MVC   MYP.PTITLE2,E.GETCMT2                                            
         MVC   MYP.PTITLE3,E.GETCMT3                                            
         LA    R5,132(R5)          NEXT PRINT LINE                              
*                                                                               
PRT30    MVI   0(R5),0             SKIP A LINE                                  
         LA    R5,132(R5)                                                       
         BAS   RE,SETTAB           ADD CMML TO SUMMARY TABLE                    
*                                                                               
         LA    R3,8(R3)                                                         
         OC    0(8,R3),0(R3)       TEST FOR PIGGYBACK                           
         BNZ   *+12                                                             
         SHI   R5,132                                                           
         B     PRT40                                                            
*                                                                               
*==================================================                             
* PIGGYBACK                                                                     
*==================================================                             
*                                                                               
         BAS   RE,GETCMML          NOW GET PIGGYBACK                            
*                                                                               
         MVC   WORK+21(8),0(R3)    MOVE SECOND CMML                             
         CLI   E.GETCMADI,C' '     TEST FOR ADID                                
         BNH   *+10                 NO                                          
         MVC   WORK+21(12),E.GETCMADI                                           
*                                                                               
PRT32    GOTO1 SQUASHER,DMCB,WORK,41                                            
*                                                                               
         L     RE,AIO1             POINT TO FIRST PRINT LINE                    
         AHI   RE,4000                                                          
         LA    R5,132(RE)          POINT R5 TO SECOND LINE                      
         MVI   PCOMML-P(RE),C'('                                                
         LA    R1,WORK                                                          
         LA    RE,PCOMML-P+1(RE)                                                
*                                                                               
PRT34    CLI   0(R1),C' '          MOVE CMML TO PRINT LINE                      
         BNH   PRT36                                                            
         MVC   0(1,RE),0(R1)       1 CHAR AT A TIME                             
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         B     PRT34                                                            
*                                                                               
PRT36    MVI   0(RE),C'-'                                                       
*                                                                               
         MVC   MYP.PCOMML,1(R1)    MOVE SECOND CMML                             
         LA    RE,MYP.PCOMML+13                                                 
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVI   1(RE),C')'                                                       
*                                                                               
         MVC   MYP.PTYPE,E.GETCMTYP                                             
         MVC   MYP.PTITLE1,E.GETCMT1                                            
         MVC   MYP.PTITLE2,E.GETCMT2                                            
         MVC   MYP.PTITLE3,E.GETCMT3                                            
         LA    R5,132(R5)                                                       
*                                                                               
         CLI   E.GETCMCLT,C' '     TEST HAVE CLT CMML                           
         BNH   PRT38               NO                                           
         MVC   MYP.PTITLE1(6),=C'CLT# ='                                        
         MVC   MYP.PTITLE1+7(20),E.GETCMCLT                                     
*                                                                               
PRT38    CLC   MYP.POTHER(12),SPACES                                            
         BE    *+8                                                              
         LA    R5,132(R5)                                                       
         CLI   E.GETCMHDF,C' '                                                  
         BNH   PRT39X                                                           
         MVC   MYP.POTHER(12),E.GETCMHDF                                        
         MVC   MYP.PTYPE(4),=C'HDEF'                                            
         MVC   MYP.PTITLE1(15),=C'-SAME AS ABOVE-'                              
         LA    R5,132(R5)                                                       
*                                                                               
PRT39    CLI   E.GETCMCTR,C' '                                                  
         BNH   PRT40                                                            
         MVC   MYP.POTHER(12),E.GETCMCTR                                        
*                                                                               
         LR    R0,R3               PRESERVE TABLE POINTER                       
         GOTO1 VTRPACK,DMCB,(C'P',E.GETCMCTR),DUB                               
*NOP     MVC   DUB,E.GETCMCTR                                                   
         LA    R3,DUB                                                           
         BAS   RE,GETCMML          GET CENTERCUT CMML                           
         LR    R3,R0               RESTORE TABLE POINTER                        
*                                                                               
         MVC   MYP.PTYPE,=C'CNTR'                                               
         MVC   MYP.PTITLE1,E.GETCMT1 TITLE                                      
         MVC   MYP.PTITLE2,E.GETCMT2                                            
         MVC   MYP.PTITLE3,E.GETCMT3                                            
         LA    R5,132(R5)                                                       
*                                                                               
PRT39X   MVI   0(R5),0             SKIP A LINE                                  
         CLC   0(132,R5),SPACES                                                 
         BNH   *+8                                                              
         LA    R5,132(R5)                                                       
*                                                                               
PRT40    L     R1,FIRSTLIN         CALC LINES USED                              
         SR    R1,R5                                                            
         LPR   R1,R1                                                            
         SR    R0,R0                                                            
         D     R0,=F'132'                                                       
*>>>>>>  BCTR  R1,0                ADJUST FOR LAST LINE (ALWAYS BLANK)          
         STC   R1,ALLOWLIN         MAKE SURE THEY PRINT ON 1 PAGE               
*                                                                               
         LR    R5,R1               SAVE NUMBER OF LINES                         
         L     R6,FIRSTLIN         POINT TO FIRST PRINT LINE                    
*                                                                               
PRT42    LA    R0,4                                                             
         LA    R1,P                                                             
*                                                                               
         CR    R5,R0               MORE THAN 4 LINES LEFT                       
         BH    PRT43               YES                                          
         LR    R0,R5               ELSE PRINT REMAINING                         
*                                                                               
PRT43    MVC   0(132,R1),0(R6)                                                  
         LA    R6,132(R6)                                                       
         LA    R1,132(R1)                                                       
         BCT   R0,PRT43                                                         
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         AHI   R5,-4                                                            
         BP    PRT42                                                            
         MVI   P,0                 SKIP A LINE                                  
         GOTO1 (RF),(R1),(R8)                                                   
*                                                                               
PRT44    LA    R3,12(R3)                                                        
         AHI   R4,-L'CMLCMMLS                                                   
         BP    PRT14                                                            
         DROP  MYP                                                              
         EJECT                                                                  
         MVC   P(L'SHIPTEXT),SHIPTEXT                                           
         MVI   ALLOWLIN,5                                                       
*                                                                               
         OC    SVSTAFF,SVSTAFF     IS THERE AN AFFILIATE CODE                   
         BZ    PRT46                NO                                          
         CLI   SVSTAFF,0           IS IT REALLY AMS GROUP CODE                  
         BNE   PRT46                NO                                          
         MVC   P+51(21),=C'THIS AMS GROUP CODE: '                               
         LLC   RE,SVSTAFF+1                                                     
         BCTR  RE,0                                                             
         MH    RE,=AL2(L'STAAMSGC)                                              
         LA    RE,AMSGCTAB(RE)                                                  
         MVC   P+72(L'STAAMSGC),0(RE)                                           
*                                                                               
PRT46    GOTO1 SPOOL,DMCB,(R8)      PRINT THE TEXT                              
*                                                                               
         MVI   P,0                                                              
         GOTO1 (RF),(R1),(R8)      SKIP A LINE                                  
*                                                                               
         MVI   FORCEMID,C'Y'                                                    
         MVI   RCSUBPRG,2          SET NEW MIDLINES                             
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
*                                                                               
* SET NUMBER OF STATIONS TO PRINT THIS PAGE                                     
*                                                                               
         LLC   R0,MAXLINES                                                      
         LLC   RE,LINE                                                          
         SR    R0,RE               GIVES REMAINING LINES THIS PAGE              
         AHI   R0,-3               ALLOW FOR MIDLINES                           
         STH   R0,STALINES         SET NUMBER OF STA LINES THIS PAGE            
         MVC   SVACML,STLACML                                                   
*                                                                               
* FORMAT ALL STATIONS WITH THE SAME CMML LIST *                                 
*                                                                               
PRT50    DS    0H                                                               
         L     R0,AIO3             CLEAR PAGE BUILD AREA                        
         L     R1,SIZEIO                                                        
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R4,AIO3             SET TO BUILD A PAGE                          
         LH    R0,STALINES                                                      
         AR    R0,R0               X 2 GIVES NUM STATIONS THIS PAGE             
*                                                                               
PRT60    MVC   0(L'STLDATA,R4),STLDATA   MOVE DATA TO PAGE BUFFER               
*                                                                               
         MVC   DUB(3),STLSTA                                                    
         CLI   DUB,X'E8'           TEST CABLE                                   
         BL    *+10                NO                                           
         NC    DUB(3),=X'FFFF80'   DROP NETWORK                                 
*                                                                               
         BAS   RE,ADDTAB           ADD TO COUNTERS                              
*                                                                               
PRT62    LA    R2,STLNEXT          NEXT LIST ENTRY                              
*                                                                               
         CLC   STLACML,SVACML      TEST SAME LIST NEXT STATION                  
         BNE   PRT70                                                            
*                                                                               
         MVC   DUB+3(3),STLSTA     MOVE NEW STATION                             
         CLI   DUB+3,X'E8'         TEST CABLE                                   
         BL    PRT64                                                            
         NC    DUB+3(3),=X'FFFF80' DROP NETWORK                                 
         CLC   DUB(3),DUB+3        TEST SAME HEADEND                            
         BNE   PRT64               NO - CONTINUE                                
*                                                                               
         CLC   STLFTD-STLDATA(3,R4),STLFTD  SAVE EARLIEST FTD                   
         BNH   *+10                                                             
         MVC   STLFTD-STLDATA(3,R4),STLFTD                                      
         B     PRT62                                                            
*                                                                               
PRT64    LA    R4,L'STLDATA(R4)    NEXT OUTPUT POSITION                         
         BCT   R0,PRT60                                                         
*                                                                               
* BUFFER IS BUILT - NOW PRINT STATIONS IN TWO VERTICAL COLUMNS *                
*                                                                               
PRT70    L     R4,AIO3                                                          
         LH    R0,STALINES                                                      
*                                                                               
PRT72    OC    0(L'STLDATA,R4),0(R4)  TEST MORE DATA                            
         BZ    PRT76                                                            
*                                                                               
         MVC   WORK(L'STLDATA),0(R4)                                            
         MVI   MKTSW,C'L'          INDICATE LEFT HAND COLUMN                    
         BAS   RE,FMTLIN                                                        
*                                                                               
         LH    RE,STALINES         GET NUM STA LINES THIS PAGE                  
         LA    RF,L'STLDATA        GET DATA LENGTH FOR EACH STA                 
         MR    RE,RE               GIVES DSPL TO THIS ENTRY                     
         AR    RF,R4               POINT TO ENTRY                               
         OC    0(L'STLDATA,RF),0(RF) TEST DATA                                  
         BZ    PRT74                                                            
         MVC   WORK(L'STLDATA),0(RF) MOVE DATA                                  
         MVI   MKTSW,C'R'          INDICATE RIGHT HAND COLUMN                   
         BAS   RE,FMTLIN                                                        
*                                                                               
PRT74    DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R4,L'STLDATA(R4)    POINT TO NEXT STA                            
         BCT   R0,PRT72                                                         
*                                                                               
         LLC   RE,MAXLINES         GET MAX LINES PER PAGE                       
         AHI   RE,-5               ADJUST FOR MIDLINES + SPACES                 
*                                                                               
* ADJUST FOR NUMBER OF HEADLINE LINES                                           
*                                                                               
         LA    R0,14                                                            
         LA    R1,H14                                                           
         CLC   0(132,R1),SPACES                                                 
         BNE   *+12                                                             
         AHI   R1,-(L'HEAD1)                                                    
         BCT   R0,*-14                                                          
         SR    RE,R0               ADJUST FOR HEADLINES                         
         STH   RE,STALINES                                                      
*                                                                               
PRT76    CLC   STLACML,SVACML      TEST SAME LIST NEXT STATION                  
         BE    PRT50                YES - CONTINUE                              
*                                                                               
         OC    STLACML,STLACML     TEST REACHED EOL                             
         BZ    PRT80                                                            
*                                                                               
*SM                                                                             
         ICM   R1,3,SUBCNTR        INCREMENT TSAR SUB-COUNTER                   
         LA    R1,1(R1)                                                         
         STCM  R1,3,SUBCNTR                                                     
*SM                                                                             
*                                                                               
* SET UP TO PRINT NEXT LIST *                                                   
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1          RESET MIDLINE TYPE                           
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         B     PRT12                                                            
*                                                                               
* ALL DATA PROCESSED - TEST TO CONTINUE *                                       
*                                                                               
PRT80    OC    SVKEY,SVKEY         TEST TO CONTINUE                             
         BZ    PRT100              NO- DONE                                     
         MVC   KEY,SVKEY                                                        
         B     GEN140                                                           
         EJECT                                                                  
*==========================================================                     
* PRINT SHIPPING SUMMARY                                                        
*==========================================================                     
                                                                                
PRT100   MVI   FORCEHED,C'Y'                                                    
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         MVI   RCSUBPRG,3                                                       
*                                                                               
         LA    R0,14                                                            
         LA    R1,H1                                                            
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         USING CTABD,R3                                                         
         L     R3,ASHPSUM          POINT TO COMMERCIAL SUMMARY LIST             
         CLI   0(R3),0                                                          
         BE    NODATA                                                           
*                                                                               
         SR    R0,R0                                                            
         LR    R1,R3                                                            
*                                                                               
         CLI   0(R1),0                                                          
         BE    *+12                                                             
         LA    R1,CTABNEXT-CTABENT(R1)                                          
         BCT   R0,*-12                                                          
         LPR   R0,R0                                                            
         GOTO1 XSORT,DMCB,(R3),(R0),L'CTABENT,L'CTABENT,0                       
         XC    LSTCMLCT,LSTCMLCT                                                
*                                                                               
PRT102   MVC   LSTCML,CTABCMLS                                                  
         MVC   LSTCMLCT,CTABTOT    SAVE TOTAL                                   
*                                                                               
         MVI   PSUMMARY,1                                                       
         BAS   RE,GETCMML                                                       
         MVI   PSUMMARY,0                                                       
*                                                                               
         MVC   WORK,SPACES                                                      
         MVC   WORK(8),CTABCMLS     COMMERCIAL CODE                             
*                                                                               
         CLI   E.GETCMADI,C' '     TEST FOR ADID                                
         BNH   *+10                                                             
         MVC   WORK(12),E.GETCMADI                                              
         B     PRT104                                                           
*                                                                               
PRT104   MVC   PCOMML,WORK         ASSUME ONLY 1 COMMERCIAL                     
         MVC   POTHER,E.GETCMHDF                                                
*                                                                               
PRT106   MVC   PTITLE1,E.GETCMT1    CMML TITLE                                  
         MVC   PTITLE2,E.GETCMT2                                                
         MVC   PTITLE3,E.GETCMT3                                                
         MVC   PTYPE,16(R3)        COMMERCIAL TYPE                              
*                                                                               
         CLI   E.GETCMCLT,C' '                                                  
         BNH   PRT111                                                           
         MVC   PTITLE1+132(6),=C'CLT# ='                                        
         MVC   PTITLE1+132+7(20),E.GETCMCLT                                     
*                                                                               
PRT111   LA    R3,8(R3)                                                         
         CLI   0(R3),0             TEST PIGGYBACK                               
         BE    PRT120              NO                                           
*                                                                               
         MVI   PSUMMARY,1                                                       
         BAS   RE,GETCMML                                                       
         MVI   PSUMMARY,0                                                       
*                                                                               
         MVC   WORK+21(8),0(R3)                                                 
*                                                                               
         CLI   E.GETCMADI,C' '                                                  
         BNH   *+10                                                             
         MVC   WORK+21(12),E.GETCMADI                                           
*                                                                               
PRT112   GOTO1 SQUASHER,DMCB,WORK,41                                            
*                                                                               
         MVI   PCOMML,C'('                                                      
         LA    R1,WORK                                                          
         LA    RE,PCOMML+1                                                      
*                                                                               
PRT116   CLI   0(R1),C' '          TEST END OF FIRST CMML                       
         BNH   PRT118                                                           
         MVC   0(1,RE),0(R1)       MOVE CHAR TO PRINT LINE                      
         LA    R1,1(R1)                                                         
         LA    RE,1(RE)                                                         
         B     PRT116                                                           
*                                                                               
PRT118   MVI   0(RE),C'-'                                                       
         MVC   PCOMML+133(20),1(R1)  MOVE SECOND CMML TO LINE 2                 
*                                                                               
         LA    RE,PCOMML+154                                                    
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVI   1(RE),C')'                                                       
*                                                                               
         CLC   E.GETCMHDF,SPACES                                                
         BE    PRT118X                                                          
         OC    E.GETCMHDF,E.GETCMHDF                                            
         BZ    PRT118X                                                          
*                                                                               
         LA    R1,POTHER                                                        
         CLC   POTHER,SPACES                                                    
         BNH   PRT118X                                                          
*                                                                               
PRT118C  CLI   0(R1),C' '          TEST END OF FIRST CMML                       
         BNH   *+12                                                             
         LA    R1,1(R1)                                                         
         B     PRT118C                                                          
*                                                                               
         MVI   0(R1),C'-'                                                       
*                                                                               
PRT118X  MVC   POTHER+132,E.GETCMHDF                                            
*                                                                               
         MVC   PTITLE1+132,E.GETCMT1 CMML TITLE                                 
         MVC   PTITLE2+132,E.GETCMT2                                            
         MVC   PTITLE3+132,E.GETCMT3                                            
         CLI   E.GETCMCLT,C' '                                                  
         BNH   PRT120                                                           
         MVC   PTITLE1+132(6),=C'CLT# ='                                        
         MVC   PTITLE1+132+7(20),E.GETCMCLT                                     
*                                                                               
PRT120   OC    LSTCMLCT,LSTCMLCT                                                
         BZ    PRT122                                                           
         CLI   SVTSPR05,C'Y'       SUPPRESS COMMERCIAL COUNTS                   
         BE    PRT122                                                           
*                                                                               
         LH    R0,LSTCMLCT         PRINT NUMBER TO SHIP                         
         EDIT  (R0),(4,PCOMMLCT)                                                
*                                                                               
PRT122   MVI   SPACING,2                                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         LA    R3,L'CTABENT-8(R3)   POINT TO NEXT ENTRY                         
*                                                                               
PRT126   CLI   0(R3),0                                                          
         BNE   PRT102                                                           
*                                                                               
         OI    SVOPTSW,OPTDID      SET ON DID RUN REPORT                        
*                                                                               
         TM    SVOPTSW,OPTSIZE     ASKED FOR TABLE SIZE DISPLAY                 
         BO    TABERR               YES                                         
         CLI   TABLSIZE,0          ANY TABLES EXCEEDED                          
         BE    PRT130               NO                                          
         B     TABERR               NO                                          
*                                                                               
PRT130   L     R3,ATWA              I MUST RELOAD R3                            
         USING T216FFD,R3                                                       
*                                                                               
         CLC   TRAPRD(3),=C'ALL'    SEE IF DOING ALL PRODUCT REQ                
         BNE   PRT220                                                           
         BAS   RE,NEXTPRD                                                       
         BE    GEN110               GO DO REPORT FOR NEXT PRODUCT               
*                                                                               
PRT220   CLI   SVTWPR05,C'2'       SEE IF PRODUCING 2ND REPORT                  
         BNE   PRT270                                                           
*                                                                               
         TM    WHEN,X'20'          IF SOON, WITH AGY COPY                       
         BZ    PRT210                                                           
         BRAS  RE,AREQ                                                          
         B     PRT270                                                           
*                                                                               
PRT210   MVI   SPMODE,X'FF'        CLOSE PRINT QUEUE ENTRY                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVC   TRAPD(6),=C'CPYID='                                              
         MVC   TRAPD+6(3),REMUSER                                               
*                                                                               
         MVI   TRAPD+9,C','                                                     
         SR    R0,R0                                                            
         ICM   R0,3,SPOOLRPN                                                    
         EDIT  (R0),(4,TRAPD+10),ALIGN=LEFT                                     
         OI    TRAPDH+6,X'80'                                                   
*                                                                               
         MVI   SVTWPR05,C'Y'         SET FOR FAX REPORT                         
         OI    SVOPTSW,OPTCPY2       SO I WON'T REMARK FILE                     
         MVI   PQSW,1                SO I WILL OPEN THE PRTQUE                  
*                                    IN PRT03C (FAX RUN)                        
         L     R3,ATWA                                                          
         CLC   TRAPRD(3),=C'ALL'                                                
*        BNE   GEN100                                                           
         BNE   PRT03C                                                           
         MVI   BPRD,0               RESET FOR FIST PRODUCT                      
         B     PRT03C                                                           
*        B     GEN100                REDO WHOLE REPORT                          
         DROP  R3                                                               
*                                                                               
*SM  **ESHIP                                                                    
*                                                                               
PRT270   OC    SVOPTICA,SVOPTICA                                                
         B     PRT300      <<<<<< *NOP XML FOR OPTICA                           
         BZ    PRT300                                                           
*                                                                               
         GOTO1 AMKFILE,DMCB        CREATE FLAT FILE                             
*                                                                               
         GOTO1 AMKXML,DMCB,SVADGEND,ATWA,QUESNAME   CREATE XML FILE             
*                                                                               
* NOW UNLOCK USING LOCKET *                                                     
*                                                                               
PRT300   CLI  OFFLINE,C'Y'         UNLESS OFFLINE                               
         JNE  EXIT                  DON'T BOTHER                                
*                                                                               
         TM    WHEN,X'20'           ONLY FOR SOON                               
         JZ    EXIT                  DON'T BOTHER                               
*                                                                               
*        SKIP VALILOC CALL WHEN IN TEST MODE                                    
         TM    SVOPTSW,OPTTEST     TEST ON                                      
         JO    EXIT                                                             
*                                                                               
         MVC   DUB,=X'E4010A2500000000' U=UNLOCK, 01=1 ENTRY, 0A25 RECS         
         GOTO1 VALILOC,0                                                        
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
TABERR   LA    R0,14                                                            
         LA    R1,H1                                                            
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(,R1)                                                      
         BCT   R0,*-10                                                          
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
         XC    HEADHOOK,HEADHOOK                                                
         XC    SPECS,SPECS                                                      
         CLI   TABLSIZE,0          ANY TABLES EXCEEDED                          
         BNE   TABERR12             YES                                         
         TM    SVOPTSW,OPTSIZE     ASKED FOR TABLE SIZE DISPLAY                 
         BO    TABERR10             YES                                         
         DC    H'0'                                                             
TABERR10 CLI   OFFLINE,C'Y'                                                     
         BNE   TABERR60                                                         
         CLI   TABLSIZE,0          ANY TABLES EXCEEDED                          
         BE    TABERR40             NO                                          
TABERR12 MVC   H1(35),=CL35'PROGRAM ERROR - TABLE SIZE EXCEEDED'                
         LA    R1,P2                                                            
         TM    TABLSIZE,STASIZER   STATION TABLE EXCEEDED                       
         BZ    TABERR20              NO                                         
         MVC   0(13,R1),=CL13'STATION TABLE'                                    
         MVC   14(14,R1),=CL14'SIZE TOO SMALL'                                  
         LA    R1,132(,R1)                                                      
TABERR20 TM    TABLSIZE,CMLSIZER   CML LISTS TABLE EXCEEDED                     
         BZ    TABERR30             NO                                          
         MVC   0(10,R1),=CL10'CML LISTS'                                        
         MVC   11(14,R1),=CL14'SIZE TOO SMALL'                                  
         LA    R1,132(,R1)                                                      
TABERR30 TM    TABLSIZE,SUMSIZER   CML SHIPPING SUMMARY TABLE EXCEEDED          
         BZ    TABERR40             NO                                          
         MVC   0(13,R1),=CL13'CML SHIP SUMM'                                    
         MVC   14(14,R1),=CL14'SIZE TOO SMALL'                                  
TABERR40 MVI   FORCEHED,C'Y'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
TABERR50 LA    R2,P                                                             
         B     TABERR70                                                         
TABERR60 L     R3,ATWA                                                          
         USING T216FFD,R3                                                       
         LA    R2,TRATBL1                                                       
         OI    TRATBL1H+6,X'80'                                                 
         OI    TRATBL2H+6,X'80'                                                 
         OI    TRATBL3H+6,X'80'                                                 
TABERR70 MVC   0(7,R2),=CL7'STATION'                                            
         MVC   8(36,R2),=CL36'TABLE =         BYTES,         USED'              
         LM    R4,R5,ASTLIST       FIND STATION LIST SIZE AND USED              
         LA    R6,L'STLDATA                                                     
         BAS   RE,TABSIZ                                                        
         LA    R2,132(,R2)                                                      
         CLI   TABLSIZE,0          ANY TABLES EXCEEDED                          
         BNE   TABERR72             YES                                         
         CLI   OFFLINE,C'Y'                                                     
         BE    *+8                                                              
         LA    R2,TRATBL2                                                       
*                                                                               
TABERR72 MVC   0(7,R2),=CL7'CML LST'                                            
         MVC   8(36,R2),=CL36'TABLE =         BYTES,         USED'              
         LM    R4,R5,ACMLST        FIND CML LISTS SIZE AND USED                 
         LA    R6,18                                                            
         BAS   RE,TABSIZ                                                        
*                                                                               
         LA    R2,132(,R2)                                                      
         CLI   TABLSIZE,0          ANY TABLES EXCEEDED                          
         BNE   TABERR74             YES                                         
         CLI   OFFLINE,C'Y'                                                     
         BE    *+8                                                              
         LA    R2,TRATBL3                                                       
*                                                                               
TABERR74 MVC   0(7,R2),=CL7'SHP SUM'                                            
         MVC   8(36,R2),=CL36'TABLE =         BYTES,         USED'              
         LM    R4,R5,ASHPSUM       FIND SHIPPING SUMMARY SIZE/USED              
         LA    R6,CTABNEXT-CTABENT                                              
         BAS   RE,TABSIZ                                                        
         CLI   TABLSIZE,0          ANY TABLES EXCEEDED                          
         BNE   TABERR76             YES                                         
         TM    SVOPTSW,OPTSIZE     ASKED FOR TABLE SIZE DISPLAY                 
         JZ    EXIT                 NO                                          
TABERR76 GOTO1 SPOOL,DMCB,(R8)                                                  
         TM    SVOPTSW,OPTTEST     TEST ON                                      
         BO    *+6                                                              
         DC    H'0'                                                             
         GOTO1 ERREX2                                                           
TABSIZ   NTR1                                                                   
         LR    R3,R5                                                            
         SR    R3,R4               FIND TABLE SIZE                              
         EDIT  (R3),(7,16(R2)),COMMAS=YES                                       
         LR    R3,R4               SAVE TABLE START                             
TABSIZ10 EX    R6,TABSIZ30         FIND USED TABLE                              
         BZ    TABSIZ20            END OF USED                                  
         AR    R4,R6                                                            
         CR    R4,R5                                                            
         BL    TABSIZ10                                                         
TABSIZ20 SR    R4,R3                                                            
         EDIT  (R4),(7,31(R2)),COMMAS=YES                                       
         J     EXIT                                                             
TABSIZ30 OC    0(0,R4),0(R4)                                                    
*                                                                               
TABSIZER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TABSIZMS),TABSIZMS                                     
         LA    R2,TRAMEDH                                                       
         OI    CONHEADH+6,X'80'                                                 
*                                                                               
         TM    SVOPTSW,OPTTEST     OPT TEST                                     
         BO    TABERR               YES, TELL WORLD                             
         DC    H'0',C'$ABEND'      MUST BOMB TO 'RESTORE' RECS WRITTEN          
*                                                                               
SHIPTEXT DC    C'  SHIP ALL OF THE COMMERCIALS IN THE ABOVE LIST TO THEX        
                FOLLOWING STATIONS'                                             
*                                                                               
*                                                                               
*=============================================================                  
* GET ENVIRONMENT                                                               
*=============================================================                  
*                                                                               
GENVRNMT NTR1                                                                   
*                                                                               
         L     R3,ATWA                                                          
*                                                                               
         L     RF,TWAMASTC                                                      
         L     RF,MCSSB-MASTD(RF)                                               
         MVC   ENVIRNMT(1),SSODSPAC-SSOOFF(RF)  DSPACE                          
*                                                                               
         LA    RF,DENVTBL          DEFAULT ENVIRONMENT TABLE                    
         USING ENVD,RF                                                          
*                                                                               
         LA    R0,DENVTBLN                                                      
*                                                                               
ENV04    CLC   ENVDSP,ENVIRNMT     MATCH ON ENVIRONMENT ?                       
         BNE   ENV06                                                            
*                                                                               
ENV08    MVC   ENVIRNMT,ENVXML     XML ENVIRONMENT                              
         MVC   QUESNAME,ENVQUE     QUEUE SUB-NAME                               
         B     ENVX                                                             
*                                                                               
ENV06    LA    RF,ENVLEN(RF)                                                    
         BCT   R0,ENV04                                                         
         DC    H'0'                BUG CATCHER                                  
*                                                                               
ENVX      XIT1                                                                  
*                                                                               
         DROP  RF                                                               
*                                                                               
*                                                                               
* DEFAULT ENVIRONMENT TABLE                                                     
*                                                                               
*                     DSPACE XML  QUEUE NAME                                    
DENVTBL  DC    C'   ',C'P',C'ADV',C'SEND'   ADV                                 
         DC    C'   ',C'A',C'ADV',C'SEND'                                       
         DC    C'   ',C'T',C'TST',C'SEND'   TST/DEV                             
         DC    C'   ',C'C',C'CSC',C'SEND'   CSC/YDY                             
DENVTBLN EQU   (*-DENVTBL)/ENVLEN                                               
*                                                                               
*                                                                               
*=============================================================                  
* GET USER EMAIL ADDRESS  IF ANY                                                
*=============================================================                  
*                                                                               
GUEMAIL  NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING SAPEREC,R4                                                       
         MVI   SAPETYP,SAPETYPQ                                                 
         MVI   SAPESUB,SAPESUBQ                                                 
         MVC   SAPEAGY,AGENCY                                                   
         MVC   SAPEPID,SVSECPID                                                 
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'CTFILE',KEY,AIO3,0                    
         L     R4,AIO3                                                          
         CLC   KEY(23),0(R4)                                                    
         BNE   GUEMX                                                            
*                                                                               
         LA    R5,SAPEDATA                                                      
GUEM020  CLI   0(R5),0                                                          
         BE    GUEMX                                                            
         CLI   0(R5),X'E5'                                                      
         BE    GUEM040                                                          
         ZIC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     GUEM020                                                          
*                                                                               
GUEM040  DS    0H                                                               
         USING SAPEED,R5                                                        
         ZIC   R6,1(R5)            ELEM LEN                                     
         SHI   R6,3                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   SVEMAIL(0),SAPEEID                                               
*                                                                               
GUEMX    J     EXIT                                                             
         DROP  R4,R5                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*=============================================================                  
* GET COMMERCIAL RECORD AND FORMAT DATA IN ELEM                                 
*=============================================================                  
GETCMML  NTR1                                                                   
         XC    ELEM,ELEM                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0AC1'     TRY ADID POINTER                             
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(8),0(R3)      COMMERCIAL                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    GETCML10                                                         
         MVC   KEY,KEYSAVE         RESTORE                                      
         MVC   KEY(2),=X'0A21'                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GETCML10 MVC   AIO,AIO1                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
*                                                                               
         MVC   E.GETCMT1,CMLTITLE                                               
         MVC   E.GETCMOV1(2),CMLOVRD1                                           
         CLI   SVTSPR04,C'Y'       SUPPRESS CML TYPE (NOTYP)                    
         BE    *+10                                                             
         MVC   E.GETCMTYP,CMLTYPE                                               
*                                                                               
         CLI   SVT1PROF+6,C'B'     PRINT CLT CMML NO                            
         BE    *+12                                                             
         CLI   SVT1PROF+6,C'Y'                                                  
         BNE   *+10                                                             
         MVC   E.GETCMCLT,CMLCLTNO                                              
*                                                                               
*SM                                                                             
         OC    SVOPTICA,SVOPTICA   OPTICA                                       
         BZ    GETCM19                                                          
*                                                                               
         MVC   SVLEN,CMLSLN          LEN                                        
         MVC   SVCMLSTR,CMLRLSE    REL DATE                                     
         MVC   SVCMLEND,SPACES                                                  
         CLC   CMLRCL,=C'UFN'                                                   
         BE    GETCM18                                                          
         MVC   SVCMLEND,CMLRCL     RECALL DATE                                  
*                                                                               
GETCM18  MVI   ELCODE,X'20'                                                     
         BRAS  RE,NEXTEL           PRODUCT ELEM                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    SVCMLPRD,SVCMLPRD                                                
*                                                                               
         CLI   1(R6),3             ELEM LEN                                     
         BNE   GETCM18X                                                         
         MVC   SVCMLPRD(1),2(R6)   YES, SAVE 1 PROD                             
         B     GETCM19                                                          
*                                                                               
GETCM18X LLC   R1,1(R6)            GET ELEM LEN                                 
         SHI   R1,3                MINUS ELEM ID/ELEM LEN - 1                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVCMLPRD(0),2(R6)   SAVE PRODS                                   
*SM                                                                             
GETCM19  MVI   ELCODE,X'30'                                                     
         BRAS  RE,NEXTEL           ANY EXTRA LINES OF DESC                      
         BNE   GETCM20                                                          
         MVC   E.GETCMT2,3(R6)                                                  
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   GETCM20                                                          
         MVC   E.GETCMT3,3(R6)                                                  
         DROP  R6                                                               
*                                                                               
GETCM20  DS    0H                                                               
         CLI   0(R3),C'A'                                                       
         BL    *+10                                                             
         MVC   E.GETCMADI(8),0(R3) PRESET TO 8 CHAR ISCII                       
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'A0'                                                     
         BRAS  RE,GETEL                                                         
         BNE   GETCM21                                                          
*                                                                               
         USING CMLADIEL,R6                                                      
         MVC   E.GETCMADI,CMLADID                                               
         B     GETCM22                                                          
*                                                                               
* FIX FOR CMMLS ADDED WITHOUT ADID ELEMENTS                                     
*                                                                               
GETCM21  CLI   KEY+1,X'C1'         TEST HAVE ADID POINTER                       
         BNE   GETCM22                                                          
         GOTO1 VTRPACK,DMCB,(C'U',KEY+5),E.GETCMADI                             
*                                                                               
GETCM22  MVI   ELCODE,X'24'        LOOK FOR EXTENDED DATA EL                    
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   GETCM24                                                          
*                                                                               
         USING CMLXDTEL,R6                                                      
         MVC   E.GETCMHDF,CMLXHDEF                                              
         MVC   E.GETCMCTR,CMLXCNTR                                              
         DROP  R6                                                               
*SM                                                                             
*                                                                               
GETCM24  OC    SVOPTICA,SVOPTICA   OPTICA                                       
         BZ    GETCMX               NO                                          
*                                                                               
         CLI   PSUMMARY,1          PRINTING SUMMARY                             
         BE    GETCMX               YES, DONE                                   
*                                                                               
         MVI   BYTE,0              INIT RECORD ADDED THIS TIME                  
         LA    R0,3                3 CMLS: ADID/HIDEF/CNTRCT                    
         LA    R4,E.GETCMADI       ADID                                         
         B     *+8                                                              
GETCM24B LA    R4,12(R4)                                                        
*                                                                               
         OC    0(12,R4),0(R4)      ANY CML                                      
         BNZ   GETCM24C                                                         
         BCT   R0,GETCM24B                                                      
         CLI   BYTE,1              CML ADDED THIS TIME                          
         BE    GETCM24D             YES                                         
         B     GETCMX                                                           
*                                                                               
GETCM24C LA    R6,ELEM+240                                                      
         USING XCOMMLD,R6                                                       
*                                                                               
         MVC   XCOMMID,SUBCNTR     COMML SUB-COUNTER ID                         
         MVC   XCOMML,0(R4)        CML                                          
*                                                                               
         SAM31                                                                  
         GOTO1 =V(BINSRCH),BINPAR2,(R6),,,(1,XCOMNXT),(0,XCOMNXT),     X        
               RR=SPTR46RR                                                      
         SAM24                                                                  
         L     R6,0(R1)                                                         
         LTR   R6,R6                                                            
         BNZ   *+6                                                              
         DC    H'0'            *TABLE FULL                                      
*                                                                               
         TM    0(R1),X'80'         WAS RECORD ADDED THIS TIME                   
         BZ    *+8                                                              
         MVI   BYTE,1              RECORD ADDED THIS TIME                       
         BCT   R0,GETCM24B                                                      
         CLI   BYTE,1               YES                                         
         BE    GETCM24D                                                         
         B     GETCMX                                                           
*                                                                               
* SEE IF NEED TO ADD THIS CML TO TSAR FOR FLAT FILE                             
GETCM24D DS    0H                                                               
         LA    R1,0(R1)            CLEAR HOB                                    
         SAM31                                                                  
         L     R1,ACOMMLT                                                       
D        USING XCOMMLD,R1                                                       
*                                                                               
GETC24D1 CLC   D.XCOMML,XCOMML    SAME CML                                      
         BNE   GETC24D2                                                         
         CLC   D.XCOMMID,XCOMMID  SAME ID                                       
         BNE   GETCMX             SAME CML,DIFFERNT ID, DO NOT ADD DUP          
*                                                                               
GETC24D2 LA    R1,XCOMNXT(R1)    NEXT ENTRY                                     
         CLI   D.XCOMML,0          EOT?                                         
         BNE   GETC24D1                                                         
         SAM24                                                                  
*                                                                               
         DROP  D,R6                                                             
*                                                                               
* ESHIP                                                                         
*     ADD SPOT/ADID RECORD - CML/TITLE 1-3/FORMAT/LEN                           
GETCM24F GOTO1 ABLDESHP,DMCB,('ESACML',E.GETCMADI),E.GETCMT1,          X        
               E.GETCMT2,E.GETCMT3,SVLEN                                        
*                                                                               
*                                ADD RELEASE, RECALL, PO, HIDEF,CNTRCT          
         GOTO1 ABLDESHP,DMCB,('ESACML',SVCMLSTR),SVCMLEND,SVPONUM,     X        
               E.GETCMHDF,E.GETCMCTR                                            
*                                                                               
* CHECK PRODUCTS IN COMMERCIAL RECORD                                           
         XC    WORK,WORK                                                        
         CLI   SVCMLPRD,X'FF'      PRD=ALL CML                                  
         BNE   *+14                                                             
         MVC   WORK(3),=C'ALL'      YES                                         
         B     GPRD10                                                           
         DROP  R3                                                               
*                                                                               
* SEND OVER ALL PRODUCTS                                                        
*                                                                               
         LA    R0,13               MAX 13 PRODS                                 
         LA    RF,SVCMLPRD         LIST OF PRODS                                
         LA    R3,WORK             SAVE PRODS HERE                              
*                                                                               
GPRD01   L     R1,ASVCLIST                                                      
GPRD02   CLC   0(1,RF),3(R1)       MATCH ON PRD                                 
         BE    GPRD03                                                           
         LA    R1,4(R1)            BUMP IN ASVCLIST                             
         CLI   0(R1),C' '                                                       
         BH    GPRD02                                                           
         DC    H'0'                                                             
*                                                                               
GPRD03   MVC   0(3,R3),0(R1)       SAVE PROD IN WORK                            
         LA    R3,3(R3)            BUMP IN WORK                                 
         LA    RF,1(RF)            BUMP IN SVCMLPRD                             
         CLI   0(RF),0             ANY MORE PRD?                                
         BE    GPRD10               NO, DONE                                    
         BCT   R0,GPRD01                                                        
*                                                                               
* GET PROD NAME                                                                 
GPRD10   LA    R3,WORK             3 CHAR PROD                                  
GPRD12   XC    TALID,TALID         TALENT AGENCY ID                             
         CLC   0(3,R3),=C'ALL'     PRD=ALL                                      
         BNE   *+14                                                             
         MVC   WORK+40(3),=C'ALL'  PROD NAME=ALL                                
         B     *+12                                                             
         LA    R4,WORK+40          SAVE PROD NAME HERE                          
         BAS   RE,GETPRDNM         GET PRODUCT NAME                             
*                                                                               
*ADD PROD REC TO BINSRCH'                                                       
         XC    ELEM+240(15),ELEM+240                                            
         LA    R6,ELEM+240                                                      
         USING XPRODD,R6                                                        
*                                                                               
         MVC   XPRODID,SUBCNTR     PROD SUB-COUNTER ID                          
         MVC   XPROD,0(R3)         PRODUCT                                      
*                                                                               
         SAM31                                                                  
         GOTO1 =V(BINSRCH),BINPAR1,(R6),,,(1,XPRDNXT),(2,L'XPROD),     X        
               RR=SPTR46RR                                                      
         SAM24                                                                  
         L     R6,0(R1)                                                         
         LTR   R6,R6                                                            
         BNZ   *+6                                                              
         DC    H'0'            *TABLE FULL                                      
*                                                                               
         TM    0(R1),X'80'         WAS RECORD ADDED THIS TIME                   
         BZ    GPRD20               NO                                          
*                                                                               
* ESHIP                                                                         
*     ADD PRODUCT (BRAND) RECORD       PRD   PRD NAME TALENT                    
         GOTO1 ABLDESHP,DMCB,('ESAPRD',0(R3)),0(R4),TALID                       
*                                                                               
GPRD20   LA    R1,E.GETCMADI                                                    
         BAS   RE,ACMPR            ADD CML/PROD REC TO BINSRCH                  
         BZ    GPRD22               RECORD ALREADY EXISTS                       
*                                                                               
*     ADD CML/PRD RECORD                   CML   PRD CODE                       
         GOTO1 ABLDESHP,DMCB,('ESACMP',E.GETCMADI),0(R3)                        
*                                                                               
GPRD22   OC    E.GETCMHDF,E.GETCMHDF   ANY HIDEF                                
         BZ    GPRD25                                                           
*                                                                               
         LA    R1,E.GETCMHDF                                                    
         BAS   RE,ACMPR            ADD CML/PROD REC TO BINSRCH                  
         BZ    GPRD25               RECORD ALREADY EXISTS                       
*                                                                               
         GOTO1 ABLDESHP,DMCB,('ESACMP',E.GETCMHDF),0(R3)                        
*                                                                               
GPRD25   OC    E.GETCMCTR,E.GETCMCTR   ANY CENTRCUT                             
         BZ    GPRD30                                                           
*                                                                               
         LA    R1,E.GETCMCTR                                                    
         BAS   RE,ACMPR            ADD CML/PROD REC TO BINSRCH                  
         BZ    GPRD30               RECORD ALREADY EXISTS                       
*                                                                               
         GOTO1 ABLDESHP,DMCB,('ESACMP',E.GETCMCTR),0(R3)                        
*                                                                               
GPRD30   CLC   0(3,R3),=C'ALL'     IF PROD=ALL                                  
         BE    GETCMX              THEN DONE                                    
*                                                                               
         LA    R3,3(R3)            BUMP IN WORK TO NEXT PRD                     
         CLI   0(R3),0             DONE?                                        
         BNE   GPRD12                                                           
*                                                                               
GETCMX   J     EXIT                                                             
*                                                                               
PSUMMARY DC    X'00'               PRINTING SUMMARY                             
*                                                                               
*============================================                                   
* ADD CML/PROD REC TO BINSRCH                                                   
* ON ENTRY R1= CML (ADID OR HIDEF)                                              
*============================================                                   
ACMPR    NTR1                                                                   
         XC    ELEM+240(15),ELEM+240                                            
         LA    R6,ELEM+240                                                      
         USING XCMLPRDD,R6                                                      
*                                                                               
         MVC   XCMPRID,SUBCNTR     CML/PROD SUB-COUNTER ID                      
         MVC   XCML,0(R1)          CML                                          
         MVC   XPRD,0(R3)          PROD CODE                                    
*                                                                               
         SAM31                                                                  
         GOTO1 =V(BINSRCH),BINPAR3,(R6),,,(1,XCPRNXT),(2,L'XCMLPRD),   X        
               RR=SPTR46RR                                                      
         SAM24                                                                  
         L     R6,0(R1)                                                         
         LTR   R6,R6                                                            
         BNZ   *+6                                                              
         DC    H'0'            *TABLE FULL                                      
*                                                                               
         TM    0(R1),X'80'         WAS RECORD ADDED THIS TIME                   
         J     EXIT                                                             
*                                                                               
* ADD COMMERCIAL TO SHIP SUM TABLE IF NOT ALREADY THERE *                       
*                                                                               
SETTAB   NTR1                                                                   
         L     R6,ASHPSUM                                                       
         USING CTABD,R6                                                         
*                                                                               
SETTAB2  CLI   0(R6),0             TEST EOT                                     
         BE    SETTAB4                                                          
         CLC   CTABCMLS(L'CTABSORT),0(R3)  TEST SAME CMMLS/TYPE                 
         JE    EXIT                                                             
         LA    R6,L'CTABENT(R6)                                                 
         B     SETTAB2                                                          
*                                                                               
SETTAB4  C     R6,ASHPSUMX         GET END OF TABLE ADDRESS                     
         BH    SETTAB10            SET TABLE EXCEEDED                           
*                                                                               
         MVC   CTABCMLS(L'CTABSORT),0(R3)  MOVE CMMLS & TYPE                    
         MVC   CTABCTR,E.GETCMCTR                                               
         MVC   CTABHDF,E.GETCMHDF                                               
         MVC   CTABCLT,E.GETCMCLT                                               
         LA    R0,1                                                             
         TM    ONECMML,X'81'       TEST TO SHIP TWO COPIES                      
         BNO   *+8                                                              
         LA    R0,2                                                             
         STH   R0,CTABNUM                                                       
         J     EXIT                                                             
SETTAB10 OI    TABLSIZE,SUMSIZER   SET SHIP SUMMARY TABLE OVERFLOW SW           
         CLI   OFFLINE,C'Y'                                                     
         JE    EXIT                                                             
         MVC   TABSIZMS+9(3),=C'SHP'                                            
         B     TABSIZER                                                         
         DROP  E                                                                
TABSIZMS DC    C'* NOTE * XXX TOO LARGE FOR ONLINE-RUN OV/SOON *'               
         EJECT                                                                  
* SUBROUTINE TO ADD TO FILM COUNTERS FOR SHIP SUMMARY *                         
*                                                                               
ADDTAB   NTR1                                                                   
*                                                                               
         L     R3,STLACML          GET CMML LIST ADDRESS                        
         SR    R4,R4                                                            
         ICM   R4,3,0(R3)          GET LENGTH                                   
*                                                                               
* ADJUST LENGTH TO REAL LENGTH                                                  
*                                                                               
         AHI   R4,-(CMLCMMLS-CMLLISTD)                                          
*                                                                               
* POINT TO FIRST COMMERCIAL                                                     
*                                                                               
         LA    R3,CMLCMMLS-CMLLISTD(,R3)                                        
*                                                                               
ADDTAB2  L     R6,ASHPSUM                                                       
*                                                                               
ADDTAB4  CLC   0(20,R6),0(R3)      MATCH FILM(S) AND TYPE                       
         BE    ADDTAB6                                                          
         LA    R6,L'CTABENT(R6)                                                 
         CLI   0(R6),0                                                          
         BNE   ADDTAB4                                                          
         CLI   TABLSIZE,0                                                       
         JNE   EXIT                MUST HAVE EXCEEDED TABLE SIZE                
         DC    H'0'                                                             
*                                                                               
ADDTAB6  LH    R0,CTABNUM          GET NUM PER STATION                          
         AH    R0,CTABTOT          ADD TO TOTAL                                 
         STH   R0,CTABTOT          SAVE                                         
*                                                                               
         LA    R3,L'CMLCMMLS(,R3)                                               
         AHI   R4,-(L'CMLCMMLS)                                                 
         BP    ADDTAB2                                                          
         J     EXIT                                                             
         EJECT                                                                  
*======================================================                         
* SUBROUTINE TO FORMAT STATION DATA TO PRINT LINE                               
*======================================================                         
*                                                                               
FMTLIN   NTR1                                                                   
         LA    R4,STLMKT-STLDATA+WORK   POINT TO MKT/STA                        
         ST    R4,DMCB                                                          
         CLI   2(R4),X'E8'         TEST CABLE                                   
         BL    *+10                                                             
         NC    2(3,R4),=X'FFFF80'  DROP NETWORK                                 
*                                                                               
         GOTO1 MSUNPK,DMCB,(X'80',STLMKT-STLDATA+WORK),WORK+32,DUB              
*                                                                               
         MVC   STAPRNT(4),DUB                                                   
         MVC   STAPRNT+4(3),SPACES                                              
         MVC   STAXML,STAPRNT                                                   
*                                                                               
         CLI   QMED,C'T'                                                        
         BE    FMTLIN04                                                         
*                                                                               
* FOR MEDIA R SEND TO OPTICA (WABCA FOR WABC-AM OR ANN A FOR ANN-AM)            
         MVC   STAXML,DUB                                                       
*                                                                               
FMTLIN04 CLI   2(R4),X'E8'         TEST CABLE                                   
         BNL   FMTLIN10                                                         
*                                                                               
         CLI   DUB+4,C' '                                                       
         BNE   *+8                                                              
         MVI   DUB+4,C'T'                                                       
*                                                                               
         LA    RE,STAPRNT+3                                                     
         CLI   0(RE),C' '                                                       
         BNE   *+6                                                              
         BCTR  RE,0                                                             
         MVI   1(RE),C'-'                                                       
         MVC   2(1,RE),DUB+4                                                    
         MVI   3(RE),C'V'                                                       
         CLI   QMED,C'T'                                                        
         BE    FMTLIN10                                                         
         MVI   3(RE),C'M'                                                       
         CLI   QMED,C'R'                                                        
         BE    FMTLIN10                                                         
         MVI   3(RE),C' '                                                       
*                                                                               
FMTLIN10 LA    R4,MKTL                                                          
         BE    *+8                                                              
         CLI   MKTSW,C'L'                                                       
         BE    *+8                                                              
         LA    R4,MKTR                                                          
*                                                                               
         CLC   0(4,R4),WORK+32    TEST SAME MARKET AS PREV                      
         BE    FMTLIN30                                                         
         MVC   0(4,R4),WORK+32     SAVE MARKET NUMBER                           
*                                                                               
         L     R6,AIO1             READ MARKET RECORD                           
         USING MKTRECD,R6                                                       
         MVI   0(R6),C'0'                                                       
         MVC   1(14,R6),0(R6)                                                   
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,QMED                                                     
         MVC   MKTKMKT,0(R4)                                                    
         MVC   MKTKAGY,AGENCY                                                   
*                                                                               
         MVC   KEY(17),0(R6)                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,(R6)                     
*                                                                               
         MVC   4(24,R4),=CL24'**** UNKNOWN ****'                                
         CLC   KEY(8),0(R6)                                                     
         BNE   FMTLIN30                                                         
*                                                                               
         MVC   4(24,R4),MKTNAME                                                 
         OC    4(24,R4),SPACES      NEEDED WHEN FAXING                          
*                                                                               
FMTLIN30 LA    R5,P+1                                                           
         CLI   MKTSW,C'L'          TEST LEFT HAND COL                           
         BE    *+8                                                              
         LA    R5,P+58                                                          
*                                                                               
         MVC   1(7,R5),STAPRNT                                                  
*                                                                               
         MVC   13(24,R5),4(R4)     MOVE MKT NAME                                
         LA    R4,STLFTD-STLDATA+WORK                                           
         GOTO1 DATCON,DMCB,(3,(R4)),(5,38(R5))                                  
*                                                                               
*SM                                                                             
         OC    SVOPTICA,SVOPTICA                                                
         JZ    EXIT                                                             
*SM                                                                             
         BAS   RE,AORIT            ADD ORDER ITEM RECORDS TO TSAR               
         J     EXIT                                                             
*                                                                               
         DROP  R6                                                               
*                                                                               
*===================================================                            
* ADD ORDER ITEM RECORD  (COMML, STATION, FTD)                                  
* ON ENTRY R4= FTD                                                              
*===================================================                            
*                                                                               
AORIT    NTR1                                                                   
         SAM31                                                                  
         L     R3,ACOMMLT                                                       
         USING XCOMMLD,R3                                                       
*                                                                               
AORT02   CLC   XCOMMID,SUBCNTR     SAME COUNTER ID                              
         BE    AORT04                                                           
*                                                                               
         LA    R3,XCOMNXT(R3)      NEXT ENTRY                                   
         CLI   XCOMML,0            EOT?                                         
         BNE   AORT02                                                           
         B     AORTX                                                            
*                                                                               
AORT04   SAM24                                                                  
         XC    ELEM+230(25),ELEM+230                                            
         LA    R6,ELEM+230                                                      
         USING XDESTD,R6                                                        
*                                                                               
         MVC   XDESTID,SUBCNTR     DEST SUB-COUNTER ID                          
*        MVC   XDEST,STAPRNT       DESTINATION (STATION)                        
         MVC   XDEST,STAXML        DESTINATION (STATION)                        
         SAM31                                                                  
         MVC   XDCML,XCOMML        CML                                          
*                                                                               
         GOTO1 =V(BINSRCH),BINPAR4,(R6),,,(1,XDESTNXT),(2,L'XDESCML),  X        
               RR=SPTR46RR                                                      
         SAM24                                                                  
         L     R6,0(R1)                                                         
         LTR   R6,R6                                                            
         BNZ   *+6                                                              
         DC    H'0'            *TABLE FULL                                      
*                                                                               
         TM    0(R1),X'80'         WAS RECORD ADDED THIS TIME                   
         BZ    AORT10               NO                                          
*                                                                               
         MVI   BYTE,C'R'           PRESET TO REGULAR SHIP                       
         TM    SVOPTSW,OPTRERUN    RERUN (FORCED SHIP)                          
         BZ    *+8                                                              
         MVI   BYTE,C'F'                                                        
*                                        CML  STATION   FTD                     
         SAM31                                                                  
         MVC   SVXCOMML,XCOMML                                                  
         SAM24                                                                  
         GOTO1 ABLDESHP,DMCB,('ESAOIT',SVXCOMML),STAXML,0(R4),BYTE              
*                                                                               
AORT10   SAM31                                                                  
         LA    R3,XCOMNXT(R3)      NEXT ENTRY                                   
         CLI   XCOMML,0            DONE?                                        
         BNE   AORT02                                                           
*                                                                               
AORTX    J     EXIT                                                             
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
         EJECT                                                                  
AUTOPOER L     R1,=A(AUTOPOMS)                                                  
         B     ERREXIT2                                                         
PRDREQER L     R1,=A(PRDREQMS)                                                  
         B     ERREXIT2                                                         
POLPRERR L     R1,=A(POLPRDMS)                                                  
         CLI   SVPROF2,C'Y'        PRODUCT REQUIRED                             
         BNE   ERREXIT2                                                         
         CLI   SVT1PR16,C'Y'       SPECIFIC PRODUCT REQUIRED                    
         BE    ERREXIT2                                                         
         L     R1,=A(PROPRDMS)                                                  
         B     ERREXIT2                                                         
NODATA   L     R3,ATWA             RESTORE TWA ADDRESS                          
         USING T216FFD,R3                                                       
         CLC   TRAPRD(3),=C'ALL'   SEE IF DOING ALL PRODUCTS                    
         BE    PRT220             TRY NEXT PRODUCT                              
*                                                                               
         TM    SVOPTSW,OPTDID      SET ON DID RUN REPORT                        
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   CONHEAD(23),=C'** NO DATA GENERATED **'                          
         LA    R2,TRAMEDH          POSITION CURSOR                              
*                                                                               
* NOW UNLOCK USING LOCKET *                                                     
*                                                                               
         CLI  OFFLINE,C'Y'         UNLESS OFFLINE                               
         BNE  NODTAERX              DON'T BOTHER                                
*                                                                               
         TM    WHEN,X'20'           ONLY FOR SOON                               
         BZ    NODTAERX              DON'T BOTHER                               
*                                                                               
*        SKIP VALILOC CALL WHEN IN TEST MODE                                    
         TM    SVOPTSW,OPTTEST     TEST ON                                      
         BO    NODTAERX                                                         
*                                                                               
         MVC   DUB,=X'E4010A2500000000' U=UNLOCK, 01=1 ENTRY, 0A25 RECS         
         GOTO1 VALILOC,0                                                        
*                                                                               
NODTAERX GOTO1 ERREX2                                                           
*                                                                               
FAXOFFER L     R1,=A(FAXOFFMS)                                                  
         LA    R2,TRAFAXH                                                       
*                                                                               
ERREXIT2 A     R1,SPTR46RR                                                      
         BCTR  R1,0                                                             
         LLC   RF,0(R1)                                                         
         LA    R0,L'CONHEAD-1                                                   
         CR    R0,RF                                                            
         BNL   *+6                                                              
         DC    H'0'                                                             
         EX    RF,ERREXMVC                                                      
         GOTO1 ERREX2                                                           
*                                                                               
INVPRDER MVI   ERROR,INVPROD                                                    
         B     GENERR                                                           
IOSERR   L     R1,=A(IOSERMS)                                                   
         A     R1,SPTR46RR                                                      
         MVC   CONHEAD(L'IOSERMS),0(R1)                                         
         OI    CONHEADH+6,X'80'                                                 
         DC    H'0',C'$ABEND'      MUST BOMB TO 'RESTORE' RECS WRITTEN          
ERREXMVC MVC   CONHEAD(0),1(R1)                                                 
PRDHSERR MVI   ERROR,INVPRHSE                                                   
*                                                                               
GENERR   GOTO1 ERREX                                                            
         DROP  RA                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
IOSERMS  DC    C'* ERROR * TOO MUCH DATA FOR NOW, RUN SOON/OV/DDS *'            
         DC    AL1(L'AUTOPOMS-1)                                                
AUTOPOMS DC    CL60'* ERROR * T0 PROFILE 9 (PRINT P/O ON SHIPPER) IS SEC        
               T ON *'                                                          
         DC    AL1(L'FAXOFFMS-1)                                                
FAXOFFMS DC    C'* ERROR * FAX NOT ALLOWED FOR OFFLINE *'                       
         DC    AL1(L'POLPRDMS-1)                                                
POLPRDMS DC    C'* ERROR * LEAVE BLANK FOR POL/ALL PRODUCTS *'                  
         DC    AL1(L'PRDREQMS-1)                                                
PRDREQMS DC    C'* ERROR * PRODUCT REQUIRED (PROFILE T0 2) *'                   
         DC    AL1(L'PROPRDMS-1)                                                
PROPRDMS DC    CL60'* ERROR * SHIP GEN MUST RUN BY PRODUCT(PROFILE T0 2C        
               ) *'                                                             
         LTORG                                                                  
         EJECT                                                                  
*-------------------------------------------------------------------*           
* END OF SHIP RECORDS FOR THIS STATION, CK FOR EQUAL SHIP LIST *                
*-------------------------------------------------------------------*           
ENDSTA   NTR1  BASE=*,LABEL=*                                                   
         ST    R4,NEXTADDR         SAVE CURRENT EOL ADDRESS                     
*                                                                               
         CLC   STLFTD,=3X'FF'      TEST NO DATA                                 
         BE    ENDSTA70                                                         
*                                                                               
* SET LENGTH OF COMMERCIAL LIST *                                               
*                                                                               
         L     RE,STLACML                                                       
         SR    R4,RE                                                            
         STCM  R4,3,0(RE)                                                       
         BZ    ENDSTA70            SET TO EXIT IF NO ENTRIES                    
         LR    R1,R4                                                            
         LR    R4,RE                                                            
*                                                                               
         CLI   STLAMSGC,0          THIS HAVE AMS GROUP CODE                     
         BE    ENDSTA04             NO                                          
         MVC   CMLAFF+1-CMLLISTD(1,R4),STLAMSGC                                 
         B     ENDSTA06            BYPASS AFFILIATE CHECK                       
*                                                                               
ENDSTA04 CLI   SVTSPR02,C'Y'       PAGE BREAK BY STATION AFFILIATE              
         BNE   ENDSTA06                                                         
*                                                                               
         MVC   CMLAFF-CMLLISTD(,R4),STLAFF  SAVE STATION AFFILIATE              
*                                                                               
* SORT TO ALPHA SEQUENCE *                                                      
*                                                                               
ENDSTA06 SR    R0,R0                                                            
         D     R0,=A(L'CMLCMMLS)   DIVIDE BY ENTRY LENGTH                       
*                                                                               
         CHI   R0,5                SHOULD BE REMAINDER OF 5                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LTR   R0,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CHI   R0,1                DON'T SORT 1 ENTRY                           
         BE    ENDSTA10                                                         
*                                                                               
         GOTO1 XSORT,DMCB,5(R4),(R0),L'CMLCMMLS,L'CMLCMMLS,0                    
                                                                                
*==================================================                             
* SEARCH FOR IDENTICAL LIST                                                     
*==================================================                             
                                                                                
ENDSTA10 L     R5,ACMLST                                                        
*                                                                               
ENDSTA20 CR    R4,R5               TEST SAME ENTRY                              
         BE    ENDXIT              DONE - NO MATCH                              
         STM   R4,R5,DUB                                                        
         CLC   0(2,R4),0(R5)       TEST SAME LEN                                
         BNE   ENDSTA50                                                         
*                                                                               
* TEST ENTRIES EQUAL *                                                          
*                                                                               
         LA    R0,256                                                           
         SR    RE,RE                                                            
         ICM   RE,3,0(R4)                                                       
ENDSTA30 CR    RE,R0                                                            
         BNH   ENDSTA40                                                         
         CLC   0(256,R4),0(R5)                                                  
         BNE   ENDSTA50                                                         
         AR    R4,R0                                                            
         AR    R5,R0                                                            
         SR    RE,R0                                                            
         B     ENDSTA30                                                         
ENDSTA40 BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(R5) *EXECUTED*                                         
         BE    ENDSTA60                                                         
*                                                                               
ENDSTA50 LM    R4,R5,DUB                                                        
         AH    R5,0(R5)            POINT TO NEXT ENTRY                          
         B     ENDSTA20                                                         
*                                                                               
* ENTRIES ARE IDENTICAL *                                                       
*                                                                               
ENDSTA60 L     R4,STLACML          POINT R4 TO START OF CURRENT ENTRY           
         ST    R4,NEXTADDR         AND SET AS NEXT                              
         XC    0(5,R4),0(R4)                                                    
         MVC   STLACML,DUB+4       SET MATCHING LIST ADDRESS                    
         B     ENDXIT                                                           
*                                                                               
ENDSTA70 MVC   NEXTADDR,STLACML    RESTORE START ADDRESS                        
         L     R4,NEXTADDR         AND SET AS NEXT                              
         XC    0(5,R4),0(R4)                                                    
         XC    STLDATA,STLDATA     CLEAR THE ENTRY                              
ENDXIT   XIT1                                                                   
         EJECT                                                                  
*                                                                               
* CONTACT IS REQUIRED BUT NOT VALIDATED (UNLESS CON=) *                         
*                                                                               
         DS    0H                                                               
VALCONT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    QUESTOR,QUESTOR                                                  
         XC    CONTEL,CONTEL                                                    
         XC    CONFAX,CONFAX                                                    
         XC    CONEMAIL,CONEMAIL                                                
         GOTO1 ANY                                                              
         MVC   QUESTOR(L'TRACON),TRACON                                         
         CLC   =C'CON=',WORK       THIS AGY CONTACT KEY                         
         BNE   VCON20                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CNTKEY,R4                                                        
         MVC   CNTKID,=X'0A36'                                                  
         MVC   CNTKAM(3),BAGYMD                                                 
*                                                                               
         LLC   R1,5(R2)            INPUT LENGTH                                 
         AHI   R1,-5               MINUS 4 (CON=) 1 (FOR EX)                    
         CHI   R1,7                                                             
         BNH   *+8                                                              
         LA    R1,7                NO MORE THAN 8 CHARS                         
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CNTKNAME(0),12(R2)                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VCON10                                                           
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         XC    CNTKCLT,CNTKCLT                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VCON10                                                           
         MVI   ERROR,NOTFOUND                                                   
         GOTO1 ERREX                                                            
*                                                                               
VCON10   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CNTDTAEL,R6                                                      
         MVC   QUESTOR,CNTNAME                                                  
         MVC   CONTEL,CNTTEL                                                    
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VCON15                                                           
         USING CNTFAXEL,R6                                                      
         MVC   CONFAX,CNTFTEL                                                   
*                                                                               
VCON15   L     R6,AIO1                                                          
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VCON20                                                           
         USING CNTEMLEL,R6                                                      
         MVC   CONEMAIL,CNTEMLAD                                                
*                                                                               
VCON20   OC    QUESTOR,SPACES                                                   
         XIT1                                                                   
         DROP  R4,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
* GENERATE OFFLINE OV REQUEST FOR HARD COPY *                                   
* PUT OUT FOR SOON REQUESTS WITH COPY ONLY  *                                   
*                                                                               
AREQ     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   CONWHEN(2),=C'OV'                                                
         MVC   CONWHEN+2(L'CONWHEN-4),CONWHEN+4                                 
         MVC   CONWHEN+L'CONWHEN-2(2),SPACES                                    
*                                                                               
         LLC   R2,TRAOPTH+5                                                     
         LA    R5,TRAOPT(R2)                                                    
         LR    R6,R5                                                            
         SR    R4,R4                                                            
*                                                                               
         LTR   R2,R2                                                            
         BZ    AREQ10                                                           
         MVI   0(R6),C','                                                       
         LA    R6,1(,R6)                                                        
         LA    R4,1                                                             
*                                                                               
AREQ10   MVI   0(R6),C'#'                                                       
         LA    R4,1(,R4)                                                        
         LA    RF,0(R2,R4)                                                      
         STC   RF,TRAOPTH+5                                                     
*                                                                               
         XC    REQHDR,REQHDR                                                    
         MVC   REQUEST,SPACES                                                   
         MVI   REQHDR+15,X'01'     GENERATE LINKED REQUESTS                     
         MVC   REQUEST(2),=C'TO'                                                
         MVC   REQUEST+2(2),AGENCY                                              
*                                                                               
         ICM   RE,15,TWAMASTC                                                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   REQUEST+5(6),MCREQREC+5-MASTD(RE)                                
*                                                                               
         XC    REQSPOOK,REQSPOOK   NEED TO PASS A SPOOK                         
         GOTO1 REQTWA,DMCB,(0,ATWA),REQHDR,DATAMGR,RCCOMFAC,REQSPOOK            
*                                                                               
         MVC   WORK(L'CONWHEN-4),CONWHEN+2                                      
         MVC   CONWHEN+4(L'CONWHEN-4),WORK                                      
         MVC   CONWHEN(4),=C'SOON'                                              
         XC    CONHEAD,CONHEAD                                                  
*                                                                               
         EX    R4,NREQCLR                                                       
         STC   R2,TRAOPTH+5                                                     
         XIT1                                                                   
*                                                                               
NREQCLR  MVC   0(0,R5),SPACES                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* READ PRODUCTION HOUSE ADDRESS *                                               
*                                                                               
RPHSE    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SVOPTICA,SVOPTICA                                                
         XC    SVDELAY,SVDELAY                                                  
*                                                                               
         L     R3,ATWA                                                          
         USING T216FFD,R3                                                       
         LA    R2,TRAHSEH            FIRST CHECK FOR HOUSE OVERRIDE             
*                                                                               
         CLI   5(R2),0                                                          
         BE    RPHSE10                                                          
         GOTO1 ANY                                                              
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A29'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+3(6),WORK                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PRDHSER                                                          
         B     RPHSE50                  READ HOUSE AND SAVE ADDRESS             
*                                                                               
RPHSE10  XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(8),=C'99999999'                                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NOPRDHSE                                                         
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,24(R6)                                                        
         CLI   0(R6),X'10'                                                      
         BNE   NOPRDHSE            MUST BE 9'S KEY PTR TO PROD HSE              
*                                                                               
         USING CMLDTAEL,R6                                                      
*                                                                               
* BUILD PRODUCTION HOUSE KEY *                                                  
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A29'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(6),CMLTITLE   PROD HOUSE ID IS IN TITLE                    
         OC    KEY+3(6),SPACES                                                  
         DROP  R6                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   PRDHSER                                                          
*                                                                               
RPHSE50  L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   HOUSECD,3(R6)                                                    
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PRDHSELS,2(R6)     SAVE ADDRESS                                  
*                                                                               
* OPTICA NOT SUPPORTED FOR "NOW" REQUESTS (PHASE 1)                             
         CLI   WHEN,X'40'          IS THIS NOW REQUEST                          
         BE    RPHSE55             YES, OPTICA IS FOR SOON ONLY                 
*                                                                               
         TM    SVOPTSW,OPTTEST     TEST MODE                                    
         BO    RPHSE55             YES, NO OPTICA                               
*                                                                               
         CLI   TRAFAX,C'N'         NO FAX REQUEST                               
         BE    RPHSE55             YES, NO OPTICA                               
*                                                                               
         L     RE,TWAMASTC                                                      
         TM    MCAGCOPT-MASTD(RE),MCAGCUAT UAT AGY?                             
         BZ    RPHSE52                                                          
         MVI   SVTWPR05,C'N'       YES                                          
         MVI   TRAFAX,C'N'         NO FAX AND                                   
         B     RPHSE55             NO MQ TO OPTICA                              
*                                                                               
RPHSE52  L     R6,AIO1                                                          
         MVI   ELCODE,X'25'        OPTICA ELEMENT                               
         BRAS  RE,GETEL                                                         
         BNE   RPHSE55                                                          
*                                                                               
         USING PRHOPCEL,R6                                                      
         MVC   SVOPTICA,PRHOPC     OPTICA COMPANY                               
         MVC   SVDELAY,PRHDELAY    DELAY IN MINUTES                             
*                                                                               
*NOP>>>  MVI   SVTWPR05,C'N'       NO FAX, OPTICA ONLY                          
         B     RPHSEX                                                           
*                                                                               
RPHSE55  XC    PRDHFAX,PRDHFAX                                                  
         L     R6,AIO1                                                          
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,GETEL                                                         
         BNE   RPHSE60                                                          
         USING PRHFAXEL,R6                                                      
         LA    R1,PRDHFAX                                                       
         CLI   PRHFAXLN,20                                                      
         BE    RPHSE54                                                          
         CLI   PRHFTEL1,0                                                       
         BE    RPHSE54                                                          
         MVC   PRDHFAX(1),PRHFTEL1                                              
         LA    R1,1(,R1)                                                        
*                                                                               
RPHSE54  MVC   0(18,R1),PRHFTEL                                                 
         B     RPHSEX                                                           
         DROP  R6                                                               
*                                                                               
RPHSE60  CLI   SVTWPR05,C'N'         SEE IF FAX REQUIRED                        
         BE    RPHSEX                                                           
         B     NOFAXERR                                                         
*                                                                               
RPHSEX   XIT1                                                                   
*                                                                               
NOPRDHSE MVI   ERROR,NOPRDHS                                                    
         B     PRDHSERA                                                         
*                                                                               
PRDHSER  MVI   ERROR,INVPRHSE                                                   
*                                                                               
PRDHSERA LA    R2,TRAMEDH          POSITION CURSOR                              
         GOTO1 ERREX                                                            
*                                                                               
NOFAXERR MVC   CONHEAD,NOFAXMSG                                                 
         GOTO1 ERREX2                                                           
*                                                                               
NOFAXMSG DC    CL60'* ERROR * PRODUCTION HOUSE MUST HAVE FAX NUMBER *'          
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
* VALID OPTIONS ARE    'TEST'  (X80)                                            
*                      'RERUN' (X40) DATE REQUIRED                              
*                      'DATE'  AS OF DATE                                       
*                      'CT'    SHOW COUNTS       \                              
*                      'NOCT'  DON'T SHOW COUNTS  \ OVERRIDES PROFILE           
*                      'TYP'   SHOW COMML TYPES  \                              
*                      'NOTYP' DON'T SHOW TYPES   \ OVERRIDES PROFILE           
*                      'CML'/'COM'/'CMML' 3 TO 8 CHAR                           
*                      'CLASS'                                                  
*                      'MKT'                                                    
*                      'AFF'                                                    
*                      'O/A' AUTO TURN AROUND REQ FROM ONLINE INSTR             
*                      'T/A' AUTO TURN AROUND REQ FROM OFFLINE INSTR            
VOPT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,ATWA                                                          
         USING T216FFD,R3                                                       
*                                                                               
         XC    SVCMLADI,SVCMLADI                                                
         XC    SVCMLADP,SVCMLADP                                                
         XC    ENVIRNMT,ENVIRNMT                                                
         XC    QUESNAME,QUESNAME                                                
*                                                                               
         LA    R2,TRAOPTH                                                       
         CLI   TRAOPT,C'?'         HELP FOR AVAIL OPTS                          
         BE    OPTHLP                                                           
*                                                                               
         XC    OPTIONS,OPTIONS                                                  
         XC    BLOCK(256),BLOCK                                                 
         LA    R4,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(20,(R2)),(7,(R4))                                  
*                                                                               
VOPT10   CLI   0(R4),0             TEST FOR MORE DATA                           
         BE    VOPTCHK                                                          
         CLI   0(R4),2                                                          
         BL    VOPT11                                                           
         BNE   VOPT12                                                           
         CLC   =C'CT ',12(R4)                                                   
         BE    VOPT72                                                           
*                                                                               
VOPT11   CLI   12(R4),C'#'         THIS AGY COPY FOLLOW UP                      
         BNE   VOPTERR                                                          
         CLI   OFFLINE,C'Y'                                                     
         BNE   VOPTERR             MUST BE OFFLINE                              
         OI    SVOPTSW1,OPTHDCPY   AGENCY COPY                                  
         MVI   SVTWPR05,C'N'                                                    
         B     VOPT999                                                          
*                                                                               
VOPT12   CLI   0(R4),3                                                          
         BNE   VOPT14                                                           
         CLC   =C'CML ',12(R4)                                                  
         BE    VOPT30                                                           
         CLC   =C'COM ',12(R4)                                                  
         BE    VOPT30                                                           
         CLC   =C'MKT ',12(R4)                                                  
         BE    VOPT50                                                           
         CLC   =C'T/A ',12(R4)                                                  
         BE    VOPT60                                                           
         CLC   =C'O/A ',12(R4)                                                  
         BE    VOPT64                                                           
         CLC   =C'FAX ',12(R4)                                                  
         BE    VOPT68                                                           
         CLC   =C'TYP ',12(R4)                                                  
         BE    VOPT76                                                           
         CLC   =C'AFF ',12(R4)                                                  
         BE    VOPT80                                                           
         CLC   =C'ADI ',12(R4)                                                  
         BE    VOPT90                                                           
         B     VOPT100                                                          
                                                                                
VOPT14   CLI   0(R4),4                                                          
         BNE   VOPT15                                                           
         CLC   =C'ADID',12(R4)                                                  
         BE    VOPT90                                                           
         CLC   =C'DATE',12(R4)                                                  
         BE    VOPT70                                                           
         CLC   =C'CMML',12(R4)                                                  
         BE    VOPT30                                                           
         CLC   =C'SIZE',12(R4)                                                  
         BE    VOPT40                                                           
         CLC   =C'NOCT',12(R4)                                                  
         BE    VOPT74                                                           
         CLC   =C'TEST',12(R4)                                                  
         BNE   VOPTERR                                                          
         OI    SVOPTSW,OPTTEST     TEST MODE                                    
         B     VOPT999                                                          
*                                                                               
VOPT15   CLI   0(R4),5                                                          
         BNE   VOPT16                                                           
         CLC   =C'CLASS',12(R4)                                                 
         BE    VOPT20                                                           
         CLC   =C'NOTYP',12(R4)                                                 
         BE    VOPT78                                                           
         CLC   =C'RERUN',12(R4)                                                 
         BNE   VOPTERR                                                          
*                                                                               
         TM    SVOPTSW,OPTDATE                                                  
         BO    RRDTEER             NO RERUN AND AS OF DATE                      
*                                                                               
         OI    SVOPTSW,OPTRERUN                                                 
         MVI   ERROR,NODATE        MUST SPECIFY ORIGINAL DATE                   
         CLI   1(R4),0                                                          
         BE    VOPTGEN                                                          
         GOTO1 DATVAL,DMCB,22(R4),DUB                                           
         MVI   ERROR,INVDATE                                                    
         OC    0(4,R1),0(R1)                                                    
         BZ    VOPTGEN                                                          
         GOTO1 DATCON,DMCB,DUB,(3,SVOPTDTE)                                     
         B     VOPT999                                                          
VOPT16   CLI   0(R4),6                                                          
         BNE   VOPTERR                                                          
         CLC   =C'MARKET',12(R4)                                                
         BE    VOPT50                                                           
         B     VOPTERR                                                          
*                                                                               
* FILTER ON COMMERCIAL CLASS *                                                  
*                                                                               
VOPT20   CLI   1(R4),1                                                          
         BNE   CMLCLSER                                                         
         MVC   SVOPTCLS,22(R4)                                                  
         B     VOPT999                                                          
*                                                                               
* FILTER ON 3-8 CHARS OF COMMERCIAL *                                           
*                                                                               
VOPT30   MVI   ERROR,INVCOMM                                                    
         CLI   1(R4),3             MUST SPECIFY AT LEAST 3 CHARS                
         BL    COMFTRER                                                         
         CLI   1(R4),8                                                          
         BH    VOPTGEN                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(3),BAGYMD                                                  
         MVC   KEY+5(8),22(R4)                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE                                                   
         BNE   VOPT38                                                           
         LLC   R6,1(R4)                                                         
         BCTR  R6,0                                                             
         EX    R6,VOPTCLC                                                       
         BNE   VOPT38                                                           
         MVC   SVOPTCML,22(R4)     SAVE INPUT                                   
         MVC   SVCMLLN,1(R4)       SAVE NUMBER OF CHARS                         
         B     VOPT999                                                          
VOPTCLC  CLC   KEY+5(0),KEYSAVE+5                                               
VOPT38   MVI   ERROR,INVCOMM                                                    
         B     VOPTGEN                                                          
*                                                                               
* SHOW TABLE SIZES ONLINE *                                                     
*                                                                               
VOPT40   L     RF,ATWA                                                          
         CLI   OFFLINE,C'Y'                                                     
         BE    VOPT44                                                           
         CLI   1(RF),C'*'          THIS A DDS TERMINAL                          
         BNE   VOPTERR                                                          
VOPT44   OI    SVOPTSW,OPTSIZE                                                  
         B     VOPT999                                                          
*                                                                               
* ONLY DO MARKET 0000 *                                                         
*                                                                               
* CONSTRUCT FLDHDR FOR VALIMKT *                                                
*                                                                               
VOPT50   XC    ELEM,ELEM                                                        
         LLC   RE,1(R4)                                                         
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
         LA    R2,TRAOPTH                                                       
         CLI   ERROR,0                                                          
         BNE   VOPTGEN                                                          
         MVC   SVOPTMKT,BMKT                                                    
         B     VOPT999                                                          
*                                                                               
* THIS IS AN AUTO TURN/AROUND FROM INSTRUCTIONS *                               
*                                                                               
VOPT60   CLI   OFFLINE,C'Y'        MUST BE OFFLINE                              
         BNE   VOPTERR                                                          
         OI    SVOPTSW,OPTTA       T/A FROM OFFLINE INSTR                       
         B     VOPT999                                                          
*                                                                               
VOPT64   CLI   OFFLINE,C'Y'        MUST BE OFFLINE                              
         BNE   VOPTERR                                                          
         OI    SVOPTSW,OPTOA       T/A FROM ONLINE INSTR                        
         B     VOPT999                                                          
*                                                                               
VOPT68   CLI   22(R4),C'Y'                                                      
         BNE   VOPT68C                                                          
         CLI   SVTWPR05,C'N'         TW PROFILE MUST ALLOW FAXED                
         BE    VOPTINV               SHIPPING                                   
         B     VOPT999                                                          
*                                                                               
VOPT68C  CLI   22(R4),C'N'                                                      
         BNE   VOPTINV                                                          
         MVI   SVTWPR05,C'N'        SET FOR NO FAX SHIPPING                     
         B     VOPT999                                                          
*                                                                               
* GENERATE SHIP GEN AS OF DATE                                                  
VOPT70   TM    SVOPTSW,OPTRERUN                                                 
         BO    RRDTEER             NO RERUN AND AS OF DATE                      
*                                                                               
         OI    SVOPTSW,OPTDATE                                                  
         MVI   ERROR,NODATE        MUST SPECIFY ORIGINAL DATE                   
         CLI   1(R4),0                                                          
         BE    VOPTGEN                                                          
         GOTO1 DATVAL,DMCB,22(R4),DUB                                           
         MVI   ERROR,INVDATE                                                    
         OC    0(4,R1),0(R1)                                                    
         BZ    VOPTGEN                                                          
         GOTO1 DATCON,DMCB,DUB,(3,SVOPTDTE)                                     
         B     VOPT999                                                          
*                                                                               
VOPT72   MVI   SVTSPR05,C'N'       SHOW COUNTS     (CT)                         
         B     VOPT999                                                          
VOPT74   MVI   SVTSPR05,C'Y'       SUPPRESS COUNTS (NOCT)                       
         B     VOPT999                                                          
*                                                                               
VOPT76   MVI   SVTSPR04,C'N'       SHOW COMML TYPE  (TYP)                       
         B     VOPT999                                                          
VOPT78   MVI   SVTSPR04,C'Y'       SUPPRESS CML TYPE (NOTYP)                    
         B     VOPT999                                                          
*                                                                               
* FILTER ON STATION AFFILIATE *                                                 
*                                                                               
VOPT80   CLI   SVTSPR02,C'Y'       PAGE BREAK BY AFFILIATE                      
         BNE   VOPTINV              NO, NOT OK                                  
         CLI   1(R4),0                                                          
         BE    AFFLENER                                                         
         MVC   SVOPTAFF,22(R4)                                                  
         B     VOPT999                                                          
*                                                                               
VOPT90   DS    0H                                                               
         MVI   ERROR,0                                                          
         CLI   1(R4),4             MUST SPECIFY AT LEAST 3 CHARS                
         BL    ADIDLENM                                                         
         CLI   1(R4),12                                                         
         BH    ADIDLENM                                                         
                                                                                
         MVC   SVADILEN,1(R4)                                                   
         ZIC   R1,1(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVCMLADI(0),22(R4)                                               
         B     VOPT999                                                          
*                                                                               
* CHECK FOR ENVIRONMENT                                                         
VOPT100  CLI   ENVIRNMT,0          DID WE GET THE ENVIRONMENT ?                 
         BNE   VOPTERR              YES, NO OTHER VALID OPTIONS.                
*                                                                               
         LA    R5,ENVTABLE                                                      
         USING ENVD,R5                                                          
*                                                                               
         LA    R0,ENVTABLN                                                      
*                                                                               
VENV04   CLC   ENVOPT,12(R4)       MATCH ON OPTION ?                            
         BNE   VENV09                                                           
*                                                                               
         CLI   ENVIRNMT,0          DID WE GET THE ENVIRONMENT ?                 
         BNE   VENV06               YES                                         
*                                                                               
* GET ENVIRONMENT (A)DV, (T)ST, (C)SC                                           
*                                                                               
         CLI  OFFLINE,C'Y'         IF OFFLINE                                   
         BE   VENV05                                                            
*                                                                               
         L     R1,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(R1)                                         
         GOTO1 (RF),DMCB,0                                                      
         L     RE,DMCB             FAFACTS                                      
         MVC   ENVIRNMT(1),FADSPACE-FACTSD(RE)  1ST LETTER OF ENVRNMNT          
         B     VENV06                                                           
*                                                                               
VENV05   L     R3,ATWA                                                          
         L     RF,TWAMASTC                                                      
         L     RF,MCSSB-MASTD(RF)                                               
         MVC   ENVIRNMT(1),SSODSPAC-SSOOFF(RF)  DSPACE                          
*                                                                               
VENV06   CLC   ENVDSP,ENVIRNMT     MATCH ON ENVIRONMENT ?                       
         BNE   VENV09                                                           
*                                                                               
VENV08   MVC   ENVIRNMT,ENVXML     XML ENVIRONMENT                              
         MVC   QUESNAME,ENVQUE     QUEUE SUB-NAME                               
         B     VOPT999                                                          
*                                                                               
VENV09   LA    R5,ENVLEN(R5)                                                    
         BCT   R0,VENV04                                                        
*                                                                               
         B     VOPTERR                                                          
*                                                                               
         DROP  R5                                                               
*                                                                               
VOPT999  LA    R4,42(R4)                                                        
         B     VOPT10                                                           
*                                                                               
*                                                                               
* ENVIRONMENT TABLE WHEN SPECIFIC OPTION IS ENTERED.                            
*                                                                               
*            OPTION  DSPACE XML ENV  QUEUE                                      
ENVTABLE DC    C'TRN',C'P',C'ADV',C'TRN*'   TRAINING                            
         DC    C'TRN',C'A',C'ADV',C'TRN*'                                       
         DC    C'DEM',C'P',C'ADV',C'DEM*'   DEMO                                
         DC    C'DEM',C'A',C'ADV',C'DEM*'                                       
         DC    C'UAT',C'P',C'ADV',C'UAT*'   UAT                                 
         DC    C'UAT',C'A',C'ADV',C'UAT*'                                       
         DC    C'STG',C'T',C'TST',C'STG*'   STG                                 
ENVTABLN EQU   (*-ENVTABLE)/ENVLEN                                              
*                                                                               
*                                                                               
*                                                                               
VOPTERR  MVC   CONHEAD,SPACES                                                   
         MVC   CONHEAD(25),=C'* ERROR * - UNKNOWN INPUT'                        
         MVC   CONHEAD+28(8),12(R4)                                             
VOPTERRX GOTO1 ERREX2                                                           
*                                                                               
RRDTEER  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(36),=CL36'* ENTER RERUN OR DATE= BUT NOT BOTH *'         
         B     VOPTERRX                                                         
*                                                                               
AFFLENER MVC   CONHEAD,SPACES                                                   
         MVC   CONHEAD(30),=C'* ERROR * MUST ENTER AFF=XXX *'                   
         B     VOPTERRX                                                         
*                                                                               
*                                                                               
VOPTCHK  DS    0H                 CROSS CHECK OPTIONS                           
         TM    SOXSW,SOXOKFLG      IF DDS, AND FACTEST, OKAY TO GO              
         BO    VOPTCHKB                                                         
*                                                                               
         TM    SOXSW,SOXERFLG      IF RD ONLY/RD ONLY MODE/WRONG ADV            
         BZ    VOPTCHKB                                                         
*                                                                               
         TM    SVOPTSW,OPTTEST                                                  
         BO    VOPTCHKB                                                         
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
VOPTCHKB DS    0H                                                               
         CLI   SVTWPR05,C'N'      SEE IF FAXING                                 
         BE    VOPTX               NO                                           
         TM    SVOPTSW,OPTTEST    WITH OPTION TEST                              
         BNO   VOPTX                                                            
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(32),=CL32'* USE FAX=NO WITH TEST REQUEST *'              
         GOTO1 ERREX2                                                           
*                                                                               
VOPTX    XIT1                                                                   
*                                                                               
VOPTINV  MVI   ERROR,INVALID                                                    
         B     VOPTGEN                                                          
*                                                                               
VOPTGEN  GOTO1 ERREX                                                            
*                                                                               
OPTHLP   MVC   CONHEAD,OPTHLPMS                                                 
         GOTO1 ERREX2                                                           
CMLCLSER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'CMLCLSMS),CMLCLSMS                                     
         GOTO1 ERREX2                                                           
COMFTRER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'COMFTRMS),COMFTRMS                                     
         GOTO1 ERREX2                                                           
ADIDLENM XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'ADIDLEN),ADIDLEN                                       
         GOTO1 ERREX2                                                           
ADIDERRM XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'ADIDERR),ADIDERR                                       
         GOTO1 ERREX2                                                           
ADIDBADM XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'ADIDBAD),ADIDBAD                                       
         GOTO1 ERREX2                                                           
         DROP  R3                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         DC    AL1(L'OPTHLPMS-1)                                                
OPTHLPMS DC    CL60'OPTS=TEST/RERUN=/COM=(3-8 CMML CD)/CLASS/MKT/MB *'          
CMLCLSMS DC    C'* ERROR * CML CLASS MUST BE 1 CHARACTER *'                     
COMFTRMS DC    C'* ERROR * COMMERCIAL MUST BE 3-8 CHARACTRS *'                  
ADIDLEN  DC    C'* ERROR * ADID MUST BE 9-12 CHARACTERS *'                      
ADIDERR  DC    C'* ERROR * UNKNOWN ADID *'                                      
ADIDBAD  DC    C'* ERROR * INVALID ADID *'                                      
         EJECT                                                                  
*                                                                               
HDSPECS  DS    0H                                                               
         SPROG 1,THRU,10                                                        
*                                                                               
         SSPEC H1,1,AGYNAME                                                     
         SSPEC H2,1,AGYADD                                                      
*                                                                               
*NOTE*   SSPEC H1,40,C'SPOT TELEVISION SHIPPING ORDER'                          
*NOTE*   SSPEC H2,40,C'------------------------------'                          
*                                                                               
         SSPEC H1,77,RUN                                                        
         SSPEC H2,77,REPORT                                                     
         SSPEC H3,77,REQUESTOR                                                  
         SSPEC H3,96,PAGE                                                       
*                                                                               
         SPROG 1,3                                                              
         SSPEC M1,1,C'-CMML NUMBER-   -- OTHER ---   TYPE'                      
         SSPEC M2,1,C'-------------   ------------   ----'                      
*                                                                               
         SSPEC M1,38,C'TITLE1'                                                  
         SSPEC M2,38,C'---------------'                                         
*                                                                               
         SSPEC M1,55,C'TITLE2'                                                  
         SSPEC M2,55,C'--------------------'                                    
*                                                                               
         SSPEC M1,77,C'TITLE3'                                                  
         SSPEC M2,77,C'--------------------'                                    
*                                                                               
         SSPEC M1,99,C'NUMBER'                                                  
         SSPEC M2,99,C'------'                                                  
*                                                                               
         SPROG 2                                                                
         SSPEC M1,1,C'  STATION     --- MARKET NAME ---    FIRST TLCST'         
         SSPEC M2,1,C'  -------     -------------------    -----------'         
*                                                                               
         SSPEC M1,60,C'STATION     --- MARKET NAME ---    FIRST TLCST'          
         SSPEC M2,60,C'-------     -------------------    -----------'          
*                                                                               
         SPROG 3                                                                
         SSPEC H1,38,C'***********************************'                     
         SSPEC H2,38,C'* S H I P P I N G   S U M M A R Y *'                     
         SSPEC H3,38,C'***********************************'                     
*                                                                               
         DC    X'00'               END OF TABLE                                 
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* GET COMMERCIAL TYPE, CLASS, AND FILTER ON PRODUCT IF NEEDED                   
*==================================================================             
                                                                                
         USING SHPDTAEL,R6                                                      
CML      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,CMLTSIZ                                                       
         LA    R5,SVCMLTBL                                                      
         USING CMLTABD,R5                                                       
CML10    OC    CMLTENT,CMLTENT     EMPTY ENTRY                                  
         BZ    CML30                                                            
         CLC   CMLTSEQ,KEY+SHPKCSEQ+1-SHPKEY                                    
         BNE   *+14                                                             
         CLC   CMLTCML2,SHPCMML2                                                
         BE    CML12                                                            
         LA    R5,CMLTNEXT                                                      
         BCT   R4,CML10                                                         
         B     CML30                                                            
*                                                                               
CML12    CLI   CMLTUSD,CMLTSIZ     ALREADY MOST RECENTLY USED                   
         BE    CML20                                                            
         LA    R0,CMLTSIZ          DROP MOST RECENTLY USED COUNT                
         LA    R1,SVCMLTBL                                                      
         SR    RF,RF                                                            
CML14    CR    R1,R5                                                            
         BE    CML16                                                            
         CLC   CMLTUSD,CMLTUSD-CMLTENT(R1)  LEAVE IF < NEW LAST USED            
         BH    CML16                                                            
         ICM   RF,1,CMLTUSD-CMLTENT(R1)                                         
         BZ    CML18                                                            
         BCTR  RF,0                                                             
         STC   RF,CMLTUSD-CMLTENT(,R1)                                          
CML16    LA    R1,L'CMLTENT(,R1)                                                
         BCT   R0,CML14                                                         
         B     *+16                                                             
CML18    OC    0(2,R1),0(R1)       SHOULD BE AN EMPTY ENTRY                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   CMLTUSD,CMLTSIZ     SET MOST RECENTLY USED                       
CML20    CLI   SVOPTCLS,0          FILTER ON COMML CLASS                        
         BE    CML22                NO                                          
         CLC   SVOPTCLS,CMLTCLS                                                 
         BNE   CMLNE                                                            
         OC    CMLTCML2,CMLTCML2   IS THERE A P/B CML                           
         BZ    CML22                NO                                          
         CLC   SVOPTCLS,CMLTCLS2                                                
         BNE   CMLNE                                                            
CML22    CLI   BPRD,0              FILTER ON PROD                               
         BE    CML24                NO                                          
         CLI   CMLTPRD,X'FF'       ALL PRODS                                    
         BE    CML24                                                            
         CLC   CMLTPRD,BPRD                                                     
         BNE   CMLNE                NO                                          
CML24    CLI   BPRD2,0             FILTER ON P/B PROD                           
         BE    CML28                NO                                          
         CLI   BPRD2,X'FF'         P/B = NONE                                   
         BNE   CML26                NO                                          
         OC    SHPCMML2,SHPCMML2                                                
         BNZ   CMLNE                                                            
         B     CML28                NO                                          
CML26    CLI   CMLTPRD2,X'FF'      ALL PRODS                                    
         BE    CML28                                                            
         CLC   CMLTPRD2,BPRD2                                                   
         BNE   CMLNE                                                            
*                                                                               
CML28    CLI   SVTSPR04,C'Y'       SUPPRESS COMMERCIAL TYPE                     
         BE    *+10                                                             
         MVC   SVCMLTYP,CMLTTYP                                                 
         B     CMLEQ                                                            
         EJECT                                                                  
*=================================================================              
* NOW CHECK AIO3 FOR POSSIBLE COMMERCIAL RECORD, IF NOT GET IT                  
*=================================================================              
                                                                                
CML30    MVC   SVCMML2,SHPCMML2                                                 
         MVI   BYTE,0                                                           
         LA    R5,WORK                                                          
         XC    WORK,WORK                                                        
         L     R6,AIO3                                                          
         CLC   =X'0A21',0(R6)                                                   
         BNE   CML34                                                            
         CLC   2(3,R6),BAGYMD     A-M/CLT                                       
         BNE   CML34                                                            
         CLC   CMLSEQ-CMLKEY(,R6),KEY+SHPKCSEQ-SHPKEY                           
         BE    CML40                                                            
*                                                                               
CML34    ST    R6,AIO                                                           
         MVC   SVKEY,KEY                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLRECD,R4                                                       
         MVC   CMLPID,=X'0AA1'                                                  
         MVC   CMLPAM(3),BAGYMD     A-M/CLT                                     
         MVC   CMLPSEQ,SVKEY+SHPKCSEQ-SHPKEY                                    
         DROP  R4                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVC   KEY(13),SVKEY       RESTORE SHIP RECAP                           
*                                                                               
         TM    SVOPTSW,OPTTEST     TEST ON                                      
         BZ    *+8                                                              
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         TM    SVOPTSW,OPTTEST     THIS TEST RUN                                
         BO    CML40                                                            
         TM    SVOPTSW,OPTCPY2     THIS SECOND COPY (FAX)                       
         BO    CML40                                                            
         GOTO1 GETREC                                                           
*                                                                               
CML40    MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
*                                                                               
         MVC   CMLTSEQ,CMLSEQ+1                                                 
         MVC   CMLTCLS,CMLCLASS                                                 
*                                                                               
         CLI   SVTSPR04,C'Y'       SUPPRESS COMMERCIAL TYPE                     
         BE    CML42                                                            
         MVC   CMLTTYP,CMLTYPE                                                  
         MVC   SVCMLTYP,CMLTYPE                                                 
*                                                                               
CML42    CLI   SVOPTCLS,0          FILTER ON COMML CLASS                        
         BE    CML44                NO                                          
         CLC   SVOPTCLS,CMLCLASS                                                
         BE    CML44                                                            
         MVI   BYTE,1                                                           
*                                                                               
CML44    CLI   BPRD,0              FILTER ON PROD                               
         BE    CML50                NO                                          
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,2(,R6)           GET PROD LIST POINTER FIRST                  
*                                                                               
         CLI   2(R6),X'FF'         CK ALL PRODUCTS                              
         BE    CML46                YES                                         
         LLC   R0,1(R6)            NOW GET ELEM LENGTH                          
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
         CLC   0(1,R1),BPRD        THIS PRODUCT                                 
         BE    CML46                YES                                         
         LA    R1,1(,R1)                                                        
         BCT   R0,*-14                                                          
         MVI   BYTE,1                                                           
         MVC   CMLTPRD,2(R6)            SAVE ANY NE PROD                        
         B     *+10                                                             
CML46    MVC   CMLTPRD,0(R1)            SAVE VALID PROD                         
*                                                                               
* NOW CHECK FOR PTR CML IF NEEDED *                                             
*                                                                               
CML50    CLI   BPRD2,0             DO WE CARE ABOUT PTR PROD                    
         BE    CML70                NO                                          
*                                                                               
         CLI   BPRD2,X'FF'         PTR PROD NONE                                
         BNE   CML52                NO                                          
*                                                                               
         OC    SVCMML2,SVCMML2     P/B COMMERCIAL                               
         BZ    CML70                NO                                          
         MVI   BYTE,1                                                           
         B     CML70                                                            
*                                                                               
CML52    OC    SVCMML2,SVCMML2     P/B COMMERCIAL                               
         BNZ   *+12                 YES                                         
         MVI   BYTE,1                                                           
         B     CML70                                                            
         MVC   CMLTCML2,SVCMML2                                                 
         L     R6,AIO3                                                          
         CLC   =X'0A21',0(R6)                                                   
         BNE   CML54                                                            
         CLC   2(3,R6),BAGYMD     A-M/CLT                                       
         BNE   CML54                                                            
         CLC   CMLKCML-CMLKEY(,R6),SVCMML2                                      
         BE    CML60                                                            
CML54    ST    R6,AIO                                                           
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLRECD,R4                                                       
         MVC   CMLPID,=X'0A21'                                                  
         MVC   CMLKAM(3),BAGYMD     A-M/CLT                                     
         MVC   CMLKCML,SVCMML2                                                  
         DROP  R4                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         MVC   KEY(13),SVKEY       RESTORE SHIP RECAP                           
*                                                                               
         TM    SVOPTSW,OPTTEST     TEST ON                                      
         BZ    *+8                                                              
         MVI   RDUPDATE,C'N'                                                    
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         TM    SVOPTSW,OPTTEST     THIS TEST RUN                                
         BO    CML60                                                            
         TM    SVOPTSW,OPTCPY2     THIS SECOND COPY (FAX)                       
         BO    CML60                                                            
         GOTO1 GETREC                                                           
*                                                                               
CML60    MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
*                                                                               
         CLI   SVTSPR04,C'Y'       SUPPRESS TYPE                                
         BE    *+10                                                             
         MVC   CMLTTYP2,CMLTYPE                                                 
         MVC   CMLTCLS2,CMLCLASS                                                
*                                                                               
         CLI   SVOPTCLS,0          FILTER ON COMML CLASS                        
         BE    CML64                NO                                          
         CLC   SVOPTCLS,CMLCLASS                                                
         BE    CML64                                                            
         MVI   BYTE,1                                                           
*                                                                               
CML64    MVI   ELCODE,X'20'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   BPRD2,0             FILTER ON PROD 2                             
         BE    CML70                NO                                          
         CLI   2(R6),X'FF'         CK ALL PRODUCTS                              
         BE    CML66                YES                                         
         LLC   R0,1(R6)                                                         
         BCTR  R0,0                                                             
         BCTR  R0,0                                                             
         LA    R1,2(R6)                                                         
         CLC   0(1,R1),BPRD2       THIS PRODUCT                                 
         BE    CML66                YES                                         
         LA    R1,1(,R1)                                                        
         BCT   R0,*-14                                                          
         MVI   BYTE,1                                                           
         MVC   CMLTPRD,2(R6)                                                    
         B     *+10                                                             
CML66    MVC   CMLTPRD2,0(R1)                                                   
*                                                                               
CML70    BAS   RE,CMLTBL           GO BUILD TABLE                               
*                                                                               
         CLI   BYTE,0              DID THIS PASS ALL FILTERS                    
         BE    CMLEQ                                                            
*                                                                               
CMLNE    CR    RB,RD                                                            
         B     CMLX                                                             
*                                                                               
CMLEQ    CR    RB,RB                                                            
CMLX     XIT1                                                                   
         EJECT                                                                  
* BUILD CML TABLE ENTRY AND UPDATE LAST USED BYTE *                             
*                                                                               
CMLTBL   LA    R4,CMLTSIZ                                                       
         LA    R5,SVCMLTBL                                                      
CMLTBL14 OC    CMLTENT,CMLTENT      EMPTY ENTRY                                 
         BZ    CMLTBL20                                                         
         CLI   CMLTUSD,1                                                        
         BE    CMLTBL20                                                         
         LA    R5,CMLTNEXT                                                      
         BCT   R4,CMLTBL14                                                      
         DC    H'0'                                                             
CMLTBL20 MVC   CMLTENT,WORK                                                     
*                                                                               
         MVI   CMLTUSD,CMLTSIZ     SET MOST RECENTLY USED                       
*                                                                               
         LA    R0,CMLTSIZ          DROP MOST RECENTLY USED COUNT                
         LA    R1,SVCMLTBL                                                      
         SR    RF,RF                                                            
CMLTBL30 CR    R1,R5                                                            
         BE    CMLTBL36                                                         
         ICM   RF,1,CMLTUSD-CMLTENT(R1)                                         
         BZ    CMLTBL40                                                         
         BCTR  RF,0                                                             
         STC   RF,CMLTUSD-CMLTENT(,R1)                                          
CMLTBL36 LA    R1,L'CMLTENT(,R1)                                                
         BCT   R0,CMLTBL30                                                      
         BR    RE                                                               
CMLTBL40 OC    0(L'CMLTENT,R1),0(R1)  SHOULD BE AN EMPTY ENTRY                  
         BZR   RE                                                               
         DC    H'0'                                                             
*                                                                               
         LTORG                                                                  
         DROP  R5,R6                                                            
         EJECT                                                                  
*==========================================================                     
* HEADHOOK ROUTINE *                                                            
*==========================================================                     
                                                                                
HDHK     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   HEADHOOK,C'R'       TEST RETURN CALL                             
         BE    HDHKCMT                                                          
*                                                                               
         LA    R5,H2+90                                                         
         TM    SVOPTSW,OPTTEST                                                  
         BZ    *+14                                                             
         MVC   0(6,R5),=C'(TEST)'                                               
         LA    R5,7(R5)                                                         
*                                                                               
         TM    SVOPTSW,OPTTA       THIS AUTO T/A FROM OFFLINE INST              
         BZ    *+14                                                             
         MVC   0(3,R5),=C'T/A'                                                  
         LA    R5,4(R5)                                                         
*                                                                               
         TM    SVOPTSW,OPTOA       THIS AUTO T/A FROM ONLINE INST               
         BZ    *+14                                                             
         MVC   0(3,R5),=C'O/A'                                                  
         LA    R5,4(R5)                                                         
*                                                                               
         TM    SVOPTSW,OPTRERUN                                                 
         BZ    *+14                                                             
         MVC   0(5,R5),=C'RERUN'                                                
         LA    R5,6(R5)                                                         
*                                                                               
         MVC   H4(6),=C'CLIENT'                                                 
         MVC   H4+9(3),QCLT                                                     
         MVC   H4+13(20),CLTNM                                                  
*                                                                               
         CLI   BPRD,0              WAS THIS FILTERED ON PRODUCT                 
         BE    HDHK2                NO                                          
         MVC   H5(7),=C'PRODUCT'                                                
         MVC   H5+9(3),QPRD                                                     
         MVC   H5+13(20),PRDNM                                                  
*                                                                               
HDHK2    CLI   BPRD2,0             WAS THIS FILTERED ON PARTNER                 
         BE    HDHK4                                                            
         MVC   H6(7),=C'PARTNER'                                                
         MVC   H6+9(4),=C'NONE'                                                 
         CLI   BPRD2,X'FF'         PARTNER = NONE                               
         BE    HDHK4                                                            
         MVC   H6+9(3),QPRD2                                                    
         MVI   H6+12,C' '                                                       
         MVC   H6+13(20),PRD2NM                                                 
*                                                                               
HDHK4    CLI   SVPROF5,C'N'       TEST TO BLANK REPORT/QUESTOR                  
         BNE   HDHK10                                                           
         MVC   H2+76(34),SPACES                                                 
         MVC   H3+76(17),SPACES                                                 
*                                                                               
HDHK10   CLI   SVPROF9,C'Y'       TEST TO PRINT ORDER NUM                       
         BNE   HDHK14                                                           
*                                                                               
         MVC   H4+45(12),=CL24'ORDER NUMBER'                                    
         MVC   H4+58(2),AGENCY                                                  
         MVC   H4+60(1),QMED                                                    
         MVC   H4+61(3),QCLT                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),DUB                                            
         LA    R4,H4+63                                                         
         CLI   0(R4),C' '                                                       
         BH    *+6                                                              
         BCTR  R4,0                                                             
         MVC   1(4,R4),DUB+2       MOVE MMDD TO ORDER NUMBER                    
         LA    R1,H4+45            SET DEFAULT POSN FOR AGY COPY MSG            
         MVC   SVPONUM,SPACES                                                   
         MVC   SVPONUM(10),H4+58   SAVE PO NUMBER                               
         B     HDHK16                                                           
*                                                                               
* PRINT MANUAL PURCHASE ORDER IF ANY *                                          
*                                                                               
HDHK14   L     R3,ATWA                                                          
         USING T216FFD,R3                                                       
         LA    R1,H4+37            SET DEFAULT POSN FOR AGY COPY MSG            
         CLI   TRAPOH+5,0          ANY ENTRY                                    
         BE    HDHK16               NO                                          
         MVC   H4+45(25),SPACES                                                 
         MVC   H4+37(12),=CL12'ORDER NUMBER'                                    
         LLC   R1,TRAPOH+5                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   H4+50(0),TRAPO                                                   
         DROP  R3                                                               
         GOTO1 CENTER,DMCB,H4+37,37                                             
*                                                                               
         LA    R1,H4+37                                                         
         CLI   0(R1),C' '          FIND FIRST CHAR                              
         BH    HDHK16                                                           
         LA    R1,1(R1)                                                         
         B     *-12                                                             
*                                                                               
HDHK16   TM    SVOPTSW1,OPTHDCPY   FOLLOW UP AGENCY COPY                        
         BO    HDHK18                                                           
         CLI   SVTWPR05,C'2'       SEE IF AGENCY COPY                           
         BNE   HDHK20                                                           
         TM    WHEN,X'20'          IF SOON MUST BE FAXING, BYPASS               
         BO    *+10                                                             
HDHK18   MVC   132(19,R1),=C'*** AGENCY COPY ***'                               
                                                                                
*                                                                               
HDHK20   MVC   H8(2),=C'TO'                                                     
         MVC   H8+9(24),PRDHSEL1                                                
         MVC   H9+9(24),PRDHSEL2                                                
         MVC   H10+9(24),PRDHSEL3                                               
         MVC   H11+9(24),PRDHSEL4                                               
*                                                                               
         CLI   PRDHFAX,C' '                                                     
         BNH   HDHK22                                                           
         MVC   H12+9(3),=C'FAX'                                                 
         MVC   H12+13(13),PRDHFAX                                               
         OC    PRDHFAX+14(5),PRDHFAX+14   SEE IF I HAVE EXTENSION               
         BZ    HDHK22                                                           
         MVC   H12+27(3),=C'EXT'                                                
         MVC   H12+31(5),PRDHFAX+14                                             
*                                                                               
HDHK22   MVC   H8+51(9),=C'ISSUED BY'                                           
         MVC   H8+62(24),QUESTOR                                                
*                                                                               
         OC    CONEMAIL,SPACES                                                  
         MVC   H9+51(50),CONEMAIL                                               
*                                                                               
         MVC   H10+51(9),=C'TELEPHONE'                                          
         MVC   H10+62(12),CONTEL                                                
         OC    CONTEL+13(5),CONTEL+13   ANY EXTENSION                           
         BZ    HDHK24                    NO                                     
         MVC   H10+75(3),=C'EXT'                                                
         MVC   H10+79(5),CONTEL+13                                              
*                                                                               
HDHK24   OC    CONFAX(18),CONFAX        ANY FAX                                 
         BZ    HDHK30                    NO                                     
         MVC   H11+51(3),=C'FAX'                                                
         MVC   H11+62(12),CONFAX                                                
         OC    CONFAX+13(5),CONFAX+13   ANY EXTENSION                           
         BZ    HDHK30                    NO                                     
         MVC   H11+75(3),=C'EXT'                                                
         MVC   H11+79(5),CONFAX+13                                              
*                                                                               
HDHK30   CLI   RCSUBPRG,3                                                       
         BE    HDHK40                                                           
*                                                                               
         MVC   H1+40(16),=CL16'SPOT RADIO'                                      
         CLI   QMED,C'R'                                                        
         BE    HDHK35                                                           
         MVC   H1+40(16),=CL16'SPOT TELEVISION'                                 
         CLI   QMED,C'T'                                                        
         BE    HDHK35                                                           
         MVC   H1+40(16),=CL16'NETWORK RADIO'                                   
         CLI   QMED,C'X'                                                        
         BE    HDHK35                                                           
         MVC   H1+40(18),=CL18'NETWORK TELEVISION'                              
         CLI   QMED,C'N'                                                        
         BE    HDHK35                                                           
         DC    H'0'                                                             
*                                                                               
HDHK35   MVC   H1+59(14),=C'SHIPPING ORDER'                                     
         GOTO1 SQUASHER,DMCB,H1+40,33                                           
         GOTO1 CENTER,(R1),,33                                                  
         GOTO1 UNDERLIN,(R1),(33,H1+40),H2+40                                   
*                                                                               
HDHK40   CLI   SVOPTCLS,0           TEST HAVE CMML CLASS FILTER                 
         BE    HDHK50               NO                                          
         MVC   H5+77(6),=C'CLASS ='                                             
         MVC   H5+84(1),SVOPTCLS                                                
*                                                                               
HDHK50   CLI   RCSUBPRG,3          SHIP SUMMARY                                 
         BE    HDHK60               YES, BYPASS                                 
*                                                                               
         OC    SVSTAFF,SVSTAFF     WAS THERE AN AFFILIATE                       
         BZ    HDHK52                                                           
*                                                                               
         CLI   SVSTAFF,0           IS THIS AMS GROUP CODE                       
         BE    HDHK60                                                           
*                                                                               
HDHK52   CLI   SVTSPR02,C'Y'       PAGE BREAK BY STATION AFFILIATE              
         BNE   HDHK60               NO                                          
*                                                                               
         MVC   H6+77(11),=C'AFFILIATE ='                                        
         MVC   H6+90(3),SVSTAFF                                                 
         CLC   SVSTAFF,SPACES                                                   
         BH    HDHK60                                                           
         MVC   H6+90(4),=C'NONE'                                                
*                                                                               
HDHK60   CLC   PAGE,=H'1'                                                       
         BE    *+10                                                             
         MVC   H12+76(21),=C'***** CONTINUED *****'                             
*                                                                               
         CLI   SVTSPR04,C'Y'       SUPPRESS COMMERCIAL TYPE                     
         BNE   HDHK64                                                           
         CLI   RCSUBPRG,1                                                       
         BE    *+12                                                             
         CLI   RCSUBPRG,3                                                       
         BNE   HDHK64                                                           
*                                                                               
         MVC   MID1+PTYPE-P(4),SPACES                                           
         MVC   MID2+PTYPE-P(4),SPACES                                           
*                                                                               
HDHK64   CLI   RCSUBPRG,1                                                       
         BE    *+12                                                             
         CLI   RCSUBPRG,3                                                       
         BNE   HDHK90                                                           
*                                                                               
         CLI   SVTSPR05,C'Y'       SUPPRESS COMMERCIAL COUNTS                   
         BNE   HDHK90                                                           
*                                                                               
         MVC   MID1+98(6),SPACES                                                
         MVC   MID2+98(6),SPACES                                                
                                                                                
*===============================================================                
* IF FAXING, ON FIRST PAGE OF ALL REPORTS BUT THE VERY FIRST,                   
* NEED A /PAGE IN H1                                                            
* FOR THE VERY FIRST PAGE, MAXLINES=40.                                         
* DO THIS BY MOVING ALL HEADLINES DOWN 1 LINE                                   
*===============================================================                
                                                                                
HDHK90   B     HDHK100                                                          
*                                                                               
*** BYPASS THIS CODE EDIWIDE WILL TAKE CARE OF /PAGE                            
*                                                                               
         CLI   SVTWPR05,C'N'       SEE IF FAXING                                
         BE    HDHK100             NO                                           
         CLC   PAGE,=H'1'          TEST PAGE 1                                  
         BNE   HDHK100                                                          
         CLI   MAXLINES,40         WILL ONLY BE 40 FOR FIRST PAGE               
         BE    HDHK100             OF FIRST INST                                
*                                                                               
         LA    R1,H13              MOVE HEADLINES DOWN 1                        
         LA    R0,13                                                            
         MVC   132(132,R1),0(R1)                                                
         SHI   R1,132                                                           
         BCT   R0,*-10                                                          
*                                                                               
         MVC   H1,SPACES                                                        
         MVC   H1(5),=C'/PAGE'                                                  
         MVI   MAXLINES,FAXMAX     RESET MAXLINES NOW!                          
*                                                                               
*** END OF BYPASSED CODE                                                        
*                                                                               
HDHK100  CLC   PAGE,=H'1'                                                       
         BNE   HDHKX                                                            
         CLI   RCSUBPRG,1                                                       
         BH    HDHKX                                                            
         MVI   HEADHOOK,C'R'       SET RETURN TO PRINT COMMENT                  
*                                                                               
HDHKX    XIT1                                                                   
         EJECT                                                                  
*======================================================                         
* FORMAT COMMENT FROM HOUSE RECORD IN AIO1                                      
*======================================================                         
                                                                                
HDHKCMT  MVI   HEADHOOK,0          CLEAR RETURN CALL FLAG                       
*                                                                               
         LA    R0,14               CLEAR THE HEADLINES                          
         LA    R1,H1                                                            
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         LA    R4,H1               SET FIRST OUTPUT POSN                        
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         BNE   HDHKCM30                                                         
*                                                                               
         CLC   =C'BOX=',3(R6)                                                   
         BNE   HDHKCM20                                                         
         LA    RF,BOXER                                                         
         GOTO1 (RF),DMCB,(60,AIO1)                                              
*                                                                               
         L     RE,AIO1                                                          
*                                                                               
HDHKCM10 MVC   0(60,R4),0(RE)                                                   
         LA    R4,132(R4)                                                       
         LA    RE,60(RE)                                                        
         CLI   0(RE),0                                                          
         BNE   HDHKCM10                                                         
         MVI   0(R4),0                                                          
         B     HDHKCM30                                                         
                                                                                
*=============================================                                  
* FORMAT UNBOXED COMMENT                                                        
*=============================================                                  
                                                                                
HDHKCM20 LLC   RE,1(R6)                                                         
         AHI   RE,-4                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),3(R6) *EXECUTED*                                         
         LA    R4,132(R4)                                                       
         BRAS  RE,NEXTEL                                                        
         BE    HDHKCM20                                                         
                                                                                
*===========================================================                    
* PRINT COMMENTS FROM SCREEN IF ANY                                             
*===========================================================                    
                                                                                
HDHKCM30 L     R3,ATWA              I MUST RELOAD R3                            
         USING T216FFD,R3                                                       
         LA    R2,TRACMT1H                                                      
         LA    R5,4                                                             
         LR    RE,R2                                                            
         LR    RF,R5                                                            
         DROP  R3                                                               
*                                                                               
HDHKCM40 CLI   5(RE),0             ANY DATA                                     
         BNE   HDHCM50              YES                                         
         LLC   R0,0(RE)                                                         
         AR    RE,R0                                                            
         IC    R0,0(RE)                                                         
         AR    RE,R0                                                            
         BCT   RF,HDHKCM40                                                      
         B     HDHKX               NO ENTRIES, BYPASS                           
*                                                                               
HDHCM50  CLI   5(R2),0             ANY DATA                                     
         BE    HDHKCM62             NO                                          
         LLC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,HDHKMVC                                                       
*                                                                               
HDHKCM60 LA    R4,132(R4)         NEXT PRINT LINE                               
*                                                                               
HDHKCM62 LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   R5,HDHCM50                                                       
         B     HDHKX                                                            
HDHKMVC  MVC   0(0,R4),8(R2)                                                    
*                                                                               
FAXMAX   EQU   42                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================                          
* SUBROUTINE TO PRINT BOXES AROUND TEXT                                         
* AIO MUST HAVE RECORD ADDRESS                                                  
* ELCODDE MUST CONTAIN COMMENT ELEM CODE                                        
* P1  (1) = MAXIMUM LEN OF EXPANDED COMMENT                                     
* P1+1(3) = EXPANDED COMMENT OUTPUT AREA                                        
*=====================================================                          
                                                                                
BOXER    NTR1                                                                   
*                                                                               
* FIRST - FIND LENGTH OF LONGEST COMMENT *                                      
*                                                                               
         L     R7,0(R1)            GET OUTPUT AREA ADDRESS                      
         LLC   R4,0(R1)            GET OUTPUT RECORD SIZE                       
*                                                                               
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R5,R5                                                            
*                                                                               
BOX2     LLC   RE,1(R6)                                                         
         CLC   =C'BOX=',3(R6)                                                   
         BNE   *+8                                                              
         AHI   RE,-4                                                            
         CR    R5,RE                                                            
         BH    *+6                                                              
         LR    R5,RE                                                            
         BRAS  RE,NEXTEL                                                        
         BE    BOX2                                                             
*                                                                               
* LENGTH IN R5 INCLUDES 3 FOR ELCODE/LEN/SEQ - NOW ADJUST FOR                   
* '*/SP' AND 'SP/*' AT EITHER END                                               
*                                                                               
         LA    R5,1(R5)                                                         
*                                                                               
* CREATE ROW OF *'S THIS LENGTH                                                 
*                                                                               
         EX    R4,BOXSPC                                                        
         MVI   0(R7),C'*'                                                       
         BCTR  R5,0                ADJUST FOR FIRST *                           
         BCTR  R5,0                SET FOR EX                                   
         EX    R5,BOXR7                                                         
         AR    R7,R4               POINT TO NEXT OUTPUT LINE                    
         LA    R5,2(R5)            RESTORE LENGTH                               
*                                                                               
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
BOX4     EX    R4,BOXSPC                                                        
*                                                                               
         LLC   RE,1(R6)                                                         
         LA    RF,3(R6)                                                         
*                                                                               
         CLC   =C'BOX=',0(RF)                                                   
         BNE   *+12                                                             
         AHI   RE,-4                                                            
         LA    RF,4(RF)                                                         
*                                                                               
         MVI   0(R7),C'*'                                                       
         AHI   RE,-4               SET FOR EX                                   
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   2(0,R7),0(RF) *EXECUTED*                                         
*                                                                               
         LA    RE,0(R7,R5)         POINT TO END OF LINE                         
         BCTR  RE,0                BACK UP                                      
         MVI   0(RE),C'*'                                                       
         AR    R7,R4               POINT TO NEXT LINE                           
         BRAS  RE,NEXTEL                                                        
         BE    BOX4                                                             
*                                                                               
         EX    R4,BOXSPC                                                        
         MVI   0(R7),C'*'                                                       
         BCTR  R5,0                ADJUST FOR FIRST *                           
         BCTR  R5,0                SET FOR EX                                   
         EX    R5,BOXR7                                                         
         AR    R7,R4                                                            
         MVI   0(R7),0             SET END OF BUFFER FLAG                       
         J     EXIT                                                             
*                                                                               
BOXR7    MVC   1(0,R7),0(R7)  *EXECUTED*                                        
BOXSPC   MVC   0(0,R7),SPACES *EXECUTED*                                        
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*==================================================                             
* BUILD TSAR RECORDS FOR ELECTRONIC SHIP                                        
*==================================================                             
*                                                                               
BLDESHP  NMOD1 0,*BLDSHP*,(R7)                                                  
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         L     R3,ACOMSUB          COMMON AREA                                  
         USING COMSUBS,R3                                                       
         LA    R5,BREC                                                          
         USING BLDSHPD,R5                                                       
         LAY   R2,TSARBLK                                                       
         USING TSARD,R2                                                         
*                                                                               
         MVC   ESPARMS(24),0(R1)   AND PARMS                                    
         MVC   ESMODE,0(R1)        MODE IS HOB OF PARM1                         
*                                                                               
         CLI   FRSTSW,0                                                         
         BNE   MP00                                                             
         BRAS  RE,INITSAR          INIT TSAR FOR ESHP                           
         BRAS  RE,BINTBLE          ALLOCATE BINSRCH TABLES                      
*                                                                               
MP00     CLI   OPENFSW,0                                                        
         BNE   MP02                                                             
         OPEN  (RANKWK,OUTPUT)                                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   OPENFSW,1           FILE IS OPEN                                 
*                                                                               
* FIND MODE                                                                     
MP02     LA    RF,MODLIST                                                       
         LA    R0,MODLISTN                                                      
*                                                                               
MP04     DS    0H                                                               
         CLC   ESMODE,0(RF)                                                     
         BE    MP04D                                                            
         LA    RF,5(RF)                                                         
         BCT   R0,MP04                                                          
         DC    H'0'                                                             
*                                                                               
MP04D    DS    0H                                                               
         ICM   RF,15,1(RF)                                                      
         BASR  RE,RF                                                            
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
**********************************************************************          
*  ADD HEADER RECORD TO TSAR  (HEDR)                                            
**********************************************************************          
*                                                                               
ESAHD    NTR1                                                                   
*                                                                               
         TM    DONESW,PARTSW       PARTIAL RECORD                               
         BO    ESAHD10              YES, CONTINUE WITH THE REST                 
*                                                                               
         TM    DONESW,HEADSW       DID HEADER RECORD ?                          
         BO    ESAHDX                                                           
*                                                                               
         OI    DONESW,PARTSW       PARTIAL RECORD                               
*                                                                               
         XC    BLKEY,BLKEY                                                      
         MVC   BLREC(2),=Y(BLHEDLN) RECORD LENGTH                               
         MVI   BLKRCLS,ESHEADQ     HEADER SORT SEQ #                            
         MVI   BLKRCD,ESHEDQ       RECORD TYPE (HEADER)                         
*                                                                               
         L     RF,ESPARMS                                                       
         MVC   BLHAGY,0(RF)        AGENCY ALPHA                                 
*                                                                               
         L     RF,ESPARMS+4                                                     
         MVC   BLHPID,0(RF)        PID                                          
*                                                                               
         L     RF,ESPARMS+8                                                     
         MVC   BLHFNME,0(RF)       FIRST NAME                                   
*                                                                               
         L     RF,ESPARMS+12                                                    
         MVC   BLHLNME,0(RF)       LAST NAME                                    
*                                                                               
         L     RF,ESPARMS+16                                                    
         MVC   BLHEMAIL,0(RF)      USER EMAIL ADDRESS                           
*                                                                               
         L     RF,ESPARMS+20                                                    
         MVC   BLHDLV,0(RF)        DELIVERY SYSTEM (CAD)                        
         B     ESAHDX                                                           
*                                                                               
ESAHD10  L     RF,ESPARMS                                                       
         MVC   BLHSMED,0(RF)        SUB-MEDIA (T)V, (R)ADIO                     
*                                                                               
         L     RF,ESPARMS+4                                                     
         MVC   BLHDELAY,0(RF)       ORDER DELAY                                 
*                                                                               
         L     RF,ESPARMS+8                                                     
         MVC   BLHENV,0(RF)         ENVIRONMENT                                 
*                                                                               
         OI    DONESW,HEADSW        DONE HEADER RECORD                          
         NI    DONESW,X'FF'-PARTSW  TURN OFF PARTIAL RECORD                     
*                                                                               
         BAS   RE,ESPUT                                                         
ESAHDX   J     EXIT1                                                            
*                                                                               
**********************************************************************          
*         ADD AGENCY RECORD TO TSAR                                             
**********************************************************************          
*                                                                               
ESAAG    NTR1                                                                   
         TM    DONESW,AGYSW        DID AGENCY RECORD ?                          
         BO    ESAAGX                                                           
         OI    DONESW,AGYSW                                                     
*                                                                               
         XC    BLKEY,BLKEY                                                      
         MVC   BLREC(2),=Y(BLAGYLN) RECORD LENGTH                               
         MVI   BLKRCLS,ESAGYQ      AGENCY SORT SEQ#                             
         MVI   BLKRCD,ESAGQ        RECORD TYPE (AGENCY)                         
*                                                                               
         L     RF,ESPARMS                                                       
         MVC   BLAUID,0(RF)        USER ID                                      
*                                                                               
         L     RF,ESPARMS+4                                                     
         MVC   BLANAME,0(RF)       USER NAME                                    
*                                                                               
         BAS   RE,ESPUT                                                         
ESAAGX   J     EXIT1                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
**********************************************************************          
*         ADD ADVERTISER RECORD TO TSAR (CLT CODE/NAME)                         
**********************************************************************          
*                                                                               
ESADV    NTR1                                                                   
         TM    DONESW,ADVSW        DID ADVERTISER RECORD ?                      
         BO    ESADVX                                                           
         OI    DONESW,ADVSW                                                     
*                                                                               
         XC    BLKEY,BLKEY                                                      
         MVC   BLREC(2),=Y(BLADRLN) RECORD LENGTH                               
         MVI   BLKRCLS,ESADVQ      ADV REC SORT                                 
         MVI   BLKRCD,ESADQ        RECORD TYPE (ADVERTISER)                     
*                                                                               
         L     RF,ESPARMS                                                       
         MVC   BLADCDE,0(RF)       CLT CODE                                     
*                                                                               
         L     RF,ESPARMS+4                                                     
         MVC   BLADNME,0(RF)       CLT NAME                                     
*                                                                               
         BAS   RE,ESPUT                                                         
ESADVX   J     EXIT1                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
**********************************************************************          
*        PRODUCT (BRAND) RECORD                                                 
**********************************************************************          
*                                                                               
ESAPR    NTR1                                                                   
         XC    BLKEY,BLKEY                                                      
         MVC   BLREC(2),=Y(BLPRLN) RECORD LENGTH                                
         MVI   BLKRCLS,ESPRODQ     PRODUCT SORT GROUP                           
         MVI   BLKRCD,ESPRDQ       RECORD TYPE (PRODUCT DEFINITION)             
*                                                                               
         L     RF,ESPARMS                                                       
         MVC   BLPRCDE,0(RF)       PRD CODE                                     
         L     RF,ESPARMS+4                                                     
         MVC   BLPRNME,0(RF)       PRD NAME                                     
         L     RF,ESPARMS+8                                                     
         MVC   BLPRTAL,0(RF)       TALENT                                       
*                                                                               
         BAS   RE,ESPUT                                                         
ESAPRX   J     EXIT1                                                            
*                                                                               
*                                                                               
**********************************************************************          
*        COMMERICAL/PRODUCT RECORD                                              
**********************************************************************          
*                                                                               
ESACP    NTR1                                                                   
         XC    BLKEY,BLKEY                                                      
         MVC   BLREC(2),=Y(BLPRLN) RECORD LENGTH                                
         MVI   BLKRCLS,ESCMPRQ     CML/PRD SORT GROUP                           
         MVI   BLKRCD,ESCPRQ       RECORD TYPE (CML/PRD)                        
*                                                                               
         L     RF,ESPARMS                                                       
         MVC   BLADID,0(RF)        CML                                          
*                                                                               
         L     RF,ESPARMS+4                                                     
         MVC   BLADPRD,0(RF)       PRD                                          
*                                                                               
         BAS   RE,ESPUT                                                         
         J     EXIT1                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
**********************************************************************          
*        ADD SPOT/COMMERCIAL RECORD TO TSAR (SPOT)                              
**********************************************************************          
*                                                                               
ESACL    NTR1                                                                   
         TM    DONESW,PARTSW       PARTIAL RECORD                               
         BO    ESACL10              YES, CONTINUE WITH THE REST                 
*                                                                               
         OI    DONESW,PARTSW       PARTIAL RECORD                               
*                                                                               
         XC    BLKEY,BLKEY                                                      
         MVC   BLREC(2),=Y(BLCMRLN) RECORD LENGTH                               
         MVI   BLKRCLS,ESCMMLQ     COMML SORT SEQ#                              
         MVI   BLKRCD,ESCMLQ       RECORD TYPE (COMML DEFINITION)               
*                                                                               
         L     RF,ESPARMS                                                       
         MVC   BLCML,0(RF)         COMMERCIAL                                   
*                                                                               
         L     RF,ESPARMS+4                                                     
         MVC   BLCMDSC,0(RF)       DESCRIPTION                                  
*                                                                               
         L     RF,ESPARMS+8                                                     
         MVC   BLCMDSC2,0(RF)      DESCRIPTION 2                                
*                                                                               
         L     RF,ESPARMS+12                                                    
         MVC   BLCMDSC3,0(RF)      DESCRIPTION 3                                
*                                                                               
         L     RF,ESPARMS+16       SLN                                          
         MVC   BLCMLEN,0(RF)                                                    
         B     ESACLX                                                           
*                                                                               
ESACL10  L     RF,ESPARMS                                                       
         MVC   BLCMREL,0(RF)       RELEASE DATE                                 
*                                                                               
         L     RF,ESPARMS+4                                                     
         MVC   BLCMRCL,0(RF)       RECALL DATE                                  
*                                                                               
         L     RF,ESPARMS+8                                                     
         MVC   BLCMPO,0(RF)        PO NUMBER                                    
*                                                                               
         L     RF,ESPARMS+12                                                    
         MVC   BLCMLHD,0(RF)       HIDEF                                        
*                                                                               
         L     RF,ESPARMS+16                                                    
         MVC   BLCMLCT,0(RF)       CNTRCT                                       
*                                                                               
         NI    DONESW,X'FF'-PARTSW TURN OFF PARTIAL RECORD                      
*                                                                               
         BAS   RE,ESPUT                                                         
ESACLX   J     EXIT1                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
**********************************************************************          
*        ADD ORDER RECORD TO TSAR                                               
**********************************************************************          
*                                                                               
ESAOR    NTR1                                                                   
         TM    DONESW,ORDSW        DID ORDER RECORD ?                           
         BO    ESAORX                                                           
         OI    DONESW,ORDSW                                                     
*                                                                               
         XC    BLKEY,BLKEY                                                      
         MVC   BLREC(2),=Y(BLORRLN) RECORD LENGTH                               
         MVI   BLKRCLS,ESORDRQ     ORDER SORT GROUP                             
         MVI   BLKRCD,ESORDQ       RECORD TYPE (ORDER)                          
*                                                                               
         L     RF,ESPARMS                                                       
         MVC   BLORPRI,0(RF)       PRIORITY (1, 2 OR 3)                         
*                                                                               
         L     RF,ESPARMS+4                                                     
         MVC   BLORPO,0(RF)        PURCHASE ORDER NUMBER (PO)                   
*                                                                               
         L     RF,ESPARMS+8                                                     
         MVC   BLORCMT,0(RF)       COMMENTS 1-4                                 
*                                                                               
         L     RF,ESPARMS+12                                                    
         MVC   BLORPSR,0(RF)       PROD SERVICES REQUIRED                       
*                                                                               
         L     RF,ESPARMS+16                                                    
         MVC   BLORPRD,0(RF)       PRODUCT CODE                                 
*                                                                               
         BAS   RE,ESPUT                                                         
ESAORX   J     EXIT1                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
**********************************************************************          
*        ADD ORDER ITEM RECORD TO TSAR (ORIT)                                   
**********************************************************************          
*                                                                               
ESAOI    NTR1                                                                   
         XC    BLKEY,BLKEY                                                      
         MVC   BLREC(2),=Y(BLOIRLN) RECORD LENGTH                               
         MVI   BLKRCLS,ESORITQ      ORDER ITEMT SEQ#                            
         MVC   BLKSCLS,SUBCNTR      AND SUB-CLASS                               
         MVI   BLKRCD,ESOITQ       RECORD TYPE (ORDER ITEM)                     
*                                                                               
         L     RF,ESPARMS                                                       
         MVC   BLOIADID,0(RF)      CML                                          
*                                                                               
         L     RF,ESPARMS+4                                                     
         MVC   BLOISTA,0(RF)       STATION CALL LETTERS (WABC)                  
*                                                                               
         L     RF,ESPARMS+8                                                     
         MVC   BLOTFTD,0(RF)       FTD (YMD)                                    
*                                                                               
         L     RF,ESPARMS+12       SHIP TYPE                                    
         MVC   BLOTTYP,0(RF)       (R)EGULAR,(F)ORCED/RERUN                     
*                                                                               
         BAS   RE,ESPUT                                                         
         J     EXIT1                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*****************************************************************               
*        ESPUT - RECORD TO TSAR                                                 
*****************************************************************               
*                                                                               
ESPUT    NTR1                                                                   
         MVC   BLKSEQ,SEQNO        SET SEQUENCE NUMBER                          
         LH    RF,SEQNO            AND BUMP                                     
         LA    RF,1(RF)                                                         
         STH   RF,SEQNO                                                         
*                                                                               
         LA    R1,BREC                                                          
         ST    R1,TSAREC                                                        
*                                                                               
         LAY   R2,TSARBLK                                                       
         USING TSARD,R2                                                         
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+12                                                             
         MVI   TSOFFACT,TSAADD                                                  
         B     *+8                                                              
         MVI   TSACTN,TSAADD                                                    
*                                                                               
         GOTO1 ATSAR,(R2)                                                       
*                                                                               
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,CLRBREC          SET BUFFER REC TO SPACES                     
         J     EXIT1                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
SEQNO    DC    H'0001'                                                          
*                                                                               
******************************************************************              
*        CLEAR RECORD AREAS                                                     
******************************************************************              
*                                                                               
CLRBREC  NTR1                                                                   
         LA    R0,BREC             SET BUFFER REC TO SPACES                     
         LA    R1,L'BREC                                                        
         SR    RE,RE                                                            
         IC    RF,=X'40'                                                        
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
         J     EXIT1                                                            
*                                                                               
*SM                                                                             
**********************************************************************          
*        INITIALIZATION TSAR FOR ELECTRONIC SHIP INSTRUCTIONS                   
**********************************************************************          
*                                                                               
INITSAR  NTR1  BASE=*,LABEL=*                                                   
         MVI   FRSTSW,1                                                         
*                                                                               
         XC    ATSAR,ATSAR                                                      
         XC    TSARBYTE,TSARBYTE                                                
*                                                                               
         MVI   SUBCNTR+1,1         INIT CML/STA TSAR SUB-CLASS                  
*                                                                               
         LAY   RF,FREC                                                          
         ST    RF,AFREC                                                         
         LA    RF,4(RF)                                                         
         ST    RF,AFRECP4          REC+4 IS START OF DATA                       
         L     RF,AFREC                                                         
         AHI   RF,L'FREC                                                        
         ST    RF,AFRECX                                                        
*                                                                               
         LAY   RE,TSARBLK                                                       
         LR    R2,RE                                                            
         LA    RF,TSARDL2                                                       
         XCEF                                                                   
*                                                                               
         USING TSARD,R2                                                         
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSKEYL,BLKLN        KEY LENGTH                                   
         MVC   TSRECL,=Y(BLRECL)   RECORD LENGTH                                
         OI    TSRECI,TSRVAR       SET FOR VARIABLE RECS                        
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   INIT10                                                           
*                                                                               
* GET STORAGE FOR BINSRCH TABLES FOR OPTICA                                     
         SAM31                                                                  
         L     R0,=A(BUFFSIZ)       BUFFER SIZE                                 
         GETMAIN RC,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,ABTBLES          A(STORAGE FOR BIN TABLES)                    
                                                                                
         AR    R1,R0                                                            
         ST    R1,ABTBLESX         END OF TABLES AREA                           
         SAM24                                                                  
*                                                                               
         L     R0,=A(BLSIZE)       BUFFER SIZE                                  
         ST    R0,TSARBUFL                                                      
*                                                                               
         GETMAIN RC,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,TSARBUFF                                                      
*                                                                               
         MVC   TSABUF,TSARBUFF                                                  
         MVC   TSAREC,=A(BLSIZE)   BUFFER SIZE                                  
*                                                                               
* TSAROFF CALLOV HERE                                                           
*                                                                               
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A7D')                                 
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ATSAR                                                         
*                                                                               
         MVI   TSOFFACT,TSAINI      SET 1ST TSAR FOR INIT                       
         B     INIT20                                                           
*                                                                               
* TSAR CALLOV HERE                                                              
*                                                                               
INIT10   GOTO1 CALLOV,DMCB,0,(C'R',X'00000A5D')                                 
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ATSAR                                                         
*                                                                               
         MVI   TSPAGN,20            NUMBER OF TEMPSTR PAGES TO BE USED          
         OI    TSINDS,TSINODSK                                                  
         OI    TSIND2,TSI2MANY+TSI2BIGN   USE BOTH BUFFERS                      
         MVI   TSACTN,TSAINI         SET 1ST TSAR FOR INIT                      
         L     R0,=A(BLSIZE)       BUFFER SIZE                                  
         ST    R0,TSARBUFL                                                      
*                                                                               
INIT20   GOTO1 ATSAR,(R2)                                                       
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         J     EXIT                                                             
*                                                                               
         DROP  R2                                                               
*                                                                               
BUFFSIZ  EQU   300000              BUFFER SIZE                                  
*                                                                               
*SM **ESHIP                                                                     
*---------------------------------                                              
* ALLOCATE BINSRCH TABLES FOR XML                                               
*---------------------------------                                              
BINTBLE  NTR1  BASE=*,LABEL=*                                                   
         MVI   FCOUNT+1,1          SET FLAT FILE REC COUNT TO 1                 
* XML TABLES                                                                    
         SAM31                                                                  
         L     RF,ABTBLES                                                       
         MVC   0(8,RF),=C'**PROD**'    XML PRD BINTABLE                         
         LA    RF,8(RF)                                                         
         ST    RF,APROD                                                         
         LHI   RE,NPROD*XPRDNXT        ROOM FOR 255 PRDS                        
         AR    RF,RE                                                            
         ST    RF,MXPROD                                                        
*                                                                               
         MVC   0(8,RF),=C'*COMMLT*'    XML CML BINTABLE                         
         LA    RF,8(RF)                                                         
         ST    RF,ACOMMLT                                                       
         SR    R0,R0                                                            
         LA    R1,NCOMMLT              3000 CMLS                                
         LA    RE,XCOMNXT              ENTRY LENGTH                             
         MR    R0,RE                                                            
         AR    RF,R1                                                            
         ST    RF,MXCOMMLT                                                      
*                                                                               
         MVC   0(8,RF),=C'*CMLPRD*'    XML CML/PRD BINTABLE                     
         LA    RF,8(RF)                                                         
         ST    RF,ACMLPRD                                                       
         SR    R0,R0                                                            
         LA    R1,NCMLPRD              3000 CML/PRD                             
         LA    RE,XCPRNXT                                                       
         MR    R0,RE                                                            
         AR    RF,R1                                                            
         ST    RF,MXCMPRD                                                       
*                                      STATION                                  
         MVC   0(8,RF),=C'**DEST**'    XML DESTINATION BINTABLE                 
         LA    RF,8(RF)                                                         
         ST    RF,ADEST                                                         
         SR    R0,R0                                                            
         LA    R1,NDEST                ROOM FOR 3000 DESTINATIONS               
         LA    RE,XDESTNXT                                                      
         MR    R0,RE                                                            
         AR    RF,R1                                                            
         ST    RF,MXDEST                                                        
*                                                                               
         C     RF,ABTBLESX                                                      
         BNH   *+6                                                              
         DC    H'0'                 NEED BIGGER STORAGE                         
*                                                                               
         BAS   RE,IXTBL                                                         
                                                                                
         J     EXIT                                                             
*SM  **ESHIP                                                                    
*                                                                               
*===========================================================                    
* INITIALIZE BINTABLES FOR XML                                                  
*===========================================================                    
                                                                                
IXTBL    NTR1                                                                   
         SAM31                                                                  
         L     RE,APROD           CLEAR PROD BINTABLE                           
         L     RF,MXPROD          END OF TABLE                                  
         SR    RF,RE                                                            
         XCEFL                                                                  
*                                                                               
         L     RE,ACOMMLT         CLEAR COMML BINTABLE                          
         L     RF,MXCOMMLT        END OF TABLE                                  
         SR    RF,RE                                                            
         XCEFL                                                                  
*                                                                               
         L     RE,ACMLPRD         CLEAR COMML/PRD  BINTABLE                     
         L     RF,MXCMPRD         END OF TABLE                                  
         SR    RF,RE                                                            
         XCEFL                                                                  
*                                                                               
         L     RE,ADEST           CLEAR DESTINATION BINTABLE (STATION)          
         L     RF,MXDEST          END OF TABLE                                  
         SR    RF,RE                                                            
         XCEFL                                                                  
*                                                                               
         SR    R0,R0               ADDRESS OF REC TO BE ADDED                   
         L     R1,APROD            ADDRESS OF PRD BINTABLE                      
         SR    R2,R2               NUMBER OF RECS IN TABLE                      
         LA    R3,XPRDNXT          LENGTH OF BIN RECORD                         
         LA    R4,L'XPROD          LENGTH OF BIN KEY                            
         LA    R5,NPROD           MAXIMUM RECORDS IN BINTBL                     
         STM   R0,R5,BINPAR1                                                    
*                                                                               
         SR    R0,R0               ADDRESS OF REC TO BE ADDED                   
         L     R1,ACOMMLT          ADDRESS OF CML BINTABLE                      
         SR    R2,R2               NUMBER OF RECS IN TABLE                      
         LA    R3,XCOMNXT          LENGTH OF BIN RECORD                         
         LA    R4,XCOMNXT          LENGTH OF BIN KEY                            
         LA    R5,NCOMMLT         MAXIMUM RECORDS IN BINTBL                     
         STM   R0,R5,BINPAR2                                                    
*                                                                               
         SR    R0,R0               ADDRESS OF REC TO BE ADDED                   
         L     R1,ACMLPRD          ADDRESS OF CML/PRD BINTABLE                  
         SR    R2,R2               NUMBER OF RECS IN TABLE                      
         LA    R3,XCPRNXT          LENGTH OF BIN RECORD                         
         LA    R4,L'XCMLPRD        LENGTH OF BIN KEY                            
         LA    R5,NCMLPRD         MAXIMUM RECORDS IN BINTBL                     
         STM   R0,R5,BINPAR3                                                    
*                                                                               
         SR    R0,R0               ADDRESS OF REC TO BE ADDED                   
         L     R1,ADEST            ADDRESS OF DESTINATION BINTABLE              
         SR    R2,R2               NUMBER OF RECS IN TABLE                      
         LA    R3,XDESTNXT         LENGTH OF BIN RECORD                         
         LA    R4,L'XDESCML        LENGTH OF BIN KEY                            
         LA    R5,NDEST           MAXIMUM RECORDS IN BINTBL                     
         STM   R0,R5,BINPAR4                                                    
*                                                                               
         J     EXIT                                                             
*SM                                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                  TSAR RECORD CODE TYPE EQUATES                                
ESHEDQ   EQU   10                  HEADER RECORD                                
ESAGQ    EQU   20                  AGENCY RECORD                                
ESADQ    EQU   30                  ADVERTISER                                   
ESPRDQ   EQU   40                  PRODUCT DEFINITION                           
ESCMLQ   EQU   50                  SPOT/COMMERCIAL RECORD                       
ESCPRQ   EQU   60                  CML/PRD RECORD                               
ESORDQ   EQU   70                  ORDER RECORD                                 
ESOITQ   EQU   80                  ORDER ITEM RECORD                            
*                                                                               
*                   SORT SEQUENCE GROUPS                                        
ESHEADQ  EQU   10                  HEADER RECORD                                
ESAGYQ   EQU   20                  AGENCY RECORD                                
ESADVQ   EQU   30                  ADVERTISER                                   
ESPRODQ  EQU   40                  PRODUCT (BRAND) DEFINITION                   
ESCMMLQ  EQU   50                  SPOT/COMMERCIAL RECORD                       
ESCMPRQ  EQU   60                  CML/PRD RECORD                               
ESORDRQ  EQU   70                  ORDER RECORD                                 
ESORITQ  EQU   80                  ORDER ITEM RECORD                            
*                                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
FDELIM   EQU   X'6A'               FIELD DELIMITER (º)                          
*                                                                               
*********************************************************                       
*  CREATE OUTPUT FILE FROM TSAR BUFFER                                          
*********************************************************                       
*                                                                               
MAKEFILE NMOD1 0,**MKF***,R7,RA                                                 
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         L     R3,ACOMSUB          COMMON AREA                                  
*                                                                               
         LAY   R2,TSARBLK                                                       
         USING TSARD,R2                                                         
*                                                                               
         LA    R5,BREC                                                          
         ST    R5,TSAREC                                                        
*                                                                               
         XC    BREC(BLKLN+2),BREC  CLEAR KEY PLUS LENGTH                        
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BNE   *+12                                                             
         MVI   TSOFFACT,TSARDH                                                  
         B     *+8                                                              
         MVI   TSACTN,TSARDH                                                    
         B     MKF4B                                                            
*                                                                               
MKF4     CLI   OFFLINE,C'Y'                                                     
         BNE   *+12                                                             
         MVI   TSOFFACT,TSANXT                                                  
         B     *+8                                                              
         MVI   TSACTN,TSANXT                                                    
*                                                                               
MKF4B    GOTO1 ATSAR,(R2)                                                       
         TM    TSERRS,TSEEOF                                                    
         BO    MKF20                                                            
         TM    TSERRS,X'FF'-TSERNF (RECORD NOT FOUND IS OK)                     
         BZ    *+6                                                              
         DC    H'0'                NO OTHER ERROR IS ACCEPTABLE                 
                                                                                
**************************                                                      
* CODE TO TRACE TSAR                                                            
**************************                                                      
*        L     RF,APATCH                                                        
*        TM    1(RF),X'80'         TRACE RECORDS RETURNED FROM TSAR?            
*        BZ    MKF4E                                                            
*        SR    R0,R0                                                            
*        ICM   R0,3,BREC           RECORD LENGTH                                
******   GOTO1 PRNTBL,DMCB,=C'TSAGET',BREC,C'DUMP',(R0),=C'1D'                  
                                                                                
MKF4E    LA    RF,MKFROUTS         FIND ROUTINE                                 
         LHI   R0,MKFRTSN                                                       
*                                                                               
MKF5     CLC   BREC+7(1),0(RF)                                                  
         BE    *+14                                                             
         LA    RF,5(RF)                                                         
         BCT   R0,MKF5                                                          
         DC    H'0'                                                             
*                                                                               
         ICM   RF,15,1(RF)                                                      
         BASR  RE,RF                                                            
         B     MKF4                NEXT TSAR RECORD                             
*                                                                               
MKF20    MVI   DONESW,0              CLEAR SWITCH                               
*                                                                               
         CLOSE RANKWK                                                           
         MVI   OPENFSW,0           FILE IS CLOSED                               
*                                                                               
         J     EXIT1                                                            
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*  ADD HEADER RECORD TO FLAT FILE                                               
**********************************************************************          
*                                                                               
MFHU     NTR1                                                                   
         MVC   ANXTPOS,AFRECP4     NEW RECORD                                   
         GOTO1 BLDREC,DMCB,(4,=C'HEDR')                                         
         GOTO1 BLDREC,DMCB,(2,=C'DS') LEGACY SYSTEM                             
*                                                                               
         GOTO1 BLDREC,DMCB,(L'BLHAGY,BLHAGY) AGENCY ALPHA                       
*                                                                               
         GOTO1 BLDREC,DMCB,(L'BLHENV,BLHENV) ENVIRONMENT (TST/CSC/PRD)          
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(23,TDATE)                                     
         GOTO1 BLDREC,DMCB,(L'TDATE,TDATE)  DATE YYYY-MM-DD                     
*                                                                               
         TIME  DEC                 R0=HHMMSSHH                                  
*                                                                               
         SRDL  R0,8                TENTH OF SECOND TO R1                        
         SLL   R0,4                R0=0HHMMSS0                                  
         LA    R2,X'0C'                                                         
         OR    R2,R0               R1=0HHMMSS+                                  
         ICM   R0,15,=PL4'060000'  R0=6.00 AM (0060000+)                        
         ST    R0,DUB                                                           
         ST    R2,FULL                                                          
         AP    DUB(4),FULL                                                      
*                                                                               
         CP    DUB(4),=P'240000'      PAST MIDNIGHT?                            
         BL    TIME02                                                           
         SP    DUB(4),=P'240000'      YES                                       
TIME02   L     R2,DUB                                                           
         SRL   R2,4                GET RID OF SIGN                              
         LR    R0,R2                                                            
         SLDL  R0,8                PUT BACK TENTH OF SECOND                     
*                                                                               
         STC   R0,TIME+10                                                       
         OI    TIME+10,X'F0'                                                    
         SRL   R0,4                                                             
         STC   R0,TIME+9           HH PORTION                                   
         OI    TIME+9,X'F0'                                                     
*                                                                               
         SRL   R0,4                                                             
         STC   R0,TIME+7                                                        
         OI    TIME+7,X'F0'                                                     
         SRL   R0,4                                                             
         STC   R0,TIME+6           SS PORTION                                   
         OI    TIME+6,X'F0'                                                     
*                                                                               
         SRL   R0,4                                                             
         STC   R0,TIME+4                                                        
         OI    TIME+4,X'F0'                                                     
         SRL   R0,4                                                             
         STC   R0,TIME+3           MM PORTION                                   
         OI    TIME+3,X'F0'                                                     
*                                                                               
         SRL   R0,4                                                             
         STC   R0,TIME+1                                                        
         OI    TIME+1,X'F0'                                                     
         SRL   R0,4                                                             
         STC   R0,TIME+0           HH PORTION                                   
         OI    TIME+0,X'F0'                                                     
*                                                                               
         MVI   TIME+2,C':'                                                      
         MVI   TIME+5,C':'                                                      
         MVI   TIME+8,C'.'                                                      
*                                                                               
         GOTO1 BLDREC,DMCB,(L'TIME,TIME)  TIME HH:MM:SS.HH                      
*                                                                               
         GOTO1 BLDREC,DMCB,(3,=C'000')  SEQ                                     
*                                                                               
         GOTO1 BLDREC,DMCB,(L'BLHPID,BLHPID)  PID                               
         GOTO1 BLDREC,DMCB,(L'BLHFNME,BLHFNME) FIRST NAME                       
         GOTO1 BLDREC,DMCB,(L'BLHLNME,BLHLNME) LAST NAME                        
         GOTO1 BLDREC,DMCB,(L'BLHEMAIL,BLHEMAIL) EMAIL ADDRESS                  
         GOTO1 BLDREC,DMCB,(L'BLHDLV,BLHDLV)  DELIVERY SYSTEM (CAD)             
         GOTO1 BLDREC,DMCB,(2,=C'SP')         MEDIA (SP)OT                      
         GOTO1 BLDREC,DMCB,(L'BLHSMED,BLHSMED) SUB MEDIA (T)V, (R)ADIO          
*                                                                               
         OC    BLHDELAY,BLHDELAY                                                
         BNZ   MFHU03                                                           
         GOTO1 BLDREC,DMCB,(1,=C'0')          DELAY                             
         B     MFHU04                                                           
                                                                                
MFHU03   EDIT  (B2,BLHDELAY),(4,FULL),ALIGN=LEFT                                
         GOTO1 BLDREC,DMCB,(4,FULL)          DELAY                              
*                                                                               
MFHU04   XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING FNAME,R4                                                         
         MVC   FNAGY(2),BLHAGY        AGENCY ALPHA                              
         MVI   FNAGY+2,C' '                                                     
*                                                                               
         MVC   FNENV,BLHENV         ENVIRONMENT PRD/TST/CSC                     
*                                                                               
MFHU05   GOTO1 DATCON,DMCB,(5,0),(20,TDATE8)                                    
         MVC   FNTDTE,TDATE8       TODAY'S DATE YYYYMMDD                        
         MVC   FNTIME(2),TIME      TIME HH                                      
         MVC   FNTIME+2(2),TIME+3       MM                                      
         MVC   FNTIME+4(2),TIME+6       SS                                      
         MVC   FNTIME+6(2),TIME+9       HS                                      
         MVC   FNSEQ,=C'00'        SEQUENCE (NOT SUPPORTED)                     
*                                                                               
         GOTO1 BLDREC,DMCB,(24,FNAME) FILE NAME                                 
         DROP  R4                                                               
*                                                                               
         GOTO1 BLDREC,DMCB,(1,=C'0')  REVISION (NOT SUPPORTED)                  
         GOTO1 BLDREC,DMCB,(2,=C'DS') SOURCE                                    
*                                                                               
         BAS   RE,MFPUT                                                         
*                                                                               
         J     EXIT1                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
**********************************************************************          
*   ADD AGENCY RECORD TO FLAT FILE                                              
**********************************************************************          
*                                                                               
MFAGY    NTR1                                                                   
         MVC   ANXTPOS,AFRECP4     NEW RECORD                                   
         GOTO1 BLDREC,DMCB,(4,=C'AGNY')                                         
         GOTO1 BLDREC,DMCB,(L'BLAUID,BLAUID)  USER ID                           
         GOTO1 BLDREC,DMCB,(L'BLANAME,BLANAME)  USER NAME                       
*                                                                               
         BAS   RE,MFPUT                                                         
*                                                                               
         J     EXIT1                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
**********************************************************************          
*   ADD ADVERTISER TO FLAT FILE                                                 
**********************************************************************          
*                                                                               
MFADV    NTR1                                                                   
         MVC   ANXTPOS,AFRECP4     NEW RECORD                                   
         GOTO1 BLDREC,DMCB,(4,=C'ADVR')                                         
         GOTO1 BLDREC,DMCB,(L'BLADCDE,BLADCDE)  CLT CODE                        
         GOTO1 BLDREC,DMCB,(L'BLADNME,BLADNME)  CLT NAME                        
*                                                                               
         BAS   RE,MFPUT                                                         
*                                                                               
         J     EXIT1                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
**********************************************************************          
*     ADD PRODUCT (BRAND) RECORD TO FLAT FILE                                   
**********************************************************************          
*                                                                               
MFPRD    NTR1                                                                   
         MVC   ANXTPOS,AFRECP4     NEW RECORD                                   
         GOTO1 BLDREC,DMCB,(4,=C'BRND')                                         
                                                                                
         GOTO1 BLDREC,DMCB,(L'BLPRCDE,BLPRCDE) PRD                              
         GOTO1 BLDREC,DMCB,(L'BLPRNME,BLPRNME) PRD NAME                         
         GOTO1 BLDREC,DMCB,(L'BLPRTAL,BLPRTAL) TALENT ID                        
*                                                                               
         BAS   RE,MFPUT                                                         
*                                                                               
         J     EXIT1                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
**********************************************************************          
*     ADD CML/PRD RECORD TO FLAT FILE                                           
**********************************************************************          
*                                                                               
MFCPR    NTR1                                                                   
         MVC   ANXTPOS,AFRECP4     NEW RECORD                                   
         GOTO1 BLDREC,DMCB,(4,=C'SPBR')                                         
                                                                                
         GOTO1 BLDREC,DMCB,(L'BLADID,BLADID)   CML                              
         GOTO1 BLDREC,DMCB,(L'BLADPRD,BLADPRD) PRD CODE                         
*                                                                               
         BAS   RE,MFPUT                                                         
*                                                                               
         J     EXIT1                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
**********************************************************************          
*     SPOT/ADID RECORD                                                          
*     ADD COMMERCIAL DEFINITION RECORD TO FLAT FILE                             
*     HIDEF IS TREATED AS A SEPARATE CML                                        
**********************************************************************          
*                                                                               
MFCML    NTR1                                                                   
*                                                                               
         NI    FLAG1,X'FF'-(HIDEFSW+CNTRCSW) INIT FLAG                          
*                                                                               
MFCML02  MVC   ANXTPOS,AFRECP4     NEW RECORD                                   
         GOTO1 BLDREC,DMCB,(4,=C'SPOT')                                         
*                                                                               
         TM    FLAG1,HIDEFSW       HIDEF CML                                    
         BO    MFCML03                                                          
*                                                                               
         TM    FLAG1,CNTRCSW       CNTRCUT CML                                  
         BZ    MFCML02C                                                         
*                                                                               
         GOTO1 BLDREC,DMCB,(L'BLCML,BLCMLCT) ADD CNTRCT                         
         B     MFCML04                                                          
*                                                                               
MFCML02C GOTO1 BLDREC,DMCB,(L'BLCML,BLCML)   ADD COMML                          
         B     MFCML04                                                          
*                                                                               
MFCML03  GOTO1 BLDREC,DMCB,(L'BLCML,BLCMLHD) ADD HIDEF                          
*                                                                               
MFCML04  GOTO1 BLDREC,DMCB,(L'BLCMDS0,BLCMDS0) TITLE/DESCRIPTION                
*                                                                               
         TM    FLAG1,HIDEFSW       HIDEF CML                                    
         BO    MFCML05                                                          
*                                                                               
         TM    FLAG1,CNTRCSW       CNTRCT CML                                   
         BZ    MFCML04C                                                         
*                                                                               
         GOTO1 BLDREC,DMCB,(2,=C'CT') CML FORMAT                                
         NI    FLAG1,X'FF'-CNTRCSW       TURN OFF CNTRCT FLAG                   
         B     MFCML06                                                          
*                                                                               
MFCML04C GOTO1 BLDREC,DMCB,(1,=C' ') PLACE HOLDER                               
         OI    FLAG1,HIDEFSW       TURN ON HIDEF FLAG FOR LATER                 
         B     MFCML06                                                          
*                                                                               
MFCML05  GOTO1 BLDREC,DMCB,(2,=C'HD') CML FORMAT                                
         NI    FLAG1,X'FF'-HIDEFSW       TURN OFF HIDEF FLAG                    
         OI    FLAG1,CNTRCSW       TURN ON CNTRCT FLAG FOR LATER                
*                                                                               
MFCML06  EDIT  (B1,BLCMLEN),(3,FULL),ALIGN=LEFT                                 
         GOTO1 BLDREC,DMCB,(3,FULL)   CML LENGTH                                
*                                                                               
         GOTO1 BLDREC,DMCB,(1,=C' ') PLACE HOLDER                               
         GOTO1 BLDREC,DMCB,(1,=C' ') PLACE HOLDER                               
*                                                                               
         GOTO1 BLDREC,DMCB,(L'BLCMPO,BLCMPO)   PO NUMBER                        
*                                                                               
* *******************  PER RAMUNE - NOT SUPPORTED *************                 
*NOP     GOTO1 DATCON,DMCB,(3,BLCMREL),(20,WORK) YYYYMMDD                       
*        GOTO1 BLDREC,DMCB,(L'BLCMREL0,BLCMREL) RELEASE                         
*                                                                               
*NOP     CLC   BLCMRCL,SPACES      UFN = SPACES                                 
*        BE    MFCML10                                                          
****     GOTO1 DATCON,DMCB,(3,BLCMRCL),(20,WORK) YYYYMMDD                       
*                                                                               
*FCML10  GOTO1 BLDREC,DMCB,(L'BLCMRCL0,BLCMRCL)   RECALL                        
******** MVC   CMLRCL,BLCMRCL                                                   
*****************************************************************               
         BAS   RE,MFPUT                                                         
*                                                                               
         OC    BLCMLHD,BLCMLHD     ANY HIDEF                                    
         BZ    MFCMLX                                                           
*                                                                               
         TM    FLAG1,HIDEFSW       IS HIDEF FLAG ON?                            
         BO    MFCML02                                                          
*                                                                               
         OC    BLCMLCT,BLCMLCT     ANY CNTRCUT                                  
         BZ    MFCMLX                                                           
*                                                                               
         TM    FLAG1,CNTRCSW       IS CNTRCUT FLAG ON?                          
         BO    MFCML02                                                          
*                                                                               
MFCMLX   J     EXIT1                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
**********************************************************************          
*   ADD ORDER RECORD TO FLAT FILE                                               
**********************************************************************          
*                                                                               
MFORD    NTR1                                                                   
         MVC   ANXTPOS,AFRECP4     NEW RECORD                                   
         GOTO1 BLDREC,DMCB,(4,=C'ORDR')                                         
         GOTO1 BLDREC,DMCB,(1,BLORPRI)  DELIVERY PRIORITY                       
         GOTO1 BLDREC,DMCB,(L'BLORPO,BLORPO)   PO NUMBER                        
         GOTO1 BLDREC,DMCB,(L'BLORCMT,BLORCMT) COMMENT LINES                    
         GOTO1 BLDREC,DMCB,(L'BLORPSR,BLORPSR) PROD SERVICES                    
         GOTO1 BLDREC,DMCB,(L'BLORPRD,BLORPRD) PRODUCT CODE                     
         BAS   RE,MFPUT                                                         
         J     EXIT1                                                            
*                                                                               
*                                                                               
**********************************************************************          
*   ADD ORDER ITEM RECORD TO FLAT FILE                                          
**********************************************************************          
*                                                                               
MFOIR    NTR1                                                                   
         MVC   ANXTPOS,AFRECP4     NEW RECORD                                   
         GOTO1 BLDREC,DMCB,(4,=C'ORIT')                                         
*                                                                               
         GOTO1 BLDREC,DMCB,(L'BLOIADID,BLOIADID) CML                            
         GOTO1 BLDREC,DMCB,(L'BLOISTA,BLOISTA) STATION (0032-TV)                
*                                                                               
         GOTO1 DATCON,DMCB,(3,BLOTFTD),(23,WORK) YYYY-MM-DD                     
         GOTO1 BLDREC,DMCB,(10,WORK)    FTD                                     
         GOTO1 BLDREC,DMCB,(L'BLOTLTD,BLOTLTD) LTD (NOT SUPPORTED)              
         GOTO1 BLDREC,DMCB,(L'BLOTTYP,BLOTTYP) SHIP TYPE                        
*                                                                               
         BAS   RE,MFPUT                                                         
*                                                                               
EXIT1    XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*********************************************************************           
*   PUT RECORD TO OUTPUT FILE                                                   
*********************************************************************           
*                                                                               
MFPUT    NTR1                                                                   
*                                                                               
         ICM   RF,3,FCOUNT                                                      
         LA    RF,1(RF)            INCREMENT RECORD COUNT                       
         STCM  RF,3,FCOUNT                                                      
*                                                                               
         L     R2,ANXTPOS          SET RECORD LENGTH                            
         S     R2,AFREC                                                         
         LAY   RF,FREC                                                          
         STCM  R2,3,0(RF)                                                       
*                                                                               
         LA    RF,4(RF)            POINT TO ACTUAL DATA                         
***      SHI   R2,5                4 BYTES FOR LEN 1 EX                         
**       EX    R2,*+8                                                           
**       B     *+10                                                             
*****    TR    0(0,RF),TRTAB       TRANSLATE DATA                               
*                                                                               
*        LA    R2,EDIOUT                                                        
         LA    R2,RANKWK                                                        
         LAY   RF,FREC                                                          
         PUT   (R2),0(RF)                                                       
*                                                                               
         J     EXIT1                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
* ADD DATA ITEM TO RECORD                                                       
* P1 POINTS TO ITEM, BYTE 0 = LENGTH                                            
***********************************************************************         
*                                                                               
BLDREC   NTR1                                                                   
         SR    R2,R2                                                            
         ICM   R2,7,1(R1)          A(DATA ITEM)                                 
         SR    RF,RF                                                            
         ICM   RF,1,0(R1)          LENGTH                                       
*                                                                               
         AR    RF,R2               STRIP ANY TRAILING BLANKS                    
         BCTR  RF,0                                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         SR    RF,R2                                                            
         BNM   *+10                                                             
         SR    RF,RF                                                            
         B     BR03                                                             
         LA    RF,1(RF)                                                         
*                                                                               
BR03     L     R3,ANXTPOS          WHERE TO PUT                                 
         LA    R4,0(RF,R3)                                                      
         C     R4,AFRECX                                                        
         BL    *+6                                                              
         DC    H'0'                REC OVERFLOW                                 
*                                                                               
         LTR   RF,RF                                                            
         BNP   BR06                                                             
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)                                                    
         LA    R3,1(R3,RF)                                                      
*                                                                               
BR06     MVI   0(R3),FDELIM                                                     
         LA    R3,1(R3)                                                         
*                                                                               
         ST    R3,ANXTPOS                                                       
*                                                                               
         J     EXIT1                                                            
*                                                                               
*DIOUT   DCB   DDNAME=EDIOUT                                                    
RANKWK   DCB   DDNAME=RANKWK,                                          X        
               BLKSIZE=4000,                                           X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               MACRF=(GM,PM),                                          X        
               EODAD=TEMPED                                                     
TEMPED   DS    0H                                                               
                                                                                
**********************************************************************          
*        COMMON SUBROUTINES AND DATA AREAS                                      
**********************************************************************          
*                                                                               
COMSUBS  DS    0D                                                               
*                                                                               
MODLIST  DS    0X                  MODE LIST                                    
         DC    AL1(ESAHED),AL4(ESAHD)   ADD HEADER RECORD                       
         DC    AL1(ESAAGY),AL4(ESAAG)   ADD AGENCY RECORD                       
         DC    AL1(ESAADV),AL4(ESADV)   ADD ADVERTISER                          
         DC    AL1(ESAPRD),AL4(ESAPR)   ADD PROD (BRAND) RECORD                 
         DC    AL1(ESACMP),AL4(ESACP)   ADD CML/PROD RECORD                     
         DC    AL1(ESACML),AL4(ESACL)   ADD COMMERCIAL DEFINITION               
         DC    AL1(ESAORD),AL4(ESAOR)   ADD ORDER RECORD                        
         DC    AL1(ESAOIT),AL4(ESAOI)   ADD ORDER ITEM RECORD                   
MODLISTN EQU   (*-MODLIST)/5                                                    
*                                                                               
*                                                                               
MKFROUTS DS    0X                  MAKEFILE RECORD ROUTINE LIST                 
         DC    AL1(ESHEDQ),AL4(MFHU)     ADD HEADER RECORD                      
         DC    AL1(ESAGQ),AL4(MFAGY)      AGENCY RECORD                         
         DC    AL1(ESADVQ),AL4(MFADV)     ADVERTISER                            
         DC    AL1(ESPRDQ),AL4(MFPRD)     PRODUCT (BRAND) RECORD                
         DC    AL1(ESCMLQ),AL4(MFCML)     COMMERCIAL DEFINITION REC             
         DC    AL1(ESCPRQ),AL4(MFCPR)     CML/PRD RECORD                        
         DC    AL1(ESORDQ),AL4(MFORD)     ORDER RECORD                          
         DC    AL1(ESOITQ),AL4(MFOIR)     ORDER ITEM RECORD                     
MKFRTSN  EQU   (*-MKFROUTS)/5                                                   
*                                                                               
*                                                                               
FRSTSW   DC    X'00'                                                            
OPENFSW  DC    X'00'                                                            
*                                                                               
TRTAB    DC    X'40404040404040404040404040404040'     00-0F                    
         DC    X'40404040404040404040404040404040'     10-1F                    
         DC    X'40404040404040404040404040404040'     20-2F                    
         DC    X'40404040404040404040404040404040'     30-3F                    
         DC    X'404040404040404040404040404D4E4F'     40-4F                    
         DC    X'4E4040404040404040405A5B5C5D5E40'     50-5F                    
         DC    X'604040404040404040406A6B6C6D406F'     60-6F                    
         DC    X'404040404040404040797A7B7C407E40'     70-7F                    
         DC    X'40818283848586878889404040404040'     80-8F                    
         DC    X'40919293949596979899404040404040'     90-9F                    
         DC    X'4040A2A3A4A5A6A7A8A9404040404040'     A0-AF                    
         DC    X'40404040404040404040404040404040'     B0-BF                    
         DC    X'C0C1C2C3C4C5C6C7C8C9404040404040'     C0-CF                    
         DC    X'D0D1D2D3D4D5D6D7D8D9404040404040'     D0-DF                    
         DC    X'E040E2E3E4E5E6E7E8E9404040404040'     E0-EF                    
         DC    X'F0F1F2F3F4F5F6F7F8F9404040404040'     F0-FF                    
*                                                                               
         LTORG                                                                  
*                                                                               
**********************************************************************          
* READ FLAT FILE CREATED IN MKF, CREATE XML MAP AND MQ XML                      
**********************************************************************          
*                                                                               
MAKEXML  NMOD1 (LWSX-LWSD),*MAKEXML,CLEAR=YES,R7,R8                             
*                                                                               
         LR    R9,RC                                                            
         USING LWSD,R9                                                          
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
*                                                                               
         L     RA,4(R1)                                                         
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
*                                                                               
         L     RE,TWAMASTC                                                      
         L     RF,MCSSB-MCBLOCK(RE)                                             
*                                                                               
         L     RF,MCVMQRPT-MCBLOCK(RE)                                          
         ST    RF,AMQRPT                                                        
*                                                                               
         L     R1,8(R1)            QUEUE SUB-NAME (QUESNAME)                    
         LA    RF,MQNAME           OPTICA MQ                                    
         MVC   6(4,RF),0(R1)                                                    
*                                                                               
* X'80' SUPPRESS LENGTH                                                         
         GOTO1 AMQRPT,DMCB,(0,=C'OPEN'),(0,0(RF)),(X'A0',0),0                   
         B     MKX03                                                            
*                                                                               
MQNAME   DC    C'OPTICAXXXX******'                                              
*                                                                               
*                                                                               
MKX03    L     R3,AFFROUT          FLAT FILE ROUTINE TABLE                      
         USING FFROUT,R3                                                        
*                                                                               
*SM *NOP OPEN  (RANKWK,INPUT)                                                   
         OPEN  (RANKWK2,INPUT)                                                  
*                                                                               
         LA    R6,INREC                                                         
         ST    R6,AINREC                                                        
         LA    RF,4(R6)                                                         
         ST    RF,AINRECP4         REC+4 IS START OF DATA                       
*                                                                               
         LA    RF,MAP              SET ADDR'S OF REC AREAS                      
         ST    RF,AMAP                                                          
                                                                                
         GET   RANKWK2,(R6)                                                     
         L     R6,AINRECP4         POINT TO ACTUAL DATA                         
*                                                                               
         XC    SVRECTYP,SVRECTYP   INIT RECORD TYPE                             
         XC    SVENDMSG,SVENDMSG        END MSG AREA                            
         XC    AROUT,AROUT              A(ROUTIN) IN MAP DATA                   
         MVI   NEWTYP,0                 NEW TYPE FLAG                           
*                                                                               
         BRAS  RE,CXHED            CREATE XML HEADING                           
*                                                                               
* FIND ROUTINE                                                                  
FL02     LA    RF,FLATAB                                                        
         LA    R0,FLATABLN                                                      
*                                                                               
FL04     DS    0H                                                               
         CLC   0(4,R6),0(RF)                                                    
         BE    FL04A                                                            
         LA    RF,8(RF)                                                         
         BCT   R0,FL04                                                          
         DC    H'0'                                                             
*                                                                               
FL04A    DS    0H                                                               
         ICM   RF,15,4(RF)                                                      
         BASR  RE,RF                                                            
*                                                                               
         L     R6,AINREC                                                        
*NOP     GET   EDIOUT,(R6)                                                      
         GET   RANKWK2,(R6)                                                     
*                                                                               
         L     R6,AINRECP4         POINT TO ACTUAL DATA                         
*                                                                               
         OC    SVRECTYP,SVRECTYP   1ST TIME IN                                  
         BZ    FL10                 YES                                         
*                                                                               
         CLC   SVRECTYP,0(R6)      PREV REC TYP TO CURR                         
         BE    FL02                                                             
*                                                                               
         OC    SVENDMSG,SVENDMSG   ANY END MSG                                  
         BZ    *+8                 NO                                           
         BRAS  RE,PENDMSG          PUT OUT END MSG                              
*                                                                               
         MVC   SVRECTYP,0(R6)      SAVE THIS REC TYPE                           
         OI    NEWTYP,HEADTSW      PRINT HEADING                                
         B     FL02                                                             
*                                                                               
FL10     MVC   SVRECTYP,0(R6)      SAVE THIS REC TYPE                           
         B     FL02                NO END MSG YET                               
*                                                                               
EXIT2    XIT1                                                                   
*                                                                               
*                                                                               
RANKWK2  DCB   DDNAME=RANKWK,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               MACRF=(GM,PM),                                          X        
               EODAD=ENDXML                                                     
ENDXML   DS    0H                                                               
         OC    AROUT,AROUT         ANYTHING TO PUT OUT                          
         BZ    ENDX10                                                           
                                                                                
         OI    NEWTYP,TYPNEQ       RECORD SUB-TYPE CHANGED                      
         BRAS  RE,PROUT                                                         
                                                                                
ENDX10   DS    0H                                                               
         OC    SVENDMSG,SVENDMSG   ANY END MSG                                  
         BZ    *+8                 NO                                           
         BRAS  RE,PENDMSG          PUT OUT END MSG                              
                                                                                
         BRAS  RE,XMLENDMS         XML END MSG                                  
                                                                                
         CLOSE (RANKWK)                                                         
                                                                                
         CLI   OUTMDE,C'D'         XML OUTPUT TO DATASET                        
         BNE   ENDX15                                                           
                                                                                
         CLOSE (XMLOUT)                                                         
         J     EXIT2                                                            
                                                                                
* DONE EMAILING, CLOSE MQ                                                       
ENDX15   GOTO1 AMQRPT,DMCB,(0,=C'CLOSE'),0,0,0                                  
         CLI   DMCB+8,0                                                         
         JE    EXIT2                                                            
*                                                                               
         XC    VTXT(100),VTXT                                                   
         MVC   VTXT(18),=CL18'AUTONOTE*SMURDDNY:'                               
         MVC   VTXT+18+1(15),=CL15'MQ CLOSE FAILED'                             
         ICM   R1,15,DMCB+12                                                    
         MVC   VTXT+35(28),0(R1)                                                
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(63,VTXT)                                 
         J     EXIT2                                                            
*NOP1>> ADD CODE TO NOTIFY CLIENT VIA EMAIL                                     
*                                                                               
RELO     DS    A                                                                
AFFROUT  DC    A(FFROUT)          A(FLAT FILE ROUTINE)                          
*                                                                               
*                                                                               
CXHED    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   OUTMDE,C'Q'         XML TO MQ                                    
         B     *+8                                                              
         MVI   OUTMDE,C'D'         FOR TESTING XML TO DATASET                   
*                                                                               
*                                  PATCH TO 'D' FOR DATASET                     
         CLI   OUTMDE,C'D'         OUTPUT TO DATASET                            
         BNE   CXH10                                                            
                                                                                
         OPEN  (XMLOUT,(OUTPUT))                                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
CXH10    BRAS  RE,SXMLMSG          XML START MSG                                
                                                                                
         J     EXIT2                                                            
*                                                                               
*=======================================================                        
* XML START MESSAGE                                                             
*=======================================================                        
XMLMSG   DS    0X                                                               
         DC    AL1(CONQ),AL2(0,10),C'<shipList>'                                
         DC    AL1(EORQ),AL2(4+10)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
*                                                                               
*================================================================               
*  HEADER MESSAGE                                                               
*================================================================               
*                                                                               
HDMSG    DS    0X                                                               
         DC    AL1(CONQ),AL2(0,8),C'<header>'                                   
         DC    AL1(EORQ),AL2(4+8)                                               
                                                                                
         DC    AL1(CONQ),AL2(0,16),C'<moLegacySystem>'                          
         DC    AL1(FVDQ),AL2(16,2),AL2(HDRLEG-HDD)                              
         DC    AL1(CONQ),AL2(18,17),C'</moLegacySystem>'                        
         DC    AL1(EORQ),AL2(4+16+2+17)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,8),C'<tenant>'                                   
         DC    AL1(FVDQ),AL2(8,2),AL2(HDRAGY-HDD)                               
         DC    AL1(CONQ),AL2(10,9),C'</tenant>'                                 
         DC    AL1(EORQ),AL2(4+8+2+9)                                           
                                                                                
         DC    AL1(CONQ),AL2(0,13),C'<environment>'                             
         DC    AL1(FVDQ),AL2(13,3),AL2(HDRENV-HDD)                              
         DC    AL1(CONQ),AL2(16,14),C'</environment>'                           
         DC    AL1(EORQ),AL2(4+13+3+14)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,20),C'<shipListCreateDate>'                      
         DC    AL1(FVDQ),AL2(20,10),AL2(HDRDTE-HDD)                             
         DC    AL1(CONQ),AL2(30,21),C'</shipListCreateDate>'                    
         DC    AL1(EORQ),AL2(4+20+2+21)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,20),C'<shipListCreateTime>'                      
         DC    AL1(FVDQ),AL2(20,11),AL2(HDRTME-HDD)                             
         DC    AL1(CONQ),AL2(31,21),C'</shipListCreateTime>'                    
         DC    AL1(EORQ),AL2(4+20+11+21)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,24),C'<shipListSequenceNumber>'                  
         DC    AL1(FVDQ),AL2(24,3),AL2(HDRSEQ-HDD)                              
         DC    AL1(CONQ),AL2(27,25),C'</shipListSequenceNumber>'                
         DC    AL1(EORQ),AL2(4+24+3+25)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,8),C'<userId>'                                   
         DC    AL1(FVDQ),AL2(8,8),AL2(HDRPID-HDD)                               
         DC    AL1(CONQ),AL2(16,9),C'</userId>'                                 
         DC    AL1(EORQ),AL2(4+8+8+9)                                           
                                                                                
         DC    AL1(CONQ),AL2(0,15),C'<userFirstName>'                           
         DC    AL1(FVDQ),AL2(15,9),AL2(0)                                       
         DC    AL1(CONQ),AL2(24,16),C'</userFirstName>'                         
         DC    AL1(EORQ),AL2(4+15+9+16)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,14),C'<userLastName>'                            
         DC    AL1(FVDQ),AL2(14,18),AL2(0)                                      
         DC    AL1(CONQ),AL2(32,15),C'</userLastName>'                          
         DC    AL1(EORQ),AL2(4+14+18+15)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,14),C'<emailAddress>'                            
         DC    AL1(FVDQ),AL2(14,63),AL2(0)                                      
         DC    AL1(CONQ),AL2(77,15),C'</emailAddress>'                          
         DC    AL1(EORQ),AL2(4+14+63+15)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,17),C'<shiplistVersion>'                         
         DC    AL1(CONQ),AL2(0,3),C'7.0'                                        
         DC    AL1(CONQ),AL2(77,18),C'</shiplistVersion>'                       
         DC    AL1(EORQ),AL2(4+18+5+18)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,12),C'<vendorCode>'                              
         DC    AL1(FVDQ),AL2(12,3),AL2(0)                                       
         DC    AL1(CONQ),AL2(15,13),C'</vendorCode>'                            
         DC    AL1(EORQ),AL2(4+12+3+13)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,13),C'<mediaSystem>'                             
         DC    AL1(FVDQ),AL2(13,2),AL2(0)                                       
         DC    AL1(CONQ),AL2(14,14),C'</mediaSystem>'                           
         DC    AL1(EORQ),AL2(4+13+2+14)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,16),C'<mediaSubSystem>'                          
         DC    AL1(FVDQ),AL2(16,1),AL2(0)                                       
         DC    AL1(CONQ),AL2(17,17),C'</mediaSubSystem>'                        
         DC    AL1(EORQ),AL2(4+16+1+17)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,23),C'<orderDelayedStartTime>'                   
         DC    AL1(SOPQ),AL2(0)                                                 
         DC    AL1(FVDQ),AL2(23,4),AL2(0)                                       
         DC    AL1(EOPQ),AL2(0)                                                 
         DC    AL1(CONQ),AL2(27,24),C'</orderDelayedStartTime>'                 
         DC    AL1(EOPQ),AL2(0)                                                 
         DC    AL1(EORQ),AL2(4+23+4+24)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,10),C'<fileName>'                                
         DC    AL1(FVDQ),AL2(10,24),AL2(0)                                      
         DC    AL1(CONQ),AL2(34,11),C'</fileName>'                              
         DC    AL1(EORQ),AL2(4+10+24+11)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,10),C'<revision>'                                
         DC    AL1(FVDQ),AL2(10,1),AL2(0)                                       
         DC    AL1(CONQ),AL2(11,11),C'</revision>'                              
         DC    AL1(EORQ),AL2(4+10+1+11)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,8),C'<source>'                                   
         DC    AL1(FVDQ),AL2(8,2),AL2(0)                                        
         DC    AL1(CONQ),AL2(10,9),C'</source>'                                 
         DC    AL1(EORQ),AL2(4+8+2+9)                                           
                                                                                
         DC    AL1(CONQ),AL2(0,9),C'</header>'                                  
         DC    AL1(EORQ),AL2(4+9)                                               
                                                                                
         DC    AL1(ENDQ)                                                        
*                                                                               
*                                                                               
*================================================================               
*  AGENCY MESSAGE                                                               
*================================================================               
*                                                                               
AGMSG    DS    0X                                                               
         DC    AL1(CONQ),AL2(0,8),C'<agency>'                                   
         DC    AL1(EORQ),AL2(4+8)                                               
                                                                                
         DC    AL1(CONQ),AL2(0,10),C'<location>'                                
         DC    AL1(FVDQ),AL2(8,10),AL2(AGUID-AGD)                               
         DC    AL1(CONQ),AL2(18,11),C'</location>'                              
         DC    AL1(EORQ),AL2(4+10+10+11)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,6),C'<name>'                                     
         DC    AL1(CONQ),AL2(6,9),C'<!CDATA'                                  
         DC    AL1(FVDQ),AL2(15,33),AL2(0)                                      
         DC    AL1(CONQ),AL2(48,3),C'ÙÙ>'                                       
         DC    AL1(CONQ),AL2(51,7),C'</name>'                                   
         DC    AL1(EORQ),AL2(4+6+9+33+3+7)                                      
                                                                                
         DC    AL1(CONQ),AL2(0,9),C'</agency>'                                  
         DC    AL1(EORQ),AL2(4+9)                                               
                                                                                
         DC    AL1(ENDQ)                                                        
*                                                                               
*                                                                               
*================================================================               
*  ADVERTISER MESSAGE                                                           
*================================================================               
*                                                                               
ADMSG    DS    0X                                                               
         DC    AL1(CONQ),AL2(0,12),C'<advertiser>'                              
         DC    AL1(EORQ),AL2(4+12)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,14),C'<advertiserId>'                            
         DC    AL1(FVDQ),AL2(14,3),AL2(ADCDE-ADVD)                              
         DC    AL1(CONQ),AL2(17,15),C'</advertiserId>'                          
         DC    AL1(EORQ),AL2(4+14+3+15)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,6),C'<name>'                                     
         DC    AL1(CONQ),AL2(6,9),C'<!CDATA'                                  
         DC    AL1(FVDQ),AL2(15,20),AL2(0)                                      
         DC    AL1(CONQ),AL2(35,3),C'ÙÙ>'                                       
         DC    AL1(CONQ),AL2(38,7),C'</name>'                                   
         DC    AL1(EORQ),AL2(4+6+9+20+3+7)                                      
                                                                                
         DC    AL1(CONQ),AL2(0,13),C'</advertiser>'                             
         DC    AL1(EORQ),AL2(4+13)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
*                                                                               
*                                                                               
*================================================================               
*  BRAND (PRODUCT) MESSAGE                                                      
*================================================================               
*                                                                               
BRMSG    DS    0X                                                               
         DC    AL1(SHDQ),AL2(0,8),C'<brands>'                                   
         DC    AL1(EORQ),AL2(4+8)                                               
                                                                                
         DC    AL1(CONQ),AL2(0,7),C'<brand>'                                    
         DC    AL1(EORQ),AL2(4+7)                                               
                                                                                
         DC    AL1(CONQ),AL2(0,9),C'<brandId>'                                  
         DC    AL1(FVDQ),AL2(9,3),AL2(PRDPRD-PRDD)                              
         DC    AL1(CONQ),AL2(12,10),C'</brandId>'                               
         DC    AL1(EORQ),AL2(4+9+3+10)                                          
                                                                                
         DC    AL1(CONQ),AL2(0,6),C'<name>'                                     
         DC    AL1(CONQ),AL2(6,9),C'<!CDATA'                                  
         DC    AL1(FVDQ),AL2(15,20),AL2(0)                                      
         DC    AL1(CONQ),AL2(35,3),C'ÙÙ>'                                       
         DC    AL1(CONQ),AL2(38,7),C'</name>'                                   
         DC    AL1(EORQ),AL2(4+6+9+20+3+7)                                      
                                                                                
         DC    AL1(CONQ),AL2(0,10),C'<talentId>'                                
         DC    AL1(SOPQ),AL2(0)                                                 
         DC    AL1(FVDQ),AL2(10,6),AL2(0)                                       
         DC    AL1(EOPQ),AL2(0)                                                 
         DC    AL1(CONQ),AL2(0,11),C'</talentId>'                               
         DC    AL1(EORQ),AL2(4+10+6+11)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,8),C'</brand>'                                   
         DC    AL1(EORQ),AL2(4+8)                                               
                                                                                
         DC    AL1(SENQ),AL2(0,9),C'</brands>'                                  
         DC    AL1(EORQ),AL2(4+9)                                               
                                                                                
         DC    AL1(ENDQ)                                                        
*                                                                               
*                                                                               
*================================================================               
*  SPOTS (CML) MESSAGE                                                          
*================================================================               
*                                                                               
SPMSG    DS    0X                                                               
         DC    AL1(SHDQ),AL2(0,7),C'<spots>'                                    
         DC    AL1(EORQ),AL2(4+7)                                               
                                                                                
         DC    AL1(CONQ),AL2(0,6),C'<spot>'                                     
         DC    AL1(EORQ),AL2(4+6)                                               
                                                                                
         DC    AL1(CONQ),AL2(0,6),C'<adId>'                                     
         DC    AL1(FVDQ),AL2(6,12),AL2(SCMLCML-SCMLD)                           
         DC    AL1(CONQ),AL2(18,7),C'</adId>'                                   
         DC    AL1(EORQ),AL2(4+6+12+7)                                          
                                                                                
         DC    AL1(CONQ),AL2(0,7),C'<title>'                                    
         DC    AL1(CONQ),AL2(7,9),C'<!CDATA'                                  
         DC    AL1(FVDQ),AL2(16,57),AL2(0)                                      
         DC    AL1(CONQ),AL2(73,3),C'ÙÙ>'                                       
         DC    AL1(CONQ),AL2(76,8),C'</title>'                                  
         DC    AL1(EORQ),AL2(4+7+9+57+3+8)                                      
                                                                                
         DC    AL1(CONQ),AL2(0,8),C'<format>'                                   
         DC    AL1(SOPQ),AL2(0)                                                 
         DC    AL1(FVDQ),AL2(8,2),AL2(0)                                        
         DC    AL1(EOPQ),AL2(0)                                                 
         DC    AL1(CONQ),AL2(10,9),C'</format>'                                 
         DC    AL1(EORQ),AL2(4+8+2+9)                                           
                                                                                
         DC    AL1(CONQ),AL2(0,10),C'<duration>'                                
         DC    AL1(FVDQ),AL2(10,3),AL2(0)                                       
         DC    AL1(CONQ),AL2(13,11),C'</duration>'                              
         DC    AL1(EORQ),AL2(4+10+3+11)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,14),C'<firstAirDate>'                            
         DC    AL1(SOPQ),AL2(0)                                                 
         DC    AL1(FVDQ),AL2(14,10),AL2(0)                                      
         DC    AL1(EOPQ),AL2(0)                                                 
         DC    AL1(CONQ),AL2(0,15),C'</firstAirDate>'                           
         DC    AL1(EORQ),AL2(4+14+10+15)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,13),C'<lastAirDate>'                             
         DC    AL1(SOPQ),AL2(0)                                                 
         DC    AL1(FVDQ),AL2(14,10),AL2(0)                                      
         DC    AL1(EOPQ),AL2(0)                                                 
         DC    AL1(CONQ),AL2(0,14),C'</lastAirDate>'                            
         DC    AL1(EORQ),AL2(4+13+10+14)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,10),C'<poNumber>'                                
         DC    AL1(SOPQ),AL2(0)                                                 
         DC    AL1(FVDQ),AL2(10,24),AL2(0)                                      
         DC    AL1(EOPQ),AL2(0)                                                 
         DC    AL1(CONQ),AL2(34,11),C'</poNumber>'                              
         DC    AL1(EORQ),AL2(4+10+24+11)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,7),C'</spot>'                                    
         DC    AL1(EORQ),AL2(4+7)                                               
                                                                                
         DC    AL1(SENQ),AL2(0,8),C'</spots>'                                   
         DC    AL1(EORQ),AL2(4+8)                                               
                                                                                
         DC    AL1(ENDQ)                                                        
*                                                                               
*                                                                               
*================================================================               
*  SPOT/BRANDS (CML/PROD) MESSAGE                                               
*================================================================               
*                                                                               
SBMSG    DS    0X                                                               
         DC    AL1(SHDQ),AL2(0,12),C'<spotBrands>'                              
         DC    AL1(EORQ),AL2(4+12)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,11),C'<spotBrand>'                               
         DC    AL1(EORQ),AL2(4+11)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,6),C'<adId>'                                     
         DC    AL1(FVDQ),AL2(6,12),AL2(ADBRID-ADBRD)                            
         DC    AL1(CONQ),AL2(18,7),C'</adId>'                                   
         DC    AL1(EORQ),AL2(4+6+12+7)                                          
                                                                                
         DC    AL1(CONQ),AL2(0,9),C'<brandId>'                                  
         DC    AL1(FVDQ),AL2(9,3),AL2(0)                                        
         DC    AL1(CONQ),AL2(12,10),C'</brandId>'                               
         DC    AL1(EORQ),AL2(4+9+3+10)                                          
                                                                                
         DC    AL1(CONQ),AL2(0,12),C'</spotBrand>'                              
         DC    AL1(EORQ),AL2(4+12)                                              
                                                                                
         DC    AL1(SENQ),AL2(0,13),C'</spotBrands>'                             
         DC    AL1(EORQ),AL2(4+13)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
*                                                                               
*                                                                               
*================================================================               
*  ORDER USER MESSAGE                                                           
*================================================================               
*                                                                               
ORMSG    DS    0X                                                               
         DC    AL1(CONQ),AL2(0,7),C'<order>'                                    
         DC    AL1(EORQ),AL2(4+7)                                               
                                                                                
         DC    AL1(CONQ),AL2(0,10),C'<priority>'                                
         DC    AL1(FVDQ),AL2(10,1),AL2(ORPRI-ORD)                               
         DC    AL1(CONQ),AL2(11,11),C'</priority>'                              
         DC    AL1(EORQ),AL2(4+10+1+11)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,10),C'<poNumber>'                                
         DC    AL1(SOPQ),AL2(0)                                                 
         DC    AL1(FVDQ),AL2(10,24),AL2(0)                                      
         DC    AL1(EOPQ),AL2(0)                                                 
         DC    AL1(CONQ),AL2(11,11),C'</poNumber>'                              
         DC    AL1(EORQ),AL2(4+10+24+11)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,7),C'<notes>'                                    
         DC    AL1(CONQ),AL2(7,9),C'<!CDATA'                                  
         DC    AL1(FVDQ),AL2(16,236),AL2(0)                                     
         DC    AL1(CONQ),AL2(251,3),C'ÙÙ>'                                      
         DC    AL1(CONQ),AL2(254,8),C'</notes>'                                 
         DC    AL1(EORQ),AL2(4+7+9+236+3+8)                                     
                                                                                
         DC    AL1(CONQ),AL2(0,18),C'<prodServicesFlag>'                        
         DC    AL1(FVDQ),AL2(18,5),AL2(0)                                       
         DC    AL1(CONQ),AL2(23,19),C'</prodServicesFlag>'                      
         DC    AL1(EORQ),AL2(4+18+5+19)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,13),C'<productCode>'                             
         DC    AL1(FVDQ),AL2(13,3),AL2(0)                                       
         DC    AL1(CONQ),AL2(16,14),C'</productCode>'                           
         DC    AL1(EORQ),AL2(4+13+3+14)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,8),C'</order>'                                   
         DC    AL1(EORQ),AL2(4+8)                                               
                                                                                
         DC    AL1(ENDQ)                                                        
*                                                                               
*                                                                               
*================================================================               
*  ORDER ITEMS MESSAGE                                                          
*================================================================               
*                                                                               
OIMSG    DS    0X                                                               
         DC    AL1(SHDQ),AL2(0,12),C'<orderItems>'                              
         DC    AL1(EORQ),AL2(4+12)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,11),C'<orderItem>'                               
         DC    AL1(EORQ),AL2(4+11)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,6),C'<adId>'                                     
         DC    AL1(FVDQ),AL2(6,12),AL2(ORIADID-ORID)                            
         DC    AL1(CONQ),AL2(18,7),C'</adId>'                                   
         DC    AL1(EORQ),AL2(4+6+12+7)                                          
                                                                                
         DC    AL1(CONQ),AL2(0,17),C'<destinationCode>'                         
         DC    AL1(FVDQ),AL2(17,7),AL2(0)                                       
         DC    AL1(CONQ),AL2(24,18),C'</destinationCode>'                       
         DC    AL1(EORQ),AL2(4+17+7+18)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,19),C'<firstTelecastDate>'                       
         DC    AL1(SOPQ),AL2(0)                                                 
         DC    AL1(FVDQ),AL2(19,10),AL2(0)                                      
         DC    AL1(EOPQ),AL2(0)                                                 
         DC    AL1(CONQ),AL2(29,20),C'</firstTelecastDate>'                     
         DC    AL1(EORQ),AL2(4+19+10+20)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,18),C'<lastTelecastDate>'                        
         DC    AL1(SOPQ),AL2(0)                                                 
         DC    AL1(FVDQ),AL2(18,10),AL2(0)                                      
         DC    AL1(EOPQ),AL2(0)                                                 
         DC    AL1(CONQ),AL2(28,19),C'</lastTelecastDate>'                      
         DC    AL1(EORQ),AL2(4+18+10+19)                                        
                                                                                
         DC    AL1(CONQ),AL2(0,14),C'<shippingType>'                            
         DC    AL1(FVDQ),AL2(14,1),AL2(0)                                       
         DC    AL1(CONQ),AL2(15,15),C'</shippingType>'                          
         DC    AL1(EORQ),AL2(4+14+1+15)                                         
                                                                                
         DC    AL1(CONQ),AL2(0,12),C'</orderItem>'                              
         DC    AL1(EORQ),AL2(4+12)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
*                                                                               
*                                                                               
*================================================================               
*  XML END MESSAGE                                                              
*================================================================               
*                                                                               
XMEMSG   DS    0X                                                               
         DC    AL1(CONQ),AL2(0,13),C'</orderItems>'                             
         DC    AL1(EORQ),AL2(4+13)                                              
                                                                                
         DC    AL1(CONQ),AL2(0,11),C'</shipList>'                               
         DC    AL1(EORQ),AL2(4+11)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
*                                                                               
*                                                                               
*================================================================               
*  END MESSAGE                                                                  
*================================================================               
*                                                                               
ENMSG    DS    0X                                                               
         DC    AL1(VENQ),AL2(0,30),AL2(0)                                       
         DC    AL1(EORQ),AL2(4+30)                                              
                                                                                
         DC    AL1(ENDQ)                                                        
*                                                                               
*                                                                               
**********************************************************************          
*        FLAT FILE ROUTINES TABLE                                               
**********************************************************************          
*                                                                               
FFROUT   DS    0D                                                               
*                                                                               
FLATAB   DC    C'HEDR',AL4(XMHD) XML HEADER                                     
         DC    C'AGNY',AL4(XMAG) XML AGENCY                                     
         DC    C'ADVR',AL4(XMAD) XML ADVERTISER                                 
         DC    C'BRND',AL4(XMBR) XML BRAND (PRODUCT)                            
         DC    C'SPOT',AL4(XMSP) XML SPOT (CML)                                 
         DC    C'SPBR',AL4(XMSB) XML SPOT/BRAND                                 
         DC    C'ORDR',AL4(XMOR) XML ORDER                                      
         DC    C'ORIT',AL4(XMOI) XML ORDER ITEM                                 
FLATABLN EQU   (*-FLATAB)/8                                                     
*                                                                               
AXMLMSG  DC     A(XMLMSG)          START OF XML                                 
AHDMSG   DC     A(HDMSG)           HEADER                                       
AAGMSG   DC     A(AGMSG)           AGENCY                                       
AADMSG   DC     A(ADMSG)           ADVERTISER                                   
ABRMSG   DC     A(BRMSG)           BRAND (PRODUCT)                              
ASPMSG   DC     A(SPMSG)           SPOT (CML)                                   
ASBMSG   DC     A(SBMSG)           SPOT/BRAND                                   
AORMSG   DC     A(ORMSG)           ORDER                                        
AOIMSG   DC     A(OIMSG)           ORDER ITEM                                   
AENDMSG  DC     A(ENMSG)           END MESSAGE                                  
AXMEMSG  DC     A(XMEMSG)          END OF XML                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
*==================================================                             
* XML START MESSAGE                                                             
*==================================================                             
*                                                                               
SXMLMSG  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,AXMLMSG                                                       
         BRAS  RE,PUTR                                                          
*                                                                               
         J     EXIT                                                             
*                                                                               
*==================================================                             
* WRAP XML HEADER MESSAGE                                                       
*==================================================                             
*                                                                               
XMHD     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AINRECP4         POINT TO ACTUAL DATA                         
*                                                                               
         L     R3,AHDMSG           POINT TO HEADER MESSAGE                      
         BRAS  RE,PUTR                                                          
*                                                                               
         J     EXIT2                                                            
*                                                                               
*                                                                               
*==================================================                             
* XML WRAP AGENCY MESSAGE                                                       
*==================================================                             
XMAG     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AINRECP4         POINT TO ACTUAL DATA                         
*                                                                               
         L     R3,AAGMSG           POINT TO AGENCY MESSAGE                      
         BRAS  RE,PUTR                                                          
*                                                                               
         J     EXIT2                                                            
*                                                                               
*                                                                               
*==================================================                             
* XML WRAP ADVERTISER MESSAGE                                                   
*==================================================                             
XMAD     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AINRECP4         POINT TO ACTUAL DATA                         
*                                                                               
         L     R3,AADMSG           POINT TO ADVERTISER MSG                      
         BRAS  RE,PUTR                                                          
*                                                                               
         J     EXIT2                                                            
*                                                                               
*                                                                               
*==================================================                             
* XML WRAP BRAND (PRODUTCT) MESSAGE                                             
*==================================================                             
XMBR     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AINRECP4         POINT TO ACTUAL DATA                         
*                                                                               
         L     R3,ABRMSG           POINT TO BRAND MSG                           
         BRAS  RE,PUTR                                                          
*                                                                               
         J     EXIT2                                                            
*                                                                               
*                                                                               
*==================================================                             
* XML WRAP SPOT (CML) MESSAGE                                                   
*==================================================                             
XMSP     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AINRECP4         POINT TO ACTUAL DATA                         
*                                                                               
         L     R3,ASPMSG           POINT TO SPOT MSG                            
         BRAS  RE,PUTR                                                          
*                                                                               
         J     EXIT2                                                            
*                                                                               
*==================================================                             
* XML WRAP SPOT/BRAND MESSAGE                                                   
*==================================================                             
XMSB     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AINRECP4         POINT TO ACTUAL DATA                         
*                                                                               
         L     R3,ASBMSG           POINT TO SPOT/BRAND MSG                      
         BRAS  RE,PUTR                                                          
*                                                                               
         J     EXIT2                                                            
*                                                                               
*                                                                               
*==================================================                             
* XML WRAP ORDER MESSAGE                                                        
*==================================================                             
XMOR     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AINRECP4         POINT TO ACTUAL DATA                         
*                                                                               
         L     R3,AORMSG           POINT TO ORDER MSG                           
         BRAS  RE,PUTR                                                          
*                                                                               
         J     EXIT2                                                            
*                                                                               
*==================================================                             
* XML WRAP ORDER ITEM MESSAGE                                                   
*==================================================                             
XMOI     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R4,AINRECP4         POINT TO ACTUAL DATA                         
*                                                                               
         L     R3,AOIMSG           POINT TO ORDER ITEM MSG                      
         BRAS  RE,PUTR                                                          
*                                                                               
         J     EXIT2                                                            
*                                                                               
*                                                                               
*==================================================                             
* END MESSAGE                                                                   
*==================================================                             
*                                                                               
PENDMSG  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,AENDMSG          POINT TO END MESSAGE                         
         BRAS  RE,PUTR                                                          
*                                                                               
         XC    SVENDMSG,SVENDMSG   CLEAR END MSG AREA                           
*                                                                               
         J     EXIT2                                                            
*                                                                               
*                                                                               
*==================================================                             
* XML END MESSAGE                                                               
*==================================================                             
*                                                                               
XMLENDMS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,AXMEMSG                                                       
         BRAS  RE,PUTR                                                          
*                                                                               
         J     EXIT                                                             
*                                                                               
*                                                                               
*==================================================                             
* PROCESS ROUTIN FROM MAP TABLE                                                 
*==================================================                             
PROUT    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,AROUT            POINT TO ROUTIN IN MAP TABLE                 
         BRAS  RE,PUTR                                                          
*                                                                               
         XC    AROUT,AROUT                                                      
         NI    NEWTYP,X'FF'-TYPNEQ  RESET SUB-TYPE CHANGED                      
*                                                                               
         J     EXIT2                                                            
*                                                                               
*                                                                               
*********************************************************************           
* GENERATE XML RECORDS FROM LINE ITEM                               *           
*  NTRY R3=A(XML MAP RECORD)                                        *           
*       R4=A(DATA ON FLAT FILE)                                     *           
*********************************************************************           
         USING MAPD,R3                                                          
PUTR     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
PUTR10   L     R0,AMAP             CLEAR IO TO SPACES                           
         LA    R1,L'MAP                                                         
         SR    RE,RE                                                            
         LA    RF,X'40'                                                         
         SLL   RF,24                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,AMAP                                                          
         XC    0(4,R2),0(R2)                                                    
         LA    R2,4(R2)            PAST RECORD LENGTH BYTES                     
*                                                                               
PUTR20   CLI   MAPTYP,FVDQ         VAR LEN DATA FROM FILE                       
         BNE   PUTR25                                                           
*                                                                               
         SR    R5,R5                                                            
         ICM   R5,3,MAPFLD                                                      
         AR    R5,R2               R5=A(OUTPUT FIELD)                           
                                                                                
         L     RF,AINREC                                                        
         SR    R1,R1                                                            
         ICM   R1,3,0(RF)          RECORD LEN                                   
         AR    RF,R1                                                            
         ST    RF,EOR              SAVE A(END OF RECORD)                        
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,MAPDATA        DISPLACEMENT TO DATA ON FILE                 
         LTR   RE,RE                                                            
         BNZ   *+14                                                             
         L     R1,ANEXTPOS         A(NEXT POS) TO DATA ON FILE                  
         LR    RE,R1               SAVE START OF DATA                           
         B     PF06                                                             
*                                                                               
         AR    RE,R4               POINT TO START OF DATA ON FILE               
         LR    R1,RE                                                            
*                                                                               
PF06     CLI   0(R1),FDELIM                                                     
         BE    PF06D                                                            
*                                                                               
         C     R1,EOR                                                           
         BNL   PUTR50              FIELD DELIMETER MISSING                      
*                                                                               
         LA    R1,1(R1)            NEXT POSITION                                
         B     PF06                                                             
*                                                                               
PF06D    LR    RF,R1               SAVE END OF DATA                             
                                                                                
         LA    R1,1(R1)            BUMP PAST SEMI-COLON                         
         ST    R1,ANEXTPOS         SAVE A(NEXT POSITION)TO DATA ON FILE         
                                                                                
         SR    RF,RE               GET LENGTH OF DATA                           
         BP    PF07                IF FIELD LEN=0                               
         MVI   VLEN,1              TREAT AS 1                                   
         MVI   VTXT,C' '                                                        
         B     PF08                                                             
*                                                                               
PF07     STC   RF,VLEN             LENGTH OF VARIABLE FIELD                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   VTXT(0),0(RE)       SAVE TEXT                                    
*                                                                               
PF08     ICM   R6,3,MAPLEN         R6=LENGTH OF DATA                            
         STC   R6,FLEN                                                          
         CLC   VLEN,FLEN                                                        
         BNH   *+6                                                              
         DC    H'0'                FIELD DATA TOO LONG                          
*                                                                               
         LLC   RF,VLEN             MOVE VARIABLE TEXT                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,(R5)),VTXT                                                   
*                                                                               
         BAS   RE,FIXBUMP          FIX DISP TO OUTPUT AND BUMP                  
         B     PUTR80                                                           
*                                                                               
PUTR25   CLI   MAPTYP,SOPQ         START OF OPTIONAL FIELD                      
         BNE   PUTR28                                                           
*                                                                               
         ICM   R5,3,MAPFLD         SAVE DISP TO OUTPUT                          
*                                                                               
         LA    R3,3(R3)            BUMP TO NEXT RECORD TYPE                     
         LR    R6,R3               SAVE PT TO RECORD IN MAP                     
*                                                                               
         BAS   RE,FDATA            FIND OPTIONAL DATA                           
         BNE   PUTR20              DATA NOT FOUND                               
*                                                                               
         LR    R3,R6               RESTORE PT IN MAP                            
         B     PUTR20              PROCESS THIS RECORD                          
*                                                                               
PUTR28   CLI   MAPTYP,EOPQ         END OF OPTIONAL FIELD                        
         BNE   PUTR40                                                           
         LA    R3,3(R3)            BUMP TO NEXT REC IN MAP                      
         B     PUTR20                                                           
*                                                                               
PUTR40   CLI   MAPTYP,VENQ         VAR LEN END MESSAGE                          
         BNE   PUTR50                                                           
*                                                                               
* GET MESSAGE LENGTH                                                            
         LA    RE,SVENDMSG         POINT TO MESSAGE                             
         LA    R1,L'SVENDMSG-1(RE)                                              
PUTR41   CLI   0(R1),X'40'                                                      
         BH    PUTR42                                                           
         CLI   0(R1),C'>'          END OF MSG                                   
         BE    PUTR42                                                           
         BCTR  R1,0                                                             
         B     PUTR41                                                           
*                                                                               
PUTR42   AHI   R1,1                ADJ LEN                                      
         SR    R1,RE               MESSAGE LEN                                  
         BP    *+6                                                              
         DC    H'0'                SHOULD NEVER GET HERE                        
*                                                                               
         STCM  R1,3,MAPLEN         SAVE MSG LEN                                 
*                                                                               
         STC   R1,VLEN             LENGTH OF VARIABLE FIELD                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   VTXT(0),0(RE)       SAVE TEXT                                    
*                                                                               
         LLC   RF,VLEN             MOVE VARIABLE TEXT                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,(R2)),VTXT      OUTPUT                                       
*                                                                               
         BAS   RE,FIXBUMP          FIX DISP TO OUTPUT AND BUMP IN MAP           
         B     PUTR80                                                           
*                                                                               
PUTR50   SR    R5,R5                                                            
         ICM   R5,3,MAPFLD                                                      
         AR    R5,R2               R5=A(OUTPUT FIELD)                           
         SR    R6,R6                                                            
         ICM   R6,3,MAPLEN         R6=LENGTH OF DATA                            
         STC   R6,VLEN                                                          
*                                                                               
         CLI   MAPTYP,SHDQ         CHK TO SKIP HEADER MESSAGE                   
         BNE   PUTR52                                                           
*                                                                               
         TM    NEWTYP,HEADTSW      PRINT HEADING                                
         BZ    *+12                                                             
         NI    NEWTYP,X'FF'-HEADTSW                                             
         B     PUTR55                                                           
*                                                                               
         BAS   RE,FIXBUMP          NO, BYPASS THIS RECORD                       
         B     PUTR90                                                           
*                                                                               
PUTR52   CLI   MAPTYP,SENQ         SAVE END MESSAGE                             
         BNE   *+12                                                             
         LA    R5,SVENDMSG         POINT TO SAVE MSG AREA                       
         B     PUTR55                                                           
*                                                                               
         CLI   MAPTYP,CONQ         IS DATA A CONSTANT ?                         
         BNE   PUTR60                                                           
*                                                                               
PUTR55   BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),MAPCON      MOVE CONSTANT TO OUTPUT                      
*                                                                               
         CLI   MAPTYP,SENQ         SAVE END MESSAGE                             
         BE    PUTRX                                                            
*                                                                               
         BAS   RE,FIXBUMP          FIX DISP TO OUTPUT AND BUMP IN MAP           
         B     PUTR80                                                           
*                                                                               
PUTR60   CLI   MAPTYP,EORQ                                                      
         BE    PUTR80                                                           
         CLI   MAPTYP,ENDQ         ARE WE ENDING OUTER LOOP                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,1(R3)                                                         
*                                                                               
PUTR80   CLI   0(R3),EORQ          END OF RECORD                                
         BNE   PUTR20                                                           
*                                                                               
         L     R2,AMAP                                                          
*                                                                               
         CLI   OUTMDE,C'D'         OUTPUT TO DATASET                            
         BNE   PUTR88                                                           
*                                                                               
         MVC   0(2,R2),1(R3)       SET RECORD LENGTH                            
*                                                                               
         PUT   XMLOUT,(R2)                                                      
         B     PUTR90                                                           
*                                                                               
PUTR88   ICM   R5,3,1(R3)                                                       
         SHI   R5,4                4 BYTES FOR LEN                              
         GOTO1 AMQRPT,DMCB,(0,=C'PUT'),4(R2),(R5),0                             
         CLI   DMCB+8,0                                                         
         BE    *+8                                                              
         B     MQERRX                                                           
*                                                                               
PUTR90   DS    0H                                                               
         LA    R3,3(R3)                                                         
         CLI   0(R3),ENDQ          TEST END OF LOOP                             
         BNE   PUTR10                                                           
         LA    R3,1(R3)                                                         
         B     PUTRX                                                            
*                                                                               
PUTRX    J     EXIT2                                                            
*                                                                               
MQERRX   XC    VTXT(100),VTXT                                                   
         MVC   VTXT(18),=CL18'AUTONOTE*SMURDDNY:'                               
         MVC   VTXT+18+1(20),=CL13'MQ PUT FAILED *USER'                         
         ICM   R1,15,DMCB+12                                                    
         MVC   VTXT+39(28),0(R1)                                                
         GOTO1 DATAMGR,DMCB,=C'OPMSG',(67,VTXT)                                 
*NOP1 >>************************************************                        
*  ADD CODE TO SEND NOTIFICATION TO CLIENT                                      
*NOP1 >>************************************************                        
         B     PUTRX                                                            
*                                                                               
*                                                                               
*---------------------------------------------------------------                
* FIX DISPLACEMENT TO OUTPUT AND BUMP TO NEXT ENTRY IN MAP TABLE                
*---------------------------------------------------------------                
*                                                                               
FIXBUMP  SR    R5,R5                                                            
         ICM   R5,3,MAPFLD         DISP TO OUTPUT                               
         LLC   R6,VLEN             FIELD LEN                                    
         AR    R5,R6                                                            
                                                                                
FDIS01   CLI   MAPTYP,SHDQ         SKIP HEADER MESSAGE                          
         BE    FDIS02                                                           
                                                                                
         CLI   MAPTYP,SENQ         SAVE END MESSAGE                             
         BE    FDIS02                                                           
                                                                                
         CLI   MAPTYP,CONQ                                                      
         BNE   *+14                                                             
FDIS02   LA    RF,MAPCON-MAPD(R6)  BUMP TO NEXT ENTRY IN TABLE                  
         AR    R3,RF                                                            
         B     *+8                                                              
         LA    R3,MAPLNQ(R3)       BUMP TO NEXT ENTRY IN TABLE                  
                                                                                
         CLI   0(R3),EOPQ          END OF OPTIONAL DATA                         
         BNE   *+8                                                              
         LA    R3,3(R3)                                                         
                                                                                
         CLI   0(R3),EORQ          IF END OF RECORD                             
         BNE   *+8                                                              
         AHI   R5,4                ADD 4 FOR REC LEN FIELD                      
         STCM  R5,3,MAPFLD                                                      
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
*==============================================                                 
* FIND OPTIONAL DATA ON FILE                                                    
* ON ENTRY R5=MAPFLD (DISP TO OUTPUT)                                           
*==============================================                                 
*                                                                               
FDATA    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
FDAT01   SR    R6,R6                                                            
         ICM   R6,3,MAPLEN                                                      
*                                                                               
         CLI   MAPTYP,EOPQ         END OF OPTIONAL DATA                         
         BNE   *+16                                                             
         LA    R3,3(R3)                                                         
         STCM  R5,3,MAPFLD         OUTPUT DISPLACEMENT                          
         B     FDATNE              DATA NOT FOUND                               
*                                                                               
         CLI   MAPTYP,FVDQ         FLAT FILE VARIABLE LEN DATA                  
         BE    FDAT10                                                           
*                                                                               
         CLI   MAPTYP,SHDQ         SKIP HEADER MESSAGE                          
         BE    FDAT02                                                           
                                                                                
         CLI   MAPTYP,SENQ         SAVE END MESSAGE                             
         BE    FDAT02                                                           
                                                                                
         CLI   MAPTYP,CONQ                                                      
         BNE   *+14                                                             
FDAT02   LA    RF,MAPCON-MAPD(R6)  BUMP TO NEXT ENTRY IN TABLE                  
         AR    R3,RF                                                            
         B     *+8                                                              
         LA    R3,MAPLNQ(R3)       BUMP TO NEXT ENTRY IN TABLE                  
*                                                                               
         CLI   MAPTYP,EORQ                                                      
         BNE   FDAT01                                                           
         LA    R3,3(R3)                                                         
*                                                                               
         CLI   MAPTYP,EOPQ         END OF OPTIONAL DATA                         
         BNE   FDAT01                                                           
         LA    R3,3(R3)            BUMP TO NEXT RECORD                          
         B     FDATNE              AND GET OUT                                  
*                                                                               
FDAT10   L     RF,AINREC                                                        
         SR    R1,R1                                                            
         ICM   R1,3,0(RF)          RECORD LEN                                   
         AR    RF,R1                                                            
         ST    RF,EOR              SAVE A(END OF RECORD)                        
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,MAPDATA        DISPLACEMENT TO DATA ON FILE                 
         LTR   RF,RF                                                            
         BNZ   *+14                                                             
         L     R1,ANEXTPOS         A(NEXT POS) TO DATA ON FILE                  
         LR    RE,R1               SAVE START OF DATA                           
         B     FDAT12                                                           
*                                                                               
         LR    R1,R4               START OF RECORD ON FILE                      
*                                  LOOK FOR FIELD DELIMITER                     
         CLI   4(R1),FDELIM1       5RD FIELD IS A (º)                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,5(R1)            BUMP PAST RECORD ID                          
         LR    RE,R1               SAVE START OF DATA                           
*                                                                               
FDAT12   DS    0H                                                               
         CLI   0(R1),FDELIM1                                                    
         BE    FDAT14                                                           
*                                                                               
         C     R1,EOR                                                           
         BNL   FDATNE              FIELD DELIMETER MISSING                      
*                                                                               
         LA    R1,1(R1)            NEXT POSITION                                
         B     FDAT12                                                           
*                                                                               
FDAT14   DS    0H                                                               
         LR    RF,R1               SAVE END OF DATA                             
                                                                                
         SR    RF,RE               GET LENGTH OF DATA                           
         BP    FDATEQ              DATA FOUND                                   
                                                                                
* DATA NOT FOUND BUMP TO END OF OPTION DATA (EOPQ)                              
         LA    R1,1(R1)            BUMP PAST º                                  
         ST    R1,ANEXTPOS         SAVE A(NEXT POSITION)TO DATA ON FILE         
         LA    R3,MAPLNQ(R3)       BUMP TO NEXT ENTRY IN TABLE                  
         CLI   MAPTYP,EORQ                                                      
         BNE   FDAT01                                                           
         LA    R3,3(R3)                                                         
*                                                                               
         CLI   MAPTYP,EOPQ         END OF OPTIONAL DATA                         
         BNE   FDAT01                                                           
         LA    R3,3(R3)                                                         
         STCM  R5,3,MAPFLD         OUTPUT DISPLACEMENT                          
*                                                                               
FDATNE   LTR   RB,RB               DATA NOT FOUND                               
         B     *+6                                                              
FDATEQ   CR    RB,RB                                                            
         XIT1  REGS=(R3)                                                        
         DROP  R3                                                               
*                                                                               
XMLOUT   DCB   DDNAME=XMLOUT,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               MACRF=PM,                                               X        
               EODAD=TEMPX                                                      
TEMPX    DS    0H                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDREPMASTD                                                     
         EJECT                                                                  
       ++INCLUDE DDREMOTED                                                      
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRSHIP                                                       
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRSTA                                                        
         EJECT                                                                  
       ++INCLUDE SPTRAGYCON                                                     
         EJECT                                                                  
       ++INCLUDE SPTRPRH                                                        
         EJECT                                                                  
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
STAMASD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
TWADCOND DSECT                                                                  
       ++INCLUDE DDTWADCONS                                                     
       ++INCLUDE SPTRAFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAD6D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE SPTRAWORKD                                                     
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE SPTRESHPD                                                      
         EJECT                                                                  
SYSD     DSECT            ******   SYSD CONTINUATION   ******                   
         ORG   SVSPAREX                                                         
SPTR46RR DS    A                                                                
ATSAR    DS    A                   TSAR CORE RESIDENT T00A5D/T00A7D             
ABTBLES  DS    A                   A(STORAGE) FOR OPTICA BIN TABLES             
ABTBLESX DS    A                    END OF TABLES                               
*                                                                               
*SM  **ESHIP                                                                    
*                                                                               
* PROD TABLE FOR XML INFO                                                       
APROD    DS    A                   ADDR OF PROD TABLE FOR BINSRCH               
MXPROD   DS    A                   END OF TABLE                                 
NPROD    EQU   255                 NUMBER OF RECORDS IN TABLE                   
*                                                                               
* CML TABLE FOR XML INFO                                                        
ACOMMLT  DS    A                   ADDR OF COMML TABLE FOR BINSRCH              
MXCOMMLT DS    A                   END OF TABLE                                 
NCOMMLT  EQU   3000                NUMBER OF RECORDS IN TABLE                   
*                                                                               
* CML/PRD TABLE FOR XML INFO                                                    
ACMLPRD  DS    A                   ADDR OF CML/PRD TABLE FOR BINSRCH            
MXCMPRD  DS    A                   END OF TABLE                                 
NCMLPRD  EQU   3000                NUMBER OF RECORDS IN TABLE                   
*                                                                               
* DESTINATION (STATION) TABLE FOR XML INFO                                      
ADEST    DS    A                   ADDR OF DEST TABLE FOR BINSRCH               
MXDEST   DS    A                   END OF TABLE                                 
NDEST    EQU   3000                NUMBER OF RECORDS IN TABLE                   
*                                                                               
SVCMLTBL EQU   BLOCK                                                            
CMLTSIZ  EQU   480/L'CMLTENT                                                    
*                                  HOLDS 20 10 BYTE ENTRIES                     
*                                  3=SEQ/4=TYPE/1=CLASS/1=PRD/1=LAST            
*                                  MULTI PRD CMMLS ARE NOT TABLED               
*                                                                               
TALID    DS    CL6                 TALENT AGENCY ID                             
SVCMLPRD DS    CL13                SAVE PRODS FROM CML RECORD                   
SVOPTICA DS    CL3                 SAVE OPTICA CODE FROM PRDHOUSE               
SVDELAY  DS    XL2                 AND DELAY IN MINUTES                         
TODAY    DS    XL3                                                              
STAXML   DS    CL7                 STATION (WABC-AM) OR (ABC FOR TV)            
SVXCOMML DS    CL(L'XCOMML)                                                     
*                                                                               
*SM                                                                             
**ESHIP*                                                                        
*                                                                               
SVCMT    DS    0CL236              COMMENT LINES FROM SCREEN 1-4                
SVCMT1   DS    CL58                                                             
         DS    CL1                                                              
SVCMT2   DS    CL58                                                             
         DS    CL1                                                              
SVCMT3   DS    CL58                                                             
         DS    CL1                                                              
SVCMT4   DS    CL58                                                             
         DS    CL1                                                              
*                                                                               
SVSECPID DS    CL8                 PID                                          
SVSECFNM DS    CL9                 FIRST NAME                                   
SVSECLNM DS    CL18                LAST NAME                                    
FCOUNT   DS    XL2                 FLAT FILE RECORD COUNT                       
SVLEN    DS    XL1                 CML SLN                                      
SVCMLSTR DS    XL3                                                              
SVCMLEND DS    XL3                                                              
SVPONUM  DS    CL24                PURCHASE ORDER NUMBER (PO)                   
SVPRI    DS    CL1                 OPTICA DELIVERY PRIORITY                     
SVPSR    DS    CL5 (true/false)    PRODUCTION SERVICES REQUIRED                 
SVEMAIL  DS    CL63                SAVE USER EMAIL ADDRESS                      
ENVIRNMT DS    CL3                 PRD/TST/CSC                                  
QUESNAME DS    CL4                 QUEUE SUB NAME                               
SVTTEST  DS    CL3                 STAGED (T=A,B OR C)                          
*                                                                               
TDATE8   DS   0CL8                 YYYYMMDD                                     
TDATE    DS    CL10                YYYY-MM-DD                                   
TIME     DS    CL11                HH:MM:SS.HH                                  
*                                                                               
*                                                                               
SUBCNTR  DS    XL2                 TSAR SUB-CLASS COUNTER FOR SORT              
*                                                                               
LINENUM  DS    XL2                 LINENUM                                      
*                                                                               
ENTLEN   DS    XL1                 ENTRY LENGTH                                 
OUTMODE  DS    CL1                 EBI INSTRUCTIONS MODE                        
ESMODE   DS    XL1                                                              
ESPARMS  DS    6F                                                               
*                                                                               
BINDMCB  DS    6F                                                               
BINPAR1  DS    6F                                                               
BINPAR2  DS    6F                                                               
BINPAR3  DS    6F                                                               
BINPAR4  DS    6F                                                               
BINPAR5  DS    6F                                                               
*                                                                               
TSARBUFL DS    A(BLSIZE)          BUFFER LENGTH FOR ### RECORDS <<<??           
TSARBUFF DS    A                   ADDRESS OF GETMAIN BUFFER                    
TSARBYTE DS    F                   CT OF TSAR BYTE SENT                         
TSARLAST DS    H                   LAST TSAR RECORD NUMBER                      
TSRKEYF  DS    H                   SAVE FIRST TSAR KEY NUMBER                   
TSRKEYL  DS    H                     AND LAST                                   
*                                                                               
ABLDESHP DS    A                   A(USER BLDESHP ROUTINE)                      
ACOMSUB  DS    A                   A(COMMON SUBROUTINE)                         
AMKFILE  DS    A                   A(MAKEFILE ROUTINE)                          
AMKXML   DS    A                   A(XML MAP ROUTINE)                           
*                                                                               
AFREC    DS    A                                                                
AFRECP4  DS    A                                                                
AFRECX   DS    A                                                                
ANXTPOS  DS    A                                                                
*                                                                               
BREC     DS    XL300               TSAR BUFFER RECORD                           
FREC     DS    XL300               OUTPUT FILE RECORD                           
*                                                                               
DONESW   DC    X'00'                                                            
HEADSW   EQU   X'80'               HEADER RECORD                                
ADVSW    EQU   X'40'               ADVERTISER                                   
AGYSW    EQU   X'20'               AGENCY                                       
ORDSW    EQU   X'10'               ORDER                                        
PARTSW   EQU   X'01'               PARTIAL RECORD                               
*                                                                               
FLAG1    DC    X'00'                                                            
HIDEFSW  EQU   X'80'               HIDEF                                        
CNTRCSW  EQU   X'40'               CNTRCUT                                      
*                                                                               
FLDH     DS    CL8                                                              
FLD      DS    CL40                                                             
*                                                                               
*SM **ESHIP                                                                     
*                                                                               
SVADILEN DS    XL1                                                              
SVCMLADI DS    CL12                                                             
SVADIWRK DS    CL12                                                             
SVCMLADP DS    XL8                                                              
SCANBLK  DS    CL256                                                            
*                                                                               
SVTSPROF DS    CL16                                                             
SVTSPR01 EQU   SVTSPROF+0          USE GENERIC STATION FOR MARKET               
SVTSPR02 EQU   SVTSPROF+1          PAGE BREAK BY STATION AFFILIATE              
SVTSPR03 EQU   SVTSPROF+2          FORCE 1 COMMERCIAL PER LIST                  
SVTSPR04 EQU   SVTSPROF+3          SUPPRESS COMMERCIAL TYPE                     
SVTSPR05 EQU   SVTSPROF+4          SUPPRESS COMMERCIAL COUNTS                   
*                                                                               
SVTWPROF DS    CL16                                                             
SVTWPR05 EQU   SVTWPROF+4                                                       
*                                                                               
PRDHSELS DS   0CL96                                                             
PRDHSEL1 DS    CL24                                                             
PRDHSEL2 DS    CL24                                                             
PRDHSEL3 DS    CL24                                                             
PRDHSEL4 DS    CL24                                                             
PRDHFAX  DS    CL19                 PROD HOUSE FAX NUMBER                       
SVMLBOX  DS    CL16                 MAILBOX FAX REQUEST                         
HOUSECD  DS    CL6                                                              
*                                                                               
OPTIONS  DS   0CL(ENDOPTS-SVOPTSW)                                              
SVOPTSW  DS    X                                                                
OPTTEST  EQU   X'80'               TEST RUN - NO UPDATE                         
OPTRERUN EQU   X'40'                                                            
OPTTA    EQU   X'20'               AUTO TURN AROUND FROM OFFLINE INSTR          
OPTSIZE  EQU   X'10'               DISPLAY TABLE SIZES                          
OPTDATE  EQU   X'08'               AS OF DATE                                   
OPTCPY2  EQU   X'04'               PRODUCE AGENCY COPY/FAX TOGETHER             
OPTOA    EQU   X'02'               AUTO TURN AROUND FROM ONLINE INSTR           
OPTDID   EQU   X'01'               DID A REPORT - IF NOT ON, NO DATA            
*                                                                               
SVOPTSW1 DS    X                                                                
OPTHDCPY EQU   X'80'               HARD COPY - AGENCY COPY                      
*                                                                               
SVOPTCLS DS    CL1                 COMML CLASS FILTER                           
*                                                                               
SVOPTDTE DS    CL3                                                              
SVCMLLN  DS    XL1                                                              
SVOPTCML DS    CL8                                                              
SVOPTAFF DS    CL3                                                              
*                                                                               
SVOPTMKT DS    XL2                                                              
ENDOPTS  EQU   *                                                                
*                                                                               
*                                  END OF OPTIONS                               
*                                                                               
PQPROF   DS    A                                                                
ASTLIST  DS    A                   START OF STATION LIST                        
ASTLISTX DS    A                   END OF STATION LIST                          
ASHPSUM  DS    A                   START OF CML SHIPPING SUMMARY                
ASHPSUMX DS    A                   END OF CML SHIPPING SUMMARY                  
NEXTADDR DS    A                   ADDR OF START OF NEXT LIST                   
ACMLST   DS    A                   START OF COMMERCIAL LISTS                    
ACMLSTX  DS    A                   END OF COMMERCIAL LISTS                      
SVACML   DS    A                                                                
FIRSTLIN DS    A                                                                
VTRPACK  DS    A                                                                
STALINES DS    H                                                                
SVSTCMLT DS    CL4                 CMML TYPE FROM STATION REC                   
SVSTAFF  DS    CL3                 STATION AFFILIATE FROM STATION REC           
SVCMLTYP DS    CL4                 CMML TYPE FROM COMML REC                     
SVCMLCLS DS    CL4                 CMML CLASS FROM COMML REC                    
SVCMML2  DS    CL8                                                              
*                                                                               
PROFKEY  DS    CL5                 READ PROGRAM PROFILE RECORD                  
SVQLTYP1 DS    CL1                 STORE ARCHIVE SETTING                        
SVFAXARC DS    CL1                 ARCHIVE SETTING FOR FAX COPIES               
*                                                                               
QUESTOR  DS    CL24       1ST 12 ARE FROM TRACONT, ALL 24 AND                   
CONTEL   DS    CL18       TEL ARE FROM AGENCY CONTACT REC                       
CONFAX   DS    CL18       FAX ALSO FROM AGENCY CONTACT REC                      
CONEMAIL DS    CL50                                                             
*                                                                               
REQPRD   DS    CL3        ORIGINAL REQUESTED PRODUCT                            
*                                                                               
*                                                                               
* USED TO SUBTOTAL COMMLS WITH DIFF TYPES *                                     
*                                                                               
LSTCML   DS    CL16                                                             
LSTCMLCT DS    H                                                                
LSTCMLNO DS    XL1                                                              
ONECMML  DS    C                                                                
MKTSW    DS    C                                                                
MKTL     DS    CL4                                                              
MKTLNM   DS    CL24                                                             
MKTR     DS    CL4                                                              
MKTRNM   DS    CL24                                                             
*                                                                               
TABLSIZE DS    XL1                 0 = ALL TABLES LARGE ENOUGH                  
STASIZER EQU   X'80'                80 STATION TABLE TOO SMALL                  
CMLSIZER EQU   X'40'                40 CML LISTS TABLE TOO SMALL                
SUMSIZER EQU   X'20'                20 CML SHIPPING SUMMARY TOO SMALL           
*                                                                               
AMSGCLEN EQU   L'AMSGCTAB/L'STAAMSGC                                            
AMSGCTAB DS    CL(100*L'STAAMSGC)                                               
*                                                                               
*SM                                                                             
         DS    0D                                                               
TSARBLK  DS    CL(TSARDL2)                                                      
*SM                                                                             
DYNAWORK DS    0D                  FIRST 5000 BYTES FOR SHIP SUM DATA           
         EJECT                                                                  
*SM *ESHIP                                                                      
*                                                                               
* BINSRCH TABLE PRODUCT DSECT FOR XML                                           
                                                                                
XPRODD   DSECT                                                                  
XPRODID  DS    XL2                 PROD SUB-COUNTER ID                          
XPROD    DS    CL3                 PRODUCT                                      
XPRDNXT  EQU   *-XPRODID                                                        
*                                                                               
* BINSRCH TABLE COMMERCIAL DSECT FOR XML                                        
                                                                                
XCOMMLD  DSECT                                                                  
XCOMMID  DS    XL2                 COMMERCIAL SUB-COUNTER ID                    
XCOMML   DS    CL12                COMMERCIAL CODE                              
XCOMNXT  EQU   *-XCOMMID                                                        
*                                                                               
* BINSRCH TABLE CML/PRD DSECT FOR XML                                           
                                                                                
XCMLPRDD DSECT                                                                  
XCMPRID  DS    XL2                 CML/PRD SUB-COUNTER ID                       
XCMLPRD  DS   0CL15                CML/PRD                                      
XCML     DS    CL12                COMMERCIAL CODE                              
XPRD     DS    CL3                 PRODUCT                                      
XCPRNXT  EQU   *-XCMPRID                                                        
*                                                                               
* BINSRCH TABLE DESTINATION (STATION) DSECT FOR XML                             
                                                                                
XDESTD   DSECT                                                                  
XDESTID  DS    XL2                 DESTINATION SUB-COUNTER ID                   
XDESCML  DS   0CL19                                                             
XDEST    DS    CL7                 DESTINATION (STATION)                        
XDCML    DS    CL12                CML                                          
XDESTNXT EQU   *-XDESTID                                                        
*                                                                               
* FILE NAME DSECT                                                               
*                                                                               
FNAME    DSECT                                                                  
FNAGY    DS    CL3                 AGENCY ALPHA (2 CHAR ALPHA+1 SPACE)          
FNENV    DS    CL3                 ENVIRONMENT (TST/CSC/PRD)                    
FNTDTE   DS    CL8                 TODAY'S DATE YYYYMMDD                        
FNTIME   DS    CL8                 TIME HHMMSSHS                                
FNSEQ    DS    CL2                 SEQUENCE 00 (NOT SUPPORTED)                  
FNLEN    EQU   *-FNAGY                                                          
*                                                                               
*                                                                               
* ENVIRONMENT DSECT                                                             
*                                                                               
ENVD     DSECT                                                                  
ENVOPT   DS    CL3                 OPTION                                       
ENVDSP   DS    CL1                 DSPACE                                       
ENVXML   DS    CL3                 XML ENVIRONMENT                              
ENVQUE   DS    CL4                 VARIABLE PART OF QUEUE NAME                  
ENVLEN   EQU   *-ENVD                                                           
*                                                                               
*SM *ESHIP                                                                      
* DSECT FOR CMML DATA BUILT IN ELEM ON GETCMML CALL                             
*                                                                               
GETCMMLD DSECT                                                                  
GETCMT1  DS    CL15                TITLE 1                                      
GETCMOV1 DS    XL1                 SLN OVRD 1                                   
GETCMOV2 DS    CL1                 SLN OVRD 2                                   
GETCMT2  DS    CL20                TITLE 2                                      
GETCMT3  DS    CL20                TITLE 3                                      
GETCMTYP DS    CL4                 TYPE                                         
GETCMCLT DS    CL20                CLIENT CMML                                  
GETCMADI DS    CL12                ADID                                         
GETCMHDF DS    CL12                HIDEF CMML                                   
GETCMCTR DS    CL12                CENTERCUT CMML                               
*                                                                               
* DSECT FOR SHIPPING SUMMARY DATA ENTRIES *                                     
*                                                                               
CTABD    DSECT                                                                  
CTABENT  DS    0CL68                                                            
CTABSORT DS   0CL(CTABNUM-CTABCMLS)                                             
CTABCMLS DS    CL16                                                             
CTABTYPE DS    CL4                 CMML TYPE (CAN BE FROM STATION)              
CTABNUM  DS    XL2                 NUMBER TO SHIP EACH STA                      
CTABTOT  DS    XL2                 TOTAL NUMBER TO BE SHIPPED                   
CTABCLT  DS    CL20                                                             
CTABHDF  DS    CL12                HIDEF CMML                                   
CTABCTR  DS    CL12                CENTERCUT CMML                               
CTABNEXT EQU   *                                                                
*                                                                               
* DSECT FOR STATION LIST DATA *                                                 
*                                                                               
STALISTD DSECT                                                                  
*                                                                               
STLDATA  DS    0XL16                                                            
STLACML  DS    XL4                 ADDRESS OF CMML LIST                         
STLMKT   DS    XL2                                                              
STLSTA   DS    XL3                                                              
STLFTD   DS    XL3                                                              
STLAFF   DS    CL3                                                              
STLAMSGC DS    XL1                 TABLE NUMBER FOR AMS GROUP CODE              
STLNEXT  DS    0C                                                               
*                                                                               
* DSECT FOR COMMERCIAL LIST DATA *                                              
*                                                                               
CMLLISTD DSECT                                                                  
*                                                                               
CMLLSTLN DS    XL2                                                              
CMLAFF   DS    CL3                                                              
CMLCMMLS DS    0CL20               8-CMML, 8-P/B CMML, 4 CMML TYPE              
*                                                                               
*                                                                               
* DSECT FOR COMMERCIAL TABLE LOOK UP TABLE *                                    
*                                                                               
CMLTABD  DSECT                                                                  
*                                                                               
CMLTENT  DS   0XL23                                                             
CMLTSEQ  DS    XL2                                                              
CMLTTYP  DS    XL4                                                              
CMLTCLS  DS    XL1                                                              
CMLTPRD  DS    XL1                                                              
CMLTCML2 DS    XL8                                                              
CMLTTYP2 DS    XL4                                                              
CMLTCLS2 DS    XL1                                                              
CMLTPRD2 DS    XL1                                                              
CMLTUSD  DS    XL1                                                              
CMLTNEXT EQU   *                                                                
         EJECT                                                                  
GEND     DSECT                                                                  
         ORG   ELEM                                                             
REQHDR   DS    CL26                                                             
REQUEST  DS    CL80                                                             
REQSPOOK DS    CL64                                                             
*                                                                               
*********************************************************************           
* LOCAL WORKING STORAGE FOR MAKEXML                                 *           
*********************************************************************           
                                                                                
LWSD     DSECT                                                                  
*                                                                               
AMQRPT   DS    A                                                                
*                                                                               
AMAP     DS    A         **ESHIP                                                
AINREC   DS    A                                                                
AINRECP4 DS    A                                                                
ANEXTPOS DS    A                                                                
AROUT    DS    A                   A(START OF MAP DATA)                         
EOR      DS    A                                                                
*                                                                               
VLEN     DS    X                                                                
VTXT     DS    XL300                                                            
FLEN     DS    X                                                                
OUTMDE   DS    CL1                 XML INSTRUCTIONS MODE                        
*                                                                               
MAP      DS    CL(L'BREC)                                                       
INREC    DS    XL(L'BREC)          OUTPUT FILE RECORD                           
*                                                                               
NEWTYP   DS    CL1                 NEW RECORD TYPE FLAG                         
HEADTSW  EQU   X'80'               PUT OUT HEADING                              
TYPNEQ   EQU   X'10'               REC TYPE NOT EQ                              
*                                                                               
SVRECTYP DS    CL4                 SAVE FLAT FILE RECORD TYPE                   
SVSTRMSG DS    CL30                SAVE START MESSSAGE AREA                     
SVENDMSG DS    CL30                SAVE END MESSAGE AREA                        
*                                                                               
FDELIM1  EQU   X'6A'               FIELD DELIMITER  (º)                         
*                                                                               
LWSX     EQU   *                                                                
*                                                                               
*                                                                               
SPOOLD   DSECT                                                                  
*                                                                               
         ORG   P                                                                
PCOMML   DS    CL13                                                             
         DS    CL3                                                              
POTHER   DS    CL12                                                             
         DS    CL3                                                              
PTYPE    DS    CL4                                                              
         DS    CL2                                                              
PTITLE1  DS    CL15                                                             
         DS    CL2                                                              
PTITLE2  DS    CL20                                                             
         DS    CL2                                                              
PTITLE3  DS    CL20                                                             
         DS    CL2                                                              
PCOMMLCT DS    CL4                                                              
         ORG   P                                                                
*                                                                               
       ++INCLUDE EDIDESTD                                                       
*                                                                               
         ORG   P                                                                
*                                                                               
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE SEACSFILE                                                      
       ++INCLUDE FASSBOFF                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017SPTRA46   02/26/19'                                      
         END                                                                    
