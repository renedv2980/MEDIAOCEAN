*          DATA SET SPWRI14    AT LEVEL 018 AS OF 02/22/21                      
*PHASE T20414A,*                                                                
*INCLUDE PRNTBL                                                                 
         TITLE 'T20414 - STATION LOCKIN REPORT'                                 
*                                                                               
***********************************************************************         
*                                                                     *         
*          SPWRI14  (T20414) - SPOT WRITER STATION LOCKIN REPORT      *         
*                                                                     *         
***********************************************************************         
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-42845  02/22/21 NEW MARKET LOCK OPTIONS FOR SL            *         
* AKAT SPEC-33698  11/19/19 SUPPORT FOR 2 DECIMAL IMPRESSIONS         *         
* AKAT SPEC-35894  10/21/19 ADD COMSCORE DEMO NAME ELEMENT            *         
* AKAT SPEC-19808  02/16/18 COMSCORE SUPPORT FOR SL REPORT            *         
***********************************************************************         
*                                                                   *           
* 17DEC12 14 AKT -- SUPPORT FOR DAILY ESTIMATES                     *           
* 17AUG11 13 AKT -- FIX BUG WITH HELLO CALL                         *           
* 23DEC10 12 AKT -- FIX CLIENT RECORD BUG                           *           
* 28JAN10 11 AKT -- REMOVE CODE FOR DEFUCNT AGENCIES                *           
* 21MAY07 10 EFJ -- NO BUY COPY REQ'S FOR MEDIA X                   *           
* 22AUG05 09 EFJ -- HELP FOR 2 DECIMAL DEMOS SUPPORT                *           
* 01FEB05 08 AKT -- 2 DECIMAL DEMOS SUPPORT                         *           
* 03NOV03 07 AKT -- FIX MGROUP X'40' BUGS                           *           
* 27MAR02 05 EFJ -- SET TWAWHEN/SPOOKWEN FOR UPDATIVE SOON          *           
* 15MAY01 04 EFJ -- UPDATIVE SOON/BUY COPY SOON                     *           
*                -- RE-DO CURRENT LOCKIN DELETES S.T. EACH REC IS   *           
*                   UPDATED ONLY ONCE (UPDATIVE SOON PROCESSING)    *           
*                -- MOVE SAVED DCB TO SPFUSER                       *           
* 31JAN01 04 YKVA-- SL AGREES W/COS2                                *           
* 11NOV00 03 EFJ -- COPY CK CODE FOR JM                             *           
* 26OCT00 02 EFJ -- FIX DEMO CHANGED PROBLEM (DELETE ANY LOCKIN     *           
*                   ELEM OUTSIDE OF ESTIMATE DATES)                 *           
*--------------- LEVEL RESET  --------------------------------------*           
* 25OCT00 101EFJ -- SET GLOPTS+4 EARLY!                             *           
* 06SEP00    YYUN - CHANGE TO XSPFIL (SPGENXLK) RECORDS             *           
* **NOP** 99 EFJ -- CHANGE DEMO CATEGORIES - NO ERROR FOR CHANGE    *           
* 10FEB99 93 EFJ -- CLEAR SVDEMS                                    *           
* 15JAN99 ?? EFJ -- DON'T RESTORE SAVVALS WHEN ONLINE               *           
* 22SEP98 85 AMB -- GENERATE SBY REQUEST FILE IF PROFILE SET.       *           
* 10MAR98 83 EFJ -- CALL SETDATE TO PROPERLY SET SBQSTART/END ETC.  *           
* 30JUL97 82 EFJ/MH -- IF EST HAS NO DEMOS, MOVE IN 0 DEMO          *           
*                -- DROP INACTIVE USINGS                            *           
* 08APR96 60 AJR -- UPDATE BUYING GUIDELINE STATUS BYTE             *           
* 27DEC95 56 EFJ -- TOMBSTONE ADDED                                 *           
*                                                                   *           
*********************************************************************           
T20414   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20414,RA                                                      
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING SYSD,R9             SYSTEM SPECIFIC WORK                         
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7                                                   
*                                                                               
         L     R1,ASPOOLD                                                       
         MVC   SBPRINT,VPRINT-SPOOLD(R1)                                        
*                                                                               
*                                  REPORT CALLING MODE                          
         L     R1,TWADCONS                                                      
         L     R1,TSPFUSER-TWADCOND(R1)                                         
         ST    R1,ASAVE                                                         
*                                  REPORT CALLING MODE                          
         CLI   RPMODE,RPRNFRST     RUNFRST                                      
         BE    FRST                                                             
         CLI   RPMODE,RPINIT       INITIALIZATION                               
         BE    INIT                                                             
         CLI   RPMODE,RPVAL        VALIDATION                                   
         BE    VALID                                                            
         CLI   RPMODE,RPINPUT      INPUT MODE                                   
         BE    INPUT                                                            
         CLI   RPMODE,RPDRHOOK     DRIVER HOOK                                  
         BE    DRHOOK                                                           
         CLI   RPMODE,RPAFTOUT     AFTER DRIVER OUTPUT                          
         BE    AFTOUT                                                           
         CLI   RPMODE,RPFINAL      FINAL CALL                                   
         BE    FINAL                                                            
         CLI   RPMODE,RPRUNLST     REQUEST LAST                                 
         BE    RUNLST                                                           
         B     XIT                                                              
*                                                                               
EQXIT    CR    RB,RB                                                            
         B     XIT                                                              
*                                                                               
NEXIT    LTR   RB,RB                                                            
         B     XIT                                                              
*                                                                               
XIT      XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* RUNFRST                                                             *         
***********************************************************************         
         SPACE 1                                                                
FRST     DS    0H                                                               
         MVI   RPMODE,RPSTOP       SKIP RUNFRST IN WRI01!                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     DS    0H                                                               
         L     R1,ASAVE                                                         
         LA    RE,STWORK                                                        
         LA    RF,STWORKLN                                                      
         XCEF                                                                   
         ST    R1,ASAVE                                                         
*                                                                               
         TM    OPTIND2,OPTINOUP    TEST WRITE=NO                                
         BZ    *+8                                                              
         MVI   TWAWRITE,C'N'       YES-TELL GENCON                              
*                                                                               
         MVI   SBQSKIP,SBQSKGL+SBQSKBIL   READ BUY RECORDS ONLY                 
         MVI   SBQSEPES,C'Y'       ESTIMATES ARE SEPARATE                       
         CLI   SBQDATA,0           TEST DATA OPTION SET                         
         BNE   *+8                                                              
         MVI   SBQDATA,SBQDPUR     NO-DEFAULT IS PURCHASED                      
         MVI   SBQPER,SBQPWK       PERIODS ARE WEEKS                            
         MVI   SBQPERLO,1          ALL WEEKS WITHIN PERIOD                      
         MVI   SBQPERHI,X'FF'                                                   
         MVI   SBEUNALL,C'N'       EXCLUDE UNALLOCATED SPOTS                    
         OI    DATAIND,DILEN       REPORT BY SPOT LENGTH                        
         OI    DATAIND4,DIDEMHED   DEMO NAMES REQUIRED                          
         CLI   SBQDATA,SBQDPUR                                                  
         LAY   R1,CLTREC           CLIENT RECORD I/O AREA                       
         ST    R1,SBACLTRC         KEEP CLIENT RECORD IN THIS PROGRAM           
*                                                                               
         MVI   SVPRD,0             CLEAR SAVE AREAS                             
         MVI   SVEST,0                                                          
         MVI   SVOWD,0                                                          
         LA    R0,PRDESTMX                                                      
         LA    R1,PRDESTTB                                                      
         ST    R1,NXTENTRY                                                      
         XC    0(3,R1),0(R1)                                                    
         LA    R1,3(R1)                                                         
         BCT   R0,*-10                                                          
***                                                                             
* READ AGENCY LEVEL 00 PROFILE FOR 2 DECIMAL DEMOS                              
***                                                                             
         XC    WORK,WORK           PREPARE FOR PROFILE READ                     
         MVC   WORK(4),=C'S000'                                                 
         MVC   WORK+4(2),SBQAGY    GET AGENCY LEVEL 00 PROFILE                  
         GOTO1 GETPROF,DMCB,(X'90',WORK),WORK+16,DATAMGR                        
         CLI   WORK+25,C'Y'        2 DECIMAL RATINGS?                           
         BNE   INIT00A             NO - CANNOT HAVE 2 DEC IMPS THEN             
*                                                                               
         LA    R1,SBLOCK                                                        
         OI    SBEFLAG4-SBLOCK(R1),SBE42DEC      YES                            
         OI    COLIND,COLINDR      DEMO ROUNDING                                
***                                                                             
* READ AGENCY LEVEL 00A PROFILE FOR 2 DECIMAL IMPRESSIONS                       
***                                                                             
INIT00   XC    WORK,WORK           PREPARE FOR PROFILE READ                     
         MVC   WORK(4),=C'S00A'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE A               
         MVC   WORK+4(2),SBQAGY    GET AGENCY LEVEL 00 PROFILE                  
         GOTO1 GETPROF,DMCB,(X'90',WORK),WORK+16,DATAMGR                        
         CLI   WORK+22,C'Y'        2 DECIMAL IMPRESSIONS?                       
         BNE   INIT00A             NO                                           
*                                                                               
         LA    R1,SBLOCK                                                        
         OI    SBEFLAG9-SBLOCK(R1),SBE92DEC                                     
*                                                                               
INIT00A  XC    WORK,WORK           READ SL PROFILE                              
         MVC   WORK(4),=C'S0SL'                                                 
         MVC   WORK+4(2),SBQAGY                                                 
         MVC   WORK+6(1),SBQMED                                                 
         MVC   WORK+7(3),SBQCLT                                                 
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SBCOFF                                                
         GOTO1 GETPROF,DMCB,WORK,SLPROF,DATAMGR                                 
*                                                                               
         TM    SBQPIND,SBQPPB+SBQPNOPB   TEST PBSPLIT OPTION SET                
         BNZ   *+16                                                             
         CLI   SLPROF,C'N'         NO-TEST PROFILE SAYS NOT TO SPLIT            
         BNE   *+8                    PB'S                                      
         OI    SBQPIND,SBQPPB      YES                                          
*                                                                               
         MVI   NDEMOS,1            NUMBER OF DEMOS (DEFAULT=1)                  
         LA    R2,SLODEMH                                                       
         CLI   5(R2),0             TEST SCREEN OPTION SET                       
         BNE   INIT1                                                            
         CLI   SLPROF+1,0          NO-PICK UP N'DEMOS FROM PROFILE              
         BE    INIT2                  IF IT'S THERE                             
         MVC   NDEMOS,SLPROF+1                                                  
         B     INIT2                                                            
*                                                                               
INIT1    MVC   NDEMOS,8(R2)                                                     
         CLI   8(R2),C'1'                                                       
         BL    EINV                                                             
         CLI   8(R2),C'4'                                                       
         BH    EINV                                                             
*                                                                               
INIT2    NI    NDEMOS,X'0F'                                                     
         LA    R2,SLOTITH          TITLE                                        
         MVC   TITLE,SPACES                                                     
         MVC   TITLE(21),=C'STATION LOCKIN REPORT'                              
         CLI   5(R2),0                                                          
         BE    INIT4                                                            
         GOTO1 ANY                                                              
         MVC   TITLE,WORK                                                       
*                                                                               
INIT4    GOTO1 CENTER,DMCB,TITLE,63                                             
*                                                                               
         CLI   SBQPGRD,C' '        SET LEVELS                                   
         BH    *+14                                                             
         XC    RPTLEVS+3(2),RPTLEVS+3  SET PRODUCT GROUPS                       
         B     INIT6                                                            
         CLC   SBPGR1LN,SBPGR2LN                                                
         BNE   INIT6                                                            
         MVI   RPTLEVS+4,0                                                      
*                                                                               
INIT6    LA    R1,LEVELS           SET THE LEVELS                               
         LA    RE,RPTLEVS                                                       
         LA    RF,1                                                             
INIT8    CLI   0(RE),X'FF'                                                      
         BE    INIT12                                                           
         CLI   0(RE),0                                                          
         BE    INIT10                                                           
         MVC   0(1,R1),0(RE)                                                    
         CLI   0(R1),QEST                                                       
         BNE   *+8                                                              
         STC   RF,LSTHEDLV         LAST HEADLINE LEVEL                          
         LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
INIT10   LA    RE,1(RE)                                                         
         B     INIT8                                                            
*                                                                               
INIT12   MVI   MYFIRSTH,11         SET DRIVER'S FIRST HEADLINE                  
*                                                                               
         MVI   FRSTLAST,C'Y'       REQUEST REQ FIRST AND LAST                   
         MVI   DCBOPEN,C'N'        NOT OPEN YET                                 
         CLI   TWAFIRST,0          TEST FIRST REQUEST                           
         BNE   INIT14                                                           
         CLI   OFFLINE,C'Y'                                                     
         BNE   INITX                                                            
*                                                                               
         GOTO1 COVAIL,DMCB,C'GET',A(RECTABLN),A(RECTABLN)                       
         ICM   R4,15,4(R1)         GET FREE STORAGE                             
         BNZ   *+6                                                              
         DCHO                                                                   
         ST    R4,ARECTAB                                                       
         ST    R4,ANXTREC                                                       
         L     R1,=A(RECTABLN)                                                  
         AR    R4,R1                                                            
         ST    R4,ARECEND                                                       
*                                                                               
         L     R1,TWAMASTC                                                      
         USING MASTD,R1                                                         
         OC    MCREMPQK,MCREMPQK   TEST SOON                                    
         BNZ   INITX                YES - DON'T OPEN FILE                       
         DROP  R1                                                               
*                                                                               
         L     R4,ASAVE            OPEN TEMP FILE                               
         AHI   R4,SAVVALSL         SAVE DCB IN SPFUSER AFTER SAVVALS            
         ST    R4,ABYRQDCB                                                      
         MVC   0(BYRQFILL,R4),BYRQFIL                                           
*                                                                               
         OPEN  ((R4),OUTPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   DCBOPEN,C'Y'        YES-DCB OPEN                                 
         B     INITX                                                            
*                                                                               
INIT14   L     RE,ASAVE            NO-RESTORE SAVED VALUES                      
         LA    RF,SAVVALSL                                                      
         LA    R0,SAVVALS                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,ARECTAB          AND CLEAR RECTAB BETWEEN REQUESTS            
         ST    R0,ANXTREC                                                       
         L     R1,=A(RECTABLN)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
INITX    B     XIT                                                              
         SPACE 2                                                                
RPTLEVS  DC    AL1(QMED)           HEADLINES                                    
         DC    AL1(QCLT)                                                        
         DC    AL1(QSTA)                                                        
         DC    AL1(QPRDGR1)                                                     
         DC    AL1(QPRDGR2)                                                     
         DC    AL1(QPRD)                                                        
         DC    AL1(QEST)                                                        
         DC    AL1(QDPT)           DETAIL                                       
         DC    AL1(QLEN)                                                        
         DC    AL1(QLEN)                                                        
         DC    AL1(QPER)                                                        
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* FURTHER REQUEST VALIDATION                                          *         
***********************************************************************         
         SPACE 1                                                                
VALID    NI    COLIND,255-COLIRND  IGNORE ROUND OPTION                          
         LA    R2,SLOCLTH                                                       
         OC    SBQBCLT,SBQBCLT     SINGLE CLIENT ONLY                           
         BZ    EINV                                                             
         LA    R2,SLOPRDH                                                       
         CLC   SBQPRD,=C'POL'      PRODUCT POL IS INVALID                       
         BE    EINV                                                             
         CLI   SBQBPRD2,0          PARTNER REQUEST PRODUCT INVALID              
         BNE   EINV                                                             
         CLI   SBQBPRD,0                                                        
         BE    *+12                                                             
         TM    SBQPIND,SBQPPB      PRD=ALL REQUIRED FOR PBSPLIT=N               
         BO    EPIG                                                             
         LA    R2,SLOMKTH                                                       
         CLI   SBQMGRD,0           MARKET GROUPS ARE INVALID                    
         BH    EINV                                                             
         LA    R2,SLOSTAH                                                       
         CLI   SBQSGRD,C' '        STATION GROUPS ARE INVALID                   
         BH    EINV                                                             
*                                                                               
         LA    R2,SLOOPTH          VALIDATE VARIOUS OPTIONS                     
         CLI   SBQSPILL,C'N'       SPILL IS INVALID                             
         BNE   ESPILL                                                           
         TM    SBQPIND,SBQPOLAL    POL=BOTH INVALID                             
         BO    EPOL                                                             
         OC    SBQDEMOS,SBQDEMOS   MENU OPTION INVALID                          
         BNZ   EMENU                                                            
         OC    SBPDEMOS,SBPDEMOS   DEMOS OPTION INVALID                         
         BNZ   EDEMOPT                                                          
         CLI   SBQDATA,SBQDPUR     VALIDATE DATA OPTION                         
         BNE   *+12                                                             
         OI    DATAIND,DIDEMP                                                   
         B     VALID10                                                          
         CLI   SBQDATA,SBQDRERT                                                 
         BNE   *+12                                                             
         OI    DATAIND,DIDEMR                                                   
         B     VALID10                                                          
         CLI   SBQDATA,SBQDAFFD                                                 
         BNE   EDATA                                                            
         OI    DATAIND,DIDEMA                                                   
*                                                                               
VALID10  DS    0H                                                               
         CLI   SLPROF+2,C'Y'       AUTO SBY PROFILE SET                         
         BNE   VALID20                                                          
         CLC   AGENCY,=C'CK'       SKIP FOR COKE                                
         BE    VALID20                                                          
         CLC   SBQEST,SBQESTND     REQUEST FOR 1 ESTIMATE                       
         BNE   EESTSBY             MUST SPECIFY EST FOR AUTO SBY                
*                                                                               
VALID20  TM    WHEN,X'20'          IF RUNNING SOON,                             
         BZ    *+8                                                              
         MVI   TWAWHEN,5            SET UPDATIVE SOON                           
*                                                                               
         LA    R2,SLOOPTH          VALIDATE ML/GL OPTIONS                       
         TM    FLAGS,FLGLOCK       ALSO LOCK GOALS IN ML?                       
         BZ    VALID25             NO                                           
         TM    FLAGS,FLGONLY       ONLY LOCK GOALS IN ML?                       
         BNZ   ERRMLGL1            YES - CANNOT HAVE BOTH OPTIONS SET           
*                                                                               
VALID25  TM    FLAGS,FLGLOCK       ALSO LOCK GOALS IN ML?                       
         BNZ   *+12                YES                                          
         TM    FLAGS,FLGONLY       ONLY LOCK GOALS IN ML?                       
         BZ    VALID30             NO                                           
         TM    FLAGS,FLMLOCK       ADD ML REQUEST?                              
         BZ    ERRMLGL2            NO - CAN'T USE GL=Y/O WITHOUT ML=Y           
*                                                                               
VALID30  TM    FLAGS,FLMLOCK       ADD ML REQUEST?                              
         BZ    VALIDX              NO                                           
*                                                                               
         LA    R2,SLOMEDH          MEDIA FIELD HEADER                           
         CLI   SBQMED,C'*'         ALL MEDIA REQUEST?                           
         BE    ERRMLMED            YES - INVALID REQUEST                        
*                                                                               
         LA    R2,SLOESTH          ESTIMATE FIELD HEADER                        
         CLC   SBQEST,SBQESTND     REQUEST FOR 1 ESTIMATE?                      
         BNE   ERRMLEST            NO - INVALID REQUEST                         
*                                                                               
         LA    R2,SLOPRDH          PRODUCT FIELD HEADER                         
         OC    SBQPGR,SBQPGR       IS IT A PRODUCT GROUP?                       
         BNZ   ERRMLPRD            YES - INVALID REQUEST                        
*                                                                               
VALIDX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ERROR EXITS AND MESSAGES                                            *         
***********************************************************************         
         SPACE 1                                                                
EINV     MVI   ERROR,INVALID                                                    
         B     CURSOR                                                           
*                                                                               
EPIG     XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(39),=C'SINGLE PRODUCT INVALID FOR THIS REQUEST'          
         B     MYCURSOR                                                         
*                                                                               
EPOL     XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(18),=C'POL OPTION INVALID'                               
         B     MYCURSOR                                                         
*                                                                               
ESPILL   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(20),=C'INVALID SPILL OPTION'                             
         B     MYCURSOR                                                         
*                                                                               
EDATA    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(19),=C'INVALID DATA OPTION'                              
         B     MYCURSOR                                                         
*                                                                               
EESTSBY  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(29),=C'MUST SPECIFY EST FOR AUTO SBY'                    
         B     MYCURSOR                                                         
*                                                                               
EMENU    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(22),=C'MENU OPTION IS INVALID'                           
         B     MYCURSOR                                                         
*                                                                               
EDEMOPT  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(23),=C'DEMOS OPTION IS INVALID'                          
         B     MYCURSOR                                                         
*                                                                               
ABORT    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(32),=C'**** STATION LOCKIN ABORTED ****'                 
         B     MYCURSOR                                                         
*                                                                               
ERRMLGL1 XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(29),=C'CANNOT USE BOTH GL=Y AND GL=O'                    
         B     MYCURSOR                                                         
*                                                                               
ERRMLGL2 XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(38),=C'CANNOT USE GL OPTION WITHOUT ML OPTION'           
         B     MYCURSOR                                                         
*                                                                               
ERRMLMED XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(35),=C'CANNOT ADD ML REQUEST FOR ALL MEDIA'              
         B     MYCURSOR                                                         
*                                                                               
ERRMLEST XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(36),=C'CANNOT ADD ML FOR MULTIPLE ESTIMATES'             
         B     MYCURSOR                                                         
*                                                                               
ERRMLPRD XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(40),=C'CANNOT ADD ML REQUEST FOR PRODUCT GROUPS'         
         B     MYCURSOR                                                         
*                                                                               
MYCURSOR MVI   ERROR,X'FE'                                                      
CURSOR   GOTO1 CURSERR                                                          
         EJECT                                                                  
***********************************************************************         
* INPUT HOOK FROM SPOTIO                                              *         
***********************************************************************         
         SPACE 1                                                                
INPUT    L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   SBMODE,SBPROCCL     CLIENT FIRST                                 
         BE    CLIENT                                                           
         CLI   SBMODE,SBPROCSP                                                  
         BE    PROCBUY                                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* CLIENT RECORD HOOK                                                  *         
***********************************************************************         
         SPACE 1                                                                
CLIENT   DS    0H                                                               
         OC    SBADATE,SBADATE     TEST A(DATES) SET YET                        
         BNZ   CLT10                                                            
         GOTO1 SETDATE             NO - SET THE DATES                           
         BAS   RE,DATES            SET WEEK DATES                               
*                                                                               
CLT10    MVC   SVBASDAY,SBSPPROF+8 SAVE FISCAL BASE DAY                         
         OI    ININD,INIOWSDY      MAKE SURE ESTIMATE FIRST DOES NOT            
*                                  RE-SET THE DATES                             
CLTX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* BUY RECORD HOOK                                                     *         
***********************************************************************         
         SPACE 1                                                                
PROCBUY  L     R6,SBAIO1                                                        
         USING BUYRECD,R6                                                       
         CLC   BUYKPRD,SVPRD       TEST PRODUCT/ESTIMATE CHANGE                 
         BNE   *+14                                                             
         CLC   BUYKEST,SVEST                                                    
         BE    BUY10                                                            
         MVC   SVPRD,BUYKPRD       YES-FIND OUT OF WEEK START DAY               
         MVC   SVEST,BUYKEST                                                    
         LA    R3,PRDESTTB         SEARCH PRODUCT/ESTIMATE TABLE                
         LA    R0,PRDESTMX                                                      
*                                                                               
BUY2     CLI   0(R3),0                                                          
         BE    BUY4                                                             
         CLC   SVPRD,0(R3)                                                      
         BNE   *+14                                                             
         CLC   SVEST,1(R3)                                                      
         BE    BUY8                FOUND                                        
         LA    R3,3(R3)                                                         
         BCT   R0,BUY2                                                          
*                                                                               
         L     R3,NXTENTRY                                                      
         LA    R1,3(R3)                                                         
         C     R1,=A(PRDESTTX)                                                  
         BL    *+8                                                              
         LA    R1,PRDESTTB                                                      
         ST    R1,NXTENTRY                                                      
*                                                                               
BUY4     MVC   0(1,R3),SVPRD       NEW ENTRY                                    
         MVC   1(1,R3),SVEST                                                    
         XC    KEY,KEY             READ ESTIMATE RECORD                         
         LA    R2,KEY                                                           
         USING ESTHDRD,R2                                                       
         MVC   EKEYAM,BUYKAM                                                    
         MVC   EKEYCLT,BUYKCLT                                                  
         LAY   R1,CLTREC           CLIENT RECORD                                
         AHI   R1,CLIST-CLTRECD    CLIST                                        
*                                                                               
BUY6     CLI   0(R1),C' '                                                       
         BH    *+6                                                              
         DC    H'0'                                                             
         CLC   SVPRD,3(R1)                                                      
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     BUY6                                                             
         MVC   EKEYPRD,0(R1)                                                    
         MVC   EKEYEST,SVEST                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,SBAIO2                                                        
         ST    R2,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVC   2(1,R3),EOWSDAY     EXTRACT OUT OF WEEK DAY                      
*                                                                               
BUY8     MVI   SBESTOWD,0                                                       
         CLI   2(R3),1                                                          
         BNH   *+10                                                             
         MVC   SBESTOWD,2(R3)                                                   
         CLC   SVOWD,SBESTOWD      TEST CHANGE IN OUT OF WEEK DAY               
         BE    BUY10                                                            
         MVC   SVOWD,SBESTOWD      YES-                                         
         CLC   SVOWD,SBSPPROF+8    TEST CHANGE AFFECTS THE WEEK DATES           
         BE    BUY10                                                            
         MVC   SBSPPROF+8(1),SVBASDAY   YES-                                    
         CLI   SVOWD,0                                                          
         BE    *+10                                                             
         MVC   SBSPPROF+8(1),SVOWD                                              
         BAS   RE,DATES                 SET THE DATES AGAIN                     
*                                                                               
BUY10    B     BUYX                                                             
*                                                                               
BUYX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SET LIST OF WEEK DATES                                              *         
***********************************************************************         
         SPACE 1                                                                
DATES    NTR1  ,                                                                
         XC    WORK,WORK           BUILD WEEKS TABLE                            
         MVC   WORK(4),GETBROAD                                                 
         MVC   WORK+4(4),ADDAY                                                  
         MVC   WORK+8(4),GETDAY                                                 
         MVC   WORK+12(4),DATCON                                                
         GOTO1 MOBILE,DMCB,(55,SBQREQST),(5,AWEEKS),WORK,SBSPPROF               
         SR    RE,RE                                                            
         L     R1,AWEEKS                                                        
         TM    FLAGS,FLEDAILY      DAILY FLAG FOR SL REPORT?                    
         BZ    DAT20               NO                                           
         L     R1,ADAYS            YES - DAILY FLAG GETS DAYS                   
         ST    R1,SBADATE          SET A(DATE LIST) FOR SPOTBUY                 
         LA    R0,NDAYS            92 DAYS MAX                                  
                                                                                
DAT10    OC    0(4,R1),0(R1)       END OF DAY TABLE?                            
         BZ    DAT30               YES                                          
         LA    R1,4(R1)            BUMP TO NEXT ENTRY                           
         LA    RE,1(RE)            BUMP COUNTER                                 
         BCT   R0,DAT10            NEXT TABLE ENTRY                             
         B     DAT30               SET N'DATES                                  
*                                                                               
DAT20    ST    R1,SBADATE          SET A(DATE LIST) FOR SPOTBUY                 
         CLI   0(R1),X'FF'                                                      
         BE    *+16                                                             
         LA    R1,4(R1)                                                         
         LA    RE,1(RE)                                                         
         B     *-16                                                             
         MVI   0(R1),0                                                          
DAT30    ST    RE,SBNDATES         SET N'DATES                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DRIVER HOOKS                                                        *         
***********************************************************************         
         SPACE 1                                                                
DRHOOK   L     R4,AGLOBAL                                                       
         USING GLOBALD,R4                                                       
         CLI   GLHOOK,GLRESOLV     RESOLVE ADDRESSES                            
         BE    RESOLVE                                                          
         CLI   GLHOOK,GLINIT       DRIVER INITIALIZATION                        
         BE    DRVINIT                                                          
         CLI   GLHOOK,GLROUT       EXECUTE ROUTINES                             
         BE    EXEC                                                             
         CLI   GLHOOK,GLOUTPUT     DRIVER'S OUTPUT STAGE                        
         BE    OUTPUT                                                           
         CLI   GLHOOK,GLHEAD       HEADHOOK                                     
         BE    HEAD                                                             
*                                                                               
DRHOOKX  B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DRIVER HOOK ROUTINE TO RESOLVE ADDRESSES                            *         
***********************************************************************         
         SPACE 1                                                                
RESOLVE  LA    R1,RTNLIST          SEARCH LIST FOR ROUTINE NAME                 
*                                                                               
RESOLVE2 CLI   0(R1),FF                                                         
         BE    XIT                                                              
         CLC   0(8,R1),GLLABEL                                                  
         BE    *+12                                                             
         LA    R1,12(R1)                                                        
         B     RESOLVE2                                                         
         MVC   GLAROUT,8(R1)       YES-RETURN ADDRESS                           
         B     XIT                                                              
         SPACE 1                                                                
RTNLIST  DS    0F                                                               
         DC    CL8'ISTAT   ',A(ISTA)                                            
         DC    CL8'OSTAT   ',A(OSTA)                                            
         DC    CL8'IDAYPT  ',A(IDPT)                                            
         DC    CL8'ODAYPT  ',A(ODPT)                                            
         DC    CL8'ISLN1   ',A(ISLN1)                                           
         DC    CL8'OSLN1   ',A(OSLN1)                                           
         DC    CL8'ISLN2   ',A(ISLN2)                                           
         DC    CL8'OSLN2   ',A(OSLN2)                                           
         DC    CL8'OKEY    ',A(OKEY)                                            
         DC    CL8'OWK     ',A(OWEEK)                                           
         DC    CL8'ODOL    ',A(ODOL)                                            
         DC    CL8'ONET    ',A(ONET)                                            
         DC    CL8'OCOS2   ',A(OCOS2)                                           
         DC    CL8'OCOS2NET',A(OCOS2NET)                                        
         DC    CL8'OSPOTS  ',A(OSPOTS)                                          
         DC    CL8'IDEM    ',A(IDEM)                                            
         DC    CL8'ODEM    ',A(ODEM)                                            
         DC    CL8'OPUTWEEK',A(PUTEL)                                           
         DC    CL8'HDEMO   ',A(HDEM)                                            
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* DRIVER INITIALIZATION                                               *         
***********************************************************************         
         SPACE 1                                                                
DRVINIT  MVC   GLOPTS+2(1),NDEMOS  PASS N'DEMOS TO DRIVER                       
*                                                                               
         MVI   GLOPTS+3,C'N'       PASS PIGGYBACK OPTION                        
         TM    SBQPIND,SBQPPB                                                   
         BZ    *+8                                                              
         MVI   GLOPTS+3,C'Y'       DON'T SPLIT PIGGYBACKS                       
*                                                                               
         MVI   GLOPTS+4,C'N'       DEFAULT NO 2ND COST                          
         TM    FLAGS,FLCOS2                                                     
         BZ    *+8                                                              
         MVI   GLOPTS+4,C'Y'                                                    
*                                                                               
         MVI   GLOPTS+5,C'N'       DEFAULT NO NO DAILY EST                      
         TM    FLAGS,FLEDAILY      DAILY FLAG FOR SL REPORT?                    
         BZ    DREXIT              NO                                           
         MVI   GLOPTS+5,C'Y'       YES                                          
         OI    SBQPER,SBQPDY       SET NEED DAYS                                
*                                                                               
DREXIT   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DRIVER HOOK TO EXECUTE ROUTINES                                     *         
***********************************************************************         
         SPACE 1                                                                
EXEC     L     R2,GLAIFLD          R2=A(INPUT)                                  
         L     R3,GLAOFLD          R3=A(OUTPUT)                                 
         CLI   GLMODE,GLINPUT      TEST INPUT PHASE                             
         BNE   EXECX                                                            
         L     R1,GLADTENT         YES-                                         
         USING DRIND,R1                                                         
         CLI   DRINLEV,1           TEST LEVEL 1                                 
         BH    *+8                                                              
         MVI   INDATA,0            YES-RESET DATA INDICATOR                     
         DROP  R1                                                               
         L     R5,SBACURCH         R5=A(CHUNK ENTRY)                            
         USING SCHUNKD,R5                                                       
         L     R6,SBAIO1           R6=A(BUY RECORD)                             
         USING BUYRECD,R6                                                       
         B     EXECX                                                            
*                                                                               
EXECX    L     RF,GLAROUT          BRANCH TO ROUTINE                            
         BR    RF                                                               
         EJECT                                                                  
***********************************************************************         
* INPUT ROUTINES                                                      *         
***********************************************************************         
         SPACE 1                                                                
ISTA     XC    0(8,R2),0(R2)       STATION                                      
         MVC   0(5,R2),SBSTA                                                    
         CLI   SBMED,C'N'                                                       
         BNE   *+8                                                              
         MVI   4(R2),C'N'                                                       
         CLC   SBCBLNET,SPACES     TEST CABLE STATION                           
         BNH   *+10                                                             
         MVC   5(3,R2),SBCBLNET    YES-CABLE NETWORK                            
         MVC   8(2,R2),SBBMKT      INCLUDE THE MARKET                           
         MVC   10(3,R2),SBBSTA     PACKED STATION                               
         B     XIT                                                              
         SPACE 1                                                                
IDPT     MVC   0(1,R2),BDDAYPT     DAYPART                                      
         B     XIT                                                              
         SPACE 1                                                                
ISLN1    MVC   0(1,R2),SCSLN1      SPOT LENGTH 1                                
         B     XIT                                                              
         SPACE 1                                                                
ISLN2    MVC   0(1,R2),SCSLN2      SPOT LENGTH 2                                
         B     XIT                                                              
         SPACE 1                                                                
IDEM     LA    R1,SCDEMOS          DEMOS                                        
         LLC   RE,GLARGS                                                        
         BCTR  RE,0                                                             
         SLL   RE,3                                                             
         LA    R1,0(RE,R1)                                                      
         MVC   0(4,R2),0(R1)                                                    
         OC    0(4,R2),0(R2)       TEST ANY DATA                                
         BZ    XIT                                                              
         MVI   INDATA,1            YES                                          
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* OUTPUT ROUTINES                                                     *         
***********************************************************************         
         SPACE 1                                                                
OSTA     MVC   STATION,0(R2)       SAVE THE STATION                             
         CLC   SBBSTA,10(R2)       TEST NEW STATION                             
         BNE   *+14                                                             
         CLC   SBBMKT,8(R2)        (OR MARKET)                                  
         BE    OSTAX                                                            
         MVC   SBBSTA,10(R2)       YES-                                         
         MVC   SBBMKT,8(R2)                                                     
OSTAX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* MORE OUTPUT ROUTINES                                                *         
***********************************************************************         
         SPACE 1                                                                
ODPT     MVC   SVDPT,0(R2)         DAYPART                                      
         MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
         SPACE 1                                                                
OSLN1    MVC   SVSLN1,0(R2)        LENGTH 1                                     
         MVI   SVSLN2,0                                                         
         MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
         SPACE 1                                                                
OSLN2    MVC   SVSLN2,0(R2)        LENGTH 2                                     
         MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
         SPACE 1                                                                
OWEEK    DS    0H                  WEEK                                         
         GOTO1 DATCON,DMCB,(2,(R2)),(5,(R3))                                    
         MVC   SVWEEK,0(R2)                                                     
         B     XIT                                                              
         SPACE 1                                                                
ODOL     MVC   SVDOL,0(R2)         DOLLARS                                      
         MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
         SPACE 1                                                                
ONET     MVC   SVNET,0(R2)         NET DOLLARS                                  
         MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
         SPACE 1                                                                
OCOS2    MVC   SVDOL2,0(R2)        COST 2 DOLLARS                               
         MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
         SPACE 1                                                                
OCOS2NET MVC   SVNET2,0(R2)        COST 2 DOLLARS                               
         MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
         SPACE 1                                                                
OSPOTS   MVC   SVSPT,0(R2)         SPOTS                                        
         MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
         SPACE 1                                                                
ODEM     LLC   R1,GLARGS           DEMOS                                        
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         LA    R1,DEMVALS(R1)                                                   
         MVC   0(4,R1),0(R2)                                                    
*                                                                               
         TM    COLIND,COLINDR      DEMO ROUNDING                                
         BZ    ODEMX               YES                                          
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,GLARGS                                                      
         BZ    ODEMX                                                            
         BCTR  RF,0                TEST THIS DEMO IS RATING                     
         MHI   RF,3                                                             
         L     RE,ADEMLST                                                       
         AR    RF,RE                                                            
*                                                                               
         CLI   2(RF),0             COMSCORE DEMO?                               
         BNE   ODEM9               NO                                           
         XR    RE,RE               CLEAR RE                                     
         ICM   RE,1,1(RF)          HAVE COMSCORE INDEX?                         
         BZ    ODEMX               NO                                           
         BCTR  RE,0                -1                                           
         MHI   RE,8                INDEX INTO COMDLIST                          
         LA    RF,COMDLIST         RF = COMDLIST                                
         AR    RF,RE               PLUS INDEX = DEMO NAME                       
         BCTR  RF,0                -1 SO WE CAN USE NEXT INSTRUCTIONS           
*                                                                               
ODEM9    CLI   1(RF),C'R'                                                       
         BE    ODEM10                                                           
         CLI   1(RF),C'E'                                                       
         BNE   ODEM11                                                           
*                                                                               
ODEM10   LA    RF,SBLOCK                                                        
         TM    SBEFLAG4-SBLOCK(RF),SBE42DEC  WANT 2 DECIMAL PLACES?             
         B     *+12                GO TEST CC                                   
*                                                                               
ODEM11   LA    RF,SBLOCK                                                        
         TM    SBEFLAG9-SBLOCK(RF),SBE92DEC  WANT 2 DECIMAL IMPS?               
         BZ    ODEMX               NO                                           
         OI    0(R1),X'40'         SET THAT IT'S A 2 DEC NUMBER!                
         L     RE,GLADTENT                                                      
         LA    RE,DRODEC-DROD(RE)  A(INPUT RECORD)                              
         MVI   0(RE),2             2 DECIMAL DEMOS                              
*                                                                               
ODEMX    MVI   GLHOOK,GLEDIT                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD LOCKIN RECORD KEY.                                 *         
* IF KEY MATCHES RECORD IN STORAGE, EXIT.                             *         
* ELSE PUT THE RECORD THAT'S IN STORAGE AND READ HIGH FOR NEW KEY -   *         
* IF KEY FOUND, READ RECORD INTO STORAGE, ELSE BUILD NEW RECORD.      *         
***********************************************************************         
         SPACE 1                                                                
OKEY     LA    R6,KEY                                                           
         XC    KEY,KEY                                                          
         USING SLKRECD,R6                                                       
         MVI   SLKKTYP,SLKKTYPQ    BUILD LOCKIN KEY                             
         MVI   SLKKSUB,SLKKSUBQ                                                 
         MVC   SLKKAGMD,SBBAGYMD                                                
         MVC   SLKKCLT,SBBCLT                                                   
         MVC   SLKKMKT,SBBMKT                                                   
         MVC   SLKKSTA,SBBSTA                                                   
         MVC   SLKKPRD,SBBPRD                                                   
         MVC   SLKKPRD2,SBBPRD2                                                 
         MVC   SLKKEST,SBBEST                                                   
         MVC   SLKKDPT,SVDPT                                                    
         MVC   SLKKLEN,SVSLN1                                                   
         MVC   SLKKLEN2,SVSLN2                                                  
*                                                                               
         L     R5,SBAIO3                                                        
         CLC   SLKKEY,0(R5)        TEST RECORD SITTING IN CORE                  
         BE    OKEYX               YES-THEN WE'LL JUST ADD TO IT                
* NEED TO ADD NEW RECORD                                                        
         MVC   SVKEY,KEY           SAVE THE KEY                                 
*                                                                               
         XC    C2KEY,C2KEY                                                      
K        USING PWRECD,C2KEY        BUILD COS2 KEY                               
         MVC   K.PWKTYP,=X'0D7A'                                                
         MVC   K.PWKAGMD,SLKKAGMD                                               
         MVC   K.PWKCLT,SLKKCLT                                                 
         MVC   K.PWKPRD,SLKKPRD                                                 
         MVC   K.PWKEST,SLKKEST                                                 
         MVC   K.PWKMKT,SLKKMKT                                                 
         MVC   K.PWKSTA,SLKKSTA                                                 
         DROP  K                                                                
*                                                                               
         GOTO1 GETDEMS,DMCB,SLKKPRD,SLKKEST    GET THE DEMOS                    
*                                                                               
         OC    0(32,R5),0(R5)      TEST ANY RECORD IN CORE                      
         BNZ   *+14                                                             
         MVC   SVC2KEY,C2KEY                                                    
         B     OKEY4               FIRST TIME                                   
*                                                                               
         ST    R5,AIO              YES-PUT THE RECORD                           
         CLC   SVC2KEY,C2KEY                                                    
         BE    *+8                 COS2 REC IS STILL THE SAME, IF NOT           
         BAS   RE,CHKC2REC         SEE IF $$ IN C2 AND XLK RECS AGREE           
         CLI   NEWREC,C'Y'         TEST NEW RECORD                              
         BE    OKEY2               YES-THEN ADD IT                              
         TM    IOINDS,IOIRERD      TEST NEED TO RE-READ THE RECORD              
         BZ    OKEY1                                                            
*                                                                               
         LA    RF,XTRAREC          SFI!!!                                       
         ST    RF,AIO                                                           
* REREAD RECORD                                                                 
         MVC   KEY(32),0(R5)                                                    
         BAS   RE,XSPHIGH                                                       
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,XSPGET                                                        
         NI    IOINDS,255-IOIRERD                                               
         ST    R5,AIO              SFI!!!                                       
*                                                                               
OKEY1    BAS   RE,XSPPUT                                                        
         B     OKEY4                                                            
*                                                                               
OKEY2    BAS   RE,XSPADD                                                        
         CLI   SLKKPRD2,0          TEST PIGGYBACK PRODUCT                       
         BE    OKEY4                                                            
         BAS   RE,ADDPSSV                                                       
*                                                                               
OKEY4    MVC   KEY,SVKEY                                                        
         BAS   RE,XSPHIGH                                                       
*                                                                               
         MVI   NEWREC,C'N'                                                      
         L     R6,SBAIO3                                                        
         CLC   KEY(32),KEYSAVE     TEST RECORD FOUND                            
         BNE   OKEY8                                                            
         ST    R6,AIO              YES-GET THE RECORD                           
         BAS   RE,XSPGET                                                        
         XC    SVEST,SVEST                                                      
         BRAS  RE,REMOLD           REMOVE OLD LOKIN ELS                         
         L     R1,ARECTAB          REMOVE THIS ENTRY'S D/A FROM TABLE           
*                                                                               
* IT'S OK IF THE REC IS NOT IN THE TABLE - THAT JUST MEANS THAT THERE           
* WERE NO ELEMS IN THE REQ PERIOD THAT NEEDED TO BE DELETED BEFORE              
* IT GOT TO THE UPDATE CODE                                                     
OKEY5    CLC   0(L'SLKKEY,R1),0(R6)                                             
         BE    OKEY5A                                                           
         AHI   R1,RECLENQ                                                       
         C     R1,ARECEND                                                       
         BH    OKEY5B                                                           
         B     OKEY5                                                            
*&&DO                                                                           
         BL    *+6                                                              
         DCHO                      KEY NOT FOUND IN TABLE                       
         OC    0(RECLENQ,R1),0(R1)                                              
         BNZ   OKEY5                                                            
         DCHO                      KEY NOT FOUND IN TABLE                       
*&&                                                                             
*                                                                               
OKEY5A   XC    36(4,R1),36(R1)     CLEAR D/A TO SHOW REC UPDATED                
*                                                                               
OKEY5B   LA    R1,SLKDEM1          SET DEMO CATEGORIES IN THE                   
         LA    RE,SVDEMS           DESCRIPTION ELEMENT                          
         LLC   R0,NDEMOS                                                        
         STC   R0,NUMDEMS                                                       
         SR    RF,RF                                                            
*                                                                               
OKEY6    CLI   1(RE),0             NO MORE DEMOS?                               
         BNE   *+12                                                             
         STC   RF,NUMDEMS                                                       
         B     OKEY12                                                           
*                                                                               
         CLI   1(R1),0             IS THERE A DEMO ON THE LOCKIN REC?           
         BNE   *+14                 YES - MAKE SURE THEY'RE THE SAME            
         MVC   0(3,R1),0(RE)                                                    
         B     OKEY7                                                            
*                                                                               
         CLC   1(2,R1),1(RE)                                                    
         BE    OKEY7                                                            
         TM    OPTIND2,OPTINOUP    DEMO CATEGORY DISCREPENCY                    
         BO    OKEY7               OK IF WRITE=NO OPTION SET                    
         DC    H'0'                ELSE FATAL ERROR                             
*                                                                               
OKEY7    LA    R1,3(R1)                                                         
         LA    RE,3(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,OKEY6                                                         
         B     OKEY12                                                           
*                                                                               
OKEY8    MVI   NEWREC,C'Y'         NO-BUILD NEW RECORD                          
         XC    SLKRECD(256),SLKRECD                                             
         MVC   SLKKEY,KEYSAVE      KEY                                          
         MVI   SLKELCD,SLKELCDQ    DESCRIPTION ELEMENT                          
         MVI   SLKELLN,SLKELLNQ                                                 
         MVC   SLKCRDT,BTODAY                                                   
         MVC   SLKUPDT,BTODAY                                                   
         MVC   SLKLEN,=Y(SLKEL-SLKRECD+SLKELLNQ+1)  RECORD LENGTH               
         LLC   R0,NDEMOS           DEMO CATEGORIES                              
         STC   R0,NUMDEMS          SAVE N'DEMOS THIS TIME                       
         LA    RE,SVDEMS                                                        
         LA    R1,SLKDEM1                                                       
         SR    RF,RF                                                            
*                                                                               
OKEY10   CLI   1(RE),0                                                          
         BNE   *+12                                                             
         STC   RF,NUMDEMS          N'DEMOS LESS THAN REQUESTED NUMBER           
         B     OKEY12                                                           
         MVC   0(3,R1),0(RE)                                                    
         LA    R1,3(R1)                                                         
         LA    RE,3(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,OKEY10                                                        
*                                                                               
OKEY12   BAS   RE,PUTCOM                                                        
         B     XIT                                                              
*                                                                               
OKEYX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD AND PUT A COMSCORE DEMO NAME ELEMENT               *         
***********************************************************************         
PUTCOM   NTR1                                                                   
*                                                                               
         OC    COMDLIST,COMDLIST   HAVE ANY COMSCORE DEMOS?                     
         BZ    PUTCOMX             NO - DONE                                    
*                                                                               
         L     R6,SBAIO3           A(STATION LOCK-IN RECORD)                    
         USING SLKRECD,R6          STATION LOCK-IN RECORD DSECT                 
         LA    R5,SLKFSTEL         A(FIRST ELEMENT)                             
         SR    R0,R0               CLEAR R0                                     
*                                                                               
PUTCOM2  CLI   0(R5),0             END OF RECORD?                               
         BE    PUTCOM4             YES - ADD THE ELEMENT                        
         CLI   0(R5),COMELCDQ      FOUND THE X'50' ELEMENT?                     
         BE    PUTCOM3             YES - DELETE IT                              
         IC    R0,1(R5)            ELEMENT LENGTH                               
         AR    R5,R0               BUMP TO NEXT ELEMENT                         
         B     PUTCOM2             TEST NEXT ELEMENT                            
         DROP  R6                  DROP STATION LOCK-IN RECORD USING            
*                                                                               
PUTCOM3  MVI   0(R5),FF            DELETE THE ELEMENT                           
         GOTO1 HELLO,DMCB,(C'D',=C'XSPFIL'),(X'FF',(R6)),0,0,0                  
*                                                                               
PUTCOM4  XC    ELEM,ELEM           BUILD THE X'50' ELEMENT HERE                 
         LA    R5,ELEM             R5 = ELEM                                    
         USING COMDEMEL,R5         COMSCORE DEMO NAME ELEMENT DSECT             
         MVI   0(R5),COMELCDQ      X'50'                                        
*                                                                               
         LA    RE,COMDMONM         COMSCORE DEMO NAME IN ELEMENT                
         LA    RF,COMDLIST         COMSCORE DEMO LIST                           
         LA    R0,20               MAX 20 COMSCORE DEMO NAMES                   
*                                                                               
PUTCOM5  CLC   0(8,RF),SPACES      HAVE A COMSCORE DEMO NAME?                   
         BNH   PUTCOM6             NO                                           
         MVC   0(8,RE),0(RF)       MOVE THE COMSCORE DEMO NAME                  
         AHI   RE,COMDLEN          BUMP TO THE NEXT POSITION IN ELEM            
         AHI   RF,8                NEXT COMSCORE DEMO NAME IN COMDLIST          
         BCT   R0,PUTCOM5          SEE IF WE HAVE MORE                          
*                                                                               
PUTCOM6  LA    RE,COMDLIST         START OF COMDLIST                            
         SR    RF,RE               SEE HOW MANY BYTES WE MOVED                  
         AHI   RF,2                ADD 2 BYTES FOR ELEM OVERHEAD                
         STC   RF,COMELLN          ELEMENT LENGTH                               
         DROP  R5                  DROP COMSCORE DEMO NAME ELEM USING           
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'XSPFIL'),(R6),ELEM,0,0                       
*                                                                               
PUTCOMX  B     XIT                 DONE                                         
***********************************************************************         
* CHKC2REC  WILL COMPARE $$ IN XLK LOKEL WITH $$ IN PWREC C2STEL ELEM *         
*      IF NEQ WILL REPLACE WITH ACCUMED TOTAL FROM XLK REC            *         
***********************************************************************         
CHKC2REC NTR1                                                                   
         L     R2,AIO              SAVE AIO                                     
         OI    IOINDS,IOIRERD                                                   
         NI    FLAG,X'FF'-UPDATE                                                
         XC    KEY,KEY             READ STATION PW KEY                          
         MVC   KEY(L'SVC2KEY),SVC2KEY                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'SVC2KEY),KEYSAVE                                           
         BNE   CHKC240             IF NO SUCH COS2 REC, EXIT                    
*                                                                               
         L     R6,SBAIO2                                                        
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,PWEL-PWRECD(R6)  POINT TO FIRST ELEM                          
CHKC210  CLI   0(R6),0                                                          
         BE    CHKC230             END OF REC                                   
         CLI   0(R6),X'04'                                                      
         BNE   CHKC220             LOOK FOR COS2 ELEM                           
*                                                                               
         USING C2STEL,R6                                                        
         CLC   C2STLKC2,ACMLKC2                                                 
         BE    *+14                                                             
         MVC   C2STLKC2,ACMLKC2                                                 
         OI    FLAG,UPDATE                                                      
         CLC   C2STLK,ACMLK                                                     
         BE    CHKC220                                                          
         MVC   C2STLK,ACMLK                                                     
         OI    FLAG,UPDATE                                                      
*                                                                               
CHKC220  LLC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CHKC210                                                          
*                                                                               
CHKC230  TM    FLAG,UPDATE         ANY CHANGE TO AMOUNTS ?                      
         BZ    CHKC240                                                          
         GOTO1 PUTREC                                                           
*                                                                               
CHKC240  MVC   SVC2KEY,C2KEY                                                    
         ST    R2,AIO              RESTORE AIO TO WHATEVER IT WAS               
         XC    ACMLKC2,ACMLKC2                                                  
         XC    ACMLK,ACMLK                                                      
         XIT1                                                                   
***********************************************************************         
* ROUTINE TO BUILD AND PUT A LOCKIN WEEK ELEMENT                      *         
***********************************************************************         
         SPACE 1                                                                
PUTEL    TM    GLINDS,GLTOTLIN     ONLY FOR DETAIL LINES                        
         BO    PUTELX                                                           
         XC    WORK,WORK           BUILD ELEMENT FROM VALUES SAVED              
         LA    R5,WORK             FROM COLUMN OUTPUT ROUTINES                  
         USING LOKEL,R5                                                         
         MVI   LOKELCD,LOKELCDQ                                                 
         MVI   LOKELLN,LOKNET2-LOKELCD+L'LOKNET2   FIX-LEN ELEMENT              
         MVC   LOKWEEK,SVWEEK                                                   
         MVC   LOKSPOTS,SVSPT+2                                                 
         OC    SVSPT(2),SVSPT                                                   
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   LOKDOLS,SVDOL                                                    
         MVC   LOKNET,SVNET                                                     
         MVC   LOKDOL2,SVDOL2                                                   
         MVC   LOKNET2,SVNET2                                                   
         MVC   LOKDEM(16),DEMVALS                                               
*                                                                               
         ICM   R0,15,LOKDOLS                                                    
         A     R0,ACMLK                                                         
         ST    R0,ACMLK                                                         
         ICM   R0,15,LOKDOL2                                                    
         A     R0,ACMLKC2                                                       
         ST    R0,ACMLKC2                                                       
*                                                                               
         CLI   NUMDEMS,0                                                        
         BNE   *+10                                                             
         XC    LOKDEM(16),LOKDEM                                                
*                                                                               
         L     R6,SBAIO3                                                        
         USING SLKRECD,R6                                                       
         LA    R5,SLKEL            SCAN RECORD FOR WEEK MATCH                   
         SR    R0,R0                                                            
*                                                                               
PUTEL2   CLI   0(R5),0                                                          
         BE    PUTEL8              NOT FOUND - ADD THE ELEMENT                  
         CLI   0(R5),LOKELCDQ                                                   
         BNE   PUTEL6                                                           
         CLC   LOKWEEK,SVWEEK      COMPARE WEEKS                                
         BNE   PUTEL6                                                           
         LLC   RF,LOKELLN          REPLACE THE ELEMENT                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     PUTEL9                                                           
         MVC   LOKEL(0),WORK                                                    
*                                                                               
PUTEL6   IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     PUTEL2                                                           
         DROP  R5                                                               
*                                                                               
PUTEL8   GOTO1 HELLO,DMCB,(C'P',=C'XSPFIL'),(R6),WORK,0,0                       
*                                                                               
PUTEL9   CLC   SBQAGY,=C'CK'                                                    
         BNE   PUTELX                                                           
         OI    IOINDS,IOIRERD                                                   
*                                                                               
         LA    R1,XTRAREC          NOTE ** USED TO BE ESTHDR                    
         ST    R1,AIO              REMEMBER ** IN CASE OF PROBLEM LATER         
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING BGRKEYD,R3                                                       
         MVI   BGRKTYPE,BGRKTYPQ                                                
         MVI   BGRKSUB,BGRKSUBQ                                                 
         MVC   BGRKAM,SBBAGYMD                                                  
         MVC   BGRKCLT,SBBCLT                                                   
         MVC   BGRKPRD,SBBPRD                                                   
         MVC   BGRKEST,SBBEST                                                   
         MVC   BGRKMKT,SBBMKT                                                   
         BAS   RE,XSPHIGH                                                       
*                                                                               
PUTEL10  LA    R3,KEY                                                           
         CLC   KEY(BGRKMINK-BGRKEY),KEYSAVE                                     
         BNE   PUTELX                                                           
         OC    BGRDDA,BGRDDA                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,XSPGET                                                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                CHECK FOR ANY ERROR ON RETURN                
*                                                                               
         LA    R3,XTRAREC                                                       
         LA    R3,BGRELS                                                        
PUTEL11  CLI   0(R3),0                                                          
         BE    PUTEL14                                                          
         CLI   0(R3),BGRWSELQ      WEEKLY STATUS ELEMENT?                       
         BE    PUTEL13                                                          
PUTEL12  LLC   R1,1(R3)            GET NEXT ELEM                                
         AR    R3,R1                                                            
         B     PUTEL11                                                          
*                                                                               
PUTEL13  DS    0H                                                               
         USING BGRWSELD,R3                                                      
         CLC   SVWEEK,BGRWSTRT                                                  
         BNE   PUTEL12                                                          
         OI    BGRWSBGS,BGRWSLKQ   BUYS LOCKED IN                               
*                                                                               
         LA    R3,KEY              POINT TO KEY TO GET DISK ADDRESS             
         USING BGRKEYD,R3                                                       
*                                                                               
         CLI   TWAWRITE,C'N'       WRITE= NO                                    
         BE    PUTEL13A                                                         
         BAS   RE,XSPPUT                                                        
PUTEL13A B     PUTELX                                                           
*                                                                               
PUTEL14  DS    0H                  CHECK FOR ANOTHER REC WITH SAME KEY          
         BAS   RE,XSPSEQ                                                        
         B     PUTEL10             BECAUSE THE RECS ARE MINIO                   
         DROP  R3                                                               
*                                                                               
PUTELX   B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* HEADING ROUTINES                                                    *         
***********************************************************************         
         SPACE 1                                                                
HDEM     DS    0H                  DEMO HEADING                                 
         LLC   R1,GLARGS                                                        
         BCTR  R1,0                                                             
         MHI   R1,7                                                             
         LA    R1,DEMNAMES(R1)                                                  
         MVC   0(7,R3),0(R1)                                                    
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DRIVER'S OUTPUT STAGE                                               *         
* BUILD A LIST OF ALL LOCKIN RECS THAT NEED TO BE CHANGED             *         
***********************************************************************         
         SPACE 1                                                                
OUTPUT   BRAS  RE,BLDLST                                                        
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DRIVER HEADHOOK                                                     *         
***********************************************************************         
         SPACE 1                                                                
HEAD     L     R2,AH4              FORMAT STATION TO THE HEADLINES              
         A     R2,PWIDTH                                                        
         MVC   57(2,R2),=C'**'                                                  
         LA    RF,69(R2)                                                        
         LA    R1,60(R2)                                                        
         MVC   0(4,R1),STATION                                                  
         CLI   STATION,C'0'        TEST CABLE STATION                           
         BL    *+18                                                             
         MVI   4(R1),C'/'          YES-FORMAT CABLE NETWORK ALSO                
         MVC   5(3,R1),STATION+5                                                
         B     HD2                                                              
         BCTR  RF,0                                                             
         MVI   4(R1),C'-'                                                       
         MVC   5(1,R1),STATION+4                                                
         MVI   6(R1),C'M'                                                       
         CLI   STATION+4,C'T'                                                   
         BE    *+12                                                             
         CLI   STATION+4,C' '                                                   
         BH    *+10                                                             
         MVC   5(2,R1),=C'TV'                                                   
         CLI   3(R1),C' '                                                       
         BH    HD2                                                              
         MVC   3(6,R1),4(R1)                                                    
         MVI   9(R1),C' '                                                       
         BCTR  RF,0                                                             
*                                                                               
HD2      MVC   0(2,RF),=C'**'                                                   
         A     R2,PWIDTH           AND THE MARKET                               
         A     R2,PWIDTH                                                        
         MVC   58(6,R2),=C'MARKET'                                              
         SR    RE,RE                                                            
         ICM   RE,3,SBBMKT                                                      
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  65(4,R2),DUB                                                     
*                                                                               
HEADX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* FINAL CALL                                                          *         
***********************************************************************         
         SPACE 1                                                                
FINAL    DS    0H                  ADD SBY REQUEST                              
         CLI   SLPROF+2,C'Y'       PROFILE SET                                  
         BNE   FINXX                                                            
         CLI   SBQMED,C'X'         NO BUY COPY FOR MEDIA X                      
         BE    FINXX                                                            
*                                                                               
         CLC   AGENCY,=C'CK'       COKE CAN HAVE EST=ALL                        
         BE    FINX04                                                           
         ICM   R1,15,SBAESTTB      MUST HAVE POL EST OPEN                       
         BZ    FINXX               MUST SPECIFY EST FOR AUTO SBY                
         LA    RE,255-1            SET INDEX TO POL DATA                        
         SLL   RE,8                X 256 (256 BYTES PER PRD-1 PER EST)          
         LLC   R0,SBQEST                                                        
         BCTR  R0,0                                                             
         AR    RE,R0               INDEX TO ESTIMATE IN POL BLK                 
         AR    RE,R1               ADD TO ADDRESS OF BLOCK                      
         CLI   0(RE),0             ACTIVE?                                      
         BE    FINXX               MUST HAVE POL EST                            
*                                                                               
FINX04   MVC   BYREQST,SPACES                                                   
         USING FQRECORD,R3                                                      
         LA    R3,BYREQST                                                       
         MVC   FQCODE,=C'BY'                                                    
         MVC   FQAGY,SBQAGY        AGENCY                                       
         MVC   FQMED,SBQMED        MEDIA                                        
         MVC   FQCLT,SBQCLT        CLIENT                                       
*                                                                               
         OC    SBQPGR,SBQPGR       IS IT A PRODUCT GROUP?                       
         BZ    FINX06                                                           
         MVC   FQPGR,SBQPGRD       PRD GROUP DEFINITION                         
         MVC   FQPRD,SBQPGRF       PRD GROUP                                    
         B     FINX08                                                           
FINX06   MVC   FQPRD,SBQPRD        PRODUCT                                      
         CLI   SBCPROF+0,C'0'                                                   
         BNE   *+10                                                             
         MVC   FQPRD,=C'POL'                                                    
*                                                                               
FINX08   MVC   FQMKT,SBQMKT        MARKET                                       
         OC    FQMKT,FQMKT                                                      
         BNZ   *+10                                                             
         MVC   FQMKT,=C'ALL'                                                    
*                                                                               
         MVC   FQSTA,SBQSTA        STATION                                      
         OC    FQSTA,FQSTA                                                      
         BNZ   *+10                                                             
         MVC   FQSTA,=C'ALL  '                                                  
*                                                                               
         CLC   =C'ALL',SLOEST      ESTIMATE                                     
         BNE   FINX10                                                           
         MVC   FQEST,=C'ALL'                                                    
         B     FINX12                                                           
FINX10   EDIT  (B1,SBQEST),(3,FQEST),FILL=0                                     
*                                                                               
FINX12   MVC   FQSTART,SBQSTART    START DATE                                   
         MVC   FQEND,SBQEND        END DATE                                     
*                                                                               
         L     R1,TWAMASTC                                                      
         USING MASTD,R1                                                         
         OC    MCREMPQK,MCREMPQK   TEST SOON                                    
         BZ    FINX13                                                           
*                                                                               
         ICM   RF,15,MCAMCORE      A(MONSOON'S CORE RES RTN LIST)               
         BNZ   *+6                                                              
         DCHO                                                                   
         CLI   0(RF),X'FF'         FIND END OF LIST                             
         BE    *+12                                                             
         AHI   RF,8                                                             
         B     *-12                                                             
*                                                                               
         USING MONSPRMD,RF                                                      
         L     RE,AJCLBUF                                                       
         MVC   0(80,RE),BYREQST                                                 
         MVI   80(RE),X'FF'                                                     
         B     FINXX                                                            
         DROP  R1,RF                                                            
*                                                                               
FINX13   CLI   DCBOPEN,C'Y'                                                     
         BE    FINX14                                                           
         L     R4,ABYRQDCB                                                      
         OPEN  ((R4),OUTPUT)                                                    
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   DCBOPEN,C'Y'        YES-DCB OPEN                                 
*                                                                               
FINX14   L     R4,ABYRQDCB                                                      
         LA    R0,BYREQST                                                       
         PUT   (R4),(R0)                                                        
*                                                                               
FINXX    L     R0,ASAVE            SAVE INTER-REQUEST VALUES                    
         LA    R1,SAVVALSL                                                      
         LA    RE,SAVVALS                                                       
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* AFTER DRIVER OUTPUT CALL (BEFORE FINAL, SINCE TABLES ARE FREED)     *         
***********************************************************************         
         SPACE 1                                                                
AFTOUT   L     R6,SBAIO3                                                        
         OC    0(32,R6),0(R6)      TEST LOCKIN RECORD SITTING IN CORE           
         BZ    AFT30                                                            
         BAS   RE,CHKC2REC         CHECK IF XLK AND COS2 $$ ARE SAME            
         ST    R6,AIO              YES-PUT THE RECORD                           
         CLI   NEWREC,C'Y'                                                      
         BE    AFT20                                                            
*                                                                               
         TM    IOINDS,IOIRERD      TEST NEED TO RE-READ THE RECORD              
         BZ    AFT10                                                            
         LA    RF,XTRAREC                                                       
         ST    RF,AIO                                                           
*                                                                               
         XC    KEY,KEY             YES-GET IT AGAIN                             
         MVC   KEY(32),0(R6)                                                    
         BAS   RE,XSPHIGH                                                       
*                                                                               
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,XSPGET                                                        
         NI    IOINDS,255-IOIRERD                                               
         ST    R6,AIO                                                           
*                                                                               
AFT10    BAS   RE,XSPPUT                                                        
         B     AFT30                                                            
*                                                                               
         USING SLKRECD,R6                                                       
AFT20    BAS   RE,XSPADD                                                        
         CLI   SLKKPRD2,0          TEST PIGGYBACK PRODUCT                       
         BE    AFT30                                                            
         BAS   RE,ADDPSSV                                                       
*                                                                               
* NOW GO REMOVE ELEMS FROM UNCHANGED RECS                                       
AFT30    XC    SVEST,SVEST                                                      
         L     R2,AIO1                                                          
         ST    R2,AIO                                                           
         L     R2,ARECTAB                                                       
*                                                                               
AFT40    OC    0(40,R2),0(R2)                                                   
         BZ    AFTX                                                             
         C     R2,ARECEND                                                       
         BL    *+6                                                              
         DCHO                                                                   
         OC    36(4,R2),36(R2)     IF NO D/A, REC ALREADY UPDATED               
         BZ    AFT50                                                            
*                                                                               
         MVC   KEY(40),0(R2)       MOVE IN KEY AND D/A                          
         BAS   RE,XSPGET           AND GET THE RECORD                           
         BRAS  RE,REMOLD           AND DELETE THE OLD ELEMS                     
         CLI   CHANGE,C'Y'         IF IT HAS CHANGED,                           
         BNE   AFT50                                                            
         BAS   RE,XSPPUT           PUT IT BACK                                  
*                                                                               
AFT50    AHI   R2,RECLENQ                                                       
         B     AFT40                                                            
*                                                                               
AFTX     J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* RUN LAST                                                            *         
***********************************************************************         
         SPACE 1                                                                
RUNLST   L     RE,ASAVE            RESTORE SAVED VALUES                         
         LA    RF,SAVVALSL                                                      
         LA    R0,SAVVALS                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R3,ARECTAB          FREE UP RECTAB                               
         L     R5,=A(RECTABLN)                                                  
         GOTO1 COVAIL,DMCB,C'FREE',(R3),(R5)                                    
*                                                                               
         L     R1,TWAMASTC                                                      
         USING MASTD,R1                                                         
         OC    MCREMPQK,MCREMPQK   TEST SOON                                    
         BNZ   RLX                 YES - NO FILE TO CLOSE                       
         DROP  R1                                                               
*                                                                               
         CLI   DCBOPEN,C'Y'                                                     
         BNE   XIT                                                              
         CLI   OFFLINE,C'Y'                                                     
         BNE   XIT                                                              
         MVC   BYREQST,SPACES                                                   
         MVC   BYREQST(2),=C'/*'      END OF BY REQUESTS                        
         L     R4,ABYRQDCB                                                      
         LA    R0,BYREQST                                                       
         PUT   (R4),(R0)                                                        
*                                                                               
         CLOSE ((R4))                                                           
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
RLX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* GET DEMOS                                                           *         
* INPUT  : PARM1=A(PRD CODE)                                          *         
*          PARM2=A(EST CODE)                                          *         
* OUTPUT : CC EQ - DEMOS SET IN SVDEMS                                *         
*          CC NE - INVALID ESTIMATE                                   *         
***********************************************************************         
         SPACE 1                                                                
GETDEMS  NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         LLC   RE,0(R2)            TEST VALID PRODUCT/ESTIMATE                  
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         LLC   RF,0(R3)                                                         
         BCTR  RF,0                                                             
         AR    RE,RF                                                            
         L     R4,SBAESTTB                                                      
         LA    R4,0(RE,R4)                                                      
         SR    R1,R1                                                            
         ICM   R1,1,0(R4)                                                       
         BZ    GETDEMS9            NO                                           
         BCTR  R1,0                YES-GET THE DEMOS                            
         MHI   R1,ESTBUFFL                                                      
         L     RE,SBAESTBF                                                      
         LA    R1,0(R1,RE)                                                      
         USING ESTBUFFD,R1                                                      
         MVC   SVDEMS,EBDEMOS                                                   
         DROP  R1                                                               
         CR    RB,RB                                                            
         B     GETDEMSX                                                         
*                                                                               
GETDEMS9 LTR   RB,RB                                                            
*                                                                               
GETDEMSX J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* I/O ROUTINES                                                        *         
***********************************************************************         
         SPACE 1                                                                
XSPHIGH  MVC   KEYSAVE,KEY                                                      
         LA    RF,=C'DMRDHI'                                                    
         B     XSPDIR                                                           
*                                                                               
XSPSEQ   LA    RF,=C'DMRSEQ'                                                    
         B     XSPDIR                                                           
*                                                                               
XSPDIR   LR    R0,RE                                                            
         ST    RF,DMCB                                                          
         GOTO1 DATAMGR,DMCB,,=C'XSPDIR',KEYSAVE,KEY                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
XSPGET   LR    R0,RE                                                            
         LA    RF,=C'GETREC'                                                    
         B     XSPFILE                                                          
         SPACE 1                                                                
XSPPUT   LR    R0,RE                                                            
*                                                                               
         CLI   SBQTRACE,C'Y'                                                    
         BNE   *+14                                                             
         MVC   DUB,=CL8'*PUTREC*'                                               
         BAS   RE,TRACE                                                         
*                                                                               
         CLI   TWAWRITE,C'N'       WRITE= NO                                    
         BE    XSPFILEX                                                         
*                                                                               
         LA    RF,=C'PUTREC'                                                    
         B     XSPFILE                                                          
*                                                                               
XSPADD   LR    R0,RE                                                            
*                                                                               
         CLI   SBQTRACE,C'Y'                                                    
         BNE   *+14                                                             
         MVC   DUB,=CL8'*ADDREC*'                                               
         BAS   RE,TRACE                                                         
         CLI   TWAWRITE,C'N'       WRITE= NO                                    
         BE    XSPFILEX                                                         
*                                                                               
         LA    RF,=C'ADDREC'                                                    
*                                                                               
XSPFILE  ST    RF,DMCB                                                          
         GOTO1 DATAMGR,DMCB,,=C'XSPFIL',KEY+36,AIO,DMWORK                       
*                                                                               
XSPFILEX LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* ADD PASSIVE PIGGYBACK POINTER                                                 
*                                                                               
ADDPSSV  NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING SLKRECD,R6                                                       
*                                                                               
         L     R1,SBAIO3                                                        
         MVC   SLKKEY,0(R1)                                                     
*                                                                               
         IC    RE,SLKKPRD                                                       
         MVC   SLKKPRD,SLKKPRD2                                                 
         STC   RE,SLKKPRD2                                                      
         IC    RE,SLKKLEN                                                       
         MVC   SLKKLEN,SLKKLEN2                                                 
         STC   RE,SLKKLEN2                                                      
         OI    SLKKIND,SLKKIPSV                                                 
*                                                                               
         MVC   KEY+36(4),DMDSKADD                                               
*                                                                               
         CLI   SBQTRACE,C'Y'                                                    
         BNE   ADDPSSV2                                                         
*                                                                               
         MVC   DUB,=CL8'*ADDPSSV'                                               
         GOTO1 =V(PRNTBL),DMCB,(8,DUB),KEY,C'DUMP',40,=C'2D',          X        
               (C'P',SBPRINT)                                                   
*                                                                               
ADDPSSV2 CLI   TWAWRITE,C'N'       WRITE= NO                                    
         BE    ADDPSSVX                                                         
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMADD'),=C'XSPDIR',KEY,KEY                    
*                                                                               
ADDPSSVX B     XIT                                                              
         DROP  R6                                                               
*                                                                               
TRACE    NTR1  ,                                                                
         L     R2,AIO                                                           
         SR    RF,RF                                                            
         ICM   RF,3,32(R2)                                                      
         ST    RF,DMCB+12                                                       
         GOTO1 =V(PRNTBL),DMCB,(8,DUB),(R2),C'DUMP',,=C'2D',           X        
               (C'P',SBPRINT)                                                   
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* STORAGE                                                             *         
***********************************************************************         
         SPACE 1                                                                
STWORK   DS    0D                                                               
NXTENTRY DS    A                                                                
ASAVE    DS    A                                                                
ANXTREC  DS    A                   A(NEXT ENTRY IN RECTAB)                      
*                                                                               
STATION  DS    CL8                                                              
SVPRD    DS    XL1                                                              
SVEST    DS    XL1                                                              
SVOWD    DS    XL1                                                              
SVDPT    DS    CL1                                                              
SVSLN1   DS    XL1                                                              
SVSLN2   DS    XL1                                                              
SVWEEK   DS    XL2                                                              
SVDOL    DS    XL4                                                              
SVNET    DS    XL4                                                              
SVDOL2   DS    XL4                                                              
SVNET2   DS    XL4                                                              
SVSPT    DS    XL4                                                              
DEMVALS  DS    0XL16                                                            
DEMVAL1  DS    XL4                                                              
DEMVAL2  DS    XL4                                                              
DEMVAL3  DS    XL4                                                              
DEMVAL4  DS    XL4                                                              
SVKEY    DS    XL48                                                             
SVC2KEY  DS    XL13                                                             
C2KEY    DS    XL13                                                             
ACMLK    DS    F                                                                
ACMLKC2  DS    F                                                                
FLAG     DS    XL1                                                              
UPDATE   EQU   X'01'                                                            
SVBASDAY DS    XL1                                                              
PHASE    DS    XL1                                                              
ERR      DS    XL1                                                              
NUMDEMS  DS    XL1                                                              
CHANGE   DS    CL1                                                              
SAVED    DS    CL1                                                              
ADDSEQ0  DS    CL1                                                              
ADDHDR   DS    CL1                                                              
NEWREC   DS    CL1                                                              
REQST    DS    XL2                                                              
REQEND   DS    XL2                                                              
IOINDS   DS    XL1                 I/O INDICATORS                               
IOIRERD  EQU   X'80'               RE-READ                                      
SLPROF   DS    CL16                                                             
SVDEMS   DS    CL(MAXDEMS*3+1)                                                  
MYPRINT  DS    CL132                                                            
BYREQST  DS    CL80                SBY REQUEST                                  
PRDESTTB DS    (PRDESTMX)XL3                                                    
PRDESTTX EQU   *                                                                
PRDESTMX EQU   255                                                              
*                                                                               
STWORKLN EQU   *-STWORK                                                         
*                                                                               
ERRMSG1  DC    C'*** PIGGYBACK LOCKIN RECORDS FOUND FOR SINGLE PRODUCT X        
               REQUEST ***'                                                     
ERRMSG2  DC    C'*** DEMOS HAVE CHANGED SINCE LAST LOCKIN ***'                  
*                                                                               
MAXDEMS  EQU   6                   MAX N'LOCKIN DEMOS PER RECORD                
*                                                                               
SPTFIL   DC    C'SPTFIL '                                                       
XFF      DC    XL16'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'                           
FF       EQU   X'FF'                                                            
*                                                                               
SAVVALS  DS    0F                  START SAVED VALUES                           
ABYRQDCB DS    A                                                                
ARECTAB  DS    A                   RECORD KEY SAVE AREA                         
ARECEND  DS    A                   END OF REC TABLE                             
RECLENQ  EQU   40                  SIZE OF REC TO SAVE (KEY + DA)               
RECTABLN EQU   10000*RECLENQ       40 BYTES TO SAVE, MAX 10000 RECS             
DCBOPEN  DS    CL1                                                              
*                                                                               
SAVVALSL EQU   *-SAVVALS                                                        
*                                                                               
         DS    0D                                                               
BYRQFIL  DCB   DDNAME=BYRQFIL,DSORG=PS,RECFM=FB,BLKSIZE=3200,          X        
               LRECL=80,MACRF=PM                                                
BYRQFILL EQU   *-BYRQFIL                                                        
*                                                                               
XTRAREC  DS    XL2000                                                           
CLTREC   DS    XL1500                                                           
***********************************************************************         
* BUILD A LIST OF RECORDS THAT MAY NEED TO HAVE LOCKIN ELEMENTS       *         
* DELETED.  A SEPERATE ROUTINE WILL ACTUALLY DELETE THE ELEMS.        *         
***********************************************************************         
         SPACE 1                                                                
BLDLST   NTR1  BASE=*,LABEL=*                                                   
         L     R2,=F'-6'           SET REQUEST START AND END WEEKS              
         GOTO1 ADDAY,DMCB,SBQREQST,DUB,(R2)                                     
         GOTO1 DATCON,DMCB,DUB,(2,REQST)                                        
         GOTO1 (RF),(R1),SBQREQND,(2,REQEND)                                    
*                                                                               
         XC    KEY,KEY             READ THE STATION LOCKIN HEADERS              
         XC    SVEST,SVEST                                                      
         LA    R2,KEY                                                           
         USING SLKRECD,R2                                                       
         MVI   SLKKTYP,SLKKTYPQ                                                 
         MVI   SLKKSUB,SLKKSUBQ                                                 
         MVC   SLKKAGMD,SBBAGYMD                                                
         MVC   SLKKCLT,SBBCLT                                                   
         LA    R3,SLKKMKT-SLKKEY-1                                              
         OC    SBQMKT,SBQMKT       TEST MARKET FILTER                           
         BZ    BLD10                                                            
         CLC   SBQMKT(3),=C'ALL'                                                
         BE    BLD10                                                            
         PACK  DUB,SBQMKT                                                       
         CVB   R1,DUB                                                           
         STCM  R1,3,SLKKMKT                                                     
         LA    R3,L'SLKKMKT(R3)                                                 
         OC    SBQSTA,SBQSTA       TEST STATION FILTER                          
         BZ    BLD10                                                            
         CLC   SBQSTA(3),=C'ALL'                                                
         BE    BLD10                                                            
         GOTO1 MSPACK,DMCB,SBQMKT,SBQSTA,DUB                                    
         MVC   SLKKSTA,DUB+2                                                    
         LA    R3,L'SLKKSTA(R3)                                                 
         CLI   SBQBPRD,0           TEST PRODUCT FILTER                          
         BE    BLD10                                                            
         CLI   SBQBPRD,X'FF'                                                    
         BE    BLD10                                                            
         MVC   SLKKPRD,SBQBPRD                                                  
         LA    R3,L'SLKKPRD(R3)                                                 
*                                                                               
BLD10    BAS   RE,XSPHIGH                                                       
         B     *+8                                                              
*                                                                               
BLD20    BAS   RE,XSPSEQ                                                        
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE      TEST ANY MORE LOCKIN RECORDS                 
         BNE   BLD130                                                           
         TM    SLKKIND,SLKKIPSV    SKIP PASSIVES                                
         BO    BLD20                                                            
*                                                                               
         GOTO1 GETDEMS,DMCB,SLKKPRD,SLKKEST                                     
         BE    *+14                                                             
*                                  INVALID ESTIMATE-SKIP BEYOND EST             
         MVC   SLKKDPT(4),XFF      SKIP PAST ESTIMATE                           
         B     BLD10               AND READ HIGH                                
*                                                                               
         MVI   ERR,0                                                            
         MVI   SAVED,C'N'                                                       
*                                                                               
         CLC   SVEST,SLKKEST                                                    
         BE    BLD40                                                            
         GOTO1 GETESTNM            TO GET EST DATES                             
         MVC   SVEST,SLKKEST                                                    
*                                                                               
BLD40    CLI   SBQBPRD,0           YES-TEST PRODUCT FILTER                      
         BE    BLD50                                                            
         CLI   SBQBPRD,X'FF'                                                    
         BE    BLD50                                                            
         CLI   SLKKPRD2,0          YES-TEST PARTNER PRODUCT IN KEY              
         BE    BLD50                                                            
         OI    ERR,X'80'           YES-TAKE A NOTE                              
*                                                                               
BLD50    L     R2,SBAIO1                                                        
         ST    R2,AIO                                                           
         BAS   RE,XSPGET                                                        
*                                                                               
BLD60    SR    R0,R0                                                            
         LA    R6,SLKEL                                                         
*                                                                               
BLD70    CLI   0(R6),0                                                          
         BE    BLD100                                                           
         CLI   0(R6),LOKELCDQ                                                   
         BNE   BLD90                                                            
         USING LOKEL,R6                                                         
* IF THE LOKWEEK IS BEFORE START OF ESTIMATE, IT'S FROM AN OLD                  
* ESTIMATE AND SHOULD BE DELETED!                                               
         CLC   LOKWEEK,SBESTSTP    WEEK BEFORE EST START?                       
         BL    BLD80                YES                                         
*                                                                               
         CLC   LOKWEEK,REQST       CHECK WEEK'S WITHIN REQUEST RANGE            
         BL    *+14                                                             
         CLC   LOKWEEK,REQEND                                                   
         BNH   *+12                                                             
         OI    ERR,X'40'           NO-TAKE NOTE                                 
         B     BLD90                                                            
*                                                                               
         TM    ERR,X'80'           TEST SINGLE PRD REQUEST AND THERE'S          
         BZ    BLD80               A PARTNER PRODUCT IN THE KEY                 
         OC    LOKSPOTS,LOKSPOTS   YES-TEST ANY DATA                            
         BNZ   BLDERR10            YES-EXIT WITH FATAL ERROR MESSAGE            
*                                                                               
BLD80    CLI   SAVED,C'Y'                                                       
         BE    BLD90                                                            
         L     R1,ANXTREC                                                       
         MVC   0(40,R1),KEY        SAVE KEY & D/A                               
         AHI   R1,RECLENQ                                                       
         C     R1,ARECEND                                                       
         BL    *+6                                                              
         DCHO                      RECTAB TOO SMALL                             
         ST    R1,ANXTREC                                                       
         MVI   SAVED,C'Y'                                                       
*                                                                               
BLD90    IC    R0,1(R6)            NEXT ELEMENT                                 
         AR    R6,R0                                                            
         B     BLD70                                                            
*                                                                               
BLD100   TM    ERR,X'40'           TEST LOCKIN LEFT OUTSIDE PERIOD              
         BZ    BLD120                                                           
         LA    R0,MAXDEMS          YES-CHECK THAT THE DEMOS AREN'T              
         LA    R1,SVDEMS               CHANGING                                 
         LA    RE,SLKDEM1                                                       
*                                                                               
BLD110   CLI   1(RE),0                                                          
         BE    BLD120                                                           
*                                                                               
         CLC   1(2,RE),1(R1)                                                    
         BNE   BLDERR20            DEMOS HAVE CHANGED - FATAL ERROR             
         LA    R1,3(R1)                                                         
         LA    RE,3(RE)                                                         
         BCT   R0,BLD110                                                        
         B     BLD120                                                           
*                                                                               
BLD120   LA    R2,KEY              READ SEQUENTIAL                              
         B     BLD20                                                            
*                                                                               
BLD130   XC    SBBMKT,SBBMKT                                                    
         XC    SBBSTA,SBBSTA                                                    
         L     R1,SBAIO3                                                        
         XC    0(256,R1),0(R1)                                                  
         B     BLDX                                                             
*                                                                               
BLDERR10 MVC   MYPRINT,SPACES                                                   
         MVC   MYPRINT(L'ERRMSG1),ERRMSG1                                       
         B     BLDERRX                                                          
*                                                                               
BLDERR20 MVC   MYPRINT,SPACES                                                   
         MVC   MYPRINT(L'ERRMSG2),ERRMSG2                                       
*                                                                               
BLDERRX  GOTO1 SBPRINT,DMCB,MYPRINT-1,=C'BL01'                                  
         MVI   RPMODE,RPSTOP       STOP PROCESSING NOW                          
*                                                                               
* RPSTOP DOESN'T PREVENT RPFINAL - MAKE SURE IO3 IS CLEAR                       
         L     R1,SBAIO3                                                        
         XC    0(256,R1),0(R1)                                                  
*                                                                               
BLDX     J     XIT                                                              
         DROP  R2,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
* REMOVE OLD LOCKIN ELEMENTS FROM RECORD IN AIO.                      *         
***********************************************************************         
         SPACE 1                                                                
REMOLD   NTR1  BASE=*,LABEL=*                                                   
         USING SLKRECD,R2                                                       
         L     R2,AIO                                                           
         MVI   CHANGE,C'N'                                                      
         CLC   SVEST,SLKKEST                                                    
         BE    REM10                                                            
         CLI   RPMODE,RPAFTOUT     AFTER DRIVER OUTPUT                          
         BNE   REM8                                                             
         MVC   SBBPRD,SLKKPRD                                                   
         MVC   SBBEST,SLKKEST                                                   
*                                                                               
REM8     GOTO1 GETESTNM            TO GET EST DATES                             
         MVC   SVEST,SLKKEST                                                    
*                                                                               
REM10    SR    R0,R0                                                            
         LA    R6,SLKEL                                                         
*                                                                               
REM20    CLI   0(R6),0                                                          
         BE    REM50                                                            
         CLI   0(R6),LOKELCDQ                                                   
         BNE   REM40                                                            
         USING LOKEL,R6                                                         
* IF THE LOKWEEK IS BEFORE START OF ESTIMATE, IT'S FROM AN OLD                  
* ESTIMATE AND SHOULD BE DELETED!                                               
         CLC   LOKWEEK,SBESTSTP    WEEK BEFORE EST START?                       
         BL    REM30                YES                                         
*                                                                               
         CLC   LOKWEEK,REQST       CHECK WEEK'S WITHIN REQUEST RANGE            
         BL    *+14                                                             
         CLC   LOKWEEK,REQEND                                                   
         BNH   *+12                                                             
         OI    ERR,X'40'           NO-TAKE NOTE                                 
         B     REM40                                                            
*                                                                               
REM30    MVI   0(R6),FF            YES-DELETE THE ELEMENT                       
         GOTO1 HELLO,DMCB,(C'D',=C'XSPFIL'),(X'FF',(R2)),0,0,0                  
         MVI   CHANGE,C'Y'                                                      
         B     REM20                                                            
*                                                                               
REM40    IC    R0,1(R6)            NEXT ELEMENT                                 
         AR    R6,R0                                                            
         B     REM20                                                            
*                                                                               
REM50    TM    ERR,X'40'           WRITE PHASE - TEST ANY LOCKIN                
         BO    REM60                             ELEMENTS LEFT                  
         MVI   CHANGE,C'Y'         NO-REMOVE DEMO CATEGORIES                    
         LA    RE,MAXDEMS                                                       
         MHI   RE,3                                                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     REM60                                                            
         XC    SLKDEM1(0),SLKDEM1                                               
*                                                                               
REM60    CLI   CHANGE,C'Y'         YES-TEST RECORD CHANGED                      
         BNE   *+10                                                             
         MVC   SLKUPDT,BTODAY      UPDATE THE LATEST CHANGE DATE                
         J     XIT                                                              
*                                                                               
         DROP  R2,R6                                                            
         LTORG                                                                  
CLTRECD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENXLK                                                       
         EJECT                                                                  
       ++INCLUDE SPGENWIPW                                                      
         EJECT                                                                  
       ++INCLUDE SPGENBGR                                                       
         EJECT                                                                  
FQRECORD DSECT                     SPONSOR STYLE REQUEST CARD                   
FQAREA   DS    0CL80   COLUMN                                                   
FQPROG   DS    0CL2    ------                                                   
FQCODE   DS    CL2        1        PROGRAM CODE                                 
FQAGY    DS    CL2        3        AGENCY CODE                                  
FQMED    DS    CL1        5        MEDIA CODE (R/T)                             
FQCLT    DS    CL3        6        CLIENT CODE                                  
FQPGR    DS    CL1        9        PROCESS BY DIVISION                          
FQMGR    DS    CL1       10        PROCESS BY DISTRICT                          
FQCLOFFC DS    CL1       11        CLIENT OFFICE FILTER                         
FQBYID   EQU   FQCLOFFC            C'Y' IF BUYS PROCESSED BY ID                 
FQPRD    DS    CL3       12        PRODUCT MNEMONIC                             
FQMKT    DS    CL4       15        MARKET NUMBER                                
FQSTA    DS    CL5       19        STATION CALL LETTERS                         
FQEST    DS    CL3       24        ESTIMATE NUMBER                              
FQESTEND DS    CL3       27        LAST NUMBER IN ESTIMATE GROUP                
FQDEMOVRD DS   CL1       30        Y=DEMO OVERRIDE ACTIVE                       
FQCONTREQ DS   CL1       31        C'*' ==> DATA IN QAREA2                      
FQSTAUTO DS    CL3       32        AUTO REQUEST START DATE                      
FQENDAUTO DS   CL3       35        AUTO REQUEST END DATE                        
         ORG   FQSTAUTO+2                                                       
FQDEMNOS DS    CL4                 DEMO OVERRIDE NUMBERS                        
FQSTART  DS    CL6       38        REQUEST START DATE                           
FQEND    DS    0CL6      44        REQUEST END DATE                             
FQTODAY  DS    CL6       44                                                     
FQBOOK1  DS    CL4       50        RATING BOOK (YYMM) GOAL/ESTD DATA            
FQHUT1   DS    CL2       54        HUT ADJUSTMENT MONTH                         
FQRERATE DS    CL1       56        RERATE TYPE  I=INVOICE                       
*                                               P=PURCHASED                     
*                                               A=ADJUST ONLY                   
*                                               U=UPGRADE (+Q2BOOK2)            
FQCOMPARE DS   CL1       57        DATA COMPARE OPTION                          
*                                  A=GOAL V PURCHASED                           
*                                  B=GOAL V AFFIDAVIT                           
*                                  C=PURCHASED V PURCHASED (RERATED)            
*                                  D=PURCHASED V AFFIDAVIT                      
*                                  E=LOCKIN V PURCHASED                         
*                                  F=LOCKIN V AFFIDAVIT                         
*                                  L=GOAL V PURCHASED, LOCKIN PURCHASED         
FQAFFIL  DS    CL1       58        AFFILIATION FILTER                           
FQPRGTYPE DS   CL1       59        PROGRAM TYPE FILTER                          
FQDPTDET DS    CL1       60        DAYPART DETAIL CONTROL                       
*                                  A=SHOW FULL DETAIL (DEFAULT)                 
*                                  B=SUPPRESS SPOT-LENGTH                       
*                                  C=SUPPRESS SPOT-LENGTH & DAY-PART            
FQDPTMENU DS   CL1       61        DAYPART MENU OVERRIDE                        
FQOPT1   DS    CL1       62        OPTION 1                                     
FQOPT2   DS    CL1       63        OPTION 2                                     
FQOPT3   DS    CL1       64        OPTION 3                                     
FQOPT4   DS    CL1       65        OPTION 4                                     
FQOPT5   DS    CL1       66        OPTION 5                                     
FQGRP    DS    CL2       67        GROUP                                        
FQFILTER EQU   FQGRP               FILTER TYPE/VALUE                            
FQUESTOR DS    CL12      69        REQUESTOR NAME                               
         EJECT                                                                  
* OTHER DSECTS ARE HIDDEN IN HERE                                               
*                                                                               
*SPWRIWORKD                                                                     
*DDSPOOLD                                                                       
*DDSPLWORKD                                                                     
*DDBIGBOX                                                                       
*DRGLOBAL                                                                       
*DRIVETABLE                                                                     
*DDMASTD                                                                        
*DDMONSPARM                                                                     
*SPGENEST                                                                       
*SPGENBUY                                                                       
*SPWRIFFD                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPWRIWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DRGLOBAL                                                       
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DDMASTD                                                        
MONSPRMD DSECT                                                                  
       ++INCLUDE DDMONSPARM                                                     
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPWRIFFD                                                       
         PRINT ON                                                               
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SPWRIF1D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPWRIEBD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDTWADCOND                                                     
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018SPWRI14   02/22/21'                                      
         END                                                                    
