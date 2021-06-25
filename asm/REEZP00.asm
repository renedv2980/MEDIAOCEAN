*          DATA SET REEZP00    AT LEVEL 207 AS OF 05/16/13                      
*PHASE T82A00A                                                                  
*INCLUDE TWABLD                                                                 
*INCLUDE UNBOOK                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE REPRPUPV                                                               
         TITLE 'REEZP00(T82A00) - REP PC EZ-POST'                               
*********************************************************************           
* ALL UTIL ROUTINES SHOULD FOLLOW USE THE FOLLOWING OUTPUT FORMAT:              
*                                                                               
*   CONDITION CODE NOT EQUAL                                                    
*                                                                               
*       - INDICATES THE ROUTINE FAILED                                          
*            COULD BE BECAUSE OF INVALID DATA, NO DATA ON FILE, ETC.            
*       - REASON/ERROR CODE IN FIRST HALF WORD OF OUTPUT AREA                   
*                                                                               
*   CONDITION CODE EQUAL                                                        
*                                                                               
*       - INDICATES THE ROUTINE SUCCEEDED                                       
*       - OUPUT AREA HAS THE FOLLOWING FORMAT:                                  
*          BYTE 0-1    LENGTH OF DATA FOR A SINGLE ITEM (I)                     
*          BYTE 2      NUMBER OF SUBDIVISIONS IN A SINGLE DATA ITEM (N)         
*          BYTE 3-(3+2N) 1 BYTE LENGTH OF EACH DATA SUBDIVISION                 
*                        1 BYTE DATA TYPE CODE                                  
*          BYTE (4+2N)-(4+2N+JI)   DATA (J = 0 - NUMBER OF ITEMS)               
*          BYTE (5+2N+JI)          00  END OF DATA                              
*---------------------------------------------------------------------*         
*  HISTORY AS OF 11/17/99                                             *         
*                                                                     *         
* 11/17/1999  JRD    NEW PROGRAM                                      *         
* 07/19/2000  JRD    ADD CONTRACT DOWNLOAD TRACKING ELEMENT           *         
* 01/29/2001  JRD    HANDLE COMMENTS WITH NULLS(FROM MAKEGOODS)       *         
* 05/22/2001  JRD    HANDLE CANCELLED BUYLINES BY CHECKING MODCODE    *         
* 07/29/2002  ABEA   ADD AGENCY INVOICES                              *         
*             JRD        BUYLINE RATINGS                              *         
*                        AGENCY DEMO CATEGORIES                       *         
*                        VARIOUS BUG FIXES                            *         
* 10/02/2002  JRD    SEND PRIMARY DEMOS WITH A P                      *         
* 10/08/2002  JRD    SEND PRIMARY DEMOS WITH A P ONLY 2.0.0.12+       *         
* 01/24/2003  JRD    ONLY ONE MARKET IN GTBKS                         *         
* 06/18/2003  JRD    MULTIPLE MARKETS IN GTBKS AND SEND STANDARD      *         
*                    SURVEY BOOKTYPE 2.0.0.24+ (VERSION IN HEX=18)    *         
* 09/30/2003  JRD    CHECK FOR OLD STYLE AGENCY DEMO ELEMENTS         *         
*                    CHECK FOR SPACES AS THE DEMO                     *         
* 02/13/2004  JRD    CHECK FOR SECOND OLD STYLE AGENCY DEMO ELEMENTS  *         
*                                                                     *         
* 04/06/2004  HQ     SKIP USER DEFINED DEMO CATEGORY                  *         
* 02/04/2005  HQ     HARD CODE FOR SELTEL WHAM-T CALL LETTER SWITCH   *         
* 10/19/2005  HQ     LOCAL INVOICE PROCESSING                         *         
* 03/07/2006  HQ     FIX LOCAL INVOICE START END DATE BUG             *         
* 03/16/2006  HQ     FIX MINIO SEQUENCE ERROR, FIX MULTI-PERIOD READ  *         
* 04/10/2006  HQ     CLEAR KEY                                        *         
* 04/11/2006  HQ     FIX EZP ACTIVITY ELEMENT UPDATE                  *         
* 03/13/2007  HQ     NEW CODE TO READ MO NATIONAL INVOICES            *         
* 03/23/2007  HQ     FIX FOR READING REPPACK CONTRACT INVOICES        *         
* 03/26/2007  BU     ACCESS URL RECORD                                *         
* 05/16/2007  BU     URLSTORE INCREASED TO 80 CHARS:  REASSEMBLY      *         
* 07/26/2007  BU    STATION/OFFICE LOOKUP:  LOOK FOR 3A IF NO 2E      *         
*                   ELEMENT ON FILE.                                  *         
*                                                                     *         
* 06/09/2008  KUI    2-CHAR BOOTYPE SUPPORT                           *         
* 11/12/2008  KUI    2 DECIMAL AND OTP BOOKTYPE SUPPORT               *         
* 06/17/2009  KUI    SUPPORT ISCI CODE, AND EXACT TIME URL            *         
* 07/21/2009  KUI    ADD EI ADV/PRD/EST AND TRAFFIC NUMBER            *         
* 08/04/2009  KUI    GET EI ADV/PRD/EST/TRAFFIC NUMBER FOR BUY REQUEST*         
* 05/16/2013  KUI    SKIP UPDATING A7 ACTIVITY ELEMENT                *         
***********************************************************************         
T82A00   CSECT                                                                  
         PRINT NOGEN                                                            
         NMODL LENWORK,T82A00,RR=R2,CLEAR=YES                                   
         LR    R7,RB                                                            
         AH    R7,=Y(COMMON-T82A00)                                             
         USING COMMON,R7                                                        
*                                                                               
*---------------------*                                                         
* INITIALIZATION CODE *                                                         
*---------------------*                                                         
         USING WORKD,RC                                                         
         ST    R2,BASERELO                                                      
         ST    RD,BASERD                                                        
         ST    R1,ASYSPARM                                                      
         MVC   ATIOB,0(R1)                                                      
         MVC   ATWA,4(R1)                                                       
         MVC   ASYSFACS,8(R1)                                                   
         MVC   ATIA,12(R1)                                                      
         MVC   ACOMFACS,16(R1)                                                  
*                                                                               
         LR    RE,RC                                                            
         AH    RE,=Y(IOAREA1-WORKD)                                             
         ST    RE,AIOREC                                                        
         ST    RE,AIO1                                                          
         AH    RE,=Y(LENIO)                                                     
         ST    RE,AIO2                                                          
         AH    RE,=Y(LENIO)                                                     
         ST    RE,AIO3                                                          
         AH    RE,=Y(LENIO)                                                     
         ST    RE,AIO4                                                          
*                                                                               
         L     RE,=V(TWABLD)                                                    
         A     RE,BASERELO                                                      
         ST    RE,VTWABLD                                                       
         L     RE,=V(UNBOOK)                                                    
         A     RE,BASERELO                                                      
         ST    RE,VUNBOOK                                                       
         L     RE,=V(RECUP)                                                     
         A     RE,BASERELO                                                      
         ST    RE,VRECUP                                                        
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         L     RE,ATWA                                                          
         USING T82AFFD,RE                                                       
         MVC   REPALPHA,TWAAGY                                                  
         MVC   USRID,TWAUSRID                                                   
         DROP  RA                                                               
         DROP  RE                                                               
*                                                                               
         USING T82AFFD,RA                                                       
MB       USING FAMSGD,FAMSGBLK                                                  
*                                                                               
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         MVC   VDMGR,CDATAMGR                                                   
         MVC   VCOLY,CCALLOV                                                    
         MVC   VGETMSG,CGETMSG                                                  
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VSCANNER,CSCANNER                                                
         MVC   VHEXIN,CHEXIN                                                    
         MVC   VHEXOUT,CHEXOUT                                                  
         MVC   VCASHVAL,CCASHVAL                                                
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VDATCON,CDATCON                                                  
         MVC   VADDAY,CADDAY                                                    
         MVC   VPERVERT,CPERVERT                                                
         MVC   VGETDAY,CGETDAY                                                  
         MVC   VPERVAL,CPERVAL                                                  
         MVC   VGLOBBER,CGLOBBER                                                
         MVC   VDEMAND,CDEMAND                                                  
         MVC   VSWITCH,CSWITCH                                                  
         DROP  R1                                                               
*                                                                               
***********************************                                             
* SET UP ADDRESSES FOR CORE-RESIDENT PHASES                                     
***********************************                                             
         L     R2,=A(PHASES)       R2=A(PHASE LIST)                             
         A     R2,BASERELO                                                      
         LA    R3,APHASES          R3=A(ADDRESS LIST)                           
         LA    R4,PHASESN          R4=MAX NUMBER OF PHASES (CORERES)            
         SR    R0,R0                                                            
         ICM   R0,14,=X'D9000A'                                                 
         LA    R1,DMCB                                                          
         L     RF,VCOLY                                                         
*                                                                               
         GOTO1 (RF),(R1),0,(R0)    <==  SPECIAL FOR BOOKVAL BECAUSE             
         MVC   VBOOKVAL,0(R1)             QBOOKVAL IS EQUATED TO 0              
*                                                                               
INIT0002 ICM   R0,1,0(R2)          ANY ENTRY HERE?                              
         BZ    INIT0004            NONE, SKIP TO THE NEXT ENTRY                 
*                                                                               
         GOTO1 (RF),(R1),0,(R0)                                                 
         MVC   0(4,R3),0(R1)                                                    
*                                                                               
INIT0004 LA    R2,1(R2)            BUMP TO THE NEXT ENTRY                       
         LA    R3,4(R3)                                                         
         BCT   R4,INIT0002                                                      
*                                                                               
         LR    RE,RB                                                            
         AH    RE,=Y(VROUTS-T82A00)                                             
         LA    R0,NUMROUTS                                                      
         SR    RF,RF                                                            
         LA    R1,VREAD                                                         
INIT0010 DS    0H                                                               
         ST    RE,0(R1)                                                         
         STC   RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,INIT0010                                                      
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         GOTOR GUSER               GET USER NAME BASE ON SIGN ON ID             
         GOTOR CHKLOCAL            CHECK TO SEE IF IT IS LOCAL SIGNON           
         GOTOR URLFIND             RETRIEVE URL FROM 17 RECORD                  
*                                                                               
K        USING RREPKEY,KEY         GET PARENT REP CODE                          
         XC    K.RREPKEY,K.RREPKEY                                              
         MVI   K.RREPKTYP,X'01'                                                 
         MVC   K.RREPKREP,REPALPHA                                              
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RREPKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                NO REP RECORD                                
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         L     R6,AIOREC                                                        
         USING RREPREC,R6                                                       
         MVC   REPCODE,RREPKREP                                                 
         DROP  R6                                                               
*                                                                               
         LA    R6,RREPELEM-RREPREC(R6)                                          
         CLI   0(R6),X'01'                                                      
         BE    *+6                                                              
         DC    H'0'                LOST REP ELEMENT                             
*                                                                               
         USING RREPELEM,R6                                                      
         MVC   PARALPHA,RREPPAR                                                 
         MVC   REPNAME,RREPNAME                                                 
         MVC   REPADDR,RREPADDR                                                 
         MVC   LOWRND,RREPPROF+20                                               
         NI    LOWRND,X'0F'        MAKE IT HEX                                  
         MVC   HIRND,RREPPROF+21                                                
         NI    HIRND,X'0F'         MAKE IT HEX                                  
*                                                                               
         CLI   LOWRND,5            MAKE SURE ROUNDING IS VALID                  
         BH    *+20                BAD VALUE - CLEAR ROUNDING                   
         CLI   HIRND,5                                                          
         BL    *+12                BAD VALUE - CLEAR ROUNDING                   
         CLI   HIRND,9                                                          
         BL    *+12                EVERYTHING IS OK                             
         MVI   LOWRND,0                                                         
         MVI   HIRND,0                                                          
*                                                                               
         MVI   MISCFLG,0                                                        
         DROP  R6                                                               
*                                                                               
*  URL NO LONGER DRAWN FROM REP RECORD                                          
*&&DO                                                                           
         XC    URLSTORE,URLSTORE   CLEAR URL STORAGE                            
         XC    URLLEN,URLLEN       CLEAR LENGTH                                 
INIT0020 EQU   *                                                                
         ZIC   RF,1(R6)                                                         
         AR    R6,RF               BUMP TO NEXT ELEMENT                         
         CLI   0(R6),0             END OF RECORD?                               
         BE    INIT0040            YES                                          
         CLI   0(R6),X'11'         NO  - URL ELEMENT?                           
         BNE   INIT0020            NO  - GO BACK FOR NEXT ELEMENT               
         ZIC   RF,1(R6)            YES - GET ELEMENT LENGTH                     
         SH    RF,=H'2'            CALCULATE URL LENGTH                         
         ST    RF,URLLEN           SAVE URL LENGTH                              
         BCTR  RF,0                SET REG FOR MOVE BY LENGTH                   
         EX    RF,INIT0030         MOVE DATE BY LENGTH                          
         B     INIT0040                                                         
INIT0030 EQU   *                                                                
         MVC   URLSTORE(0),2(R6)   MOVE URL BY LENGTH                           
*&&                                                                             
INIT0040 EQU   *                                                                
         GOTO1 GETPROF,DMCB,('RREPQEZP',EZPPROFS)                               
         GOTO1 GETPROF,DMCB,('RREPQCNT',CONPROFS)                               
*                                                                               
         BAS   RE,INIFALNK         INITIALIZE FALINK BLOCK                      
         GOTO1 VFALINK,DMCB,FABLK  GIVE FALINK CONTROL                          
         B     EXIT                                                             
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
COMMON   DS    0D                                                               
         DC    CL8'**FAMAP*'                                                    
       ++INCLUDE REEZPMAP                                                       
         EJECT                                                                  
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    DS    0H                  SET CC LOW & FAMSGNO                         
         MVC   MB.FAMSGNO,ERROR                                                 
         CLI   *,FF                                                             
         B     EXIT                                                             
EXITNO   LTR   RB,RB               SET CC NOT EQUAL                             
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         B     EXIT                                                             
EXIT     DS    0H                  JUST EXIT                                    
         XIT1                                                                   
*                                                                               
ETOOBIG  MVC   ERROR,=Y(804)                                                    
         B     EXITL                                                            
*                                                                               
EPARMSEQ MVC   ERROR,=Y(210)                                                    
         B     EXITL                                                            
*                                                                               
EINVLEN  MVC   ERROR,=Y(85)                                                     
         B     EXITL                                                            
*                                                                               
DINVMERR MVC   ERROR,=Y(1299)                                                   
         B     EXITL                                                            
*                                                                               
DINVSERR MVC   ERROR,=Y(1267)                                                   
         B     EXITL                                                            
*                                                                               
EINVUPG  MVC   ERROR,=Y(235)                                                    
         B     EXITL                                                            
*                                                                               
EXITINV  MVC   ERROR,=Y(2)                                                      
         B     EXITL                                                            
*                                                                               
EBADSTA  DS    0H                                                               
         MVC   ERROR,=Y(150)                                                    
         B     ADDFLD                                                           
*                                                                               
EBADBOOK DS    0H                                                               
         MVC   ERROR,=Y(232)                                                    
         B     ADDFLD                                                           
*                                                                               
EBADDEMO DS    0H                                                               
         MVC   ERROR,=Y(233)                                                    
         B     ADDFLD                                                           
*                                                                               
EBADUPNM DS    0H                                                               
         MVC   ERROR,=Y(836)                                                    
         B     ADDFLD                                                           
*                                                                               
ADDFLD   DS    0H                  ADD FIELD IN WORK TO MESSAGE                 
         MVI   MB.FAMSGXTR,C'('                                                 
         ZIC   RE,WORK+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   MB.FAMSGXTR+1(0),WORK+8                                          
         LA    RE,MB.FAMSGXTR+2(RE)                                             
         MVI   0(RE),C')'                                                       
         B     EXITL                                                            
*                                                                               
         GETEL R8,=Y(RCONELEM-RCONREC),ELCODE                                   
         GETEL2 R6,=Y(SNVELS-SNVKEY),ELCODE                                     
*                                                                               
***********************************************************************         
* TABLE ITERATION ROUTINE  - EXPECTS RF TO HOLD A(TABLE)                        
*                          - EXPECTS R1 TO HOLD MAPCODE                         
***********************************************************************         
ITER     DS    0H                                                               
         CLC   0(2,RF),=X'0000'    E.O.T.                                       
         BE    ITER04              UNKNOWN MAPCODE                              
         CLM   R1,3,0(RF)          R1 HOLDS MAPCODE                             
         BE    ITER02              MATCHED                                      
         LA    RF,FLDTABLQ(RF)                                                  
         B     ITER                ITERATE THIS TABLE                           
*                                                                               
ITER02   LR    RE,RF               @@ DEBUG  @@                                 
         ICM   RF,15,4(RF)         ROUTINE TO HANDLE THE VERB                   
         A     RF,BASERELO                                                      
         BR    RF                                                               
*                                                                               
ITER04   DS    0H                                                               
         MVC   ERROR,=Y(841)                                                    
         B     EXITL                                                            
***********************************************************************         
* SWITCH TO SPOT SYSTEM                                                         
***********************************************************************         
REPSYS   NTR1                                                                   
         GOTO1 VSWITCH,DMCB,=C'REP ',0                                          
         CLI   4(R1),2             SYSTEM NOT OPEN                              
         BE    EXITL                                                            
         CLI   4(R1),1                                                          
         BE    EXITL                                                            
         MVI   CURRSYS,C'R'                                                     
         B     EXITOK                                                           
***********************************************************************         
* SWITCH TO SPOT SYSTEM                                                         
***********************************************************************         
SPTSYS   NTR1                                                                   
         GOTO1 VSWITCH,DMCB,=C'SPOT',0                                          
         CLI   4(R1),2             SYSTEM NOT OPEN                              
         BE    EXITL                                                            
         CLI   4(R1),1                                                          
         BE    EXITL                                                            
                                                                                
         MVI   CURRSYS,C'S'                                                     
         B     EXITOK                                                           
***********************************************************************         
* DATAMGR CALLS FOR XSPDIR AND XSPFIL                                           
***********************************************************************         
XSPDHIGH NTR1                                                                   
         MVC   KEYSAVE,KEY                                                      
         MVC   COMMAND,=CL8'DMRDHI'                                             
         B     XSPDRCT                                                          
*                                                                               
XSPDSEQ  NTR1                                                                   
         MVC   COMMAND,=CL8'DMRSEQ'                                             
*                                                                               
XSPDRCT  DS    0H                                                               
         CLI   CURRSYS,C'S'                                                     
         BE    XSPD10                                                           
         GOTO1 SPTSYS                                                           
         BNE   EXITL               SPOT SYSTEM NOT STARTED                      
*                                                                               
XSPD10   XC    DMWORK,DMWORK                                                    
         XC    DMINBTS,DMINBTS                                                  
         GOTO1 VDMGR,DMCB,(DMINBTS,COMMAND),=C'XSPDIR',KEY,KEY                  
         CLI   DMCB+8,0            NO ERRORS?                                   
         BE    XSPD20                                                           
         TM    DMCB+8,X'82'        EOF OR RECORD DELETED?                       
         BNZ   XSPD20                                                           
         DC    H'0'                NO, THEN DIE                                 
*                                                                               
XSPD20   DS    0H                                                               
*                                                                               
XSPDX    XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* INITIALIZE FALINK                                                             
***********************************************************************         
INIFALNK NTR1                                                                   
         LA    R0,FABLK                                                         
         LA    R1,FALINKDL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         OI    CONSERVH+1,X'01'    SERVICE REQUEST ALWAYS MODIFIED              
         OI    CONSERVH+6,X'80'                                                 
*                                                                               
         XC    VERSION,VERSION     CLEAR PC VERSION - UNLESS FROM GLOB          
*                                                                               
         LA    R2,FABLK                                                         
         ST    R2,AFABLK           FOR OTHER OVERLAYS                           
         USING FALINKD,R2                                                       
         LA    R1,CONINPH          SET A(FIRST SCREEN POSITION)                 
         ST    R1,FALABLD                                                       
         MVC   FALTBLD,VTWABLD         A(TWABLD)                                
         L     R1,ACOMFACS             A(SWITCH)                                
         L     R1,CSWITCH-COMFACSD(R1)                                          
         ST    R1,FALASWCH                                                      
         LA    R1,*                                                             
         A     R1,=A(RECEIVE-(*-4))    A(MY RECEIVE ROUTINE)                    
         ST    R1,FALARCV                                                       
         LA    R1,*                                                             
         A     R1,=A(SEND-(*-4))       A(MY SEND ROUTINE)                       
         ST    R1,FALASND                                                       
***JRD   LA    R1,TRANSLATE            A(ELEMENT TRANSLATION ROUTINE)           
***JRD   ST    R1,FALATRN                                                       
         LA    R1,*                                                             
         A     R1,=A(BREAK-(*-4))      A(BREAK ROUTINE)                         
         ST    R1,FALASTP                                                       
         LA    R1,*                                                             
         A     R1,=A(RESUME-(*-4))     A(RESUME ROUTINE)                        
         ST    R1,FALARSM                                                       
*        LA    R1,*                                                             
*        A     R1,=A(OVRFLO-(*-4))     A(RESUME ROUTINE)                        
*        ST    R1,FALAFUL                                                       
         XC    FAMSGBLK,FAMSGBLK                                                
         LA    R1,FAMSGBLK             A(MESSAGE BLOCK)                         
         ST    R1,FALAMSG                                                       
         LA    R1,FACON                A(CONTROL FIELD BUFFER)                  
         ST    R1,FALACON                                                       
         L     R1,ATWA                                                          
         AH    R1,=Y(SVFALINK-T82AFFD) A(FALINK SAVED STORAGE)                  
         ST    R1,FALASVE                                                       
         LA    R1,*                                                             
         A     R1,=A(FAMAP-(*-4))      A(MAP TABLE)                             
         ST    R1,FALAMAP                                                       
         ST    R1,AMAPTAB          FOR OTHER OVERLAYS                           
         MVC   FALAPGS,TWAPGS                                                   
         B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
TWAPGS   DC    AL4(FALATMS)                                                     
         EJECT                                                                  
***********************************************************************         
* GSTABKS - GET BOOKS FOR STATION(FROM TAPE)                                    
*   INPUT:   P1  A(STATION)                                                     
*            P2  A(OUTPUT AREA)                                                 
*                                                                               
***********************************************************************         
GSTABKS  NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
*                                                                               
         L     R4,AIO1                                                          
DB       USING DBLOCKD,R4                                                       
         XC    DB.DBLOCK,DB.DBLOCK                                              
         MVC   DB.DBAREC,AIO2                                                   
         MVC   DB.DBSELAGY,REPALPHA                                             
         MVC   DB.DBFILE,=C'TP '                                                
         MVC   DB.DBCOMFCS,ACOMFACS                                             
         MVI   DB.DBSELMED,C'T'                                                 
         MVC   DB.DBSELSRC,RTSRVC                                               
         MVC   DB.DBSELSTA,0(R2)                                                
         CLI   DB.DBSELSTA+4,C' '                                               
         BH    *+8                                                              
         MVI   DB.DBSELSTA+4,C'T'                                               
*                                                                               
         MVI   DB.DBFUNCT,DBGETMB     GET MARKET BOOKS FOR STATION              
*                                                                               
         MVI   BYTE,0                 CLEAR SAVED BOOK TYPE                     
         CLC   VERSION,=XL4'02000000'                                           
         BL    *+8                                                              
         MVI   BYTE,FF                SHOW STANDARD                             
*                                                                               
         XC    HALF,HALF              CLEAR SAVED MARKET                        
*                                                                               
         GOTO1 VDEMAND,DMCB,DB.DBLOCK,BOOKHOOK,0                                
         DROP  DB                                                               
*                                                                               
         B     EXITOK                                                           
*----------------------------------------------------------------------         
* BOOKHOOD FOR GTSTABKS                                                         
*----------------------------------------------------------------------         
BOOKHOOK NTR1                                                                   
         L     R4,AIO1                                                          
         USING DBLOCKD,R4                                                       
         L     R5,DBAREC                                                        
         USING SBKEY,R5                                                         
         OC    SBKMKT,SBKMKT       NO SPILL MARKETS                             
         BNZ   EXITOK                                                           
         CLI   SBBTYP,X'05'        NO 05 BOOK TYPES                             
         BE    EXITOK                                                           
*                                                                               
         CLC   VERSION,=XL4'02000018'                                           
         BNL   BKH0004                                                          
*                                                                               
         OC    HALF,HALF                                                        
         BNZ   BKH0002                                                          
*                                                                               
         MVC   HALF,SBRMKT                                                      
         B     BKH0004                                                          
*                                                                               
BKH0002  DS    0H                                                               
         CLC   HALF,SBRMKT         ONLY ONE MARKET                              
         BNE   EXITOK                                                           
*                                                                               
BKH0004  DS    0H                                                               
         CLC   SBBOOK,=AL2(BTCUTOFF) IF BK IS PRIOR TO BKTYP CUT-OFF,           
         BNL   *+8                                                              
         MVI   SBBTYP,0                KILL THE BOOKTYPE                        
*                                                                               
         CLI   SBBTYP,X'E0'          IF XTRA SPILL,                             
         BNE   *+8                                                              
         MVI   SBBTYP,0                KILL THE BOOKTYPE TOO                    
*                                                                               
         OI    SBBTYP,C' '           CHANGE BOOKTYPE TO UPPERCASE               
*                                                                               
         CLC   VERSION,=XL4'02000018'                                           
         BNL   BKH0008                                                          
*                                                                               
         CLI   SBBTYP,C' '                                                      
         BNH   BKH0010                                                          
*                                                                               
BKH0008  DS    0H                                                               
         CLC   BYTE,SBBTYP         SAME AS LAST BOOK TYPE?                      
         BE    BKH0010              YES - SKIP                                  
*                                                                               
         MVC   BYTE,SBBTYP                                                      
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,STABTYEL,SBBTYP,1                           
*                                                                               
BKH0010  DS    0H                                                               
         XC    WORK+100(30),WORK+100     BUILD FAKE FIELD FOR BOOK NAME         
         MVI   WORK+100,16+8                                                    
         XC    WORK(10),WORK                                                    
         MVC   WORK+1(L'SBBOOK),SBBOOK                                          
*                                                                               
         GOTO1 VUNBOOK,DMCB,(1,WORK),WORK+100,0,(C'+',=CL6' ')                  
         GOTO1 AADDDATA,DMCB,AFABLK,STABKEL,WORK+100+8,8                        
*                                                                               
         B     EXITOK                                                           
*                                                                               
BTCUTOFF EQU   X'5A07'             KILL BOOKTYPE PRIOR TO JUL90                 
         DROP  R4,R5                                                            
***********************************************************************         
* GETPROF - GET THE PROGRAM PROFILES                                            
*   INPUT:   P1  BYTE 1      PROGRAM #                                          
*                BYTE 2-4    A(PROFILE AREA) CL10                               
*                                                                               
*   OUTPUT:   PROFILES IN PROFILE AREA                                          
*                                                                               
***********************************************************************         
GETPROF  NTR1                                                                   
         ZIC   R3,0(R1)                                                         
         L     R2,0(R1)                                                         
         XC    0(10,R2),0(R2)                                                   
*                                                                               
K        USING RREPKEY,KEY         GET PARENT REP CODE                          
         XC    K.RREPKEY,K.RREPKEY                                              
         MVI   K.RREPKTYP,X'01'                                                 
         MVC   K.RREPKREP,REPALPHA                                              
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RREPKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                NO REP RECORD                                
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         L     R6,AIOREC                                                        
         LA    R6,RREPELEM-RREPREC(R6)                                          
GPROF02  CLI   0(R6),0                                                          
         BE    GETPROFX            NO PROFILE ELEMENT                           
         CLI   0(R6),X'04'                                                      
         BE    GPROF04                                                          
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     GPROF02                                                          
*                                                                               
GPROF04  DS    0H                                                               
         USING RREPPGMP,R6                                                      
         ZIC   RF,RREPPGM#         # OF PROGRAM UNITS (LOOP COUNTER)            
         LA    R6,RREPPGM1                                                      
         USING RREPPGM1,R6                                                      
GPROF10  CLM   R3,1,RREPPGM1       CORRECT PROGRAM?                             
         BE    GPROF20             YES                                          
         LA    R6,RREPPGML(R6)                                                  
         BCT   RF,GPROF10                                                       
         B     GETPROFX            NOT FOUND. USE DEFAULTS.                     
*                                                                               
GPROF20  MVC   0(10,R2),RREPPGM1   SAVE PROGRAM PROFILES UNIT                   
         DROP  R6                                                               
GETPROFX B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
*                  COMMUNICATION WITH DATA MANAGER (DIRECTORY)                  
*-------------------------------------------------------------------            
VROUTS   NTR1  BASE=*,LABEL=*                                                   
         SRL   RF,24                                                            
         B     ROUTTAB(RF)                                                      
*                                                                               
ROUTTAB  B     READ                                                             
         B     SEQ                                                              
         B     HIGH                                                             
         B     ADD                                                              
         B     WRITE                                                            
         B     GETREC                                                           
         B     PUTREC                                                           
         B     ADDREC                                                           
NUMROUTS EQU   (*-ROUTTAB)/4                                                    
*                                                                               
READ     MVC   COMMAND,=C'DMREAD'                                               
         B     DIRCTRY                                                          
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         B     DIRCTRY                                                          
ADD      MVC   COMMAND,=C'DMADD '                                               
         B     DIRCTRY                                                          
WRITE    MVC   COMMAND,=C'DMWRT '                                               
         B     DIRCTRY                                                          
DIRCTRY  CLI   UPDATE,C'Y'                                                      
         BNE   *+8                                                              
         OI    DMINBTS,X'80'                                                    
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDMGR,DMCB,(DMINBTS,COMMAND),=C'REPDIR',KEYSAVE,KEY,0            
         B     DMCHECK                                                          
         EJECT                                                                  
*-------------------------------------------------------------------            
*                  COMMUNICATION WITH DATA MANAGER (FILE)                       
*-------------------------------------------------------------------            
GETREC   MVC   COMMAND,=C'GETREC'                                               
         B     FILE                                                             
PUTREC   MVC   COMMAND,=C'PUTREC'                                               
         B     FILE                                                             
ADDREC   MVC   COMMAND,=C'ADDREC'                                               
         B     FILE                                                             
FILE     CLI   UPDATE,C'Y'                                                      
         BNE   *+8                                                              
         OI    DMINBTS,X'80'                                                    
         MVC   AIOREC,0(R1)                                                     
         LA    R0,KEY+28                                                        
         CLI   COMMAND,C'A'                                                     
         BNE   *+8                                                              
         LA    R0,KEY                                                           
         GOTO1 VDMGR,DMCB,(DMINBTS,COMMAND),=C'REPFILE',(R0),          X        
               AIOREC,(0,DMWORK)                                                
         EJECT                                                                  
*-------------------------------------------------------------------            
*                  DATA MANAGER ERRORS AND EXIT                                 
*-------------------------------------------------------------------            
DMCHECK  DS    0H                                                               
         MVI   DMINBTS,X'00'                                                    
         MVI   UPDATE,C'N'                                                      
         MVC   DMBYTE,DMCB+8                                                    
*                                                                               
         NC    DMBYTE,DMOUTBTS                                                  
         BZ    DMEXITOK                                                         
         B     DMEXITOK                                                         
*                                                                               
DMEXITL  DS    0H                  SET CC LOW & FAMSGNO                         
         CLI   *,FF                                                             
         B     *+6                                                              
DMEXITOK DS    0H                                                               
         CR    RB,RB               SET CC EQUAL                                 
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
***********************************************************************         
* XARSE- EXTENDED PARSE                                                         
*                                                                               
*   PARSE GENERIC RETURN FOR AND SEND TO PC BASED ON TABLE                      
*                                                                               
*    P1 - A(GENERIC OUTPUT STREAM)                                              
*    P2 - A(NULL TERMINATED TABLE)                                              
*                                                                               
***********************************************************************         
XARSE    NTR1  BASE=*,LABEL=*                                                   
         L     R8,0(R1)            A(OUTPUT STREAM)                             
         L     R6,4(R1)            A(TABLE)                                     
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,1,2(R8)                                                       
         BZ    EXITL               NO SUBDIVISIONS                              
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,1,2(R8)                                                       
         MHI   R2,2                                                             
         LA    R2,3(R2,R8)         INDEX TO DATA                                
*                                                                               
XARSE002 DS    0H                                                               
         LA    R5,3(R8)                                                         
         ICM   R4,1,2(R8)                                                       
         NI    MISCFLG1,FF-MF1TMPB2                                             
XARSE010 DS    0H                                                               
         LA    R3,4(R6)            CHECK FOR DATA IN TABLE                      
XARSE012 DS    0H                                                               
         CLI   0(R3),0             END OF TYPES?                                
         BE    XARSE100            YES                                          
         CLC   0(1,R3),1(R5)       TYPE MATCH?                                  
         BE    XARSE020            YES                                          
         LA    R3,XTABLQ(R3)                                                    
         B     XARSE012                                                         
*                                                                               
XARSE020 DS    0H                                                               
         CLI   1(R3),2             GENERIC ELEMENT ADD?                         
         BH    XARSE100            NO                                           
*                                                                               
         CLI   1(R3),1             SPACES CHECK?                                
         BNE   XARSE022            NO                                           
*                                                                               
         ZIC   RE,0(R5)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),SPACES                                                   
         BNH   XARSE034                                                         
*                                                                               
XARSE022 DS    0H                                                               
         TM    MISCFLG1,MF1TMPB2   HEADER SENT?                                 
         BO    XARSE030            YES                                          
*                                                                               
         ICM   RF,15,0(R6)         GET ELEMENT HEADER ADDRESS                   
         BZ    XARSE028                                                         
*                                                                               
         A     RF,BASERELO                                                      
         GOTO1 ASETELEM,DMCB,AFABLK,(RF),0                                      
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
XARSE028 DS    0H                                                               
         OI    MISCFLG1,MF1TMPB2   HEADER SENT                                  
*                                                                               
XARSE030 DS    0H                                                               
         ZIC   R0,0(R5)            GET DATA LENGTH                              
         ICM   RF,15,4(R3)         GET MAPTABLE ADDRESS                         
         A     RF,BASERELO                                                      
*                                                                               
         CLI   1(R3),2             MAPCODE ONLY?                                
         BNE   XARSE032            NO                                           
*                                                                               
         CLI   0(R2),C'Y'                                                       
         BNE   XARSE034                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,(RF),0,0                                    
         B     XARSE034                                                         
*                                                                               
XARSE032 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,(RF),(R2),(R0)                              
*                                                                               
XARSE034 DS    0H                                                               
*                                                                               
XARSE100 DS    0H                                                               
         ZIC   R0,0(R5)            BUMP DISPLACEMENT INTO DATA                  
         AR    R2,R0                                                            
*                                                                               
         LA    R5,2(R5)            NEXT ENTRY                                   
         BCT   R4,XARSE010                                                      
*                                                                               
         CLI   0(R2),0                                                          
         BNE   XARSE002                                                         
*                                                                               
XARSEX   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
********************************************************************            
* LITERALS AND CONSTANTS                                                        
********************************************************************            
FF       EQU   X'FF'                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
BREAK    NTR1  BASE=*,LABEL=*                                                   
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
RESUME   NTR1  BASE=*,LABEL=*                                                   
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
OVRFLO   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,FABLK                                                         
         USING FALINKD,R2                                                       
*                                                                               
         CLI   MAHMOUNS,X'00'      DOES FALINK WANT TO BREAK?                   
         BNE   OVRFLO10                                                         
         OI    BRKFLAG,BRKFULL                                                  
         B     OVRFLOX                                                          
*                                                                               
OVRFLO10 CLI   MAHMOUNS,X'FF'      DOES FALINK WANT TO RESUME?                  
         BNE   OVRFLOX                                                          
         NI    BRKFLAG,X'FF'-BRKFULL                                            
         OI    BRKFLAG,BRKRESM                                                  
*                                                                               
OVRFLOX  CR    RB,RB                                                            
         XIT1                                                                   
         DROP  R2                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
SEND     NTR1  BASE=*,LABEL=*                                                   
         OC    SENDROUT,SENDROUT                                                
         BZ    SENDX                                                            
*                                                                               
         MVC   ASETELEM,0(R1)      SAVE FALINK ROUTINE ADDRESSES                
         MVC   AADDDATA,4(R1)                                                   
*                                                                               
         L     RF,SENDROUT                                                      
         BASR  RE,RF                                                            
         BL    EXITL                                                            
*                                                                               
*        TM    BRKFLAG,BRKFULL                                                  
*        BO    SENDX                                                            
*                                                                               
         TM    MISCFLG1,MF1DATA    ANY DATA IN BUFFER?                          
         BZ    SENDX               NO - DON'T CLOSE                             
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,FALADNE,FALADNE,0                           
SENDX    DS    0H                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK TO SEE IF SIGN ON IS A LOCAL REP, IF SO, SET FLAG IN CTLRFLG1           
***********************************************************************         
CHKLOCAL NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,X'01'           READ REP RECORD                              
         MVC   KEY+25(2),REPALPHA                                               
         MVC   KEYSAVE,KEY                                                      
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDMGR,DMCB,(0,=C'DMREAD'),=C'REPDIR',KEYSAVE,KEY,       X        
               0,0                                                              
         CLI   DMCB+8,X'10'                                                     
         BNE   *+6                                                              
         DC    H'0'                REP RECORD NOT ON FILE?  HOW?                
*                                                                               
         XC    DMCB(24),DMCB                                                    
         GOTO1 VDMGR,DMCB,(0,=C'GETREC'),=C'REPFILE',KEY+28,AIOREC,    X        
               DMWORK,0                                                         
*                                                                               
         L     R6,AIOREC                                                        
         LA    RE,34(R6)           START OF ELEMENT                             
*                                                                               
LOCAL20  DS    0H                                                               
         CLI   0(RE),0             END OF RECORD W/O MATCH?                     
         BNE   *+6                                                              
         DC    H'0'                SHOULDN'T HAPPEN                             
*                                                                               
         CLI   0(RE),X'01'         PROGRAM PROFILE ELEMENT?                     
         BE    LOCAL40                                                          
*                                                                               
         ZIC   RF,1(RE)            GET NEXT ELEMENT                             
         AR    RE,RF                                                            
         B     LOCAL20                                                          
*                                                                               
LOCAL40  DS    0H                                                               
         TM    RREPFLGS-RREPELEM(RE),X'80'                                      
         BZ    LOCALNO             NOT A LOCAL REP                              
         MVI   LOCALREP,1                                                       
*                                                                               
LOCALYES SR    RC,RC                                                            
LOCALNO  LTR   RC,RC                                                            
         B     EXIT                                                             
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* GET USER NAME BASE ON ID NUMBER                                               
***********************************************************************         
GUSER    NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4           BUILD ID RECORD KEY                          
         USING TWAD,R3                                                          
*                                                                               
         L     R3,ATWA                                                          
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),USRID    FROM TWA                                    
         DROP  R3                                                               
*                                                                               
         GOTO1 VDMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',KEY,AIOREC                
*                                                                               
         L     R4,AIOREC                                                        
         LA    R3,CTIDATA                                                       
*                                                                               
GUSER08  CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),X'02'                                                      
         BE    GUSER15                                                          
         ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GUSER08                                                          
*                                                                               
         USING CTDSCD,R3                                                        
GUSER15  MVC   SVSIGNON(5),CTDSC   SAVE FOR FUTURE REF                          
         CLI   SVSIGNON+4,C'L'                                                  
         BNE   GUSERX                                                           
         MVI   LOCALREP,1                                                       
GUSERX   B     EXIT                                                             
         EJECT                                                                  
         DROP  R3                                                               
*&&                                                                             
***>>>>> RETRIEVE URL FROM 17 RECORD                                            
URLFIND  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    URLSTORE,URLSTORE   CLEAR URL STORAGE                            
         XC    URLSTOET,URLSTOET   CLEAR URL STORAGE                            
         XC    URLLEN,URLLEN       CLEAR LENGTH                                 
*                                                                               
K        USING RURLKEY,KEY         GET PARENT REP CODE                          
         XC    K.RURLKEY,K.RURLKEY                                              
         MVI   K.RURLKTYP,X'17'                                                 
         MVC   K.RURLKREP,REPALPHA                                              
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RURLKEY),KEYSAVE                                           
         BNE   URLF0900            NOT FOUND:  EXIT NO URL                      
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         L     R6,AIOREC                                                        
         LA    R6,RURLELEM-RURLREC(R6)                                          
         CLI   0(R6),X'01'         DESCRIPTIVE ELEMENT?                         
         BE    *+6                 YES                                          
         DC    H'0'                RECORD CORRUPT: ABORT                        
URLF0020 EQU   *                                                                
         CLI   0(R6),0             END OF RECORD?                               
         BE    URLF0900            YES - EZPOST URL NOT FOUND                   
         CLI   0(R6),RURLEZP       NO  - EZPOST URL?                            
         BE    URLF0040            YES - LOAD IT UP                             
         CLI   0(R6),RURLEXT       NO  - EXACT TIMES?                           
         BE    URLF0070            YES - LOAD IT UP                             
URLF0030 ZIC   RF,1(R6)            NO  - BUMP TO NEXT ELEMENT                   
         AR    R6,RF                                                            
         B     URLF0020            GO BACK FOR NEXT                             
URLF0040 EQU   *                                                                
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         SH    RF,=H'13'           CALCULATE URL LENGTH: MINUS ALL CTL          
         ST    RF,URLLEN           SAVE URL LENGTH                              
         BCTR  RF,0                SET REG FOR MOVE BY LENGTH                   
         EX    RF,*+8              MOVE DATE BY LENGTH                          
         B     URLF0030                                                         
         MVC   URLSTORE(0),13(R6)  MOVE URL BY LENGTH                           
*                                                                               
URLF0070 EQU   *                                                                
         ZIC   RF,1(R6)            GET ELEMENT LENGTH                           
         SH    RF,=H'13'           CALCULATE URL LENGTH: MINUS ALL CTL          
         ST    RF,URLLENET         SAVE URL LENGTH                              
         BCTR  RF,0                SET REG FOR MOVE BY LENGTH                   
         EX    RF,*+8              MOVE DATE BY LENGTH                          
         B     URLF0030                                                         
         MVC   URLSTOET(0),13(R6)  MOVE URL BY LENGTH                           
*                                                                               
URLF0900 EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***>>>>> RETRIEVE URL FROM 17 RECORD                                            
***********************************************************************         
* GET USER NAME BASE ON ID NUMBER                                               
***********************************************************************         
GUSER    NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4           BUILD ID RECORD KEY                          
         USING TWAD,R3                                                          
*                                                                               
         L     R3,ATWA                                                          
         MVI   CTIKTYP,C'I'                                                     
         MVC   CTIKID+8(2),USRID    FROM TWA                                    
         DROP  R3                                                               
*                                                                               
         GOTO1 VDMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',KEY,AIOREC                
*                                                                               
         L     R4,AIOREC                                                        
         LA    R3,CTIDATA                                                       
*                                                                               
GUSER08  CLI   0(R3),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),X'02'                                                      
         BE    GUSER15                                                          
         ZIC   RF,1(R3)                                                         
         AR    R3,RF                                                            
         B     GUSER08                                                          
*                                                                               
         USING CTDSCD,R3                                                        
GUSER15  MVC   SVSIGNON(5),CTDSC   SAVE FOR FUTURE REF                          
         CLI   SVSIGNON+4,C'L'                                                  
         BNE   GUSERX                                                           
         MVI   SVSIGNON+4,C' '                                                  
GUSERX   B     EXIT                                                             
         EJECT                                                                  
         DROP  R3                                                               
***********************************************************************         
RECEIVE  NTR1  BASE=*,LABEL=*                                                   
         MVC   AGETDATA,0(R1)      SAVE FALINK ROUTINE ADDRESSE                 
         XC    AFLDTAB,AFLDTAB                                                  
         XC    SENDROUT,SENDROUT                                                
*                                                                               
RCV000   DS    0H                                                               
         GOTO1 AGETDATA,DMCB,FABLK,FPARMS                                       
         BL    EXIT                FALINK ERROR                                 
         BH    RCV100              END OF DATA                                  
*                                                                               
         CLI   FPARMS,0            HEADER?                                      
         BNE   RCV010              NO                                           
*                                                                               
         BAS   RE,PRCHDR           PROCESS HEADER                               
         BNL   RCV020                                                           
         B     EXITL                                                            
*                                                                               
RCV010   DS    0H                  FIELD DATA                                   
         BAS   RE,PRCFLD           PROCESS FIELD DATA                           
         BNL   RCV020                                                           
         B     EXITL                                                            
*                                                                               
RCV020   DS    0H                                                               
         B     RCV000                                                           
*                                                                               
RCV100   DS    0H                                                               
         B     EXITOK                                                           
*---------------------------------------------------------------------          
* PRCHDR - PROCESS HEADER ELEMENT                                               
*---------------------------------------------------------------------          
PRCHDR   NTR1                                                                   
         L     R6,FPARMS                                                        
         USING MHELD,R6            R6=A(HEADER ENTRY)                           
         TM    MHUFLG,MHUFRQ       REQUEST HEADER?                              
         BO    *+14                YES                                          
         MVC   ERROR,=Y(54)                                                     
         BL    EXITL                                                            
*                                                                               
         LA    RF,*                                                             
         A     RF,=A(REQHDRS-(*-4))                                             
         SR    R1,R1                                                            
PHDR0010 DS    0H                                                               
         CLC   0(2,RF),=X'0000'    E.O.T.                                       
         BNE   *+14                                                             
         MVC   ERROR,=Y(841)       UNKNOWN MAPCODE                              
         B     EXITL                                                            
*                                                                               
         CLC   MHCODE,0(RF)        R1 HOLDS MAPCODE                             
         BE    PHDR0012            MATCHED                                      
         LA    RF,ROUTABLQ(RF)                                                  
         B     PHDR0010            ITERATE THIS TABLE                           
*                                                                               
PHDR0012 DS    0H                                                               
         LH    R0,=Y(SVPARMBF-T82AFFD)                                          
         ST    R0,ADDR             SAVE START DISPLACEMENT                      
*                                                                               
         A     R0,ATWA                                                          
         LH    R1,=Y(L'SVPARMBF)                                                
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         MVCL  R0,R2               CLEAR PARAMETER BUFFER                       
*                                                                               
         XC    HALF,HALF           CLEAR LAST FIELD SEEN                        
         NI    MISCFLG1,FF-MF1DATA    NO DATA IN BUFFER                         
*                                                                               
         ICM   RE,15,4(RF)                                                      
         A     RE,BASERELO                                                      
         ST    RE,SENDROUT         SAVE RESPONSE ROUTINE ADDRESS                
         ICM   RE,15,8(RF)                                                      
         A     RE,BASERELO                                                      
         ST    RE,AFLDTAB          SAVE ADDRESS OF FIELD ROUTINE TABLE          
*                                                                               
         ICM   RF,15,12(RF)        CHECK FOR EXTRA SETUP                        
         BZ    PHDR0020                                                         
*                                                                               
         A     RF,BASERELO         GOTO EXTRA SETUP                             
         BASR  RE,RF                                                            
*                                                                               
PHDR0020 DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
*.....................................................................          
* FVERHDR - PROCESS VERSION INFO INDICATOR                                      
*.....................................................................          
FVERHDR  DS    0H                                                               
         NI    MISCFLG1,FF-MF1VERCK                                             
         CLC   MHCODE,=Y(FVERHDRQ)                                              
         BNE   *+8                                                              
         OI    MISCFLG1,MF1VERCK   CHECK REQUIRED                               
         B     EXITOK                                                           
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------------------          
* PRCFLD - PROCESS FIELD ELEMENTS TO BUILD REQUEST PARAMETERS                   
*---------------------------------------------------------------------          
PRCFLD   NTR1                                                                   
         L     R6,FPARMS                                                        
         USING MDELD,R6            R6=A(FIELD ENTRY)                            
*                                                                               
         L     RF,AFLDTAB                                                       
         SR    R1,R1                                                            
         ICM   R1,3,MDCODE                                                      
         B     ITER                                                             
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* VERSION INFO - VERSION FIELD                                                  
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
FVERVER  LR    RB,RF                                                            
         CLC   FPARMS+8,=F'4'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         L     RF,FPARMS+4                                                      
         MVC   VERSION,0(RF)        COPY VERSION                                
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* INIT REQUEST - SALESPERSON FIELD                                              
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
INITSAL  LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BNZ   EPARMSEQ            NO - PARAMETER SEQUENCE ERROR                
*                                                                               
         CLC   FPARMS+8,=F'3'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPSAL,0(RF)        COPY SALESPERSON                             
         OC    VHPSAL,SPACES                                                    
         DROP  RE                                                               
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* LCON REQUEST - SALESPERSON FIELD                                              
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
LCSAL    LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BNZ   EPARMSEQ            NO - PARAMETER SEQUENCE ERROR                
*                                                                               
         CLC   FPARMS+8,=F'3'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPSAL,0(RF)        COPY SALESPERSON                             
         OC    VHPSAL,SPACES                                                    
         DROP  RE                                                               
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* LCON REQUEST - STATION FIELD                                                  
*.....................................................................          
         USING *,RB                                                             
LCSTA    LR    RB,RF                                                            
         CLC   =Y(LCSALQ),HALF     LAST FIELD THE SALESPERSON?                  
         BNE   EPARMSEQ            NO                                           
*                                                                               
         CLC   FPARMS+8,=F'5'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPSTA,0(RF)        COPY SALESPERSON                             
         OC    VHPSTA,SPACES                                                    
         DROP  RE                                                               
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* LCON REQUEST - FLIGHT START FIELD                                             
*.....................................................................          
         USING *,RB                                                             
LCFLTST  LR    RB,RF                                                            
         CLC   =Y(LCSTAQ),HALF     LAST FIELD THE STATION?                      
         BNE   EPARMSEQ            NO                                           
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPFLS,0(RF)        COPY FLIGHT START                            
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* LCON REQUEST - FLIGHT END FIELD                                               
*.....................................................................          
         USING *,RB                                                             
LCFLTEN  LR    RB,RF                                                            
         CLC   =Y(LCFLTSTQ),HALF   LAST FIELD THE FLIGHT START?                 
         BNE   EPARMSEQ            NO                                           
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPFLE,0(RF)        COPY FLIGHT END                              
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* LCON REQUEST - AGENCY FIELD                                                   
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
LCAGY    LR    RB,RF                                                            
         CLC   =Y(LCFLTENQ),HALF   LAST FIELD THE FLIGHT END?                   
         BNE   EPARMSEQ            NO - PARAMETER SEQUENCE ERROR                
*                                                                               
         CLC   FPARMS+8,=F'4'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPAGY,0(RF)        COPY AGENCY                                  
         OC    VHPAGY,SPACES                                                    
         DROP  RE                                                               
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* LCON REQUEST - AGENCY OFFICE FIELD                                            
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
LCAOF    LR    RB,RF                                                            
         CLC   =Y(LCAGYQ),HALF     LAST FIELD THE AGENCY?                       
         BNE   EPARMSEQ            NO - PARAMETER SEQUENCE ERROR                
*                                                                               
         CLC   FPARMS+8,=F'2'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPAOF,0(RF)        COPY AGENCY OFFICE                           
         OC    VHPAOF,SPACES                                                    
         DROP  RE                                                               
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* LCON REQUEST - ADVERTISER FIELD                                               
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
LCADV    LR    RB,RF                                                            
         CLC   =Y(LCFLTENQ),HALF   LAST FIELD THE FLIGHT END?                   
         BE    LCADV002            YES                                          
         CLC   =Y(LCAGYQ),HALF     LAST FIELD THE AGENCY?                       
         BE    LCADV002            YES                                          
         CLC   =Y(LCAOFQ),HALF     LAST FIELD THE AGENCY OFFICE?                
         BNZ   EPARMSEQ            NO - PARAMETER SEQUENCE ERROR                
*                                                                               
LCADV002 DS    0H                                                               
         CLC   FPARMS+8,=F'4'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPADV,0(RF)        COPY ADVERTISER                              
         OC    VHPADV,SPACES                                                    
         DROP  RE                                                               
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* RATING DOWLOAD REQUEST - RATING SERVICE FIELD                                 
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
RTRTSRV  LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BNZ   EPARMSEQ                                                         
*                                                                               
         CLC   FPARMS+8,=F'1'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RF,FPARMS+4                                                      
         MVC   RTSRVC,0(RF)        COPY RATING SERVICE                          
         OC    RTSRVC,SPACES                                                    
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* RATING DOWLOAD - BOOK FIELD(S)                                                
*.....................................................................          
         USING *,RB                                                             
RTBOOK   LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   HALF,=Y(RTBOOKQ)    FIRST BOOK FIELD?                            
         BE    RTBK0010            NO                                           
         CLC   =Y(RTRTSRVQ),HALF   LAST FIELD THE RATING SERVICE?               
         BNE   EPARMSEQ                                                         
*                                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF UPGRADES                       
         LA    RE,1(RE)            NUMBER OF UPGRADES GOES HERE                 
         ST    RE,ADDR                                                          
*                                                                               
RTBK0010 DS    0H                                                               
         MVC   HALF,=Y(RTBOOKQ)                                                 
         XC    WORK,WORK           CONSTRUCT FAKE FIELD                         
         MVI   WORK,16+8           16 BYTE FIELD                                
         L     RF,FPARMS+4         ADDRESS OF DATA                              
         L     RE,FPARMS+8         LENGTH OF INPUT                              
         STC   RE,WORK+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),0(RF)                                                  
*                                                                               
         XC    FULL,FULL                                                        
         GOTO1 VBOOKVAL,DMCB,(RTSRVC,WORK),(1,WORK+16+8),              +        
               (C'B',VSCANNER),FULL,(C'C',ACOMFACS)                             
*                                                                               
         CLI   4(R1),0             GOOD BOOK?                                   
         BE    EBADBOOK            NO                                           
*                                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         MVC   0(3,RE),WORK+16+8   STORE BOOKVAL BYTES                          
*                                                                               
         MVI   3(RE),C'I'          SET DEMO FILE(INV IS DEFAULT)                
         CLC   MDCODE,=Y(RTTPBKQ)                                               
         BNE   *+8                                                              
         MVI   3(RE),C'T'          TIME PERIOD                                  
         CLC   MDCODE,=Y(RTT4BKQ)                                               
         BNE   *+8                                                              
         MVI   3(RE),C'4'          4WEEK                                        
         CLC   MDCODE,=Y(RTPVBKQ)                                               
         BNE   *+8                                                              
         MVI   3(RE),C'P'          PAV                                          
*                                                                               
* SET PRECISION TABLE INDICATOR                                                 
*                                                                               
         MVI   PRCTABLE,C'T'       DEFAULT USE TIME PERIOD                      
         CLI   3(RE),C'P'          PAV?                                         
         BNE   *+8                                                              
         MVI   PRCTABLE,C'P'                                                    
*                                                                               
         MVC   4(1,RE),FULL        SET BOOK TYPE                                
         MVI   5(RE),0             MAKE IT NEW                                  
*                                                                               
         LA    RE,6(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWANOGO-100)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* NEW DATA FOR INVENTORY REQUEST - UPGRADE FIELD(S)                             
*.....................................................................          
         USING *,RB                                                             
RTUPG    LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   HALF,=Y(RTUPGQ)     FIRST UPGRADE FIELD?                         
         BE    RTUPG010            NO                                           
         CLC   =Y(RTBOOKQ),HALF    LAST FIELD A BOOK?                           
         BE    RTUPG004            YES                                          
         CLC   =Y(RTRTSRVQ),HALF   LAST FIELD THE RATING SERVICE?               
         BNE   EPARMSEQ                                                         
*                                                                               
         MVI   0(RE),0                 - SET NO BOOKS                           
         LA    RE,1(RE)                                                         
RTUPG004 DS    0H                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF UPGRADES                       
         LA    RE,1(RE)            NUMBER OF UPGRADES GOES HERE                 
         ST    RE,ADDR                                                          
*                                                                               
RTUPG010 DS    0H                                                               
         MVC   HALF,=Y(RTUPGQ)                                                  
         XC    WORK,WORK           CONSTRUCT FAKE FIELD                         
         MVI   WORK,79+8           79 BYTE FIELD                                
         L     RF,FPARMS+4         ADDRESS OF DATA                              
         L     RE,FPARMS+8         LENGTH OF INPUT                              
         MVC   WORK+8(4),=C'UPT='                                               
         LA    R0,4(RE)                                                         
         STC   R0,WORK+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8+4(0),0(RF)                                                
*                                                                               
         MVC   WORK+100(4),VBOOKVAL                                             
         MVC   WORK+104(4),ACOMFACS                                             
         MVC   WORK+108(4),VUPVAL                                               
         GOTO1 =V(REPRPUPV),DMCB,WORK,WORK+120,WORK+100,RR=Y                    
         BE    *+14                                                             
         MVC   ERROR,WORK+120                                                   
         B     EXITL                                                            
*                                                                               
         OC    WORK+120+15(3),WORK+120+15                                       
         BZ    EINVUPG             REQUIRES A SHARE BOOK                        
*                                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
*                                                                               
UE       USING RAVLNEL,WORK+120                                                 
         TM    UE.RAVLNTYP,X'20'   INVENTORY UPGRADE?                           
         BNZ   EINVUPG             YES                                          
*                                                                               
         MVI   3(RE),C'T'          TIME PERIOD                                  
         CLC   MDCODE,=Y(RTT4UPGQ)                                              
         BNE   *+8                                                              
         MVI   3(RE),C'4'          4WEEK                                        
         CLC   MDCODE,=Y(RTPVUPGQ)                                              
         BNE   *+8                                                              
         MVI   3(RE),C'P'          PAV                                          
*                                                                               
         MVC   UE.RAVLNBT,WORK+120+14      SPECIAL BOOK TYPE                    
         DROP  UE                                                               
*                                                                               
* SET PRECISION TABLE INDICATOR                                                 
*                                                                               
         MVI   PRCTABLE,C'T'       DEFAULT USE TIME PERIOD                      
         CLI   3(RE),C'P'          PAV?                                         
         BNE   *+8                                                              
         MVI   PRCTABLE,C'P'                                                    
*                                                                               
         MVC   0(3,RE),WORK+120+15         1ST BASE BOOKS                       
         MVC   4(1,RE),WORK+120+14         SPECIAL BOOK TYPE                    
         MVC   5(2,RE),WORK+120+18+1       2ND BASE BOOK(YM ONLY)               
         MVC   7(2,RE),WORK+120+21+1       3RD                                  
         MVC   9(2,RE),WORK+120+24+1       4TH                                  
         MVC   11(14,RE),WORK+120          UPGRADE ELEMENT                      
         MVI   11+14(RE),0                 MAKE IT NEW                          
         LA    RE,11+14+1(RE)                                                   
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWANOGO-100)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* RATING DOWLOAD - DEMO FIELD                                                   
*.....................................................................          
         USING *,RB                                                             
RTDEMO   LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   HALF,=Y(RTDEMOQ)    FIRST DEMO FIELD?                            
         BE    RTDEM010            NO                                           
*                                                                               
         CLC   =Y(RTUPGQ),HALF     LAST FIELD AN UPGRADE?                       
         BE    RTDEM002            YES                                          
         CLC   =Y(RTBOOKQ),HALF    LAST FIELD A BOOK?                           
         BE    RTDEM001            YES                                          
         CLC   =Y(RTRTSRVQ),HALF   LAST FIELD THE RATING SERVICE?               
         BNE   EPARMSEQ                                                         
*                                                                               
         BAS   RE,SETDUMMY         NO BOOK SPECIFIED, SET DUMMY BOOK            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
*                                                                               
RTDEM001 DS    0H                                                               
         MVI   0(RE),0             YES - SET NO UPGRADES                        
         LA    RE,1(RE)                                                         
RTDEM002 DS    0H                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF DEMOS                          
         LA    RE,1(RE)            NUMBER OF DEMOS GOES HERE                    
         ST    RE,ADDR                                                          
*                                                                               
RTDEM010 DS    0H                                                               
         MVC   HALF,=Y(RTDEMOQ)                                                 
         XC    WORK,WORK           CONSTRUCT FAKE FIELD                         
         MVI   WORK,16+8             16 BYTE FIELD                              
         L     RF,FPARMS+4         ADDRESS OF DATA                              
         L     RE,FPARMS+8         LENGTH OF INPUT                              
         STC   RE,WORK+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),0(RF)                                                  
*                                                                               
         L     RE,AIO1                                                          
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'PAV'                                                   
         DROP  RE                                                               
*                                                                               
         GOTO1 VDEMOVAL,DMCB,(1,WORK),(1,WORK+16+8),(0,AIO1),0                  
*                                                                               
         CLI   4(R1),0             GOOD DEMO?                                   
         BE    EBADDEMO            NO                                           
*                                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         MVC   0(3,RE),WORK+16+8   STORE DEMOVAL BYTES                          
         MVI   3(RE),0             MAKE IT NEW                                  
         LA    RE,4(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWANOGO-100)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
*.....................................................................          
* SET A DUMMY BOOK : FEB08                                                      
* SINCE USER DID NOT PROVIDE ONE                                                
*.....................................................................          
SETDUMMY NTR1  BASE=*,LABEL=*                                                   
         L     RE,ADDR                                                          
         ST    RE,ADDR2            SAVE START OF UPGRADES                       
         LA    RE,1(RE)            NUMBER OF UPGRADES GOES HERE                 
         ST    RE,ADDR                                                          
*                                                                               
         MVC   HALF,=Y(RTBOOKQ)                                                 
         XC    WORK,WORK           CONSTRUCT FAKE FIELD                         
         MVI   WORK,16+8           16 BYTE FIELD                                
         MVI   WORK+5,5                                                         
         MVC   WORK+8(5),=C'FEB08'                                              
*                                                                               
         XC    FULL,FULL                                                        
         GOTO1 VBOOKVAL,DMCB,(RTSRVC,WORK),(1,WORK+16+8),              +        
               (C'B',VSCANNER),FULL,(C'C',ACOMFACS)                             
*                                                                               
         CLI   4(R1),0             GOOD BOOK?                                   
         BE    EBADBOOK            NO                                           
*                                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         MVC   0(3,RE),WORK+16+8   STORE BOOKVAL BYTES                          
*                                                                               
         MVI   3(RE),C'T'          TIME PERIOD                                  
*                                                                               
* SET PRECISION TABLE INDICATOR                                                 
*                                                                               
         MVI   PRCTABLE,C'T'       DEFAULT USE TIME PERIOD                      
*                                                                               
         MVC   4(1,RE),FULL        SET BOOK TYPE                                
         MVI   5(RE),0             MAKE IT NEW                                  
*                                                                               
         LA    RE,6(RE)            BUMP TO NEXT ENTRY                           
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWANOGO-100)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
*.....................................................................          
* RATING DOWNLOAD - STATION FIELD                                               
*.....................................................................          
         USING *,RB                                                             
RTSTA    LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         CLC   =Y(RTETIMQ),HALF    LAST FIELD AN END TIME?                      
         BE    RTSTA012            YES                                          
         CLC   =Y(RTDATEQ),HALF    LAST FIELD A EFFECTIVE DATE?                 
         BE    RTSTA012            YES                                          
         CLC   =Y(RTSTIMQ),HALF    LAST FIELD A START TIME?                     
         BE    RTSTA010            YES                                          
         CLC   =Y(RTDEMOQ),HALF    LAST FIELD A DEMO?                           
         BNE   EPARMSEQ                                                         
*                                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR             STATION GOES HERE                            
         B     RTSTA020                                                         
*                                                                               
RTSTA010 DS    0H                  SET MISSING END TIME TO ZERO                 
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         XC    0(2,RE),0(RE)                                                    
         MVI   2(RE),X'FF'         ADD END OF PROGRAM MARKER                    
         LA    RE,3(RE)                                                         
*                                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWANOGO-100)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
RTSTA012 DS    0H                                                               
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         MVI   0(RE),0             SET END OF PREVIOUS STATION                  
         LA    RE,1(RE)                                                         
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
RTSTA020 DS    0H                                                               
         CLC   FPARMS+8,=F'5'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   0(5,RE),0(RF)       COPY STATION CALL LETTERS                    
         OC    0(5,RE),SPACES                                                   
         LA    RF,5(RE)            BUMP TO NEXT ENTRY                           
         LR    R1,RE                                                            
         S     RF,ATWA             STORE NEW ENTRY                              
         ST    RF,ADDR                                                          
*                                                                               
         CHI   RF,(TWANOGO-100)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* RATING DOWLOAD - DAY FIELD                                                    
*.....................................................................          
         USING *,RB                                                             
RTDAY    LR    RB,RF                                                            
         CLC   =Y(RTXTIMQ),HALF    LAST FIELD AN ORBIT END TIME?                
         BE    RTDAY012            YES                                          
         CLC   =Y(RTETIMQ),HALF    LAST FIELD AN END TIME?                      
         BE    RTDAY012            YES                                          
         CLC   =Y(RTDATEQ),HALF    LAST FIELD A EFFECTIVE DATE?                 
         BE    RTDAY012            YES                                          
         CLC   =Y(RTSTIMQ),HALF    LAST FIELD A START TIME?                     
         BE    RTDAY010            YES                                          
         CLC   =Y(RTSTAQ),HALF     LAST FIELD A STATION?                        
         BE    RTDAY012            YES                                          
         B     EPARMSEQ                                                         
*                                                                               
RTDAY010 DS    0H                  SET MISSING END TIME TO ZERO                 
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         XC    0(2,RE),0(RE)                                                    
         MVI   2(RE),X'FF'         ADD END OF PROGRAM MARKER                    
         LA    RE,3(RE)                                                         
*                                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWANOGO-100)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
RTDAY012 DS    0H                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   0(1,RE),0(RF)       COPY DAY                                     
         LA    RE,1(RE)                                                         
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWANOGO-100)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* RATING DOWLOAD - START TIME FIELD                                             
*.....................................................................          
         USING *,RB                                                             
RTSTIM   LR    RB,RF                                                            
         CLC   =Y(RTDAYQ),HALF     LAST FIELD A STATION?                        
         BNE   EPARMSEQ                                                         
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   0(2,RE),0(RF)       COPY START TIME                              
         LA    RE,2(RE)                                                         
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWANOGO-100)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* RATING DOWLOAD - END TIME FIELD                                               
*.....................................................................          
         USING *,RB                                                             
RTETIM   LR    RB,RF                                                            
         CLC   =Y(RTSTIMQ),HALF     LAST FIELD THE START TIME?                  
         BNE   EPARMSEQ                                                         
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         L     RF,FPARMS+4                                                      
         MVC   0(2,RE),0(RF)       COPY END TIME                                
         LA    RE,2(RE)                                                         
*                                                                               
         CLC   HALF,=Y(RTETIMQ)    LAST END TIME?                               
         BNE   *+12                                                             
         MVI   0(RE),X'FF'         YES ADD MARKER                               
         LA    RE,1(RE)                                                         
*                                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWANOGO-100)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
*  RATING DOWLOAD - EFFECTIVE DATE FIELD (REPLACES END TIME)                    
*.....................................................................          
         USING *,RB                                                             
RTDATE   LR    RB,RF                                                            
         CLC   =Y(RTSTIMQ),HALF     LAST FIELD THE START TIME?                  
         BNE   EPARMSEQ                                                         
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     R2,ADDR                                                          
         A     R2,ATWA                                                          
         L     R3,FPARMS+4                                                      
         MVI   0(R2),X'FF'         SET END TIME IS DATE                         
         GOTO1 VDATCON,DMCB,(8,0(R3)),(2,1(R2))                                 
         MVI   3(R2),X'FF'         ADD END OF PROGRAM MARKER                    
         LA    R2,4(R2)                                                         
*                                                                               
         S     R2,ATWA                                                          
         ST    R2,ADDR                                                          
*                                                                               
         CHI   R2,(TWANOGO-100)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
*  RATING DOWLOAD - END OF REQUEST MARKER                                       
*.....................................................................          
         USING *,RB                                                             
RTEMRK   LR    RB,RF                                                            
         CLC   =Y(RTSTIMQ),HALF    LAST FIELD A START TIME?                     
         BNE   RTEMRKX             NO                                           
*                                                                               
         L     RE,ADDR             SET MISSING END TIME TO ZERO                 
         A     RE,ATWA                                                          
         XC    0(2,RE),0(RE)                                                    
         MVI   2(RE),X'FF'         ADD END OF PROGRAM MARKER                    
         LA    RE,3(RE)                                                         
*                                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWANOGO-100)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
RTEMRKX  DS    0H                                                               
         MVC   HALF,MDCODE                                                      
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* RATING DOWLOAD REQUEST - OVERNIGHTS REQUESTED FIELD                           
*        FIELD SHOULD BE SENT AT END, BEFORE END OF REQUEST MARKER              
*        THERE IS NO REQUIREMENT THAT ANY OTHER ELEMENTS BE                     
*        ASSOCIATED WITH THIS ELEMENT                                           
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
RTOVRNT  LR    RB,RF                                                            
*                                                                               
****     OC    HALF,HALF           NO OTHER FIELD ASSOCIATIONS !!               
****     BNZ   EPARMSEQ                                                         
*                                                                               
         CLC   FPARMS+8,=F'1'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RF,FPARMS+4                                                      
         MVC   OVRNTFLG,0(RF)      COPY OVERNIGHT FLAG                          
         OC    OVRNTFLG,SPACES                                                  
         MVI   PRCTABLE,C'T'       DEFAULT TIME PERIOD PRECISION TABLE          
*                                                                               
         B     EXITOK                                                           
**********************************************************************          
* RATING DOWLOAD REQUEST - RHOMES DEMO OVERRIDE FIELD                           
*        FIELD SHOULD BE SENT AT END, BEFORE END OF REQUEST MARKER              
*        THERE IS NO REQUIREMENT THAT ANY OTHER ELEMENTS BE                     
*        ASSOCIATED WITH THIS ELEMENT                                           
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
RTOVHMS  LR    RB,RF                                                            
*                                                                               
****     OC    HALF,HALF           NO OTHER FIELD ASSOCIATIONS !!               
****     BNZ   EPARMSEQ                                                         
*                                                                               
         CLC   FPARMS+8,=F'1'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RF,FPARMS+4                                                      
         MVC   OVHMSFLG,0(RF)      COPY DEMO OVERRIDE FLAG                      
         OC    OVHMSFLG,SPACES                                                  
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* RATING DOWLOAD REQUEST - DECIMAL PRECISION                                    
*        FIELD SHOULD BE SENT AT END, BEFORE END OF REQUEST MARKER              
*        THERE IS NO REQUIREMENT THAT ANY OTHER ELEMENTS BE                     
*        ASSOCIATED WITH THIS ELEMENT                                           
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
RTDECPC  LR    RB,RF                                                            
*                                                                               
****     OC    HALF,HALF           NO OTHER FIELD ASSOCIATIONS !!               
****     BNZ   EPARMSEQ                                                         
*                                                                               
         CLC   FPARMS+8,=F'1'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RF,FPARMS+4                                                      
         MVC   RTGPRCSN,0(RF)      COPY PRECISION                               
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* RATING DOWLOAD REQUEST - BOOKTYPE FOR OVERNIGHTS                              
*        FIELD SHOULD BE SENT AT END, BEFORE END OF REQUEST MARKER              
*        THERE IS NO REQUIREMENT THAT ANY OTHER ELEMENTS BE                     
*        ASSOCIATED WITH THIS ELEMENT                                           
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
RTOTPBK  LR    RB,RF                                                            
*                                                                               
****     OC    HALF,HALF           NO OTHER FIELD ASSOCIATIONS !!               
****     BNZ   EPARMSEQ                                                         
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RF,FPARMS+4                                                      
         L     RE,FPARMS+8         LENGTH OF INPUT                              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   OVRNTBKT(0),0(RF)   COPY OVERNIGHT BOOKTYPE                      
         OC    OVRNTBKT,SPACES                                                  
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* VALIDATE HEADER REQUEST - STATION FIELD                                       
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
VHSTA    LR    RB,RF                                                            
         CLC   FPARMS+8,=F'5'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPSTA,0(RF)        COPY STATION CALL LETTERS                    
         OC    VHPSTA,SPACES                                                    
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* VALIDATE HEADER REQUEST - ADVERTISER FIELD                                    
*.....................................................................          
         USING *,RB                                                             
VHADV    LR    RB,RF                                                            
         CLC   FPARMS+8,=F'4'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPADV,0(RF)        COPY ADVERTISER                              
         OC    VHPADV,SPACES                                                    
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* VALIDATE HEADER REQUEST - PRODUCT FIELD                                       
*.....................................................................          
         USING *,RB                                                             
VHPRD    LR    RB,RF                                                            
         CLC   FPARMS+8,=F'3'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPPRD,0(RF)        COPY PRODUCT                                 
         OC    VHPPRD,SPACES                                                    
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* VALIDATE HEADER REQUEST - AGENCY FIELD                                        
*.....................................................................          
         USING *,RB                                                             
VHAGY    LR    RB,RF                                                            
         CLC   FPARMS+8,=F'4'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPAGY,0(RF)        COPY AGENCY                                  
         OC    VHPAGY,SPACES                                                    
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* VALIDATE HEADER REQUEST - AGENCY OFFICE FIELD                                 
*.....................................................................          
         USING *,RB                                                             
VHAOF    LR    RB,RF                                                            
         CLC   =Y(VHAGYQ),HALF     LAST FIELD THE AGENCY?                       
         BNE   EPARMSEQ            NO                                           
*                                                                               
         CLC   FPARMS+8,=F'2'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPAOF,0(RF)        COPY AGENCY OFFICE                           
         OC    VHPAOF,SPACES                                                    
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* VALIDATE HEADER REQUEST - FLIGHT START FIELD                                  
*.....................................................................          
         USING *,RB                                                             
VHFLS    LR    RB,RF                                                            
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPFLS,0(RF)        COPY FLIGHT START                            
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* VALIDATE HEADER REQUEST - FLIGHT END FIELD                                    
*.....................................................................          
         USING *,RB                                                             
VHFLE    LR    RB,RF                                                            
         CLC   =Y(VHFLSQ),HALF     LAST FIELD THE FLIGHT END?                   
         BNE   EPARMSEQ            NO                                           
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPFLE,0(RF)        COPY FLIGHT END                              
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* VALIDATE HEADER REQUEST - CONTRACT TYPE FIELD (OPTIONAL)                      
*.....................................................................          
         USING *,RB                                                             
VHCTYP   LR    RB,RF                                                            
         CLC   FPARMS+8,=F'1'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPCTY,0(RF)        COPY CONTRACT TYPE                           
         OC    VHPCTY,SPACES                                                    
         B     EXITOK                                                           
         DROP  RE                                                               
*.....................................................................          
* VALIDATE HEADER REQUEST - DEV CONTRACT TYPE FIELD (OPTIONAL)                  
*.....................................................................          
         USING *,RB                                                             
VHDCT    LR    RB,RF                                                            
         CLC   FPARMS+8,=F'2'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPDCT,0(RF)        COPY DEV CONTRACT TYPE                       
         DROP  RE                                                               
         B     EXITOK                                                           
*.....................................................................          
* VALIDATE HEADER REQUEST - DEV CONTRACT TYPE FIELD (OPTIONAL)                  
*.....................................................................          
         USING *,RB                                                             
VHDSP    LR    RB,RF                                                            
         CLC   FPARMS+8,=F'3'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPDSP,0(RF)        COPY DEV SALEPERSON                          
         DROP  RE                                                               
         B     EXITOK                                                           
         SPACE 3                                                                
*.....................................................................          
* VALIDATE HEADER REQUEST - SALESPERSON FIELD (OPTIONAL)                        
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
VHSAL    LR    RB,RF                                                            
         CLC   FPARMS+8,=F'3'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
         MVC   VHPSAL,0(RF)        COPY SALESPERSON                             
         OC    VHPSAL,SPACES                                                    
         DROP  RE                                                               
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* VALIDATE BOOKS/UPGRADES/DEMOS REQUEST - RATING SERVICE FIELD                  
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
VBRTSRV  LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BNZ   EPARMSEQ            NO - PARAMETER SEQUENCE ERROR                
*                                                                               
         CLC   FPARMS+8,=F'1'      LENGTH OK?                                   
         BNE   EINVLEN                                                          
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RF,FPARMS+4                                                      
         MVC   RTSRVC,0(RF)        COPY RATING SERVICE                          
         OC    RTSRVC,SPACES                                                    
*                                                                               
         L     R1,ADDR             SET POSITION FOR VALIDATED DATA              
         AHI   R1,VHPARMLQ                                                      
         ST    R1,ADDR2                                                         
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* VALIDATE BOOKS/UPGRADES/DEMOS REQUEST - BOOK FIELD(S)                         
*.....................................................................          
         USING *,RB                                                             
VBBOOK   LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BZ    EPARMSEQ            YES - PARAMETER SEQUENCE ERROR               
*                                                                               
         OI    MISCFLG1,MF1BKS                                                  
*                                                                               
         MVC   HALF,=Y(VBBOOKQ)                                                 
         XC    WORK,WORK           CONSTRUCT FAKE FIELD                         
         MVI   WORK,16+8           16 BYTE FIELD                                
         L     RF,FPARMS+4         ADDRESS OF DATA                              
         L     RE,FPARMS+8         LENGTH OF INPUT                              
         STC   RE,WORK+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),0(RF)                                                  
*                                                                               
         XC    FULL,FULL                                                        
         GOTO1 VBOOKVAL,DMCB,(RTSRVC,WORK),(1,WORK+16+8),              +        
               (C'B',VSCANNER),FULL,(C'C',ACOMFACS)                             
*                                                                               
         CLI   4(R1),0             GOOD BOOK?                                   
         BE    EBADBOOK            NO                                           
*                                                                               
         XC    WORK+60(20),WORK+60                                              
         MVC   WORK+60(2),=X'0B07'      PUT OUT BOOK TYPE                       
         MVC   WORK+60+2(1),FULL                                                
         GOTOX VUNBOOK,DMCB,(1,WORK+16+8),WORK,(C'L',WORK+60),         +        
               (C'+',=CL6' ')                                                   
*                                                                               
         ZIC   RE,WORK                                                          
         BCTR  RE,0                                                             
         LA    RE,WORK(RE)                                                      
         CLI   0(RE),C' '          REMOVE SPACES                                
         BH    *+8                                                              
         BCT   RE,*-8                                                           
*                                                                               
         CLI   0(RE),C')'                                                       
         BNE   VBBK0020                                                         
         BCTR  RE,0                                                             
VBBK0005 CLI   0(RE),C' '                                                       
         BE    VBBK0006                                                         
         CLI   0(RE),0                                                          
         BNE   VBBK0008                                                         
VBBK0006 BCT   RE,VBBK0005                                                      
VBBK0008 CLI   0(RE),C'('                                                       
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     VBBK0020                                                         
*                                                                               
* SUPPORT FOR 2-CHAR BOOKTYPE                                                   
*                                                                               
         LR    R4,RE                                                            
         MVC   BYTE,0(R4)                                                       
         L     R3,ACOMFACS                                                      
         SPACE                                                                  
         L     RF,CDEMTABS-COMFACSD(,R3)                                        
         GOTO1 (RF),DMCB,SPBOOKTB                                               
         ICM   RF,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
VBBK0010 DS    0H                                                               
         CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   BYTE,SPBKTYPN                                                    
         BE    *+10                                                             
         AR    RF,R0                                                            
         B     VBBK0010                                                         
*                                                                               
         MVC   0(2,R4),SPBKTYPA    BOOK TYPE                                    
         LA    R4,1(R4)                                                         
         LR    RE,R4                                                            
         CLI   SPBKTYPA+1,0                                                     
         BE    VBBK0015                                                         
         CLI   SPBKTYPA+1,C' '                                                  
         BE    VBBK0015                                                         
         DROP  RF                                                               
*                                                                               
         LA    RE,1(RE)                                                         
VBBK0015 MVI   0(RE),C')'                                                       
*                                                                               
VBBK0020 DS    0H                                                               
         L     R1,ADDR2            COPY UNBOOKED NAME                           
         A     R1,ATWA                                                          
         MVC   0(2,R1),HALF        SAY ITS A BOOK                               
         TM    WORK+16+8,X'2E'     CHECK FOR E/P/S/T BOOKS                      
         BZ    *+10                                                             
         MVC   0(2,R1),=Y(VBINBKQ) SAY ITS AN INVENTORY BOOK                    
*                                                                               
         LA    RF,WORK+8                                                        
         SR    RE,RF                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R1),WORK+8      MOVE THE TEXT                                
         LA    RE,1(RE)                                                         
         STC   RE,2(R1)            SAVE ITS LENGTH                              
         LA    R1,3(RE,R1)         BUMP TO NEXT SLOT                            
         S     R1,ATWA                                                          
         ST    R1,ADDR2                                                         
*                                                                               
         CHI   R1,(TWANOGO-100)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* VALIDATE BOOKS/UPGRADES/DEMOS REQUEST - UPGRADE FIELD                         
*.....................................................................          
         USING *,RB                                                             
VBUPGRD  LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BZ    EPARMSEQ            YES - PARAMETER SEQUENCE ERROR               
*                                                                               
         OI    MISCFLG1,MF1BKS                                                  
*                                                                               
         MVC   HALF,=Y(VBUPGRDQ)                                                
*                                                                               
         XC    WORK,WORK           CONSTRUCT FAKE FIELD                         
         MVI   WORK,79+8           79 BYTE FIELD                                
         L     RF,FPARMS+4         ADDRESS OF DATA                              
         L     RE,FPARMS+8         LENGTH OF INPUT                              
         MVC   WORK+8(4),=C'UPT='                                               
         LA    R0,4(RE)                                                         
         STC   R0,WORK+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8+4(0),0(RF)                                                
*                                                                               
         MVC   WORK+100(4),VBOOKVAL                                             
         MVC   WORK+104(4),ACOMFACS                                             
         MVC   WORK+108(4),VUPVAL                                               
         GOTO1 =V(REPRPUPV),DMCB,WORK,WORK+120,WORK+100,RR=Y                    
         BE    *+14                                                             
         MVC   ERROR,WORK+120                                                   
         B     EXITL                                                            
*                                                                               
         OC    WORK+120+15(3),WORK+120+15                                       
         BZ    EINVUPG             REQUIRES A SHARE BOOK                        
*                                                                               
         L     R1,ADDR2                                                         
         A     R1,ATWA                                                          
*                                                                               
UE       USING RAVLNEL,WORK+120                                                 
         TM    UE.RAVLNTYP,X'20'   INVENTORY UPGRADE?                           
         BNZ   EINVUPG             YES                                          
*                                                                               
         MVC   0(2,R1),=Y(VBNIUPGQ)   SAY ITS A NON INVENTORY UPGRADE           
         DROP  UE                                                               
*                                                                               
VBUPG020 DS    0H                                                               
         L     RF,FPARMS+4         ADDRESS OF DATA                              
         L     RE,FPARMS+8         LENGTH OF INPUT                              
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R1),0(RF)                                                    
         LA    RE,1(RE)                                                         
         STC   RE,2(R1)            SAVE ITS LENGTH                              
         LA    R1,3(RE,R1)         BUMP TO NEXT SLOT                            
         S     R1,ATWA                                                          
         ST    R1,ADDR2                                                         
*                                                                               
         CHI   R1,(TWANOGO-100)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* VALIDATE BOOKS/UPGRADES/DEMOS REQUEST - DEMO FIELD                            
*.....................................................................          
         USING *,RB                                                             
VBDEMO   LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BZ    EPARMSEQ            YES - PARAMETER SEQUENCE ERROR               
*                                                                               
         OI    MISCFLG1,MF1BKS                                                  
*                                                                               
         MVC   HALF,MDCODE                                                      
         XC    WORK,WORK           CONSTRUCT FAKE FIELD                         
         MVI   WORK,16+8             16 BYTE FIELD                              
         L     RF,FPARMS+4         ADDRESS OF DATA                              
         L     RE,FPARMS+8         LENGTH OF INPUT                              
         STC   RE,WORK+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),0(RF)                                                  
*                                                                               
         L     RE,AIO1                                                          
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'PAV'                                                   
         DROP  RE                                                               
*                                                                               
         GOTO1 VDEMOVAL,DMCB,(1,WORK),(1,WORK+16+8),(0,AIO1),0                  
*                                                                               
         CLI   4(R1),0             GOOD DEMO?                                   
         BE    EBADDEMO            NO                                           
*                                                                               
         L     RE,AIO1                                                          
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'INV'                                                   
         DROP  RE                                                               
*                                                                               
         XC    WORK+60(50),WORK+60                                              
         MVC   WORK+60(3),WORK+16+8                                             
         CLI   WORK+60+1,C'T'      FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   WORK+60+1,C'I'                                                   
*                                                                               
         GOTO1 VDEMOCON,DMCB,(1,WORK+60),(9,WORK),(0,AIO1)                      
*                                                                               
         ZIC   RE,0(R1)                                                         
         BCTR  RE,0                                                             
*                                                                               
         L     R1,ADDR2            COPY UNDEMOED NAME                           
         A     R1,ATWA                                                          
         MVC   0(2,R1),HALF        SAY ITS A DEMO                               
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R1),WORK        MOVE THE TEXT                                
         LA    RE,1(RE)                                                         
         STC   RE,2(R1)            SAVE ITS LENGTH                              
         LA    R1,3(RE,R1)         BUMP TO NEXT SLOT                            
         S     R1,ATWA                                                          
         ST    R1,ADDR2                                                         
*                                                                               
         CHI   R1,(TWANOGO-100)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* VALIDATE BOOKS/UPGRADES/DEMOS REQUEST - UPGRADE NAME                          
*.....................................................................          
         USING *,RB                                                             
VBUPGNM  LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BZ    EPARMSEQ            YES - PARAMETER SEQUENCE ERROR               
*                                                                               
         OI    MISCFLG1,MF1BKS                                                  
*                                                                               
         MVC   HALF,=Y(VBUPGNMQ)                                                
         XC    WORK,WORK           CONSTRUCT FAKE FIELD                         
         MVI   WORK,16+8           16 BYTE FIELD                                
         L     RF,FPARMS+4         ADDRESS OF DATA                              
         L     RE,FPARMS+8         LENGTH OF INPUT                              
         STC   RE,WORK+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),0(RF)                                                  
*                                                                               
         XC    FULL,FULL                                                        
         GOTO1 VBOOKVAL,DMCB,(RTSRVC,WORK),(1,WORK+16+8),              +        
               (C'B',VSCANNER),FULL,(C'C',ACOMFACS)                             
*                                                                               
         CLI   4(R1),0             GOOD BOOK?                                   
         BNE   EBADUPNM            YES - BAD UPGRADE NAME                       
*                                                                               
         L     R1,ADDR2            COPY UNBOOKED NAME                           
         A     R1,ATWA                                                          
         MVC   0(2,R1),=Y(VBUPGNMQ) SAY ITS AN UPGRADE NAME                     
*                                                                               
         ZIC   RE,WORK+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R1),WORK+8      MOVE THE TEXT                                
         LA    RE,1(RE)                                                         
         STC   RE,2(R1)            SAVE ITS LENGTH                              
         LA    R1,3(RE,R1)         BUMP TO NEXT SLOT                            
         S     R1,ATWA                                                          
         ST    R1,ADDR2                                                         
*                                                                               
         CHI   R1,(TWANOGO-100)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD CONTRACT HEADER FIELDS REQUEST - CONTRACT NUMBER FIELD               
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
DCONCON  LR    RB,RF                                                            
         L     R9,ADDR                                                          
         A     R9,ATWA                                                          
         OC    HALF,HALF           FIRST CONTRACT?                              
         BNZ   DCCON010            NO                                           
*                                                                               
         S     R9,ATWA                                                          
         ST    R9,ADDR2            SAVE START OF CONTRACT                       
         LA    R9,1(R9)            NUMBER OF CONTRACTS GOES HERE                
         ST    R9,ADDR                                                          
*                                                                               
DCCON010 DS    0H                                                               
         MVC   HALF,MDCODE                                                      
         L     RF,FPARMS+4         A(DATA)                                      
         L     R1,FPARMS+8         LENGTH OF DATA                               
         L     R9,ADDR                                                          
         A     R9,ATWA                                                          
         LR    R8,R9               SAVE OFF ORIGINAL ADDRESS                    
*                                                                               
         USING VHSPARMD,R9                                                      
         XC    WORK,WORK           SET UP FAKE HEADER FOR SCANNER               
         STC   R1,WORK+5                                                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),0(RF)                                                  
*                                                                               
         GOTO1 VSCANNER,DMCB,WORK,WORK2,0                                       
         CLI   DMCB+4,0                                                         
         BNE   *+14                                                             
         MVC   ERROR,=Y(82)                                                     
         B     EXITL                                                            
*                                                                               
* PARSE AND SAVE CONTRACT NUMBER                                                
*                                                                               
         ZIC   R3,DMCB+4           SCANNER LINE COUNTER                         
         LA    R4,WORK2            SCANNER BLOCK                                
         CLI   0(R4),0                                                          
         BE    DCCON050                                                         
*                                                                               
         SR    R2,R2               REPPAK CONTRACT #                            
         IC    R2,0(R4)                                                         
         LA    R4,12(R4)                                                        
         GOTOR GETPCK,DMCB,(5,(R4)),(R2)                                        
         MVC   VHSCON,WORK                                                      
*                                                                               
DCCON050 BCTR  R3,0                MO CONTRACT #                                
         LTR   R3,R3                                                            
         BZ    DCCON200                                                         
         LA    R4,WORK2+32         IF THERE IS ONE                              
         CLI   0(R4),0                                                          
         BZ    DCCON100                                                         
         SR    R2,R2                                                            
         IC    R2,0(R4)                                                         
         LA    R4,12(R4)                                                        
         GOTOR GETPCK,DMCB,(6,(R4)),(R2)                                        
         MVC   VHSMORD,WORK                                                     
*                                                                               
DCCON100 BCTR  R3,0                R3 = SCANNER LINE COUNTER                    
         LTR   R3,R3                                                            
         BZ    DCCON200            END OF INPUT                                 
         LA    R4,WORK2+64                                                      
         CLI   0(R4),0                                                          
         BE    DCCON200                                                         
         LA    R4,12(R4)                                                        
         MVC   VHSSORD,0(R4)                                                    
*                                                                               
DCCON200 DS    0H                                                               
         LR    R9,R8               RESTORE ORIGINAL ADDRESS                     
         LA    R9,VHSPRMLQ(R9)                                                  
*                                                                               
         S     R9,ATWA                                                          
         ST    R9,ADDR                                                          
*                                                                               
         CHI   R9,(TWANOGO-100)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         L     R9,ADDR2                                                         
         A     R9,ATWA                                                          
         ZIC   RF,0(R9)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(R9)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
         DROP  R9                                                               
*.....................................................................          
* DOWNLOAD CONTRACT HEADER FIELDS REQUEST - DOWNLOAD BUYS FLAG                  
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
DCONBUY  LR    RB,RF                                                            
*                                                                               
         OI    MISCFLG1,MF1BUYS                                                 
*                                                                               
         B     EXITOK                                                           
*.....................................................................          
* DOWNLOAD CONTRACT HEADER FIELDS REQUEST - POST START DATE                     
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
DCONPSD  LR    RB,RF                                                            
         MVC   HALF,MDCODE                                                      
         L     R2,FPARMS+4                                                      
         GOTO1 VDATCON,DMCB,(8,0(R2)),(0,WORK)                                  
*                                  ORIGINAL START DATE -> EBCDIC                
         GOTO1 VGETDAY,DMCB,WORK,DUB                                            
         CLI   DMCB,1              ORIGINAL START = MONDAY?                     
         BE    DCPSD010                                                         
*                                                                               
         ZIC   R0,DMCB             MAKE IT A MONDAY                             
         BCTR  R0,0                                                             
         LNR   R0,R0                                                            
         GOTO1 VADDAY,DMCB,(C'D',WORK),(0,WORK),(R0)                            
*                                                                               
DCPSD010 DS    0H                                                               
         GOTO1 VDATCON,DMCB,(0,WORK),(3,POSTST)                                 
         B     EXITOK                                                           
*.....................................................................          
* DOWNLOAD CONTRACT HEADER FIELDS REQUEST - POST END DATE                       
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
DCONPED  LR    RB,RF                                                            
         MVC   HALF,MDCODE                                                      
         L     R2,FPARMS+4                                                      
         GOTO1 VDATCON,DMCB,(8,0(R2)),(0,WORK)                                  
*                                  ORIGINAL END DATE -> EBCDIC                  
         GOTO1 VGETDAY,DMCB,WORK,DUB                                            
         CLI   DMCB,7              ORIGINAL END = SUNDAY?                       
         BE    DCPED010                                                         
*                                                                               
         ZIC   R0,DMCB             MAKE IT A SUNDAY                             
         LNR   R0,R0                                                            
         AHI   R0,7                                                             
*                                                                               
         GOTO1 VADDAY,DMCB,(C'D',WORK),(0,WORK),(R0)                            
*                                                                               
DCPED010 DS    0H                                                               
         GOTO1 VDATCON,DMCB,(0,WORK),(3,POSTEN)                                 
         B     EXITOK                                                           
*.....................................................................          
* DOWNLOAD CONTRACT HEADER FIELDS REQUEST - DOWNLOAD INVS FLAG                  
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
DCONINV  LR    RB,RF                                                            
*                                                                               
         OI    MISCFLG1,MF1INVS                                                 
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* DOWNLOAD INVOICE HEADER FIELDS REQUEST -                                      
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
DINVORD  LR    RB,RF                                                            
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         OC    HALF,HALF           FIRST ORDER?                                 
         BNZ   DIORD010            NO                                           
*                                                                               
         S     RE,ATWA                                                          
         ST    RE,ADDR2            SAVE START OF CONTRACT                       
         LA    RE,1(RE)            NUMBER OF CONTRACTS GOES HERE                
         ST    RE,ADDR                                                          
*                                                                               
DIORD010 DS    0H                                                               
         MVC   HALF,MDCODE                                                      
         L     RF,FPARMS+4                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         MVC   0(L'VHPORD,RE),0(RF)       SAVE ORDER #                          
         LA    RE,L'VHPORD(RE)            BUMP TO NEXT ENTRY                    
         S     RE,ATWA                                                          
         ST    RE,ADDR                                                          
*                                                                               
         CHI   RE,(TWANOGO-100)                                                 
         BNL   ETOOBIG                                                          
*                                                                               
         L     RE,ADDR2                                                         
         A     RE,ATWA                                                          
         ZIC   RF,0(RE)                                                         
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)            UPDATE NUMBER OF ENTRIES                     
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
*.....................................................................          
* DOWNLOAD INVOICE HEADER FIELDS REQUEST -                                      
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
DINVDET  LR    RB,RF                                                            
         OI    MISCFLG1,MF1IDET                                                 
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
*.....................................................................          
* DOWNLOAD INVOICE HEADER FIELDS REQUEST -                                      
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
DINVPSD  LR    RB,RF                                                            
         MVC   HALF,MDCODE                                                      
         L     R2,FPARMS+4                                                      
         GOTO1 VDATCON,DMCB,(8,0(R2)),(0,WORK)                                  
         GOTO1 VDATCON,DMCB,(8,0(R2)),(3,BMOSFF)                                
*                                  ORIGINAL START DATE -> EBCDIC                
         GOTO1 VGETDAY,DMCB,WORK,DUB                                            
         CLI   DMCB,1              ORIGINAL START = MONDAY?                     
         BE    DIPSD010                                                         
*                                                                               
         ZIC   R0,DMCB             MAKE IT A MONDAY                             
         BCTR  R0,0                                                             
         LNR   R0,R0                                                            
         GOTO1 VADDAY,DMCB,(C'D',WORK),(0,WORK),(R0)                            
*                                                                               
*                                                                               
DIPSD010 DS    0H                                                               
         GOTO1 VDATCON,DMCB,(0,WORK),(3,POSTST)                                 
         B     EXITOK                                                           
*.....................................................................          
* DOWNLOAD INVOICE HEADER FIELDS REQUEST - POST END DATE                        
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
DINVPED  LR    RB,RF                                                            
         MVC   HALF,MDCODE                                                      
         L     R2,FPARMS+4                                                      
         GOTO1 VDATCON,DMCB,(8,0(R2)),(0,WORK)                                  
         GOTO1 VDATCON,DMCB,(8,0(R2)),(3,BMOSFFE)                               
*                                  ORIGINAL END DATE -> EBCDIC                  
         GOTO1 VGETDAY,DMCB,WORK,DUB                                            
         CLI   DMCB,7              ORIGINAL END = SUNDAY?                       
         BE    DIPED010                                                         
*                                                                               
         ZIC   R0,DMCB             MAKE IT A SUNDAY                             
         LNR   R0,R0                                                            
         AHI   R0,7                                                             
*                                                                               
         GOTO1 VADDAY,DMCB,(C'D',WORK),(0,WORK),(R0)                            
*                                                                               
DIPED010 DS    0H                                                               
         GOTO1 VDATCON,DMCB,(0,WORK),(3,POSTEN)                                 
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
**********************************************************************          
* MEDIA OCEAN PROFILE REQUEST - STATION FIELD                                   
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
MOSTSTA  LR    RB,RF                                                            
         OC    HALF,HALF           FIRST FIELD?                                 
         BNZ   EPARMSEQ            NO - PARAMETER SEQUENCE ERROR                
*                                                                               
         CLC   FPARMS+8,=F'5'      LENGTH OK?                                   
         BNE   EINVLEN             NO  - TOO MANY CHARACTERS                    
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
*                                                                               
         MVC   VHPSTA,0(RF)        COPY STATION                                 
         OC    VHPSTA,SPACES                                                    
*                                                                               
         B     EXITOK                                                           
         DROP  RE                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* MEDIA OCEAN PROFILE REQUEST - OFFICE  FIELD                                   
*.....................................................................          
         DS    0D                                                               
         USING *,RB                                                             
MOSTOFF  LR    RB,RF                                                            
*                                                                               
         CLC   FPARMS+8,=F'2'      LENGTH OK?                                   
         BNE   EINVLEN             NO                                           
*                                                                               
         MVC   HALF,MDCODE                                                      
         L     RE,ADDR                                                          
         A     RE,ATWA                                                          
         USING VHPARMD,RE                                                       
         L     RF,FPARMS+4                                                      
*                                                                               
         MVC   VHPOFF,0(RF)        COPY OFFICE                                  
         OC    VHPOFF,SPACES                                                    
*                                                                               
         B     EXITOK                                                           
         DROP  RE                                                               
         LTORG                                                                  
GETPCK   NTR1  BASE=*,LABEL=*                                                   
         LM    R3,R4,0(R1)                                                      
         MVC   BYTE,0(R1)                                                       
         GOTOR VCASHVAL,DMCB,(C'0',(R3)),(R4)                                   
         CLI   0(R1),FF                                                         
         BNE   *+6                                                              
         DC    H'0'                INVALID VALUE                                
                                                                                
         XC    WORK,WORK                                                        
         SR    RF,RF                                                            
         IC    RF,BYTE                                                          
         BCTR  RF,0                                                             
         SLL   RF,4                                                             
         EX    RF,GETPZAP          ZAP IN DATA                                  
GETPCKX  J     EXIT                                                             
                                                                                
GETPZAP  ZAP   WORK(0),DMCB+4(8)                                                
         EJECT                                                                  
**********************************************************************          
* INITIAL DOWNLOAD                                                              
**********************************************************************          
INITDWN  NTR1  BASE=*,LABEL=*                                                   
*                                  ADD NEW REP COMPANY ELEMENT                  
         GOTO1 ASETELEM,DMCB,AFABLK,REPDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,REPCODEL,REPCODE,0                          
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,REPNAMEL,REPNAME,0                          
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,REPADDEL,REPADDR,0                          
*                                                                               
*                                                                               
         CLI   LOWRND,0            DATA?                                        
         BE    IDWN0002            NO                                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,REPDLREL,LOWRND,0                           
         GOTO1 AADDDATA,DMCB,AFABLK,REPDHREL,HIRND,0                            
*                                                                               
IDWN0002 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,REPLOCEL,LOCALREP,0                         
         OC    URLLEN,URLLEN       ANY URL AVAILABLE?                           
         BZ    IDWN0003            NO                                           
         GOTO1 AADDDATA,DMCB,AFABLK,REPENPEL,URLSTORE,0                         
IDWN0003 DS    0H                                                               
         OC    URLLENET,URLLENET   ANY URL AVAILABLE?                           
         BZ    IDWN0005            NO                                           
         GOTO1 AADDDATA,DMCB,AFABLK,REPETURL,URLSTOET,0                         
IDWN0005 DS    0H                                                               
         TM    EZPPROF+EZPHCONB,EZPHCONA                                        
         BZ    IDWN0009                                                         
         CLC   REPALPHA,=C'NB'     NOT FOR NBC                                  
         BE    IDWN0009                                                         
*                                                                               
         LA    RE,CUTOFF#                                                       
IDWN0006 CLI   0(RE),X'FF'                                                      
         BE    IDWN0008                                                         
         CLC   REPALPHA,0(RE)                                                   
         BE    IDWN0007                                                         
         LA    RE,6(RE)                                                         
         B     IDWN0006                                                         
IDWN0007 DS    0H                                                               
         MVC   FULL,2(RE)                                                       
         B     *+10                                                             
IDWN0008 MVC   FULL,=X'0098967F'    9999999                                     
         GOTO1 AADDDATA,DMCB,AFABLK,REPHICEL,FULL,0                             
IDWN0009 DS    0H                                                               
*                                                                               
*  ADD PROFILES                                                                 
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,PRFDATA,0                                   
         GOTO1 AADDDATA,DMCB,AFABLK,PRFCNT1L,CONPROF,0                          
         GOTO1 AADDDATA,DMCB,AFABLK,PRFCNT2L,CONPROF+4,0                        
         GOTO1 AADDDATA,DMCB,AFABLK,PRFEZP1L,EZPPROF,0                          
         GOTO1 AADDDATA,DMCB,AFABLK,PRFEZP2L,EZPPROF+4,0                        
*                                                                               
         XC    FULL,FULL           EXACT TIME USER?                             
         MVI   FULL,C'N'                                                        
         TM    EZPPROF+EZPEXTMB,EZPEXTMA                                        
         BZ    *+8                                                              
         MVI   FULL,C'Y'                                                        
         GOTO1 AADDDATA,DMCB,AFABLK,PRFETUSR,FULL,0                             
*                                                                               
         GOTO1 VCOLY,DMCB,(X'10',0),(0,0)                                       
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADDR,DMCB                                                        
*                                                                               
         LH    R5,=Y(SVPARMBF-T82AFFD)                                          
         A     R5,ATWA                                                          
         USING VHPARMD,R5                                                       
         OC    VHPSAL,VHPSAL       VALIDATE SALESPERSON?                        
         BZ    IDWN0018            NO                                           
*                                                                               
         GOTOX (VSALQ,ADDR),DMCB,(RC),(0,VHPSAL),VHPARMLQ(R5)                   
         BE    *+14                                                             
         MVC   ERROR,=Y(154)                                                    
         B     EXITL                                                            
         DROP  R5                                                               
*                                                                               
         GOTOR GETSAL,DMCB,(R5)    GET SALES NAME                               
*                                                                               
         LA    R0,*                                                             
         AHI   R0,SALTBL-(*-4)                                                  
         GOTOR XARSE,DMCB,VHPARMLQ(R5),(R0)                                     
*                                                                               
IDWN0018 DS    0H                                                               
         GOTOX (GDPLISTQ,ADDR),DMCB,(RC),VHPARMLQ(R5)                           
         BNE   IDWN0030                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,DPTTBL-(*-4)                                                  
         GOTOR XARSE,DMCB,VHPARMLQ(R5),(R0)                                     
*                                                                               
IDWN0030 DS    0H                                                               
         CLI   LOCALREP,1                                                       
         BNE   IDWN0040                                                         
         GOTO1 ASETELEM,DMCB,AFABLK,STADATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,STASTAEL,SVSIGNON,0                         
*                                                                               
IDWN0040 DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* CUT OFF CONTRACT # TABLE                                                      
*                                                                               
CUTOFF#  DS    0A                                                               
         DC    X'FF'                                                            
**********************************************************************          
* SAVE SALESPERSON NAME FROM OUTPUT STREAM                                      
**********************************************************************          
GETSAL   NTR1  BASE=*,LABEL=*                                                   
         L     R5,0(R1)                                                         
         LA    R8,VHPARMLQ(R5)                                                  
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,1,2(R8)                                                       
         BZ    EXITL               NO SUBDIVISIONS                              
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,1,2(R8)                                                       
         MHI   R2,2                                                             
         LA    R2,3(R2,R8)         INDEX TO DATA                                
*                                                                               
         LA    R5,3(R8)                                                         
         ICM   R4,1,2(R8)                                                       
*                                                                               
GSAL10   CLI   1(R5),C'N'          NAME                                         
         BE    GSAL20              YES                                          
*                                                                               
         ZIC   R0,0(R5)            BUMP DISPLACEMENT INTO DATA                  
         AR    R2,R0                                                            
*                                                                               
         LA    R5,2(R5)            NEXT ENTRY                                   
         BCT   R4,GSAL10                                                        
         B     GSALX                                                            
*                                                                               
*        L     RA,ATWA                                                          
*        USING TWAD,RA                                                          
GSAL20   MVC   TWASALN,0(R2)                                                    
GSALX    B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
**********************************************************************          
* LIST CONTRACT DOWNLOAD                                                        
**********************************************************************          
LCONDWN  NTR1  BASE=*,LABEL=*                                                   
         LH    R5,=Y(SVPARMBF-T82AFFD)                                          
         A     R5,ATWA                                                          
         USING VHPARMD,R5                                                       
*                                                                               
         XC    KEY,KEY                                                          
K        USING RCON8ETP,KEY                                                     
KS       USING RCON8ETP,KEYSAVE                                                 
         MVI   K.RCON8ETP,X'8E'                                                 
         MVC   K.RCON8ERP,REPALPHA                                              
         MVC   K.RCON8EST,VHPSTA                                                
*                                                                               
         GOTO1 VDATCON,DMCB,(8,VHPFLS),(2,FULL)                                 
         GOTO1 VDATCON,DMCB,(8,VHPFLE),(2,FULL+2)                               
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
LCD0010  DS    0H                                                               
         CLC   KEY(RCON8EFS-RCON8ETP),KEYSAVE                                   
         BNE   LCDX                                                             
*                                                                               
         CLI   K.RCON8EID,1                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    VHPAGY,VHPAGY       FILTER AGENCY?                               
         BZ    LCD0014                                                          
*                                                                               
         CLC   VHPAGY,K.RCON8EAG                                                
         BNE   LCD0100                                                          
*                                                                               
         OC    VHPAOF,VHPAOF       FILTER AGENCY OFFICE?                        
         BZ    LCD0014                                                          
*                                                                               
         CLC   VHPAOF,K.RCON8EAG+4                                              
         BNE   LCD0100                                                          
*                                                                               
LCD0014  DS    0H                                                               
         OC    VHPADV,VHPADV       FILTER AGENCY?                               
         BZ    LCD0016                                                          
*                                                                               
         CLC   VHPADV,K.RCON8EAV                                                
         BNE   LCD0100                                                          
*                                                                               
LCD0016  DS    0H                                                               
         GOTO1 VSEQ                                                             
         CLI   K.RCON8EID,2                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LCD0020  DS    0H                                                               
         CLC   K.RCON8EFE,FULL     CHECK FOR FLIGHT OVERLAP                     
         BL    LCD0100                                                          
         CLC   K.RCON8EFS,FULL+2                                                
         BH    LCD0100                                                          
*                                                                               
         CLC   K.RCON8ESP,VHPSAL                                                
         BNE   LCD0100                                                          
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
         L     R8,AIOREC                                                        
         USING RCONREC,R8                                                       
         MVI   ELCODE,X'03'        SEE IF THERE IS MONEY                        
         BAS   RE,GETEL                                                         
         BNE   LCD0100                                                          
*                                                                               
         L     R8,AIOREC                                                        
         USING RCONREC,R8                                                       
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,CONDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RCONKCON                                                 
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONNUMEL,WORK,0                             
         GOTO1 VDATCON,DMCB,(3,RCONDATE),(19,WORK)                              
         GOTO1 AADDDATA,DMCB,AFABLK,CONFLSEL,WORK,0                             
         GOTO1 VDATCON,DMCB,(3,RCONDATE+3),(19,WORK)                            
         GOTO1 AADDDATA,DMCB,AFABLK,CONFLEEL,WORK,0                             
         GOTO1 AADDDATA,DMCB,AFABLK,CONBYREL,RCONBUYR,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONAGYEL,RCONKAGY,0                         
         CLC   RCONKAOF,SPACES                                                  
         BNH   LCD0022                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,CONAOFEL,RCONKAOF,0                         
*                                                                               
LCD0022  DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONADVEL,RCONKADV,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONSTAEL,RCONKSTA,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONSALEL,RCONSAL,0                          
         GOTO1 AADDDATA,DMCB,AFABLK,CONCTYEL,RCONTYPE,0                         
*                                                                               
         CLC   RCONPRD,SPACES                                                   
         BNH   LCD0030                                                          
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONPRDEL,RCONPRD,L'RCONPRD                  
         B     LCD0032                                                          
*                                                                               
LCD0030  DS    0H                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'05'        GET PRODUCT ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   LCD0032                                                          
*                                                                               
         USING RCONEXEL,R8                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,CONPRDEL,RCONEXPR,L'RCONEXPR                
         DROP  R8                                                               
*                                                                               
LCD0032  DS    0H                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'18'        DEVELOPMENT INFO ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   LCD0040                                                          
*                                                                               
         USING RCONDVEL,R8                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,CONDCTEL,RCONDVCT,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONDSPEL,RCONDVSP,0                         
         DROP  R8                                                               
*                                                                               
LCD0040  DS    0H                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'A2'        EASI CODE ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   LCD0045                                                          
         USING RCONIEL,R8                                                       
         GOTO1 AADDDATA,DMCB,AFABLK,CONEADEL,RCONIADV,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONEPDEL,RCONIPRD,0                         
         CLC   RCONIEST,SPACES                                                  
         BNH   LCD0042                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,CONEESEL,RCONIEST,0                         
         B     LCD0045                                                          
*                                                                               
LCD0042  DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONEESEL,RCONXEST,0                         
         DROP  R8                                                               
*                                                                               
LCD0045  DS    0H                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'1F'        LOOK FOR TRAFFIC NUMBER                      
         BAS   RE,GETEL                                                         
         BNE   LCD0048                                                          
         USING RCONXEL,R8                                                       
         GOTO1 AADDDATA,DMCB,AFABLK,CONTNMEL,RCONTRF,0                          
         DROP  R8                                                               
*                                                                               
LCD0048  DS    0H                                                               
         MVI   K.RCON8EID,X'03'    READ DEMO KEY                                
         XC    K.RCON8EAG(L'KEY-(RCON8EAG-RCON8ETP)),K.RCON8EAG                 
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(RCON8EAG-RCON8ETP),KEYSAVE                                   
         BNE   LCD0050                                                          
*                                                                               
         L     RE,AIO2                                                          
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'INV'                                                   
         DROP  RE                                                               
*                                                                               
         XC    WORK+60(50),WORK+60                                              
         MVC   WORK+60(3),K.RCON8EMO                                            
         CLI   WORK+60+1,C'T'      FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   WORK+60+1,C'I'                                                   
*                                                                               
         GOTO1 VDEMOCON,DMCB,(1,WORK+60),(9,WORK),(0,AIO2)                      
         ZIC   R0,0(R1)                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONDEMEL,WORK,(R0)                          
*                                                                               
LCD0050  DS    0H                                                               
         MVC   KEY,KEYSAVE         RESET KEY                                    
*                                                                               
LCD0100  DS    0H                                                               
*                                                                               
         MVI   K.RCON8EID,X'FF'    READ NEXT RECORD SET                         
         XC    K.RCON8EAG(L'KEY-(RCON8EAG-RCON8ETP)),K.RCON8EAG                 
         GOTO1 VHIGH                                                            
         B     LCD0010                                                          
*                                                                               
LCDX     DS    0H                                                               
         B     EXITOK                                                           
         DROP  K,KS,R5                                                          
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* RATINGS DOWNLOAD                                                              
**********************************************************************          
RTGSDWN  NTR1  BASE=*,LABEL=*                                                   
         L     R2,ATWA                                                          
         AHI   R2,(SVPARMBF-T82AFFD)                                            
         ST    R2,ADDR                                                          
*                                                                               
         MVC   NUMBKS,0(R2)        NUMBER OF BOOKS                              
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,ADDR             DISP. TO 1ST BOOK                            
         ST    RF,FRSTBK                                                        
         ZIC   RF,NUMBKS                                                        
         MH    RF,=Y(BKLENQ)       BUMP TO UPGRADES                             
         AR    R2,RF                                                            
*                                                                               
         MVC   NUMUPGS,0(R2)       NUMBER OF UPGRADES                           
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,ADDR             DISP. TO 1ST UPGRADE                         
         ST    RF,FRSTUPG                                                       
         ZIC   RF,NUMUPGS                                                       
         MH    RF,=Y(UPGLENQ)      BUMP TO DEMOS                                
         AR    R2,RF                                                            
*                                                                               
         MVC   NUMDEMS,0(R2)       NUMBER OF DEMOS                              
         LA    R2,1(R2)                                                         
         LR    RF,R2                                                            
         S     RF,ADDR             DISP. TO 1ST DEMO                            
         ST    RF,FRSTDEM                                                       
         ZIC   RF,NUMDEMS                                                       
         MH    RF,=Y(DEMLENQ)      BUMP TO RATE CARDS                           
         AR    R2,RF                                                            
         EJECT                                                                  
*                                                                               
         LR    RF,R2                                                            
         S     RF,ADDR             DISP. TO 1ST STATION                         
         ST    RF,FRSTSTA                                                       
         LA    R2,5(R2)            POINT TO STATIONS PROGRAMS                   
*                                                                               
         LR    RF,R2                                                            
         S     RF,ADDR             DISP. TO INVENOTRY                           
         ST    RF,CURINV                                                        
*                                                                               
         MVC   CURSTA,FRSTSTA      SET CURRENT STATION                          
         ZAP   INVSEQ,=P'1'                                                     
         NI    MISCFLG2,X'FF'-MF2DCPRC DECIMAL PRECISION FLAG                   
*                                                                               
********************************************************                        
* LOOP THROUGH ALL THE STATIONS / INVENTORY                                     
********************************************************                        
XPSTA000 DS    0H                                                               
         LA    R0,FETCHBLK         CLEAR THE FETCH PARAMTER BLOCK               
         LH    R1,=Y(RFTBLKL)                                                   
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R4,FETCHBLK                                                      
         USING RFTBLKD,R4                                                       
         MVC   RFTACOM,ACOMFACS    A(COMFACS)                                   
         MVC   RFTAIO1,AIO3        A(2K IO AREA)                                
         MVC   RFTAIO2,AIO4        A(2K IO AREA)                                
         LA    RE,FETCHWRK                                                      
         STCM  RE,15,RFTAWRK       A(6K WORK AREA)                              
         MVC   RFTCREP,REPALPHA    REP CODE                                     
         MVC   RFTCSRC,RTSRVC      RATING SERVICE                               
         MVI   RFTAMODE,RFTADIRQ   FETCH MODE                                   
         MVI   RFTCDCTL,RFTCDC1Q   FETCH METHOD                                 
         LA    RE,FTCHHOOK         HOOK ROUTINE                                 
         STCM  RE,15,RFTHOOKA                                                   
         OI    RFTCNTL,RFTCHDRQ    INCLUDE HEADER                               
         OI    RFTCNTL,RFTCDEMQ    INCLUDE DEMOS                                
         OI    RFTCNTL,RFTCSLVQ    INCLUDE SHARES AND LEVELS                    
         OI    RFTCNTL,RFTCFTNQ    INCLUDE FOOTNOTES                            
*                                                                               
         MVC   RFTPRCSN,RTGPRCSN   SET RATING PRECISION                         
         CLC   RFTPRCSN,SPACES                                                  
         BH    XPSTA010                                                         
         MVI   RFTPRCSN,C'1'       IF NOT SPECIFIED, DEFAULT TO 1               
*                                                                               
XPSTA010 DS    0H                                                               
         L     RE,ADDR             POINT TO CURRENT STATIONS                    
         A     RE,CURSTA                                                        
         MVC   RFTCSTAT,0(RE)      STATION CALL LETTERS                         
         CLI   RFTCSTAT+4,C' '     NEED 'T' SET?                                
         BNE   *+8                 NO                                           
         MVI   RFTCSTAT+4,C'T'                                                  
*                                                                               
         L     RE,ADDR             POINT TO CURRENT INVENOTRY                   
         A     RE,CURINV                                                        
*                                                                               
         CLI   0(RE),0             ANY INVENTORY NUMBER?                        
         BE    XPSTA100            NO SKIP PLACE HOLDER                         
*                                                                               
         MVI   RFTCDTDP,C'D'       DAYPART                                      
         LA    R0,RFTCDTM+(8*RFTCDTLQ)     END OF LIST                          
         LA    RF,RFTCDTM                                                       
X        USING RFTCDTM,RF                                                       
*                                                                               
XPSTA020 DS    0H                                                               
         MVC   X.RFTCDTDY,0(RE)      DAY                                        
         MVC   X.RFTCDTST,1(RE)      START TIME                                 
*                                                                               
         CLI   3(RE),X'FF'           EFFECTIVE DATE W/NO END TIME?              
         BE    *+14                  YES                                        
         MVC   X.RFTCDTEN,3(RE)      END TIME                                   
         B     XPSTA022                                                         
*                                                                               
         XC    X.RFTCDTEN,X.RFTCDTEN                                            
         MVC   X.RFTCDTES,4(RE)                                                 
         LA    RE,1(RE)            FUDGE ADDRESS                                
         DROP  X                                                                
*                                                                               
XPSTA022 DS    0H                                                               
         CLI   5(RE),X'FF'         END OF PROGRAM?                              
         BE    XPSTA030                                                         
*                                                                               
         LA    RF,RFTCDTLQ(RF)     NO BUILD ORBIT                               
         LA    RE,5(RE)                                                         
*                                                                               
         CR    RF,R0               PAST END OF LIST?                            
         BL    XPSTA020            NO -  ADD TO REQUEST                         
         B     XPSTA022            YES - SKIP OVER                              
*                                                                               
XPSTA030 DS    0H                                                               
         S     RE,ADDR             SAVE CURRENT POINTER IN CASE OF              
         ST    RE,CURINV            ORBITS                                      
*                                                                               
         L     RF,AIO1                                                          
         ST    RF,ACURPARM         STORE ADDRESS OF CURRENT PARMS               
         EJECT                                                                  
*-----------------*                                                             
* BUILD BOOK LIST *                                                             
*-----------------*                                                             
         CLI   NUMDEMS,0                                                        
         BE    DEMSX99                                                          
*                                                                               
         CLI   NUMBKS,0                                                         
         BE    BKSX099                                                          
*                                                                               
         L     RF,ACURPARM         GET ADDRESS OF CURRENT PARMS                 
         MVI   RFTCBKS,LONGPARM    SET PARM AS NULL TERM. LIST                  
         STCM  RF,15,RFTCBKS+1                                                  
         ZIC   R1,NUMBKS           SET NUMBER OF REMAINING BOOKS                
         L     RE,ADDR             POINT TO CURRENT BOOK                        
         A     RE,FRSTBK                                                        
BKSX010  DS    0H                  LOOP AND SET BOOKS                           
         LR    R0,RF               VERIFY DATA FITS IN PARMS                    
         S     R0,AIO1                                                          
         CH    R0,=Y(LENIO-(RFTCBKLQ*2))                                        
         BH    ETOOBIG             NO ROOM                                      
*                                                                               
         USING RFTCBKS,RF                                                       
         XC    0(RFTCBKLQ,RF),0(RF)                                             
         MVC   0(RFTCBKLQ,RF),0(RE)                                             
         LA    RF,RFTCBKLQ(RF)                                                  
         LA    RE,BKLENQ(RE)                                                    
         DROP  RF                                                               
*                                                                               
         BCT   R1,BKSX010                                                       
*                                                                               
BKSX020  DS    0H                                                               
         XC    0(RFTCBKLQ,RF),0(RF)                                             
         LA    RF,RFTCBKLQ(RF)                                                  
         ST    RF,ACURPARM                                                      
*                                                                               
BKSX099  DS    0H                                                               
         EJECT                                                                  
*--------------------*                                                          
* BUILD UPGRADE LIST *                                                          
*--------------------*                                                          
         CLI   NUMUPGS,0                                                        
         BE    UPGSX99                                                          
*                                                                               
         L     RF,ACURPARM         GET ADDRESS OF CURRENT PARMS                 
         STCM  RF,15,RFTCUPGA                                                   
         ZIC   R1,NUMUPGS          SET NUMBER OF REMAINING BOOKS                
         L     RE,ADDR             POINT TO CURRENT BOOK                        
         A     RE,FRSTUPG                                                       
UPGSX10  DS    0H                  LOOP AND SET BOOKS                           
         LR    R0,RF               VERIFY DATA FITS IN PARMS                    
         S     R0,AIO1                                                          
         CH    R0,=Y(LENIO-(UPGLENQ*2))                                         
         BH    ETOOBIG             NO ROOM                                      
*                                                                               
         MVC   0(UPGLENQ-1,RF),0(RE)                                            
         LA    RF,UPGLENQ-1(RF)                                                 
         LA    RE,UPGLENQ(RE)                                                   
         BCT   R1,UPGSX10                                                       
*                                                                               
         XC    0(UPGLENQ,RF),0(RF)                                              
         LA    RF,UPGLENQ(RF)                                                   
         ST    RF,ACURPARM                                                      
*                                                                               
UPGSX99  DS    0H                                                               
         EJECT                                                                  
*-----------------*                                                             
* BUILD DEMO LIST *                                                             
*-----------------*                                                             
         LA    RF,RFTCDEMS                                                      
         ZIC   R1,NUMDEMS          SET NUMBER OF REMAINING DEMOS                
         CLI   NUMDEMS,((RFTCBKS-RFTCDEMS)/3)-1                                 
         BNH   *+8                 NOT TOO MANY DEMOS                           
         LA    R1,((RFTCBKS-RFTCDEMS)/3)-1                                      
         L     RE,ADDR             POINT TO CURRENT DEMO                        
         A     RE,FRSTDEM                                                       
DEMSX10  DS    0H                  LOOP AND SET DEMOS                           
         MVC   0(3,RF),0(RE)                                                    
         LA    RF,3(RF)                                                         
         LA    RE,DEMLENQ(RE)                                                   
         BCT   R1,DEMSX10                                                       
*                                                                               
DEMSX99  DS    0H                                                               
         MVI   RFTOVRNT,X'00'      SET OVERNIGHT REQUEST OFF                    
         CLI   OVRNTFLG,C'Y'       REQUEST FOR OVERNIGHT DATA?                  
         BNE   OVNT0020            NO                                           
         MVC   RFTOVRNT,OVRNTFLG   YES                                          
         MVI   RFTOVHMS,X'00'      SET DEMO OVERRIDE FLAG OFF                   
         CLI   OVRNTFLG,C'Y'       REQUEST FOR DEMO OVERRIDE?                   
         BNE   OVNT0020            NO                                           
         MVC   RFTOVHMS,OVHMSFLG   YES                                          
*                                                                               
         MVC   RFTOVNBK,OVRNTBKT   SET OVERNIGHT BOOKTYPE                       
         CLI   OVRNTBKT+1,C' '     2 CHAR BOOKTYPE?                             
         BE    OVNT0020                                                         
*                                                                               
         L     R3,ACOMFACS                                                      
         L     RF,CDEMTABS-COMFACSD(,R3)                                        
         GOTO1 (RF),DMCB,SPBOOKTB                                               
         ICM   RF,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
OVNT0010 DS    0H                                                               
         CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   OVRNTBKT,SPBKTYPA                                                
         BE    *+10                                                             
         AR    RF,R0                                                            
         B     OVNT0010                                                         
*                                                                               
         MVC   RFTOVNBK,SPBKTYPN   SET INTERNAL OVERNIGHT BOOKTYPE              
         DROP  RF                                                               
*                                                                               
OVNT0020 EQU   *                                                                
         EJECT                                                                  
*------------*                                                                  
* FETCH CALL *                                                                  
*------------*                                                                  
         NI    MISCFLG1,FF-MF1TMPB2   SET NEW INVENTORY                         
         GOTO1 VFETCH,DMCB,FETCHBLK                                             
         DROP  R4                                                               
*                                                                               
XPSTA100 DS    0H                  PROCESS NEXT INVENTORY                       
         L     RE,ADDR                                                          
         A     RE,CURINV                                                        
         LA    RE,6(RE)                                                         
         CLI   0(RE),0             END OF INVENOTRY FOR THIS STATION?           
         BNE   XPSTA110            YES                                          
*                                                                               
         LA    RE,1(RE)            PROCESS NEXT STATION                         
         CLI   0(RE),0             END OF STATIONS?                             
         BE    XPSTAX              YES                                          
*                                                                               
         LR    RF,RE                                                            
         S     RF,ADDR                                                          
         ST    RF,CURSTA                                                        
         LA    RE,5(RE)            BUMP TO INVENTORY                            
*                                                                               
XPSTA110 DS    0H                  PROCESS NEXT INVENTORY                       
         S     RE,ADDR                                                          
         ST    RE,CURINV                                                        
         AP    INVSEQ,=P'1'                                                     
         B     XPSTA000               FETCH IT                                  
*                                                                               
XPSTAX   DS    0H                  END OF FETCH LOOP                            
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* HOOK FOR THE FETCH ROUTINE TO ADD NEW DETAIL CLUSTERS                         
***********************************************************************         
FTCHHOOK NTR1  BASE=*,LABEL=*                                                   
         LA    R4,FETCHBLK                                                      
         USING RFTBLKD,R4                                                       
         OC    RFTERR,RFTERR                                                    
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   RFTMODE,RFTNBKQ                                                  
         BE    INVBK                                                            
         B     EXITOK                                                           
         EJECT                                                                  
*=============================*                                                 
* PROCESS INVENTORY BOOK HOOK *                                                 
*=============================*                                                 
INVBK    DS    0H                                                               
         OC    RFTFDEMS(24*4),RFTFDEMS                                          
         BNZ   IBK00001                                                         
         OC    RFTFSHRS(24*4),RFTFSHRS                                          
         BNZ   IBK00001                                                         
         OC    RFTFLVLS(24*4),RFTFLVLS                                          
         BZ    EXITOK              NO INTERESTING DATA                          
*                                                                               
IBK00001 DS    0H                                                               
*=============================*                                                 
* SPECIFY DECIMAL PRECISION FOR DEMO VALUES                                     
*=============================*                                                 
         TM    MISCFLG2,MF2DCPRC   DONE IT ALREADY?                             
         BNZ   IBK00005            YES, SKIP                                    
         OI    MISCFLG2,MF2DCPRC                                                
         ZIC   R2,NUMDEMS                                                       
         LTR   R2,R2                                                            
         BZ    IBK00005                                                         
         LA    R5,1                DEMO SEQUENCE NUMBER                         
         LA    R3,RFTCDEMS         DEMO CATEGORIES                              
*                                                                               
IPRC0010 DS    0H                                                               
*                                  ADD NEW PRECISION ELEMENT                    
         GOTO1 ASETELEM,DMCB,AFABLK,PRCDATA,0                                   
*                                                                               
         STC   R5,BYTE             SET SEQUENCE NUMBER                          
         GOTO1 AADDDATA,DMCB,AFABLK,PRCSEQEL,BYTE,0                             
*                                                                               
         CLC   RTGPRCSN,SPACES                                                  
         BH    *+8                                                              
         MVI   RTGPRCSN,C'1'       IF NOT SPECIFIED, DEFAULT TO 1               
*                                                                               
         L     R6,=A(DEC1TAB)                                                   
         CLI   RTGPRCSN,C'1'                                                    
         BE    IPRC0020                                                         
         L     R6,=A(DEC2TAB)                                                   
         CLI   RTGPRCSN,C'2'                                                    
         BE    IPRC0020                                                         
         L     R6,=A(DEC0TAB)                                                   
*                                                                               
IPRC0020 DS    0H                                                               
         A     R6,BASERELO                                                      
         L     RF,AIO1                                                          
DB       USING DBLOCKD,RF                                                       
*                                                                               
IPRC0030 DS    0H                                                               
         CLC   PRCTABLE,0(R6)                                                   
         BNE   IPRC0040                                                         
         CLC   1(1,R3),1(R6)                                                    
         BE    IPRC0050                                                         
         DROP  DB                                                               
*                                                                               
IPRC0040 DS    0H                                                               
         AHI   R6,L'DEC0TAB                                                     
         CLI   0(R6),0                                                          
         BNE   IPRC0030                                                         
         MVI   BYTE,0                                                           
         B     IPRC0060                                                         
*                                                                               
IPRC0050 DS    0H                                                               
         MVC   BYTE,2(R6)                                                       
*                                                                               
IPRC0060 DS    0H                  SET PRECISION VALUE                          
         GOTO1 AADDDATA,DMCB,AFABLK,PRCPRCEL,BYTE,0                             
*                                                                               
         AHI   R5,1                                                             
         AHI   R3,L'RFTCDEMS                                                    
         BCT   R2,IPRC0010                                                      
*                                                                               
*=============================*                                                 
*                                                                               
IBK00005 DS    0H                                                               
         OC    RFTFBK,RFTFBK       WAS IT A BOOK?                               
         BZ    INVUPG              NO - ITS AN UPGRADE                          
*                                                                               
         OI    MISCFLG1,MF1DATA    SET DATA IN BUFFER                           
         ZIC   RF,NUMBKS           GET BOOK SEQUENCE NUMBER                     
         L     RE,FRSTBK                                                        
         A     RE,ADDR                                                          
IBK0010  DS    0H                                                               
         CLC   RFTFBK,0(RE)        BOOK MATCH?                                  
         BE    IBK0020             YES                                          
         LA    RE,BKLENQ(RE)                                                    
         BCT   RF,IBK0010                                                       
         DC    H'0'                UM - THIS CAN'T HAPPEN                       
*                                                                               
IBK0020  DS    0H                                                               
         ZIC   RE,NUMBKS                                                        
         SR    RE,RF                                                            
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
*                                                                               
*                                  ADD NEW BOOK ELEMENT                         
         GOTO1 ASETELEM,DMCB,AFABLK,BKSDATA,0                                   
*                                                                               
         TM    MISCFLG1,MF1TMPB2   NEED INVENTORY SEQUENCE NUMBER?              
         BNZ   IBK0022             NO                                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BKSPRGEL,INVSEQ,0                           
*                                                                               
         OI    MISCFLG1,MF1TMPB2   SET INVENTORY INDICATED                      
IBK0022  DS    0H                                                               
*                                  SET SEQUENCE NUMBER                          
         GOTO1 AADDDATA,DMCB,AFABLK,BKSSEQEL,BYTE,0                             
*                                                                               
         L     RE,RFTFTX1A         GET FOOTNOTE LENGTH                          
         OC    0(30,RE),SPACES                                                  
         LA    RE,30(RE)                                                        
IBK0030  CLI   0(RE),C' '                                                       
         BH    IBK0032                                                          
         BCTR  RE,0                                                             
         C     RE,RFTFTX1A                                                      
         BNL   IBK0030                                                          
         B     IBK0040             NO SIGNIFICANT FOOTNOTE                      
*                                                                               
IBK0032  DS    0H                                                               
         S     RE,RFTFTX1A                                                      
         LA    R0,1(RE)            SAVE FOOTNOTE LENGTH                         
*                                                                               
*&&DO                                                                           
         MVI   BYTE,3              SET FLUFF LENGTH                             
         GOTO1 AADDDATA,DMCB,AFABLK,BKSFOOEL,BYTE,0                             
*&&                                SET FOOTNOTE                                 
         L     RF,RFTFTX1A                                                      
         LA    RF,3(RF)            SKIP FLUFF FOR NOW                           
         SH    R0,=H'3'                                                         
         BNP   IBK0040                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,BKSFTNEL,(RF),(R0)                          
*                                                                               
IBK0040  DS    0H                  DO DEMOS                                     
         B     INVDEM                                                           
*================================*                                              
* PROCESS INVENTORY UPGRADE HOOK *                                              
*================================*                                              
INVUPG   DS    0H                                                               
         OI    MISCFLG1,MF1DATA    SET DATA IN BUFFER                           
         GOTO1 ASETELEM,DMCB,AFABLK,BKSDATA,0                                   
*                                                                               
         TM    MISCFLG1,MF1TMPB2   NEED INVENTORY SEQUENCE NUMBER?              
         BNZ   IUPG002             NO                                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BKSPRGEL,INVSEQ,0                           
*                                                                               
         OI    MISCFLG1,MF1TMPB2   SET INVENTORY INDICATED                      
IUPG002  DS    0H                                                               
         SR    R0,R0               SET SEQUENCE NUMBER                          
         L     R1,RFTFUPGA                                                      
         S     R1,RFTCUPGA         DISPLACEMENT TO UPGRADE                      
         LTR   R1,R1                                                            
         BZ    IUPG010                                                          
*                                                                               
         LA    RE,UPGLENQ-1                                                     
         DR    R0,RE                                                            
IUPG010  DS    0H                                                               
         LA    R0,1(R1)            1 BASED SEQUENCE NUMBER                      
         STC   R0,BYTE                                                          
*                                                                               
         MHI   R1,UPGLENQ          ADDRESS INPUT UPGRADE                        
         A     R1,FRSTUPG                                                       
         A     R1,ADDR                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,BKSUPGEL,BYTE,0                             
*                                                                               
IUPG040  DS    0H                                                               
         L     RE,RFTFTX1A         GET FOOTNOTE LENGTH                          
         OC    0(30,RE),SPACES                                                  
         LA    RE,30(RE)                                                        
IUPG050  CLI   0(RE),C' '                                                       
         BH    IUPG052                                                          
         BCTR  RE,0                                                             
         C     RE,RFTFTX1A                                                      
         BNL   IUPG050                                                          
         B     IUPG060             NO SIGNIFICANT FOOTNOTE                      
*                                                                               
IUPG052  DS    0H                                                               
         S     RE,RFTFTX1A                                                      
         LA    R0,1(RE)            SAVE FOOTNOTE LENGTH                         
*                                                                               
*&&DO                                                                           
         MVI   BYTE,3              SET FLUFF LENGTH                             
         GOTO1 AADDDATA,DMCB,AFABLK,BKSFOOEL,BYTE,0                             
*&&                                SET FOOTNOTE                                 
         L     RF,RFTFTX1A                                                      
         LA    RF,3(RF)            SKIP FLUFF FOR NOW                           
         SH    R0,=H'3'                                                         
         BNP   IUPG060                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,BKSFTNEL,(RF),(R0)                          
*                                                                               
IUPG060  DS    0H                  DO DEMOS                                     
         B     INVDEM                                                           
         EJECT                                                                  
*========================*                                                      
* PROCESS INVENTORY DEMO *                                                      
*========================*                                                      
INVDEM   DS    0H                                                               
         LA    R5,1                DEMO SEQUENCE NUMBER                         
         LA    R2,RFTFDEMS         RATING VALUES                                
         LA    R3,RFTFSHRS         SHARE VALUES                                 
         LA    R6,RFTFLVLS         HUT/PUT LEVEL VALUES                         
*                                                                               
IDEM010  DS    0H                                                               
         OC    0(4,R2),0(R2)       ANY RATING?                                  
         BNZ   IDEM020             YES                                          
         OC    0(4,R3),0(R3)       ANY SHARE?                                   
         BNZ   IDEM020             YES                                          
         OC    0(4,R6),0(R6)       ANY HUT/PUT LEVEL?                           
         BNZ   IDEM020             YES                                          
         B     IDEM050             DON'T SEND THIS DEMO                         
*                                                                               
IDEM020  DS    0H                                                               
*                                  ADD NEW DEMO ELEMENT                         
         GOTO1 ASETELEM,DMCB,AFABLK,DEMDATA,0                                   
*                                                                               
         STC   R5,BYTE             SET SEQUENCE NUMBER                          
         GOTO1 AADDDATA,DMCB,AFABLK,DEMSEQEL,BYTE,0                             
*                                                                               
         OC    0(4,R2),0(R2)       ANY RATING?                                  
         BZ    IDEM030             NO                                           
         GOTO1 AADDDATA,DMCB,AFABLK,DEMRTGEL,(R2),0                             
*                                                                               
IDEM030  DS    0H                                                               
         OC    0(4,R3),0(R3)       ANY SHARE?                                   
         BZ    IDEM040             NO                                           
*DHAB    GOTO1 AADDDATA,DMCB,AFABLK,DEMSHREL,(R3),0                             
*                                                                               
IDEM040  DS    0H                                                               
         OC    0(4,R6),0(R6)       ANY HUT/PUT LEVEL?                           
         BZ    IDEM050             NO                                           
*DHAB    GOTO1 AADDDATA,DMCB,AFABLK,DEMLVLEL,(R6),0                             
*                                                                               
IDEM050  DS    0H                                                               
         LA    R2,4(R2)                                                         
         LA    R3,4(R3)                                                         
         LA    R6,4(R6)                                                         
*                                                                               
         LA    R5,1(R5)                                                         
         CLM   R5,1,NUMDEMS                                                     
         BNH   IDEM010                                                          
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* VALIDATE HEADER DOWNLOAD                                                      
**********************************************************************          
VHDRDWN  NTR1  BASE=*,LABEL=*                                                   
         TM    MISCFLG1,MF1BKS                                                  
         BZ    VHDWN002                                                         
*                                                                               
         BAS   RE,VBKSDWN          OUTPUT VALIDATED BKS,DEMOS,UPGS              
*                                                                               
VHDWN002 DS    0H                                                               
         GOTO1 VCOLY,DMCB,(X'10',0),(0,0)                                       
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADDR2,DMCB                                                       
*                                                                               
         L     R6,ATWA                                                          
         AHI   R6,(SVPARMBF-T82AFFD)                                            
         USING VHPARMD,R6                                                       
*                                                                               
         LA    R0,VHPARMLQ(R6)     A(OUTPUT)                                    
         ST    R0,ADDR                                                          
*                                                                               
         CLC   VHPSTA,SPACES                                                    
         BNH   VHDWN010                                                         
*                                                                               
         GOTOX (VSTAQ,ADDR2),DMCB,(RC),(0,VHPSTA),ADDR                          
         BNE   VHDWNERR                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,STATBL-(*-4)                                                  
         GOTOR XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
         GOTO1 GSTABKS,DMCB,(0,VHPSTA),ADDR                                     
         BNE   VHDWNERR                                                         
*                                                                               
VHDWN010 DS    0H                                                               
         CLC   VHPADV,SPACES                                                    
         BNH   VHDWN020                                                         
*                                                                               
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
*                                                                               
         GOTOX (VADVQ,ADDR2),DMCB,(RC),(0,VHPADV),ADDR                          
         BNE   VHDWNERR                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,ADVTBL-(*-4)                                                  
         GOTOR XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
VHDWN020 DS    0H                                                               
         CLC   VHPPRD,SPACES                                                    
         BNH   VHDWN030                                                         
*                                                                               
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
*                                                                               
         GOTOX (VPRDQ,ADDR2),DMCB,(RC),(0,VHPPRD),(0,VHPADV),ADDR               
         BNE   VHDWNERR                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,PRDTBL-(*-4)                                                  
         GOTOR XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
VHDWN030 DS    0H                                                               
         CLC   VHPAGY,SPACES                                                    
         BNH   VHDWN040                                                         
*                                                                               
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
*                                                                               
         GOTOX (VAGYQ,ADDR2),DMCB,(RC),(0,VHPAGY),(0,VHPAOF),ADDR               
         BNE   VHDWNERR                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,AGYTBL-(*-4)                                                  
         GOTOR XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
         GOTOX (AGYADDRQ,ADDR2),DMCB,(RC),(0,VHPAGY),(0,VHPAOF),       +        
               (0,VHPADV),(0,VHPCTY),ADDR                                       
         BNE   VHDWN040                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,AGADTBL-(*-4)                                                 
         GOTOR XARSE,DMCB,ADDR,(R0)                                             
         BNE   VHDWN040                                                         
*                                                                               
VHDWN040 DS    0H                                                               
         OC    VHPFLS,VHPFLS                                                    
         BZ    VHDWN050                                                         
*                                                                               
         GOTOX (VFLIGHTQ,ADDR2),DMCB,(RC),(0,VHPFLS),(0,VHPFLE),       +        
               (0,VHPSTA),ADDR                                                  
         BNE   VHDWNERR                                                         
*                                                                               
VHDWN050 DS    0H                                                               
         CLC   VHPDSP,SPACES                                                    
         BNH   VHDWN060                                                         
*                                                                               
         GOTOX (VDEVSALQ,ADDR2),DMCB,(RC),(0,VHPDSP),ADDR                       
         BNE   VHDWNERR                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,DSPTBL-(*-4)                                                  
         GOTOR XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
VHDWN060 DS    0H                                                               
         CLC   VHPDCT,SPACES                                                    
         BNH   VHDWN070                                                         
*                                                                               
         GOTOX (VDEVTYPQ,ADDR2),DMCB,(RC),(0,VHPDCT),ADDR                       
         BNE   VHDWNERR                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,DCTTBL-(*-4)                                                  
         GOTOR XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
VHDWN070 DS    0H                                                               
         CLC   VHPSAL,SPACES                                                    
         BNH   VHDWN080                                                         
*                                                                               
         GOTOX (VSALQ,ADDR2),DMCB,(RC),(0,VHPSAL),ADDR                          
         BNE   VHDWNERR                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,SALTBL-(*-4)                                                  
         GOTOR XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
VHDWN080 DS    0H                                                               
*                                                                               
VHDWNX   DS    0H                                                               
         B     EXITOK                                                           
         DROP  R6                                                               
*                                                                               
VHDWNERR DS    0H                                                               
         L     RE,ADDR                                                          
         MVC   ERROR,0(RE)                                                      
         B     EXITL                                                            
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* VALIDATE BOOKS/UPGRADES/DEMOS DOWNLOAD                                        
**********************************************************************          
VBKSDWN  NTR1  BASE=*,LABEL=*                                                   
         L     R6,ADDR                                                          
         AHI   R6,VHPARMLQ                                                      
         A     R6,ATWA                                                          
*                                                                               
         SR    R2,R2               UPGRADE SEQ# COUNT                           
         SR    R4,R4               UPGRADE NAME COUNT                           
VBDWN010 DS    0H                                                               
         OC    0(3,R6),0(R6)       END OF DATA?                                 
         BZ    VBDWN200            YES                                          
*                                                                               
         CLC   =Y(VBINBKQ),0(R6)   INVENTORY BOOK?                              
         BE    *+14                YES                                          
         CLC   =Y(VBBOOKQ),0(R6)   BOOK?                                        
         BNE   VBDWN020            NO                                           
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,BKSDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         CLC   =Y(VBINBKQ),0(R6)   INVENTORY BOOK?                              
         BNE   VBDWN012            NO                                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BKSINVEL,0,0                                
*                                                                               
VBDWN012 DS    0H                                                               
         ZIC   RF,2(R6)                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,BKSNAMEL,3(R6),(RF)                         
         B     VBDWN100                                                         
*                                                                               
VBDWN020 DS    0H                                                               
         CLC   =Y(VBDEMOQ),0(R6)   DEMO?                                        
         BNE   VBDWN030            NO                                           
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,DEMDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         ZIC   RF,2(R6)                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,DEMNAMEL,3(R6),(RF)                         
         B     VBDWN100                                                         
*                                                                               
VBDWN030 DS    0H                                                               
         CLC   =Y(VBUPGRDQ),0(R6)  INVENTORY UPGRADE?                           
         BE    *+14                YES                                          
         CLC   =Y(VBNIUPGQ),0(R6)  UPGRADE?                                     
         BNE   VBDWN040            NO                                           
*                                                                               
         LA    R2,1(R2)                                                         
         GOTO1 ASETELEM,DMCB,AFABLK,BKSDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         CLC   =Y(VBUPGRDQ),0(R6)  INVENTORY UPGRADE?                           
         BNE   VBDWN032            NO                                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BKSINVEL,0,0                                
*                                                                               
VBDWN032 DS    0H                                                               
         STC   R2,BYTE                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,BKSUPGEL,BYTE,0                             
         B     VBDWN100                                                         
*                                                                               
VBDWN040 DS    0H                                                               
         CLC   =Y(VBUPGNMQ),0(R6)  UPGRADE NAME?                                
         BNE   VBDWN050            NO                                           
*                                                                               
         LA    R4,1(R4)                                                         
****     GOTO1 ASETELEM,DMCB,AFABLK,BKSDATA,0                                   
****     OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         STC   R4,BYTE                                                          
****     GOTO1 AADDDATA,DMCB,AFABLK,BKSUPGEL,BYTE,0                             
*                                                                               
         ZIC   RF,2(R6)                                                         
****     GOTO1 AADDDATA,DMCB,AFABLK,BKSNAMEL,3(R6),(RF)                         
         B     VBDWN100                                                         
*                                                                               
VBDWN050 DS    0H                                                               
*                                                                               
VBDWN100 DS    0H                                                               
         ZIC   RF,2(R6)                                                         
         LA    R6,3(RF,R6)                                                      
         B     VBDWN010                                                         
*                                                                               
VBDWN200 DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* REFRESH CONTRACT DOWNLOAD                                                     
**********************************************************************          
DCONDWN  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VCOLY,DMCB,(X'10',0),(0,0)                                       
         CLI   DMCB+4,X'FF'                                                     
         JNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADDR2,DMCB                                                       
*                                                                               
         L     R6,ATWA                                                          
         AHI   R6,(SVPARMBF-T82AFFD)                                            
         ZIC   R2,0(R6)            NUMBER OF CONTRACTS                          
         LA    R3,1(R6)            START OF CONTRACTS                           
         LR    R6,R2                                                            
         MHI   R6,VHSPRMLQ                                                      
         LA    R6,0(R6,R3)         BUMP PAST CONTRACTS                          
         LA    R0,VHPARMLQ(R6)     A(OUTPUT)                                    
         ST    R0,ADDR                                                          
*                                                                               
         USING VHPARMD,R6                                                       
DCDWN010 DS    0H                                                               
         XC    VHPARMD(VHPARMLQ),VHPARMD                                        
         MVC   VHPCON,0(R3)                                                     
*                                                                               
         BAS   RE,DCONFOO                                                       
         BNE   EXITL                                                            
*                                                                               
         LA    R3,VHSPRMLQ(R3)                                                  
         BCT   R2,DCDWN010                                                      
         DROP  R6                                                               
*                                                                               
         B     EXITOK                                                           
*                                                                               
*---------------------------------------------------------------------          
* DO THE WORK OF DOWNLOADING A CONTRACT                                         
*    R6       POINTS TO VHPARMS WITH CON# FILLED IN                             
*    ADDR     POINTS TO USABLE OUTPUT AREA                                      
*    ADDR2    POINTS TO T82A10 - THE UTILITY ROUTINE                            
*---------------------------------------------------------------------          
DCONFOO  NTR1  BASE=*,LABEL=*                                                   
         USING VHPARMD,R6                                                       
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),VHPCON                                                
         MVO   WORK(5),WORK+10(5) CONTRACT NUM IN 9'S COMPLEMENT                
*                                                                               
         XC    KEY,KEY                                                          
K        USING RCONPTYP,KEY                                                     
         MVI   K.RCONPTYP,X'8C'                                                 
         MVC   K.RCONPREP,REPALPHA                                              
         MVC   K.RCONPCON,WORK                                                  
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RCONKEY),KEYSAVE                                           
         BE    *+14                                                             
         MVC   ERROR,=Y(82)                                                     
         B     EXITL                                                            
*                                                                               
         MVI   UPDATE,C'Y'                                                      
         GOTO1 VGETREC,AIOREC                                                   
         MVI   UPDATE,C'Y'                                                      
*                                                                               
         L     R8,AIOREC                                                        
         USING RCONREC,R8                                                       
*                                                                               
*        BAS   RE,CONACTIV         UPDATE DOWNLOAD ACTIVITY                     
*                                                                               
         MVC   VHPSTA,RCONKSTA                                                  
         MVC   VHPADV,RCONKADV                                                  
         MVC   VHPPRD,RCONPRD                                                   
         MVC   VHPAGY,RCONKAGY                                                  
         MVC   VHPAOF,RCONKAOF                                                  
         GOTO1 VDATCON,DMCB,(3,RCONDATE),(19,VHPFLS)                            
         GOTO1 VDATCON,DMCB,(3,RCONDATE+3),(19,VHPFLE)                          
         MVC   VHPSAL,RCONSAL                                                   
         MVC   VHPCTY,RCONTYPE                                                  
         MVC   VHPBUYER,RCONBUYR                                                
         DROP  R8                                                               
*                                                                               
         CLC   VHPPRD,SPACES       PRODUCT GIVEN?                               
         BH    DCON0002            YES - DON'T LOOK FOR FREE FORM               
*                                                                               
         MVI   ELCODE,X'05'        GET PRODUCT ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   DCON0002                                                         
*                                                                               
         USING RCONEXEL,R8                                                      
         MVC   VHPFFPRD,RCONEXPR                                                
         DROP  R8                                                               
*                                                                               
DCON0002 DS    0H                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'18'        DEVELOPMENT INFO ELEMENT                     
         BAS   RE,GETEL                                                         
         BNE   DCON0005                                                         
*                                                                               
         USING RCONDVEL,R8                                                      
         MVC   VHPDCT,RCONDVCT                                                  
         MVC   VHPDSP,RCONDVSP                                                  
         DROP  R8                                                               
*                                                                               
DCON0005 DS    0H                                                               
*                                  ADD NEW CONTRACT ELEMENT                     
         GOTO1 ASETELEM,DMCB,AFABLK,CONDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONNUMEL,VHPCON,0                           
*                                                                               
* EI CODES                                                                      
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'A2'        EASI CODE ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   DCON0010                                                         
         USING RCONIEL,R8                                                       
         GOTO1 AADDDATA,DMCB,AFABLK,CONEADEL,RCONIADV,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONEPDEL,RCONIPRD,0                         
         CLC   RCONIEST,SPACES                                                  
         BNH   DCON0008                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONEESEL,RCONIEST,0                         
         B     DCON0010                                                         
*                                                                               
DCON0008 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONEESEL,RCONXEST,0                         
         DROP  R8                                                               
*                                                                               
* TRAFFIC NUMBER                                                                
*                                                                               
DCON0010 DS    0H                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'1F'        LOOK FOR TRAFFIC NUMBER                      
         BAS   RE,GETEL                                                         
         BNE   DCON0011                                                         
         USING RCONXEL,R8                                                       
         GOTO1 AADDDATA,DMCB,AFABLK,CONTNMEL,RCONTRF,0                          
         DROP  R8                                                               
*                                                                               
DCON0011 DS    0H                                                               
         TM    MISCFLG1,MF1BUYS+MF1INVS                                         
         BNZ   DCON0100                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONFLSEL,VHPFLS,0                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONFLEEL,VHPFLE,0                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,CONBYREL,VHPBUYER,0                         
*                                                                               
* GET DEMOS FROM PENDING ELEMENT                                                
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'12'        SAR ELEMENT                                  
         BAS   RE,GETEL                                                         
         BNE   DCON0014                                                         
*                                                                               
         LA    R2,7                NUMBER OF DEMOS                              
         LA    R8,RSARXDEM-RSARXEL(R8)                                          
*                                                                               
DCON0012 DS    0H                                                               
         OC    0(3,R8),0(R8)                                                    
         BZ    DCON0014                                                         
*                                                                               
         L     RE,AIO2                                                          
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'INV'                                                   
         DROP  RE                                                               
*                                                                               
         XC    WORK+60(50),WORK+60                                              
         MVC   WORK+60(3),0(R8)                                                 
         CLI   WORK+60+1,C'T'      FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   WORK+60+1,C'I'                                                   
*SAP                                                                            
         CLC   VERSION,=XL4'0200000C'                                           
         BNL   *+8                                                              
         MVI   WORK+60,0           NO PRIMARY                                   
*                                                                               
         GOTO1 VDEMOCON,DMCB,(1,WORK+60),(9,WORK),(0,AIO2)                      
         ZIC   R0,0(R1)                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONDEMEL,WORK,(R0)                          
*                                                                               
         LA    R8,3(R8)                                                         
         BCT   R2,DCON0012                                                      
*                                                                               
DCON0014 DS    0H                                                               
*                                                                               
* GET DEMOS FROM AGENCY DEMO ELEMENT                                            
*                                                                               
         CLC   VERSION,=XL4'02000018'                                           
         BL    DCON0018                                                         
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'DD'        AGENCY DEMO ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   DCON0018                                                         
*                                                                               
         CLI   1(R8),18            OLD SYTLE ELEMENT                            
         BE    DCON0018            YES - SKIP                                   
*                                                                               
         ZIC   R2,1(R8)                                                         
         AR    R2,R8               END OF DEMO ELEMENT                          
*                                                                               
         LA    R8,2(R8)                                                         
*                                                                               
DCON0016 DS    0H                                                               
         OC    0(3,R8),0(R8)                                                    
         BZ    DCON0018                                                         
*                                                                               
         CLC   0(3,R8),=C'   '                                                  
         BE    DCON0018                                                         
*                                                                               
         CLI   0(R8),C'('                                                       
         BE    DCON0018                                                         
*                                                                               
         L     RE,AIO2                                                          
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'INV'                                                   
         DROP  RE                                                               
*                                                                               
         XC    WORK+60(50),WORK+60                                              
         MVC   WORK+60(3),0(R8)                                                 
         CLI   WORK+60+1,C'T'      FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   WORK+60+1,C'I'                                                   
*SAP                                                                            
         CLC   VERSION,=XL4'0200000C'                                           
         BNL   *+8                                                              
         MVI   WORK+60,0           NO PRIMARY                                   
*                                                                               
         GOTO1 VDEMOCON,DMCB,(1,WORK+60),(9,WORK),(0,AIO2)                      
         ZIC   R0,0(R1)                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,CONDDMEL,WORK,(R0)                          
*                                                                               
         LA    R8,3(R8)                                                         
         CR    R8,R2                                                            
         BL    DCON0016                                                         
*                                                                               
DCON0018 DS    0H                                                               
         CLC   VHPPRD,SPACES       PRODUCT GIVEN?                               
         BH    DCON0026            YES - USE VALPRD                             
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,PRDDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         LA    R0,L'VHPFFPRD                                                    
         GOTO1 AADDDATA,DMCB,AFABLK,PRDNAMEL,VHPFFPRD,(R0)                      
         B     DCON0028                                                         
*                                                                               
DCON0026 DS    0H                                                               
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
*                                                                               
         GOTOX (VPRDQ,ADDR2),DMCB,(RC),(0,VHPPRD),(0,VHPADV),ADDR               
         BNE   DCONERR                                                          
*                                                                               
         LA    R0,*                                                             
         AHI   R0,PRDTBL-(*-4)                                                  
         GOTOR XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
DCON0028 DS    0H                                                               
         CLC   VHPCTY,SPACES                                                    
         BNH   DCON0030                                                         
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,CTYDATA1,0                                  
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         LA    R0,L'VHPCTY                                                      
         GOTO1 AADDDATA,DMCB,AFABLK,CTYCTYEL,VHPCTY,(R0)                        
*                                                                               
DCON0030 DS    0H                                                               
         GOTOX (VSTAQ,ADDR2),DMCB,(RC),(0,VHPSTA),ADDR                          
         BNE   DCONERR                                                          
*                                                                               
         LA    R0,*                                                             
         AHI   R0,STATBL-(*-4)                                                  
         GOTOR XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
         GOTO1 GSTABKS,DMCB,(0,VHPSTA),ADDR                                     
         BNE   DCONERR                                                          
*                                                                               
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
*                                                                               
         GOTOX (VADVQ,ADDR2),DMCB,(RC),(0,VHPADV),ADDR                          
         BNE   DCONERR                                                          
*                                                                               
         LA    R0,*                                                             
         AHI   R0,ADVTBL-(*-4)                                                  
         GOTOR XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
         L     RE,ADDR                                                          
         XC    0(255,RE),0(RE)                                                  
*                                                                               
         GOTOX (VAGYQ,ADDR2),DMCB,(RC),(0,VHPAGY),(0,VHPAOF),ADDR               
         BNE   DCONERR                                                          
*                                                                               
         LA    R0,*                                                             
         AHI   R0,AGYTBL-(*-4)                                                  
         GOTOR XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
         GOTOX (AGYADDRQ,ADDR2),DMCB,(RC),(0,VHPAGY),(0,VHPAOF),       +        
               (0,VHPADV),(0,VHPCTY),ADDR                                       
         BNE   DCON0040                                                         
*                                                                               
         LA    R0,*                                                             
         AHI   R0,AGADTBL-(*-4)                                                 
         GOTOR XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
DCON0040 DS    0H                                                               
         GOTOX (VSALQ,ADDR2),DMCB,(RC),(0,VHPSAL),ADDR                          
         BE    *+14                                                             
         MVC   ERROR,=Y(154)                                                    
         B     EXITL                                                            
*                                                                               
         LA    R0,*                                                             
         AHI   R0,SALTBL-(*-4)                                                  
         GOTOR XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
         CLC   VHPDSP,SPACES                                                    
         BNH   DCON0050                                                         
*                                                                               
         GOTOX (VDEVSALQ,ADDR2),DMCB,(RC),(0,VHPDSP),ADDR                       
         BNE   DCONERR                                                          
*                                                                               
         LA    R0,*                                                             
         AHI   R0,DSPTBL-(*-4)                                                  
         GOTOR XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
DCON0050 DS    0H                                                               
         CLC   VHPDCT,SPACES                                                    
         BNH   DCON0060                                                         
*                                                                               
         GOTOX (VDEVTYPQ,ADDR2),DMCB,(RC),(0,VHPDCT),ADDR                       
         BNE   DCONERR                                                          
*                                                                               
         LA    R0,*                                                             
         AHI   R0,DCTTBL-(*-4)                                                  
         GOTOR XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
DCON0060 DS    0H                                                               
         EJECT                                                                  
**********************************************************************          
* READ BUYLINES FOR PC                                                          
**********************************************************************          
DCON0100 DS    0H                                                               
         TM    MISCFLG1,MF1BUYS                                                 
         BZ    DCON0200                                                         
*                                                                               
         ZAP   WORK+10(5),VHPCON                                                
         MVO   WORK(5),WORK+10(5) PWOS CONTRACT NUM                             
*                                                                               
         XC    KEY,KEY                                                          
K        USING RBUYKEY,KEY                                                      
         MVI   K.RBUYKTYP,X'0B'                                                 
         MVC   K.RBUYKREP,REPALPHA                                              
         GOTOX (RFCONNUM,VREPFACS),DMCB,(1,WORK),(3,K.RBUYKCON)                 
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
DCON0101 DS    0H                                                               
         CLC   KEY(RBUYKPLN-RBUYKEY),KEYSAVE                                    
         BNE   DCON0200                                                         
*                                                                               
         CLC   K.RBUYKPLN,=X'FFFFFF'                                            
         BNE   DCON0190            SKIP THIS BUY ITS A PLAN                     
         DROP  K                                                                
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         L     R8,AIOREC                                                        
         USING RBUYREC,R8                                                       
*                                                                               
         TM    RBUYFLG2,X'20'+X'10'+X'08'                                       
         BNZ   DCON0190            SKIP THIS BUY,ITS CANCELED                   
*                                                                               
         CLI   RBUYCHGI,C'C'                                                    
         BE    DCON0190            SKIP THIS BUY,ITS CANCELED                   
*                                                                               
         OC    POSTST,POSTST       START DATE FILTER?                           
         BNZ   *+14                YES                                          
         OC    POSTEN,POSTEN       END DATE FILTER?                             
         BZ    DCON0102            NO                                           
*                                                                               
         GOTO1 CHOPBUY,DMCB,(R8)                                                
         BNE   DCON0190            SKIP THIS BUY                                
*                                                                               
DCON0102 DS    0H                                                               
         MVC   BYTE2,RBUYNW        SAVE #/WK FOR 03 ELEMENTS                    
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,BUYDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BUYMLNEL,RBUYKMLN,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,BUYLINEL,RBUYKLIN,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,BUYCOSEL,RBUYCOS,0                          
         GOTO1 AADDDATA,DMCB,AFABLK,BUYTSPEL,RBUYTSPT,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,BUYTCSEL,RBUYTCOS,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,BUYTWKEL,RBUYTWKS,0                         
         ZIC   R1,RBUYSTED                                                      
         SRL   R1,4                                                             
         BCTR  R1,0                                                             
         LA    R0,DAYBITS(R1)                                                   
*DHAB    GOTO1 AADDDATA,DMCB,AFABLK,BUYSDYEL,(R0),0                             
         MVC   BYTE,RBUYSTED                                                    
         NI    BYTE,X'0F'                                                       
         ZIC   R1,BYTE                                                          
         BCTR  R1,0                                                             
         LA    R0,DAYBITS(R1)                                                   
*DHAB    GOTO1 AADDDATA,DMCB,AFABLK,BUYEDYEL,(R0),0                             
*                                                                               
         CLI   RBUYDPT,C' '                                                     
         BNH   DCON0104                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BUYDPTEL,RBUYDPT,0                          
*                                                                               
DCON0104 DS    0H                                                               
         TM    RBUYFLG2,X'80'      DAILY?                                       
         BZ    DCON0106            NO                                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BUYDALEL,0,0                                
*                                                                               
DCON0106 DS    0H                                                               
         TM    RBUYSFG,X'40'       SPORTS BUY?                                  
         BNZ   DCON108A            YES                                          
*                                                                               
         MVC   HALF,RBUYDUR                                                     
         NI    HALF,X'FF'-X'80'                                                 
*                                                                               
         TM    RBUYDUR,X'80'       MINUTES?                                     
         BZ    DCON0108            NO                                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BUYMINEL,0,0                                
DCON0108 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BUYLENEL,HALF,0                             
         DROP  R8                                                               
*                                                                               
DCON108A DS    0H                                                               
         MVC   WORK(3),=X'FFFFFF'  GET ABSOLUTE START AND END                   
         XC    WORK+3(3),WORK+3                                                 
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'03'        EFFECTIVE DATE ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   DCON01XA                                                         
*                                                                               
DCON0109 DS    0H                                                               
         USING RBUYDTEL,R8                                                      
         CLC   RBUYDTST,WORK                                                    
         BH    *+10                                                             
         MVC   WORK(3),RBUYDTST                                                 
*                                                                               
         CLC   RBUYDTED,WORK+3                                                  
         BL    *+10                                                             
         MVC   WORK+3(3),RBUYDTED                                               
         DROP  R8                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    DCON0109                                                         
*                                                                               
DCON01XA DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BUYSDTEL,WORK,0                             
         GOTO1 AADDDATA,DMCB,AFABLK,BUYEDTEL,WORK+3,0                           
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'04'        COMMENT ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   DCON01XC                                                         
*                                                                               
DCON01XB DS    0H                                                               
         ZIC   RF,1(R8)                                                         
         AHI   RF,-3                                                            
         EX    RF,*+4                                                           
         OC    2(0,R8),SPACES                                                   
         LA    RF,1(RF)                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BUYCOMEL,2(R8),(RF)                         
*                                                                               
**JRD    BAS   RE,NEXTEL                                                        
**JRD    BE    DCON01XB                                                         
*                                                                               
DCON01XC DS    0H                                                               
         L     R8,AIOREC                                                        
         CLC   VERSION,=XL4'01020010'                                           
         BL    DCON01XE                                                         
*                                                                               
         MVI   ELCODE,X'21'        PROGRAM ELEMENT                              
         BAS   RE,GETEL                                                         
         BNE   DCON01XE                                                         
*                                                                               
DCON01XD DS    0H                                                               
         ZIC   RF,1(R8)                                                         
         AHI   RF,-3                                                            
         EX    RF,*+4                                                           
         OC    2(0,R8),SPACES                                                   
         LA    RF,1(RF)                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BUYPGMEL,2(R8),(RF)                         
*                                                                               
**JRD    BAS   RE,NEXTEL                                                        
**JRD    BE    DCON01XD                                                         
*                                                                               
DCON01XE DS    0H                                                               
         CLC   VERSION,=XL4'02000018'                                           
         BL    DCON01XX                                                         
*                                                                               
         BRAS  RE,DBUYDEM          DEMOS FOR BUYLINE                            
*                                                                               
DCON01XX DS    0H                                                               
         L     R8,AIOREC                                                        
         USING RBUYREC,R8                                                       
         TM    RBUYSFG,X'40'       SPORTS BUY?                                  
         BNZ   DCON0120            YES - SKIP DAYTIMES                          
         DROP  R8                                                               
*                                                                               
**TEST                                                                          
***      USING RBUYREC,R8                                                       
***      CLI   RBUYKLIN,X'19'                                                   
***      BNE   *+6                                                              
***      DC    H'0'                                                             
***      DROP  R8                                                               
**TEST                                                                          
*                                                                               
         MVI   ELCODE,X'02'        DAY/TIME ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   DCON0120                                                         
*                                                                               
DCON0110 DS    0H                                                               
         USING RBUYDYEL,R8                                                      
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,BDYDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         ZIC   R1,RBUYDYIN                                                      
         SRL   R1,4                                                             
         BCTR  R1,0                                                             
         LA    R0,DAYBITS(R1)                                                   
*DHAB    GOTO1 AADDDATA,DMCB,AFABLK,BDYSDYEL,(R0),0                             
         MVC   BYTE,RBUYDYIN                                                    
         NI    BYTE,X'0F'                                                       
         ZIC   R1,BYTE                                                          
         BCTR  R1,0                                                             
         LA    R0,DAYBITS(R1)                                                   
*DHAB    GOTO1 AADDDATA,DMCB,AFABLK,BDYEDYEL,(R0),0                             
         GOTO1 AADDDATA,DMCB,AFABLK,BDYDAYEL,RBUYDAYS,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,BDYSTMEL,RBUYDYT1,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,BDYETMEL,RBUYDYT2,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,BDYDWTEL,RBUYDYWT,0                         
         DROP  R8                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    DCON0110                                                         
*                                                                               
DCON0120 DS    0H                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'03'        EFFECTIVE DATE ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   DCON0140                                                         
*                                                                               
DCON0130 DS    0H                                                               
         USING RBUYDTEL,R8                                                      
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,BDTDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         GOTO1 VDATCON,DMCB,(3,RBUYDTST),(19,WORK)                              
         GOTO1 AADDDATA,DMCB,AFABLK,BDTSDTEL,WORK,0                             
         GOTO1 VDATCON,DMCB,(3,RBUYDTED),(19,WORK)                              
         GOTO1 AADDDATA,DMCB,AFABLK,BDTEDTEL,WORK,0                             
         GOTO1 AADDDATA,DMCB,AFABLK,BDTWKSEL,RBUYDTWK,0                         
*                                                                               
         TM    RBUYDTIN,X'80'                                                   
         BZ    DCON0131                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BDTEWKEL,0,0                                
*                                                                               
DCON0131 DS    0H                                                               
         TM    RBUYDTIN,X'40'                                                   
         BZ    DCON0132                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BDTAWKEL,0,0                                
*                                                                               
DCON0132 DS    0H                                                               
         TM    RBUYDTIN,X'10'                                                   
         BZ    DCON0133                                                         
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BDTSPWEL,RBUYDTNW,0                         
         B     DCON0134                                                         
*                                                                               
DCON0133 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,BDTSPWEL,BYTE2,0                            
*                                                                               
DCON0134 DS    0H                                                               
         DROP  R8                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
         BE    DCON0130                                                         
*                                                                               
DCON0140 DS    0H                                                               
*                                                                               
DCON0190 DS    0H                                                               
         GOTO1 VSEQ                                                             
         B     DCON0101                                                         
         EJECT                                                                  
**********************************************************************          
* READ INVOICES FOR PC                                                          
**********************************************************************          
DCON0200 DS    0H                                                               
         TM    MISCFLG1,MF1INVS          INVOICES REQUESTED                     
         BZ    DCON0300                                                         
         BRAS  RE,DCON0201               AND SEND INVOICES                      
*                                                                               
DCON0300 DS    0H                                                               
*                                                                               
DCONX    DS    0H                                                               
         B     EXITOK                                                           
         DROP  R6                                                               
*                                                                               
DCONERR  DS    0H                                                               
         L     RE,ADDR                                                          
         MVC   ERROR,0(RE)                                                      
         B     EXITL                                                            
         EJECT                                                                  
DAYBITS  DC    X'40201008040201'   DAYVAL BITS M-SU                             
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
*....................................................................           
* CONACTIV                                                                      
*    R8 -> CONTRACT RECORD                                                      
*                                                                               
*    UPDATE DOWNLOAD ACTIVITY IN THE CONTRACT RECORD                            
*....................................................................           
CONACTIV NTR1  BASE=*,LABEL=*                                                   
         LR    R3,R8                                                            
*                                                                               
         MVI   ELCODE,X'A7'        EZ-POST ACTIVITY ELEMENT                     
         BAS   RE,GETEL                                                         
         BE    CACT0010                                                         
*                                                                               
         XC    WORK,WORK                                                        
E        USING RCONEZEL,WORK                                                    
         MVI   E.RCONEZCD,X'A7'                                                 
         MVI   E.RCONEZLN,RCONEZLQ                                              
         MVI   E.RCONEZNM,0                                                     
         GOTO1 VDATCON,DMCB,(5,0),(19,E.RCONEZ1D)                               
         DROP  E                                                                
*                                                                               
         LR    R8,R3               FIND ELEMENT A HOME                          
         LA    R8,RCONELEM-RCONREC(R8)                                          
CACT0002 CLI   0(R8),X'A7'                                                      
         BH    CACT0004                                                         
         CLI   0(R8),0                                                          
         BE    CACT0004                                                         
         ZIC   RE,1(R8)                                                         
         AR    R8,RE                                                            
         B     CACT0002                                                         
*                                                                               
CACT0004 DS    0H                                                               
         GOTO1 VRECUP,DMCB,(C'R',(R3)),WORK,(R8)                                
*                                                                               
CACT0010 DS    0H                                                               
E        USING RCONEZEL,R8                                                      
         GOTO1 VDATCON,DMCB,(5,0),(19,E.RCONEZLD)                               
         LA    R1,1                                                             
         AH    R1,E.RCONEZNM                                                    
         STH   R1,E.RCONEZNM                                                    
         DROP  E                                                                
*                                                                               
         GOTO1 VPUTREC,AIOREC                                                   
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
*....................................................................           
* CHOPBUY                                                                       
*    P1  - A(BUYLINE)                                                           
*                                                                               
*    CHOP THE BUY LINE POINTED TO BY P1 TO FIT WITHIN THE DATES                 
*    GIVEN IN POSTST AND POSTEN. IF THE ENTIRE BUY IS DELETED                   
*    RETURN CC NOT EQUAL OTHERWISE RETURN EQUAL                                 
*                                                                               
*....................................................................           
CHOPBUY  NTR1  BASE=*,LABEL=*                                                   
         L     R6,0(R1)                                                         
*                                                                               
         NI    MISCFLG1,FF-MF1TMPB1                                             
         XC    FULL,FULL                                                        
         XC    FULL2,FULL2                                                      
*                                                                               
         LR    R8,R6                                                            
         MVI   ELCODE,X'03'        EFFECTIVE DATE ELEMENT                       
         USING RBUYDTEL,R8                                                      
         BAS   RE,GETEL                                                         
         BNE   EXITL                                                            
*                                                                               
CBUY0010 DS    0H                                                               
         CLI   0(R8),0             EOR?                                         
         BE    CBUY0400            YES                                          
         CLI   0(R8),X'03'         EFFECTIVE DATE ELEMENT?                      
         BNE   CBUY0300            NO                                           
*                                                                               
* POST START DATE CHOPPING                                                      
*                                                                               
         OC    POSTST,POSTST       START DATE CHOPPING?                         
         BZ    CBUY0100            NO                                           
*                                                                               
         CLC   RBUYDTED,POSTST     END BEFORE THE POST START?                   
         BL    CBUY0302                                                         
*                                  YES - DELETE ELEMENT                         
         CLC   RBUYDTST,POSTST     START DATE AFTER THE POST START?             
         BNL   CBUY0030            YES - LEAVE DATE ALONE                       
*                                                                               
* START DATE IS ALWAYS A MONDAY.  CHECK BUY START DATE.  IF NOT                 
*        MONDAY, ADJUST BEFFDATE INSERT INTO BUY TO PROVIDE                     
*        OUT-OF-WEEK ROTATOR SUPPORT.                                           
*                                                                               
         GOTO1 VDATCON,DMCB,(3,RBUYDTST),(0,WORK)                               
*                                  ORIGINAL BUY START DATE -> EBCDIC            
         GOTO1 VGETDAY,DMCB,WORK,DUB                                            
         CLI   DMCB,1              ORIGINAL START = MONDAY?                     
         BNE   *+14                NO  - OFFSET TO ORIG BUY START DAY           
         MVC   RBUYDTST,POSTST     YES - USE DATE AS IS                         
         B     CBUY0030                                                         
*                                                                               
         ZIC   R0,DMCB             GET DAY OF WEEK NUMBER                       
         BCTR  R0,0                SUBTRACT 1 DAY                               
*                                                                               
         GOTO1 VDATCON,DMCB,(3,POSTST),(0,WORK)                                 
*                                  CONVERT START DATE TO EBCDIC                 
         GOTO1 VADDAY,DMCB,(C'D',WORK),(0,WORK),(R0)                            
         GOTO1 VDATCON,DMCB,(0,WORK),(3,RBUYDTST)                               
*                                  CONVERT OOWR START TO BINARY                 
         CLC   RBUYDTED,RBUYDTST                                                
         BL    CBUY0302            END BEFORE START:  DROP BUYLINE              
*                                     OOWR IN SINGLE WEEK:  SETS                
*                                     START AFTER END DATE                      
CBUY0030 DS    0H                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* POST END DATE CHOPPING                                                        
*                                                                               
CBUY0100 DS    0H                                                               
         OC    POSTEN,POSTEN       END DATE CHOPPING?                           
         BZ    CBUY0200            NO                                           
*                                                                               
         CLC   RBUYDTST,POSTEN     START AFTER THE POST END?                    
         BH    CBUY0302            YES - DELETE ELEMENT                         
*                                                                               
         CLC   RBUYDTED,POSTEN     END DATE BEFORE THE POST END?                
         BNH   CBUY0130            YES - LEAVE DATE ALONE                       
*                                                                               
* START DATE IS ALWAYS A MONDAY.  CHECK BUY START DATE.  IF NOT                 
*        MONDAY, ADJUST BEFFDATE INSERT INTO BUY TO PROVIDE                     
*        OUT-OF-WEEK ROTATOR SUPPORT.                                           
*                                                                               
         GOTO1 VDATCON,DMCB,(3,RBUYDTED),(0,WORK)                               
*                                  ORIGINAL BUY START DATE -> EBCDIC            
         GOTO1 VGETDAY,DMCB,WORK,DUB                                            
         CLI   DMCB,7              ORIGINAL START = SUNDAY?                     
         BNE   *+14                NO  - OFFSET TO ORIG BUY END DAY             
         MVC   RBUYDTED,POSTEN     YES - USE DATE AS IS                         
         B     CBUY0130                                                         
*                                                                               
         ZIC   R0,DMCB             GET DAY OF WEEK NUMBER                       
         LNR   R0,R0               MAKE IT NEGATIVE                             
         AHI   R0,7                ADD 7                                        
         LNR   R0,R0               MAKE IT NEGATIVE                             
*                                                                               
         GOTO1 VDATCON,DMCB,(3,POSTEN),(0,WORK)                                 
*                                  CONVERT START DATE TO EBCDIC                 
         GOTO1 VADDAY,DMCB,(C'D',WORK),(0,WORK),(R0)                            
         GOTO1 VDATCON,DMCB,(0,WORK),(3,RBUYDTED)                               
*                                  CONVERT OOWR END TO BINARY                   
         CLC   RBUYDTED,RBUYDTST                                                
         BL    CBUY0302            END BEFORE START:  DROP BUYLINE              
*                                     OOWR IN SINGLE WEEK:  SETS                
*                                     START AFTER END DATE                      
CBUY0130 DS    0H                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
CBUY0200 DS    0H                                                               
         GOTO1 VDATCON,DMCB,(3,RBUYDTST),(0,WORK)                               
*                                  EBCDIC START DATE                            
         GOTO1 VDATCON,DMCB,(3,RBUYDTED),(0,WORK+6)                             
*                                  EBCDIC END   DATE                            
         GOTO1 VPERVERT,DMCB,WORK,WORK+6                                        
*                                  CALCULATE DATE RANGE FIGURES                 
         ZICM  RF,DMCB+12,2        GET # WEEKS (DAYS/7)                         
         ZICM  RE,DMCB+10,2        CHECK REMAINDER DAYS/7                       
         LTR   RE,RE                                                            
         BZ    CBUY0220            NO REMAINDER                                 
         LA    RF,1(RF)            REMAINDER:  ADD 1 TO NUM WEEKS               
CBUY0220 EQU   *                                                                
         STC   RF,RBUYDTWK         RESET NUMBER OF WEEKS                        
         TM    RBUYDTIN,X'40'      ALTERNATE WEEK ELEMENT?                      
         BNO   CBUY0240            NO                                           
         TM    RBUYDTWK,X'01'      ODD NUMBER OF WEEKS?                         
*                                     (LOW-ORDER BIT SET?)                      
         BO    CBUY0230            YES - JUST RECALC # WEEKS                    
         GOTO1 VDATCON,DMCB,(3,RBUYDTST),(0,WORK)                               
*                                  CONVERT BUY START DATE TO EBCDIC             
         GOTO1 VADDAY,DMCB,(C'D',WORK),(0,WORK),7                               
*                                  BUMP TO NEXT WEEK                            
         GOTO1 VDATCON,DMCB,(0,WORK),(3,RBUYDTST)                               
*                                  CONVERT NEW START TO BINARY                  
         ZIC   RF,RBUYDTWK         DECREASE WEEKS BY 1                          
         BCTR  RF,0                                                             
         STC   RF,RBUYDTWK                                                      
CBUY0230 EQU   *                   RECALCULATE NUMBER OF WEEKS                  
         ZIC   RF,RBUYDTWK         EXTRACT NUMBER OF WEEKS                      
         LA    RF,1(RF)            MAKE NUMBER OF WEEKS EVEN                    
         SRL   RF,1                DIVIDE NUMBER OF WEEKS BY 2                  
         STC   RF,RBUYDTWK         REPLACE NUMBER OF WEEKS                      
CBUY0240 EQU   *                                                                
         ZIC   RE,RBUYDTWK         GET NUMBER OF WEEKS                          
         A     RE,FULL             CALCULATE TOTAL NUMBER OF WEEKS              
         ST    RE,FULL             SAVE TOTAL NUMBER OF WEEKS                   
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),RBUYDTNW  GET SPOTS/WEEK                               
         ZIC   RE,RBUYDTWK         GET NUMBER OF WEEKS                          
         MH    RE,HALF             NUM WKS * SPTS/WK = TOTAL SPOTS              
         A     RE,FULL2            CALCULATE TOTAL NUMBER SPOTS                 
         ST    RE,FULL2            SAVE TOTAL NUMBER SPOTS                      
*                                                                               
         OI    MISCFLG1,MF1TMPB1   SET 'EFFECTIVE DATE ELEMENTS'                
*                                                                               
CBUY0300 DS    0H                  NEXT ELEMENT                                 
         ZIC   R0,1(R8)                                                         
         AR    R8,R0                                                            
         B     CBUY0010                                                         
*                                                                               
CBUY0302 DS    0H                  DELETE ELEMENT                               
         GOTO1 VRECUP,DMCB,(C'R',(R6)),(R8)                                     
         B     CBUY0010                                                         
*                                                                               
CBUY0400 DS    0H                  DONE WITH RECORD                             
         TM    MISCFLG1,MF1TMPB1   ANY 03'S?                                    
         BZ    EXITL               NO                                           
         DROP  R8                                                               
*                                                                               
         USING RBUYREC,R6                                                       
         MVC   RBUYTSPT,FULL2+2    LOAD TOTAL NUMBER OF SPOTS                   
         MVC   RBUYTWKS,FULL+3     LOAD TOTAL NUMBER OF WEEKS                   
*                                                                               
         SR    RE,RE                                                            
         L     RF,FULL2            LOAD TOTAL # SPOTS                           
         MVC   FULL,RBUYCOS        TOT SPOTS * COST = TOTAL COST                
         M     RE,FULL                                                          
         STCM  RF,15,RBUYTCOS      TOTAL COST OF BUY                            
*                                                                               
         B     EXITOK                                                           
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*....................................................................           
*        DOWNLOAD BUYLINE DEMOS                                                 
*....................................................................           
DBUYDEM  NTR1  BASE=*,LABEL=*                                                   
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'0D'        AGENCY DEMO ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   DBDEM020                                                         
*                                                                               
         CLI   1(R8),X'12'                                                      
         BE    EXITOK              OLD ELEMENT - SKIP                           
*                                                                               
         CLI   1(R8),X'22'                                                      
         BE    EXITOK              OLD ELEMENT - SKIP                           
*                                                                               
         ZIC   R6,1(R8)            ELEMENT LENGTH                               
         AR    R6,R8               END OF ELEMENT                               
*                                                                               
         AHI   R8,2                FIRST DEMO                                   
         USING RBUYDMCV,R8                                                      
DBDEM010 DS    0H                                                               
         CR    R8,R6               PAST END OF ELEMENT?                         
         BNL   DBDEM020            YES                                          
*                                                                               
         OC    RBUYDMCT,RBUYDMCT   NO DEMO CATEGORY                             
         BZ    DBDEM018            SKIP                                         
*                                                                               
         CLI   RBUYDMCT,C'('       USER DEFINED DEMO CATEGORY?                  
         BE    DBDEM018            SKIP                                         
*                                                                               
         CLC   RBUYDMDM,=X'FFFFFFFF'                                            
         BE    DBDEM018            NO VALUE SKIP                                
*                                                                               
         BRAS  RE,DBUNDEM                                                       
*                                                                               
         L     RF,FULL                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,BUYADEMO,WORK,(RF)                          
*                                                                               
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RBUYDMDM                                                 
         GOTO1 AADDDATA,DMCB,AFABLK,BUYPRTG,WORK,0                              
*                                                                               
         CLC   RBUYDM2M,=X'FFFFFFFF'                                            
         BE    DBDEM012            NO VALUE PREVIOUS VALUE                      
*                                                                               
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RBUYDM2M                                                 
         GOTO1 AADDDATA,DMCB,AFABLK,BUYPPRTG,WORK,0                             
*                                                                               
DBDEM012 DS    0H                                                               
*                                                                               
DBDEM018 DS    0H                                                               
         AHI   R8,L'RBUYDMCV                                                    
         B     DBDEM010                                                         
         DROP  R8                                                               
*                                                                               
DBDEM020 DS    0H                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'0E'        REP DEMO ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   DBDEM030                                                         
*                                                                               
         ZIC   R6,1(R8)            ELEMENT LENGTH                               
         AR    R6,R8               END OF ELEMENT                               
*                                                                               
         AHI   R8,2                FIRST DEMO                                   
         USING RBUYDMCV,R8                                                      
DBDEM022 DS    0H                                                               
         CR    R8,R6               PAST END OF ELEMENT?                         
         BNL   DBDEM030            YES                                          
*                                                                               
         CLC   RBUYDMDM,=X'FFFFFFFF'                                            
         BE    DBDEM028            NO VALUE SKIP                                
*                                                                               
         BRAS  RE,DBUNDEM                                                       
*                                                                               
         L     RF,FULL                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,BUYRDEMO,WORK,(RF)                          
*                                                                               
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RBUYDMDM                                                 
         GOTO1 AADDDATA,DMCB,AFABLK,BUYPRTG,WORK,0                              
*                                                                               
         CLC   RBUYDM2M,=X'FFFFFFFF'                                            
         BE    DBDEM024            NO VALUE PREVIOUS VALUE                      
*                                                                               
         ZAP   WORK(5),=P'0'                                                    
         MVO   WORK(5),RBUYDM2M                                                 
         GOTO1 AADDDATA,DMCB,AFABLK,BUYPPRTG,WORK,0                             
*                                                                               
DBDEM024 DS    0H                                                               
*                                                                               
DBDEM028 DS    0H                                                               
         AHI   R8,L'RBUYDMCV                                                    
         B     DBDEM022                                                         
         DROP  R8                                                               
*                                                                               
DBDEM030 DS    0H                                                               
*                                                                               
         B     EXITOK                                                           
*-------------------------------------------------------------                  
DBUNDEM  NTR1                                                                   
         L     RE,AIO4                                                          
         USING DBLOCKD,RE                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         MVC   DBFILE,=C'INV'                                                   
         DROP  RE                                                               
*                                                                               
         XC    WORK,WORK           CONSTRUCT FAKE FIELD                         
         MVC   WORK+60(3),0(R8)                                                 
         CLI   WORK+60+1,C'T'      FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   WORK+60+1,C'I'                                                   
*                                                                               
         GOTO1 VDEMOCON,DMCB,(1,WORK+60),(9,WORK),(0,AIO4)                      
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL+3(1),0(R1)                                                  
*                                                                               
         B     EXITOK                                                           
         LTORG                                                                  
         EJECT                                                                  
*....................................................................           
*        SEND INVOICES -                                                        
*....................................................................           
DCON0201 NTR1  BASE=*,LABEL=*                                                   
         USING VHPARMD,R6                                                       
         ZAP   WORK+10(5),=P'99999999'   SET CONTRACT IN WORK                   
         SP    WORK+10(5),VHPCON                                                
         MVO   WORK(5),WORK+10(5)        NUM IN 9'S COMPLEMENT                  
*                                                                               
         MVC   BMOSFFND,=X'FFFF'   COMPLEMENT - FFFF=END MONTH DFLT             
         MVC   BMOSFFST,=X'0000'   COMPLEMENT - 0000=STRT MONTH                 
*                                                                               
         OC    POSTST,POSTST       WAS A START DATE REQUESTED                   
         BZ    DCON0204                                                         
         MVC   WORK+40(3),POSTST                                                
         MVI   WORK+42,X'01'       FORCE TO FIRST OF MONTH                      
         GOTO1 VDATCON,DMCB,(3,WORK+40),(2,BMOSFFND)                            
         XC    BMOSFFND,=X'FFFF'   COMPLEMENTED START = END                     
*                                                                               
DCON0204 OC    POSTEN,POSTEN       WAS AN END DATE REQUESTED                    
         BZ    DCON0210                                                         
         MVC   WORK+40(3),POSTEN                                                
         MVI   WORK+42,X'01'       FORCE TO FIRST OF MONTH                      
         GOTO1 VDATCON,DMCB,(3,WORK+40),(2,BMOSFFST)                            
         XC    BMOSFFST,=X'FFFF'   COMPLEMENTED END = START                     
*                                                                               
DCON0210 GOTO1 VSWITCH,DMCB,=C'SPOT',0                                          
         CLI   4(R1),2             SYSTEM NOT OPEN                              
         BE    DCON02XX            ????                                         
         CLI   4(R1),1                                                          
         BE    DCON02XX            ????                                         
*                                                                               
         XC    KEY,KEY                                                          
K        USING SNVRKEY,KEY                                                      
         MVI   K.SNVRTYP,X'0E'                                                  
         MVI   K.SNVRSUB,X'A3'                                                  
         MVC   K.SNVREP,REPALPHA                                                
*                                                                               
* TEMPORARY HARD CODING FOR SELNY STATION WHAM-T CALL LETTER SWITCH             
*                                                                               
         MVC   K.SNVRSTA,VHPSTA                                                 
         CLI   K.SNVRSTA+4,C' '                                                 
         BH    *+8                                                              
         MVI   K.SNVRSTA+4,C'T'                                                 
         MVC   K.SNVRMOS,BMOSFFST  START MONTH                                  
         MVC   K.SNVRCON,WORK      CONTRACT #                                   
*                                                                               
         CLC   REPALPHA,=C'SZ'                                                  
         BNE   DCON0212                                                         
*                                                                               
         CLC   =C'WHAM',K.SNVRSTA                                               
         BNE   DCON0212                                                         
*                                                                               
         CLC   K.SNVRMOS,=X'2DBE'  FEB/01/05 - CUT OFF DATE                     
         BNH   DCON0212            ON OR AFTER FEB01/05 - WHAM                  
         MVC   K.SNVRSTA(4),=C'WOKR'  BEFORE FEB01/05 - WOKR                    
*                                                                               
* END OF  TEMPORARY HARD CODING                                                 
*                                                                               
*                                                                               
DCON0212 MVC   KEYSAVE,KEY                                                      
*                                                                               
         GOTO1 VDMGR,DMCB,=C'DMRDHI',=C'XSPDIR',KEY,KEY,0                       
         B     DCON0224                                                         
*                                                                               
DCON0220 GOTO1 VDMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',KEY,KEY,0                       
*                                                                               
DCON0224 CLC   KEY(9),KEYSAVE      MUST MATCH THRU STATION                      
         BNE   DCON0299                                                         
*                                                                               
         CLC   K.SNVRCON,WORK      MATCH CONTRACT                               
         BNE   DCON0220            TRY NEXT                                     
*                                                                               
         CLC   K.SNVRMOS,BMOSFFST  MUST BE AFTER START                          
         BL    DCON0220            GET NEXT                                     
         CLC   K.SNVRMOS,BMOSFFND  MUST BE BEFORE END                           
         BH    DCON0299            DONE                                         
         DROP  K,R6                                                             
*                                                                               
         GOTO1 VDMGR,DMCB,=C'GETREC',=C'XSPFIL',KEY+36,AIOREC,DMWORK            
*                                                                               
         L     R6,AIOREC                                                        
         LA    R6,42(R6)                                                        
*                                                                               
DCON0230 CLI   0(R6),0                                                          
         BE    DCON0220            TRY NEXT RECORD                              
         CLI   0(R6),X'10'         HEADER ELEM                                  
         BE    DCON0234                                                         
         CLI   0(R6),X'40'         DETAIL ELEM                                  
         BE    DCON0236                                                         
DCON0232 ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DCON0230                                                         
*                                                                               
         USING SNVHDELD,R6                                                      
DCON0234 MVC   SVHDCTL,SNVHDCTL    SAVE CTL BYTE AND START DATE                 
         GOTO1 VDATCON,DMCB,(2,SNVHDSDT),(0,PERSTART)                           
         B     DCON0232            NEXT ELEM                                    
*                                                                               
         USING SNVIDELD,R6                                                      
DCON0236 DS    0H                                                               
         ZIC   R3,SNVIDDAY         CALCULATE DATE                               
         GOTO1 VADDAY,DMCB,PERSTART,WORK+40,(R3)                                
         GOTO1 VDATCON,DMCB,(0,WORK+40),(3,WORK+50)                             
         OC    POSTST,POSTST       START DATE ?                                 
         BZ    *+14                                                             
         CLC   POSTST,WORK+50                                                   
         BH    DCON0232            GET NEXT ELEM                                
*                                                                               
         OC    POSTEN,POSTEN       END DATE?                                    
         BZ    *+14                                                             
         CLC   POSTEN,WORK+50                                                   
         BL    DCON0232            GET NEXT RECORD                              
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,INVDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,INVDATE,WORK+50,0                           
*                                                                               
         SR    R0,R0                                                            
         ZICM  R1,SNVIDTIM,2                                                    
         D     R0,=F'60'           CONVERT THE TIME TO MILITARY                 
         MH    R1,=H'100'                                                       
         AR    R1,R0                                                            
         AH    R1,=H'600'          THE TIME IS DISPLACEMENT FROM 6:00A          
         CH    R1,=H'2400'         EARLY MORNING?                               
         BL    *+8                                                              
         SH    R1,=H'2400'         YES                                          
         STH   R1,HALF                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,INVTIME,HALF,0                              
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,INVLEN,SNVIDSLN,0                           
         GOTO1 AADDDATA,DMCB,AFABLK,INVCOST,SNVIDCST,0                          
*                                                                               
         BAS   RE,GETFLMCD         GET FILM CODE                                
*                                                                               
         B     DCON0232            NEXT ELEM                                    
*                                                                               
DCON0299 GOTO1 VSWITCH,DMCB,=C'REP ',0                                          
         CLI   4(R1),2             SYSTEM NOT OPEN                              
         BE    DCON02XX            ????                                         
         CLI   4(R1),1                                                          
         BE    DCON02XX            ????                                         
*                                                                               
DCON02XX XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* GET FILM CODE                                                                 
* R6 HAS X'40' DETAIL INVOICE ELEMENT                                           
**********************************************************************          
GETFLMCD NTR1  BASE=*,LABEL=*                                                   
         USING SNVIDELD,R6                                                      
         L     R4,AIOREC                                                        
         LA    R4,42(R4)                                                        
         USING SNVCMELD,R4                                                      
*                                                                               
GETFC10  CLI   0(R4),0                                                          
         BE    GETFCX                                                           
         CLI   0(R4),X'30'         COMMERCIAL CODE ELEM                         
         BNE   GETFC20                                                          
         CLC   SNVCMICD,SNVIDCML                                                
         BE    GETFC30                                                          
GETFC20  ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     GETFC10                                                          
         DROP  R6                                                               
*                                                                               
GETFC30  DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,INVISCI,SNVCMCD,0                           
GETFCX   DS    0H                                                               
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
**********************************************************************          
* LIST INVOICE DOWNLOAD                                                         
**********************************************************************          
LINVDWN  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VCOLY,DMCB,(X'10',0),(0,0)                                       
         CLI   DMCB+4,X'FF'                                                     
         JNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADDR2,DMCB                                                       
*                                                                               
         LH    R4,=Y(SVPARMBF-T82AFFD)                                          
         A     R4,ATWA                                                          
         USING VHPARMD,R4                                                       
         MVC   BMOSFFND,=X'FFFF'   COMPLEMENT - FFFF=END MONTH DFLT             
         MVC   BMOSFFST,=X'0000'   COMPLEMENT - 0000=STRT MONTH                 
*                                                                               
         OC    VHPSAL,VHPSAL       VALIDATE SALESPERSON?                        
         BZ    LINV0100            NO                                           
*                                                                               
         GOTOX (VSALQ,ADDR2),DMCB,(RC),(0,VHPSAL),VHPARMLQ(R4)                  
         BE    *+14                                                             
         MVC   ERROR,=Y(154)                                                    
         B     EXITL                                                            
*                                                                               
         GOTOR GETSAL,DMCB,(R4)    GET SALES NAME                               
*                                                                               
LINV0100 OC    VHPFLS,VHPFLS       WAS A START DATE REQUESTED                   
         BZ    LINV0204                                                         
*        GOTO1 VDATCON,DMCB,(8,VHPFLS),(3,WORK+40)                              
*                                                                               
* FORCE TO START OF BROADCAST MONTH                                             
*                                                                               
         GOTO1 VDATCON,DMCB,(8,VHPFLS),(0,WORK+50)                              
         GOTO1 VGTBROAD,DMCB,(1,WORK+50),WORK+60,VGETDAY,VADDAY                 
         GOTO1 VDATCON,DMCB,(0,WORK+60),(2,BMOSFFND)                            
         XC    BMOSFFND,=X'FFFF'   COMPLEMENTED START = END                     
*                                                                               
LINV0204 OC    VHPFLE,VHPFLE       WAS AN END DATE REQUESTED                    
         BZ    LINV0210                                                         
         GOTO1 VDATCON,DMCB,(8,VHPFLE),(3,WORK+40)                              
*        MVI   WORK+42,X'01'       FORCE TO FIRST OF MONTH                      
         GOTO1 VDATCON,DMCB,(3,WORK+40),(2,BMOSFFST)                            
         XC    BMOSFFST,=X'FFFF'   COMPLEMENTED END = START                     
*                                                                               
LINV0210 GOTO1 VSWITCH,DMCB,=C'SPOT',0                                          
         CLI   4(R1),0             SUCCESSFUL?                                  
         BNE   LINV02XX            ????                                         
*                                                                               
         XC    KEY,KEY                                                          
K        USING SNVLKEY,KEY                                                      
         MVI   K.SNVLTYP,X'0E'                                                  
         MVI   K.SNVLSUB,X'B3'     LOCAL REP INVOICE KEY                        
         MVC   K.SNVLID,USRID                                                   
         MVC   K.SNVLSTA,VHPSTA                                                 
*                                                                               
         CLI   K.SNVLSTA+4,C' '                                                 
         BH    *+8                                                              
         MVI   K.SNVRSTA+4,C'T'                                                 
*                                                                               
         CLI   K.SNVLSTA+4,C'L'                                                 
         BH    *+8                                                              
         MVI   K.SNVRSTA+4,C'T'                                                 
*                                                                               
         MVC   K.SNVRMOS,BMOSFFST  START MONTH                                  
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         GOTOR XSPDHIGH                                                         
         B     LINV0224                                                         
*                                                                               
LINV0220 GOTOR XSPDSEQ                                                          
*                                                                               
LINV0224 CLC   KEY(9),KEYSAVE      MUST MATCH THRU STATION                      
         BNE   LINV0299                                                         
*                                                                               
         CLC   K.SNVLPOW,REPALPHA  MATCH ON REP CODE                            
         BNE   LINV0220            NEXT                                         
         CLC   K.SNVRMOS,BMOSFFST  MUST BE AFTER START                          
         BL    LINV0220            GET NEXT                                     
         CLC   K.SNVRMOS,BMOSFFND  MUST BE BEFORE END                           
         BH    LINV0299            DONE                                         
*                                                                               
         GOTO1 VDMGR,DMCB,=C'GETREC',=C'XSPFIL',KEY+36,AIO4,DMWORK              
*                                                                               
         L     R6,AIO4                                                          
         MVI   ELCODE,SNVRIELQ                                                  
         BAS   RE,GETEL2                                                        
         BNE   LINV0220                                                         
*                                                                               
LINV0228 MVC   WORK2(L'SNVRISNM),SNVRISNM-SNVRINVD(R6)                          
         OC    WORK2(L'SNVRISNM),SPACES                                         
         CLC   TWASALN(SALNQ),WORK2                                             
         BNE   LINV0220            NEXT RECORD                                  
*                                                                               
LINV0236 DS    0H                                                               
         L     R6,AIO4                                                          
         LA    R6,42(R6)                                                        
*                                                                               
K        USING SNVLKEY,KEY                                                      
         GOTO1 ASETELEM,DMCB,AFABLK,LIVDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
         GOTO1 AADDDATA,DMCB,AFABLK,LIVSTOEL,K.SNVLSORD,0                       
         DROP  K                                                                
*                                                                               
LINV0240 CLI   0(R6),0                                                          
         BE    LINV0220            TRY NEXT RECORD                              
         CLI   0(R6),SNVHDELQ      HEADER ELEM                                  
         BE    LINV0244                                                         
         CLI   0(R6),SNVRIELQ      INVOICE ELEM                                 
         BE    LINV0254                                                         
LINV0242 ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     LINV0240                                                         
*                                                                               
         USING SNVHDELD,R6                                                      
LINV0244 DS    0H                                                               
         GOTO1 VDATCON,DMCB,(2,SNVHDSDT),(19,WORK+50)                           
         GOTO1 AADDDATA,DMCB,AFABLK,LIVFLSEL,WORK+50,0                          
         GOTO1 VDATCON,DMCB,(2,SNVHDEDT),(19,WORK+50)                           
         GOTO1 AADDDATA,DMCB,AFABLK,LIVFLEEL,WORK+50,0                          
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,LIVTCSEL,SNVHDTCS,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,LIVTSPEL,SNVHDTSP,0                         
         B     LINV0242            NEXT ELEM                                    
*                                                                               
         USING SNVRINVD,R6                                                      
LINV0254 DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,LIVSTAEL,VHPSTA,0                           
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,LIVNUMEL,SNVRINV#,0                         
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,ADVDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
         GOTO1 AADDDATA,DMCB,AFABLK,ADVNAMEL,SNVRIANM,L'SNVRIANM                
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,PRDDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
         GOTO1 AADDDATA,DMCB,AFABLK,PRDNAMEL,SNVRIPNM,L'SNVRIPNM                
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,AGYDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
         GOTO1 AADDDATA,DMCB,AFABLK,AGYNAMEL,SNVRIAGN,L'SNVRIAGN                
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,SALDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
         GOTO1 AADDDATA,DMCB,AFABLK,SALNAMEL,SNVRISNM,L'SNVRISNM                
*                                                                               
         LA    R0,VHPARMLQ(R4)     A(OUTPUT)                                    
         ST    R0,ADDR                                                          
         MVC   WORK2+100(L'SNVLKEY),KEY                                         
         GOTOR SENDSTA                                                          
         MVC   KEY(L'SNVLKEY),WORK2+100                                         
         GOTO1 XSPDHIGH            RESTORE SEQUENCE                             
*        GOTO1 VDMGR,DMCB,=C'GETREC',=C'XSPFIL',KEY+36,AIOREC,DMWORK            
*                                                                               
         B     LINV0242            NEXT ELEM                                    
*                                                                               
LINV0299 GOTO1 VSWITCH,DMCB,=C'REP ',0                                          
         CLI   4(R1),2             SYSTEM NOT OPEN                              
         BE    LINV02XX            ????                                         
         CLI   4(R1),1                                                          
         BE    LINV02XX            ????                                         
*                                                                               
LINV02XX B     EXITOK                                                           
         DROP  R4,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
***>>>>  MEDIA OCEAN PROFILE PROCESSING                                         
*          DATA SET REPRP00    AT LEVEL 120 AS OF 05/11/06                      
**********************************************************************          
* MEDIAOCEAN PROFILE DOWNLOAD                                                   
**********************************************************************          
MOSTDWN  NTR1  BASE=*,LABEL=*                                                   
         GOTO1 VCOLY,DMCB,(X'10',0),(0,0)                                       
         CLI   DMCB+4,X'FF'                                                     
         JNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADDR2,DMCB                                                       
*                                                                               
         MVI   WORK,C'N'           SET RETURN FLAG TO 'NO'                      
         TM    EZPPROF+EZPMOCUB,EZPMOCUA                                        
*                                  MEDIA OCEAN COMMERCE USER?                   
         BNO   MOSDN060            NO  - PUT OUT DATA AS 'NO'                   
         MVI   WORK,C'Y'           SET RETURN FLAG TO 'YES'                     
         TM    EZPPROF+EZPSOFFB,EZPSOFFA                                        
*                                  APPLY STA/OFF PROFILE?                       
         BNO   MOSDN060            NO  - PUT OUT DATA AS 'YES'                  
         MVI   WORK,C'N'           SET RETURN FLAG TO 'NO'                      
         L     R6,ATWA                                                          
         AHI   R6,(SVPARMBF-T82AFFD)                                            
         USING VHPARMD,R6                                                       
*                                                                               
         XC    KEY,KEY                                                          
K        USING RSTAKEY,KEY                                                      
         MVI   K.RSTAKTYP,X'02'                                                 
         MVC   K.RSTAKREP,REPALPHA                                              
         MVC   K.RSTAKSTA,VHPSTA                                                
         DROP  K                                                                
*                                                                               
         GOTO1 VHIGH                                                            
*                                                                               
         CLC   KEY(L'RSTAKEY),KEYSAVE                                           
         BE    *+12                                                             
         MVI   WORK,C'N'                                                        
         B     MOSDN060                                                         
*                                                                               
         GOTO1 VGETREC,AIOREC                                                   
*                                                                               
         L     R8,AIOREC                                                        
         USING RSTAREC,R8                                                       
*                                                                               
*   RETRIEVE STATION / OFFICE ELEMENTS HERE , COMPARE FOR OFFICE                
*                                                                               
         LA    R1,2                SET L(OFFICE ONLY)                           
         ST    R1,FULL             SAVE LENGTH IN FULL                          
*                                                                               
         MVI   ELCODE,X'2E'        GET STATION / OFFICE ELEMENT                 
         BAS   RE,GETEL                                                         
         BE    MOSDN010            ELEMENT FOUND: CHECK FOR OFFICE              
*                                                                               
         LA    R1,5                SET L(OFFICE + DATE)                         
         ST    R1,FULL             SAVE LENGTH IN FULL                          
*                                                                               
         L     R8,AIOREC                                                        
         MVI   ELCODE,X'3A'        GET STATION / OFFICE + DATE ELT              
         BAS   RE,GETEL                                                         
         BNE   MOSDN060            OUTPUT AS 'NO'                               
*                                                                               
MOSDN010 EQU   *                                                                
*                                                                               
*   2E AND 3D ELEMENTS ARE SAME UP TO OFFICE, SO RSTAPFEL                       
*        IS USED AS THE DSECT TO POSITION TO THE FIRST OFFICE                   
*                                                                               
         USING RSTAPFEL,R8                                                      
         ZIC   RF,1(R8)            GET ELEMENT LENGTH                           
         SH    RF,=H'13'           SUBTRACT CONTROL, ETC                        
         LA    RE,0                CLEAR REG                                    
         DR    RE,R1               DIV L(DATA) TO GET COUNT BY TYPE             
*                                     COUNT IN RF FOR LOOP                      
         LA    RE,RSTAPFFC         SET A(1ST OFFICE)                            
         DROP  R8                                                               
*                                                                               
MOSDN020 EQU   *                                                                
         CLC   VHPOFF,0(RE)        OFFICE IN ELEMENT?                           
         BE    MOSDN040            YES                                          
         AR    RE,R1               BUMP TO NEXT ELEMENT                         
         BCT   RF,MOSDN020         DO ALL OFFICES                               
         B     MOSDN060            OUTPUT AS 'NO'                               
MOSDN040 EQU   *                                                                
         MVI   WORK,C'Y'           OUTPUT AS 'Y'                                
MOSDN060 DS    0H                                                               
*                                  ADD NEW MEDIAL OCEAN ELEMENT                 
         GOTO1 ASETELEM,DMCB,AFABLK,MOSDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,MOSFLGEL,WORK,0                             
*                                                                               
*                                                                               
*                                                                               
MOSDN080 DS    0H                                                               
         B     EXITOK                                                           
*                                                                               
MOSDNERR DS    0H                                                               
         L     RE,ADDR                                                          
         MVC   ERROR,0(RE)                                                      
         B     EXITL                                                            
         EJECT                                                                  
         DROP  R6                                                               
         LTORG                                                                  
***>>>>  MEDIA OCEAN PROFILE PROCESSING                                         
**********************************************************************          
* REFRESH MEDIAOCEAN INVOICES DOWNLOAD                                          
* CALLS DINVFOO AFTER INITIALIZING THE 3 CON/ORDER NUMBERS                      
**********************************************************************          
         USING INVWORKD,R5                                                      
DMOIDWN  NTR1  BASE=*,LABEL=*,WORK=(R5,INVWORKQ)                                
         GOTO1 VCOLY,DMCB,(X'10',0),(0,0)                                       
         CLI   DMCB+4,X'FF'                                                     
         JNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADDR2,DMCB                                                       
*                                                                               
         L     R4,ATWA                                                          
         AHI   R4,(SVPARMBF-T82AFFD)                                            
         ZIC   R2,0(R4)            NUMBER OF CONTRACTS                          
         LA    R3,1(R4)            START OF CONTRACTS                           
         LR    R4,R2                                                            
         MHI   R4,VHSPRMLQ                                                      
         LA    R4,0(R4,R3)         BUMP PAST CONTRACTS                          
         LA    R0,VHPARMLQ(R4)     A(OUTPUT)                                    
         ST    R0,ADDR                                                          
*                                                                               
         USING VHPARMD,R4                                                       
         USING VHSPARMD,R3                                                      
DMODL010 DS    0H                                                               
         MVC   WORK(L'VHPSTA),VHPSTA                                            
         XC    VHPARMD(VHPARMLQ),VHPARMD                                        
         MVC   VHPCON,VHSCON                                                    
         MVC   VHPMORD,VHSMORD                                                  
         MVC   VHPSORD,VHSSORD                                                  
         MVC   VHPSTA,WORK                                                      
*                                                                               
         GOTOR DINVFOO,DMCB,(C'M',0)  MEDIAOCEAN INVOICE                        
         BNE   DMODL020                                                         
         OI    INVFLAG,X'80'       SET >0 INVOICES FOUND                        
*                                                                               
DMODL020 LA    R3,VHSPRMLQ(R3)                                                  
         BCT   R2,DMODL010                                                      
         TM    INVFLAG,X'80'       IF 0 INVOICES FOUND, ERROR OUT               
         BZ    EXITNO              ERROR, BUT EXIT QUIETLY BECAUSE              
*                                  AN ACTUALLY ERROR MESSAGE WILL RESUL         
*                                  IN USER HAVING TO SEE A DIALOG ON            
*                                  THE PC APPLICATION, THEY THEN HAVE           
*                                  TO CLICK CANCEL TO GET RID OF IT             
*                                  THIS IS A FALINK LIMITATION                  
         B     EXITOK                                                           
         DROP  R3,R4,R5                                                         
*                                                                               
**********************************************************************          
* REFRESH LOCAL INVOICE DOWNLOAD                                                
**********************************************************************          
         USING INVWORKD,R5                                                      
DINVDWN  NTR1  BASE=*,LABEL=*,WORK=(R5,INVWORKQ)                                
         GOTO1 VCOLY,DMCB,(X'10',0),(0,0)                                       
         CLI   DMCB+4,X'FF'                                                     
         JNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ADDR2,DMCB                                                       
         MVI   INVFLAG,0           CLEAR FLAG                                   
*                                                                               
         L     R4,ATWA                                                          
         AHI   R4,(SVPARMBF-T82AFFD)                                            
         ZIC   R2,0(R4)            NUMBER OF ORDERS                             
         LA    R3,1(R4)            START OF ORDERS                              
         LR    R4,R2                                                            
         MHI   R4,L'VHPORD                                                      
         LA    R4,0(R4,R3)         BUMP PAST ORDERS                             
         LA    R0,VHPARMLQ(R4)     A(OUTPUT)                                    
         ST    R0,ADDR                                                          
*                                                                               
         USING VHPARMD,R4                                                       
DIDWN010 DS    0H                                                               
         XC    VHPARMD(VHPARMLQ),VHPARMD                                        
         MVC   VHPORD,0(R3)                                                     
*                                                                               
         GOTOR DINVFOO,DMCB,(C'S',0)  LOCAL INVOICE                             
         BNE   DIDWN020                                                         
         OI    INVFLAG,X'80'       SET >0 INVOICES FOUND                        
*                                                                               
DIDWN020 LA    R3,L'VHPORD(R3)                                                  
         BCT   R2,DIDWN010                                                      
         TM    INVFLAG,X'80'                                                    
         BZ    DINVSERR            IF 0 INVOICES FOUND, ERROR OUT               
         B     EXITOK                                                           
         DROP  R4,R5                                                            
*---------------------------------------------------------------------          
* DO THE WORK OF DOWNLOADING A  LOCAL STATION INVOICE                           
* CALLED BY DINVDWN(LOCAL INVOICES)                                             
*           DMOIDWN(MO INVOICES)                                                
*    R5       POINTS LOCAL WORK AREA                                            
*    R6       POINTS TO VHPARMS WITH STATION ORDER# FILLED IN                   
*    ADDR     POINTS TO USABLE OUTPUT AREA                                      
*    ADDR2    POINTS TO T82A10 - THE UTILITY ROUTINE                            
*                                                                               
*---------------------------------------------------------------------          
         USING VHPARMD,R4                                                       
         USING INVWORKD,R5                                                      
DINVFOO  NTR1  BASE=*,LABEL=*,WORK=(R5,INVWORKQ)                                
         MVC   INVTYPE,0(R1)                                                    
         MVI   INVFLAG,0           CLEAR INVOICE SEARCH FLAG                    
         CLI   CURRSYS,C'S'                                                     
         BE    DINV006                                                          
         MVC   ERROR,=Y(1007)      SPOT SYSTEM NOT STARTED                      
         GOTO1 SPTSYS                                                           
         BNE   DINVX                                                            
*                                                                               
DINV006  DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         XC    MISCFLG,MISCFLG                                                  
         USING SNVKEY,R3                                                        
*                                                                               
         MVC   INVSTART,BMOSFF                                                  
         MVI   INVSTART+2,X'01'     FORCE TO FIRST OF MONTH                     
         MVC   INVEND,BMOSFFE                                                   
         MVI   INVEND+2,X'01'       FORCE TO FIRST OF MONTH                     
         GOTO1 VDATCON,DMCB,(3,INVEND),(2,INVCST)                               
         XC    INVCST,=X'FFFF'                                                  
         MVC   SVINVEND,INVEND                                                  
         MVC   SVINVCST,INVCST                                                  
         MVC   SVINVSTR,INVSTART                                                
*                                                                               
         CLI   INVTYPE,C'M'                                                     
         BE    DINVMO                                                           
*LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL           
* LOCAL INVOICES                                                                
*LLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLLL           
         MVI   SNVLTYP,SNVRTYPQ     X'0E'                                       
         MVI   SNVLSUB,SNVLSUBQ     X'B3'                                       
         MVC   SNVLID,USRID                                                     
         MVC   SNVLSTA,SVSIGNON                                                 
         MVC   SNVLPOW,REPALPHA                                                 
*                                                                               
         CLI   SNVLSTA+4,C' '                                                   
         BH    *+8                                                              
         MVI   SNVRSTA+4,C'T'                                                   
*                                                                               
         CLI   SNVLSTA+4,C'L'                                                   
         BH    *+8                                                              
         MVI   SNVRSTA+4,C'T'                                                   
*                                                                               
         MVC   SNVLMOS,INVCST                                                   
*                                                                               
         XC    WORK(20),WORK                                                    
         UNPK  WORK(11),VHPORD(6)                                               
         OI    WORK+10,X'F0'       CONVERT TO ABSOLUTE EBCDIC VALUE             
*                                                                               
         LA    RE,WORK+1           IGNORE THE FIRST DIGIT                       
         LA    RF,WORK+10          END OF NUMBER                                
*                                                                               
DINVS08  CR    RE,RF               BOUNDRY                                      
         BH    DINVS10                                                          
         CLI   0(RE),C'0'                                                       
         BH    DINVS10             REACH SIGNIFICANT NUMBER                     
         LA    RE,1(RE)                                                         
         B     DINVS08                                                          
*                                                                               
DINVS10  XC    SNVLSORD,SNVLSORD   MOVE SIGNIFICANT NUMBER ONLY                 
         LA    RF,SNVLSORD                                                      
DINVS12  CLI   0(RE),0                                                          
         BE    DINVS14             END OF SIGNIFICANT NUMBER                    
         MVC   0(1,RF),0(RE)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         B     DINVS12                                                          
DINVS14  DS    0H                                                               
         OC    SNVLSORD,SPACES     PAD WITH SPACES                              
         B     DINV018                                                          
*MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM          
* DONWNLOAD MEDIAOCEAN INVOICES                                                 
*---------------------------------------------------------------------          
*    INVFLAG DEFINITION:           X'80' = FOUND >0 INVOICE KEY                 
*                                  X'40' = DONE SEARCH BY MO CONTRACT#          
*                                  X'20' = DONE SEARCH BY REPPAK CON#           
*                                  X'10' = DONE SEARCH BY STATION ORD#          
*                                  X'08' = ECHO DONE                            
*                                  X'02' = HEADER DATA SENT                     
*                                  X'01' = AGY,ADV,PRD,SAL DATA SENT            
*MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM          
DINVMO   DS    0H                                                               
         XC    INVTYPE2,INVTYPE2                                                
         MVI   SNVRTYP,SNVRTYPQ    X'0E'                                        
         MVI   SNVRSUB,SNVRSUBQ    X'A3'                                        
         MVC   SNVREP,REPALPHA                                                  
         MVC   SNVRSTA,VHPSTA                                                   
         MVC   SNVLMOS,INVCST                                                   
*                                                                               
         CLI   SNVLSTA+4,C' '                                                   
         BH    *+8                                                              
         MVI   SNVRSTA+4,C'T'                                                   
*                                                                               
         CLI   SNVLSTA+4,C'L'                                                   
         BH    *+8                                                              
         MVI   SNVRSTA+4,C'T'                                                   
*-----------                                                                    
* SEARCH BY MEDIA OCEAN CONTRACT #                                              
*-----------                                                                    
         MVC   INVSVKY1,KEY                                                     
DINVM00  TM    INVFLAG,X'70'       SEARCHED ALL?                                
         BO    DINVX               YES EXIT                                     
*                                                                               
         TM    INVFLAG,X'40'       SEARCHED MO CONTRACT # ALREADY?              
         BO    DINVM30             YES, SKIP                                    
*                                                                               
         OI    INVFLAG,X'40'       MARK SEARCHED MO CONTRACT #                  
         OC    VHPMORD,VHPMORD     FIRST READ WITH MO CONTRACT #                
         BZ    DINVM30                                                          
*                                                                               
         MVI   INVTYPE2,C'M'                                                    
         XC    WORK(20),WORK                                                    
         UNPK  WORK(11),VHPMORD(6)                                              
         OI    WORK+10,X'F0'       CONVERT TO ABSOLUTE EBCDIC VALUE             
*                                                                               
         LA    RE,WORK+1           IGNORE THE FIRST DIGIT                       
         LA    RF,WORK+10          END OF NUMBER                                
DINVM08  CR    RE,RF               BOUNDRY                                      
         BH    DINVM10                                                          
         CLI   0(RE),C'0'                                                       
         BH    DINVM10             REACH SIGNIFICANT NUMBER                     
         LA    RE,1(RE)                                                         
         B     DINVM08                                                          
*                                                                               
DINVM10  XC    SNVRINV,SNVRINV     MOVE SIGNIFICANT NUMBER ONLY                 
         LA    RF,SNVRINV                                                       
DINVM12  CLI   0(RE),0                                                          
         BE    DINVM14             END OF SIGNIFICANT NUMBER                    
         MVC   0(1,RF),0(RE)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         B     DINVM12                                                          
DINVM14  DS    0H                                                               
         OC    SNVRINV,SPACES      PAD WITH SPACES                              
         GOTOR READINV,DMCB,INVSTART,INVEND                                     
         BE    DINVM80             GO RETRIEVE INVOICE WITH THIS KEY            
*-----------                                                                    
* BY STATION ORDER #                                                            
*-----------                                                                    
DINVM30  TM    INVFLAG,X'20'       SEARCHED STATION ORDER# ALREADY?             
         BO    DINVM60             YES,SKIP                                     
         OI    INVFLAG,X'20'       MARK SEARCHED STATION ORDER#                 
*                                                                               
         OC    VHPSORD,VHPSORD     READ AGAIN WITH STATION ORDER #              
         BZ    DINVM60                                                          
         MVI   INVTYPE2,C'S'                                                    
         MVC   SNVRINV,VHPSORD                                                  
         GOTOR READINV,DMCB,INVSTART,INVEND                                     
         BE    DINVM80             SEARCH NEXT KEY                              
*-----------                                                                    
* SEARCH BY REPPAK CONTRACT #                                                   
*-----------                                                                    
DINVM60  TM    INVFLAG,X'10'       SEARCHED REPPAK CONTRACT#  ALREADY?          
         BO    DINVX               YES,SKIP                                     
         OI    INVFLAG,X'10'                                                    
*                                                                               
         OC    VHPCON,VHPCON       READ AGAIN WITH REPPAK CONTRACT #            
         BZ    DINVX                                                            
         MVI   INVTYPE2,C'R'                                                    
         XC    SNVRINV,SNVRINV     CLEAR KEY                                    
         ZAP   WORK+10(5),=P'99999999'                                          
         SP    WORK+10(5),VHPCON                                                
         MVO   WORK(5),WORK+10(5)                                               
         MVC   SNVRCON,WORK                                                     
         GOTOR READINVO,DMCB,INVSTART,INVEND                                    
         BNE   DINVX               DONE ALL SEARCHING                           
*--------------                                                                 
*                                                                               
* ECHO BACK ALL ORDER/CONTRACT# FROM THE REQUEST SO THAT CALLER                 
* CAN IDENTIFY THE INVOICES DETAILS                                             
*--------------                                                                 
DINVM80  TM    INVFLAG,X'08'       ECHO DONE ALREADY?                           
         BO    DINV018                                                          
         OI    INVFLAG,X'08'                                                    
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,MOIDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
         OC    VHPCON,VHPCON                                                    
         BZ    DINVM90                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,MORCONEL,VHPCON,0                           
*                                                                               
DINVM90  DS    0H                                                               
         OC    VHPMORD,VHPMORD                                                  
         BZ    DINVM96                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,MOMCONEL,VHPMORD,0                          
*                                                                               
DINVM96  DS    0H                                                               
         OC    VHPSORD,VHPSORD                                                  
         BZ    DINVM100                                                         
         GOTO1 AADDDATA,DMCB,AFABLK,MOSORDEL,VHPSORD,0                          
*                                                                               
DINVM100 MVC   SNVLMOS,INVCST      RESTORE THE START MONTH                      
         B     DINV018                                                          
***********************************************************************         
* GET THE INVOICE RECORD                                                        
* IN: KEY SHOULD BE SET TO AN APPROPRIATE INVOICE KEY                           
***********************************************************************         
DINV018  DS    0H                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDMGR,DMCB,(X'08',=C'DMRDHI'),=C'XSPDIR',KEY,KEY                 
*                                                                               
DINV019  MVC   INVSVKEY,KEY        SAVE OFF THE KEY TO RESTORE SEQUENCE         
         CLC   KEY(L'SNVRMAST),KEYSAVE                                          
         BE    DINV040                                                          
*                                                                               
DINV020  DS    0H                  -1 MONTH, LOOP                               
         NI    MISCFLG,X'FF'-X'40'                                              
         XC    WORK,WORK                                                        
         GOTO1 VDATCON,DMCB,(3,INVEND),(0,WORK+50)    JAN01/06                  
         GOTO1 VADDAY,DMCB,(C'M',WORK+50),WORK+90,-1   DEC01/05                 
         GOTO1 VDATCON,DMCB,(0,WORK+90),(3,WORK+100)                            
         CLC   WORK+100(3),INVSTART                                             
         BL    DINV030             LOWER THAN START DATE?                       
         MVC   KEY,KEYSAVE                                                      
         MVC   INVEND,WORK+100                                                  
         GOTO1 VDATCON,DMCB,(3,INVEND),(2,INVCST)                               
         XC    INVCST,=X'FFFF'                                                  
         MVC   SNVLMOS,INVCST                                                   
         XC    KEY+SNVLMIN-SNVLKEY(14),KEY+SNVLMIN-SNVLKEY  REST O KEY          
         CLI   INVTYPE2,C'R'                                                    
         BNE   DINV018                                                          
*                                                                               
* CONTRACT KEYS HAS DIFFERENT DISPLACEMENT SO MUST BE TREATED DIFFEREN          
*                                                                               
         XC    KEY+SNVRINV-SNVLKEY(25),KEY+SNVRINV-SNVLKEY                      
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDMGR,DMCB,(X'08',=C'DMRDHI'),=C'XSPDIR',KEY,KEY                 
         MVC   INVSVKEY,KEY        SAVE OFF THE KEY TO RESTORE SEQUENCE         
         CLC   KEY(15),KEYSAVE                                                  
         BE    DINV040                                                          
         B     DINV020             TRY AGAIN                                    
*                                                                               
DINV030  CLI   INVTYPE,C'M'        MEDIA-OCEAN MODE?                            
         BNE   DINVX               NO, EXIT                                     
         MVC   INVEND,SVINVEND     YES, RESTORE START AND END ATE               
         MVC   INVCST,SVINVCST                                                  
         MVC   INVSTART,SVINVSTR                                                
         MVC   KEY,INVSVKY1                                                     
         B     DINVM00             LOOP AND TRY OTHER KEYS                      
*                                                                               
DINV040  OI    INVFLAG,X'80'       FOUND >0 INVOICE                             
         GOTO1 VDMGR,DMCB,=C'GETREC',=C'XSPFIL',KEY+36,AIO4,DMWORK              
*                                                                               
         TM    MISCFLG,X'40'       SAME SALESPERSON?                            
         BO    DINV048                                                          
         CLI   INVTYPE,C'M'        MEDIA-OCEAN?                                 
         BE    DINV048                                                          
*                                                                               
         L     R6,AIO4                                                          
         MVI   ELCODE,SNVRIELQ                                                  
         BAS   RE,GETEL2                                                        
         BNE   DINV030                                                          
*                                                                               
DINV046  MVC   WORK2(L'SNVRISNM),SNVRISNM-SNVRINVD(R6)                          
         OC    WORK2(L'SNVRISNM),SPACES                                         
         CLC   TWASALN(SALNQ),WORK2                                             
         BNE   DINV030             EXIT                                         
         OI    MISCFLG,X'40'       SAME SALESPERSON                             
*                                                                               
DINV048  L     R6,AIO4                                                          
         LA    R6,42(R6)                                                        
DINV050  CLI   0(R6),0                                                          
         BNE   DINV060             TRY NEXT RECORD                              
         MVC   KEY,INVSVKEY        RESTORE KEY                                  
         GOTO1 VDMGR,DMCB,(X'08',=C'DMRDHI'),=C'XSPDIR',KEY,KEY                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 VDMGR,DMCB,=C'DMRSEQ',=C'XSPDIR',KEY,KEY,0                       
         B     DINV019                                                          
*                                                                               
DINV060  CLI   INVTYPE,C'M'                                                     
         BE    DINV070                                                          
         CLI   0(R6),X'25'                                                      
         BE    DINV150                                                          
DINV070  CLI   0(R6),X'10'         HEADER ELEM                                  
         BE    DINV100                                                          
         CLI   0(R6),X'40'         DETAIL ELEM                                  
         BE    DINV200                                                          
DINV080  ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DINV050                                                          
*                                                                               
         USING SNVHDELD,R6                                                      
DINV100  XC    PERSTART,PERSTART                                                
         OC    SNVHDSDT,SNVHDSDT                                                
         BZ    DINV102                                                          
         GOTO1 VDATCON,DMCB,(2,SNVHDSDT),(0,PERSTART)                           
*                                                                               
DINV102  CLI   INVTYPE,C'M'                                                     
         BE    DINV080                                                          
         TM    INVFLAG,X'02'                                                    
         BO    DINV116                                                          
         GOTO1 ASETELEM,DMCB,AFABLK,IVHDATA,0                                   
         OI    INVFLAG,X'02'       SETELEM DONE                                 
*                                                                               
DINV103  OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
         OC    SNVHDIDT,SNVHDIDT                                                
         BZ    DINV104                                                          
         GOTO1 VDATCON,DMCB,(2,SNVHDIDT),(19,WORK)                              
         GOTO1 AADDDATA,DMCB,AFABLK,IVHDATEL,WORK,0                             
*                                                                               
DINV104  OC    SNVHDDDT,SNVHDDDT                                                
         BZ    DINV108                                                          
         GOTO1 VDATCON,DMCB,(2,SNVHDDDT),(19,WORK)                              
         GOTO1 AADDDATA,DMCB,AFABLK,IVHDUEEL,WORK,0                             
*                                                                               
DINV108  GOTO1 AADDDATA,DMCB,AFABLK,IVHCOSEL,SNVHDTCS,0                         
         GOTO1 AADDDATA,DMCB,AFABLK,IVHSPTEL,SNVHDTSP,0                         
*                                                                               
DINV114  DS    0H                                                               
         L     RF,AIO4                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,INVSTO,SNVLSORD-SNVLKEY(RF),0               
*                                                                               
DINV116  B     DINV080                                                          
*                                                                               
         USING SNVRINVD,R6                                                      
DINV150  DS    0H                                                               
         TM    INVFLAG,X'01'                                                    
         BO    DINV170                                                          
         OI    INVFLAG,X'01'                                                    
         GOTO1 ASETELEM,DMCB,AFABLK,AGYDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
         GOTO1 AADDDATA,DMCB,AFABLK,AGYNAMEL,SNVRIAGN,L'SNVRIAGN                
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,ADVDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
         GOTO1 AADDDATA,DMCB,AFABLK,ADVNAMEL,SNVRIANM,L'SNVRIANM                
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,PRDDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
         GOTO1 AADDDATA,DMCB,AFABLK,PRDNAMEL,SNVRIPNM,L'SNVRIPNM                
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,SALDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
         GOTO1 AADDDATA,DMCB,AFABLK,PRDNAMEL,SNVRISNM,L'SNVRISNM                
*                                                                               
         GOTOR SENDSTA                                                          
DINV170  B     DINV080             NEXT ELEM                                    
*                                                                               
         USING SNVIDELD,R6                                                      
DINV200  DS    0H                                                               
         ZIC   R0,SNVIDDAY         CALCULATE DATE                               
         GOTO1 VADDAY,DMCB,PERSTART,WORK+40,(R0)                                
         GOTO1 VDATCON,DMCB,(0,WORK+40),(3,WORK+50)                             
         OC    POSTST,POSTST       START DATE ?                                 
         BZ    *+14                                                             
         CLC   POSTST,WORK+50                                                   
         BH    DINV080             GET NEXT ELEM                                
*                                                                               
         OC    POSTEN,POSTEN       END DATE?                                    
         BZ    *+14                                                             
         CLC   POSTEN,WORK+50                                                   
         BL    DINV080             GET NEXT RECORD                              
*                                                                               
         GOTO1 ASETELEM,DMCB,AFABLK,INVDATA,0                                   
         OI    MISCFLG1,MF1DATA    DATA IN BUFFER                               
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,INVDATE,WORK+50,0                           
*                                                                               
         SR    R0,R0                                                            
         ZICM  R1,SNVIDTIM,2                                                    
         D     R0,=F'60'           CONVERT THE TIME TO MILITARY                 
         MH    R1,=H'100'                                                       
         AR    R1,R0                                                            
         AH    R1,=H'600'          THE TIME IS DISPLACEMENT FROM 6:00A          
         CH    R1,=H'2400'         EARLY MORNING?                               
         BL    *+8                                                              
         SH    R1,=H'2400'         YES                                          
         STH   R1,HALF                                                          
         GOTO1 AADDDATA,DMCB,AFABLK,INVTIME,HALF,0                              
*                                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,INVLEN,SNVIDSLN,0                           
         GOTO1 AADDDATA,DMCB,AFABLK,INVCOST,SNVIDCST,0                          
*                                                                               
         BAS   RE,GETFLMC2         GET FILM CODE                                
*                                                                               
         B     DINV080             NEXT ELEM                                    
*                                                                               
DINV0299 GOTO1 VSWITCH,DMCB,=C'REP ',0                                          
         CLI   4(R1),2             SYSTEM NOT OPEN                              
         BE    DINVX               ????                                         
         CLI   4(R1),1                                                          
         BE    DINVX               ????                                         
*                                                                               
DINVX    TM    INVFLAG,X'80'       AT LEAST ONE INVOICE FOUND?                  
         BO    EXITOK              YES, EXIT OK                                 
         B     EXITL               NO, ERROR OUT                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* GET FILM CODE FOR MO                                                          
* R6 HAS X'40' DETAIL INVOICE ELEMENT                                           
**********************************************************************          
         USING INVWORKD,R5                                                      
GETFLMC2 NTR1  LABEL=*,BASE=*,WORK=(R5,INVWORKQ)                                
         USING SNVIDELD,R6                                                      
         L     R4,AIO4                                                          
         LA    R4,42(R4)                                                        
         USING SNVCMELD,R4                                                      
*                                                                               
GETF210  CLI   0(R4),0                                                          
         BE    GETF2X                                                           
         CLI   0(R4),X'30'         COMMERCIAL CODE ELEM                         
         BNE   GETF220                                                          
         CLC   SNVCMICD,SNVIDCML                                                
         BE    GETF230                                                          
GETF220  ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     GETF210                                                          
         DROP  R6                                                               
*                                                                               
GETF230  DS    0H                                                               
         GOTO1 AADDDATA,DMCB,AFABLK,INVISCI,SNVCMCD,0                           
GETF2X   DS    0H                                                               
         XIT1                                                                   
         DROP  R4,R5                                                            
         LTORG                                                                  
***********************************************************************         
* GET THE INVOICE RECORD                                                        
* IN: KEY SHOULD BE SET TO AN APPROPRIATE INVOICE KEY                           
* OUT: CC EQUAL: GOT A MATCH ON THIS INVOICE KEY                                
*      CC NOT EQ: NO MATCH ON THIS INVOICE KEY                                  
***********************************************************************         
         USING INVWORKD,R5                                                      
K        USING SNVRKEY,KEY                                                      
READINV  NTR1  LABEL=*,BASE=*,WORK=(R5,INVWORKQ)                                
         LM    R2,R3,0(R1)                                                      
         MVC   INVSTART,0(R2)                                                   
         MVC   INVEND,0(R3)                                                     
         MVC   INVSVKY2,KEY                                                     
*                                                                               
RINV018  MVC   KEYSAVE,KEY                                                      
         GOTO1 VDMGR,DMCB,(X'08',=C'DMRDHI'),=C'XSPDIR',KEY,KEY                 
*                                                                               
RINV019  MVC   INVSVKEY,KEY        SAVE OFF THE KEY TO RESTORE SEQUENCE         
         CLC   KEY(L'SNVRMAST-1),KEYSAVE                                        
         BE    RINVYES                                                          
*                                                                               
RINV020  DS    0H                  -1 MONTH, LOOP                               
         GOTO1 VDATCON,DMCB,(3,INVEND),(0,WORK+50)    JAN01/06                  
         GOTO1 VADDAY,DMCB,(C'M',WORK+50),WORK+90,-1   DEC01/05                 
         GOTO1 VDATCON,DMCB,(0,WORK+90),(3,WORK+100)                            
         CLC   WORK+100(3),INVSTART                                             
         BL    RINVNO              LOWER THAN START DATE?                       
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         MVC   INVEND,WORK+100                                                  
         GOTO1 VDATCON,DMCB,(3,INVEND),(2,INVCST)                               
         XC    INVCST,=X'FFFF'                                                  
         MVC   K.SNVRMOS,INVCST                                                 
         XC    KEY+SNVLMIN-SNVLKEY(14),KEY+SNVLMIN-SNVLKEY  REST O KEY          
         B     RINV018             TRY AGAIN                                    
*                                                                               
RINVYES  SR    RC,RC                                                            
         B     RINVX                                                            
RINVNO   MVC   KEY,INVSVKY2                                                     
         LTR   RC,RC                                                            
RINVX    B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET THE OLD INVOICE RECORD                                                    
* IN: KEY SHOULD BE SET TO AN APPROPRIATE INVOICE KEY                           
* OUT: CC EQUAL: GOT A MATCH ON THIS INVOICE KEY                                
*      CC NOT EQ: NO MATCH ON THIS INVOICE KEY                                  
***********************************************************************         
         USING INVWORKD,R5                                                      
K        USING SNVRKEY,KEY                                                      
READINVO NTR1  LABEL=*,BASE=*,WORK=(R5,INVWORKQ)                                
         LM    R2,R3,0(R1)                                                      
         MVC   INVSTART,0(R2)                                                   
         MVC   INVEND,0(R3)                                                     
         MVC   INVSVKY2,KEY                                                     
*                                                                               
ROIV018  MVC   KEYSAVE,KEY                                                      
         GOTO1 VDMGR,DMCB,(X'08',=C'DMRDHI'),=C'XSPDIR',KEY,KEY                 
*                                                                               
ROIV019  MVC   INVSVKEY,KEY        SAVE OFF THE KEY TO RESTORE SEQUENCE         
         CLC   KEY(SNVRINV-SNVRKEY),KEYSAVE                                     
         BE    ROIVYES                                                          
*                                                                               
ROIV020  DS    0H                  -1 MONTH, LOOP                               
         XC    WORK,WORK                                                        
         GOTO1 VDATCON,DMCB,(3,INVEND),(0,WORK+50)    JAN01/06                  
         GOTO1 VADDAY,DMCB,(C'M',WORK+50),WORK+90,-1   DEC01/05                 
         GOTO1 VDATCON,DMCB,(0,WORK+90),(3,WORK+100)                            
         CLC   WORK+100(3),INVSTART                                             
         BL    ROIVNO              LOWER THAN START DATE?                       
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         MVC   INVEND,WORK+100                                                  
         GOTO1 VDATCON,DMCB,(3,INVEND),(2,INVCST)                               
         XC    INVCST,=X'FFFF'                                                  
         MVC   K.SNVRMOS,INVCST                                                 
         XC    KEY+SNVLMIN-SNVLKEY(14),KEY+SNVLMIN-SNVLKEY  REST O KEY          
         B     ROIV018             TRY AGAIN                                    
*                                                                               
ROIVYES  SR    RC,RC                                                            
         B     ROIVX                                                            
ROIVNO   MVC   KEY,INVSVKY2                                                     
         LTR   RC,RC                                                            
ROIVX    B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SEND  TATION INFORMATION TO FALINK                                            
***********************************************************************         
SENDSTA  NTR1  BASE=*,LABEL=*                                                   
         CLI   CURRSYS,C'R'                                                     
         BE    SSTA100                                                          
         GOTO1 REPSYS                                                           
*                                                                               
K        USING SNVLKEY,KEY                                                      
SSTA100  LA    R0,VHPARMLQ(R4)     A(OUTPUT)                                    
         ST    R0,ADDR                                                          
         XC    WORK2(5),WORK2                                                   
         MVC   WORK2(4),K.SNVLSTA                                               
         GOTOX (VSTAQ,ADDR2),DMCB,(RC),(0,WORK2),ADDR                           
         BNE   EXITL                                                            
         DROP  K                                                                
*                                                                               
         LA    R0,*                                                             
         AHI   R0,STATBL-(*-4)                                                  
         GOTOR XARSE,DMCB,ADDR,(R0)                                             
*                                                                               
SSTAX    DS    0H                                                               
         GOTO1 SPTSYS                                                           
         B     EXITOK                                                           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*====================================================================*          
*                                                                               
* INTERESTING DATA STRINGS FOR XARSE                                            
*                                                                               
*  AL4(HEADER ELEMENT)                                                          
*    AL1(DATA EQUATE), XARSE MODE  0 - DEFAULT                                  
*                                  1 - DON'T SEND UNLESS > SPACE                
*                                  2 - MAPCODE ONLY                             
*    AL4(MAPTABLE ENTRY FOR DATA)                                               
*                                                                               
SALTBL   DC    A(SALDATA)                                                       
FOO      DC    AL1(QSALKSAL,0),A(SALCODEL)                                      
XTABLQ   EQU   *-FOO                                                            
         DC    AL1(QSALNAME,0),A(SALNAMEL)                                      
         DC    AL1(QSALTEL,0),A(SALTELEL)                                       
         DC    AL1(QSALFAX,0),A(SALFAXEL)                                       
         DC    AL1(QSALTEAM,0),A(SALTEMEL)                                      
         DC    AL1(QTEMDVNM,0),A(SALDIVEL)                                      
         DC    AL1(QTEMNAME,0),A(SALTNMEL)                                      
         DC    AL1(QSALEMAL,0),A(SALEMLEL)                                      
         DC    AL1(QSALOFF,0),A(SALOFFEL)                                       
         DC    AL1(QSALOFFN,0),A(SALOFNEL)                                      
         DC    AL1(QSALMGR,2),A(SALMGREL)                                       
         DC    AL1(QSALADD0,0),A(SALADDEL)                                      
         DC    AL1(QSALADD1,0),A(SALADDEL)                                      
         DC    AL1(QSALMOSP,0),A(SALMOUEL)                                      
         DC    AL1(0)                                                           
CTYTBL   DC    A(CTYDATA1)                                                      
         DC    AL1(QCTYKCTY,0),A(CTYCTYEL)                                      
         DC    AL1(QCTYDESC,0),A(CTYDSCEL)                                      
         DC    AL1(0)                                                           
DCTTBL   DC    A(DCTDATA)                                                       
         DC    AL1(QDCTKCTY,0),A(DCTCTYEL)                                      
         DC    AL1(QDCTNAME,0),A(DCTNAMEL)                                      
         DC    AL1(0)                                                           
DSPTBL   DC    A(DSPDATA)                                                       
         DC    AL1(QDSPKSAL,0),A(DSPCODEL)                                      
         DC    AL1(QDSPNAME,0),A(DSPNAMEL)                                      
         DC    AL1(QDSPTEL,0),A(DSPTELEL)                                       
         DC    AL1(QDSPFAX,0),A(DSPFAXEL)                                       
         DC    AL1(0)                                                           
STATBL   DC    A(STADATA)                                                       
         DC    AL1(QSTAKSTA,0),A(STASTAEL)                                      
         DC    AL1(QSTAMKT,0),A(STAMKTEL)                                       
         DC    AL1(QSTAAFFL,0),A(STAAFFEL)                                      
         DC    AL1(QSTACHAN,0),A(STACHNEL)                                      
         DC    AL1(0)                                                           
ADVTBL   DC    A(ADVDATA)                                                       
         DC    AL1(QADVKADV,0),A(ADVCODEL)                                      
         DC    AL1(QADVNAME,0),A(ADVNAMEL)                                      
         DC    AL1(0)                                                           
AGYTBL   DC    A(AGYDATA)                                                       
         DC    AL1(QAGYKAGY,0),A(AGYCODEL)                                      
         DC    AL1(QAGYKAOF,1),A(AGYOFFEL)                                      
         DC    AL1(QAGYNAME,0),A(AGYNAMEL)                                      
         DC    AL1(0)                                                           
AGADTBL  DC    A(0)                                                             
         DC    AL1(QAGADLN0,0),A(AGYAD0EL)                                      
         DC    AL1(QAGADLN1,0),A(AGYAD1EL)                                      
         DC    AL1(QAGADLN2,0),A(AGYAD2EL)                                      
         DC    AL1(QAGADLN3,0),A(AGYAD3EL)                                      
         DC    AL1(QAGADNAM,0),A(AGYONMEL)                                      
         DC    AL1(0)                                                           
PRDTBL   DC    A(PRDDATA)                                                       
         DC    AL1(QPRDKPRD,1),A(PRDCODEL)                                      
         DC    AL1(QPRDNAME,0),A(PRDNAMEL)                                      
         DC    AL1(0)                                                           
DPTTBL   DC    A(DPTDATA)                                                       
         DC    AL1(QDPTCODE,0),A(DPTCODEL)                                      
         DC    AL1(QDPTSNAM,0),A(DPTSNMEL)                                      
         DC    AL1(QDPTLNAM,0),A(DPTLNMEL)                                      
         DC    AL1(0)                                                           
         SPACE 3                                                                
*====================================================================*          
*                                                                               
* ROUTINE LIST FOR REQUEST HEADERS                                              
*                                                                               
*   AL2 - HEADER EQUATE NUMBER                                                  
*   AL4 - ADDRESS OF DOWNLOAD ROUTINE                                           
*   AL4 - ADDRESS OF FIELD LIST FOR REQUEST                                     
*   AL4 - ADDRESS OF OPTIONAL EXTRA SETUP ROUTINE                               
*                                                                               
*                                                                               
REQHDRS  DS    0A                                                               
         DC    AL2(INITHDRQ),A(INITDWN),A(INITFLDS),A(0)                        
ROUTABLQ EQU   *-REQHDRS                                                        
         DC    AL2(LCONHDRQ),A(LCONDWN),A(LCONFLDS),A(0)                        
         DC    AL2(DCONHDRQ),A(DCONDWN),A(DCONFLDS),A(0)                        
         DC    AL2(RTGSHDRQ),A(RTGSDWN),A(RTGSFLDS),A(0)                        
         DC    AL2(VHDRHDRQ),A(VHDRDWN),A(VHDRFLDS),A(0)                        
**       DC    AL2(VBKSHDRQ),A(VBKSDWN),A(VBKSFLDS),A(0)                        
         DC    AL2(FVERHDRQ),A(0),A(FVERFLDS),A(FVERHDR)                        
         DC    AL2(FVE2HDRQ),A(0),A(FVERFLDS),A(FVERHDR)                        
         DC    AL2(LINVHDRQ),A(LINVDWN),A(LINVFLDS),A(0)                        
         DC    AL2(DINVHDRQ),A(DINVDWN),A(DINVFLDS),A(0)                        
         DC    AL2(DMOIHDRQ),A(DMOIDWN),A(DMOIFLDS),A(0)                        
         DC    AL2(MOSTHDRQ),A(MOSTDWN),A(MOSTFLDS),A(0)                        
         DC    AL2(0000)                                                        
*====================================================================*          
*                                                                               
* FIELD ROUTINE LIST FOR VERSION INFO                                           
*                                                                               
FVERFLDS DS    0A                                                               
         DC    AL2(FVERVERQ),A(FVERVER)                                         
FLDTABLQ EQU   *-FVERFLDS                                                       
         DC    AL2(0000)                                                        
*                                                                               
* FIELD ROUTINE LIST FOR INIT REQUEST                                           
*                                                                               
INITFLDS DS    0A                                                               
         DC    AL2(INITSALQ),A(INITSAL)                                         
         DC    AL2(0000)                                                        
*                                                                               
* FIELD ROUTINE LIST FOR MEDIA OCEAN STATION OFFICE VALIDATION                  
*                                                                               
MOSTFLDS DS    0A                                                               
         DC    AL2(MOSTSTAQ),A(MOSTSTA)                                         
         DC    AL2(MOSTOFFQ),A(MOSTOFF)                                         
         DC    AL2(0000)                                                        
*                                                                               
* FIELD ROUTINE LIST FOR LIST CONTRACT REQUEST                                  
*                                                                               
LCONFLDS DS    0A                                                               
         DC    AL2(LCSALQ),A(LCSAL)                                             
         DC    AL2(LCSTAQ),A(LCSTA)                                             
         DC    AL2(LCFLTSTQ),A(LCFLTST)                                         
         DC    AL2(LCFLTENQ),A(LCFLTEN)                                         
         DC    AL2(LCAGYQ),A(LCAGY)                                             
         DC    AL2(LCAOFQ),A(LCAOF)                                             
         DC    AL2(LCADVQ),A(LCADV)                                             
         DC    AL2(0000)                                                        
*                                                                               
LINVFLDS DS    0A                                                               
         DC    AL2(LCSALQ),A(LCSAL)                                             
         DC    AL2(LCSTAQ),A(LCSTA)                                             
         DC    AL2(LCFLTSTQ),A(LCFLTST)                                         
         DC    AL2(LCFLTENQ),A(LCFLTEN)                                         
         DC    AL2(0000)                                                        
*                                                                               
* FIELD ROUTINE LIST FOR RATING DOWNLOAD REQUEST                                
*                                                                               
RTGSFLDS DS    0A                                                               
         DC    AL2(RTRTSRVQ),A(RTRTSRV)                                         
         DC    AL2(RTTPBKQ),A(RTBOOK)                                           
         DC    AL2(RTT4BKQ),A(RTBOOK)                                           
         DC    AL2(RTPVBKQ),A(RTBOOK)                                           
         DC    AL2(RTTPUPGQ),A(RTUPG)                                           
         DC    AL2(RTT4UPGQ),A(RTUPG)                                           
         DC    AL2(RTPVUPGQ),A(RTUPG)                                           
         DC    AL2(RTDEMOQ),A(RTDEMO)                                           
         DC    AL2(RTSTAQ),A(RTSTA)                                             
         DC    AL2(RTDAYQ),A(RTDAY)                                             
         DC    AL2(RTSTIMQ),A(RTSTIM)                                           
         DC    AL2(RTETIMQ),A(RTETIM)                                           
         DC    AL2(RTXTIMQ),A(RTETIM)                                           
         DC    AL2(RTDATEQ),A(RTDATE)                                           
         DC    AL2(RTOVRNTQ),A(RTOVRNT)                                         
         DC    AL2(RTOVHMSQ),A(RTOVHMS)                                         
         DC    AL2(RTDECPCQ),A(RTDECPC)                                         
         DC    AL2(RTOTPBKQ),A(RTOTPBK)                                         
         DC    AL2(RTEMRKQ),A(RTEMRK)                                           
         DC    AL2(0000)                                                        
*                                                                               
* FIELD ROUTINE LIST FOR VALIDATE HEADER                                        
*   AND BOOKS/UPGRADES/DEMOS                                                    
*                                                                               
VHDRFLDS DS    0A                                                               
         DC    AL2(VBRTSRVQ),A(VBRTSRV)                                         
         DC    AL2(VBBOOKQ),A(VBBOOK)                                           
         DC    AL2(VBUPGRDQ),A(VBUPGRD)                                         
         DC    AL2(VBDEMOQ),A(VBDEMO)                                           
         DC    AL2(VBNIUPGQ),A(VBUPGRD)                                         
         DC    AL2(VBUPGNMQ),A(VBUPGNM)                                         
         DC    AL2(VHSTAQ),A(VHSTA)                                             
         DC    AL2(VHADVQ),A(VHADV)                                             
         DC    AL2(VHPRDQ),A(VHPRD)                                             
         DC    AL2(VHAGYQ),A(VHAGY)                                             
         DC    AL2(VHAOFQ),A(VHAOF)                                             
         DC    AL2(VHFLSQ),A(VHFLS)                                             
         DC    AL2(VHFLEQ),A(VHFLE)                                             
         DC    AL2(VHCTYPQ),A(VHCTYP)                                           
         DC    AL2(VHDCTQ),A(VHDCT)                                             
         DC    AL2(VHDSPQ),A(VHDSP)                                             
         DC    AL2(VHSALQ),A(VHSAL)                                             
         DC    AL2(0000)                                                        
*                                                                               
* FIELD ROUTINE LIST FOR DOWNLOAD CONTRACT HEADER FIELDS                        
*                                                                               
DCONFLDS DS    0A                                                               
         DC    AL2(DCONCONQ),A(DCONCON)                                         
         DC    AL2(DCONBUYQ),A(DCONBUY)                                         
         DC    AL2(DCONPSDQ),A(DCONPSD)                                         
         DC    AL2(DCONPEDQ),A(DCONPED)                                         
         DC    AL2(DCONINVQ),A(DCONINV)                                         
         DC    AL2(0000)                                                        
*                                                                               
DMOIFLDS DS    0A                                                               
         DC    AL2(DMOICONQ),A(DCONCON)                                         
         DC    AL2(DMOISTAQ),A(VHSTA)                                           
         DC    AL2(DMOIPSDQ),A(DINVPSD)                                         
         DC    AL2(DMOIPEDQ),A(DINVPED)                                         
         DC    AL2(0000)                                                        
*                                                                               
* FIELD ROUTINE LIST FOR DOWNLOAD INVOICE HEADER FIELDS                         
*                                                                               
DINVFLDS DS    0A                                                               
         DC    AL2(DINVORDQ),A(DINVORD)                                         
         DC    AL2(DINVDETQ),A(DINVDET)      IMPLEMENTATION                     
         DC    AL2(DINVPSDQ),A(DINVPSD)                                         
         DC    AL2(DINVPEDQ),A(DINVPED)                                         
         DC    AL2(0000)                                                        
*                                                                               
*                                                                               
PHASES   DS    0X                  ** LOADED PHASE LIST **                      
         DC    AL1(0)              <=== TWABLD                                  
         DC    AL1(0)              <=== UNBOOK                                  
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(QDEMOCON)                                                    
         DC    AL1(QDEMOVAL)                                                    
         DC    AL1(QUPVAL)                                                      
         DC    AL1(QBOOKVAL)       <=== BUT THIS EQUATE IS ALSO 0               
         DC    AL1(QDAYVAL)                                                     
         DC    AL1(QDAYUNPK)                                                    
         DC    AL1(QTIMVAL)                                                     
         DC    AL1(QUNTIME)                                                     
         DC    AL1(QREFETCH)                                                    
         DC    AL1(QGETBROD)                                                    
         DC    AL1(QFALINK)                                                     
         DC    AL1(QREPFACS)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
PHASESN  EQU   *-PHASES                                                         
         EJECT                                                                  
       ++INCLUDE REDEMPRC                                                       
       ++INCLUDE REEZPPROF                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE RECNTAUTOD                                                     
       ++INCLUDE REEZPWORKD                                                     
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DEDEMTABD                                                      
         SPACE 3                                                                
*                                                                               
DURLREC  DSECT                                                                  
       ++INCLUDE REGENURL                                                       
*                                                                               
BROADTBL DSECT                                                                  
BRDTABLE DS    0CL7                                                             
         ORG   BRDTABLE                                                         
BRDSTART DS    XL3                 BINARY MONTH START DATE                      
BRDEND   DS    XL3                 BINARY MONTH END   DATE                      
BRDWEEKS DS    XL1                 NUM WEEKS IN PERIOD                          
BRDLEN   EQU   *-BRDSTART          LENGTH OF ENTRY                              
*                                                                               
INVWORKD  DSECT                                                                 
INVSVKEY DS    XL(L'KEY)                                                        
INVSTART DS    XL3                                                              
INVEND   DS    XL3                                                              
INVCST   DS    XL2                 COMPRESS START DATE                          
INVD1    DS    XL3                                                              
INVTYPE  DS    CL1                 L=LOCAL,M=MO                                 
INVTYPE2 DS    CL1                 R= REPPAK#, M=MO#, S=STATION ORDER#          
INVSVKY2 DS    XL(L'KEY)                                                        
INVFLAG  DS    XL1                 FLAG FOR INVOICE RETRIEVAL                   
*                                  X'80' = FOUND >0 INVOICE KEY                 
*                                  X'40' = DONE SEARCH BY MO CONTRACT#          
*                                  X'20' = DONE SEARCH BY REPPAK CON#           
*                                  X'10' = DONE SEARCH BY STATION ORD#          
*                                  X'01' = MEDIA-OCEAN SEARCHING MODE           
SVINVEND DS    XL(L'INVEND)                                                     
SVINVCST DS    XL(L'INVCST)                                                     
SVINVSTR DS    XL(L'INVSTART)                                                   
INVSVKY1 DS    XL(L'KEY)                                                        
INVWORKQ EQU   *-INVWORKD                                                       
*                                                                               
SALNQ    EQU   20                  SALESPERSON NAME COMPARISON LENGTH           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'207REEZP00   05/16/13'                                      
         END                                                                    
