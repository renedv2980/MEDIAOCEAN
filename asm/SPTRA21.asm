*          DATA SET SPTRA21    AT LEVEL 048 AS OF 10/22/18                      
*PHASE T21621C                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE KHDUMMY                                                                
         TITLE 'T21621 SPOT TRAFFIC TALENT TRANSFER'                            
***********************************************************************         
*                                                                     *         
* LEV 00    MAR22/06 INIT                                             *         
* LEV 02    MAR30/06 TALENT TRANSFER EXCLUDE                          *         
* LEV 03    APR27/06 FIX PRINTING PIGGYBACK BUG                       *         
* LEV 04    MAY17/06 RUN FOR ALL CLIENTS AND SORT STATIONS AND CABLE  *         
* LEV 05    JUL19/06 UPDATE FCLT ROUTINES TO WORK WITH NEW VALICLT    *         
* LEV 06    AUG07/06 ADD MARKET NAME TO WORKER FILE                   *         
* LEV 07    AUG23/06 MATCH INST AND BUY DATES BY STATION, NOT MARKET, *         
*                    FIX BUG IN ABUYDTE RTN, ADD RLP CODE FOR SOFT DTS*         
* LEV 08    OCT20/06 FIX RLP PROBLEM WITH TT PROFILE START DATE       *         
* LEV 09    NOV15/06 FIX TALENT TRANSFER EXCLUDE PIGGYBACK BUG        *         
*                    CHANGE SO TALCOM LIST DOES NOT EXCLUDE ANYTHING  *         
* LEV 10    NOV21/06 IGNORE COMBINE CABLE NETWORK PROFILE             *         
* LEV 11    DEC04/06 INCREASE SIZE OF TSAR AREA AND CABLE STA TABLE   *         
* LEV 12    FEB05/07 FIX BUG IN READBUYS ROUTINE                      *         
* LEV 14    APR02/07 CHANGE ERROR MESSAGE TO CLARUS                   *         
* LEV 15    MAY16/07 COMPARE COMM1 AND COMM2 BEFORE CLEARING BDATELST *         
* LEV 16    JUN22/07 RETURN ERROR IF NO TT PROFILE EXISTS             *         
* LEV 17    AUG13/07 MNAS - FIX FILE PROBLEM IN CLI VALIDATION        *         
* LEV 18    OCT17/07 CLEAR SVCMLCML FOR NEW CLIENT                    *         
* LEV 19    DEC12/07 FIX PROBLEM WITH CLIENT SECURITY LOCKOUT         *         
* LEV 20    AUG18/08 BREAK OUT UNITS FOR MARKETS NY/LA/CHI            *         
* LEV 21    FEB09/09 USE SPGETBUY TO READ BUY RECORDS                 *         
* LEV 22    FEB18/09 SKIP CODE IN READCML ROUTINE IF TRACCSQ=0        *         
* LEV 23    MAR19/09 COMMENT OUT - USE SPGETBUY TO READ BUY RECORDS   *         
* LEV 24    AUG26/09 ADID PROJECT                                     *         
* LEV 25    SEP29/09 DON'T PACK SPECIAL CHARACTERS IN COMMERCL FILTER *         
* LEV 26    OCT13/09 DON'T UNPACK COMM2 IF ZEROS                      *         
* LEV 27    MAY06/10 FIX BLDACT WHEN FILTERING BY MARKET              *         
* LEV 28    MAY12/10 CHANGE DEFAULT SIZE OF REQUESTS TO MEDIUM        *         
* LEV 33    FEB22/11 FIX NO RECORDS ERROR, MULT EST BUG, ADD AUDIT RTN*         
*                                                                               
* GHOA 047 11JUL18 SPEC-25103 FIX PRINTING BUG FOR NY AND LA          *         
* GHOA 048 15MAR18 SPSUG-1239  ELIM NEED AGENCY STATUSES: NX, SX, CX            
*                                                                     *         
***********************************************************************         
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)         *         
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER      *         
*                    (DDGENCON-T00A30)                                *         
*             AIO2 - PATTERN RECORDS IN INPUT PHASE, MARKET RECS      *         
*                     IN OUTPUT PRINT PHASE                           *         
*             LAST 256 BYTES OF AIO2  HOLDS ELEM INFO IN ERROR REPORT *         
*             AIO3 - 3 PRD/1 EST/1 BPRD/20 EST NAME (100 ESTIMATES)   *         
*             NOW IN VADUMMY                                          *         
*             AIO3 - CABLE STATIONS AND INDEX POINTERS TO             *         
*                    CABLE NAME TABLE                                 *         
*             AIO3 - HOLDS UNCOVERED BUY DATES IN ERROR REPORT        *         
*                                                                     *         
* REGISTER USAGE -                                                    *         
*        R0 - WORK REG                                                *         
*        R1 - WORK REG                                                *         
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR       *         
*        R3 - POINTER FOR COMML TABLE                                 *         
*        R4 - WORK REG & KEY DSECT POINTER                            *         
*        R5 - WORK REG - ALSO CTR IN LR BUILD COMML TABLE RTN         *         
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM   *         
*              FOR DSECT IN VALREC                                    *         
*        R7 - SECOND BASE                                             *         
*        R8 - POINTER TO SPOOLD                                       *         
*        R9 - POINTER TO SYSD                                         *         
*        RA - POINTER TO ATWA                                         *         
*        RB - FIRST BASE                                              *         
*        RC - POINTER TO GEND                                         *         
*        RD - SAVE AREA POINTER                                       *         
*        RE - GOTO1 REG                                               *         
*        RF - GOTO1 REG                                               *         
*                                                                     *         
*        PROGRAM LABELS MEANING:                                      *         
*        V PREFIX = VALIDATE                                          *         
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                    *         
*        F PREFIX = FIND                                              *         
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST          *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
* THIS PROGRAM READS THRU TRAFFIC INSTRUCTION RECAP RECORDS, CREATES  *         
* A MARKET ENTRY FOR EACH COMML AND FIRST/LAST DATE PAIR.             *         
* IT WILL ALSO GIVE AN ERROR REPORT ON BUYS THAT NEED TO BE TRAFFICKED*         
*                                                                     *         
***********************************************************************         
T21621   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21621**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA             BASE SCREEN FOR SYSTEM + THIS PROG           
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,SPTR49RR                                                      
*                                                                               
         XC    IOWRKND,IOWRKND                                                  
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,PRINTREP       OFF-LINE ACTIVITY                            
         BE    LR                                                               
                                                                                
*==========================================================                     
* IF RUNNING OFFLINE, BUT NOT SOON,  ADD A REQUEST FOR TA99                     
* IT WILL PICK UP ALL WORKER FILES FOR THIS AGENCY                              
* AND NEVER FOR TALCOM                                                          
*==========================================================                     
                                                                                
         CLI   OFFLINE,C'Y'                                                     
         JNE   PR02                                                             
         CLI   MODE,RUNFRST                                                     
         JNE   PR02                                                             
         TM    WHEN,X'20'          IS IT SOON                                   
         BO    PR02                YES -                                        
         CLI   TLCMFLG,C'Y'        IF RUNNING TALCOM LIST, SKIP                 
         JE    PR02                                                             
*                                                                               
         OPEN  (TR21REQ,(OUTPUT))                                               
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
*                                                                               
         L     RE,=A(REQCARD)                                                   
         MVC   2(2,RE),AGENCY     SET AGENCY CODE IN REQ                        
* INSERT REQUESTING ID IN REQUEST CARD                                          
         L     RF,TWAMASTC                                                      
         SR    R0,R0                                                            
         ICM   R0,3,MCORIGID-MASTD(RF)                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         L     RE,=A(REQCDID)                                                   
         UNPK  7(5,RE),DUB                                                      
         L     R1,=A(TR21REQ)                                                   
         L     R0,=A(REQCARD)                                                   
         PUT   (1),(0)            PUT A REQUEST FOR TA99                        
*                                                                               
         CLOSE TR21REQ                                                          
         LTR   RF,RF                                                            
         JNZ   *+2                                                              
         J     EXIT                                                             
*                                                                               
PR02     CLI   MODE,RUNLAST                                                     
         BNE   EXIT                                                             
         TM    WHEN,X'20'          SKIP IF SOON                                 
         BO    EXIT                                                             
         BRAS  RE,AUDITWRK         SAVE WRKR FILES IN DATASET                   
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
*     VALIDATE KEY ROUTINE                                                      
         SPACE                                                                  
*                                                                               
VK       MVC   DMCB+4(4),=X'D9000AFE'                                           
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   ATRPACK,DMCB        ADDRESS OF TRPACK                            
*                                                                               
         BRAS  RE,SETSCRN          SET SCREEN FOR TALCOM OR STRANS              
*                                                                               
         LA    R2,TRAMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         SPACE                                                                  
         MVI   ALLCLI,0                                                         
         LA    R2,TRACLTH          CLIENT                                       
         XC    QCLT,QCLT                                                        
         XC    BCLT,BCLT                                                        
         CLI   5(R2),0             ALL CLIENTS?                                 
         BNE   *+12                                                             
         MVI   ALLCLI,C'Y'         SET ALL CLIENTS STATUS                       
         B     VK01                                                             
*                                                                               
         SPACE                                                                  
         GOTO1 VALICLT                                                          
         MVC   SVBCLT,BCLT                                                      
*                                                                               
         CLI   TLCMFLG,C'Y'        IF RUNNING TALCOM LIST, SKIP                 
         BE    *+8                                                              
         BRAS  RE,PRDLIST          GET LIST OF VALID PRODUCTS                   
         SPACE                                                                  
* GET PROFILE REC(S)                                                            
         SPACE                                                                  
* READ T0 PROFILE *                                                             
         SPACE                                                                  
VK01     XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         CLI   ALLCLI,C'Y'                                                      
         BNE   *+10                                                             
         MVC   WORK+7(3),=C'ALL'                                                
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         SPACE                                                                  
         MVI   WORK+3,C'2'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
         SPACE                                                                  
         MVC   SVT2PR05,SVT1PROF+4  COMBINE CABLE NETWORKS                      
         SPACE                                                                  
         XC    MYDATE,MYDATE                                                    
         XC    MYDATE2,MYDATE2                                                  
         CLI   TLCMFLG,C'Y'         IF RUNNING TALCOM,                          
         BE    VK05                 SKIP TT PROFILE                             
         MVI   WORK+3,C'T'          GET TT PROFILE                              
         GOTO1 (RF),(R1),,SVT1PROF                                              
         CLI   SVT1PROF+3,0         BAD PROFILE?                                
         BE    VK04                                                             
         BRAS  RE,PROFTT                                                        
         CLI   SVNUSER,C'N'         SPOTTAL USER?                               
         BNE   VK05                                                             
VK04     LA    R2,TRACLTH           NO, CANNOT RUN STRANS                       
         B     SPTUSRER                                                         
         SPACE                                                                  
VK05     MVI   WORK+3,C'1'                                                      
         GOTO1 GETPROF,DMCB,WORK,SVT1PROF,DATAMGR                               
         SPACE                                                                  
         MVI   BPRD,0                                                           
         MVI   BPRD2,0                                                          
         XC    QPRD,QPRD                                                        
         XC    QPRD2,QPRD2                                                      
         LA    R2,TRAPRDH                                                       
         CLI   5(R2),0                                                          
         BE    VK10                                                             
         CLI   ALLCLI,C'Y'    IF RUNNING FOR ALL CLIENTS,                       
         BE    ERRINV         PRODUCT IS NOT ALLOWED                            
         SPACE                                                                  
         GOTO1 VALIPRD                                                          
         SPACE                                                                  
         MVC   QPRD,WORK                                                        
         MVC   BPRD,WORK+3                                                      
         MVC   PRDNM,WORK+5                                                     
         SPACE                                                                  
VK10     LA    R2,TRAPTRH                                                       
         CLI   5(R2),0                                                          
         BE    VK16                                                             
         SPACE                                                                  
         CLI   ALLCLI,C'Y'    IF RUNNING FOR ALL CLIENTS,                       
         BE    ERRINV         PRODUCT PARTNER IS NOT ALLOWED                    
         SPACE                                                                  
         CLI   BPRD,0                                                           
         BE    MISSPRD                                                          
         SPACE                                                                  
         GOTO1 VALIPRD                                                          
         SPACE                                                                  
         MVC   QPRD2,WORK                                                       
         MVC   BPRD2,WORK+3                                                     
         MVC   PRD2NM,WORK+5                                                    
         SPACE                                                                  
VK16     CLI   TLCMFLG,C'Y'        IF RUNNING TALCOM LIST, SKIP                 
         BE    VK18A                                                            
         LA    R2,TRATALH          TALENT AGENCY                                
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         OC    TRATAL,SPACES                                                    
         CLI   OFFLINE,C'Y'             ONLINE ONLY                             
         BE    VK18                                                             
         CLC   TWAAGY,=C'SJ'            SKIP TALENT AGY VALIDATION              
         BE    VK18                     FOR SJR                                 
         BRAS  RE,READTAL                                                       
VK18     MVC   TALAGY,TRATAL                                                    
         EJECT                                                                  
*                                                                               
VK18A    MVI   UPDATE,C'N'                                                      
         CLI   TLCMFLG,C'Y'        IF RUNNING TALCOM LIST,                      
         BE    VK19                                                             
         LA    R2,TRATSTH          TEST RUN? ACCEPT Y/N                         
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         CLI   8(R2),C'Y'          IS IT A TEST RUN?                            
         BE    VK19                YES                                          
         CLI   SVNUSER,C'N'        TALENT TRANSFER USER?                        
         BE    ERRINV              NO, MUST BE TEST RUN                         
         CLI   TWAWHEN,2           SOON IS ALWAYS A TEST RUN                    
         BE    ERRINV                                                           
         MVI   UPDATE,C'Y'                                                      
         CLI   8(R2),C'N'          IS IT A TEST RUN?                            
         BNE   ERRINV                                                           
         CLC   TWAAGY,=C'SJ'       IF AGENCY IS SJR                             
         BNE   VK19                                                             
         BRAS  RE,CKSYS            AND IF DSPACE = A,                           
         BE    ERRINV              DO NOT ALLOW TEST RUN = NO                   
*                                                                               
VK19     LA    R2,TRAMKTH          MARKET                                       
         XC    BMKT,BMKT                                                        
         XC    QMKT,QMKT                                                        
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         CLC   =C'ALL',8(R2)                                                    
         BE    VK20                                                             
         GOTO1 VALIMKT                                                          
         SPACE                                                                  
VK20     LA    R2,TRACMLH          COMML ID                                     
         XC    SVCMML,SVCMML                                                    
         BAS   RE,VCML                                                          
         SPACE                                                                  
         LA    R2,TRAPERH          PERIOD                                       
         BAS   RE,VPER                                                          
         SPACE                                                                  
         LA    R2,TRAOPTH                                                       
         BRAS  RE,VOPT             VALIDATE OPTIONS                             
         SPACE                                                                  
* NOW BUILD KEY                                                                 
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A24'                                                  
         MVC   KEY+2(3),BAGYMD AND BCLT                                         
         MVC   KEY+6(2),BMKT                                                    
         MVI   FIRSTSW,0                                                        
         SPACE                                                                  
         BRAS  RE,GETSIZE                                                       
         SPACE                                                                  
         B     EXIT                                                             
         EJECT                                                                  
* ONLINE ACTIVITY LIST *                                                        
         SPACE 2                                                                
*                                   SET TO READ INSTRUCTION RECAP               
LR       DS    0H                                                               
         MVI   29(RA),2            INDICATE RUNLAST HOOK REQD                   
*                                                                               
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A2B'  SPGETBUY FOR 2 BYTE BUYLINE              
         GOTO1 CALLOV,DMCB                                                      
         MVC   AGETBUY,DMCB                                                     
*                                                                               
         TM    SVOPTSW,OPTENAME    PRINT ESTIMATE NAME                          
         BZ    LREX10                                                           
*                                                                               
         LHI   RE,ESTLIST-SYSD                                                  
         AR    RE,R9               PROD/ESTIMATE LIST AREA                      
         LA    RF,L'ESTLIST                                                     
         XCEFL                                                                  
         SPACE                                                                  
         L     RE,AIO3             AND ESTIMATE NAME TABLE                      
         LA    RF,2500                                                          
         XCEFL                                                                  
         SPACE                                                                  
LREX10   L     R2,VADUMMY                                                       
         SPACE                                                                  
***      TM    SVOPTSW,OPTCABLE    CABLE STATION ONLY                           
***      BZ    LREX20                                                           
         SPACE                                                                  
         MVC   0(8,R2),=CL8'*CBLSTA*'                                           
         LA    R2,8(R2)                                                         
         ST    R2,CBLSTB                                                        
         LR    R0,R2                                                            
         LHI   R1,CBLSTASZ                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  (R0),(RE)                                                        
         AHI   R2,CBLSTASZ         ROOM FOR 1600 CABLE STA + INDEX              
         ST    R2,CBLSTBX                                                       
         MVC   0(8,R2),=CL8'*CBLNAM*'                                           
         LA    R2,8(R2)                                                         
         ST    R2,CBLNAM                                                        
         LR    R0,R2                                                            
         LHI   R1,CBLNAMSZ         ROOM FOR 1000 CABLE NAMES                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  (R0),(RE)                                                        
         AHI   R2,CBLNAMSZ         ROOM FOR 1000 CABLE NAMES                    
         ST    R2,CBLNAMX                                                       
         SPACE                                                                  
LREX20   MVC   0(8,R2),=CL8'*ACLIST*'  ACTIVITY LIST                            
         LA    R2,8(R2)                                                         
         ST    R2,AACLST                                                        
         LR    R0,R2                                                            
         LHI   R1,ACLSTSZ          ROOM FOR 1500 ACTIVITY ENTRIES               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  (R0),(RE)                                                        
         AHI   R2,ACLSTSZ                                                       
         ST    R2,AACLSTX          END OF ACTIVITY TABLE                        
         SPACE                                                                  
CBLSTASZ EQU   6000*5                                                           
CBLNAMSZ EQU   CBLNAML*1000                                                     
CBLNAML  EQU   24                                                               
ACLSTSZ  EQU   L'ACLDATA*1500                                                   
         SPACE                                                                  
LR00     DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING INSKEY,R4                                                        
         MVC   INSKID,=X'0A24'                                                  
         MVC   INSKAM(3),BAGYMD AND BCLT                                        
         MVC   INSKMKT,BMKT                                                     
         DROP  R4                                                               
         SPACE                                                                  
         LHI   R3,CMLIST-SYSD                                                   
*        LR    R1,R3                                                            
*        SR    R1,R9                                                            
*        ST    R1,CMLSTA                                                        
*        ST    R1,CMLSTA                                                        
         ST    R3,CMLSTA                                                        
         AR    R3,R9                                                            
         LR    R0,R9                                                            
         A     R0,LSYSD                                                         
         AHI   R0,-(L'CMLENT)                                                   
         ST    R0,CMLSTX                                                        
         CLI   OFFLINE,C'Y'        IS THIS ONLINE                               
         BNE   LR10                                                             
         L     R4,=A(SORTCARD)                                                  
         L     R5,=A(RECCARD)                                                   
         GOTO1 =V(SORTER),DMCB,(R4),(R5)                                        
         SPACE                                                                  
         USING CMLISTD,R3                                                       
LR10     CLI   UPDATE,C'N'                                                      
         BE    *+8                                                              
         BRAS  RE,OPENWRKR         OPEN WORKER FILE                             
         XC    COUNT,COUNT                                                      
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         B     LR26                                                             
         SPACE                                                                  
LR20     MVC   KEY(L'SVKEY),SVKEY                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVI   RDUPDATE,C'N'                                                    
LR24     GOTO1 SEQ                                                              
         SPACE                                                                  
LR26     LA    R4,KEY                                                           
         USING INSKEY,R4                                                        
         MVC   SVKEY,KEY                                                        
         CLC   KEY(3),KEYSAVE     ID/AM                                         
         BNE   LR80                                                             
         SPACE                                                                  
         CLI   ALLCLI,C'Y'         ALL CLIENTS?                                 
         BE    LR27                                                             
         CLC   KEY+3(2),SVBCLT     NO, MUST MATCH CLIENT                        
         BNE   LR80                                                             
         SPACE                                                                  
LR27     OC    SVBCLT,SVBCLT       1ST CLIENT?                                  
         BZ    LR28                                                             
         CLC   KEY+3(2),SVBCLT     SAME CLIENT?                                 
         BNE   LR28                                                             
         CLI   SVNUSER,C'N'                                                     
         BE    LR20                                                             
         B     LR29                                                             
*                                                                               
LR28     BRAS  RE,NEWCLI           NEW CLIENT                                   
         BNE   LR20                                                             
LR29     OC    BMKT,BMKT           WAS MARKET ENTERED                           
         BZ    LR30                                                             
         CLC   INSKMKT,BMKT                                                     
         BNE   LR24                                                             
         SPACE                                                                  
LR30     CLI   OPTEST,0                                                         
         BE    LR32                                                             
         CLC   OPTEST,INSKCOPY                                                  
         BNE   LR24                                                             
LR32     CLI   BPRD,0                                                           
         BE    LR34                                                             
         CLC   BPRD,INSKPRD                                                     
         BNE   LR24                                                             
         SPACE                                                                  
LR34     DS    0H                                                               
         TM    SVOPTSW,OPTCABLE    CABLE STATION ONLY                           
         BZ    LR36                                                             
         SPACE                                                                  
         CLI   SPOTCAN,C'C'        IF CANADIAN                                  
         BE    LR24                 NOT CABLE                                   
         SPACE                                                                  
*                    X'E8' NOW                                                  
         CLI   INSKSTA,CABLESTA    THIS A CABLE STATION                         
         BL    LR24                NO, GET NEXT                                 
         B     LR38                                                             
         SPACE                                                                  
LR36     DS    0H                                                               
         TM    SVOPTSW,OPTSTA      STD STATIONS ONLY                            
         BZ    LR38                                                             
         SPACE                                                                  
         CLI   SPOTCAN,C'C'        IF CANADIAN                                  
         BE    LR38                 NOT CABLE                                   
         SPACE                                                                  
*                    X'E8' NOW                                                  
         CLI   INSKSTA,CABLESTA    THIS A STD STATION                           
         BNL   LR24                NO, GET NEXT                                 
*                                                                               
LR38     MVC   BYTE,INSKPRD        PRODUCT                                      
         CLI   TLCMFLG,C'Y'        IF RUNNING TALCOM,                           
         BE    LR39                SKIP THIS CHECK                              
         BRAS  RE,PRDOK            PRODUCT VALID FOR TALENT AGENCY?             
         BNE   LR24                NO, GET NEXT                                 
*                                                                               
LR39     L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         LR    R4,R6                                                            
         USING INSKEY,R4                                                        
         SPACE                                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         BRAS  RE,GETINST          GET INSTRUCTION ELEMENTS                     
         CLI   INSTREC,0           NO X'10' OR X'30' ELEMENTS?                  
         BE    LR24                                                             
*                                                                               
         MVI   ELCODE,X'10'        IF READING PATTERNS                          
         CLI   INSTREC,C'P'                                                     
         BE    *+8                                                              
         MVI   ELCODE,X'30'        IF READING BUYS                              
         BAS   RE,GETEL            FIND INST DATA ELEM                          
         BNE   LR24                                                             
         USING INSDTAEL,R6                                                      
LR40     CLI   BPRD2,0                                                          
         BE    LR44                                                             
         CLC   BPRD2,INSPRD2                                                    
         BNE   LR78                                                             
         SPACE                                                                  
* CALC NO OF PATTERNS (SUBELS) IN ELEMENT *                                     
         SPACE                                                                  
LR44     SR    R0,R0                                                            
         ZIC   R1,INSDTALN                                                      
         AHI   R1,-(INSPTTN-INSDTAEL)                                           
         D     R0,=A(INSSUBEL)                                                  
         LTR   R0,R0                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         LA    R2,INSPTTN                                                       
         ST    R6,SVR6                                                          
LR46     STM   R1,R2,SVR1R2                                                     
         L     R6,SVR6                                                          
         SPACE                                                                  
         CLI   INSTREC,C'B'       IF X'30' ELEMENT, SKIP THIS PART              
         BE    LR47                                                             
         OC    0(3,R2),0(R2)      HIATUS PATTERN IS ZERO                        
         BZ    LR76                                                             
         SPACE                                                                  
LR47     CLC   PERSTRP,5(R2)       LTD                                          
         BH    LR76                                                             
         CLC   PERENDP,3(R2)       FTD                                          
         BL    LR76                                                             
         SPACE                                                                  
*        GOTO1 DATCON,DMCB,(3,3(R2)),(2,SVTCDTS)                                
*        GOTO1 (RF),(R1),(3,6(R2)),(2,SVTCDTS+2)                                
         SPACE                                                                  
         MVC   SVTCDTS,3(R2) SAVE 2 2 BYTE DATES                                
         SPACE                                                                  
***      TM    SVOPTSW,OPTCABLE    DOING CABLE STATIONS ONLY                    
***      BZ    *+8                                                              
         CLI   INSKSTA,CABLESTA    IF THIS IS A CABLE STATION,                  
         BL    *+8                                                              
         BRAS  RE,CBLFND           GET INDEX FOR CABLE NAME                     
         SPACE                                                                  
         MVC   SVPRD1,INSPRD1      SAVE PROD 1                                  
         MVC   SVPRD2,INSPRD2       PROD 2                                      
         MVC   SVSLN1,INSSLN1      SAVE LEN 1                                   
         MVC   SVSLN2,INSSLN2       LEN 2                                       
         MVC   SVEST,INSKCOPY      AND ESTIMATE                                 
         SPACE                                                                  
         CLI   INSTREC,C'B'        READING BUYS, NOT PATTERNS?                  
         BNE   LR48                                                             
         TM    INSFLAG,INSFLEST    COPY CODE = ESTIMATE?                        
         BZ    *+8                                                              
         MVI   STATEST,C'Y'        ESTIMATE STATUS = Y                          
         MVC   SAVEKEY,KEY         SAVE KEY OF LAST INST. RECAP RECORD          
         BRAS  RE,READBUYS         READ BUY RECORDS                             
         MVC   KEY,SAVEKEY         RESTORE LAST INST. RECAP RECORD              
         GOTO1 HIGH                                                             
         CLC   KEY(L'INSKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC              GET INST. RECAP RECORD                       
         B     LR76                                                             
         SPACE                                                                  
LR48     MVI   PACKFLG,0                                                        
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING PATKEY,R1                                                        
         MVC   PATKID,=X'0A22'                                                  
         MVC   PATKAM,BAGYMD                                                    
         MVC   PATKCLT,SVBCLT                                                   
         MVC   PATKPRD(4),INSPRD1                                               
         MVC   PATKCODE,INSKCOPY                                                
         MVC   PATKREF,0(R2)                                                    
         DROP  R1                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   LR76                                                             
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
*                                                                               
         CLC   PEREND,MYDATEB      PATTERN END < TT PROFILE DATE?               
         BL    LR76                                                             
*                                                                               
         TM    PATSTAT1,PATSADID   ARE ALL COMMERCIALS PACKED?                  
         BNO   *+8                                                              
         MVI   PACKFLG,C'Y'        SET PACKED FLAG                              
         DROP  R6                                                               
         L     R6,AIO2                                                          
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATCMLEL,R6                                                      
         ZIC   RE,PATCMLLN                                                      
         SRL   RE,4                DIVIDE BY 16 - DROP ODD                      
         LA    R2,2(R6)                                                         
         SPACE                                                                  
LR50     CLI   PACKFLG,C'Y'        ARE COMMERCIALS PACKED?                      
         BNE   LR51                                                             
         XC    UNPKCML1,UNPKCML1   UNPACK COMMERCIALS                           
         XC    UNPKCML2,UNPKCML2   UNPACK COMMERCIALS                           
         ST    RE,SVRE                                                          
         GOTO1 ATRPACK,DMCB,(C'U',0(R2)),UNPKCML1                               
         OC    8(8,R2),8(R2)       IS THERE A CML2?                             
         BZ    LR50A                                                            
         GOTO1 ATRPACK,DMCB,(C'U',8(R2)),UNPKCML2                               
LR50A    L     RE,SVRE                                                          
LR51     OC    SVCMML,SVCMML       WAS COMML ENTERED                            
         BZ    LR54                                                             
         CLI   PACKFLG,C'Y'        ARE COMMERCIALS PACKED?                      
         BNE   LR53                                                             
         ZIC   R1,SVCMMLN          IF FILTER LENGTH < 7 (LEN<8)                 
         CHI   R1,7                                                             
         BL    LR52                                                             
         EX    R1,LRCLCC           COMPARE WITH PACKED COMMERCIALS              
         BE    LR54                                                             
         OC    8(8,R2),8(R2)                                                    
         BZ    LR74                                                             
         EX    R1,LRCLCD                                                        
         BNE   LR74                                                             
         B     LR54                                                             
LR52     EX    R1,LRCLCE           COMPARE WITH WORK  (UNPACKED)                
         BE    LR54                                                             
         OC    8(8,R2),8(R2)                                                    
         BZ    LR74                                                             
         EX    R1,LRCLCF           AND WORK + 12                                
         BNE   LR74                                                             
         B     LR54                                                             
LR53     ZIC   R1,SVCMMLN                                                       
         EX    R1,LRCLCA                                                        
         BE    LR54                                                             
         OC    8(8,R2),8(R2)                                                    
         BZ    LR74                                                             
         EX    R1,LRCLCB                                                        
         BNE   LR74                                                             
         SPACE                                                                  
LR54     CLC   0(2,R2),=XL2'5C00' DELETED CML                                   
         BE    LR74                                                             
         CLC   0(8,R2),HIATUS     BYPASS HIATUS                                 
         BE    LR74                                                             
         OC    0(8,R2),0(R2)      BETTER BE A CML  TEMP                         
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         LA    R3,WORK                                                          
         XC    WORK,WORK                                                        
         ST    RE,SVRE                                                          
         BRAS  RE,READCOM2        READ COMML RECORD                             
         L     RE,SVRE                                                          
         XC    CMLCML(16),CMLCML                                                
         CLI   SVTALEX,C'Y'       EXCLUDE COMMERCIAL 1?                         
         BNE   LR71                                                             
         CLI   SVTALEX2,C'Y'      EXCLUDE COMMERCIAL 2?                         
         BE    LR74                                                             
         CLI   PACKFLG,C'Y'       ARE COMMERCIALS PACKED?                       
         BE    LR69                                                             
         MVC   CMLCML(8),8(R2)    DON'T EXCLUDE 2ND COMMERCIAL                  
         B     *+10                                                             
LR69     MVC   CMLCML,UNPKCML2    DON'T EXCLUDE 2ND COMMERCIAL UNPACKED         
         OC    CMLCML,CMLCML      DO WE HAVE A 2ND COMMERCIAL?                  
         BZ    LR74                                                             
         B     LR73                                                             
LR71     CLI   SVTALEX2,C'Y'                                                    
         BNE   LR72                                                             
         CLI   PACKFLG,C'Y'       ARE COMMERCIALS PACKED?                       
         BE    LR71A                                                            
         MVC   CMLCML(8),0(R2)    DON'T EXCLUDE 1ST COMMERCIAL                  
         B     LR73                                                             
LR71A    MVC   CMLCML,UNPKCML1    DON'T EXCLUDE 1ST COMMERCIAL UNPACKED         
         B     LR73                                                             
LR72     CLI   PACKFLG,C'Y'       ARE COMMERCIALS PACKED?                       
         BE    LR72A                                                            
         MVC   CMLCML(8),0(R2)   DON'T EXCLUDE EITHER COMMERCIAL                
         MVC   CMLCML2(8),8(R2)                                                 
         B     LR73                                                             
LR72A    MVC   CMLCML,UNPKCML1   DON'T EXCLUDE EITHER COMM UNPACKED             
         MVC   CMLCML2,UNPKCML2                                                 
*                                                                               
LR73     MVI   CMLINST,C'P'       READING PATTERNS                              
         ST    RE,SVRE                                                          
         BRAS  RE,PUTCML          PUT COMMERCIAL TO SORTER                      
         L     RE,SVRE                                                          
         SPACE                                                                  
LR74     LA    R2,16(,R2)          NEXT CML PR                                  
         BCT   RE,LR50                                                          
LR76     CLI   INSTREC,C'B'                                                     
         BE    LR77                                                             
         LM    R1,R2,SVR1R2                                                     
         LA    R2,INSSUBEL(,R2)                                                 
         BCT   R1,LR46                                                          
         SPACE                                                                  
         L     R6,SVR6                                                          
         MVI   ELCODE,X'10'        IF READING PATTERNS                          
         CLI   INSTREC,C'P'                                                     
         BE    *+8                                                              
LR77     MVI   ELCODE,X'30'        IF READING BUYS                              
LR78     BAS   RE,NEXTEL                                                        
         BE    LR40                                                             
         B     LR20                                                             
         SPACE                                                                  
* TABLE (IF ANY) BUILT *                                                        
         SPACE                                                                  
LR80     OC    COUNT,COUNT         WAS ANYTHING FOUND                           
         BNZ   LR90                                                             
         SPACE                                                                  
         CLI   OFFLINE,C'Y'                                                     
         BNE   LR84                                                             
         SPACE                                                                  
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         SPACE                                                                  
LR84     XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(30),=CL30'* NOTE * NO RECORDS SELECTED *'                
         B     LR100                                                            
         SPACE                                                                  
LRCLCA   CLC   0(0,R2),SVCMML                                                   
LRCLCB   CLC   8(0,R2),SVCMML                                                   
LRCLCC   CLC   0(0,R2),SVCMMLP                                                  
LRCLCD   CLC   8(0,R2),SVCMMLP                                                  
LRCLCE   CLC   UNPKCML1(0),SVCMML                                               
LRCLCF   CLC   UNPKCML2(0),SVCMML                                               
         SPACE                                                                  
LR90     MVC   SVKEY,KEY                                                        
         L     R3,CMLSTA                                                        
         AR    R3,R9                                                            
         CLI   OFFLINE,C'Y'        IF OFFLINE, SORTING, NO TABLE                
         BE    LR96                                                             
         SPACE                                                                  
* SORT BY MARKET FTD LTD CMMLS AND STATION *                                    
         SPACE                                                                  
         GOTO1 XSORT,DMCB,(R3),(R5),L'CMLENT,L'CMLENT,0                         
         SPACE                                                                  
         LR    R1,R3                                                            
         LA    R1,CMLNEXT-CMLENT(,R1)                                           
         BCT   R5,*-4                                                           
         XC    0(L'CMLENT,R1),0(R1)                                             
         SPACE                                                                  
LR96     DS    0H                                                               
         TM    SVOPTSW,OPTENAME    PRINT ESTIMATE NAME                          
         BZ    LR100                                                            
         BRAS  RE,BESTNAME         BUILD ESTIMATE NAME TABLE                    
         SPACE                                                                  
LR100    DS    0H                                                               
         BRAS  RE,BLDEST           READ ESTIMATE                                
         BRAS  RE,BLDACT           BUILD ACTIVITY FROM BUYS                     
         SPACE                                                                  
         CLI   MODE,PRINTREP       OFF-LINE ACTIVITY                            
         BE    LRR                                                              
         DC    H'0'                                                             
         EJECT                                                                  
* OFF LINE ACTIVITY LIST *                                                      
         SPACE 3                                                                
LRR      DS    0H                                                               
         CLC   CONHEAD(30),=CL30'* NOTE * NO RECORDS SELECTED *'                
         BE    LRR100                                                           
         SPACE                                                                  
         MVI   UNITMKT,0           RESET UNIT MARKETS                           
         CLI   ALLCLI,C'Y'  IF RUNNING FOR ALL CLIENTS, DO THIS LATER           
         BNE   LRR01                                                            
         XC    SVBCLT,SVBCLT                                                    
         B     *+12                                                             
LRR01    BRAS  RE,FCLT                                                          
         BNE   SECLOKER      SECURITY LOCKOUT MESSAGE                           
         SPACE                                                                  
         XC    HOLDALL,HOLDALL                                                  
         XC    TOTUNTS,TOTUNTS                                                  
         MVI   CLRDATE,0                                                        
         SPACE                                                                  
         LM    R0,R1,=A(HEADING,HDHK)                                           
         CLI   TLCMFLG,C'Y'              IF RUNNING TALCOM LIST,                
         BNE   *+8                                                              
         LM    R0,R1,=A(HEADING2,HDHK)   USE TALCOM HEADINGS                    
         A     R0,SPTR49RR                                                      
         ST    R0,SPECS                                                         
         A     R1,SPTR49RR                                                      
         ST    R1,HEADHOOK                                                      
         SPACE                                                                  
         NI    SVFLAG,X'FF'-ERREPRT  TURN OFF ERROR REPORT                      
         SPACE                                                                  
         TM    SVOPTSW,OPTDOWN     DOWNLOADING                                  
         BZ    LRR06                NO                                          
         XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
         MVI   LINE,1                                                           
         SPACE                                                                  
         MVC   P(37),=C'"MEDIA" "NAME" "CLIENT" "CLIENT NAME"'                  
         LA    R2,P+38                                                          
         SPACE                                                                  
         CLI   SVMGRPS,0           BY MARKET GROUP                              
         BE    *+14                                                             
         MVC   0(14,R2),=C'"MARKET GROUP"'                                      
         LA    R2,15(,R2)                                                       
         SPACE                                                                  
         MVC   0(12,R2),=C'"COMMERCIAL"'                                        
         LA    R2,13(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(8,R2),=C'"LENGTH"'                                             
         LA    R2,9(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(18,R2),=C'"COMMERCIAL TITLE"'                                  
         LA    R2,19(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(12,R2),=C'"COMMERCIAL"'                                        
         LA    R2,13(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(8,R2),=C'"LENGTH"'                                             
         LA    R2,9(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(18,R2),=C'"COMMERCIAL TITLE"'                                  
         LA    R2,19(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(12,R2),=C'"COMMERCIAL"'                                        
         LA    R2,13(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(8,R2),=C'"LENGTH"'                                             
         LA    R2,9(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(18,R2),=C'"COMMERCIAL TITLE"'                                  
         LA    R2,19(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(12,R2),=C'"COMMERCIAL"'                                        
         LA    R2,13(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(8,R2),=C'"LENGTH"'                                             
         LA    R2,9(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(18,R2),=C'"COMMERCIAL TITLE"'                                  
         LA    R2,19(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(6,R2),=C'"PROD"'                                               
         LA    R2,7(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(14,R2),=C'"PRODUCT NAME"'                                      
         LA    R2,15(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(6,R2),=C'"PART"'                                               
         LA    R2,7(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(14,R2),=C'"PARTNER NAME"'                                      
         LA    R2,15(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         CLI   OPTEST,0                                                         
         BE    *+14                                                             
         MVC   0(10,R2),=C'"ESTIMATE"'                                          
         LA    R2,11(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(8,R2),=C'"MARKET"'                                             
         TM    SVOPTSW,OPTCABLE    CABLE STATION ONLY                           
         BZ    *+10                                                             
         MVC   0(8,R2),=C'"CABLE "'                                             
         SPACE                                                                  
         LA    R2,9(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(7,R2),=C'"ALPHA"' ALPHA MARKET CODE FOR MEDIA RADIO            
         CLI   TRAMED,C'R'                                                      
         BE    *+10                                                             
         MVC   0(7,R2),=C'"NSI  "' NSI MARKET CODE FOR MEDIA TV                 
         LA    R2,8(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(8,R2),=C'"TALMKT"' TALENT MARKET CODE                          
         LA    R2,9(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVC   0(13,R2),=C'"MARKET NAME"'                                       
         LA    R2,14(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         TM    SVOPTSW,OPTUNIT                                                  
         BZ    LRR02                                                            
         SPACE                                                                  
         MVC   0(5,R2),=C'"UNITS"'                                              
         LA    R2,6(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
LRR02    MVC   0(14,R2),=C'"ACTIVE DATES"'                                      
         LA    R2,15(,R2)                                                       
         SPACE                                                                  
         CLI   OPTEST,0                                                         
         BNE   LRR04                                                            
         TM    SVOPTSW,OPTESTP                                                  
         BZ    LRR04                                                            
         MVC   0(21,R2),=C'"ESTIMATE(S) COVERED"'                               
         LA    R2,22(,R2)                                                       
         SPACE                                                                  
LRR04    MVI   0(R2),X'5E'         SEMI COLON                                   
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
LRR06    L     R3,CMLSTA                                                        
         AR    R3,R9                                                            
         CLI   OFFLINE,C'Y'        IF OFFLINE, SORTING, NO TABLE                
         BNE   LRR10                                                            
         XC    CMLENT,CMLENT                                                    
         BAS   RE,GSRT             GO GET SORT RECORD                           
         BNE   LRR90                                                            
         SPACE                                                                  
         OC    CMLCLI,CMLCLI       BETTER BE A CLIENT                           
         BNZ   *+6                                                              
         DC    H'00'                                                            
         SPACE                                                                  
         OC    CMLCML,CMLCML       BETTER BE AN ENTRY                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
LRR10    CLI   ALLCLI,C'Y'         RUNNING FOR ALL CLIENTS?                     
         BNE   LRR12                                                            
         CLC   SVBCLT,CMLCLI       NEW CLIENT?                                  
         BE    LRR12                                                            
LRR11    MVC   SVBCLT,CMLCLI                                                    
         XC    SVCMLCML,SVCMLCML   CLEAR COMMERCIAL FROM PREV CLIENT            
         BRAS  RE,FCLT                                                          
         BE    LRR12                                                            
LRR11A   BRAS  RE,GSRT             IF SECURITY LOCKOUT - GET NEXT REC           
         BNE   LRR90                                                            
         CLC   SVBCLT,CMLCLI       IF MORE RECS, CHECK FOR NEW CLIENT           
         BE    LRR11A              IF SAME CLIENT, GET NEXT REC                 
         B     LRR11               IF NEW CLIENT, TRY AGAIN                     
*                                                                               
LRR12    CLC   SVCMLMGR,CMLMGR                                                  
         BNE   LRR14                                                            
*                                                                               
         CLC   SVCMLCML,CMLCML     COMPARES BOTH COMMLS                         
         BNE   LRR14                                                            
*                                                                               
         CLI   CBLFLAG,C'Y'                                                     
         BE    LRR13                                                            
***      OC    CMLCSTA,CMLCSTA     STATION?                                     
***      BZ    LRR20                                                            
         CLI   CMLCBL,C'C'         STATION?                                     
         BNE   LRR20                                                            
         B     LRR14               NEW PAGE IF GOING FROM STA TO CBL            
*                                                                               
***13    OC    CMLCSTA,CMLCSTA     CABLE?                                       
***      BNZ   LRR20               NEW PAGE IF GOING FROM CBL TO STA            
LRR13    CLI   CMLCBL,C'C'         CABLE?                                       
         BE    LRR20               NEW PAGE IF GOING FROM CBL TO STA            
         SPACE                                                                  
LRR14    MVI   CBLFLAG,0           CABLE FLAG                                   
***      OC    CMLCSTA,CMLCSTA                                                  
***      BZ    *+8                                                              
         CLI   CMLCBL,C'C'         CABLE                                        
         BNE   *+8                                                              
         MVI   CBLFLAG,C'Y'        SET CABLE FLAG FOR HEADHOOKS                 
         SPACE                                                                  
         TM    SVOPTSW,OPTDOWN     IS THIS DOWNLOADING                          
         BO    LRR16                YES, NO FORCE HEADINGS                      
         SPACE                                                                  
         TM    SVFLAG,BNFNDSW      BUY NOT FOUND SW                             
         BZ    *+14                                                             
         XC    TOTUNTS,TOTUNTS                                                  
         B     LRR16                                                            
         SPACE                                                                  
         TM    SVOPTSW,OPTUNIT                                                  
         BZ    LRR16                                                            
         TM    UNITMKT,UNITMKNY+UNITMKCH+UNITMKLA                               
         BNZ   LRR14AA                                                          
         OC    TOTUNTS,TOTUNTS                                                  
         BZ    LRR16                                                            
         SPACE                                                                  
LRR14AA  MVC   PMKTNM(16),=C'COMMERCIAL TOTAL'                                  
         EDIT  (B4,TOTUNTS),(7,PMKTUNTS-1),COMMAS=YES,ZERO=NOBLANK              
*                                                                               
         TM    UNITMKT,UNITMKNY                                                 
         BZ    LRR14A                                                           
         MVC   PMKTMKTN,=C'NY'                                                  
LRR14A   TM    UNITMKT,UNITMKCH                                                 
         BZ    LRR14B                                                           
         MVC   PMKTMKTC,=C'CH'                                                  
LRR14B   TM    UNITMKT,UNITMKLA                                                 
         BZ    LRR14C                                                           
         MVC   PMKTMKTL,=C'LA'                                                  
*                                                                               
LRR14C   GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         CLC   SVMKT,CMLMKT        SAME MARKET?                                 
         BE    *+8                                                              
         MVI   UNITMKT,0           RESET UNIT MARKETS                           
         XC    TOTUNTS,TOTUNTS     RESET COMMERCIAL TOTALS                      
*                                                                               
         CLC   SVMKT,CMLMKT        SAME MARKET?                                 
         BNE   LRR16                                                            
         MVC   MKTUNTS,SVMKTUNT    PRESET UNIT IF SAME MKT                      
         SR    R0,R0                                                            
         ICM   R0,3,MKTUNTS                                                     
         A     R0,TOTUNTS                                                       
         ST    R0,TOTUNTS                                                       
*                                                                               
LRR16    MVC   SVCMLMGR,CMLMGR                                                  
         MVC   SVCMLCML,CMLCML                                                  
         TM    SVOPTSW,OPTDOWN     IS THIS DOWNLOADING                          
         BO    LRR20                YES, NO FORCE HEADINGS                      
         MVI   FORCEHED,C'Y'                                                    
         SPACE                                                                  
LRR20    CLC   SVMKT,CMLMKT                                                     
         BE    LRR21                                                            
         SPACE                                                                  
         MVC   SVMKT,CMLMKT                                                     
         SPACE                                                                  
         SR    R0,R0                                                            
         ICM   R0,3,CMLMKT                                                      
         CVD   R0,DUB                                                           
         UNPK  QMKT,DUB                                                         
         OI    QMKT+3,X'F0'                                                     
         BRAS  RE,FMKT                                                          
         SPACE                                                                  
         MVC   PMKT,QMKT                                                        
         MVC   PMKTNM,MKTNM                                                     
         OC    PMKTNM,SPACES                                                    
         SPACE                                                                  
LRR21    MVC   PMKTALPH(L'MKTALPH),MKTALPH    PRINT ALPHA MARKET CODE,          
         CLI   TRAMED,C'R'                    IF MEDIA RADIO                    
         BE    LRR22                                                            
         XC    PMKTALPH,PMKTALPH                                                
         OC    SVMNSI,SVMNSI       IF MEDIA TV, PRINT NSI MARKET CODE           
         BZ    LRR22                                                            
         ZICM  R0,SVMNSI,2                                                      
         CVD   R0,DUB                                                           
         UNPK  FULL,DUB                                                         
         OI    FULL+3,X'F0'                                                     
         MVC   PMKTALPH,FULL       PRINT NSI CODE IF MEDIA TV                   
         SPACE                                                                  
LRR22    DS   0H                                                                
         MVC   PTALMKT,SVMTALM     PRINT TALENT MARKET CODE                     
         OC    PTALMKT,SPACES      IF WE HAVE ONE                               
         SPACE                                                                  
         MVI   PMKT+4,C' '         CLEAR FOR NON-CABLE                          
         MVC   PMKT+5(3),SPACES    CLEAR FOR NON-CABLE                          
         SPACE                                                                  
         CLI   CBLFLAG,C'Y'        CABLE STATION?                               
         BE    LR22A                                                            
         TM    SVOPTSW,OPTCABLE    CABLE STATION ONLY                           
         BZ    LRR23                                                            
         SPACE                                                                  
LR22A    XC    DUB(5),DUB                                                       
         MVC   DUB+2(3),CMLCSTA                                                 
         SPACE                                                                  
         GOTO1 MSUNPK,DMCB,(X'80',DUB),WORK,WORK+4                              
         SPACE                                                                  
         MVC   PMKT,WORK+4           PRINT SYSTEM CODE                          
         LA    R1,BIGKEY                                                        
         USING STAPACKD,R1                                                      
         MVI   PMKT+4,C'/'                                                      
         MVC   PMKT+5(3),STAPQNET    PRINT CABLE NET (IF ANY)                   
         DROP  R1                                                               
         SPACE                                                                  
LRR23    DS   0H                                                                
         LA    R2,PMKTUNTS                                                      
         SPACE                                                                  
         TM    SVOPTSW,OPTUNIT                                                  
         BZ    LRR24                                                            
         SPACE                                                                  
         EDIT  (B2,SVMKTUNT),(6,(R2)),MINUS=NO,COMMAS=YES                       
         TM    SVOPTSW,OPTDOWN     IS THIS DOWNLOADING                          
         BO    *+10                DON'T CLEAR MKTUNTS HERE                     
         XC    MKTUNTS,MKTUNTS                                                  
         LA    R2,10(,R2)                                                       
         SPACE                                                                  
LRR24    DS   0H                                                                
         CLI   CBLFLAG,C'Y'        CABLE STATION?                               
         BE    *+12                                                             
         TM    SVOPTSW,OPTCABLE    CABLE STATION ONLY                           
         BZ    LRR26                                                            
         SPACE                                                                  
         OC    CMLINDX,CMLINDX     MUST HAVE INDEX TO CABLE GROUP NAME          
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         ICM   RE,3,CMLINDX                                                     
         BCTR  RE,R0                                                            
         MHI   RE,CBLNAML          LENGTH OF 1 ENTRY                            
         LR    RF,RE                                                            
         A     RF,CBLNAM           ADDR OF TABLE                                
         MVC   0(24,R2),0(RF)                                                   
         LA    R2,30(,R2)                                                       
         SPACE                                                                  
LRR26    XC    BLOCK(256),BLOCK                                                 
         XC    BLOCK+256(256),BLOCK+256                                         
         MVC   BLOCK(1),CMLEST                                                  
         MVC   BLOCK+1(1),CMLPRD                                                
         SPACE                                                                  
LRR30    MVC   HOLDALL,CMLENT      SAVE ENTIRE                                  
         SPACE                                                                  
LRR31    CLI   OFFLINE,C'Y'        IF OFFLINE, SORTING, NO TABLE                
         BNE   LRR34                                                            
         SPACE                                                                  
         BAS   RE,GSRT             GO GET SORT RECORD                           
         BNE   LRR50               END OF SORT                                  
         B     LRR36                                                            
         SPACE                                                                  
LRR34    LA    R3,CMLNEXT          BUMP TABLE POINTER                           
         SPACE                                                                  
         OC    CMLENT,CMLENT       END OF TABLE                                 
         BZ    LRR50                                                            
         SPACE                                                                  
* IF SAME ENTRY EXCEPT EST #, JUST SAVE & PRINT ESTIMATE                        
         SPACE                                                                  
LRR36    DS    0H                                                               
***      OC    CMLCSTA,CMLCSTA                                                  
***      BNZ   LRR36A                                                           
         CLI   CMLCBL,C'C'                                                      
         BNE   LRR36A                                                           
         TM    SVOPTSW,OPTCABLE    CABLE STATION ONLY                           
         BZ    *+18                                                             
LRR36A   CLC   CMLCOMPA,HOLDCOMP   MGR/CMLS/MARKET/CABLE NAME/DATES             
         BNE   LRR40                                                            
         B     LRR38                                                            
         SPACE                                                                  
***      CLC   CMLCOMPA(CMLCSTA-CMLCOMPA),HOLDCOMP                              
         CLC   CMLCOMPA(CMLFTD-CMLCOMPA),HOLDCOMP                               
         BNE   LRR40                                                            
         CLC   CMLFTD(CMLNEXT-CMLFTD),HOLDCOMP+(CMLFTD-CMLCOMPA)                
         BNE   LRR40                                                            
         SPACE                                                                  
LRR38    BRAS  RE,SEST             GO SAVE ESTIMATE (IF ANY)                    
         SPACE                                                                  
*NOP     B     LRR30                                                            
         B     LRR31                                                            
         SPACE                                                                  
* COMBINE OVERLAPPING OR CONSEQUTIVE DATES                                      
         SPACE                                                                  
***40    OC    CMLCSTA,CMLCSTA                                                  
***      BNZ   LRR40A                                                           
LRR40    CLC   CMLPRD(CMLNEXT-CMLPRD),CMLPRD-CMLENT+HOLDALL                     
         BNE   LRR50               ARE PRODUCTS/END OF RECORD THE SAME?         
         CLI   CMLCBL,C'C'                                                      
         BE    LRR40A                                                           
         TM    SVOPTSW,OPTCABLE    CABLE STATION ONLY                           
         BZ    *+18                                                             
LRR40A   CLC   CMLCOMP,HOLDCOMP    MGRP/CMLS/MARKET/CABLE NAME                  
         BNE   LRR50                                                            
         B     LRR41                                                            
         SPACE                                                                  
***      CLC   CMLCOMP(CMLCSTA-CMLCOMP),HOLDCOMP                                
         CLC   CMLCOMP(CMLFTD-CMLCOMP),HOLDCOMP                                 
         BNE   LRR50                                                            
         SPACE                                                                  
* DO DATES OVERLAP - SINCE SORT IS ON DATES, FTD OF THIS REC                    
* HAS TO BE EQUAL OR HIGHER THAN THE LAST                                       
         SPACE                                                                  
LRR41    CLC   CMLLTD-CMLENT+HOLDALL,CMLFTD   NO DATE OVERLAP                   
         BL    LRR42                                                            
         CLC   CMLFTD-CMLENT+HOLDALL,CMLFTD                                     
         BNH   *+10                                                             
         MVC   CMLFTD-CMLENT+HOLDALL,CMLFTD  SAVE LOWER START DATE              
         SPACE                                                                  
         CLC   CMLLTD-CMLENT+HOLDALL,CMLLTD  IS LAST DATE HIGHER                
         BNL   LRR38                                                            
         MVC   CMLLTD-CMLENT+HOLDALL,CMLLTD  SAVE HIGHER END DATE               
         B     LRR38                                                            
         SPACE                                                                  
LRR42    CLC   CMLLTD-CMLENT+HOLDALL,CMLFTD                                     
         BNL   LRR38                                                            
         GOTO1 DATCON,DMCB,(2,CMLFTD),(0,WORK)                                  
         LA    R4,CMLLTD-CMLENT+HOLDALL                                         
         GOTO1 (RF),(R1),(2,(R4)),(0,WORK+6)                                    
         GOTO1 ADDAY,(R1),WORK+6,WORK+12,F'1'                                   
         CLC   WORK(6),WORK+12                                                  
         BNE   LRR50                                                            
         SPACE                                                                  
         CLC   CMLLTD,CMLLTD-CMLENT+HOLDALL                                     
         BNH   LRR38                                                            
         MVC   CMLLTD-CMLENT+HOLDALL,CMLLTD  SAVE LATEST LTD                    
         B     LRR38                                                            
         SPACE                                                                  
LRR50    DS    0H                                                               
         NI    SVFLAG,X'FF'-BNFNDSW  INIT FLAG                                  
         BRAS  RE,GBUY             GET BUY ENTRY FOR THIS                       
         BE    LRR52                                                            
         OI    SVFLAG,BNFNDSW                                                   
         SPACE                                                                  
LRR52    MVI   CLRDATE,0                                                        
         TM    SVOPTSW,OPTCABLE   SKIP IF OPTION CABLE                          
         BO    LRR53                                                            
         CLI   CMLCBL-CMLENT+HOLDALL,C'C'   OR CABLE STATION                    
         BE    LRR53                                                            
         CLI   CMLCBL,C'C'         OR IF NEXT ONE IS CABLE                      
         BE    LRR53                                                            
         CLC   CMLCLI,CMLCLI-CMLENT+HOLDALL   SAME CLIENT?                      
         BNE   LRR53                                                            
         CLC   CMLCML(16),CMLCML-CMLENT+HOLDALL  SAME COMMERCIALS?              
         BNE   LRR53                                                            
         CLC   CMLMKT,CMLMKT-CMLENT+HOLDALL   SAME MARKET?                      
         BNE   LRR53                                                            
         MVI   CLRDATE,C'N'         DON'T CLEAR BDATELST                        
         B     LRR58                DON'T PRINT DATES YET                       
         SPACE                                                                  
LRR53    LA    R1,BDATELST         LIST OF DATES TO PRINT                       
         SPACE                                                                  
LRR54    OC    0(4,R1),0(R1)       ANY MORE DATES                               
         BZ    LRR58                                                            
         CLI   0(R1),X'FF'         THIS DATE ALREADY PRINTED                    
         BE    LRR55                YES                                         
         SPACE                                                                  
         MVC   CMLFTD-CMLENT+HOLDALL(4),0(R1) MOVE IN BUY DTES                  
         MVI   0(R1),X'FF'                                                      
         B     LRR56                                                            
         SPACE                                                                  
LRR55    LA    R1,4(R1)                                                         
         B     LRR54               ** I AM NOT CHECKING EOL                     
         SPACE                                                                  
LRR56    LA    R4,CMLFTD-CMLENT+HOLDALL                                         
         CLC   PERSTRP,0(R4)                                                    
         BNH   *+10                                                             
         MVC   0(2,R4),PERSTRP                                                  
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(2,(R4)),(5,(R2))                                    
         MVI   8(R2),C'-'                                                       
         GOTO1 (RF),(R1),(2,(R4)),(1,MYSTART)  USE IN WORKER FILE               
*                                                                               
         LA    R4,CMLLTD-CMLENT+HOLDALL                                         
         CLC   PERENDP,0(R4)                                                    
         BNL   *+10                                                             
         MVC   0(2,R4),PERENDP                                                  
         SPACE                                                                  
         GOTO1 (RF),(R1),(2,(R4)),(5,9(R2))                                     
         GOTO1 (RF),(R1),(2,(R4)),(1,MYEND)    USE IN WORKER FILE               
*                                                                               
         MVC   TALCMID,CMLCML-CMLENT+HOLDALL      COMMERCIAL 1                  
         BRAS  RE,PUTCOM           PUT COMMERCIALS TO WORKER FILE               
         OC    MYPIGGY,MYPIGGY     IF WE HAVE PIGGYBACKS, WE SHOULD             
         BNZ   LRR57               NOT HAVE PIGGYBACK IN CML2                   
*                                                                               
         CLC   CMLCML2-CMLENT+HOLDALL,SPACES                                    
         BE    LRR57                                                            
         OC    CMLCML2-CMLENT+HOLDALL,CMLCML2-CMLENT+HOLDALL                    
         BZ    LRR57               DOES PRODUCT 2 HAVE ITS OWN COMM?            
         MVC   TALCMID,CMLCML2-CMLENT+HOLDALL     COMMERCIAL 2                  
         BRAS  RE,PUTWRKR          PUT SORT RECORD TO WORKER FILE               
*                                                                               
LRR57    MVC   HOLDATES,0(R2)                                                   
         SPACE                                                                  
         TM    SVOPTSW,OPTDOWN     IS THIS DOWNLOADING                          
         BO    LRR60                YES                                         
         SPACE                                                                  
         BRAS  RE,PEST             GO PRINT ESTIMATE(S)                         
         B     LRR52                                                            
         SPACE                                                                  
LRR58    OC    CMLENT,CMLENT       END OF TABLE                                 
         BZ    LRR90                YES                                         
         B     LRR10                                                            
         SPACE                                                                  
LRR60    MVC   P,SPACES                                                         
         LA    R2,P                                                             
         MVI   0(R2),C'"'                                                       
         MVC   1(1,R2),QMED                                                     
         MVI   2(R2),C'"'                                                       
         LA    R2,4(,R2)                                                        
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         MVC   1(L'MEDNM,R2),MEDNM                                              
         OC    1(L'MEDNM,R2),SPACES                                             
         SPACE                                                                  
         LA    R0,L'MEDNM          LENGTH OF FIELD                              
         BAS   RE,BLKQ             GO BLANK DOUBLE QUOTES                       
         SPACE                                                                  
         LA    RF,1+L'MEDNM(,R2)                                                
         BAS   RE,DBLK                                                          
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         MVC   1(L'QCLT,R2),QCLT                                                
         LA    RF,1+L'QCLT(,R2)                                                 
         BAS   RE,DBLK                                                          
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         MVC   1(L'CLTNM,R2),CLTNM                                              
         SPACE                                                                  
         LA    R0,L'CLTNM          LENGTH OF FIELD                              
         BAS   RE,BLKQ             GO BLANK DOUBLE QUOTES                       
         SPACE                                                                  
         LA    RF,1+L'CLTNM(,R2)                                                
         BAS   RE,DBLK                                                          
         SPACE                                                                  
         CLI   SVMGRPS,0           BY MARKET GROUP                              
         BE    LRR64                                                            
         MVI   0(R2),C'"'                                                       
         MVC   1(1,R2),SVMGRPS                                                  
         SR    R0,R0                                                            
         ICM   R0,3,SVCMLMGR                                                    
         SLL   R0,4                                                             
         STCM  R0,7,DUB                                                         
         OI    DUB+2,X'0F'                                                      
         UNPK  2(4,R2),DUB(3)                                                   
         MVI   6(R2),C'"'                                                       
         LA    R2,8(,R2)                                                        
         SPACE                                                                  
LRR64    DS   0H                                                                
         MVC   TALCMID,SVCMLCML                                                 
         BRAS  RE,READCML              GET PIGGYBACKS                           
         OC    MYPIGGY,MYPIGGY                                                  
         BZ    LRR65                                                            
         LA    R1,4                                                             
         LA    R5,MYPIGGY                                                       
LRR64A   OC    0(L'TALCMID,R5),0(R5)                                            
         BZ    LRR65B                                                           
         BRAS  RE,FCML                                                          
         MVI   0(R2),C'"'                                                       
         BAS   RE,DOWNCOM                                                       
         LA    R5,L'TALCMID(R5)                                                 
         BCT   R1,LRR64A                                                        
         B     LRR70                                                            
         SPACE                                                                  
LRR65    LA    R5,SVCMLCML                                                      
         BRAS  RE,FCML                                                          
         MVI   0(R2),C'"'                                                       
         BAS   RE,DOWNCOM                                                       
         SPACE                                                                  
         OC    SVCMLCML+12(12),SVCMLCML+12                                      
         BNZ   LRR66                                                            
         LA    R1,3                    NO PIGGYS = 3 BLANK COMMLS               
LRR65B   BRAS  RE,DOWNCOM2             IF NO MORE PIGGYBACKS, MAKE              
         BCT   R1,LRR65B               REMAINING COMML FIELDS BLANK             
         B     LRR70                                                            
         SPACE                                                                  
LRR66    LA    R5,SVCMLCML+12                                                   
         BRAS  RE,FCML                                                          
         MVI   0(R2),C'"'                                                       
         MVC   1(12,R2),SVCMLCML+12                                             
         MVI   13(R2),C'"'                                                      
         LA    R2,15(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         ZIC   R0,SVCMLSLN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(3,R2),DUB                                                      
         MVI   4(R2),C'"'                                                       
         LA    R2,6(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         MVC   1(L'SVCMLDSC,R2),SVCMLDSC                                        
         SPACE                                                                  
         LA    R0,L'SVCMLDSC       LENGTH OF FIELD                              
         BAS   RE,BLKQ             GO BLANK DOUBLE QUOTES                       
         SPACE                                                                  
         LA    RF,1+L'SVCMLDSC(,R2)                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R2,2(,RF)                                                        
         SPACE                                                                  
         MVC   0(L'SVCMLDS2,R2),SVCMLDS2                                        
         OC    0(L'SVCMLDS2,R2),SPACES                                          
         SPACE                                                                  
         BCTR  R2,0                                                             
         LA    R0,L'SVCMLDS2       LENGTH OF FIELD                              
         BAS   RE,BLKQ             GO BLANK DOUBLE QUOTES                       
         LA    R2,1(,R2)                                                        
         SPACE                                                                  
         LA    RF,1+L'SVCMLDS2(,R2)                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R2,2(,RF)                                                        
         SPACE                                                                  
         MVC   0(L'SVCMLDS3,R2),SVCMLDS3                                        
         OC    0(L'SVCMLDS3,R2),SPACES                                          
         SPACE                                                                  
         BCTR  R2,0                                                             
         LA    R0,L'SVCMLDS3       LENGTH OF FIELD                              
         BAS   RE,BLKQ             GO BLANK DOUBLE QUOTES                       
         LA    R2,1(,R2)                                                        
         SPACE                                                                  
         LA    RF,1+L'SVCMLDS3(,R2)                                             
         BAS   RE,DBLK                                                          
         SPACE                                                                  
LRR70    DS    0H                                                               
         MVI   0(R2),C'"'                                                       
         MVC   1(L'QPRD,R2),QPRD                                                
         OC    1(L'QPRD,R2),SPACES                                              
         LA    RF,1+L'QPRD(,R2)                                                 
         BAS   RE,DBLK                                                          
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         MVC   1(L'PRDNM,R2),PRDNM                                              
         OC    1(L'PRDNM,R2),SPACES                                             
         SPACE                                                                  
         LA    R0,L'PRDNM          LENGTH OF FIELD                              
         BAS   RE,BLKQ             GO BLANK DOUBLE QUOTES                       
         SPACE                                                                  
         LA    RF,1+L'PRDNM(,R2)                                                
         BAS   RE,DBLK                                                          
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         CLI   BPRD2,0                                                          
         BNE   LRR72                                                            
         OC    1(9,R2),SPACES                                                   
         MVI   2(R2),C'"'          PRD CODE                                     
         MVI   4(R2),C'"'                                                       
         MVI   6(R2),C'"'          PRD NAME                                     
         LA    R2,8(,R2)                                                        
         B     LRR74                                                            
         SPACE                                                                  
LRR72    MVC   1(L'QPRD2,R2),QPRD2                                              
         LA    RF,1+L'QPRD(,R2)                                                 
         BAS   RE,DBLK                                                          
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         MVC   1(L'PRD2NM,R2),PRD2NM                                            
         OC    1(L'PRD2NM,R2),SPACES                                            
         SPACE                                                                  
         LA    R0,L'PRD2NM         LENGTH OF FIELD                              
         BAS   RE,BLKQ             GO BLANK DOUBLE QUOTES                       
         SPACE                                                                  
         LA    RF,1+L'PRD2NM(,R2)                                               
         BAS   RE,DBLK                                                          
         SPACE                                                                  
LRR74    DS    0H                                                               
         CLI   OPTEST,0                                                         
         BE    LRR76                                                            
         MVI   0(R2),C'"'                                                       
         ZIC   R0,OPTEST                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(3,R2),DUB                                                      
         MVI   4(R2),C'"'                                                       
         LA    R2,6(,R2)                                                        
         SPACE                                                                  
LRR76    DS    0H                                                               
         TM    SVOPTSW,OPTCABLE                                                 
         BO    *+12                                                             
         CLI   CBLFLAG,C'Y'        CABLE STATION?                               
         BNE   LRR76A                                                           
         MVI   0(R2),C'"'                                                       
         XC    DUB,DUB                                                          
         MVC   DUB+2(3),CMLCSTA-CMLENT+HOLDALL                                  
         GOTO1 MSUNPK,DMCB,(X'80',DUB),WORK,WORK+4                              
         MVC   1(4,R2),WORK+4        SYSTEM CODE                                
         LA    R1,BIGKEY                                                        
         USING STAPACKD,R1                                                      
         MVI   5(R2),C'/'                                                       
         MVC   6(3,R2),STAPQNET      CABLE NET (IF ANY)                         
         MVI   9(R2),C'"'                                                       
         LA    R2,11(,R2)                                                       
         BAS   RE,CKFULL                                                        
         B     LRR76B                                                           
         DROP  R1                                                               
LRR76A   MVI   0(R2),C'"'                                                       
         MVC   1(4,R2),QMKT                                                     
         MVI   5(R2),C'"'                                                       
         LA    R2,7(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
LRR76B   MVI   0(R2),C'"'                                                       
         MVC   1(L'MKTALPH,R2),MKTALPH   ALPHA MARKET CODE                      
         OC    1(4,R2),SPACES                                                   
         CLI   TRAMED,C'R'               MEDIA RADIO ONLY                       
         BE    LRR77                                                            
*                                                                               
         ZICM  R0,SVMNSI,2                                                      
         CVD   R0,DUB                                                           
         UNPK  FULL,DUB                                                         
         OI    FULL+3,X'F0'                                                     
         MVC   1(4,R2),FULL      NSI MARKET CODE                                
         OC    1(4,R2),SPACES                                                   
LRR77    MVI   5(R2),C'"'                                                       
         LA    R2,7(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         MVC   1(L'SVMTALM,R2),SVMTALM   TALENT MARKET CODE                     
         OC    1(L'SVMTALM,R2),SPACES                                           
         MVI   5(R2),C'"'                                                       
         LA    R2,7(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         MVC   1(L'MKTNM,R2),MKTNM                                              
         OC    1(L'MKTNM,R2),SPACES                                             
         SPACE                                                                  
         LA    R0,L'MKTNM          LENGTH OF FIELD                              
         BAS   RE,BLKQ             GO BLANK DOUBLE QUOTES                       
         SPACE                                                                  
         LA    RF,L'MKTNM+1(,R2)                                                
         BAS   RE,DBLK                                                          
         SPACE                                                                  
         TM    SVOPTSW,OPTUNIT                                                  
         BZ    LRR78                                                            
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         EDIT  (B2,MKTUNTS),(6,1(R2)),MINUS=NO,COMMAS=YES,ALIGN=LEFT            
         XC    MKTUNTS,MKTUNTS                                                  
         LA    RF,7(,R2)                                                        
         BAS   RE,DBLK                                                          
         SPACE                                                                  
LRR78    MVI   0(R2),C'"'                                                       
         MVC   1(5,R2),HOLDATES    KEEP DOWN LOAD TO MMMDA                      
         MVI   6(R2),C'-'                                                       
         MVC   7(8,R2),HOLDATES+9                                               
         MVI   1+14(R2),C'"'                                                    
         LA    R2,2+14(,R2)                                                     
         SPACE                                                                  
         OC    BLOCK(256),BLOCK                                                 
         BZ    LRR86                                                            
         SPACE                                                                  
         MVI   LINE,1                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         LA    R2,P                                                             
         LA    R4,P+125                                                         
         MVI   0(R2),C'"'                                                       
         LA    R5,BLOCK                                                         
         LA    R2,1(,R2)                                                        
         SPACE                                                                  
LRR80    EDIT  (B1,(R5)),(3,0(R2)),ALIGN=LEFT                                   
*NOP     LA    R5,1(,R5)                                                        
         LA    R5,2(,R5)                                                        
         AR    R2,R0                                                            
         CLI   0(R5),0                                                          
         BE    LRR84                                                            
         MVI   0(R2),C','                                                       
         LA    R2,1(,R2)                                                        
         CR    R4,R2                                                            
         BH    LRR80                                                            
         SPACE                                                                  
         MVI   LINE,1                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,P                                                             
         B     LRR80                                                            
         SPACE                                                                  
LRR84    MVI   0(R2),C'"'                                                       
         LA    R2,1(,R2)                                                        
         SPACE                                                                  
LRR86    MVI   1(R2),X'5E'         SEMI COLON                                   
         SPACE                                                                  
         MVI   LINE,1                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         B     LRR52                                                            
*                                                                               
***      OC    CMLENT,CMLENT       END OF TABLE                                 
***      BZ    LRR90                                                            
         SPACE                                                                  
***      B     LRR10                                                            
         SPACE                                                                  
LRR90    CLI   UPDATE,C'N'                                                      
         BE    *+8                                                              
         BRAS  RE,CLOSWRKR         CLOSE WORKER FILE                            
         TM    SVOPTSW,OPTDOWN     DOWNLOADING                                  
         BZ    LRR96                                                            
         MVI   P,X'7A'             COLON AS END MARKER TO DOWN LOAD             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         B     EXIT                                                             
         SPACE                                                                  
LRR96    TM    SVOPTSW,OPTUNIT                                                  
         BZ    LRR100                                                           
         TM    UNITMKT,UNITMKNY+UNITMKCH+UNITMKLA                               
         BNZ   LRR97                                                            
         OC    TOTUNTS,TOTUNTS                                                  
         BZ    LRR100                                                           
         SPACE                                                                  
LRR97    TM    SVFLAG,BNFNDSW      BUY NOT FOUND SW                             
         BZ    *+14                                                             
         XC    TOTUNTS,TOTUNTS                                                  
         B     LRR100                                                           
         SPACE                                                                  
         MVC   PMKTNM(16),=C'COMMERCIAL TOTAL'                                  
         EDIT  (B4,TOTUNTS),(7,PMKTUNTS-1),COMMAS=YES,ZERO=NOBLANK              
*                                                                               
         TM    UNITMKT,UNITMKNY                                                 
         BZ    LRR97A                                                           
         MVC   PMKTMKTN,=C'NY'                                                  
LRR97A   TM    UNITMKT,UNITMKCH                                                 
         BZ    LRR97B                                                           
         MVC   PMKTMKTC,=C'CH'                                                  
LRR97B   TM    UNITMKT,UNITMKLA                                                 
         BZ    LRR97C                                                           
         MVC   PMKTMKTL,=C'LA'                                                  
*                                                                               
LRR97C   GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         XC    TOTUNTS,TOTUNTS     RESET COMMERCIAL TOTALS                      
*                                                                               
LRR100   MVI   FORCEHED,C'Y'                                                    
         BRAS  RE,BEREPORT         GENEREATE BUY ERROR REPORT IF ANY            
         SPACE                                                                  
LRRX     B     EXIT                                                             
         SPACE 2                                                                
DOWNCOM  NTR1                                                                   
         MVC   1(8,R2),0(R5)                                                    
         MVI   9(R2),C'"'                                                       
         LA    R2,11(,R2)                                                       
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         ZIC   R0,SVCMLSLN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(3,R2),DUB                                                      
         MVI   4(R2),C'"'                                                       
         LA    R2,6(,R2)                                                        
         BAS   RE,CKFULL                                                        
         SPACE                                                                  
         MVI   0(R2),C'"'                                                       
         MVC   1(L'SVCMLDSC,R2),SVCMLDSC                                        
         SPACE                                                                  
         LA    R0,L'SVCMLDSC       LENGTH OF FIELD                              
         BAS   RE,BLKQ             GO BLANK DOUBLE QUOTES                       
         SPACE                                                                  
         LA    RF,1+L'SVCMLDSC(,R2)                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R2,2(,RF)                                                        
         SPACE                                                                  
         MVC   0(L'SVCMLDS2,R2),SVCMLDS2                                        
         OC    0(L'SVCMLDS2,R2),SPACES                                          
         SPACE                                                                  
         BCTR  R2,0                                                             
         LA    R0,L'SVCMLDS2       LENGTH OF FIELD                              
         BAS   RE,BLKQ             GO BLANK DOUBLE QUOTES                       
         LA    R2,1(,R2)                                                        
         SPACE                                                                  
         LA    RF,1+L'SVCMLDS2(,R2)                                             
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    R2,2(,RF)                                                        
         SPACE                                                                  
         MVC   0(L'SVCMLDS3,R2),SVCMLDS3                                        
         OC    0(L'SVCMLDS3,R2),SPACES                                          
         SPACE                                                                  
         BCTR  R2,0                                                             
         LA    R0,L'SVCMLDS3       LENGTH OF FIELD                              
         BAS   RE,BLKQ             GO BLANK DOUBLE QUOTES                       
         LA    R2,1(,R2)                                                        
         SPACE                                                                  
         LA    RF,1+L'SVCMLDS3(,R2)                                             
         BAS   RE,DBLK                                                          
         SPACE                                                                  
         XIT1  REGS=(R2)                                                        
         LTORG                                                                  
         EJECT                                                                  
CKFULL   LA    R0,P+70                                                          
         CR    R2,R0                                                            
         BL    CKFULLX                                                          
         SPACE                                                                  
         LR    R0,RE                                                            
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,P                                                             
         LR    RE,R0                                                            
CKFULLX  BR    RE                                                               
         SPACE 2                                                                
* DROP ALL BLANKS ON RIGHT                                                      
         SPACE                                                                  
DBLK     CLI   0(RF),C' '                                                       
         BH    DBLK10                                                           
         BCT   RF,DBLK                                                          
         DC    H'0'                                                             
DBLK10   CLI   0(RF),C'"'                                                       
         BNE   DBLK20                                                           
         LA    RF,1(,RF)           ALLOW 1 BLK BETWEEN APOSTROPHES              
         SPACE                                                                  
DBLK20   MVI   1(RF),C'"'                                                       
         LA    R2,3(,RF)                                                        
         LA    R0,P+70                                                          
         CR    R2,R0                                                            
         BLR   RE                                                               
         SPACE                                                                  
         LR    R0,RE                                                            
         MVI   LINE,1                                                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R2,P                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE                                                                  
         LA    R0,L'MKTNM          LENGTH OF FIELD                              
         SPACE 2                                                                
* BLANK ALL DOUBLE QUOTES IN FIELD *                                            
         SPACE                                                                  
BLKQ     LA    RF,1(,R2)                                                        
         SPACE                                                                  
BLKQ10   CLI   0(RF),C'"'                                                       
         BNE   BLKQ20                                                           
         MVI   0(RF),C' '                                                       
BLKQ20   LA    RF,1(,RF)                                                        
         BCT   R0,BLKQ10                                                        
         BR    RE                                                               
         EJECT                                                                  
* VALIDATE COMMERCIAL RECORD *                                                  
         SPACE                                                                  
VCML     NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    EXIT                                                             
         CLC   =C'ALL',8(R2)                                                    
         BE    EXIT                                                             
         CLI   ALLCLI,C'Y'         IF RUNNING FOR ALL CLIENTS,                  
         BE    ERRINV              COMMERCIAL IS NOT ALLOWED                    
         GOTO1 ANY                                                              
         CLI   5(R2),8                                                          
         BL    VCML10                                                           
         BE    VCML05                                                           
         CLI   5(R2),12            AD-ID COMML MUST BE 9 - 12 CHARS             
         BH    CMLENER                                                          
*                                                                               
         GOTO1 ATRPACK,DMCB,(C'P',WORK),DUB                                     
         BNE   BADCOMM                                                          
         MVC   SVCMML,WORK         SAVE UNPACKED COMMERCIAL                     
         OC    SVCMML,SPACES                                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0AC1'                                                  
         MVC   KEY+2(3),BAGYMD AND BCLT                                         
         MVC   KEY+5(8),DUB                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BADCOMM                                                          
         GOTO1 GETREC                                                           
         L     RF,AIO                                                           
         MVC   SVCMMLP,5(RF)       SAVE PACKED COMMERCIAL                       
         MVI   SVCMMLN,7           LENGTH                                       
         B     EXIT                                                             
*                                                                               
VCML05   GOTO1 ATRPACK,DMCB,(C'P',WORK),DUB                                     
         BNE   *+10                SPECIAL CHARACTERS CANNOT BE PACKED          
         MVC   SVCMMLP,DUB         SAVE PACKED COMMERCIAL                       
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(3),BAGYMD AND BCLT                                         
         MVC   KEY+5(8),WORK                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   BADCOMM                                                          
*                                                                               
VCML10   MVC   SVCMML,WORK                                                      
         OC    SVCMML,SPACES                                                    
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         STC   R1,SVCMMLN                                                       
         B     EXIT                                                             
         SPACE 3                                                                
* VALIDATE PERIOD                                                               
         SPACE                                                                  
VPER     NTR1                                                                   
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
*                                                                               
         BRAS  RE,VSFTDAT          VALIDATE USING SOFDAT                        
         BNE   VPER05                                                           
         CLI   OFFLINE,C'Y'                                                     
         BE    VPER05                                                           
         CLI   CONOUTH+4,0         RLP ONLINE, IF GROUP NAME IN OUTPUT          
         BNE   VPERX                                                            
         CLC   =C'FILE',CONDEST    AND FILE IN DESTINATION                      
         BE    VPERX                                                            
*                                                                               
VPER05   XC    PEREND,PEREND                                                    
         LA    R3,8(,R2)           START DATE                                   
         GOTO1 DATVAL,DMCB,(R3),WORK                                            
         L     R5,DMCB             GET LENGTH OF FIELD                          
         LTR   R5,R5                                                            
         BZ    DATERR                                                           
         LA    R3,1(R5,R3)         POINT TO END DATE                            
         GOTO1 DATCON,(R1),(0,WORK),(3,PERSTART)                                
         CLM   R5,1,5(R2)          WAS ONLY 1 DATE ENTERED                      
         BE    ONEDATER            YES, ALL DONE                                
         GOTO1 DATVAL,(R1),(R3),WORK+6                                          
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,WORK+6),(3,PEREND)                                
*                                                                               
VPER08   OC    MYDATE,MYDATE       DID TT PROFILE CHANGE START DATE?            
         BZ    VPER09                                                           
         GOTO1 DATCON,DMCB,(0,MYDATE),(3,PERSTART)                              
*                                                                               
VPER09   OC    MYDATE2,MYDATE2     RLP - ABSOLUTE START DATE                    
         BZ    VPER10                                                           
         CLC   PERSTART,MYDATE2                                                 
         BNL   VPER10                                                           
         MVC   PERSTART,MYDATE2                                                 
*                                                                               
VPER10   GOTO1 ADDAY,(R1),WORK,WORK+12,F'366'                                   
         CLC   WORK+6(6),WORK+12                                                
         BH    DTSPRDER            ERROR - DATE SPREAD MORE THAN 1 YEAR         
         SPACE                                                                  
         CLC   PERSTART,PEREND                                                  
         BH    DATERR                                                           
         GOTO1 DATCON,(R1),(3,PERSTART),(2,PERSTRP)                             
         GOTO1 (RF),(R1),(3,PEREND),(2,PERENDP)                                 
VPERX    B     EXIT                                                             
         EJECT                                                                  
* GET SORT RECORDS, ELIMINATING EQUALS *                                        
         SPACE                                                                  
GSRT     NTR1                                                                   
         SPACE                                                                  
         CLI   FIRSTSW,2           EOFSW SET                                    
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
GSRT10   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         SPACE                                                                  
         ICM   RF,15,4(R1)         GET ADDRESS OF REC                           
         BZ    GSRT50               EOF IF ZERO                                 
         SPACE                                                                  
         MVI   FIRSTSW,1                                                        
         SPACE                                                                  
         CLC   HOLDALL,0(RF)        EQUAL REC                                   
         BE    GSRT10                                                           
         SPACE                                                                  
         MVC   CMLENT,0(RF)         MOVE FOR VISIBLITY                          
         SPACE                                                                  
         TM    SVOPTSW,OPTRACE                                                  
         BZ    GSRT20                                                           
         MVC   SVP,P                                                            
         MVC   P,SPACES                                                         
         MVC   P+1(28),CMLENT                                                   
         GOTO1 HEXOUT,DMCB,CMLENT,P+30,30,0,0                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   P,C'H'                                                           
         MVC   P+1(28),HOLDALL                                                  
         GOTO1 HEXOUT,DMCB,HOLDALL,P+30,30,0,0                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P,SVP                                                            
         SPACE                                                                  
GSRT20   DS    0H                                                               
         CR    R0,R0                                                            
         B     EXIT                                                             
         SPACE                                                                  
GSRT50   XC    CMLENT,CMLENT                                                    
         SPACE                                                                  
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
         CLI   FIRSTSW,0           TEST FIRST TIME                              
         BNE   GSRT60              NO                                           
         MVC   P(16),=C'NO INPUT RECORDS'                                       
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         B     EXIT                                                             
         SPACE                                                                  
GSRT60   MVI   FIRSTSW,2           SET EOFSW                                    
         SPACE                                                                  
         B     EXIT                                                             
         EJECT                                                                  
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
         SPACE                                                                  
CLTREQ   XC    CONHEAD,CONHEAD                                                  
         LAY   RE,CLTREQMS                                                      
         MVC   CONHEAD(L'CLTREQMS),0(RE)                                        
         B     ERREXIT                                                          
ONEDATER XC    CONHEAD,CONHEAD                                                  
         LAY   RE,ONEDATMS                                                      
         MVC   CONHEAD(L'ONEDATMS),0(RE)                                        
         B     ERREXIT                                                          
OLER     XC    CONHEAD,CONHEAD                                                  
         LAY   RE,OLERMS                                                        
         MVC   CONHEAD(L'OLERMS),0(RE)                                          
         B     ERREXIT                                                          
DTSPRDER XC    CONHEAD,CONHEAD                                                  
         LAY   RE,DTSPRDMS                                                      
         MVC   CONHEAD(L'DTSPRDMS),0(RE)                                        
         B     ERREXIT                                                          
SPTUSRER XC    CONHEAD,CONHEAD                                                  
         LAY   RE,SPTUSRMS                                                      
         MVC   CONHEAD(L'SPTUSRMS),0(RE)                                        
         B     ERREXIT                                                          
SECLOKER XC    CONHEAD,CONHEAD                                                  
         LAY   RE,SECLOKMS                                                      
         MVC   CONHEAD(L'SECLOKMS),0(RE)                                        
         B     ERREXIT                                                          
ERREXIT  GOTO1 ERREX2                                                           
         SPACE                                                                  
CMLENER  MVI   ERROR,INVCMMLN                                                   
         B     TRAPERR                                                          
DATERR   MVI   ERROR,INVDATE                                                    
         B     TRAPERR                                                          
BADCOMM  MVI   ERROR,INVCOMM                                                    
         B     TRAPERR                                                          
ERRINV   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
MISSPRD  LA    R2,TRAPRDH                                                       
MISSERR  MVI   ERROR,MISSING                                                    
TRAPERR  GOTO1 ERREX                                                            
         SPACE                                                                  
         LTORG                                                                  
HIATUS   DC    CL6'HIATUS',XL2'00'                                              
CLTREQMS DC    C'* ERROR * CLIENT REQUIRED FOR ONLINE LIST *'                   
ONEDATMS DC    C'* ERROR * MUST ENTER BOTH FROM AND TO DATES *'                 
OLERMS   DC    C'* ERROR * ONLINE LIST NOT SUPPORTED *'                         
DTSPRDMS DC    C'* ERROR * CAN NOT RUN FOR MORE THAN 1 YEAR *'                  
SPTUSRMS DC    C'* ERROR * INVALID REQUEST - NOT A CLARUS USER*'                
SECLOKMS DC    C'* ERROR * INVALID CLIENT - SECURITY LOCKOUT*'                  
SORTCARD DC    CL80'SORT FIELDS=(1,38,A),FORMAT=BI '                            
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=47'                                    
         LTORG                                                                  
INSBSCEL EQU   15                                                               
INSSUBEL EQU   7                                                                
HEADING  SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,35,C'SPOT/TALENT TRANSFER'                                    
         SSPEC H2,35,C'--------------------'                                    
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,75,AGYADD                                                     
         SSPEC H3,75,RUN                                                        
         SSPEC H4,75,REQUESTOR                                                  
         SSPEC H4,99,PAGE                                                       
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H3,3,C'CLIENT'                                                   
         SPACE                                                                  
         DC    X'00'               END MARKER FOR SSPECS                        
         SPACE 2                                                                
HEADING2 SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,35,C'TALENT COMMERCIAL ACTIVITY REPORT'                       
         SSPEC H2,35,C'---------------------------------'                       
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,75,AGYADD                                                     
         SSPEC H3,75,RUN                                                        
         SSPEC H4,75,REQUESTOR                                                  
         SSPEC H4,99,PAGE                                                       
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H3,3,C'CLIENT'                                                   
         SPACE                                                                  
         DC    X'00'               END MARKER FOR SSPECS                        
         SPACE 2                                                                
EHDING   SSPEC H1,3,C'TRAFFIC SYSTEM'                                           
         SSPEC H1,35,C'TALENT BUY ACTIVITY ERROR REPORT'                        
         SSPEC H2,35,C'--------------------------------'                        
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,75,AGYADD                                                     
         SSPEC H3,75,RUN                                                        
         SSPEC H4,75,REQUESTOR                                                  
         SSPEC H4,99,PAGE                                                       
         SSPEC H2,3,C'MEDIA'                                                    
         SSPEC H3,3,C'CLIENT'                                                   
         SPACE                                                                  
         DC    X'00'               END MARKER FOR SSPECS                        
         SPACE 2                                                                
         DROP  RB,R7,RC                                                         
         EJECT                                                                  
* READ MARKET RECORD *                                                          
         SPACE                                                                  
FMKT     NMOD1 0,**FMKT**                                                       
         SPACE                                                                  
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),AGENCY                                                  
         SPACE                                                                  
         MVC   AIO,AIO2                                                         
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO                      
         SPACE                                                                  
         MVC   MKTNM,=CL24'**** UNKNOWN ****'                                   
         XC    MKTUNTS,MKTUNTS                                                  
         XC    SVMKTUNT,SVMKTUNT                                                
         L     R1,AIO                                                           
         CLC   KEY(8),0(R1)                                                     
         BNE   FMKTX                                                            
         SPACE                                                                  
         USING MKTRECD,R1                                                       
         MVC   MKTNM,MKTNAME                                                    
*                                                                               
         LA    R2,MKTTALM                                                       
         OC    MKTTALM,MKTTALM     TALENT MARKET                                
         BNZ   FMKT05                                                           
         LA    R2,MKTALF                                                        
         OC    MKTALF,MKTALF       ALPHA MARKET                                 
         BZ    FMKT30                                                           
FMKT05   OC    0(3,R2),SPACES                                                   
         CLC   0(3,R2),=CL3'NY'    IF TALENT/ALPHA MKT IS NY,LA,OR CHI          
         BNE   FMKT10                                                           
         OI    UNITMKT,UNITMKNY    SET APPROPRIATE BIT AND DON'T                
         B     FMKT40              ADD UNITS                                    
FMKT10   CLC   0(3,R2),=CL3'CHI'                                                
         BNE   FMKT20                                                           
         OI    UNITMKT,UNITMKCH                                                 
         B     FMKT40                                                           
FMKT20   CLC   0(3,R2),=CL3'LA'                                                 
         BNE   FMKT30                                                           
         OI    UNITMKT,UNITMKLA                                                 
         B     FMKT40                                                           
                                                                                
FMKT30   SR    R0,R0                                                            
         ICM   R0,3,MKTUNIT                                                     
         STH   R0,MKTUNTS                                                       
         MVC   SVMKTUNT,MKTUNTS    SAVE FOR NEW PAGE SAME MKT DIFF CML          
         A     R0,TOTUNTS                                                       
         ST    R0,TOTUNTS                                                       
*                                                                               
FMKT40   MVC   SVMTALM,MKTTALM   TALENT MARKET                                  
*                                                                               
         CLI   TRAMED,C'T'       IF MEDIA TV                                    
         BNE   *+10                                                             
         MVC   SVMNSI,MKTRSM1    SAVE NSI CODE                                  
*                                                                               
         CLI   TRAMED,C'R'       IF MEDIA RADIO                                 
         BNE   FMKTX                                                            
         MVC   MKTALPH,MKTALST   SAVE ALPHA MARKET CODE                         
FMKTX    J     EXIT                                                             
         DROP  R1,R4,RB,RC                                                      
         EJECT                                                                  
* IF NO INSTR RECAPS FOR A BUY THEN GENERATE BUY ERROR REPORT                   
         SPACE                                                                  
BEREPORT NMOD1 0,**BERE**                                                       
         SPACE                                                                  
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
         LM    R0,R1,=A(EHDING,HDHK)                                            
         A     R0,SPTR49RR                                                      
         ST    R0,SPECS                                                         
         A     R1,SPTR49RR                                                      
         ST    R1,HEADHOOK                                                      
         SPACE                                                                  
         OI    SVFLAG,ERREPRT          ERROR REPORT                             
         SPACE                                                                  
* GET BUY RECORDS FROM TSAR                                                     
         SPACE                                                                  
         LHI   R2,TSARBLK-SYSD                                                  
         AR    R2,R9                                                            
         USING TSARD,R2                                                         
         SPACE                                                                  
         XC    SVMKT,SVMKT                                                      
         XC    SVCSTA,SVCSTA                                                    
         XC    SVDATES,SVDATES                                                  
         SPACE                                                                  
         XC    ELEM(256),ELEM                                                   
         LA    R5,ELEM                                                          
         ST    R5,TSAREC           SET ADDRESS OF THE RECORD                    
         SPACE                                                                  
         LA    R0,1                                                             
         STH   R0,TSRNUM                                                        
         SPACE                                                                  
         MVI   TSOFFACT,TSAGET     SET GET BY NUMBER                            
         B     *+8                                                              
BERE20   MVI   TSOFFACT,TSANXT     SET GET NEXT RECORD                          
         SPACE                                                                  
         GOTO1 TSAROFF,(R2)                                                     
         B     BERE35                                                           
         SPACE                                                                  
BERE30   L     RF,AIO3                                                          
         AHI   RF,-280             END OF AIO2                                  
         MVC   ELEM,0(RF)          MOVE IN NEXT TSAR RECORD                     
         SPACE                                                                  
BERE35   TM    TSERRS,X'80'        END OF FILE                                  
         BO    BEREX                                                            
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         LA    R5,ELEM+2           RECORD IS IN ELEM+2                          
         USING TSRBUYD,R5                                                       
         SPACE                                                                  
         LA    R6,PMKTUNTS                                                      
         SPACE                                                                  
         TM    SVOPTSW,OPTUNIT                                                  
         BZ    *+8                                                              
         LA    R6,10(,R6)                                                       
         SPACE                                                                  
         TM    SVOPTSW,OPTCABLE    CABLE STATION ONLY                           
         BZ    *+8                                                              
         LA    R6,30(,R6)                                                       
         SPACE                                                                  
         TM    TSRFLG,NOBUYFR      NO BUY FOR THIS RECAP                        
         BZ    BERE38                                                           
         SPACE                                                                  
         LA    R3,TSRRFTD          POINT TO RECAP DATES                         
         OC    0(4,R3),0(R3)       BETTER HAVE DATES                            
         BNZ   BERE50                                                           
         DC    H'0'                                                             
         SPACE                                                                  
* SEE IF THERE ARE BUY DATES THAT ARE NOT COVERED BY INSTR RECAP DATES          
         SPACE                                                                  
BERE38   DS    0H                                                               
         TM    TSRFLG,ENTBUY       ENTIRE BY IS COVERED BY RECAP                
         BO    BERE20               YES, GET NEXT RECORD                        
         SPACE                                                                  
         LA    R1,TSRRFTD                                                       
         OC    0(4,R1),0(R1)       ANY RECAP DATES                              
         BNZ   BERE40               YES                                         
         SPACE                                                                  
         OI    SVFLAG,NORECAP      NO RECAP FOR THIS BUY                        
         SPACE                                                                  
* MOVE BUY DATES TO PRINT LINE                                                  
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(2,TSRFTD),(5,0(R6))                                 
         MVI   8(R6),C'-'                                                       
         GOTO1 DATCON,DMCB,(2,TSRLTD),(5,9(R6))                                 
         B     BERE80                                                           
         SPACE                                                                  
BERE40   BAS   RE,UBUYLST          BUILD LIST OF UNCOVERED BUY DATES            
         SPACE                                                                  
         L     R3,AIO3             UNCOVERED DATES LIST                         
         OC    0(4,R3),0(R3)                                                    
         BNZ   BERE50              GO PRINT DATES                               
         SPACE                                                                  
BERE45   TM    TSRFLG,NOBUYFR      NO BUY FOR RECAP                             
         BZ    BERE30                                                           
         SPACE                                                                  
         XC    ELEM(256),ELEM      GET READY TO READ NEXT RECORD                
         SPACE                                                                  
         MVI   TSOFFACT,TSANXT     SET GET NEXT RECORD                          
         SPACE                                                                  
         GOTO1 TSAROFF,(R2)                                                     
         B     BERE35                                                           
         SPACE                                                                  
* PRINT UNCOVERED OR DELETED BUY DATES                                          
         SPACE                                                                  
BERE50   DS    0H                                                               
         TM    TSRFLG,NOBUYFR      NO BUY FOR THIS RECAP                        
         BZ    *+12                                                             
         CLI   SVT1PR13,C'Y'       USE FLT/EST DATES, NOT FTD/LTD               
         BE    BERE75                                                           
         SPACE                                                                  
         OC    0(4,R3),0(R3)       ANY DATES TO PRINT                           
         BZ    BERE75               DONE                                        
         SPACE                                                                  
         CLC   0(2,R3),2(R3)       FTD TO LTD                                   
         BNH   BERE55                                                           
         XC    0(2,R3),2(R3)       SWAP DATES                                   
         XC    2(2,R3),0(R3)                                                    
         XC    0(2,R3),2(R3)                                                    
         SPACE                                                                  
BERE55   GOTO1 DATCON,DMCB,(2,0(R3)),(5,0(R6))                                  
         MVI   8(R6),C'-'                                                       
         GOTO1 DATCON,DMCB,(2,2(R3)),(5,9(R6))                                  
         SPACE                                                                  
BERE75   CLC   P,SPACES            ANYTHING TO PRINT                            
         BNH   BERE45              NO, GET NEXT RECORD                          
         SPACE                                                                  
BERE80   NI    SVFLAG,X'FF'-MKSTCHG RESET MARKET/STATION CHANGED                
         SPACE                                                                  
         CLI   TSRCSTA,CABLESTA    SKIP IF NOT CABLE                            
         BNH   BERE80A                                                          
         CLC   SVCSTA,TSRCSTA      SAME CABLE STATION                           
         BE    BERE81               YES                                         
         SPACE                                                                  
         MVC   SVCSTA,TSRCSTA                                                   
         SPACE                                                                  
BERE80A  TM    SVOPTSW,OPTCABLE    CABLE STATION ONLY                           
         BZ    BERE81                                                           
         OI    SVFLAG,MKSTCHG                                                   
         SPACE                                                                  
BERE81   CLC   SVMKT,TSRMKT        SAME MARKET                                  
         BE    BERE90                                                           
         SPACE                                                                  
         MVC   SVMKT,TSRMKT        SAVE MARKET                                  
         SPACE                                                                  
         OI    SVFLAG,MKSTCHG      MARKET CHANGED                               
         SPACE                                                                  
         SR    R0,R0                                                            
         ICM   R0,3,TSRMKT                                                      
         CVD   R0,DUB                                                           
         UNPK  QMKT,DUB                                                         
         OI    QMKT+3,X'F0'                                                     
         BRAS  RE,FMKT                                                          
         SPACE                                                                  
         MVC   PMKT,QMKT                                                        
         MVC   PMKTNM,MKTNM                                                     
         SPACE                                                                  
BERE90   DS   0H                                                                
         TM    SVOPTSW,OPTCABLE    CABLE STATION ONLY                           
         BZ    BERE100                                                          
         SPACE                                                                  
         TM    SVFLAG,MKSTCHG      SAME CABLE STATION                           
         BZ    BERE100              YES                                         
         SPACE                                                                  
         XC    DUB(5),DUB                                                       
         MVC   DUB+2(3),TSRCSTA                                                 
         SPACE                                                                  
         GOTO1 MSUNPK,DMCB,(X'80',DUB),WORK,WORK+4                              
         SPACE                                                                  
         MVC   PMKT,WORK+4                                                      
         LA    R1,BIGKEY                                                        
         USING STAPACKD,R1                                                      
         MVI   PMKT+4,C'/'                                                      
         MVC   PMKT+5(3),STAPQNET    PRINT CABLE NET (IF ANY)                   
         SPACE                                                                  
         DROP  R1                                                               
         SPACE                                                                  
BERE100  DS    0H                                                               
         TM    TSRFLG,NOBUYFR      NO BUY FOR THIS RECAP                        
         BZ    *+10                                                             
         MVC   18(11,R6),=C'**DELETED**'                                        
         SPACE                                                                  
         TM    SVOPTSW,OPTCABLE    CABLE STATION ONLY                           
         BZ    BERE115                                                          
         SPACE                                                                  
         TM    SVFLAG,MKSTCHG      MARKET CHANGED                               
         BZ    BERE115              NO                                          
         SPACE                                                                  
         LA    R6,PMKTUNTS                                                      
         SPACE                                                                  
         TM    SVOPTSW,OPTUNIT                                                  
         BZ    *+8                                                              
         LA    R6,10(,R6)                                                       
         SPACE                                                                  
         BAS   RE,FCABLE           SOMEDAY CBLFND WILL DO THE JOB               
         SPACE                                                                  
         OC    SVCBLIND,SVCBLIND   MUST HAVE INDEX TO CABLE GROUP NAME          
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         ICM   RE,3,SVCBLIND                                                    
         BCTR  RE,R0                                                            
         MHI   RE,CBLNAML          LENGTH OF 1 ENTRY                            
         LR    RF,RE                                                            
         A     RF,CBLNAM           ADDR OF TABLE                                
         SPACE                                                                  
         MVC   0(24,R6),0(RF)      CABLE SYSTEM NAME                            
         LA    R6,30(,R6)                                                       
         SPACE                                                                  
BERE115  DS    0H                                                               
         TM    SVFLAG,MKSTCHG      MARKET CHANGED                               
         BO    BERE118              YES                                         
         SPACE                                                                  
         TM    SVFLAG,NORECAP      NO RECAP FOR THESE DATES                     
         BZ    BERE117                                                          
         SPACE                                                                  
         CLC   SVDATES,TSRFTD      SAME DATES?                                  
         BE    BERE120              YES, DONE                                   
         SPACE                                                                  
         MVC   SVDATES,TSRFTD                                                   
         B     BERE118                                                          
         SPACE                                                                  
BERE117  CLC   SVDATES,0(R3)       SAME DATES?                                  
         BE    BERE119                                                          
         MVC   SVDATES,0(R3)       SAVE THESE DATES                             
         SPACE                                                                  
BERE118  DS    0H                                                               
         MVC   SVDATES,0(R3)       PRESET WITH RECAP DATES                      
         SPACE                                                                  
         TM    SVFLAG,NORECAP      NO RECAP                                     
         BZ    *+10                                                             
         MVC   SVDATES,TSRFTD      MOVE IN BUY DATES                            
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
         OI    SVFLAG,ERRPRTD      AT LEAST ONE LINE PRINTED                    
         SPACE                                                                  
         TM    SVFLAG,NORECAP      ALL DATES ARE PRINTED                        
         BO    BERE120              YES                                         
         B     *+10                                                             
BERE119  MVC   0(29,R6),SPACES     DATES ALREADY PRINTED                        
         SPACE                                                                  
         LA    R3,4(R3)            BUMP TO NEXT SET OF DATES                    
         B     BERE50                                                           
         SPACE                                                                  
BERE120  NI    SVFLAG,X'FF'-NORECAP                                             
         B     BERE20              GET NEXT BUY FROM TSAR                       
         SPACE                                                                  
BEREX    DS    0H                                                               
         TM    SVFLAG,ERRPRTD      AT LEAST ONE LINE PRINTED                    
         BO    BEREX10                                                          
*                                                                               
         XC    SPECS,SPECS         NO,CLEAR ALL                                 
         XC    HEADHOOK,HEADHOOK                                                
         MVI   FORCEHED,C'N'                                                    
*                                                                               
**NO-OP  CLC   CONHEAD(30),=CL30'* NOTE * NO RECORDS SELECTED *'                
**OCT/10 BNE   BEREXX                                                           
**NO-OP  GOTO1 ERREX2                                                           
         B     BEREXX                                                           
*                                                                               
BEREX10  MVI   P1,0                                                             
         MVC   P2(44),=C'* FOR MORE DETAILED INFO RUN BUYACT REPORT *'          
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
BEREXX   XIT1                                                                   
         EJECT                                                                  
* BUILD A LIST OF BUY DATES THAT ARE NOT COVERED BY RECAP DATES                 
         SPACE                                                                  
UBUYLST  NTR1                                                                   
         SPACE                                                                  
         XC    SVTSRMKT,SVTSRMKT   INIT SAVE MARKET AREA                        
         SPACE                                                                  
         L     RE,AIO3             CLEAR UNCOVERED BUY DATES LIST AREA          
         LA    RF,2500                                                          
         XCEFL                                                                  
         SPACE                                                                  
         LA    RE,SVBUYDTE         CLEAR SVBUYDTE                               
         LA    RF,602                                                           
         XCEFL                                                                  
**       XC    SVBUYDTE(200),SVBUYDTE                                           
**       XC    SVBUYDTE+200(200),SVBUYDTE+200                                   
**       XC    SVBUYDTE+400(202),SVBUYDTE+400                                   
         SPACE                                                                  
UBU20    XC    BDATELST(200),BDATELST  INIT LIST OF BUY DATES                   
         XC    BDATELST+200(200),BDATELST+200                                   
         XC    BDATELST+400(202),BDATELST+400                                   
         SPACE                                                                  
         MVC   SVTSRMKT,TSRMKT     SAVE MARKET                                  
         SPACE                                                                  
         LA    R7,TSRRFTD          RECAP DATES                                  
         SPACE                                                                  
         ZIC   R4,ELEM,2           TOTAL RECORD LEN                             
         AHI   R4,-L'TSRENT        MINUS FIXED REC LEN                          
         SRL   R4,2                DIVIDE BY 4                                  
         LTR   R4,R4                                                            
         BZ    UBU90               NO RECAP DATES FOR THIS BUY                  
         STC   R4,BYTE                                                          
         CLI   BYTE,1                                                           
         BE    UBU30               ONLY ONE SET OF RECAP DATES                  
         SPACE                                                                  
         BRAS  RE,SRTDTES          GO SORT DATES                                
         SPACE                                                                  
         BRAS  RE,EXPDTES          GO EXPAND THE DATES WHERE POSSIBLE           
         SPACE                                                                  
* CREATE A LIST OF BUYS NOT COVERED BY RECAP                                    
         SPACE                                                                  
UBU30    DS    0H                                                               
         SPACE                                                                  
         LA    R4,BDATELSZ         WILL FIT UPTO 200 BUY DATES                  
         LA    R3,BDATELST         BUY DATES LIST                               
         SPACE                                                                  
         LA    R7,TSRRFTD          RECAP DATES                                  
         SPACE                                                                  
         CLC   TSRFTD(4),0(R7)     BUY DATES TO RECAP DATES                     
         BNE   *+14                                                             
         MVC   BDATELST(2),=X'FFFF' ALL DATES ARE COVERED                       
         B     UBU100                                                           
         SPACE                                                                  
         MVC   0(4,R3),TSRFTD      MOVE BUY DATE TO TABLE                       
         SPACE                                                                  
         LA    R6,SVBUYDTE         SAVE UNCOVERED BUYS                          
         SPACE                                                                  
* SEE THAT BUY DATES FALL WITHIN RECAP DATE RANGE                               
         SPACE                                                                  
UBU50    CLC   0(2,R3),2(R7)       BUY FTD TO RECAP LTD                         
         BH    UBU80                BYPASS, BUMP TO NEXT BUY DATES              
         SPACE                                                                  
         CLC   2(2,R3),0(R7)       BUY LTD TO RECAP FTD                         
         BL    UBU80                BYPASS                                      
         SPACE                                                                  
         CLC   0(2,R3),0(R7)                                                    
         BNL   UBU60                                                            
         SPACE                                                                  
* BUY FTD NOT COVERED BY RECAP                                                  
         SPACE                                                                  
         MVC   0(2,R6),0(R3)       BUY FTD                                      
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(2,0(R7)),(0,WORK)  RECAP FTD                        
         GOTO1 ADDAY,(R1),WORK,WORK+6,F'-1'    MINUS 1                          
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,WORK+12)  = UNCOVERED BUY LTD          
         SPACE                                                                  
         MVC   2(2,R6),WORK+12     SAVE BUY LTD                                 
         LA    R6,4(R6)                                                         
         SPACE                                                                  
UBU60    CLC   2(2,R3),2(R7)       BUY LTD TO RECAP LTD                         
         BNH   UBU80                                                            
         SPACE                                                                  
* LTD NOT COVERED BY RECAP                                                      
         SPACE                                                                  
UBU70    GOTO1 DATCON,DMCB,(2,2(R7)),(0,WORK)  RECAP LTD                        
         GOTO1 ADDAY,(R1),WORK,WORK+6,F'1'     PLUS 1                           
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,WORK+12)  = UNCOVERED BUY FTD          
         SPACE                                                                  
         MVC   0(2,R6),WORK+12     SAVE BUY FTD                                 
         MVC   2(2,R6),2(R3)       AND LTD                                      
         SPACE                                                                  
         LA    R6,4(R6)            BUMP IN BUY TABLE                            
         SPACE                                                                  
UBU80    LA    R3,4(R3)            BUMP TO NEXT SET OF BUY DATES                
         SPACE                                                                  
         OC    0(4,R3),0(R3)       ANY BUYS                                     
         BNZ   UBU50                                                            
         SPACE                                                                  
         XC    BDATELST(200),BDATELST CLEAR OLD BUY DATES                       
         XC    BDATELST+200(200),BDATELST+200                                   
         XC    BDATELST+400(202),BDATELST+400                                   
         SPACE                                                                  
         OC    SVBUYDTE(4),SVBUYDTE ANY UNCOVERED BUY DATES                     
         BNZ   *+14                                                             
         MVC   BDATELST(2),=X'FFFF' ALL DATES ARE COVERED                       
         B     UBU100                                                           
         SPACE                                                                  
         LA    R1,SVBUYDTE         START OF TABLE                               
         LR    RF,R6                                                            
         SR    RF,R1               DISPLACEMENT FORM START                      
         BCTR  RF,0                MINUS 1 FOE EX MOVE                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BDATELST(0),0(R1)   MOVE IN UNCOVERED BUYS                       
         SPACE                                                                  
         LA    RE,SVBUYDTE         CLEAR SVBUYDTE                               
         LA    RF,602                                                           
         XCEFL                                                                  
**       XC    SVBUYDTE(200),SVBUYDTE                                           
**       XC    SVBUYDTE+200(200),SVBUYDTE+200                                   
**       XC    SVBUYDTE+400(202),SVBUYDTE+400                                   
         SPACE                                                                  
         LA    R7,4(R7)            BUMP TO NEXT RECAP DATES                     
         OC    0(4,R7),0(R7)                                                    
         BZ    UBU100                                                           
         SPACE                                                                  
         LA    R3,BDATELST                                                      
         B     UBU50                                                            
         SPACE                                                                  
* NO RECAP FOR THIS BUY                                                         
         SPACE                                                                  
UBU90    MVC   BDATELST(4),TSRFTD  MOVE BUY FTD/LTD                             
         LA    R6,3                LEN-1                                        
         SPACE                                                                  
UBU100   DS    0H                                                               
         L     RF,AIO3             SAVE UNCOVERED BUY DATES HERE                
         SPACE                                                                  
UBU110   OC    0(4,RF),0(RF)                                                    
         BZ    UBU130                                                           
         LA    RF,4(RF)                                                         
         B     UBU110                                                           
         SPACE                                                                  
UBU130   DS    0H                                                               
         CLC   BDATELST(2),=X'FFFF' ANY DATES                                   
         BNE   UBU140               YES                                         
         SPACE                                                                  
         SR    R6,R6                                                            
         B     UBU150                                                           
         SPACE                                                                  
UBU140   DS    0H                                                               
         LA    RE,BDATELST                                                      
         LR    R6,RE                                                            
         SPACE                                                                  
UBU145   CLC   0(4,R6),0(R6)                                                    
         BZ    UBU148                                                           
         LA    R6,4(R6)                                                         
         B     UBU145                                                           
         SPACE                                                                  
UBU148   SR    R6,RE                                                            
         BCTR  R6,0                                                             
         SPACE                                                                  
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),BDATELST                                                 
         SPACE                                                                  
         LA    R6,1(R6)                                                         
         SPACE                                                                  
UBU150   AR    R6,RF               POINT TO THE END OF DATE LIST                
         SPACE                                                                  
         L     RF,AIO3                                                          
         AHI   RF,-280             END OF AIO2                                  
         MVC   0(256,RF),ELEM      SAVE PREV TSAR RECORD                        
         SPACE                                                                  
         XC    ELEM(256),ELEM                                                   
         SPACE                                                                  
         MVI   TSOFFACT,TSANXT     SET GET NEXT RECORD                          
         SPACE                                                                  
         GOTO1 TSAROFF,(R2)                                                     
         SPACE                                                                  
         TM    TSERRS,X'80'        END OF FILE                                  
         BO    UBU180                                                           
         SPACE                                                                  
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         LA    R5,ELEM+2           RECORD IS IN ELEM+2                          
         USING TSRBUYD,R5                                                       
         SPACE                                                                  
         CLC   SVTSRMKT,TSRMKT     SAME MARKET?                                 
         BE    UBU20                                                            
         SPACE                                                                  
UBU180   DS    0H                                                               
         L     RF,AIO3                                                          
         AHI   RF,-280             END OF AIO2                                  
         XC    ELEM,0(RF)          SWAP RECORDS                                 
         XC    0(256,RF),ELEM                                                   
         XC    ELEM,0(RF)          RESTORE PREV TSAR RECORD TO ELEM             
         SPACE                                                                  
         L     RF,AIO3                                                          
         SR    R6,RF               LENGTH OF DATE LIST                          
         SPACE                                                                  
         LTR   R6,R6               NO DATES TO REPORT ON                        
         BZ    UBUX                                                             
         SPACE                                                                  
         SRL   R6,2                DIVIDE BY 4 = # OF ENTRIES                   
         SPACE                                                                  
         LTR   R6,R6               NO DATES                                     
         BZ    UBUX                                                             
         SPACE                                                                  
         STC   R6,BYTE                                                          
         CLI   BYTE,1              ONLY ONE ENTRY                               
         BE    UBUX                                                             
         SPACE                                                                  
         LR    R4,R6                                                            
         L     R7,AIO3                                                          
         SPACE                                                                  
         BRAS  RE,SRTDTES          GO SORT DATES                                
         SPACE                                                                  
         BRAS  RE,EXPDTES          GO EXPAND THE DATES WHERE POSSIBLE           
         SPACE                                                                  
UBUX     XIT1                                                                   
         EJECT                                                                  
* FIND CABLE STATION NAME AND SAVE CABLE STATION, INDEX, AND NAME               
         SPACE                                                                  
FCABLE   NTR1                                                                   
         SPACE                                                                  
         L     R2,CBLSTB                                                        
         USING CBLSTAD,R2                                                       
         SPACE                                                                  
FCABL10  OC    CBLSTA,CBLSTA                                                    
         BZ    FCABL50             THIS MUST BE BUY W/NO RECAP                  
         SPACE                                                                  
         CLC   CBLSTA,TSRCSTA      THIS THE SAME STATION                        
         BE    FCABL20                                                          
         LA    R2,CBLNEXT                                                       
         C     R2,CBLSTBX                                                       
         BL    FCABL10                                                          
         DC    H'0'                                                             
         SPACE                                                                  
FCABL20  MVC   SVCBLIND,CBLINDEX                                                
         B     BEREXX                                                           
         SPACE                                                                  
FCABL50  DS    0H                                                               
         MVC   CBLSTA,TSRCSTA      SAVE STATION                                 
         XC    DUB,DUB                                                          
         MVC   DUB(2),TSRMKT                                                    
         MVC   DUB+2(3),TSRCSTA                                                 
         GOTO1 MSUNPK,DMCB,(X'80',DUB),WORK,WORK+4                              
         SPACE                                                                  
         LA    R1,BIGKEY                                                        
         USING STAPACKD,R1                                                      
         MVC   FULL(3),STAPQNET    SAVE CABLE NET (IF ANY)                      
         DROP  R1                                                               
         SPACE                                                                  
         LA    R6,KEY                                                           
         USING STARECD,R6                                                       
         XC    KEY,KEY                                                          
         MVI   STAKTYPE,STAKTYPQ                                                
         MVC   STAKMED,QMED                                                     
         MVC   STAKCALL,WORK+4                                                  
         MVI   STAKCALL+4,C'T'                                                  
         MVC   STAKAGY,AGENCY                                                   
*        MVC   STAKCLT,QCLT                                                     
         MVC   STAKCLT,=C'000'     AMS CDS NOT IN CLT SPECIFIC REC              
         MVC   STAKFILL,=C'000'                                                 
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,(R6)                     
         SPACE                                                                  
         CLC   KEY(9),0(R6)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         LA    R0,1                                                             
         L     R3,CBLNAM                                                        
         SPACE                                                                  
FCABL60  DS   0H                                                                
         OC    0(24,R3),0(R3)                                                   
         BZ    FCABL70                                                          
         CLC   SSYSNAME,0(R3)      THIS THE CABLE SYSTEM NAME                   
         BE    FCABL70                                                          
         LA    R3,CBLNAML(,R3)                                                  
         AHI   R0,1                                                             
         C     R3,CBLNAMX                                                       
         BL    FCABL60                                                          
         DC    H'0'                                                             
         SPACE                                                                  
FCABL70  DS   0H                                                                
         MVC   0(24,R3),SSYSNAME   SAVE THE CABLE SYSTEM NAME                   
         STH   R0,SVCBLIND                                                      
         STCM  R0,3,CBLINDEX                                                    
         SPACE                                                                  
         MVC   SVCBLIND,CBLINDEX                                                
         B     BEREXX                                                           
         EJECT                                                                  
************************************                                            
* PUT DATES IN ORDER                                                            
* (R4=NO. OF DATE ENTRIES TO PROCESS                                            
*  R7 POINTS TO THE LIST OF DATES)                                              
************************************                                            
         SPACE                                                                  
SRTDTES  NMOD1 0,**SRTD**                                                       
         SPACE                                                                  
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
         LR    RF,R4               NUMBER OF ENTRIES TO PROCESS                 
         LR    R1,R4                                                            
         SPACE                                                                  
SRT10    BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    SRTX                                                             
         SPACE                                                                  
         LA    RE,4(R7)            POINT TO NEXT ENTRY                          
         SPACE                                                                  
SRT15    OC    0(4,RE),0(RE)       ANY ENTRY                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   0(2,R7),0(RE)                                                    
         BNH   SRT20                                                            
         XC    0(4,RE),0(R7)       SWAP DATES                                   
         XC    0(4,R7),0(RE)                                                    
         XC    0(4,RE),0(R7)                                                    
SRT20    LA    RE,4(RE)            BUMP TO NEXT SET OF DATES                    
         BCT   R1,SRT15                                                         
         SPACE                                                                  
         LA    R7,4(R7)                                                         
         BCTR  RF,0                                                             
         LR    R1,RF                                                            
         LTR   R1,R1                                                            
         BP    SRT10                                                            
         SPACE                                                                  
SRTX     XIT1                                                                   
         SPACE                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
**********************************                                              
* EXPAND DATE RANGE WHERE POSSIBLE                                              
* (R4 = NUMBER OF ENTRIES                                                       
*  R7 POINTS TO LIST OF DATES                                                   
**********************************                                              
         SPACE                                                                  
EXPDTES  NMOD1 0,**EXPD**                                                       
         SPACE                                                                  
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         BCTR  R4,0                # OF ENTRIES MINUS 1                         
         SPACE                                                                  
         LR    R5,R7               SAVE START OF LIST OF DATES                  
         LA    R3,4(R7)                                                         
         SPACE                                                                  
EXPD10   CLC   0(4,R7),0(R3)       SAME DATE RANGE                              
         BE    EXPD30               YES                                         
         SPACE                                                                  
         CLC   2(2,R7),0(R3)       LTD TO FTD                                   
         BNL   EXPD20              GO EXPAND DATES                              
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(2,2(R7)),(0,WORK)                                   
         GOTO1 ADDAY,(R1),WORK,WORK+6,F'1'                                      
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,WORK+12)                               
         SPACE                                                                  
         CLC   WORK+12(2),0(R3)                                                 
         BNE   EXPD40                                                           
         SPACE                                                                  
EXPD20   CLC   2(2,R7),2(R3)                                                    
         BH    *+10                                                             
         MVC   2(2,R7),2(R3)                                                    
         SPACE                                                                  
* ELIMINATE MERGED DATE FROM THE LIST                                           
         SPACE                                                                  
EXPD30   LR    RE,R4               # OF ENTRIES                                 
         BCTR  RE,0                                                             
         SLL   RE,2                TIMES 4 (LEN OF EACH ENTRY)                  
         SPACE                                                                  
         LR    RF,R3                                                            
         AR    RF,RE               SAVE END OF LIST POINTER                     
         SPACE                                                                  
         BCTR  RE,0                ADJUST FOR EX MOVE                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),4(R3)       SHIFT DATE OVER                              
         SPACE                                                                  
         XC    0(4,RF),0(RF)       CLEAR LAST ENTRY                             
         SPACE                                                                  
         BCT   R4,EXPD10                                                        
         B     EXPD45              DONE ALL                                     
         SPACE                                                                  
EXPD40   DS    0H                                                               
         LA    R7,4(R7)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,EXPD10                                                        
         SPACE                                                                  
EXPD45   XC    0(4,R3),0(R3)       END OF TABLE                                 
         SPACE                                                                  
EXPD50   DS    0H                                                               
         OC    0(4,R5),0(R5)       ANY MORE DATES                               
         BZ    EXPDX               DONE                                         
         CLC   0(2,R5),2(R5)       FTD TO LTD                                   
         BNH   EXPD60                                                           
         XC    0(2,R5),2(R5)       SWAP DATES                                   
         XC    2(2,R5),0(R5)                                                    
         XC    0(2,R5),2(R5)                                                    
EXPD60   LA    R5,4(R5)                                                         
         B     EXPD50                                                           
         SPACE                                                                  
EXPDX    XIT1                                                                   
         SPACE                                                                  
         DROP  RB,RC                                                            
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
* VALIDATE OPTIONS                                                              
         SPACE                                                                  
* VALID OPTIONS ARE  ESTIMATE, EST=, MGROUP, CABLE, DOWNLOAD, *                 
*                    ENAME, UNITS, STATION, TRACE             *                 
         SPACE                                                                  
VOPT     NMOD1 0,**VOPT**                                                       
         SPACE                                                                  
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
         MVI   SVOPTSW,0                                                        
         MVI   SVMGRPS,0                                                        
         MVI   OPTEST,0                                                         
         XC    SVGRPKEY,SVGRPKEY                                                
         XC    SVMGR,SVMGR                                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VOPTX               NO                                           
         CLI   8(R2),C'?'          HELP                                         
         BE    VOPTHLP             YES                                          
         CLI   5(R2),4                                                          
         BNH   VOPT02                                                           
         LA    R1,3                                                             
         B     VOPT04                                                           
VOPT02   ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
VOPT04   EX    R1,VOPTCLCJ         HELP                                         
         BE    VOPTHLP                                                          
         SPACE                                                                  
         LA    R4,BLOCK            ADDRESS OF FIRST BLOCK                       
         GOTO1 SCANNER,DMCB,TRAOPTH,(7,(R4))                                    
         ZIC   R3,DMCB+4           GET NUMBER OF BLOCKS                         
         LTR   R3,R3               SEE IF SCANNER FOUND ANYTHING                
         BZ    MISSER1             NO                                           
VOPT10   ZIC   R1,0(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         SPACE                                                                  
         EX    R1,VOPTCLCA         ESTIMATE                                     
         BNE   VOPT20                                                           
         SPACE                                                                  
         CLI   SVPROF11,C'E'       USING COPY CODE = ESTIMATE                   
         BNE   INVESTOP                                                         
         SPACE                                                                  
         CLI   1(R4),0             WAS ESTIMATE NUMBER ENTERED                  
         BNE   VOPT16               YES                                         
         OI    SVOPTSW,OPTESTP                                                  
         B     VOPT90                                                           
         SPACE                                                                  
VOPT16   CLI   ALLCLI,C'Y'         IF RUNNING FOR ALL CLIENTS,?                 
         BE    ESTERR              EST= NOT ALLOWED                             
         TM    3(R4),X'80'         IS THIS NUMERIC                              
         BZ    NUMERR                                                           
         CLC   8(4,R4),=F'255'                                                  
         BH    ESTVALER                                                         
         OC    8(4,R4),8(R4)                                                    
         BZ    ESTVALER                                                         
         MVC   OPTEST,11(R4)                                                    
         B     VOPT90                                                           
         SPACE                                                                  
VOPT20   EX    R1,VOPTCLCB         MGROUP                                       
         BNE   VOPT30                                                           
         SPACE                                                                  
         BAS   RE,VMG              GO VALIDATE MARKET GROUP                     
         SPACE                                                                  
         OI    SVOPTSW,OPTMGRP                                                  
         B     VOPT90                                                           
         SPACE                                                                  
VOPT30   EX    R1,VOPTCLCD         DOWN LOAD                                    
         BNE   VOPT40                                                           
         SPACE                                                                  
         OI    SVOPTSW,OPTDOWN                                                  
         OI    REQRTYP,REQTDOWN    SET DOWNLOAD                                 
         MVC   CONOUT(4),=C'DOWN'                                               
         MVI   CONOUTH+5,4                                                      
         OI    CONOUTH+6,X'80'                                                  
         B     VOPT90                                                           
         SPACE                                                                  
VOPT40   EX    R1,VOPTCLCU         SHOW UNITS FROM MARKET RECORD                
         BNE   VOPT50                                                           
         OI    SVOPTSW,OPTUNIT                                                  
         SPACE                                                                  
         B     VOPT90                                                           
         SPACE                                                                  
VOPT50   EX    R1,VOPTCLCC         SHOW CABLE STATIONS ONLY                     
         BNE   VOPT60                                                           
         SPACE                                                                  
         CLI   QMED,C'T'           MUST BE TV                                   
         BNE   MEDERR                                                           
         SPACE                                                                  
         TM    SVOPTSW,OPTSTA      CANNOT HAVE OPT CAB WITH OPT STA             
         BO    OPTSERR                                                          
         SPACE                                                                  
         OI    SVOPTSW,OPTCABLE                                                 
         B     VOPT90                                                           
         SPACE                                                                  
VOPT60   EX    R1,VOPTCLCS         SHOW STATIONS ONLY                           
         BNE   VOPT70                                                           
         SPACE                                                                  
         CLI   QMED,C'T'           MUST BE TV                                   
         BNE   MEDERR                                                           
         SPACE                                                                  
         TM    SVOPTSW,OPTCABLE    CANNOT HAVE OPT CAB WITH OPT STA             
         BO    OPTSERR                                                          
         SPACE                                                                  
         OI    SVOPTSW,OPTSTA                                                   
         SPACE                                                                  
         B     VOPT90                                                           
         SPACE                                                                  
VOPT70   EX    R1,VOPTCLCT         TRACE                                        
         BNE   VOPT80                                                           
         SPACE                                                                  
         OI    SVOPTSW,OPTRACE                                                  
         B     VOPT90                                                           
         SPACE                                                                  
VOPT80   EX    R1,VOPTCLCE         ENAME                                        
         BNE   VOPTHLP                                                          
         SPACE                                                                  
         OI    SVOPTSW,OPTESTP     PRINT ESTIMATE                               
         OI    SVOPTSW,OPTENAME    AND ESTIMATE NAME                            
*                                                                               
VOPT90   LA    R4,32(,R4)          POINT TO NEXT BLOCK                          
         BCT   R3,VOPT10           FOR NUMBER OF BLOCKS FOUND                   
         SPACE                                                                  
VOPTX    XIT1                                                                   
         SPACE                                                                  
VOPTHLP  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'VOPTHMS),VOPTHMS                                       
         B     ERREXT2                                                          
         SPACE 3                                                                
         LTORG                                                                  
         SPACE                                                                  
VOPTCLCA CLC   12(0,R4),=CL9'ESTIMATE '                                         
VOPTCLCB CLC   12(0,R4),=CL7'MGROUP '                                           
VOPTCLCC CLC   12(0,R4),=CL6'CABLE '                                            
VOPTCLCD CLC   12(0,R4),=CL9'DOWNLOAD '                                         
VOPTCLCE CLC   12(0,R4),=CL9'ENAME'                                             
VOPTCLCU CLC   12(0,R4),=CL6'UNITS '                                            
VOPTCLCJ CLC   8(0,R2),=CL4'HELP'                                               
VOPTCLCS CLC   12(0,R4),=CL8'STATION '                                          
VOPTCLCT CLC   12(0,R4),=CL6'TRACE '                                            
VOPTHMS  DC    C'* OPTIONS=EST, EST=NNN, ENAME, MGR, CAB, STA, UNITS *'         
         EJECT                                                                  
* VALIDATE MARKET GROUP FROM PROFILE AND BUILD MARKET GROUP TABLE *             
         SPACE                                                                  
VMG      NTR1                                                                   
         CLI   22(R4),C'A'                                                      
         BL    MGRPERR                                                          
         CLI   22(R4),C'Z'                                                      
         BH    MGRPERR                                                          
         SPACE                                                                  
         CLI   1(R4),1             ONLY 1 CHARACTER ENTRY                       
         BE    MGRPERR                                                          
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(3),BAGYMD & BCLT                                           
         MVC   KEY+8(1),22(R4)                                                  
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA SYSTEM                
         GOTO1 HIGH                                                             
         OC    KEY+5(3),KEY+5      FIND PRODUCT GROUP                           
         BZ    VMG20                                                            
         CLC   KEY(5),KEYSAVE                                                   
         BE    VMG10                                                            
         SPACE                                                                  
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLIENT                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE      FIND THIS AGENCY/MEDIA                       
         BNE   MGRPERR                                                          
         SPACE                                                                  
VMG10    CLC   KEY+8(1),22(R4)                                                  
         BE    VMG30                                                            
         MVC   KEY+8(1),22(R4)                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
VMG20    CLC   KEY(9),KEYSAVE     FIND A MARKET GROUP                           
         BE    VMG30                                                            
         SPACE                                                                  
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLIENT                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE      FIND THIS AGENCY/MEDIA                       
         BNE   BDMGRPER                                                         
         SPACE                                                                  
VMG30    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTFIL' SWITCH TO SPOT MEDIA SYSTEM                
         GOTO1 GETREC                                                           
         CLI   24(R6),01                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MGRPTITL,26(R6)                                                  
         MVI   KEY+1,X'82'                                                      
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA SYSTEM                
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVMGRPS,22(R4)                                                   
         MVC   SVMGR,23(R4)                                                     
         MVC   SVGRPKEY(4),KEY+5                                                
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         B     VOPTX                                                            
         SPACE 3                                                                
         SPACE                                                                  
MEDERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MEDMS),MEDMS                                           
         B     ERREXT2                                                          
OPTSERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BADOPTS),BADOPTS                                       
         B     ERREXT2                                                          
ESTERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BDESTOPT),BDESTOPT                                     
         B     ERREXT2                                                          
MGRPERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MGRPERMS),MGRPERMS                                     
         B     ERREXT2                                                          
BDMGRPER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'BDMGRPMS),BDMGRPMS                                     
         MVC   CONHEAD+20(1),22(R4)                                             
         B     ERREXT2                                                          
ESTVALER MVC   CONHEAD,BADESTMS                                                 
         B     ERREXT2                                                          
NUMERR   MVI   ERROR,NOTNUM                                                     
         B     TRAPER1                                                          
ERRINV2  MVI   ERROR,INVALID                                                    
         B     TRAPER1                                                          
MISSER1  MVI   ERROR,MISSING                                                    
TRAPER1  GOTO1 ERREX                                                            
         SPACE                                                                  
INVESTOP MVC   CONHEAD,INVESTMS                                                 
ERREXT2  GOTO1 ERREX2                                                           
         SPACE                                                                  
BADESTMS DC    C'* ERROR * ESTIMATE MUST BE 1 TO 255 *'                         
INVESTMS DC    C'* ERROR * TO PROFILE 11 NOT SET FOR ESTIMATE *'                
MEDMS    DC    C'* ERROR * CABLE/STATION ONLY VALID FOR MEDIA T *'              
MGRPERMS DC    C'* ERROR * ENTER MARKET GROUP CNNNN, C=CHARS A-Z, 1-4 DI        
               IGITS*'                                                          
BDMGRPMS DC    C'* ERROR * NO MGROUP X FOUND, TRY AGAIN *'                      
BADOPTS  DC    C'* ERROR * CANNOT USE OPTIONS CAB AND STA TOGETHER *'           
BDESTOPT DC    C'* ERROR * CANNOT USE OPTION EST= WITH ALL CLIENTS *'           
         DROP  RB,RC                                                            
         EJECT                                                                  
* FIND CLIENT HEADER AND SAVE CLIST                                             
         SPACE                                                                  
         DS    0H                                                               
FCLT     NMOD1 0,**FCLT**                                                       
         SPACE                                                                  
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         MVI   LKEY+1,13                                                        
         MVI   DATADISP+1,24                                                    
         MVC   SYSDIR(3),=C'SPT'                                                
         MVC   SYSFIL(3),=C'SPT'                                                
         SPACE                                                                  
FCLT10   MVC   SVKEY,KEY                                                        
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,SVBCLT,QCLT                                          
         SPACE                                                                  
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
         XC    FILENAME,FILENAME                                                
         MVI   LKEY+1,13                                                        
         MVI   DATADISP+1,24                                                    
         MVC   SYSDIR(3),=C'SPT'                                                
         MVC   SYSFIL(3),=C'SPT'                                                
         MVC   SVBCLT,BCLT                                                      
         SPACE                                                                  
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BE    FCLT20                                                           
         CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    FCLTNE                                                           
         DC    H'0'                                                             
         SPACE                                                                  
         SPACE                                                                  
* GET PROFILE REC(S)                                                            
         SPACE                                                                  
* READ T0 PROFILE *                                                             
         SPACE                                                                  
FCLT20   XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         SPACE                                                                  
         MVI   WORK+3,C'2'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
         SPACE                                                                  
         MVC   SVT2PR05,SVT1PROF+4  COMBINE CABLE NETWORKS                      
         SPACE                                                                  
         MVI   WORK+3,C'1'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
         SPACE                                                                  
         CR    RB,RB                                                            
         B     FCLTX                                                            
         SPACE                                                                  
FCLTNE   LTR   RB,RB                                                            
FCLTX    XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         SPACE                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
FCLT2    NMOD1 0,**FCL2**                                                       
         SPACE                                                                  
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         MVI   LKEY+1,13                                                        
         MVI   DATADISP+1,24                                                    
         MVC   SYSDIR(3),=C'SPT'                                                
         MVC   SYSFIL(3),=C'SPT'                                                
         SPACE                                                                  
FCL210   MVC   SVKEY,KEY                                                        
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,SVBCLT,QCLT                                          
         SPACE                                                                  
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
         XC    FILENAME,FILENAME                                                
         MVI   LKEY+1,13                                                        
         MVI   DATADISP+1,24                                                    
         MVC   SYSDIR(3),=C'SPT'                                                
         MVC   SYSFIL(3),=C'SPT'                                                
         MVC   SVBCLT,BCLT                                                      
         SPACE                                                                  
         CLI   ERROR,0                                                          
         BE    FCL220                                                           
         CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   KEY,SVKEY                                                        
         MVC   KEY+3(2),=X'FFFF'   GET NEXT CLIENT                              
         GOTO1 HIGH                                                             
         CLC   KEY(1),KEYSAVE      IF SAME REC TYPE & A/M                       
         BNE   FCL2NE                                                           
         SPACE                                                                  
         MVC   SVBCLT,KEY+1        SAVE CLIENT                                  
         B     FCL210                                                           
         SPACE                                                                  
* READ T0 PROFILE *                                                             
         SPACE                                                                  
FCL220   XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         SPACE                                                                  
         MVI   WORK+3,C'2'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
         SPACE                                                                  
         MVC   SVT2PR05,SVT1PROF+4  COMBINE CABLE NETWORKS                      
         SPACE                                                                  
         MVI   WORK+3,C'1'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
         SPACE                                                                  
         MVC   KEY(L'SVKEY),SVKEY   RESTORE KEY                                 
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         SPACE                                                                  
         CR    RB,RB                                                            
         B     FCL2X                                                            
         SPACE                                                                  
FCL2NE   LTR   RB,RB                                                            
FCL2X    XIT1                                                                   
         LTORG                                                                  
         SPACE                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
FCLT3    NMOD1 0,**FCL3**                                                       
         SPACE                                                                  
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
FCL310   XC    FILENAME,FILENAME                                                
         MVI   LKEY+1,13                                                        
         MVI   DATADISP+1,24                                                    
         MVC   SYSDIR(3),=C'SPT'                                                
         MVC   SYSFIL(3),=C'SPT'                                                
         SPACE                                                                  
         MVC   SVKEY,KEY                                                        
         MVC   SAVEKEY,KEYSAVE                                                  
         SPACE                                                                  
         GOTO1 CLUNPK,DMCB,SVBCLT,QCLT                                          
         SPACE                                                                  
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         MVI   FLDH+5,3            DATA LEN                                     
         MVC   FLD(L'QCLT),QCLT    CLIENT                                       
         LA    R2,FLDH                                                          
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
         XC    FILENAME,FILENAME                                                
         MVI   LKEY+1,13                                                        
         MVI   DATADISP+1,24                                                    
         MVC   SYSDIR(3),=C'SPT'                                                
         MVC   SYSFIL(3),=C'SPT'                                                
         MVC   SVBCLT,BCLT                                                      
         SPACE                                                                  
         CLI   ERROR,0                                                          
         BE    FCL320                                                           
         CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         MVC   SYSDIR(3),=C'TRF'                                                
         MVC   SYSFIL(3),=C'TRF'                                                
         SPACE                                                                  
         MVC   KEY,SVKEY                                                        
         MVC   KEY+5(2),=X'FFFF'    GET NEXT CLIENT                             
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE      IF SAME REC TYPE & A/M                       
         BNE   FCL3NE                                                           
         SPACE                                                                  
         MVC   SVBCLT,KEY+3        SAVE CLIENT                                  
         B     FCL310                                                           
         SPACE                                                                  
* READ T0 PROFILE *                                                             
         SPACE                                                                  
FCL320   XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
         SPACE                                                                  
         MVI   WORK+3,C'2'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
         SPACE                                                                  
         MVC   SVT2PR05,SVT1PROF+4  COMBINE CABLE NETWORKS                      
         SPACE                                                                  
         MVI   WORK+3,C'1'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         MVC   SYSDIR(3),=C'TRF'                                                
         MVC   SYSFIL(3),=C'TRF'                                                
         MVC   KEY,SAVEKEY          RESTORE SAVEKEY                             
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         CLC   KEY,KEYSAVE          IF FIRST CLIENT,                            
         BNE   FCL330               DON'T READ NEXT RECORD YET                  
         GOTO1 SEQ                                                              
         SPACE                                                                  
FCL330   CR    RB,RB                                                            
         B     FCL3X                                                            
         SPACE                                                                  
FCL3NE   LTR   RB,RB                                                            
FCL3X    XIT1                                                                   
         LTORG                                                                  
         SPACE                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
* FIND COMMERCIAL TITLE AND LENGTH *                                            
         SPACE                                                                  
FCML     NMOD1 0,**FCML**                                                       
         XC    SVCMMLP,SVCMMLP                                                  
         XC    SVCMML,SVCMML                                                    
         MVI   PCKFLAG,0                                                        
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         CLC   0(2,R5),=C'**'      IF COMML ID NOT FOUND FROM SEQ NUM,          
         BE    FCMLX               SKIP                                         
         XC    SVCMLCLT,SVCMLCLT                                                
         XC    SVCMLDS2,SVCMLDS2                                                
         XC    SVCMLDS3,SVCMLDS3                                                
         XC    SVBASIC,SVBASIC                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM,BAGYMD                                                    
         MVC   CMLKCLT,SVBCLT                                                   
         MVC   CMLKCML,0(R5)                                                    
*                                                                               
         CLI   8(R5),C' '         IF COMML ID IS > 8 CHARS                      
         BNH   FCML05             MUST BE ADID                                  
         XC    WORK,WORK                                                        
         GOTO1 ATRPACK,DMCB,(C'P',0(R5)),WORK                                   
         MVI   CMLKID+1,X'C1'     READ X'0AC1'                                  
         TM    SVFLAG,COMHDEF      IS COMMERCIAL HIDEF?                         
         BZ    *+8                                                              
         MVI   CMLKID+1,X'C2'                                                   
         MVC   CMLPADID,WORK      PACKED COMMERCIAL                             
         MVC   SVCMMLP,WORK                                                     
         MVI   PCKFLAG,C'Y'                                                     
*                                                                               
FCML05   MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'TRFDIR'                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'TRFFIL'                                            
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         MVC   SVCMLDSC(L'CMLTITLE),CMLTITLE                                    
         MVI   SVCMLDSC+L'CMLTITLE,0                                            
         TM    CMLSTAT,X'80'                                                    
         BZ    *+8                                                              
         MVI   SVCMLDSC+L'CMLTITLE,C'*'                                         
         MVC   SVCMLSLN,CMLSLN                                                  
         SPACE                                                                  
         MVC   SVCMLCLT,CMLCLTNO                                                
         SPACE                                                                  
         MVI   ELCODE,X'30'        CK FOR COMM DESC LINES 2 & 3                 
         BRAS  RE,NEXTEL                                                        
         BNE   FCML10                                                           
         MVC   SVCMLDS2,3(R6)                                                   
         BRAS  RE,NEXTEL                                                        
         BNE   FCML10                                                           
         MVC   SVCMLDS3,3(R6)                                                   
*                                                                               
**ML10   XC    SVADID,SVADID                                                    
FCML10   L     R6,AIO1             CHECK IF AD-ID                               
         MVI   ELCODE,X'A0'                                                     
         BRAS  RE,GETEL                                                         
         BNE   FCML20                                                           
         USING CMLADIEL,R6                                                      
         MVC   SVCMML,CMLADID      SAVE ADID HERE                               
*                                                                               
FCML20   DS    0H                                                               
         CLC   AGENCY,=C'H9'       THIS STARCOM?                                
         BNE   FCMLX                NO                                          
         SPACE                                                                  
         L     R6,AIO1                                                          
         MVI   ELCODE,X'90'                                                     
         BRAS  RE,GETEL                                                         
         BNE   FCMLX                                                            
         USING CMLBBEL,R6                                                       
         MVC   SVBASIC,CMLBBBCP                                                 
FCMLX    XIT1                                                                   
         DROP  R4,R6,RB,RC                                                      
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*********************************************                                   
* READ THROUGH BUYS AND BUILD ACTIVITY LIST *                                   
*********************************************                                   
         SPACE                                                                  
         DS    0H                                                               
BLDACT   NMOD1 0,*BLDACT*,R7                                                    
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
         BAS   RE,ITSAROFF       INIT TSAR FOR OFFLINE                          
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(5,0),(3,TODAYB)                                     
         SPACE                                                                  
* CLEAR EQUIVALENT PRODUCT AREA *                                               
         SPACE                                                                  
         XC    DATELIST(232),DATELIST                                           
         SPACE                                                                  
* 001-230 115 PRS (EQUIV/BASE PRODS) 203-232 SAVED PROD EQUIV TABLE             
* 233-256 24 SAVED BUY KEY                                                      
         SPACE                                                                  
* CLEAR ACTIVITY LIST BUILD AREA *                                              
         SPACE                                                                  
BLD01    L     R4,AACLST           ACTIVITY TABLE                               
         USING ACLDATA,R4                                                       
         SPACE                                                                  
         MVC   SYSDIR(3),=C'SPT'   SWITCH TO SPOT MEDIA                         
         MVC   SYSFIL(3),=C'SPT'                                                
         SPACE                                                                  
***      BRAS  RE,GBUYINIT        INITIALIZE GETBUY                             
         SPACE                                                                  
         XC    KEY,KEY                                                          
         SPACE                                                                  
         CLI   ALLCLI,C'Y'         RUNNING FOR ALL CLIENTS?                     
         BE    BLD01A                                                           
         MVC   KEY(3),BAGYMD       A-M/CLT                                      
         MVC   KEY+3(1),BPRD                                                    
         MVC   KEY+4(5),BMKT                                                    
         B     BLD02                                                            
         SPACE                                                                  
BLD01A   XC    SVBCLT,SVBCLT                                                    
         MVC   KEY(1),BAGYMD       A-M                                          
         SPACE                                                                  
BLD02    MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
***02    BRAS  RE,GBUYHIGH         NO UPDATE REQUIRED                           
         SPACE                                                                  
         CLC   KEY(1),BAGYMD       TEST SAME A-M                                
         BNE   BLD92                                                            
         CLI   ALLCLI,C'Y'         RUNNING FOR ALL CLIENTS?                     
         BE    BLD05                                                            
BLD03    CLC   KEY(3),BAGYMD       TEST SAME A-M/CLT                            
         BNE   BLD92                                                            
         SPACE                                                                  
BLD05    CLI   ALLCLI,C'Y'         RUNNING FOR ALL CLIENTS?                     
         BNE   BLD05A                                                           
         CLC   KEY+1(2),SVBCLT     TEST SAME CLIENT                             
         BE    BLD05A                                                           
         MVC   SVBCLT,KEY+1        NEW CLIENT                                   
         BRAS  RE,FCLT2            SAVE NEW CLIST                               
         BNE   BLD92                                                            
BLD05A   CLI   KEY+3,X'FF'         PRD POL ?                                    
         BNE   BLD06                                                            
         SPACE                                                                  
         GOTO1 SEQ                 YES, WAS PROCCESSED (PASSIVE PTR)            
***      BRAS  RE,GBUYRSEQ         YES, WAS PROCCESSED (PASSIVE PTR)            
         B     BLD02                                                            
         SPACE                                                                  
BLD06    MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   I WANT CONTROL BACK                          
         GOTO1 CATCHIOS            DON'T TASKNEXT OUT!                          
         CLI   ERROR,0             ANY ERROR?                                   
         BE    BLD08                                                            
         SPACE                                                                  
BLDACTER GOTO1 ERREX                                                            
         SPACE                                                                  
BLD08    CLC   KEY(9),KEYSAVE      A-M/C/P/MKT/STA                              
         BE    BLD10                                                            
         SPACE                                                                  
BLD08B   OC    KEYSAVE+4(9),KEYSAVE+4   THIS STARTING KEY                       
         BZ    BLD10                     YES, CONTINUE                          
         SPACE                                                                  
* IF MKT SPECIFIC REQ PERHAPS 1ST BUY WILL BE BYPASSED TAKE OUT *NOP            
         SPACE                                                                  
*NOP     OC    BMKT,BMKT           WAS MARKET ENTERED                           
*        BZ    BLD08F                                                           
*        SPACE                                                                  
*        CLC   BMKT,KEY+4          SAME MKT                                     
*        BNE   BLD08F                                                           
*        SPACE                                                                  
*        OC    KEYSAVE+6(7),KEYSAVE+6 STARTING KEY                              
*NOP     BZ    BLD10                                                            
         SPACE                                                                  
BLD08F   DS    0H                                                               
***      CLI   SVT2PR05,C'Y'       COMBINE CABLE NETWORKS                       
***      BNE   BLD92                NO                                          
         B     BLD92                                                            
         SPACE                                                                  
         CLI   SPOTCAN,C'C'        IF CANADIAN                                  
         BE    BLD92                NOT CABLE                                   
         SPACE                                                                  
*                    X'E8' NOW                                                  
         CLI   KEY+6,CABLESTA      THIS A CABLE STATION                         
         BL    BLD92                NO                                          
         SPACE                                                                  
         CLC   KEY(8),KEYSAVE                                                   
         BNE   BLD92                                                            
         MVC   HALF(1),KEY+8                                                    
         MVC   HALF+1(1),KEYSAVE+8                                              
         NI    HALF,CABLENET                                                    
         NI    HALF+1,CABLENET                                                  
         CLC   HALF(1),HALF+1                                                   
         BE    BLD10                                                            
         B     BLD92                                                            
         EJECT                                                                  
* IF FILTERING ON MARKET GROUP, CHECK IT OUT *                                  
         SPACE                                                                  
BLD10    DS    0H                                                               
         CLI   SVMGRPS,0           BY MARKET GROUP                              
         BE    BLD14                                                            
         CLC   KEY+4(2),BMKT                                                    
         BE    BLD14                                                            
         SPACE                                                                  
         BRAS  RE,RMGB             GO GET NEXT MARKET IN GROUP                  
         BNE   BLDX                                                             
         SPACE                                                                  
BLD14    DS    0H                                                               
         OC    BMKT,BMKT           TEST SINGLE MKT REQUEST                      
         BZ    BLD25                                                            
         CLC   KEY+4(2),BMKT                                                    
         BE    BLD25                                                            
         CLI   BPRD,0              ALL PROD REQUEST                             
         BNE   BLDX                 NO,DONE                                     
         SPACE                                                                  
         CLC   KEY(4),KEYSAVE      SAME AM/CLT/PRD                              
         BNE   BLD22                                                            
         SPACE                                                                  
         OC    BMKT,BMKT           FILTERING ON MARKET?                         
         BZ    BLD15                                                            
         CLC   KEY+4(2),BMKT       DID WE GET THE MKT YET,                      
         BNL   BLD15               FOR THIS PRODUCT?                            
         MVC   KEY+4(2),BMKT       NO, MOVE IN MARKET                           
         XC    KEY+6(7),KEY+6      AND CLEAR REST OF KEY                        
         B     BLD16                                                            
BLD15    MVI   KEY+4,X'FF'         YES, FORCE NEXT PRD                          
         SPACE                                                                  
BLD16    MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
***16    BRAS  RE,GBUYHIGH         NO UPDATE REQUIRED                           
         SPACE                                                                  
         CLC   KEY(3),KEYSAVE      SAME AM/CLT                                  
         BNE   BLDX                                                             
         SPACE                                                                  
         CLI   KEY+3,X'FF'         PRD POL ?                                    
         BNE   BLD20                                                            
         SPACE                                                                  
         GOTO1 SEQ               YES, WAS PROCCESSED (PASSIVE POINTER)          
***      BRAS  RE,GBUYRSEQ       YES, WAS PROCCESSED (PASSIVE POINTER)          
         B     BLD16                                                            
         SPACE                                                                  
BLD20    MVC   KEY+4(2),BMKT     MOVE IN MARKET                                 
         XC    KEY+6(7),KEY+6    CLEAR REST OF KEY                              
         B     BLD02                                                            
         SPACE                                                                  
BLD22    MVC   KEY+4(2),BMKT                                                    
         B     BLD02                                                            
         SPACE                                                                  
* FILTER ON ESTIMATE                                                            
         SPACE                                                                  
BLD25    DS    0H                                                               
         CLI   OPTEST,0            WAS EST ENTERED                              
         BE    BLD28                                                            
         SPACE                                                                  
         CLC   OPTEST,KEY+9        SAME ESTIMATE                                
         BE    BLD28                                                            
         SPACE                                                                  
         MVC   KEYSAVE,KEY                                                      
         MVC   KEY+10(3),=X'FFFFFF'  FORCE NEXT ESTIMATE                        
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
***      BRAS  RE,GBUYHIGH         NO UPDATE REQUIRED                           
         B     BLD05                                                            
         SPACE                                                                  
BLD28    TM    SVOPTSW,OPTSTA      STD STATIONS ONLY                            
         BZ    BLD30                                                            
         SPACE                                                                  
         CLI   SPOTCAN,C'C'        IF CANADIAN                                  
         BE    BLD32                NOT CABLE                                   
         SPACE                                                                  
*                    X'E8' NOW                                                  
         CLI   KEY+6,CABLESTA      THIS A STD STATION                           
         BNL   BLD92               NO, GET NEXT                                 
         SPACE                                                                  
BLD30    TM    SVOPTSW,OPTCABLE                                                 
         BZ    BLD32                                                            
         SPACE                                                                  
         CLI   SPOTCAN,C'C'        IF CANADIAN                                  
         BE    BLD92                NOT CABLE                                   
         SPACE                                                                  
*                    X'E8' NOW                                                  
         CLI   KEY+6,CABLESTA      THIS A CABLE STATION                         
         BL    BLD92                NO                                          
         SPACE                                                                  
BLD32    L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         EJECT                                                                  
* GET DAYPART EQUIVALENCY CODE IF PROFILE ON AND NONE IN FOR ESTIMATE *         
         SPACE                                                                  
         CLI   SVPROF11,C'A'       TEST COPY CODE = ADJACENCY                   
         BE    BLD35                                                            
         CLI   SVPROF11,C'D'       TEST COPY CODE = DAYPART                     
         BE    BLD35                                                            
         CLI   SVPROF11,C'Y'       TEST COPY CODE = DAYPART                     
         BNE   BLD42                                                            
BLD35    CLC   EQVEST,KEY+9        SAME EST                                     
         BE    BLD42                                                            
         MVC   TRBUYKEY,KEY                                                     
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
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
         BNE   BLD40                                                            
         MVI   ELCODE,X'10'                                                     
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'TRFFIL'                                            
         GOTO1 GETREC                                                           
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DPEDTAEL,R6                                                      
         LA    R1,EQVTAB                                                        
BLD36    MVC   0(2,R1),DPEDPCDE                                                 
         LA    R1,2(,R1)                                                        
         BAS   RE,NEXTEL2                                                       
         BE    BLD36                                                            
BLD40    MVC   EQVEST,TRBUYKEY+9                                                
         XC    FILENAME,FILENAME                                                
         DROP  R6                                                               
         MVC   KEY,TRBUYKEY                                                     
         SPACE                                                                  
         XC    TRBUYKEY,TRBUYKEY                                                
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
***      BRAS  RE,GBUYHIGH                                                      
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
* NOW GET BUY RECORD *                                                          
         SPACE                                                                  
BLD42    DS    0H                                                               
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 GETREC                                                           
***      BRAS  RE,GBUYGETR                                                      
         SPACE                                                                  
         MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   I WANT CONTROL BACK                          
         GOTO1 CATCHIOS            DON'T TASKNEXT OUT!                          
         CLI   ERROR,0             ANY ERROR?                                   
         BNE   BLDACTER                                                         
         SPACE                                                                  
         L     R5,AIO                                                           
         USING BUYRECD,R5                                                       
         SPACE                                                                  
         CLC   KEY+4(2),4(R5)     TEST SAME MARKET                              
         BNE   BLD90                NO - SPILL                                  
         SPACE                                                                  
         TM    15(R5),X'80'        DELETED RECORD                               
         BO    BLD90                YES, BYPASS                                 
         SPACE                                                                  
         MVC   BUYDATE,BDCHG                                                    
         SPACE                                                                  
* CALCULATE DIFFERENCE BETWEEN FIRST/LAST DAYS OF ROTATOR *                     
         SPACE                                                                  
         XC    ROTDAYS,ROTDAYS                                                  
         ZIC   R0,BDDAY                                                         
         SLL   R0,25                                                            
         SPACE                                                                  
         CLI   OWRSDAY,0                                                        
         BE    BLD45                                                            
         ZIC   RF,OWRSDAY                                                       
         SPACE                                                                  
         LTR   R0,R0                                                            
         BNM   *+8                                                              
         O     R0,=X'00800000'                                                  
         SLL   R0,1                                                             
         BCT   RF,*-14                                                          
         SPACE                                                                  
BLD45    LTR   R0,R0               REG GOES NEGATIVE WHEN BIT ARRIVES           
         BZ    *+16                                                             
         BM    *+12                                                             
         SLL   R0,1                                                             
         B     *-14                                                             
         SPACE                                                                  
         SR    RE,RE               CLEAR COUNTER                                
         SLL   R0,1                                                             
         LTR   R0,R0               SHIFT TILL NO MORE BITS ON                   
         BZ    *+8                                                              
         BCT   RE,*-10                                                          
         LPR   RE,RE                                                            
         STH   RE,ROTDAYS          AND SAVE DAYS                                
         SPACE                                                                  
         CLI   BUYKEY+3,X'FF'      TEST POOL BUYREC                             
         BNE   BLD90               BYPASS, BRAND BUYS NOT SUPPORTED             
         SPACE                                                                  
         LA    R1,24(,R5)          POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
GET61    ICM   RF,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                BLOW UP ON ELEMENT LENGTH ZERO               
         AR    R1,RF               POINT TO NEXT ELEMENT                        
         CLI   0(R1),0             TEST END OF RECORD                           
         BE    GET61ND                                                          
         CLI   0(R1),X'61'         TEST FOR MASTER CLIENT ELEMENT               
         BNE   GET61                                                            
         SPACE                                                                  
         CLI   1(R1),6             THIS ELEM INCLUDE PROD CODE                  
         BL    GET61ND                                                          
         SR    R0,R0                                                            
         ICM   R0,1,5(R1)          SAVE PROD CODE                               
         BZ    GET61ND                                                          
         SPACE                                                                  
         LA    R1,24(,R5)          POINT TO FIRST ELEMENT                       
         SPACE                                                                  
GET61EL  ICM   RF,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                BLOW UP ON ELEMENT LENGTH ZERO               
         AR    R1,RF               POINT TO NEXT ELEMENT                        
         CLI   0(R1),0             TEST END OF RECORD                           
         BE    GET61ND                                                          
         CLI   0(R1),X'0B'         TEST FOR POL ORIG                            
         BL    GET61EL                                                          
         CLI   0(R1),X'0D'         TEST FOR POL FLIP                            
         BH    GET61EL                                                          
         CLI   1(R1),10            UNALLOCATED PRODUCT                          
         BNH   GET61EL                                                          
         STC   R0,10(,R1)          STORE NEW PRODUCT CODE                       
         B     GET61EL                                                          
         SPACE                                                                  
GET61ND  DS   0H                                                                
         EJECT                                                                  
* POL PROCESSING *                                                              
         SPACE                                                                  
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         LA    R6,BDELEM                                                        
         SPACE                                                                  
BLD48    BAS   RE,BUYEL                                                         
         BNE   BLD90                                                            
         CLI   1(R6),10            TEST UNALL                                   
         BNH   BLD48                                                            
         CLI   BPRD,0              TEST ONE PRD REQUEST                         
         BE    BLD50                                                            
         CLC   BPRD,10(R6)         YES - TEST MATCH PROD                        
         BE    BLD50                                                            
         CLI   1(R6),18            PIGGYBACK                                    
         BL    BLD48                                                            
         CLC   BPRD,14(R6)                                                      
         BNE   BLD48                                                            
         SPACE                                                                  
BLD50    TM    6(R6),X'C0'         TEST MINUS OR MINUSED                        
         BNZ   BLD48                                                            
         MVC   ELDATE,2(R6)        SAVE ELEM START DATE                         
         MVC   ELDATEX,2(R6)       AND PRESET ELEM END DATE                     
*                                                                               
         CLI   SVT1PR1,0           TEST ADJUST ROTATOR DAYS                     
         BE    BLD52               NO                                           
         CLC   SVT1PR1(1),BDDAY    TEST DAYS MATCH                              
         BNE   BLD52                                                            
         SPACE                                                                  
* BACK UP ONE DAY *                                                             
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(2,ELDATE),WORK                                      
         ZIC   R0,SVT1PR2          GET NUMBER OF DAYS TO BACK UP                
         LNR   R0,R0                                                            
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,ELDATE)                                    
*                                                                               
BLD52    CLC   ELDATE,PERENDP      TEST AFTER PERIOD END DATE                   
         BH    BLD48                                                            
*                                                                               
         CLI   0(R6),11            TEST REGEL                                   
         BNE   BLD54                                                            
         GOTO1 DATCON,DMCB,(2,ELDATEX),WORK                                     
         LH    R0,ROTDAYS                                                       
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,ELDATEX)                                   
*                                                                               
BLD54    CLC   ELDATEX,PERSTRP     TEST BEFORE PERIOD START                     
         BL    BLD48                                                            
*                                                                               
         XC    ACLWORK,ACLWORK                                                  
         LA    R4,ACLWORK                                                       
         USING ACLDATA,R4                                                       
*                                                                               
         SPACE                                                                  
         BRAS  RE,CPY              GO SET COPY CODE IF ANY                      
         EJECT                                                                  
         MVC   ACLCLI,KEY+1        CLIENT                                       
         MVC   ACLSLN,11(R6)       SLN                                          
         MVC   BYTE,10(R6)                                                      
         BRAS  RE,GGETPRD                                                       
         MVC   ACLEBCP1,0(R1)      EBCDIC PRD                                   
         MVC   ACLPRD,BYTE         MAY BE EQUIVALENT PROD                       
         SPACE                                                                  
         CLI   1(R6),18            TEST PIGGYBACK                               
         BNL   BLD55                YES                                         
         CLI   BPRD2,0             TEST PTR = NONE                              
         BE    BLD60                                                            
         CLI   BPRD2,0             TEST PTR REQUIRED                            
         BNE   BLD48                YES, BYPASS                                 
         B     BLD60                                                            
         SPACE                                                                  
BLD55    CLI   BPRD2,0             TEST PTR = NONE                              
         BE    BLD48                BYPASS                                      
         MVC   ACLSLN2,15(R6)      SLN2                                         
         MVC   BYTE,14(R6)                                                      
         BRAS  RE,GGETPRD                                                       
         MVC   ACLEBCP2,0(R1)                                                   
         MVC   ACLPRD2,BYTE         MAY BE EQUIVALENT PROD                      
         SPACE                                                                  
* MUST BE IN ALPHA SEQ *                                                        
         SPACE                                                                  
         CLC   ACLEBCP1,ACLEBCP2                                                
         BL    BLD56                                                            
         SPACE                                                                  
* REVERSE THE ENTRIES *                                                         
         SPACE                                                                  
         XC    ACLPRD(2),ACLPRD2                                                
         XC    ACLPRD2(2),ACLPRD                                                
         XC    ACLPRD(2),ACLPRD2                                                
         XC    ACLEBCP1,ACLEBCP2                                                
         XC    ACLEBCP2,ACLEBCP1                                                
         XC    ACLEBCP1,ACLEBCP2                                                
         SPACE                                                                  
BLD56    CLI   BPRD2,0             LIMIT ON PTR                                 
         BE    BLD60                NO                                          
         CLC   BPRD2,ACLPRD2                                                    
         BNE   BLD48                                                            
         SPACE                                                                  
* TEST DATA IN TABLE ALREADY *                                                  
         SPACE                                                                  
BLD60    CLI   BPRD,0              ALL POL REQ                                  
         BE    BLD65                                                            
         CLC   ACLPRD,BPRD         ELSE MUST MATCH PRD 1                        
         BNE   BLD48                                                            
*                                                                               
BLD65    L     R4,AACLST           ACTIVITY TABLE                               
*                                                                               
BLD70    CLI   ACLPRD,0                                                         
         BE    BLD80                                                            
         CLC   ACLCLI(7),ACLWORK+6  CLI/PRD/SLN/PRD2/SLN2/COPY                  
         BNE   BLD75                                                            
*                                                                               
         MVC   SVDATE,ELDATE        MOVE FIRST TLCST DATE                       
         MVC   SVDATEX,ELDATEX      MOVE LAST TLCST DATE                        
*                                                                               
         BAS   RE,EXPANDTE         EXPAND DATES ONLY IF NO GAP                  
*                                                                               
         MVC   ELDATE,SVDATE       RESTORE DATES                                
         MVC   ELDATEX,SVDATEX                                                  
*                                                                               
         BE    BLD48                                                            
*                                                                               
BLD75    LA    R4,L'ACLDATA(R4)                                                 
         B     BLD70                                                            
         SPACE                                                                  
BLD80    L     RE,AACLSTX          END OF ACTIVITY TABLE                        
         CR    R4,RE                                                            
         BNH   *+6                                                              
         DC    H'0'                MAKE TABLE BIGGER                            
         SPACE                                                                  
         MVC   ACLEBCP1(13),ACLWORK                                             
         MVC   ACLFTD,ELDATE                                                    
         MVC   ACLLTD,ELDATEX                                                   
         SPACE                                                                  
         CLC   ACLFTD,PERSTRP      TEST PRIOR TO PERIOD START                   
         BH    *+10                                                             
         MVC   ACLFTD,PERSTRP                                                   
         SPACE                                                                  
         CLC   ACLLTD,PERENDP      TEST AFTER PERIOD END                        
         BL    BLD48                                                            
         MVC   ACLLTD,PERENDP                                                   
         B     BLD48                                                            
         SPACE                                                                  
BLD90    MVC   KEYSAVE,KEY         SAVE BUY DATA                                
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
BLD90F   GOTO1 SEQ                                                              
***90F   BRAS  RE,GBUYRSEQ         GET NEXT KEY                                 
         SPACE                                                                  
         CLC   TWAAGY,=C'SJ'       IF SJR, SKIP BAD RECORDS                     
         BNE   BLD91                                                            
         CLC   KEY(3),=X'C2864F'   RADIO, CLIENT BSP                            
         BE    BLD90F                                                           
         CLC   KEY(3),=X'C2D35F'   RADIO, CLIENT U1                             
         BE    BLD90F                                                           
*                                                                               
BLD91    CLI   KEY+3,X'FF'         PRD POL ?                                    
         BNE   BLD08                                                            
         CLC   KEY(12),=X'FFFFFFFFFFFFFFFFFFFFFFFF'   EOF                       
         BE    BLD08                                                            
         B     BLD90F                                                           
*                                                                               
         B     BLD08                                                            
         EJECT                                                                  
* CHANGE OF STATION *                                                           
         SPACE                                                                  
BLD92    MVC   SVBKEY,KEY           SAVE KEY THAT GAVE BREAK                    
*                                                                               
BLD92A   L     R4,AACLST                                                        
         CLI   ACLPRD,0            TEST ACTIVITY IN TABLE                       
         BE    BLD120              NO - TEST TO CONTINUE                        
         SPACE                                                                  
BLD92X   MVC   TRBUYKEY,BUYRECD    SAVE BUY KEY FOR SVTABLE                     
         SPACE                                                                  
* BLANK COPY CODES WITHOUT PATTERNS IF COPY CODE = DAYPART *                    
         SPACE                                                                  
BLD95    CLI   SVT1PR8,C'Y'        TEST AUTO P/B PATTN                          
         BE    BLD96                                                            
         CLI   SVPROF11,C'A'       TEST ADJACENCY=COPY CODE                     
         BE    BLD95C                                                           
         CLI   SVPROF11,C'D'       TEST DPT=COPY CODE                           
         BE    BLD95C                                                           
         CLI   SVPROF11,C'P'       TEST DPT=1ST CHAR OF PROGRAM CODE            
         BE    BLD95C                                                           
         CLI   SVPROF11,C'Y'       TEST DPT=COPY CODE                           
         BNE   BLD96                                                            
         SPACE                                                                  
* CHECK WHICH DAYPARTS HAVE PATTERNS *                                          
         SPACE                                                                  
BLD95C   BRAS  RE,CHKPTN                                                        
         EJECT                                                                  
* SORT BY COPY CODE/SLN2/SLN1/EBCDIC PRD *                                      
         SPACE                                                                  
BLD96    L     R4,AACLST                                                        
         SR    R0,R0                                                            
*                                                                               
         CLI   0(R4),0                                                          
         BE    *+12                                                             
         LA    R4,L'ACLDATA(R4)                                                 
         BCT   R0,*-12                                                          
*                                                                               
         L     R4,AACLST                                                        
         LPR   R0,R0                                                            
         GOTO1 XSORT,DMCB,(R4),(R0),L'ACLDATA,1,10  COPY CODE                   
         GOTO1 (RF),(R1),,,,,9                      SLN2                        
         GOTO1 (RF),(R1),,,,,7                      SLN1                        
         GOTO1 (RF),(R1),,,,6,0                     ALPHA PROD1 & 2             
         SPACE                                                                  
*NOP     CLI   SVT2PR12,C'Y'       SORT BY DATE HIGH                            
**       BNE   BLD100                                                           
         B     BLD100                                                           
         SPACE                                                                  
         GOTO1 (RF),(R1),,,,4,11                   DATE                         
         EJECT                                                                  
* BUILD TSAROFF ENTRIES FROM ACTIVITY LIST *                                    
         SPACE                                                                  
BLD100   DS    0H                                                               
         SPACE                                                                  
         LHI   R2,TSARBLK-SYSD                                                  
         AR    R2,R9                                                            
         USING TSARD,R2                                                         
         SPACE                                                                  
         XC    BLOCK(10+L'TSRENT),BLOCK                                         
         SPACE                                                                  
         LA    RE,L'TSRENT         LEN OF ENTRY                                 
         LA    RE,2(RE)            PLUS 2 FOR LEN FIELD                         
         STCM  RE,3,BLOCK                                                       
         SPACE                                                                  
         LA    R5,BLOCK+2          RECORD IS IN BLOCK+2                         
         USING TSRBUYD,R5                                                       
         SPACE                                                                  
         L     R4,AACLST                                                        
         USING ACLDATA,R4                                                       
         B     *+10                                                             
BLD110   XC    BLOCK+2(10+L'TSRENT),BLOCK+2  DO NOT CLEAR ENTRY LEN             
         SPACE                                                                  
         MVC   TSRMKT,TRBUYKEY+4   MOVE SAVED MARKET                            
         MVC   TSRCSTA,TRBUYKEY+6   AND CABLE STATION IF ANY                    
         SPACE                                                                  
         TM    SVOPTSW,OPTCABLE                                                 
         BO    *+12                                                             
         CLI   TRBUYKEY+6,CABLESTA  IF THIS IS A CABLE STATION,                 
         BL    BLD115                                                           
         SPACE                                                                  
***      CLI   SVT2PR05,C'Y'       COMBINE CABLE NETWORKS                       
***      BNE   BLD115               NO                                          
         B     BLD115                                                           
         SPACE                                                                  
         CLI   SPOTCAN,C'C'        IF CANADIAN                                  
         BE    BLD115               NOT CABLE                                   
         SPACE                                                                  
*                    X'E8' NOW                                                  
         CLI   TSRCSTA,CABLESTA    THIS A CABLE STATION                         
         BL    BLD115               NO                                          
         NI    TSRCSTA+2,CABLENET                                               
         SPACE                                                                  
BLD115   DS    0H                                                               
         SPACE                                                                  
         MVC   TSRCLI,ACLCLI                                                    
         MVC   TSRPRD,ACLPRD                                                    
         MVC   TSRPRD2,ACLPRD2                                                  
         MVC   TSRSLN1,ACLSLN                                                   
         MVC   TSRSLN2,ACLSLN2                                                  
         MVC   TSREST,ACLCOPY                                                   
         MVC   TSRFTD,ACLFTD                                                    
         MVC   TSRLTD,ACLLTD                                                    
*                                                                               
         LA    R1,BLOCK                                                         
         ST    R1,TSAREC                                                        
         SPACE                                                                  
         MVI   TSOFFACT,TSAADD      SET TSAROFF ADD                             
         GOTO1 TSAROFF,(R2)                                                     
         SPACE                                                                  
         TM    TSERRS,X'20'        DUPLICATE KEY ON ADD                         
         BO    BLD118              YES, SAME BUY DIFFERNT BUY LINE #            
         SPACE                                                                  
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
BLD118   LA    R4,L'ACLDATA(R4)                                                 
         CLI   ACLPRD,0            TEST MORE DATA                               
         BNE   BLD110               YES                                         
         SPACE                                                                  
BLD120   XC    FILENAME,FILENAME   SWITCH BACK TO STRAFFIC                      
         SPACE                                                                  
         CLI   ALLCLI,C'Y'         IF RUNNING FOR ALL CLIENTS,                  
         BNE   BLD123                                                           
         CLC   SVBKEY(1),BAGYMD    TEST SAME A-M                                
         BNE   BLDX                NO, DONE                                     
         CLC   SVBKEY(3),BAGYMD    TEST SAME A-M/C                              
         BE    BLD124                                                           
         XC    KEYSAVE,KEYSAVE                                                  
         B     BLD02               NO, CONTINUE WITH NEXT CLIENT                
BLD123   CLC   SVBKEY(3),BAGYMD    TEST SAME A-M/C/P                            
         BNE   BLDX                 NO, DONE                                    
         SPACE                                                                  
BLD124   L     R4,AACLST                                                        
         SPACE                                                                  
         L     RE,AACLST           CLEAR ACTIVITY LIST AREA                     
         L     RF,AACLSTX                                                       
         SR    RF,RE                                                            
         XCEFL                                                                  
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVBKEY),SVBKEY                                             
         SPACE                                                                  
         TM    SVOPTSW,OPTCABLE                                                 
         BZ    BLD125                                                           
         SPACE                                                                  
         CLI   SPOTCAN,C'C'        IF CANADIAN                                  
         BE    BLD02                NOT CABLE                                   
         SPACE                                                                  
*                    X'E8' NOW                                                  
         CLI   KEY+6,CABLESTA      THIS A CABLE STATION                         
         BNL   BLD02                                                            
         SPACE                                                                  
         MVI   KEY+6,CABLESTA      FORCE NEXT STATION                           
         XC    KEY+7(6),KEY+7      AND CLEAR THE REST                           
         B     BLD130                                                           
         SPACE                                                                  
BLD125   TM    SVOPTSW,OPTSTA      STD STATIONS ONLY                            
         BZ    BLD02                                                            
         SPACE                                                                  
         CLI   SPOTCAN,C'C'        IF CANADIAN                                  
         BE    BLD02                NOT CABLE                                   
         SPACE                                                                  
*                    X'E8' NOW                                                  
         CLI   KEY+6,CABLESTA      THIS A STD STATION                           
         BL    BLD02                YES, CONTINUE                               
         SPACE                                                                  
         MVC   KEY+6(3),=X'FFFFFF' FORCE NEXT STATION                           
         SPACE                                                                  
BLD130   MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
***130   BRAS  RE,GBUYHIGH         NO UPDATE REQUIRED                           
         XC    KEYSAVE+4(9),KEYSAVE+4  CLEAR MKT/STA                            
         B     BLD05                                                            
         SPACE                                                                  
         DROP  R5                                                               
         SPACE                                                                  
**DX     CLI   SVPROF15,C'Y'       USE TRAFFIC, NOT SPOT BUYS                   
**       BNE   BLDXX                                                            
         SPACE                                                                  
BLDX     MVC   SYSDIR(3),=C'TRF'                                                
         MVC   SYSFIL(3),=C'TRF'                                                
         SPACE                                                                  
         BAS   RE,BLDTR            GO PROCESS TRAFFIC BUYS                      
         SPACE                                                                  
BLDXX    XIT1                                                                   
         EJECT                                                                  
* INITIALIZE TSAROFF                                                            
         SPACE                                                                  
ITSAROFF NTR1                                                                   
         SPACE                                                                  
         LHI   RE,TSARBLK-SYSD                                                  
         AR    RE,R9                                                            
         LR    R2,RE                                                            
         LA    RF,TSARDL2                                                       
         XCEF                                                                   
         USING TSARD,R2                                                         
*                                                                               
         L     R0,=A(L'ELEM*24000)                                              
         ST    R0,TSAREC                                                        
         GETMAIN RC,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,TSABUF                                                        
*                                                                               
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSKEYL,L'TSRKEY                                                  
*        MVI   TSRECL+1,135                                                     
         MVI   TSRECL+1,185                                                     
         OI    TSRECI,TSRVAR                                                    
         OI    TSINDS,TSINODSK                                                  
         OI    TSIND2,TSI2MANY                                                  
         SPACE                                                                  
* TSAR CALLOV HERE                                                              
         SPACE                                                                  
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A7D')                                 
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,TSAROFF                                                       
         SPACE                                                                  
         MVI   TSOFFACT,TSAINI      SET 1ST TSAROFF FOR INIT                    
         GOTO1 TSAROFF,(R2)                                                     
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         EJECT                                                                  
* EXPAND DATES ONLY IF THERE ARE NO GAPS *                                      
         SPACE                                                                  
EXPANDTE NTR1                                                                   
         SPACE                                                                  
         CLC   SVDATE,PERSTRP     TEST PRIOR TO PERIOD START                    
         BH    *+10                                                             
         MVC   SVDATE,PERSTRP                                                   
         SPACE                                                                  
         CLC   SVDATEX,PERENDP    TEST AFTER PERIOD END                         
         BL    *+10                                                             
         MVC   SVDATEX,PERENDP                                                  
         SPACE                                                                  
         CLC   ACLFTD,SVDATEX      FTD IN TABLE > LTD OF ELEMENT                
         BH    EXP10                                                            
         SPACE                                                                  
         CLC   SVDATE,ACLLTD       FTD OF ELEMENT < LTD IN TABLE                
         BNH   EXP20                                                            
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(2,SVDATE),WORK                                      
         GOTO1 ADDAY,(R1),WORK,WORK+6,F'-1'                                     
         GOTO1 DATCON,DMCB,WORK+6,(2,WORK+12)                                   
         SPACE                                                                  
         CLC   WORK+12(2),ACLLTD   ANY GAP IN DATES                             
         BE    EXP20                                                            
         B     EXPNE                                                            
         SPACE                                                                  
EXP10    DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,SVDATEX),WORK                                     
         GOTO1 ADDAY,(R1),WORK,WORK+6,F'1'                                      
         GOTO1 DATCON,DMCB,WORK+6,(2,WORK+12)                                   
         SPACE                                                                  
         CLC   ACLFTD,WORK+12      ANY GAP IN DATES                             
         BNE   EXPNE                                                            
         SPACE                                                                  
EXP20    DS    0H                  EXPAND DATES                                 
         CLC   ACLFTD,SVDATE                                                    
         BNH   *+10                                                             
         MVC   ACLFTD,SVDATE                                                    
         CLC   ACLLTD,SVDATEX                                                   
         BNL   *+10                                                             
         MVC   ACLLTD,SVDATEX                                                   
         SPACE                                                                  
         CR    RB,RB                                                            
         B     *+6                                                              
EXPNE    CR    RB,RD                                                            
         XIT1                                                                   
         EJECT                                                                  
*********************************                                               
* READ TRAFFIC BUYS                                                             
*********************************                                               
         SPACE                                                                  
BLDTR    NTR1                                                                   
         XC    SVKEY,SVKEY                                                      
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A32'     SET INITIAL KEY                              
*                                                                               
         CLI   ALLCLI,C'Y'         RUNNING FOR ALL CLIENTS?                     
         BNE   BLDTR01                                                          
         XC    SVBCLT,SVBCLT                                                    
         MVC   KEY+2(1),BAGYMD     A-M                                          
         B     BLDTR01A                                                         
*                                                                               
BLDTR01  MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(1),BPRD       PRD                                          
         MVC   KEY+6(5),BMKT       MKT/STA                                      
         SPACE                                                                  
BLDTR01A CLI   SVPROF13,C'Y'       PROD EQUIV CLT                               
         BNE   BLDTR02                                                          
***      CLI   BPRD,0                                                           
***      BNE   BLDTR02                                                          
         OI    KEY+1,X'80'         X'0AB2' RECORD (POL TRAFFIC BUYS)            
         MVI   KEY+5,X'FF'                                                      
         SPACE                                                                  
BLDTR02  MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
         SPACE                                                                  
* IF POL OR EQUIV PRD, PRD WILL BE FF IN PASSIVE KEY, REAL PROD  *              
         SPACE                                                                  
BLDTR04  CLI   ALLCLI,C'Y'         RUNNING FOR ALL CLIENTS?                     
         BNE   BLDTR04A                                                         
         CLC   KEY(3),KEYSAVE      TEST SAME RECORD TYPE/A-M                    
         BNE   BLDTR04C                                                         
         CLC   KEY+3(2),SVBCLT     TEST SAME CLIENT                             
         BE    BLDTR04A                                                         
         MVC   SVBCLT,KEY+3        NEW CLIENT                                   
         BRAS  RE,FCLT3            SAVE NEW CLIST                               
         SPACE                                                                  
BLDTR04A CLC   KEY(11),KEYSAVE     ID/A-M/C/PRD/MKT/STA                         
         BE    BLDTR08                                                          
         SPACE                                                                  
         CLC   KEY(6),KEYSAVE      ID/A-M/C/PRD                                 
         BNE   BLDTR04C                                                         
         SPACE                                                                  
         OC    KEYSAVE+6(2),KEYSAVE+6  ANY MKT                                  
         BZ    *+14                                                             
         CLC   KEY+6(2),KEYSAVE+6  SAME MKT                                     
         BNE   BLDTR04C                                                         
         SPACE                                                                  
         OC    KEYSAVE+8(3),KEYSAVE+8 STARTING KEY                              
         BZ    BLDTR08                                                          
         SPACE                                                                  
BLDTR04C DS    0H                                                               
***      CLI   SVT2PR05,C'Y'       COMBINE CABLE NETWORKS                       
***      BNE   BLDTR05                                                          
         B     BLDTR05                                                          
         SPACE                                                                  
         CLI   SPOTCAN,C'C'        IF CANADIAN                                  
         BE    BLDTR05              NOT CABLE                                   
         SPACE                                                                  
*                    X'E8' NOW                                                  
         CLI   KEY+8,CABLESTA      THIS A CABLE STATION                         
         BNL   BLDTR05              NO                                          
         SPACE                                                                  
         CLC   KEY(10),KEYSAVE     A-M/C/PRD/MKT/STA                            
         BNE   BLDTR05                                                          
         MVC   HALF(1),KEY+10                                                   
         MVC   HALF+1(1),KEYSAVE+10                                             
         NI    HALF,CABLENET                                                    
         NI    HALF+1,CABLENET                                                  
         CLC   HALF(1),HALF+1                                                   
         BE    BLDTR08                                                          
         SPACE                                                                  
BLDTR05  CLC   KEY(5),KEYSAVE      REC TYPE A-M OR CLT CHANGE                   
         BE    BLDTR90              NO                                          
         CLI   ALLCLI,C'Y'         RUNNING FOR ALL CLIENTS?                     
         BNE   BLDTR06                                                          
         CLC   KEY(3),KEYSAVE      REC TYPE OR A-M CHANGE                       
         BE    BLDTR90                                                          
BLDTR06  MVI   KEY+2,X'FF'         FORCE BREAK IN CASE ONLY TYPE CHANGE         
         B     BLDTR90                                                          
         EJECT                                                                  
* IF FILTERING ON MARKET GROUP, CHECK IT OUT *                                  
         SPACE                                                                  
BLDTR08  DS    0H                                                               
         CLI   SVMGRPS,0           BY MARKET GROUP                              
         BE    BLDTR08D                                                         
         CLC   KEY+6(2),BMKT                                                    
         BE    BLDTR08D                                                         
         SPACE                                                                  
         BRAS  RE,RMGB             GO GET NEXT MARKET IN GROUP                  
         BNE   BLDTR90                                                          
         SPACE                                                                  
BLDTR08D DS    0H                                                               
         OC    BMKT,BMKT           TEST SINGLE MKT REQUEST                      
         BZ    BLDTR08F                                                         
         CLC   KEY+6(2),BMKT                                                    
         BE    BLDTR08F                                                         
         SPACE                                                                  
         CLI   BPRD,0              ALL PRD REQUEST                              
         BNE   BLDTR90              NO,DONE                                     
         SPACE                                                                  
         CLC   KEY(6),KEYSAVE      SAME AM/CLT/PRD                              
         BNE   BLDTR08E                                                         
         SPACE                                                                  
         MVI   KEY+6,X'FF'         YES, FORCE NEXT PRD                          
         SPACE                                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         SPACE                                                                  
         CLC   KEY(5),KEYSAVE      SAME AM/CLT                                  
         BNE   BLDTR90                                                          
         SPACE                                                                  
BLDTR08E MVC   KEY+6(5),BMKT                                                    
         B     BLDTR02                                                          
         SPACE                                                                  
BLDTR08F TM    SVOPTSW,OPTCABLE                                                 
         BZ    BLDTR08G                                                         
         SPACE                                                                  
         CLI   SPOTCAN,C'C'        IF CANADIAN                                  
         BE    BLDTR90              NOT CABLE                                   
         SPACE                                                                  
*                    X'E8' NOW                                                  
         CLI   KEY+8,CABLESTA      THIS A CABLE STATION                         
         BL    BLDTR90                                                          
         SPACE                                                                  
         OC    BSTA,BSTA                                                        
         BZ    BLDTR08G                                                         
         CLC   KEY+8(2),BSTA                                                    
         BNE   BLDTR90                                                          
         MVC   HALF(1),KEY+10                                                   
         NI    HALF,X'80'                                                       
         CLC   HALF(1),BSTA+2                                                   
         BNE   BLDTR90                                                          
         SPACE                                                                  
BLDTR08G DS    0H                                                               
         TM    SVOPTSW,OPTSTA      STD STATIONS ONLY                            
         BZ    BLDTR08K                                                         
         SPACE                                                                  
         CLI   SPOTCAN,C'C'        IF CANADIAN                                  
         BE    BLDTR08K             NOT CABLE                                   
         SPACE                                                                  
*                    X'E8' NOW                                                  
         CLI   KEY+6,CABLESTA      THIS A STD STATION                           
         BNL   BLDTR90             NO, GET NEXT                                 
         SPACE                                                                  
BLDTR08K L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL2                                                        
         B     *+8                                                              
         SPACE                                                                  
* SEARCH ELEMENTS FOR BUYS IN INSTR PERIOD *                                    
         SPACE                                                                  
BLDTR10  BAS   RE,NEXTEL2                                                       
         BNE   BLDTR40                                                          
         USING TBYDTAEL,R6                                                      
         CLI   OPTEST,0            WAS EST ENTERED                              
         BE    BLDTR12              NO                                          
         SPACE                                                                  
         CLC   TBYCODE,OPTEST      CK EQUAL EST                                 
         BE    *+12                                                             
         SPACE                                                                  
         CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BE    BLDTR10                                                          
         SPACE                                                                  
BLDTR12  GOTO1 DATCON,DMCB,(3,TBYSTART),(2,WORK)                                
         GOTO1 (RF),(R1),(3,TBYEND),(2,WORK+2)                                  
         CLC   WORK(2),PERENDP     TEST AFTER PERIOD END DATE                   
         BH    BLDTR10                                                          
         CLC   WORK+2(2),PERSTRP   END BEFORE PERIOD START                      
         BL    BLDTR10                                                          
         EJECT                                                                  
* LIMIT BUY DATES TO INSTR DATES *                                              
         SPACE                                                                  
         CLC   WORK(2),PERSTRP     IF BUY DATE BEFORE INSTR START               
         BNL   *+10                                                             
         MVC   WORK(2),PERSTRP     FORCE TO INSTR START DATE                    
         SPACE                                                                  
         CLC   WORK+2(2),PERENDP   IF BUY DATE AFTER INSTR END                  
         BNH   *+10                                                             
         MVC   WORK+2(2),PERENDP   FORCE TO INSTR END DATE                      
         SPACE                                                                  
* BUILD ACTIVITY TABLE ENTRY FOR BUY *                                          
         SPACE                                                                  
         XC    ACLWORK,ACLWORK                                                  
         LA    R4,ACLWORK                                                       
         MVC   BYTE,KEY+TBYKPRD-TBYKEY                                          
         CLI   BYTE,0                                                           
         BNE   *+10                                                             
         MVC   BYTE,KEY+TBYPPRD-TBYKEY                                          
         BRAS  RE,GGETPRD                                                       
         MVC   ACLCLI,KEY+3        CLIENT                                       
         MVC   ACLPRD,BYTE                                                      
         MVC   ACLSLN,TBYSLN                                                    
         MVC   ACLEBCP1,0(R1)                                                   
         CLI   BPRD,0              PROD POL                                     
         BE    *+14                                                             
         CLC   BYTE,BPRD                                                        
         BNE   BLDTR10                                                          
         SPACE                                                                  
         CLI   BPRD2,0             ANY PTR SELECTION                            
         BE    BLDTR16                                                          
         CLI   BPRD2,0             PTR SELECTION = NONE                         
         BNE   BLDTR14                                                          
         CLI   TBYPRD2,0           NO PTR PROD                                  
         BNE   BLDTR10                                                          
         B     BLDTR18                                                          
         SPACE                                                                  
BLDTR14  CLI   TBYPRD2,0           NO PTR PROD                                  
         BE    BLDTR10                                                          
         B     *+12                                                             
         SPACE                                                                  
BLDTR16  CLI   TBYPRD2,0                                                        
         BE    BLDTR18                                                          
         MVC   BYTE,TBYPRD2                                                     
         BRAS  RE,GGETPRD                                                       
         CLI   BPRD2,0             ANY PTR SELECTION                            
         BE    *+14                                                             
         CLC   BPRD2,BYTE                                                       
         BNE   BLDTR10                                                          
         MVC   ACLPRD2,BYTE                                                     
         MVC   ACLSLN2,TBYSLN2                                                  
         MVC   ACLEBCP2,0(R1)                                                   
         CLC   ACLEBCP1,ACLEBCP2                                                
         BL    BLDTR18                                                          
         SPACE                                                                  
* REVERSE THE ENTRIES *                                                         
         SPACE                                                                  
         XC    ACLPRD(2),ACLPRD2                                                
         XC    ACLPRD2(2),ACLPRD                                                
         XC    ACLPRD(2),ACLPRD2                                                
         XC    ACLEBCP1,ACLEBCP2                                                
         XC    ACLEBCP2,ACLEBCP1                                                
         XC    ACLEBCP1,ACLEBCP2                                                
         SPACE                                                                  
BLDTR18  MVC   ACLCOPY,TBYCODE                                                  
         SPACE                                                                  
         MVC   ACLFTD(4),WORK                                                   
         SPACE                                                                  
         L     R4,AACLST                                                        
         SPACE                                                                  
BLDTR20  CLI   ACLPRD,0                                                         
         BE    BLDTR28                                                          
         CLC   ACLCLI(7),ACLWORK+6    PRD/SLN/PRD2/SLN2/COPY                    
         BNE   BLDTR26                                                          
         SPACE                                                                  
*                                                                               
         MVC   SVDATE,ACLWORK+ACLFTD-ACLDATA   MOVE FTD                         
         MVC   SVDATEX,ACLWORK+ACLLTD-ACLDATA  MOVE LTD                         
*                                                                               
         BAS   RE,EXPANDTE         EXPAND DATES ONLY IF NO GAP                  
         BE    BLDTR10                                                          
*                                                                               
         MVC   ACLWORK+ACLFTD-ACLDATA,SVDATE  RESTORE DATES                     
         MVC   ACLWORK+ACLLTD-ACLDATA,SVDATEX                                   
*                                                                               
BLDTR26  LA    R4,L'ACLDATA(R4)                                                 
         L     R0,AIO3                                                          
         AHI   R0,3500                                                          
         CR    R0,R4                                                            
         BH    BLDTR20                                                          
         DC    H'0'                EXCEEDED TABLE SIZE                          
BLDTR28  MVC   0(L'ACLDATA,R4),ACLWORK                                          
         B     BLDTR10                                                          
         SPACE                                                                  
BLDTR40  MVC   KEYSAVE,KEY         SAVE BUY DATA                                
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 SEQ                                                              
         B     BLDTR04                                                          
         EJECT                                                                  
BLDTR90  L     R4,AACLST                                                        
         SPACE                                                                  
         CLI   ACLPRD,0            TEST ACTIVITY IN TABLE                       
         BE    BLDTR120            NO - TEST TO CONTINUE                        
         SPACE                                                                  
* BLANK COPY CODES WITHOUT PATTERNS IF COPY CODE = DAYPART *                    
         SPACE                                                                  
BLDTR95  CLI   SVT1PR8,C'Y'        TEST AUTO P/B PATTN                          
         BE    BLDTR96                                                          
         CLI   SVPROF11,C'A'       TEST ADJACENCY=COPY CODE                     
         BE    BLDTR95C                                                         
         CLI   SVPROF11,C'D'       TEST DPT=COPY CODE                           
         BE    BLDTR95C                                                         
         CLI   SVPROF11,C'P'       TEST DPT=1ST CHAR OF PROGRAM CODE            
         BE    BLDTR95C                                                         
         CLI   SVPROF11,C'Y'       TEST DPT=COPY CODE                           
         BNE   BLDTR96                                                          
         SPACE                                                                  
* CHECK WHICH DAYPARTS HAVE PATTERNS *                                          
         SPACE                                                                  
BLDTR95C BRAS  RE,CHKPTN                                                        
         GOTO1 HIGH                LAST BUY RECORD                              
         EJECT                                                                  
* SORT BY COPY CODE/SLN2/SLN1/EBCDIC PRD *                                      
         SPACE                                                                  
BLDTR96  L     R4,AACLST                                                        
         SR    R0,R0                                                            
*                                                                               
         CLI   0(R4),0                                                          
         BE    *+12                                                             
         LA    R4,L'ACLDATA(R4)                                                 
         BCT   R0,*-12                                                          
*                                                                               
         L     R4,AACLST                                                        
         LPR   R0,R0                                                            
*                                                                               
         GOTO1 XSORT,DMCB,(R4),(R0),L'ACLDATA,1,10  COPY CODE                   
         GOTO1 (RF),(R1),,,,,9                      SLN2                        
         GOTO1 (RF),(R1),,,,,7                      SLN1                        
         GOTO1 (RF),(R1),,,,6,0                     ALPHA PROD1 & 2             
         EJECT                                                                  
* BUILD TSAROFF ENTRIES FROM ACTIVITY LIST *                                    
         SPACE                                                                  
BLDTR100 DS    0H                                                               
         SPACE                                                                  
         LHI   R2,TSARBLK-SYSD                                                  
         AR    R2,R9                                                            
         USING TSARD,R2                                                         
         SPACE                                                                  
         XC    BLOCK(10+L'TSRENT),BLOCK                                         
         SPACE                                                                  
         LA    RE,L'TSRENT         LEN OF ENTRY                                 
         LA    RE,2(RE)            PLUS 2 FOR LEN FIELD                         
         STCM  RE,3,BLOCK                                                       
         SPACE                                                                  
         LA    R5,BLOCK+2          RECORD IS IN BLOCK+2                         
         USING TSRBUYD,R5                                                       
         SPACE                                                                  
         L     R4,AACLST                                                        
         USING ACLDATA,R4                                                       
         B     *+10                                                             
BLDTR110 XC    BLOCK+2(10+L'TSRENT),BLOCK+2  DO NOT CLEAR ENTRY LEN             
         SPACE                                                                  
         MVC   TSRMKT,KEYSAVE+6   MARKET                                        
         MVC   TSRCSTA,KEYSAVE+8  AND CABLE STATION IF ANY                      
         SPACE                                                                  
         TM    SVOPTSW,OPTCABLE                                                 
         BO    *+12                                                             
         CLI   KEYSAVE+8,CABLESTA  IF THIS IS A CABLE STATION,                  
         BL    BLDTR115                                                         
         SPACE                                                                  
***      CLI   SVT2PR05,C'Y'       COMBINE CABLE NETWORKS                       
***      BNE   BLDTR115             NO                                          
         B     BLDTR115                                                         
         SPACE                                                                  
         CLI   SPOTCAN,C'C'        IF CANADIAN                                  
         BE    BLDTR115             NOT CABLE                                   
         SPACE                                                                  
*                    X'E8' NOW                                                  
         CLI   TSRCSTA,CABLESTA    THIS A CABLE STATION                         
         BL    BLDTR115             NO                                          
         NI    TSRCSTA+2,CABLENET                                               
         SPACE                                                                  
BLDTR115 DS    0H                                                               
         SPACE                                                                  
         MVC   TSRCLI,ACLCLI      CLIENT                                        
         MVC   TSRPRD,ACLPRD                                                    
         MVC   TSRPRD2,ACLPRD2                                                  
         MVC   TSRSLN1,ACLSLN                                                   
         MVC   TSRSLN2,ACLSLN2                                                  
         MVC   TSREST,ACLCOPY                                                   
         MVC   TSRFTD,ACLFTD                                                    
         MVC   TSRLTD,ACLLTD                                                    
*                                                                               
         LA    R1,BLOCK                                                         
         ST    R1,TSAREC                                                        
         SPACE                                                                  
         MVI   TSOFFACT,TSAADD      SET TSAROFF ADD                             
         GOTO1 TSAROFF,(R2)                                                     
         SPACE                                                                  
         TM    TSERRS,X'20'         DUPLICATE KEY ON ADD                        
         BO    BLDTR118             YES, SAME BUY DIFFERENT BUY LINE #          
         SPACE                                                                  
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
BLDTR118 LA    R4,L'ACLDATA(R4)                                                 
         CLI   ACLPRD,0            TEST MORE DATA                               
         BNE   BLDTR110             YES                                         
         SPACE                                                                  
BLDTR120 XC    FILENAME,FILENAME   SWITCH BACK TO STRAFFIC                      
         SPACE                                                                  
         CLC   =X'0A32',KEY        INSURANCE                                    
         BNE   BLDXX                NO, DONE                                    
         SPACE                                                                  
         CLI   ALLCLI,C'Y'         IF RUNNING FOR ALL CLIENTS,                  
         BNE   BLDTR125                                                         
         CLC   KEY+2(1),BAGYMD     TEST SAME A-M                                
         BNE   BLDXX                NO, DONE                                    
         CLC   KEY+2(3),BAGYMD     TEST SAME A-M/C                              
         BE    BLDTR130                                                         
         XC    KEYSAVE,KEYSAVE                                                  
         B     BLDTR02             NO, CONTINUE WITH NEXT CLIENT                
*                                                                               
BLDTR125 CLC   KEY+2(3),BAGYMD     TEST SAME A-M/C/P                            
         BNE   BLDXX                NO, DONE                                    
         SPACE                                                                  
BLDTR130 L     RE,AACLST           CLEAR ACTIVITY LIST AREA                     
         L     RF,AACLSTX                                                       
         SR    RF,RE                                                            
         XCEFL                                                                  
         SPACE                                                                  
         OC    BMKT,BMKT           SINGLE MARKET REQUEST                        
         BNZ   BLDXX                YES DONE                                    
         SPACE                                                                  
         MVC   KEYSAVE,KEY                                                      
         TM    SVOPTSW,OPTCABLE                                                 
         BZ    BLDTR04                                                          
         SPACE                                                                  
         CLI   SPOTCAN,C'C'        IF CANADIAN                                  
         BE    BLDTR04              NOT CABLE                                   
         SPACE                                                                  
*                    X'E8' NOW                                                  
         CLI   KEY+8,CABLESTA      THIS A CABLE STATION                         
         BNL   BLDTR04                                                          
         SPACE                                                                  
*                    X'E8' NOW                                                  
         MVI   KEY+8,CABLESTA      FORCE NEXT STATION                           
         XC    KEY+9(4),KEY+9      AND CLEAR THE REST                           
         SPACE                                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         XC    KEYSAVE+6(5),KEYSAVE+6  CLEAR MKT/STA                            
         B     BLDTR04                                                          
         SPACE                                                                  
         DROP  R5                                                               
         EJECT                                                                  
GETEL2   AH    R6,DATADISP                                                      
*                                                                               
FIRSTEL2 CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                                                             
NEXTEL2  SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
         B     FIRSTEL2                                                         
         SPACE 3                                                                
BUYEL    CLI   0(R6),0                                                          
         BE    BUYELX                                                           
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   0(1,R6),ELCDLO                                                   
         BL    BUYEL                                                            
         CLC   0(1,R6),ELCDHI                                                   
         BH    BUYEL                                                            
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
BUYELX   LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
         DROP  RB,RC                                                            
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* FIND COPY CODE IF ANY, 1ST FROM EST, WHICH OVERRIDES ALL ELSE                 
* THEN FROM BUY IF DPT=COPY CODE PROFILE IS ON                                  
* THEN CHANGE FROM EQUIVALENCY TABLE IF PROFILE ON AND TABLE ON FILE            
*                                                                               
         SPACE                                                                  
CPY      NMOD1 0,**CPY*                                                         
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
         USING ACLDATA,R4                                                       
         USING BUYRECD,R5                                                       
         ZIC   RF,BUYKEST                                                       
         CLI   SVPROF11,C'E'      COPY CODE = ESTIMATE                          
         BE    CPY40                                                            
         LA    RF,SVESTAB(RF)                                                   
         SPACE                                                                  
         CLI   SVPROF11,C'P'      USING 1ST CHAR OF PROGRAM CODE                
         BE    CPY24                YES, ALL OKAY                               
         SPACE                                                                  
         CLI   0(RF),X'FF'        COPY CODE FROM ESTIMATE                       
         BE    CPY14               NO                                           
         MVC   ACLCOPY,0(RF)                                                    
         CLI   OFFLINE,C'Y'                                                     
         BE    CPYX                                                             
         CLI   SVPROF11,C'D'      ERROR IF DPT=COPY CODE ON                     
         BE    CPY10                                                            
         CLI   SVPROF11,C'Y'      ERROR IF DPT=COPY CODE ON                     
         BE    CPY10                                                            
         SPACE                                                                  
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
CPYX     XIT1                                                                   
         SPACE                                                                  
CPYESTER MVC   CONHEAD+10(36),=C'COPY CODED EST XXX HAS EQUIV TABLE *'          
         ZIC   R0,BUYKEY+9                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CONHEAD+25(3),DUB                                                
         B     CPYERRX                                                          
ADJCDER  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD+10(29),=C'ADJACENCY CD NOT A-Z OR 0-9 *'                 
CPYERRX  MVC   CONHEAD(9),=C'* ERROR *'                                         
         GOTO1 ERREX2                                                           
         SPACE                                                                  
         DROP  RB,RC,R4,R7,R5                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* READ MARKET LIST RECORD & SAVE IT IN THE LAST 120 BYTES OF ELEM               
         EJECT                                                                  
*****************************************************                           
* SUBROUTINE TO READ AND FILTER ESTIMATE HEADERS ON *                           
* REQUESTED DATES.                                  *                           
*****************************************************                           
         SPACE                                                                  
         DS    0H                                                               
BLDEST   NMOD1 0,*BLDE*,R7                                                      
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
         XC    SVESTAB,SVESTAB     CLEAR SVESTAB                                
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD     A-M/CLT                                      
         SPACE                                                                  
         MVC   KEY+4(3),QPRD                                                    
         SPACE                                                                  
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
         SPACE                                                                  
         OC    KEY+8(5),KEY+8      TEST BILL                                    
         BNZ   BLDEST20                                                         
         SPACE                                                                  
         CLI   QBEST,0             ANY REQUESTED ESTIMATES                      
         BE    BLDEST14             NO                                          
         CLC   QBEST,KEY+7         IN REQUESTED EST RANGE                       
         BL    BLDEST20             NO                                          
         CLC   QBESTEND,KEY+7      IN REQUESTED EST RANGE                       
         BH    BLDEST20             NO                                          
         SPACE                                                                  
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
         SPACE                                                                  
         ZIC   RE,KEY+7                                                         
         LA    RE,SVESTAB(RE)                                                   
         MVC   0(1,RE),ECOPY       SET COPY CODE IN TABLE                       
         CLI   0(RE),0                                                          
         BNE   *+12                                                             
         MVI   0(RE),X'FF'                                                      
         B     BLDEST20                                                         
         SPACE                                                                  
         CLI   SVPROF11,C'Y'      COPY CODE BY DAYPART                          
         BE    ESTCPYER                                                         
         CLI   SVPROF11,C'D'      COPY CODE BY DAYPART                          
         BE    ESTCPYER                                                         
         SPACE                                                                  
BLDEST20 MVC   KEY+8(5),HEXFF      FORCE NEXT EST                               
         CLI   QBEST,0             TEST NO ESTIMATE REQUEST                     
         BE    BLDEST10             YES - CONTINUE                              
         CLC   KEY+7(1),QBESTEND   TEST PAST END OF SERIES OR ONE               
         BL    BLDEST10             NO - CONTINUE                               
         SPACE                                                                  
* SET HI AND LOW EST NUMBERS FOR EST=NO *                                       
         SPACE                                                                  
BLDEST30 DS   0H                                                                
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
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
         SPACE                                                                  
         LA    R1,SVESTAB                                                       
         SR    RE,R1                                                            
         STC   RE,BEST                                                          
         SPACE                                                                  
         LA    RE,SVESTAB+255                                                   
         LA    R0,255                                                           
         CLI   0(RE),0                                                          
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         BCT   R0,*-10                                                          
         SPACE                                                                  
         STC   R0,BESTEND                                                       
         SPACE                                                                  
         B     EQXIT                                                            
         SPACE                                                                  
BLDEST40 B     BLDEST44    ******** BYPASS CODE BELOW **********                
*                          *************************************                
*        CLI   SVTBPR04,C'Y'       TBUY ACCEPT NO ESTIMATES                     
         BE    BLDEST44                                                         
         CLC   AGENCY,=C'TH'       OK IF BACKER                                 
         BE    BLDEST42                                                         
         CLC   AGENCY,=C'BS'       OK IF BACKER                                 
         BNE   NEQXIT                                                           
BLDEST42 CLC   QCLT,=C'MT6'        MILLER                                       
         BE    BLDEST44                                                         
         CLC   QCLT,=C'MT2'                                                     
         BE    BLDEST44                                                         
         CLC   QCLT,=C'MT3'                                                     
         BE    BLDEST44                                                         
         CLC   QCLT,=C'MT4'        MILLER                                       
         BE    BLDEST44                                                         
         CLC   QCLT,=C'OT4'        MOLSON                                       
         BNE   NEQXIT                                                           
BLDEST44 MVI   SVESTAB,X'FF'       SET ALL ESTIMATES ACTIVE                     
         MVC   SVESTAB+1(255),SVESTAB                                           
         MVI   BEST,1                                                           
         MVI   BESTEND,255                                                      
EQXIT    CR    RE,RE                                                            
         B     *+6                                                              
NEQXIT   LTR   RE,RE                                                            
         XIT1                                                                   
         SPACE                                                                  
ESTCPYER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(9),=C'* ERROR *'                                         
         MVC   CONHEAD+10(50),=C'COPY CODED EST XXX-COPY CODE BY DAYPARC        
               T PROF ON *'                                                     
         ZIC   R0,KEY+7                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  CONHEAD+25(3),DUB                                                
         GOTO1 ERREX2                                                           
         SPACE 3                                                                
HEXFF    DC    6X'FF'                                                           
         EJECT                                                                  
         DROP  RB,RC                                                            
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
* SEE PRODUCT IS EQUIVALENT *                                                   
         EJECT                                                                  
* CHECK OUT MARKET GROUP RECS TO GET NEXT MARKET - SPOT BUYS *                  
         SPACE                                                                  
         DS    0H                                                               
RMGB     NMOD1 0,*RMGB*,R7                                                      
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
RMGB10   MVC   ELEM(18),KEY                                                     
         CLI   SVPROF15,C'Y'       USE TRAFFIC, NOT SPOT BUYS                   
         BE    *+14                                                             
         MVC   ELEM+40(2),KEY+4    SAVE MARKET                                  
         B     *+10                                                             
         MVC   ELEM+40(2),KEY+6    SAVE MARKET                                  
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(3),BAGYMD & BCLT                                           
         MVC   KEY+8(1),SVMGRPS                                                 
         MVC   KEY+9(2),SVMGR                                                   
         MVC   KEY+11(2),ELEM+40   THIS MARKET                                  
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     FIND MARKET GROUP                            
         BE    RMGB20               YES                                         
         SPACE                                                                  
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLIENT                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     AT END OF MARKET GROUP                       
         BE    RMGB20               NO                                          
         MVC   BMKT,KEY+4                                                       
         B     RMGB40                                                           
         SPACE                                                                  
RMGB20   MVC   ELEM+18(18),KEY     SAVE MARKET GROUP                            
         MVC   KEY(18),ELEM                                                     
         SPACE                                                                  
         CLI   SVPROF15,C'Y'       USE TRAFFIC, NOT SPOT BUYS                   
         BE    RMGB25                                                           
         SPACE                                                                  
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
         SPACE                                                                  
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
         SPACE                                                                  
RMGB30   GOTO1 HIGH                                                             
         SPACE                                                                  
RMGB40   DS    0H                                                               
         XC    FILENAME,FILENAME                                                
         CR    RB,RB                                                            
         XIT1                                                                   
RMGBX    XC    FILENAME,FILENAME                                                
         CR    RB,RC                                                            
         XIT1                                                                   
         SPACE                                                                  
         DROP  RB,R7,RC                                                         
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* GET BUY FOR THIS INSTRUCTION RECAP ENTRY (IF ANY)                             
         SPACE                                                                  
GBUY     NMOD1 0,**GBUY**                                                       
         SPACE                                                                  
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
* SAVE RECAP DATES                                                              
         SPACE                                                                  
         NI    SVFLAG,X'FF'-BUYFSW    INIT BUY FOUND SWITCH                     
         NI    SVFLAG,X'FF'-DTECOVSW    ALL DATES COVERED SW                    
         SPACE                                                                  
         CLI   CLRDATE,C'N'           DON'T CLEAR DATES, SAME MARKET            
         BE    GBUY04                                                           
         XC    BDATELST(200),BDATELST      INIT BUY DATES LIST                  
         XC    BDATELST+200(200),BDATELST+200                                   
         XC    BDATELST+400(202),BDATELST+400                                   
         SPACE                                                                  
GBUY04   XC    SVRFTD(4),SVRFTD    INIT ORIGINAL RECAP FTD/LTD                  
         MVC   SVRFTD,CMLFTD-CMLENT+HOLDALL SAVE RECAP FTD                      
         MVC   SVRLTD,CMLLTD-CMLENT+HOLDALL SAVE RECAP LTD                      
         SPACE                                                                  
         LHI   R2,TSARBLK-SYSD                                                  
         AR    R2,R9                                                            
         USING TSARD,R2                                                         
         SPACE                                                                  
         XC    ELEM(256),ELEM                                                   
         LA    R5,ELEM                                                          
         ST    R5,TSAREC           SET ADDRESS OF THE RECORD                    
         SPACE                                                                  
         LA    R5,ELEM+2           RECORD IS IN ELEM+2                          
         USING TSRBUYD,R5                                                       
         SPACE                                                                  
         MVC   TSRCLI,CMLCLI-CMLENT+HOLDALL MOVE CLIENT                         
         MVC   TSRMKT,CMLMKT-CMLENT+HOLDALL  MOVE MARKET                        
         MVC   TSRPRD(4),CMLPRD-CMLENT+HOLDALL PRD/LEN/PRD2/LEN2                
         OC    BLOCK+3(3),BLOCK+3  MULTIPLE ESTIMATES (EST/PRD)                 
         BNZ   GBUY05               YES, DO NOT FILL IN ESTIMATE                
         CLI   CMLINST-CMLENT+HOLDALL,C'B'    IF FROM X'30' ELEMENT             
         BE    GBUY05              DO NOT FILL IN ESTIMATE                      
         MVC   TSREST,CMLEST-CMLENT+HOLDALL    ESTIMATE                         
         SPACE                                                                  
GBUY05   MVI   TSOFFACT,TSARDH     SET GET BY KEY                               
         B     *+14                                                             
GBUY10   XC    ELEM(256),ELEM                                                   
         MVI   TSOFFACT,TSANXT     SET GET NEXT RECORD                          
*                                                                               
         GOTO1 TSAROFF,(R2)                                                     
*                                                                               
         TM    TSERRS,X'80'        END OF FILE                                  
         BO    GBUY100                                                          
*                                                                               
         TM    TSERRS,X'10'                                                     
         BO    *+14                                                             
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R5,ELEM+2                                                        
*                                                                               
         CLC   TSRCLI,CMLCLI-CMLENT+HOLDALL  SAME CLIENT                        
         BNE   GBUY100                                                          
*                                                                               
         CLC   TSRMKT,CMLMKT-CMLENT+HOLDALL  SAME MARKET                        
         BNE   GBUY100                                                          
*                                                                               
         TM    SVOPTSW,OPTCABLE                                                 
         BO    *+12                                                             
         CLI   CMLCBL-CMLENT+HOLDALL,C'C'    IF CABLE,                          
         BNE   GBUY12                                                           
         CLC   TSRCSTA,CMLCSTA-CMLENT+HOLDALL  SAME CABLE STA                   
         BE    GBUY15                                                           
         CLI   CMLCSTA-CMLENT+HOLDALL+2,0   COMBINED?                           
         BE    GBUY11                                                           
         CLI   CMLCSTA-CMLENT+HOLDALL+2,CABLENET                                
         BNE   GBUY10                                                           
GBUY11   CLC   TSRCSTA(2),CMLCSTA-CMLENT+HOLDALL                                
         BNE   GBUY10                                                           
         B     GBUY15                                                           
*                                                                               
GBUY12   CLI   CMLCBL-CMLENT+HOLDALL,C'C'    IF NOT CABLE,                      
         BE    GBUY15                                                           
         CLI   TSRCSTA,CABLESTA   SKIP IF STATION IS CABLE                      
         BH    GBUY10                                                           
         CLC   TSRCSTA,CMLCSTA-CMLENT+HOLDALL  SAME STATION                     
         BNE   GBUY10                                                           
*                                                                               
GBUY15   CLC   TSRPRD(4),CMLPRD-CMLENT+HOLDALL PRD/LEN/PRD2/LEN2                
         BNE   GBUY10                                                           
*                                                                               
         OC    BLOCK+3(3),BLOCK+3  MORE THAN ONE ESTIMATE                       
         BNZ   GBUY20               YES                                         
         CLI   CMLINST-CMLENT+HOLDALL,C'B'    IF FROM X'30' ELEMENT             
         BE    GBUY20              IGNORE ESTIMATE                              
*                                                                               
         CLC   TSREST,CMLEST-CMLENT+HOLDALL    ESTIMATE                         
         BNE   GBUY10                                                           
         B     GBUY25                                                           
*                                  IF MULTIPLE ESTIMATES,                       
GBUY20   TM    TSRFLG,NOBUYFR      AND NO BUY FOR THIS RECAP                    
         BZ    GBUY21                                                           
         CLC   TSRFTD(4),=X'FFFFFFFF'  AND NO BUY DATES                         
         BNE   GBUY21                                                           
         CLC   TSRFTD(4),SVRFTD    AND DIFFERENT FTD/LTD                        
         BE    GBUY21                                                           
*                                                                               
         XC    ACLWORK,ACLWORK                                                  
         MVC   ACLWORK(L'TSRENT),TSRENT  SAVE CURRENT ENTRY                     
         XC    ELEM(256),ELEM                                                   
GBUY20A  MVI   TSOFFACT,TSANXT     SET GET NEXT RECORD                          
*                                                                               
         GOTO1 TSAROFF,(R2)                                                     
*                                                                               
         TM    TSERRS,X'80'        END OF FILE?                                 
         BNO   GBUY24                                                           
         MVC   TSRENT,ACLWORK      REREAD SAVED KEY FROM EARLIER                
         MVI   TSOFFACT,TSARDH     READ HI                                      
         GOTO1 TSAROFF,(R2)                                                     
         CLI   TSERRS,0            BETTER BE THERE                              
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
GBUY21   LA    R1,BLOCK                                                         
         LA    R0,85                                                            
*                                                                               
GBUY22   OC    0(3,R1),0(R1)       ANY MORE EST/PRD                             
         BZ    GBUY10               NO, NO MATCH                                
         CLC   TSREST,0(R1)        SAME ESTIMATE                                
         BNE   GBUY23                                                           
         MVI   2(R1),X'FF'         BUY FOR THIS EST IS FOUND                    
         B     GBUY25                                                           
GBUY23   LA    R1,3(R1)            BUMP IN BLOCK TO NEXT EST/PRD                
         BCT   R0,GBUY22                                                        
         SPACE                                                                  
         B     GBUY10                                                           
*                                                                               
GBUY24   TM    TSERRS,X'10'                                                     
         BO    *+14                                                             
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R5,ELEM+2                                                        
*                                                                               
         BRAS  RE,FILTTSR          FILTER TSAR RECORD                           
         BNE   GBUY20A             GET NEXT RECORD                              
*                                                                               
         SPACE                                                                  
GBUY25   CLC   TSRFTD(4),SVRFTD    SAME FTD/LTD                                 
         BNE   GBUY40                                                           
         MVC   FTDATE(4),SVRFTD    YES, USE THEM                                
         SPACE                                                                  
GBUY30   OI    TSRFLG,ENTBUY       BUY COVERED BY RECAP                         
         OI    SVFLAG,DTECOVSW     ALL DATES COVERED SW                         
         OI    SVFLAG,BUYFSW       AT LEAST 1 BUY FOUND SW                      
         BAS   RE,ABUYDTE          ADD BUY DATE TO LIST                         
         B     GBUY90              WRITE BACK ENTRY                             
         SPACE                                                                  
GBUY40   DS    0H                                                               
         TM    TSRFLG,NOBUYFR      NO BUY FOR THIS RECAP                        
         BZ    *+16                                                             
         CLC   TSRFTD(4),=X'FFFFFFFF'  S/B NO BUY DATES                         
         BE    GBUY138             JUST ADD RECAP DATES AND WRITE BACK          
         DC    H'0'                FLAG AND DATES ???                           
         SPACE                                                                  
         CLC   TSRLTD,SVRFTD       SEE THAT BUY DATES ARE                       
         BL    GBUY10               NOT OUTSIDE OF RECAP                        
         CLC   TSRFTD,SVRLTD         DATES.                                     
         BH    GBUY10                                                           
         SPACE                                                                  
         OI    SVFLAG,BUYFSW       BUY FOUND SWITCH                             
         SPACE                                                                  
         TM    SVFLAG,DTECOVSW     PREV BUY COVERED ALL DATES                   
         BO    GBUY60               YES, BYPASS                                 
***********************************                                             
         MVC   FTDATE(4),SVRFTD    PRESET DATES TO RECAP DATES                  
         SPACE                                                                  
         CLC   TSRFTD,SVRFTD       BUY FTD/RECAP FTD                            
         BNH   *+10                                                             
         MVC   FTDATE,TSRFTD       USE BUY FTD ITS LATER                        
         SPACE                                                                  
         CLC   TSRLTD,SVRLTD                                                    
         BNL   *+10                                                             
         MVC   LTDATE,TSRLTD       USE BUY LTD ITS EARLIER                      
         SPACE                                                                  
***********************************                                             
GBUY60   DS    0H                                                               
         BAS   RE,ABUYDTE          ADD BUY/RECAP DATES TO LIST                  
         SPACE                                                                  
* SAVE INST RECAP DATES THAT APPLIED TO THIS BUY                                
         SPACE                                                                  
         LA    RE,TSRRFTD          RECAP FTD                                    
         SPACE                                                                  
GBUY75   DS    0H                                                               
         OC    0(4,RE),0(RE)       ANY DATES                                    
         BZ    GBUY80                                                           
         SPACE                                                                  
         CLC   SVRFTD(4),0(RE)     SEE IF DATE ALREADY IN TABLE                 
         BE    GBUY10               YES, DONE                                   
         SPACE                                                                  
         LA    RE,L'TSRRENT(RE)    BUMP                                         
         B     GBUY75                                                           
         SPACE                                                                  
GBUY80   MVC   0(L'TSRRENT,RE),SVRFTD SAVE INSTR RECAP DATES                    
         SPACE                                                                  
         ZICM  R1,ELEM,2           RECORD LEN                                   
         LA    R1,L'TSRRENT(R1)                                                 
         STCM  R1,3,ELEM           NEW LEN                                      
         SPACE                                                                  
GBUY90   XC    ACLWORK,ACLWORK                                                  
         MVC   ACLWORK(L'TSRENT),TSRENT SAVE CURRENT ENTRY                      
         SPACE                                                                  
         MVI   TSOFFACT,TSAWRT     WRITE BACK THE RECORD                        
         GOTO1 TSAROFF,(R2)                                                     
         SPACE                                                                  
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         MVC   TSRENT,ACLWORK      LAST KEY PROCESSED                           
         SPACE                                                                  
         MVI   TSOFFACT,TSARDH     DUMMY HI                                     
         GOTO1 TSAROFF,(R2)                                                     
         SPACE                                                                  
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                BETTER BE THERE                              
         SPACE                                                                  
         B     GBUY10                                                           
         SPACE                                                                  
GBUY100  DS   0H                                                                
         TM    SVFLAG,BUYFSW       WAS AT LEAST ONE BUY FOUND                   
         BZ    GBUY130              NO                                          
         SPACE                                                                  
GBUY101  DS    0H                  ORGANIZE DATES                               
**NO-OP  TM    SVFLAG,DTECOVSW     BUY COVERED ALL DATES                        
**05/10  BO    GBUY210              YES                                         
         SPACE                                                                  
         LA    R7,BDATELST         BUY DATES LIST                               
         LR    R4,R7                                                            
GBUY102  OC    0(4,R4),0(R4)       ANY DATES                                    
         BZ    GBUY103                                                          
         LA    R4,4(R4)                                                         
         B     GBUY102                                                          
         SPACE                                                                  
GBUY103  SR    R4,R7               R4=LEN OF ENTRY                              
         LTR   R4,R4                                                            
         BZ    GBUY104             NO RECAP DATES FOR THIS BUY                  
         SPACE                                                                  
         SRL   R4,2                DIVIDE BY 4=NO OF ENTRIES                    
         LTR   R4,R4                                                            
         BZ    GBUY104             NO RECAP DATES FOR THIS BUY                  
         SPACE                                                                  
         STC   R4,BYTE                                                          
         CLI   BYTE,1                                                           
         BE    GBUY210                                                          
         SPACE                                                                  
GBUY104  BRAS  RE,SRTDTES          GO SORT DATES                                
         SPACE                                                                  
         BRAS  RE,EXPDTES          GO EXPAND THE DATES WHERE POSSIBLE           
         SPACE                                                                  
         B     GBUY210                                                          
         SPACE                                                                  
*                                                                               
*BUY104F MVC   CMLFTD-CMLENT+HOLDALL(4),BDATELST MOVE IN BUY DTES               
*        CLC   SVRFTD,BDATELST      RECAP FTD                                   
*        BNH   *+10                  IS EARLIER                                 
*        MVC   CMLFTD-CMLENT+HOLDALL(2),SVRFTD                                  
*        SPACE                                                                  
*        CLC   SVRLTD,BDATELST+2    RECAP LTD                                   
*        BNL   *+10                  IS LATER                                   
*        MVC   CMLLTD-CMLENT+HOLDALL(2),SVRLTD                                  
*        SPACE                                                                  
*BUY105  NI    SVFLAG,X'FF'-BUYFSW                                              
*        SPACE                                                                  
*        CLC   CMLFTD-CMLENT+HOLDALL(2),CMLLTD-CMLENT+HOLDALL                   
*        BNH   GBUY210                                                          
*        SPACE                                                                  
*        XC    CMLFTD-CMLENT+HOLDALL(2),CMLLTD-CMLENT+HOLDALL                   
*        XC    CMLLTD-CMLENT+HOLDALL(2),CMLFTD-CMLENT+HOLDALL                   
*        XC    CMLFTD-CMLENT+HOLDALL(2),CMLLTD-CMLENT+HOLDALL                   
*******  B     GBUY210                                                          
         SPACE                                                                  
* THERE IS RECAP RECORD BUT NO BUY                                              
* WAS BUY DELETED ?                                                             
         SPACE                                                                  
GBUY130  DS    0H                                                               
         SPACE                                                                  
         CLC   SVRLTD,PERENDP      LTD TO PERIOD END DATE                       
         BH    GBUY200             DONE, LTD AFTER PERIOD END DATE              
         SPACE                                                                  
         XC    ELEM(256),ELEM                                                   
         SPACE                                                                  
         LA    RE,L'TSRENT         LEN OF ENTRY                                 
         LA    RE,2(RE)            PLUS 2 FOR LEN FIELD                         
         STCM  RE,3,ELEM                                                        
         SPACE                                                                  
         LA    R5,ELEM+2           RECORD IS IN ELEM+2                          
         USING TSRBUYD,R5                                                       
         SPACE                                                                  
         MVC   TSRCLI,CMLCLI-CMLENT+HOLDALL  CLIENT                             
         MVC   TSRMKT,CMLMKT-CMLENT+HOLDALL  MOVE MARKET                        
         MVC   TSRCSTA,CMLCSTA-CMLENT+HOLDALL  AND CABLE STA                    
         MVC   TSRPRD(4),CMLPRD-CMLENT+HOLDALL PRD/LEN/PRD2/LEN2                
         MVC   TSREST,CMLEST-CMLENT+HOLDALL    ESTIMATE                         
         MVC   TSRFTD,=X'FFFF'     DUMMY FTD                                    
         MVC   TSRLTD,TSRFTD       AND LTD                                      
         SPACE                                                                  
         OI    TSRFLG,NOBUYFR      NO BUY FOR THIS RECAP                        
         SPACE                                                                  
         MVC   TSRRFTD,SVRFTD      RECAP FIRST AND                              
         MVC   TSRRLTD,SVRLTD      LAST TELECAST DATES                          
         SPACE                                                                  
         ZICM  R1,ELEM,2           RECORD LEN                                   
         LA    R1,L'TSRRENT(R1)                                                 
         STCM  R1,3,ELEM           NEW LEN                                      
         SPACE                                                                  
         LA    R5,ELEM                                                          
         ST    R5,TSAREC           SET ADDRESS OF THE RECORD                    
         SPACE                                                                  
         LA    R5,ELEM+2           RECORD IS IN ELEM+2                          
         SPACE                                                                  
         MVI   TSOFFACT,TSAADD      SET TSAROFF ADD                             
         GOTO1 TSAROFF,(R2)                                                     
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         B     GBUY200                                                          
         SPACE                                                                  
* JUST SAVE RECAP DATES                                                         
         SPACE                                                                  
GBUY138  LA    RE,TSRRFTD          RECAP FTD                                    
         SPACE                                                                  
GBUY140  OC    0(4,RE),0(RE)       ANY DATES                                    
         BZ    GBUY150                                                          
         SPACE                                                                  
         CLC   SVRFTD(4),0(RE)     SEE IF DATE ALREADY IN TABLE                 
         BE    GBUY200                                                          
         SPACE                                                                  
         LA    RE,L'TSRRENT(RE)    BUMP                                         
         B     GBUY140                                                          
         SPACE                                                                  
GBUY150  MVC   0(L'TSRRENT,RE),SVRFTD SAVE INSTR RECAP DATES                    
         SPACE                                                                  
         ZICM  R1,ELEM,2           RECORD LEN                                   
         LA    R1,L'TSRRENT(R1)                                                 
         STCM  R1,3,ELEM           NEW LEN                                      
         SPACE                                                                  
         MVI   TSOFFACT,TSAWRT     WRITE BACK THE RECORD                        
         GOTO1 TSAROFF,(R2)                                                     
         SPACE                                                                  
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         TM    SVFLAG,BUYFSW       WAS AT LEAST ONE BUY FOUND                   
         BO    GBUY101              YES                                         
         SPACE                                                                  
GBUY200  LTR   RB,RB               SET CC NE                                    
         B     GBUYX                                                            
GBUY210  CR    RB,RB               SET CC EQ                                    
GBUYX    XIT1                                                                   
         EJECT                                                                  
* ADD BUY DATES THAT APPLY TO THIS RECAP                                        
         SPACE                                                                  
ABUYDTE  NTR1                                                                   
         SPACE                                                                  
         LA    R4,BDATELSZ         WILL FIT UPTO 200 BUY DATES                  
         LA    R2,BDATELST         BUY DATES LIST                               
         SPACE                                                                  
ABD10    OC    0(4,R2),0(R2)       ANY ENTRY                                    
         BZ    ABD70               GO SAVE DATES                                
         SPACE                                                                  
         CLC   FTDATE(4),0(R2)     SAME DATES                                   
         BE    ABDX                 YES, DONE                                   
         SPACE                                                                  
         CLC   FTDATE,2(R2)        BUY FTD TO LTD IN TABLE                      
         BH    ABD20               FTD IS AFTER LTD IN TABLE                    
         SPACE                                                                  
         CLC   LTDATE,0(R2)                                                     
         BL    ABD30               BUY LTD IS BEFORE TABLE FTD                  
         SPACE                                                                  
* EXPAND DATE RANGE                                                             
         SPACE                                                                  
         CLC   FTDATE,0(R2)                                                     
         BNL   *+10                                                             
         MVC   0(2,R2),FTDATE                                                   
         SPACE                                                                  
         CLC   LTDATE,2(R2)                                                     
         BNH   ABDX                                                             
         MVC   2(2,R2),LTDATE                                                   
         B     ABDX                                                             
         SPACE                                                                  
ABD20    DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,FTDATE),(0,WORK)                                  
         GOTO1 ADDAY,(R1),WORK,WORK+6,F'-1'                                     
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,WORK+12)                               
         SPACE                                                                  
         CLC   WORK+12(2),2(R2)                                                 
         BNE   ABD50                                                            
         SPACE                                                                  
         MVC   2(2,R2),LTDATE      EXPAND DATE RANGE                            
         B     ABDX                                                             
         SPACE                                                                  
ABD30    DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,LTDATE),(0,WORK)                                  
         GOTO1 ADDAY,(R1),WORK,WORK+6,F'1'                                      
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,WORK+12)                               
         SPACE                                                                  
         CLC   WORK+12(2),0(R2)                                                 
         BNE   ABD50                                                            
         SPACE                                                                  
         MVC   0(2,R2),FTDATE                                                   
         B     ABD60                                                            
         SPACE                                                                  
ABD50    DS    0H                                                               
         CLC   FTDATE,0(R2)                                                     
         BH    ABD60                                                            
         SPACE                                                                  
         LA    R1,BDATELST         START OF LIST                                
         LA    R1,L'BDATELST(R1)   END OF LIST                                  
         AHI   R1,-4                                                            
         CLI   0(R1),0             ANY MORE ROOM?                               
         BE    *+6                                                              
         DC    H'00'               MAKE TABLES BIGGER                           
         SR    R1,R2               GET LEN TO MOVE                              
         LR    R3,R1               SAVE LENGTH                                  
         LA    RE,SVBUYDTE         USE AS TEMPORARY SPACE                       
         LR    R0,R2               ADDRESS TO COPY FROM                         
         LR    RF,R1               USE SAME LENGTH                              
         MVCL  RE,R0               COPY TABLE TEMPORARILY TO SVBUYDTE           
*                                                                               
         LA    R0,4(R2)            RECOPY TABLE BACK TO BDATELST                
         LA    RE,SVBUYDTE         COPY FROM TEMPORARY SPACE                    
         LR    R1,R3               RESTORE LENGTH                               
         LR    RF,R1               USE SAME LENGTH                              
         MVCL  R0,RE               SHIFTED 4 BYTES TO THE RIGHT                 
         SPACE                                                                  
         B     ABD70                                                            
         SPACE                                                                  
ABD60    LA    R2,4(R2)                                                         
         BCT   R4,ABD10                                                         
         DC    H'0'                MAKE TABLE BIGGER                            
         SPACE                                                                  
ABD70    MVC   0(4,R2),FTDATE      SAVE DATES IN TABLE                          
         SPACE                                                                  
ABDX     XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* SUBROUTINE USED ONLY FOR OPTION TO TREAT DAYPARTS AS COPY CODES    *          
* PATTERN RECORDS ARE READ TO DETERMINE IF ANY EXIST FOR THIS MARKET *          
* OR STATION. IF THERE ARE NONE, THE COPY CODE IS RESET, AND AT THE  *          
* END, ENTRIES WITH DUPLICATE COPY CODES ARE MERGED                  *          
**********************************************************************          
         DS    0H                                                               
CHKPTN   NMOD1 0,**CKPT**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
*                                                                               
         MVC   WORK+20(13),KEY                                                  
         MVC   WORK+33(13),KEYSAVE                                              
*                                                                               
         L     R4,AACLST           CLEAR ACTIVITY LIST AREA                     
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
         BAS   RE,GETEL1                                                        
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
         SPACE                                                                  
* NOW TEST IF THIS PATTERN APPLIES TO THIS MARKET OR STATION *                  
         SPACE                                                                  
         MVI   ELCODE,X'20'                                                     
         L     R6,AIO                                                           
         BAS   RE,GETEL1                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PATLSTEL,R6                                                      
*                                                                               
         CLI   2(R6),C'M'          TEST MARKET LIST                             
         BNE   CPTN12                                                           
         OC    PATLST,PATLST       TEST ALL MKT PATTERN                         
         BE    CPTN50              YES                                          
         SPACE                                                                  
* SET TO PROCESS MARKET OR STATION LIST *                                       
         SPACE                                                                  
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
         SPACE                                                                  
* MUST BE AFFILIATE OR STATION TYPE                                             
         SPACE                                                                  
* READ STATION MASTER RECORD FOR AFFL/TYPE *                                    
         SPACE                                                                  
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
         SPACE                                                                  
         CLC   1(3,R6),SNETWRK                                                  
         BE    CPTN50                                                           
         B     CPTN40                                                           
CPTN30   CLI   0(R6),C'T'          TYPE                                         
         BNE   CPTN40                                                           
         SPACE                                                                  
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
         SPACE                                                                  
* NOW ELIMINATE DUPLICATE ENTRIES FROM LIST *                                   
         SPACE                                                                  
         L     R4,AACLST           CLEAR ACTIVITY LIST AREA                     
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
         MVC   SVDATE,ACLFTD-ACLDATA(R5) MOVE FIRST TLCST DATE                  
         MVC   SVDATEX,ACLLTD-ACLDATA(R5) MOVE LAST TLCST DATE                  
*                                                                               
         BAS   RE,EXPDATE                                                       
         BNE   CPTN52                                                           
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
         SPACE                                                                  
* SORT THE TABLE TO ELIMINATE X'FF' ENTRIES *                                   
         SPACE                                                                  
CPTN56   SR    R0,R0               SET TO COUNT THE ENTRIES                     
*                                                                               
         L     R4,AACLST           CLEAR ACTIVITY LIST AREA                     
         CLI   0(R4),0                                                          
         BE    *+12                                                             
         LA    R4,L'ACLDATA(R4)                                                 
         BCT   R0,*-12                                                          
*                                                                               
         LPR   R0,R0                                                            
*                                                                               
         L     R4,AACLST           CLEAR ACTIVITY LIST AREA                     
         GOTO1 XSORT,DMCB,(R4),(R0),L'ACLDATA,6,0                               
*                                                                               
         L     R4,AACLST           CLEAR ACTIVITY LIST AREA                     
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
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
EXPDATE  NTR1                                                                   
         SPACE                                                                  
         CLC   SVDATE,PERSTRP     TEST PRIOR TO PERIOD START                    
         BH    *+10                                                             
         MVC   SVDATE,PERSTRP                                                   
         SPACE                                                                  
         CLC   SVDATEX,PERENDP    TEST AFTER PERIOD END                         
         BL    *+10                                                             
         MVC   SVDATEX,PERENDP                                                  
         SPACE                                                                  
         CLC   ACLFTD,SVDATEX      FTD IN TABLE > LTD OF ELEMENT                
         BH    XPD10                                                            
         SPACE                                                                  
         CLC   SVDATE,ACLLTD       FTD OF ELEMENT < LTD IN TABLE                
         BNH   XPD50                                                            
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(2,SVDATE),WORK                                      
         GOTO1 ADDAY,(R1),WORK,WORK+6,F'-1'                                     
         GOTO1 DATCON,DMCB,WORK+6,(2,WORK+12)                                   
         SPACE                                                                  
         CLC   WORK+12(2),ACLLTD   ANY GAP IN DATES                             
         BE    XPD50                                                            
         B     XPDNE                                                            
         SPACE                                                                  
XPD10    DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,SVDATEX),WORK                                     
         GOTO1 ADDAY,(R1),WORK,WORK+6,F'1'                                      
         GOTO1 DATCON,DMCB,WORK+6,(2,WORK+12)                                   
         SPACE                                                                  
         CLC   ACLFTD,WORK+12      ANY GAP IN DATES                             
         BNE   XPDNE                                                            
         SPACE                                                                  
XPD50    DS    0H                  EXPAND DATES                                 
         CLC   ACLFTD,SVDATE                                                    
         BNH   *+10                                                             
         MVC   ACLFTD,SVDATE                                                    
         CLC   ACLLTD,SVDATEX                                                   
         BNL   *+10                                                             
         MVC   ACLLTD,SVDATEX                                                   
         SPACE                                                                  
         CR    RB,RB                                                            
         B     *+6                                                              
XPDNE    CR    RB,RD                                                            
         XIT1                                                                   
         SPACE 2                                                                
GETEL1   AH    R6,DATADISP                                                      
*                                                                               
FIRSTEL1 CLI   0(R6),0                                                          
         BNE   *+10                                                             
         CLI   0(R6),1                                                          
         BR    RE                                                               
         CLI   ELCODE,0                                                         
         BCR   8,RE                                                             
         CLC   ELCODE,0(R6)                                                     
         BCR   8,RE                                                             
         SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,RF                                                            
         B     FIRSTEL1                                                         
         DROP  RB,RC                                                            
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
* HEADHOOK ROUTINE                                                              
         SPACE                                                                  
HDHK     NMOD1 0,**HDHK**                                                       
         L     RC,SVADGEND                                                      
         USING GEND,RC                                                          
         SPACE                                                                  
         TM    SVOPTSW,OPTCABLE    CABLE STATION ONLY                           
         BO    HDHK10                                                           
         CLI   CBLFLAG,C'Y'        PRINTING CABLE HEADINGS                      
         BE    HDHK10                                                           
         MVC   PMKT,QMKT                                                        
         SPACE                                                                  
HDHK10   DS    0H                                                               
         MVC   PMKTNM,MKTNM                                                     
         SPACE                                                                  
         MVC   H2+10(1),QMED                                                    
         MVC   H2+15(L'MEDNM),MEDNM                                             
         MVC   H3+10(L'QCLT),QCLT                                               
         MVC   H3+15(L'CLTNM),CLTNM                                             
         MVC   H3+37(8),=C'PERIOD ='                                            
         GOTO1 DATCON,DMCB,(3,PERSTART),(5,H3+46)                               
         MVI   H3+55,C'-'                                                       
         GOTO1 (RF),(R1),(3,PEREND),(5,H3+57)                                   
         SPACE                                                                  
         CLI   SVMGRPS,0           BY MARKET GROUP                              
         BE    HDHK14                                                           
         SPACE                                                                  
         MVC   H4+2(12),=C'MARKET GROUP'                                        
         MVC   H4+21(1),SVMGRPS                                                 
         SR    R0,R0                                                            
         ICM   R0,3,SVCMLMGR                                                    
         SLL   R0,4                                                             
         STCM  R0,7,DUB                                                         
         OI    DUB+2,X'0F'                                                      
         UNPK  H4+22(4),DUB(3)                                                  
         SPACE                                                                  
HDHK14   DS    0H                                                               
         LA    R6,H5                                                            
         TM    SVFLAG,ERREPRT          ERROR REPORT                             
         BO    HDHK60                                                           
*                                                                               
         MVC   TALCMID,SVCMLCML                                                 
         BRAS  RE,READCML          GET PIGGYBACKS                               
         LA    R6,H5                                                            
         OC    MYPIGGY,MYPIGGY                                                  
         BZ    HDHK15                                                           
         LA    R0,4                                                             
         LA    R5,MYPIGGY                                                       
HDHK14A  OC    0(L'TALCMID,R5),0(R5)                                            
         BZ    HDHK50                                                           
         BRAS  RE,FCML                                                          
         BRAS  RE,HDHKCML          COMMERCIAL HEADHOOKS                         
         LA    R5,L'TALCMID(R5)                                                 
         LR    R6,R1                                                            
         LA    R6,132(R6)                                                       
         BCT   R0,HDHK14A                                                       
         B     HDHK50                                                           
*                                                                               
HDHK15   LA    R5,SVCMLCML                                                      
         SPACE                                                                  
         BRAS  RE,FCML                                                          
         BRAS  RE,HDHKCML          COMMERCIAL HEADHOOKS                         
*                                                                               
         CLC   SVCMLCML+12(12),SPACES                                           
         BE    HDHK50                                                           
         OC    SVCMLCML+12(12),SVCMLCML+12                                      
         BZ    HDHK50                                                           
*                                                                               
         LA    R5,SVCMLCML+12                                                   
         BRAS  RE,FCML                                                          
         LR    R6,R1                                                            
         LA    R6,132(R6)                                                       
         BRAS  RE,HDHKCML                                                       
**       BRAS  RE,HDHKCML2                                                      
         SPACE                                                                  
HDHK50   CLC   SPACES,0(R6)        BLANK LINE                                   
         BE    HDHK60                                                           
         SPACE                                                                  
         LA    R6,132(,R6)                                                      
         B     HDHK50                                                           
         SPACE                                                                  
HDHK60   CLI   BPRD,0                                                           
         BE    HDHK70                                                           
         MVC   2(7,R6),=C'PRODUCT'                                              
         MVC   11(3,R6),QPRD                                                    
         MVC   16(L'PRDNM,R6),PRDNM                                             
         LA    R6,132(,R6)                                                      
         SPACE                                                                  
HDHK70   CLI   BPRD2,0                                                          
         BE    HDHK80                                                           
         MVC   2(7,R6),=C'PARTNER'                                              
         MVC   11(3,R6),QPRD2                                                   
         MVC   16(L'PRD2NM,R6),PRD2NM                                           
         LA    R6,132(,R6)                                                      
         SPACE                                                                  
HDHK80   CLI   OPTEST,0                                                         
         BE    HDHK90                                                           
         MVC   2(8,R6),=C'ESTIMATE'                                             
         ZIC   R0,OPTEST                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  11(3,R6),DUB                                                     
         LA    R6,132(,R6)                                                      
         SPACE                                                                  
HDHK90   MVC   MID1+13(6),=C'MARKET'                                            
         SPACE                                                                  
         TM    SVOPTSW,OPTCABLE    CABLE STATION ONLY                           
         BO    *+12                                                             
         CLI   CBLFLAG,C'Y'        PRINTING CABLE HEADINGS                      
         BNE   HDHK90A                                                          
         MVC   MID1+13(6),=C'CABLE '                                            
         SPACE                                                                  
HDHK90A  MVC   MID2+13(6),DASHES                                                
         SPACE                                                                  
         CLI   TRAMED,C'R'                                                      
         BNE   *+16                                                             
         MVC   MID1+23(5),=C'ALPHA'   ALPHA MARKET CODE                         
         MVC   MID2+23(5),DASHES                                                
         SPACE                                                                  
         CLI   TRAMED,C'T'                                                      
         BNE   *+16                                                             
         MVC   MID1+23(3),=C'NSI'     NSI MARKET CODE                           
         MVC   MID2+23(4),DASHES                                                
         SPACE                                                                  
         TM    SVOPTSW,OPTCABLE       CABLE STATION ONLY - SKIP                 
         BO    HDHK91                                                           
         CLI   CBLFLAG,C'Y'           PRINTING CABLE HEADINGS                   
         BE    HDHK91                                                           
         MVC   MID1+30(3),=C'TAL'     TALENT MARKET                             
         MVC   MID2+30(4),DASHES                                                
         SPACE                                                                  
HDHK91   MVC   MID1+37(11),=C'MARKET-NAME'                                      
         MVC   MID2+37(24),DASHES                                               
         SPACE                                                                  
         LA    R2,PMKTUNTS-P+MID1                                               
         SPACE                                                                  
         TM    SVOPTSW,OPTUNIT                                                  
         BZ    HDHK92                                                           
         SPACE                                                                  
         MVC   1(5,R2),=C'UNITS'                                                
         MVC   132+1(5,R2),DASHES                                               
         LA    R2,10(,R2)                                                       
         SPACE                                                                  
HDHK92   DS    0H                                                               
         CLI   CBLFLAG,C'Y'           PRINTING CABLE HEADINGS                   
         BE    HDHK93                                                           
         TM    SVOPTSW,OPTCABLE                                                 
         BZ    HDHK94                                                           
         SPACE                                                                  
HDHK93   MVC   0(17,R2),=C'CABLE SYSTEM NAME'                                   
         MVC   132(17,R2),DASHES                                                
         LA    R2,30(,R2)                                                       
         SPACE                                                                  
HDHK94   MVC   0(12,R2),=C'ACTIVE DATES'                                        
         MVC   132(14,R2),DASHES                                                
         SPACE                                                                  
         TM    SVFLAG,ERREPRT          ERROR REPORT                             
         BO    HDHKX                                                            
         SPACE                                                                  
         TM    SVOPTSW,OPTESTP                                                  
         BZ    HDHKX                                                            
         SPACE                                                                  
         MVC   PESTS-PACTDTES(19,R2),=C'ESTIMATE(S) COVERED'                    
         MVC   132+PESTS-PACTDTES(19,R2),DASHES                                 
         SPACE                                                                  
HDHKX    XIT1                                                                   
DASHES   DC    24C'-'                                                           
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        IF SYSTEM IS ADV4 RETURN CC EQ                                         
*        AGENCY SJR CANNOT HAVE TEST RUN = N ON ADV                             
*                                                                               
CKSYS    NTR1  BASE=*,LABEL=*                                                   
         CLI   OFFLINE,C'Y'        ONLINE ONLY                                  
         JE    NO                                                               
         USING FACTSD,RF                                                        
         GOTO1 GETFACT,DMCB,0      GET DSPACE ONLINE                            
         L     RF,DMCB                                                          
         CLI   FASYSID,1           IF DSPACE IS TEST                            
         JE    NO                  ALLOW TEST RUN = N                           
         CLI   FASYSID,15          OR IF DSPACE IS FQA,                         
         JE    NO                  ALL TEST RUN = N                             
         CLI   FASYSID,11          OR IF DSPACE IS CSC,                         
         JE    NO                  ALLOW TEST RUN = N                           
         J     YES                 OTHERWISE DO NOT ALLOW                       
         DROP  RF                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* PRINT ESTIMATE TABLE FROM BLOCK                                               
         SPACE                                                                  
PEST     NTR1  BASE=*,LABEL=*                                                   
         LA    R3,BLOCK                                                         
         LR    R1,R3                                                            
         SR    R0,R0                                                            
PEST10   CLI   0(R1),0                                                          
         BE    PEST14                                                           
         LA    R1,3(,R1)                                                        
         BCT   R0,PEST10                                                        
         DC    H'0'                                                             
PEST14   LPR   R4,R0                                                            
         BZ    PEST16                                                           
         GOTO1 XSORT,DMCB,BLOCK,(R4),3,1,0                                      
         SPACE                                                                  
PEST16   TM    SVOPTSW,OPTDOWN     DOWNLOADING                                  
         BO    PESTX                YES                                         
         LTR   R4,R4                                                            
         BZ    PEST30                                                           
         SPACE                                                                  
         TM    SVOPTSW,OPTESTP     PRINT ESTIMATE                               
         BZ    PEST30                                                           
         SPACE                                                                  
         LA    R5,PESTS-PACTDTES+L'PESTS(,R2)                                   
PEST20   LA    R4,PESTS-PACTDTES(,R2)                                           
         SPACE                                                                  
PEST24   OC    3(3,R3),3(R3)       MULTIPLE ESTIMATES                           
         BZ    PEST24C              NO                                          
         SPACE                                                                  
         CLI   2(R3),X'FF'         BUYS FOUND FOR THIS ESTIMATE                 
         BE    PEST24C              YES                                         
         SPACE                                                                  
         LA    R3,3(R3)                                                         
         CLI   0(R3),0                                                          
         BE    PEST30                                                           
         B     PEST24                                                           
         SPACE                                                                  
PEST24C  EDIT  (B1,(R3)),(3,0(R4)),ALIGN=LEFT                                   
         SPACE                                                                  
         TM    SVOPTSW,OPTENAME    PRINT ESTIMATE NAME                          
         BZ    PEST25               NO                                          
         SPACE                                                                  
         BRAS  RE,GENAME           GET ESTIMATE NAME                            
         SPACE                                                                  
PEST25   LA    R3,3(,R3)                                                        
         SPACE                                                                  
         CLI   0(R3),0                                                          
         BE    PEST30                                                           
         SPACE                                                                  
         TM    SVOPTSW,OPTENAME    PRINT ESTIMATE NAME                          
         BO    PEST27               NO                                          
         SPACE                                                                  
         AR    R4,R0                                                            
         MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
         CR    R5,R4                                                            
         BH    PEST24                                                           
PEST27   GOTO1 SPOOL,DMCB,(R8)                                                  
         B     PEST20                                                           
         SPACE                                                                  
PEST30   DS    0H                                                               
         CLI   0(R4),C','                                                       
         BE    PEST32                                                           
         SPACE                                                                  
         CLI   0(R4),C' '                                                       
         BH    PEST35                                                           
         BCTR  R4,0                                                             
         CLI   0(R4),C','                                                       
         BNE   PEST35                                                           
PEST32   MVI   0(R4),C' '                                                       
         SPACE                                                                  
PEST35   GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE                                                                  
PESTX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* SAVE ESTIMATE/PROD/FLAG IN BLOCK *                                            
         SPACE                                                                  
SEST     NTR1  BASE=*,LABEL=*                                                   
         CLI   CMLEST,0                                                         
         BE    SESTX                                                            
         SPACE                                                                  
         LA    R0,85                                                            
         LA    R1,BLOCK                                                         
SEST10   CLI   0(R1),0                                                          
         BE    SEST20                                                           
         CLC   CMLEST,0(R1)        SAME ESTIMATE                                
         BNE   SEST15                                                           
         CLC   CMLPRD,1(R1)        SAME PROD                                    
         BE    SESTX                                                            
SEST15   LA    R1,3(,R1)           BUMP IN BLOCK                                
         BCT   R0,SEST10                                                        
         DC    H'0'                                                             
         SPACE                                                                  
SEST20   MVC   0(1,R1),CMLEST                                                   
         MVC   1(1,R1),CMLPRD                                                   
         SPACE                                                                  
SESTX    J     EXIT                                                             
*=====================================================================          
*              VALIDATE USING SOFT DATES                                        
*              R2 = FIELD HEADER                                                
*=====================================================================          
         USING BLOCKD,R4                                                        
VSFTDAT  NTR1  BASE=*,LABEL=*                                                   
         LA    R4,BLOCK                                                         
         XC    BLOCK(256),BLOCK                                                 
         XC    BLOCK+256(256),BLOCK+256                                         
         L     R1,SYSPARMS                                                      
         L     R1,16(,R1)          COMFACS                                      
         USING COMFACSD,R1                                                      
         MVC   SOFTDATE,CSOFDAT                                                 
         DROP  R1                                                               
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(0,TODAY0)  TODAY'S DATE                       
*                                                                               
         USING SOFDATD,R1                                                       
         LA    R1,SDBLOCK                                                       
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         ST    R2,SOFAINP          A(INPUT)                                     
         LA    R3,OUTDATE                                                       
         ST    R3,SOFAOUT          A(OUTPUT)                                    
         MVC   SOFACOM,ACOMFACS    A(COMFACS)                                   
         MVI   SOFITYPE,SOFITYMD   VALIDATE FOR YEAR, MONTH, DAY                
         MVI   SOFOTYPE,SOFOTSD2   12 BYTE EBCIDIC (YYMMDDYYMMDD)               
         MVI   SOFIINDS,SOFIISFT   VALIDATE ONLY SOFT DATES                     
*                                                                               
         MVC   SOFTODAY,TODAY0     TODAY'S DATE                                 
         MVC   SOFCTRY,CTRY        COUNTRY CODE                                 
         MVC   SOFLANG,LANG        LANGUAGE CODE                                
         MVI   SOFSYSN,13          SPOT TRAFFIC SYSTEM                          
         GOTO1 SOFTDATE,SOFDATD                                                 
         BZ    VSFTDAT3                                                         
* CHECK FOR YEAR, MONTH VALIDATION                                              
         MVI   SOFITYPE,SOFITYM    VALIDATE FOR YEAR, MONTH THEN                
         GOTO1 SOFTDATE,SOFDATD                                                 
         BZ    VSFTDAT3                                                         
         MVC   ERROR,SOFERROR                                                   
         B     VSFTNO                                                           
*                                                                               
VSFTDAT3 CLI   OFFLINE,C'Y'                                                     
         BE    VSFTDAT5                                                         
         CLI   CONOUTH+4,0         RLP ONLINE, IF GROUP NAME IN OUTPUT          
         BE    VSFTDAT5                                                         
         CLC   =C'FILE',CONDEST    AND FILE IN DESTINATION                      
         BE    VSFTYES             DON'T RESOLVE DATES                          
*                                                                               
VSFTDAT5 OI    SOFIINDS,SOFIIRES   RESOLVE THE DATES TO ACTUAL                  
         GOTO1 SOFTDATE,SOFDATD                                                 
         GOTO1 DATCON,DMCB,(0,OUTDATE),(3,PERSTART)                             
         GOTO1 DATCON,DMCB,(0,OUTDATE+6),(3,PEREND)                             
         B     VSFTYES                                                          
*                                                                               
VSFTYES  SR    RC,RC                                                            
VSFTNO   LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         DROP  R1,R4                                                            
         EJECT                                                                  
* FIND CABLE STATION NAME AND SAVE CABLE STATION, INDEX, AND NAME               
         SPACE                                                                  
CBLFND   NTR1 BASE=*,LABEL=*                                                    
         SPACE                                                                  
         MVC   SAVEKEY,KEY         SAVE KEY OF LAST INST. RECAP RECORD          
         USING INSKEY,R4                                                        
         L     R2,CBLSTB                                                        
         USING CBLSTAD,R2                                                       
CBLFND10 DS   0H                                                                
         OC    CBLSTA,CBLSTA                                                    
         BZ    CBLFND30                                                         
         CLC   CBLSTA,INSKSTA      THIS THE SAME STATION                        
         BE    CBLFND20                                                         
         LA    R2,CBLNEXT                                                       
         C     R2,CBLSTBX                                                       
         BL    CBLFND10                                                         
         DC    H'0'                                                             
         SPACE                                                                  
CBLFND20 DS   0H                                                                
         MVC   SVCBLIND,CBLINDEX                                                
         B     CBLFNDX                                                          
         SPACE                                                                  
CBLFND30 DS   0H                                                                
         MVC   CBLSTA,INSKSTA      SAVE STATION                                 
         GOTO1 MSUNPK,DMCB,(X'80',INSKMKT-INSKEY+SVKEY),WORK,WORK+4             
         SPACE                                                                  
         LA    R1,BIGKEY                                                        
         USING STAPACKD,R1                                                      
         MVC   FULL(3),STAPQNET    SAVE CABLE NET (IF ANY)                      
         DROP  R1                                                               
         SPACE                                                                  
         LA    R6,KEY                                                           
         USING STARECD,R6                                                       
         XC    KEY,KEY                                                          
         MVI   STAKTYPE,STAKTYPQ                                                
         MVC   STAKMED,QMED                                                     
         MVC   STAKCALL,WORK+4                                                  
         MVI   STAKCALL+4,C'T'                                                  
         MVC   STAKAGY,AGENCY                                                   
*        MVC   STAKCLT,QCLT                                                     
         MVC   STAKCLT,=C'000'     AMS CDS NOT IN CLT SPECIFIC REC              
         MVC   STAKFILL,=C'000'                                                 
         L     R6,AIO2                                                          
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,(R6)                     
         SPACE                                                                  
         CLC   KEY(9),0(R6)                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         LA    R0,1                                                             
         L     R3,CBLNAM                                                        
         SPACE                                                                  
CBLFND50 DS   0H                                                                
         OC    0(24,R3),0(R3)                                                   
         BZ    CBLFND60                                                         
         CLC   SSYSNAME,0(R3)      THIS THE CABLE SYSTEM NAME                   
         BE    CBLFND60                                                         
         LA    R3,CBLNAML(,R3)                                                  
         AHI   R0,1                                                             
         C     R3,CBLNAMX                                                       
         BL    CBLFND50                                                         
         DC    H'0'                                                             
         SPACE                                                                  
CBLFND60 DS   0H                                                                
         MVC   0(24,R3),SSYSNAME   SAVE THE CABLE SYSTEM NAME                   
         STH   R0,SVCBLIND                                                      
         STCM  R0,3,CBLINDEX                                                    
         SPACE                                                                  
         MVC   KEY,SAVEKEY         RESTORE LAST INST. RECAP RECORD              
CBLFNDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* NEW CLIENT ROUTINE                                                            
*                                                                               
NEWCLI   NTR1  BASE=*,LABEL=*                                                   
         XC    FLDH,FLDH                                                        
         XC    FLD,FLD                                                          
         LA    R2,FLDH             FAKE FIELD HEADER                            
         MVI   5(R2),3             DATA LENGTH                                  
         XC    BCLT,BCLT                                                        
         GOTO1 CLUNPK,DMCB,KEY+3,8(R2)  UNPACK BINARY CLIENT                    
         MVI   ERROPT,C'Y'         RETURN ON ERROR                              
         GOTO1 VALICLT                                                          
         MVC   SVBCLT,BCLT                                                      
*                                                                               
         MVI   ERROPT,0                                                         
         CLI   ERROR,0                                                          
         BE    NCLI10                                                           
         CLI   ERROR,SECLOCK       ONLY VALID ERR IS SECURITY LOCK-OUT          
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY OF INST RECORD                   
         GOTO1 HIGH                                                             
         B     NCLINE                                                           
*                                                                               
NCLI10   CLI   TLCMFLG,C'Y'        IF RUNNING TALCOM LIST, SKIP                 
         BE    *+8                                                              
         BRAS  RE,PRDLIST          GET LIST OF VALID PRODUCTS                   
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
         MVI   WORK+3,C'2'                                                      
         GOTO1 (RF),(R1),,SVT1PROF                                              
*                                                                               
         MVC   SVT2PR05,SVT1PROF+4  COMBINE CABLE NETWORKS                      
*                                                                               
         XC    MYDATE,MYDATE                                                    
         XC    MYDATE2,MYDATE2                                                  
         CLI   TLCMFLG,C'Y'         IF RUNNING TALCOM,                          
         BE    NCLI30               SKIP TT PROFILE                             
         MVI   WORK+3,C'T'          GET TT PROFILE                              
         GOTO1 (RF),(R1),,SVT1PROF                                              
         CLI   SVT1PROF+3,0         BAD PROFILE?                                
         BE    *+8                                                              
         BRAS  RE,PROFTT                                                        
         CLI   SVNUSER,C'N'         SPOTTAL USER?                               
         BNE   NCLI20                                                           
         MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY OF INST RECORD                   
         GOTO1 HIGH                                                             
         B     NCLINE                                                           
*                                                                               
NCLI20   MVI   WORK+3,C'1'                                                      
         GOTO1 GETPROF,DMCB,WORK,SVT1PROF,DATAMGR                               
*                                                                               
NCLI30   MVC   KEY(L'SVKEY),SVKEY  RESTORE KEY OF INST RECORD                   
         GOTO1 HIGH                                                             
         CR    RB,RB                                                            
         B     NCLIX                                                            
*                                                                               
NCLINE   LTR   RB,RB                                                            
NCLIX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* GET ESTIMATE NAME *                                                           
* (R3 POINTS TO BLOCK WHICH IS A LIST OF EST/BPRD/FLAG)                         
         SPACE                                                                  
GENAME   NTR1  BASE=*,LABEL=*                                                   
         SPACE                                                                  
         L     R5,AIO3             ESTIMATE NAME LIST                           
         LA    R1,100              100 ESTIMATE ENTRIES                         
         SPACE                                                                  
GEST10   OC    0(25,R5),0(R5)      ANY ENTRY                                    
         BZ    GEST40               NO                                          
         SPACE                                                                  
         CLC   0(2,R3),3(R5)       SAME ESTIMATE/BPRD                           
         BNE   GEST30                                                           
         SPACE                                                                  
         MVC   4(17,R4),5(R5)                                                   
         TM    SVOPTSW,OPTCABLE+OPTUNIT                                         
         BO    *+10                                                             
         MVC   4(20,R4),5(R5)      MOVE EST NAME TO PRINT LINE                  
         B     GESTX                                                            
         SPACE                                                                  
GEST30   LA    R5,25(R5)           BUMP IN AIO3                                 
         BCT   R0,GEST10                                                        
         SPACE                                                                  
*  GET PRODUCT CODE                                                             
         SPACE                                                                  
GEST40   DS    0H                                                               
         LA    RE,220                                                           
         L     RF,ASVCLIST         TABLE OF CLIENT PROD CODES                   
GEST44   CLC   1(1,R3),3(RF)       THIS A VALID PROD CODE                       
         BE    GEST45                                                           
         LA    RF,4(,RF)           BUMP PROD PTR                                
         CLI   0(RF),C' '          AT END OF TABLE?                             
         BNH   *+8                 YES, DEATH                                   
         BCT   RE,GEST44                                                        
         DC    H'0'                                                             
         SPACE                                                                  
GEST45   MVC   SVPROD,0(RF)                                                     
         SPACE                                                                  
* READ ESTIMATE RECORD FOR ESTIMATE NAME                                        
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+4(3),SVPROD     PROD                                         
         MVC   KEY+7(1),0(R3)      ESTIMATE                                     
         SPACE                                                                  
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA                       
         GOTO1 HIGH                                                             
         SPACE                                                                  
         CLC   KEY(7),KEYSAVE      SAME A-M/CLT/PRD                             
         BNE   GEST50                                                           
         SPACE                                                                  
         OC    KEY+8(5),KEY+8                                                   
         BNZ   GEST50                                                           
         SPACE                                                                  
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVC   FILENAME,=CL8'SPTFIL' SWITCH TO SPOT MEDIA                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         SPACE                                                                  
         USING ESTHDRD,R6                                                       
         SPACE                                                                  
         MVC   4(17,R4),EDESC                                                   
         TM    SVOPTSW,OPTCABLE+OPTUNIT                                         
         BO    *+10                                                             
         MVC   4(L'EDESC,R4),EDESC MOVE EST NAME TO PRINT LINE                  
         B     GESTX                                                            
         SPACE                                                                  
GEST50   MVC   4(6,R4),=C'??????'  EST RECORD NOT FOUND                         
         SPACE                                                                  
GESTX    DS    0H                                                               
         XC    FILENAME,FILENAME                                                
         XIT1                                                                   
         LTORG                                                                  
         SPACE                                                                  
         EJECT                                                                  
*                                                                               
*        FILTER TSAR RECORD                                                     
*                                                                               
         USING TSRBUYD,R5                                                       
FILTTSR  NTR1  BASE=*,LABEL=*                                                   
         CLC   TSRCLI,CMLCLI-CMLENT+HOLDALL  SAME CLIENT                        
         JNE   NO                                                               
*                                                                               
         CLC   TSRMKT,CMLMKT-CMLENT+HOLDALL  SAME MARKET                        
         JNE   NO                                                               
*                                                                               
         TM    SVOPTSW,OPTCABLE                                                 
         BO    *+12                                                             
         CLI   CMLCBL-CMLENT+HOLDALL,C'C'    IF CABLE,                          
         BNE   FTSR20                                                           
         CLC   TSRCSTA,CMLCSTA-CMLENT+HOLDALL  SAME CABLE STA                   
         BE    FTSR30                                                           
         CLI   CMLCSTA-CMLENT+HOLDALL+2,0   COMBINED?                           
         BE    FTSR10                                                           
         CLI   CMLCSTA-CMLENT+HOLDALL+2,CABLENET                                
         JNE   NO                                                               
FTSR10   CLC   TSRCSTA(2),CMLCSTA-CMLENT+HOLDALL                                
         JNE   NO                                                               
         B     FTSR30                                                           
*                                                                               
FTSR20   CLI   CMLCBL-CMLENT+HOLDALL,C'C'    IF NOT CABLE,                      
         BE    FTSR30                                                           
         CLI   TSRCSTA,CABLESTA   SKIP IF STATION IS CABLE                      
         JH    NO                                                               
         CLC   TSRCSTA,CMLCSTA-CMLENT+HOLDALL  SAME STATION                     
         JNE   NO                                                               
*                                                                               
FTSR30   CLC   TSRPRD(4),CMLPRD-CMLENT+HOLDALL PRD/LEN/PRD2/LEN2                
         JNE   NO                                                               
*                                                                               
         CLC   TSRLTD,SVRFTD       SEE THAT BUY DATES ARE                       
         JL    NO                  NOT OUTSIDE OF RECAP DATES                   
         CLC   TSRFTD,SVRLTD                                                    
         JH    NO                                                               
*                                                                               
         LA    R1,BLOCK                                                         
         LA    R0,85                                                            
*                                                                               
FTSR40   OC    0(3,R1),0(R1)       ANY MORE EST/PRD                             
         JZ    NO                   NO, NO MATCH                                
         CLC   TSREST,0(R1)        SAME ESTIMATE                                
         BNE   FTSR50                                                           
         MVI   2(R1),X'FF'         BUY FOR THIS EST IS FOUND                    
         J     YES                                                              
FTSR50   LA    R1,3(R1)            BUMP IN BLOCK TO NEXT EST/PRD                
         BCT   R0,FTSR40                                                        
         SPACE                                                                  
         J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* SET UP HEADHOOKS FOR COMMERCIALS                                              
*                                                                               
HDHKCML  NTR1  BASE=*,LABEL=*                                                   
         CLC   AGENCY,=C'H9'       THIS STARCOM?                                
         BNE   HDHK16               NO                                          
         SPACE                                                                  
         OC    SVBASIC,SVBASIC                                                  
         BZ    HDHK16                                                           
         MVC   H4+2(14),=C'BASIC ISCI/LEN'                                      
         MVC   H4+21(16),SVBASIC                                                
         SPACE                                                                  
HDHK16   DS    0H                                                               
         MVC   2(17,R6),=C'COMMERCIAL ID/LEN'                                   
         MVC   132+2(16,R6),=C'COMMERCIAL TITLE'                                
         LA    R1,132(,R6)                                                      
*                                                                               
         MVC   21(12,R6),0(R5)                                                  
         OC    SVCMML,SVCMML    OLD ADID?                                       
         BZ    HDHK17                                                           
         MVC   21(12,R6),SVCMML                                                 
HDHK17   ZIC   R0,SVCMLSLN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  34(3,R6),DUB                                                     
         CLI   34(R6),C'0'                                                      
         BNE   *+8                                                              
         MVI   34(R6),C' '                                                      
*                                                                               
*&&DO                                                                           
         OC    SVADID,SVADID                                                    
         BZ    HDHK18                                                           
         MVC   38(7,R6),=C'AD-ID: '                                             
         MVC   45(12,R6),SVADID                                                 
*&&                                                                             
*                                                                               
HDHK18   CLC   AGENCY,=C'H9'       THIS STARCOM?                                
         BNE   HDHK20               NO                                          
         SPACE                                                                  
         OC    SVCMLCLT,SVCMLCLT                                                
         BZ    HDHK20                                                           
         SPACE                                                                  
         MVC   21+1+L'SVCMLCLT(3,R6),30(R6) MOVE CML LEN                        
         MVC   21(L'SVCMLCLT,R6),SVCMLCLT                                       
         SPACE                                                                  
HDHK20   DS   0H                                                                
         LA    R6,153(R6)                                                       
         MVC   0(L'SVCMLDSC,R6),SVCMLDSC                                        
                                                                                
         SPACE                                                                  
         OC    SVCMLDS2,SVCMLDS2                                                
         BZ    *+14                                                             
         LA    R6,21(R6)                                                        
         MVC   0(L'SVCMLDS2,R6),SVCMLDS2                                        
         SPACE                                                                  
         OC    SVCMLDS3,SVCMLDS3                                                
         BZ    *+14                                                             
         LA    R6,21(R6)                                                        
         MVC   0(L'SVCMLDS3,R6),SVCMLDS3                                        
         SPACE                                                                  
         CLC   AGENCY,=C'H9'       THIS STARCOM?                                
         BE    HDHK24               YES, ALREADY DONE                           
         SPACE                                                                  
         OC    SVCMLCLT,SVCMLCLT                                                
         BZ    HDHK24                                                           
         MVC   21(L'SVCMLCLT,R6),SVCMLCLT                                       
         SPACE                                                                  
HDHK24   DS   0H                                                                
*                                                                               
         XIT1  REGS=(R1)                                                        
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
*                                                                               
* SET UP HEADHOOKS FOR COMMERCIAL 2                                             
*                                                                               
HDHKCML2 NTR1  BASE=*,LABEL=*                                                   
         LA    R1,132(,R6)                                                      
         MVC   21(8,R6),0(R5)                                                   
         ZIC   R0,SVCMLSLN                                                      
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  30(3,R6),DUB                                                     
         CLI   30(R6),C'0'                                                      
         BNE   *+8                                                              
         MVI   30(R6),C' '                                                      
HDHK30   DS    0H                                                               
         SPACE                                                                  
         CLC   AGENCY,=C'H9'       THIS STARCOM?                                
         BNE   HDHK40               NO                                          
         SPACE                                                                  
         OC    SVCMLCLT,SVCMLCLT                                                
         BZ    HDHK40                                                           
         SPACE                                                                  
         MVC   21+1+L'SVCMLCLT(3,R6),30(R6)                                     
         MVC   21(L'SVCMLCLT,R6),SVCMLCLT                                       
         SPACE                                                                  
HDHK40   DS   0H                                                                
         LA    R6,153(R6)                                                       
         MVC   0(L'SVCMLDSC,R6),SVCMLDSC                                        
         SPACE                                                                  
         OC    SVCMLDS2,SVCMLDS2                                                
         BZ    *+14                                                             
         LA    R6,21(R6)                                                        
         MVC   0(L'SVCMLDS2,R6),SVCMLDS2                                        
         SPACE                                                                  
         OC    SVCMLDS3,SVCMLDS3                                                
         BZ    *+14                                                             
         LA    R6,21(R6)                                                        
         MVC   0(L'SVCMLDS3,R6),SVCMLDS3                                        
         SPACE                                                                  
         CLC   AGENCY,=C'H9'       THIS STARCOM?                                
         BE    HDHK49               YES, ALRADY DONE                            
         SPACE                                                                  
         CLI   SVT1PR7,C'Y'        SHOW CLIENT COMMERCIAL NUMBER                
         BNE   HDHK49               NO                                          
         SPACE                                                                  
         OC    SVCMLCLT,SVCMLCLT                                                
         BZ    HDHK49                                                           
         MVC   21(L'SVCMLCLT,R6),SVCMLCLT                                       
         SPACE                                                                  
HDHK49   DS   0H                                                                
         XIT1  REGS=(R1)                                                        
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
*                                                                               
*        DOWNLOADS - ADD BLANK FIELDS IF NO PIGGYBACK COMMERCIALS               
*                                                                               
DOWNCOM2 NTR1  BASE=*,LABEL=*                                                   
         MVI   0(R2),C'"'                                                       
         OC    1(9,R2),SPACES                                                   
         MVI   2(R2),C'"'          COMML CODE                                   
         LA    R2,4(,R2)                                                        
         SPACE                                                                  
         MVI   0(R2),C'"'          COMML LENGTH                                 
         MVI   2(R2),C'"'                                                       
         LA    R2,4(,R2)                                                        
         SPACE                                                                  
         MVI   0(R2),C'"'          COMML DESC                                   
         MVI   2(R2),C'"'                                                       
         LA    R2,4(,R2)                                                        
         XIT1  REGS=(R2)                                                        
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        SET SCREEN FIELDS DEPENDING ON IF RUNNING TALCOM OR STRANS             
*                                                                               
SETSCRN  NTR1  BASE=*,LABEL=*                                                   
         MVI   TLCMFLG,0                                                        
         CLC   CONREC(6),=CL6'TALCOM'  IF RUNNING A TALCOM LIST,                
         BNE   SSCRN10                                                          
         MVI   TLCMFLG,C'Y'            SET TALCOM FLAG                          
         OI    TRATAL1H+1,X'0C'    HIDE TALENT AGENCY FIELD                     
         OI    TRATAL1H+6,X'80'                                                 
         OI    TRATST1H+1,X'0C'    AND TEST RUN FIELD                           
         OI    TRATST1H+6,X'80'                                                 
         OI    TRATALH+1,X'20'     AND PROTECT INPUT FIELDS                     
         OI    TRATALH+6,X'80'                                                  
         OI    TRATSTH+1,X'20'                                                  
         OI    TRATSTH+6,X'80'                                                  
*                                                                               
         NI    TRAPTR1H+1,X'FF'-X'0C'    SHOW PARTNER FIELD                     
         OI    TRAPTR1H+6,X'80'                                                 
         NI    TRAMKT1H+1,X'FF'-X'0C'    AND MARKET FIELD                       
         OI    TRAMKT1H+6,X'80'                                                 
         NI    TRAPTRH+1,X'FF'-X'20'     AND UNPROTECT INPUT FIELDS             
         OI    TRAPTRH+6,X'80'                                                  
         NI    TRAMKTH+1,X'FF'-X'20'                                            
         OI    TRAMKTH+6,X'80'                                                  
*                                                                               
         XC    TRATAL,TRATAL             CLEAR TALENT AGENCY FIELD              
         OI    TRATALH+6,X'80'                                                  
         MVI   TRATALH+5,0                                                      
         XC    TRATST,TRATST             AND TEST RUN FIELD                     
         OI    TRATSTH+6,X'80'                                                  
         MVI   TRATSTH+5,0                                                      
         B     SSCRNX                                                           
*                           STRANS REPORT                                       
*                                                                               
SSCRN10  NI    TRATAL1H+1,X'FF'-X'0C'    SHOW TALENT AGENCY FIELD               
         OI    TRATAL1H+6,X'80'                                                 
         NI    TRATST1H+1,X'FF'-X'0C'    AND TEST RUN FIELD                     
         OI    TRATST1H+6,X'80'                                                 
         NI    TRATALH+1,X'FF'-X'20'     AND UNPROTECT INPUT FIELDS             
         OI    TRATALH+6,X'80'                                                  
         NI    TRATSTH+1,X'FF'-X'20'                                            
         OI    TRATSTH+6,X'80'                                                  
*                                                                               
         OI    TRAPTR1H+1,X'0C'    HIDE PARTNER FIELD                           
         OI    TRAPTR1H+6,X'80'                                                 
         OI    TRAMKT1H+1,X'0C'    AND MARKET FIELD                             
         OI    TRAMKT1H+6,X'80'                                                 
         OI    TRAPTRH+1,X'20'     AND PROTECT INPUT FIELDS                     
         OI    TRAPTRH+6,X'80'                                                  
         OI    TRAMKTH+1,X'20'                                                  
         OI    TRAMKTH+6,X'80'                                                  
*                                                                               
         XC    TRAPTR,TRAPTR       CLEAR PARTNER FIELD                          
         OI    TRAPTRH+6,X'80'                                                  
         MVI   TRAPTRH+5,0                                                      
         MVI   TRAPTR1H+5,0                                                     
         XC    TRAMKT,TRAMKT       AND MARKET FIELD                             
         OI    TRAMKTH+6,X'80'                                                  
         MVI   TRAMKTH+5,0                                                      
         MVI   TRAMKT1H+5,0                                                     
SSCRNX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        READ TT PROFILE                                                        
*                                                                               
PROFTT   NTR1  BASE=*,LABEL=*                                                   
         MVC   SVNUSER,SVT1PROF+3      SPOTTAL USER Y/N                         
*                                                                               
         MVC   BLOCK(2),SVT1PROF+1     YR/MONTH ABSOLUTE START                  
         CLI   BLOCK,0                                                          
         BE    PRFTT10                                                          
         MVI   BLOCK+2,1                FUDGE DAY                               
         GOTO1 DATCON,DMCB,(3,BLOCK),(0,BLOCK)  YYMMDD ABSOLUTE START           
*                                                                               
PRFTT10  LA    R2,TRAPERH          PERIOD                                       
         GOTO1 DATVAL,DMCB,8(R2),MYDATE VALIDATE START DATE                     
         CLC   MYDATE,=C'000000'   IF INVALID, MUST BE RLP SOFT DATE            
         BNE   PRFTT20                                                          
         GOTO1 DATCON,DMCB,(0,BLOCK),(3,MYDATE2)   SAVE ABS START DATE          
         XC    MYDATE,MYDATE                                                    
         B     PRFTTX                                                           
*                                                                               
***      ZIC   R1,SVT1PROF         GET NUMBER OF DAYS BACK 0-255                
***      MH    R1,=H'-1'                                                        
***      STCM  R1,15,DMCB+8                                                     
***      GOTO1 ADDAY,DMCB,MYDATE,MYDATE                                         
*                                                                               
* - CHK REQSTART NOT BEFORE TT PROFILE ABSOLUTE START                           
PRFTT20  CLC   MYDATE,BLOCK  IF REQ START DATE BEFORE ABSOLUTE START            
         BNL   *+10                                                             
         MVC   MYDATE,BLOCK   USE ABSOLUTE START                                
         GOTO1 DATCON,DMCB,(0,MYDATE),(3,MYDATEB)                               
PRFTTX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* BUILD ESTIMATE NAME TABLE IN AIO3 *                                           
* (3 BYTE PROD, 1 BYTE ESTIMATE, 1 BYTE BPRD 20 BYTES ESTIMATE NAME) *          
         SPACE                                                                  
BESTNAME NTR1  BASE=*,LABEL=*                                                   
         SPACE                                                                  
         MVC   SVKEY,KEY                                                        
         SPACE                                                                  
         LA    R0,100              100 ESTIMATE ENTRIES                         
         SPACE                                                                  
         LHI   R2,ESTLIST-SYSD                                                  
         AR    R2,R9               LIST OF ESTIMATES                            
***      LA    R2,ESTLIST          LIST OF ESTIMATES                            
BEST05   CLI   0(R2),0             ANY MORE TO PROCESS                          
         BE    BESTX                NO DONE                                     
         SPACE                                                                  
         L     R5,AIO3             ESTIMATE NAME LIST                           
         LA    R1,100              100 ESTIMATE ENTRIES                         
         SPACE                                                                  
BEST10   OC    0(25,R5),0(R5)      ANY ENTRY                                    
         BZ    BEST30                                                           
         CLC   0(5,R2),0(R5)       SAME PROD/EST/BPRD                           
         BE    BEST20              GET NEXT PROD/EST TO PROCESS                 
         SPACE                                                                  
         LA    R5,25(R5)           BUMP TO NEXT PROD/EST/NAME                   
         BCT   R1,BEST10                                                        
         B     BESTX               NO MORE ROOM IN TABLE                        
         SPACE                                                                  
BEST20   LA    R2,5(R2)            BUMP TO NEXT PROD/EST                        
         BCT   R0,BEST05                                                        
         B     BESTX               NO MORE ROOM IN TABLE                        
         SPACE                                                                  
BEST30   MVC   0(5,R5),0(R2)       SAVE PROD/EST/BPRD IN AIO3                   
         SPACE                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+4(3),0(R2)      PROD                                         
         MVC   KEY+7(1),3(R2)      ESTIMATE                                     
         SPACE                                                                  
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA                       
         GOTO1 HIGH                                                             
         SPACE                                                                  
         CLC   KEY(7),KEYSAVE      SAME A-M/CLT/PRD                             
         BNE   BEST50                                                           
         SPACE                                                                  
         OC    KEY+8(5),KEY+8                                                   
         BNZ   BEST50                                                           
         SPACE                                                                  
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         MVC   FILENAME,=CL8'SPTFIL' SWITCH TO SPOT MEDIA                       
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         SPACE                                                                  
         USING ESTHDRD,R6                                                       
         MVC   5(L'EDESC,R5),EDESC SAVE ESTIMATE NAME IN TABLE                  
         B     BEST60                                                           
         SPACE                                                                  
         DROP  R6                                                               
         SPACE                                                                  
BEST50   MVC   5(6,R5),=C'??????'  ESTIMATE RECORD NOT FOUND                    
         SPACE                                                                  
BEST60   LA    R2,5(R2)            BUMP TO NEXT PROD/EST                        
         BCT   R0,BEST05                                                        
         SPACE                                                                  
BESTX    MVC   KEY,SVKEY                                                        
         XC    FILENAME,FILENAME                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* FIND PRODUCT IN SVCLIST *                                                     
* INPUT = BYTE (BINARY PRODUCT CODE)                                            
* OUTPUT = FULL (EBCDIC PRODUCT CODE IN BYTES 1-3)                              
         SPACE                                                                  
FNDPRD   NTR1  BASE=*,LABEL=*                                                   
         MVC   FULL,SPACES                                                      
         L     R1,ASVCLIST       LOOK UP PRD CODE IN CLIST                      
         SPACE                                                                  
FPRD10   CLC   BYTE,3(R1)                                                       
         BE    FPRD20                                                           
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '        END OF CLIST                                   
         BH    FPRD10                                                           
         DC    H'0'                                                             
FPRD20   MVC   FULL(3),0(R1)     SET EBCDIC PRODUCT CODE                        
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* GET INSTRUCTION ELEMENTS FROM TRAFFIC INSTRUCTION RECAP RECORD                
* IF WE FIND A X'10' ELEMENT, SET TO READ PATTERN RECORDS (AUTO GEN)            
* IF WE FIND A X'30' ELEMENT, SET TO READ BUY RECORDS     (SPOT GEN)            
* IF WE FIND BOTH, USE ELEMENT WITH LATER DATE                                  
* IF BOTH ELEMENTS HAVE THE SAME DATE, USE X'10' ELEMENT, READ PATTERNS         
*                                                                               
GETINST  NTR1  BASE=*,LABEL=*                                                   
         XC    SVINDT,SVINDT                                                    
         MVI   INSTREC,0                                                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL            FIND INST DATA ELEM X'10'                    
         B     *+8                                                              
GINST10  BRAS  RE,NEXTEL                                                        
         BNE   GINST20                                                          
         USING INSDTAEL,R6                                                      
***      CLC   INSDATE,PERENDP     IS INST. DATE WITHIN REQUEST PERIOD?         
***      BH    GINST10                                                          
***      CLC   INSDATE,PERSTRP                                                  
***      BL    GINST10                                                          
         CLC   INSDATE,SVINDT      DOES THIS ELEMENT HAVE A LATER DATE          
         BNH   GINST10                                                          
         MVC   SVINDT,INSDATE      SAVE LATEST INST DATE WITHIN RANGE           
         MVI   INSTREC,C'P'        READ PATTERN RECORDS                         
         B     GINST10                                                          
*                                                                               
GINST20  L     R6,AIO                                                           
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
GINST30  BRAS  RE,NEXTEL                                                        
         BNE   GINSTX              FIND INST DATA ELEM X'30'                    
***      CLC   INSDATE,PERENDP     IS INST. DATE WITHIN REQUEST PERIOD?         
***      BH    GINST30                                                          
***      CLC   INSDATE,PERSTRP                                                  
***      BL    GINST30                                                          
         CLC   INSDATE,SVINDT      IF WE FOUND BOTH, USE LATER DATE             
         BNH   GINST30                                                          
         MVI   INSTREC,C'B'        READ BUY RECORDS                             
GINSTX   XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
************************************************                                
* READ THROUGH BUYS AND GET COMMERCIAL ID                                       
************************************************                                
*                                                                               
READBUYS NTR1  BASE=*,LABEL=*                                                   
         GOTO1 DATCON,DMCB,(5,0),(3,TODAYB)                                     
         SPACE                                                                  
* CLEAR EQUIVALENT PRODUCT AREA *                                               
         SPACE                                                                  
         XC    DATELIST(232),DATELIST                                           
         SPACE                                                                  
* 001-230 115 PRS (EQUIV/BASE PRODS) 203-232 SAVED PROD EQUIV TABLE             
* 233-256 24 SAVED BUY KEY                                                      
         SPACE                                                                  
         MVC   SYSDIR(3),=C'SPT'   SWITCH TO SPOT MEDIA                         
         MVC   SYSFIL(3),=C'SPT'                                                
         SPACE                                                                  
***      BRAS  RE,GBUYINIT         INITIALIZE GETBUY                            
         SPACE                                                                  
         XC    KEY,KEY                                                          
         SPACE                                                                  
         MVC   KEY(9),SVKEY+2      A-M/C/P/MKT/STA FROM INST RECAP              
         CLI   STATEST,C'Y'        DOES INST RECAP HAVE AN ESTIMATE?            
         BNE   RBUY02                                                           
         MVC   KEY+9(1),SVKEY+11   ESTIMATE                                     
         SPACE                                                                  
RBUY02   MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
***Y02   BRAS  RE,GBUYHIGH         NO UPDATE REQUIRED                           
         SPACE                                                                  
RBUY03   CLC   KEY(9),KEYSAVE      TEST SAME A-M/C/P/MKT/STA                    
         BNE   RBUY92                                                           
         SPACE                                                                  
RBUY05   CLI   KEY+3,X'FF'         PRD POL ?                                    
         BNE   RBUY06                                                           
         SPACE                                                                  
         GOTO1 SEQ                 YES, WAS PROCCESSED (PASSIVE PTR)            
***      BRAS  RE,GBUYRSEQ         YES, WAS PROCCESSED (PASSIVE PTR)            
         B     RBUY02                                                           
         SPACE                                                                  
RBUY06   MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   I WANT CONTROL BACK                          
         GOTO1 CATCHIOS            DON'T TASKNEXT OUT!                          
         CLI   ERROR,0             ANY ERROR?                                   
         BE    RBUY25                                                           
         SPACE                                                                  
GBACTER  GOTO1 ERREX                                                            
         SPACE                                                                  
* FILTER ON ESTIMATE                                                            
         SPACE                                                                  
RBUY25   DS    0H                                                               
         CLI   STATEST,0           WAS EST IN INST REC?                         
         BE    RBUY32                                                           
         SPACE                                                                  
         CLC   SVKEY+11(1),KEY+9   SAME ESTIMATE?                               
         BNE   RBUY92                                                           
         SPACE                                                                  
RBUY32   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         EJECT                                                                  
* GET DAYPART EQUIVALENCY CODE IF PROFILE ON AND NONE IN FOR ESTIMATE *         
         SPACE                                                                  
         CLI   SVPROF11,C'A'       TEST COPY CODE = ADJACENCY                   
         BE    RBUY35                                                           
         CLI   SVPROF11,C'D'       TEST COPY CODE = DAYPART                     
         BE    RBUY35                                                           
         CLI   SVPROF11,C'Y'       TEST COPY CODE = DAYPART                     
         BNE   RBUY42                                                           
RBUY35   CLC   EQVEST,KEY+9        SAME EST                                     
         BE    RBUY42                                                           
         MVC   TRBUYKEY,KEY                                                     
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
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
         BNE   RBUY40                                                           
         MVI   ELCODE,X'10'                                                     
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         MVC   FILENAME,=CL8'TRFFIL'                                            
         GOTO1 GETREC                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DPEDTAEL,R6                                                      
         LA    R1,EQVTAB                                                        
RBUY36   MVC   0(2,R1),DPEDPCDE                                                 
         LA    R1,2(,R1)                                                        
         BRAS  RE,NEXTEL                                                        
         BE    RBUY36                                                           
RBUY40   MVC   EQVEST,TRBUYKEY+9                                                
         XC    FILENAME,FILENAME                                                
         DROP  R6                                                               
         MVC   KEY,TRBUYKEY                                                     
         SPACE                                                                  
         XC    TRBUYKEY,TRBUYKEY                                                
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
***      BRAS  RE,GBUYHIGH         NO UPDATE REQUIRED                           
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         EJECT                                                                  
* NOW GET BUY RECORD *                                                          
         SPACE                                                                  
RBUY42   DS    0H                                                               
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 GETREC                                                           
***      BRAS  RE,GBUYGETR                                                      
         SPACE                                                                  
         MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   I WANT CONTROL BACK                          
         GOTO1 CATCHIOS            DON'T TASKNEXT OUT!                          
         CLI   ERROR,0             ANY ERROR?                                   
         BNE   GBACTER                                                          
         SPACE                                                                  
         L     R5,AIO                                                           
         USING BUYRECD,R5                                                       
         SPACE                                                                  
         CLC   KEY+4(2),4(R5)     TEST SAME MARKET                              
         BNE   RBUY90               NO - SPILL                                  
         SPACE                                                                  
         TM    15(R5),X'80'        DELETED RECORD                               
         BO    RBUY90               YES, BYPASS                                 
         SPACE                                                                  
         MVC   BUYDATE,BDCHG                                                    
         SPACE                                                                  
* CALCULATE DIFFERENCE BETWEEN FIRST/LAST DAYS OF ROTATOR *                     
         SPACE                                                                  
         XC    ROTDAYS,ROTDAYS                                                  
         ZIC   R0,BDDAY                                                         
         SLL   R0,25                                                            
         SPACE                                                                  
         CLI   OWRSDAY,0                                                        
         BE    RBUY45                                                           
         ZIC   RF,OWRSDAY                                                       
         SPACE                                                                  
         LTR   R0,R0                                                            
         BNM   *+8                                                              
         O     R0,=X'00800000'                                                  
         SLL   R0,1                                                             
         BCT   RF,*-14                                                          
         SPACE                                                                  
RBUY45   LTR   R0,R0               REG GOES NEGATIVE WHEN BIT ARRIVES           
         BZ    *+16                                                             
         BM    *+12                                                             
         SLL   R0,1                                                             
         B     *-14                                                             
         SPACE                                                                  
         SR    RE,RE               CLEAR COUNTER                                
         SLL   R0,1                                                             
         LTR   R0,R0               SHIFT TILL NO MORE BITS ON                   
         BZ    *+8                                                              
         BCT   RE,*-10                                                          
         LPR   RE,RE                                                            
         STH   RE,ROTDAYS          AND SAVE DAYS                                
         SPACE                                                                  
         CLI   BUYKEY+3,X'FF'      TEST POOL BUYREC                             
         BNE   RBUY90              BYPASS, BRAND BUYS NOT SUPPORTED             
         SPACE                                                                  
         LA    R1,24(,R5)          POINT TO FIRST ELEMENT                       
         SR    RF,RF                                                            
RB61     ICM   RF,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                BLOW UP ON ELEMENT LENGTH ZERO               
         AR    R1,RF               POINT TO NEXT ELEMENT                        
         CLI   0(R1),0             TEST END OF RECORD                           
         BE    RB61ND                                                           
         CLI   0(R1),X'61'         TEST FOR MASTER CLIENT ELEMENT               
         BNE   RB61                                                             
         SPACE                                                                  
         CLI   1(R1),6             THIS ELEM INCLUDE PROD CODE                  
         BL    RB61ND                                                           
         SR    R0,R0                                                            
         ICM   R0,1,5(R1)          SAVE PROD CODE                               
         BZ    RB61ND                                                           
         SPACE                                                                  
         LA    R1,24(,R5)          POINT TO FIRST ELEMENT                       
         SPACE                                                                  
RB61EL   ICM   RF,1,1(R1)                                                       
         BNZ   *+6                                                              
         DC    H'0'                BLOW UP ON ELEMENT LENGTH ZERO               
         AR    R1,RF               POINT TO NEXT ELEMENT                        
         CLI   0(R1),0             TEST END OF RECORD                           
         BE    RB61ND                                                           
         CLI   0(R1),X'0B'         TEST FOR POL ORIG                            
         BL    RB61EL                                                           
         CLI   0(R1),X'0D'         TEST FOR POL FLIP                            
         BH    RB61EL                                                           
         CLI   1(R1),10            UNALLOCATED PRODUCT                          
         BNH   RB61EL                                                           
         STC   R0,10(,R1)          STORE NEW PRODUCT CODE                       
         B     RB61EL                                                           
         SPACE                                                                  
RB61ND   DS   0H                                                                
         EJECT                                                                  
* POL PROCESSING *                                                              
         SPACE                                                                  
         MVI   ELCDLO,11                                                        
         MVI   ELCDHI,13                                                        
         LA    R6,BDELEM                                                        
         SPACE                                                                  
RBUY48   BAS   RE,BUYEL2                                                        
         BNE   RBUY90                                                           
         CLI   1(R6),10            TEST UNALL                                   
         BNH   RBUY48                                                           
         CLI   BPRD,0              TEST ONE PRD REQUEST                         
         BE    RBUY50                                                           
         CLC   BPRD,10(R6)         YES - TEST MATCH PROD                        
         BE    RBUY50                                                           
         CLI   1(R6),18            PIGGYBACK                                    
         BL    RBUY48                                                           
         CLC   BPRD,14(R6)                                                      
         BNE   RBUY48                                                           
         SPACE                                                                  
RBUY50   TM    6(R6),X'C0'         TEST MINUS OR MINUSED                        
         BNZ   RBUY48                                                           
         MVC   ELDATE,2(R6)        SAVE ELEM START DATE                         
         MVC   ELDATEX,2(R6)       AND PRESET ELEM END DATE                     
*                                                                               
         CLI   SVT1PR1,0           TEST ADJUST ROTATOR DAYS                     
         BE    RBUY52              NO                                           
         CLC   SVT1PR1(1),BDDAY    TEST DAYS MATCH                              
         BNE   RBUY52                                                           
         SPACE                                                                  
* BACK UP ONE DAY *                                                             
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(2,ELDATE),WORK                                      
         ZIC   R0,SVT1PR2          GET NUMBER OF DAYS TO BACK UP                
         LNR   R0,R0                                                            
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,ELDATE)                                    
*                                                                               
RBUY52   CLC   ELDATE,PERENDP      TEST AFTER PERIOD END DATE                   
         BH    RBUY48                                                           
*                                                                               
         CLI   0(R6),11            TEST REGEL                                   
         BNE   RBUY54                                                           
         GOTO1 DATCON,DMCB,(2,ELDATEX),WORK                                     
         LH    R0,ROTDAYS                                                       
         GOTO1 ADDAY,(R1),WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,(R1),WORK+6,(2,ELDATEX)                                   
*                                                                               
RBUY54   CLC   ELDATEX,PERSTRP     TEST BEFORE PERIOD START                     
         BL    RBUY48                                                           
*                                                                               
         BRAS  RE,VALCPY           VALIDATE COPY CODE                           
         BNE   RBUY90                                                           
*                                                                               
         MVC   BYTE,10(R6)                                                      
         BRAS  RE,GGETPRD                                                       
         MVC   MYPRD,BYTE          MAY BE EQUIVALENT PROD                       
*                                                                               
         CLI   1(R6),18            TEST PIGGYBACK                               
         BNL   RBUY55               YES                                         
         CLI   BPRD2,0             TEST PTR = NONE                              
         BE    RBUY60                                                           
         CLI   BPRD2,0             TEST PTR REQUIRED                            
         BNE   RBUY48               YES, BYPASS                                 
         B     RBUY60                                                           
         SPACE                                                                  
RBUY55   CLI   BPRD2,0             TEST PTR = NONE                              
         BE    RBUY48               BYPASS                                      
         MVC   BYTE,14(R6)                                                      
         BRAS  RE,GGETPRD                                                       
         MVC   MYPRD2,BYTE         MAY BE EQUIVALENT PROD                       
*                                                                               
         CLI   BPRD2,0             LIMIT ON PTR                                 
         BE    RBUY60              NO                                           
         CLC   BPRD2,MYPRD2                                                     
         BNE   RBUY48                                                           
*                                                                               
RBUY60   CLI   BPRD,0              ALL POL REQ                                  
         BE    RBUY63                                                           
         CLC   MYPRD,BPRD          ELSE MUST MATCH PRD 1                        
         BNE   RBUY48                                                           
*                                                                               
RBUY63   LR    R1,R6                                                            
RBUY66   SR    RF,RF               FIND X'18' ELEMENT THAT FOLLOWS              
         ICM   RF,1,1(R1)          X'0B' - X'0D' ELEMENTS                       
         BNZ   *+6                                                              
         DC    H'00'                                                            
         AR    R1,RF                                                            
         CLI   0(R1),0                                                          
         BE    RBUY48                                                           
         CLI   0(R1),X'0B'                                                      
         BL    RBUY66                                                           
         CLI   0(R1),X'0D'                                                      
         BNH   RBUY48                                                           
         CLC   0(2,R1),=X'1808'    TEST FOR TRAFFIC ASSIGNED COMML              
         BNE   RBUY66                                                           
         BRAS  RE,GETCOM           GET COMML ID FROM COMM SEQ                   
         B     RBUY48              GET NEXT X'0B' - X'0D' ELEMENT               
         SPACE                                                                  
RBUY90   MVC   KEYSAVE,KEY         SAVE BUY DATA                                
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
RBUY90F  GOTO1 SEQ                 GET NEXT KEY                                 
***Y90F  BRAS  RE,GBUYRSEQ         GET NEXT KEY                                 
         SPACE                                                                  
         CLI   KEY+3,X'FF'         PRD POL ?                                    
         BE    RBUY90F                                                          
         B     RBUY03                                                           
         EJECT                                                                  
RBUY92   XC    FILENAME,FILENAME   SWITCH BACK TO STRAFFIC                      
         MVC   SYSDIR(3),=C'TRF'                                                
         MVC   SYSFIL(3),=C'TRF'                                                
**       MVC   SYSDIR,SVSYSDIR     RESTORE CALLER'S FILES, ETC.                 
**       MVC   SYSFIL,SVSYSFIL                                                  
         SPACE                                                                  
         DROP  R5                                                               
         XIT1                                                                   
         EJECT                                                                  
BUYEL2   CLI   0(R6),0                                                          
         BE    BUYEL2X                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   0(1,R6),ELCDLO                                                   
         BL    BUYEL2                                                           
         CLC   0(1,R6),ELCDHI                                                   
         BH    BUYEL2                                                           
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
BUYEL2X  LTR   RE,RE                                                            
         BR    RE                                                               
* FIND PRODUCT IN SVCLIST *                                                     
         SPACE                                                                  
GGETPRD  NTR1  BASE=*,LABEL=*                                                   
         BAS   RE,GETPRD                                                        
         XIT1  REGS=(R1)                                                        
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* FIND PRODUCT IN SVCLIST *                                                     
         SPACE                                                                  
GETPRD   L     R1,ASVCLIST                                                      
         SPACE                                                                  
GETPRD10 CLC   BYTE,3(R1)                                                       
         BE    GETPRD20                                                         
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BH    GETPRD10                                                         
         DC    H'0'                                                             
GETPRD20 CLI   SVPROF13,C'Y'       PROD EQIVALENCY ACTIVE                       
         BNER  RE                   NO                                          
         SPACE                                                                  
* NOW LOOK FOR EQUIVALENT PRODUCT CODE *                                        
         SPACE                                                                  
         LA    RF,DATELIST                                                      
GETPRD30 CLC   BYTE,0(RF)                                                       
         BE    GETPRD34                                                         
         CLI   0(RF),0                                                          
         BE    GETPRD40                                                         
         LA    RF,2(,RF)                                                        
         B     GETPRD30                                                         
         SPACE                                                                  
* NOW LOOK UP EQUIVALENT PRODUCT *                                              
         SPACE                                                                  
GETPRD34 CLC   BYTE,1(RF)          IS THIS A BASE                               
         BER   RE                   YES, DONE                                   
         MVC   BYTE,1(RF)          SAVE EQUIVALENT                              
         L     R1,ASVCLIST                                                      
         SPACE                                                                  
GETPRD36 CLC   BYTE,3(R1)                                                       
         BER   RE                                                               
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BH    GETPRD36                                                         
         DC    H'0'                                                             
         SPACE                                                                  
GETPRD40 NTR1                                                                   
         SPACE                                                                  
         MVC   DATELIST+232(24),KEY                                             
         SPACE                                                                  
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
         SPACE                                                                  
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
         SPACE                                                                  
* PUT EQUIVALENT AND BASE IN TABLE *                                            
         SPACE                                                                  
GETPRD46 LA    RE,115                                                           
         LA    RF,DATELIST                                                      
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
         SPACE                                                                  
GETPRD60 LA    RE,100                                                           
         LA    RF,DATELIST                                                      
GETPRD64 CLI   0(RF),0                                                          
         BE    GETPRD66                                                         
         LA    RF,2(,RF)                                                        
         BCT   RE,GETPRD64                                                      
         DC    H'0'                                                             
GETPRD66 MVC   0(1,RF),BYTE        SAVE BASE BPRD                               
         MVC   1(1,RF),BYTE                                                     
         SPACE                                                                  
GETPRD70 MVC   KEY(24),DATELIST+232                                             
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         MVI   RDUPDATE,C'N'       NO UPDATE REQUIRED                           
         GOTO1 HIGH                                                             
         SPACE                                                                  
         XIT1  REGS=(R1)                                                        
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* GET COMMERCIAL ID FROM COMMERCIAL SEQUENCE NUMBER                             
* R1 --> TRAFFIC CMML ASSIGN ELEMENT                                            
*                                                                               
         USING TRACID,R1                                                        
GETCOM   NTR1  BASE=*,LABEL=*                                                   
         XC    TALCMID,TALCMID                                                  
         XC    WORK,WORK                                                        
         LA    R3,WORK             SORT RECORD                                  
         USING CMLISTD,R3                                                       
         MVI   CMLINST,C'B'        READING BUYS                                 
         MVC   HALF,TRACCSQ                                                     
         BRAS  RE,READCOM          READ COMMERCIAL RECORD                       
*                                                                               
         CLI   SVTALEX,C'Y'        EXCLUDE FROM TALENT TRANSFER?                
         BE    GCOM10                                                           
*                                                                               
         MVC   CMLCML,TALCMID                                                   
         OC    CMLCML,SPACES                                                    
*                                                                               
         OC    SVCMML,SVCMML       WAS COMML ENTERED                            
         BZ    GCOM10                                                           
***      ZIC   R1,SVCMMLN                                                       
***      EX    R1,*+8                                                           
***      B     *+10                                                             
***      CLC   CMLCML(0),SVCMML                                                 
         CLC   CMLCML,SVCMML                                                    
         BE    GCOM10                                                           
         XC    CMLCML,CMLCML                                                    
*                                                                               
GCOM10   XC    TALCMID,TALCMID                                                  
         MVC   HALF,TRACCSQ2                                                    
         OC    HALF,HALF                                                        
         BZ    GCOM15                                                           
         BRAS  RE,READCOM          READ COMML 2 RECORD                          
*                                                                               
         CLI   SVTALEX,C'Y'        EXCLUDE FROM TALENT TRANSFER?                
         BE    GCOM15                                                           
*                                                                               
         MVC   CMLCML2,TALCMID                                                  
         OC    CMLCML2,SPACES                                                   
*                                                                               
         OC    CMLCML2,CMLCML2                                                  
         BZ    GCOM15                                                           
***      ZIC   R1,SVCMMLN                                                       
***      EX    R1,*+8                                                           
***      B     *+10                                                             
***      CLC   CMLCML2(0),SVCMML                                                
         CLC   CMLCML2,SVCMML                                                   
         BE    GCOM20                                                           
         XC    CMLCML2,CMLCML2                                                  
*                                                                               
GCOM15   OC    CMLCML,CMLCML       NEITHER COMMERCIALS MATCH FILTER             
         BZ    GCOMX                                                            
GCOM20   LA    R4,SAVEKEY          KEY OF INSTRUCTION RECAP RECORD              
         BRAS  RE,PUTCML           PUT COMMERCIAL TO SORTER                     
*                                                                               
GCOMX    XIT1                                                                   
         DROP  R1,R3                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* READ PASSIVE COMMERCIAL PROFILE RECORD USING COMML SEQUENCE NUMBER            
*                                                                               
         USING TRACID,R1                                                        
READCOM  NTR1  BASE=*,LABEL=*                                                   
         XC    MYWORK2,MYWORK2                                                  
         MVI   SVTALEX,0                                                        
*                                                                               
         MVC   SVBKEY,KEY          SAVE KEY OF BUY RECORD                       
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CMLKEY,R6                                                        
         MVC   CMLPID,=X'0AA1'                                                  
         MVC   CMLPAM,BAGYMD       AGENCY/MEDIA                                 
         MVC   CMLPCLT,BCLT        CLIENT                                       
         MVC   CMLPSEQ+1(2),HALF   COMML SEQ NO                                 
*                                                                               
         MVC   SYSDIR(3),=C'TRF'                                                
         MVC   SYSFIL(3),=C'TRF'                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    RCOM10                                                           
         EDIT  (B2,TRACCSQ),(4,TALCMID+2),FILL=0,WRK=MYWORK2                    
         MVC   TALCMID(2),=C'**'                                                
         MVC   TALCMID+6(2),=C'**'                                              
         B     RCOMX                                                            
*                                                                               
RCOM10   MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         XC    TALCMID,TALCMID                                                  
         MVC   TALCMID(L'CMLKCML),CMLKCML       COMML ID                        
*                                                                               
         L     R6,AIO                                                           
         USING CMLADIEL,R6         IS IT AN AD-ID COMMERCIAL?                   
         MVI   ELCODE,X'A0'                                                     
         BRAS  RE,GETEL                                                         
         BNE   RCOM20                                                           
         MVC   TALCMID,CMLADID                                                  
*                                                                               
RCOM20   MVI   ELCODE,X'10'        GET CMML DATA ELEMENT                        
         BRAS  RE,GETEL                                                         
         BNE   RCOM30                                                           
         USING CMLDTAEL,R6                                                      
         CLI   TLCMFLG,C'Y'        IF RUNNING TALCOM, DON'T EXCLUDE             
         BE    RCOM30                                                           
         MVC   SVTALEX,CMLTALEX    EXCLUDE FROM TALENT TRANSFER                 
         DROP  R6                                                               
*                                                                               
RCOM30   MVC   SYSDIR(3),=C'SPT'   SWITCH TO SPOT MEDIA                         
         MVC   SYSFIL(3),=C'SPT'                                                
         MVC   AIO,AIO1                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVBKEY),SVBKEY        RESTORE LAST BUY RECORD              
         GOTO1 HIGH                                                             
RCOMX    XIT1                                                                   
         DROP  R1                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* READ COMMERCIAL PROFILE RECORD USING COMML ID                                 
*                                                                               
READCOM2 NTR1  BASE=*,LABEL=*                                                   
         MVI   SVTALEX,0                                                        
         MVI   SVTALEX2,0                                                       
*                                                                               
         MVC   SVBKEY,KEY          SAVE KEY OF PATTERN RECORD                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CMLKEY,R6                                                        
         MVC   CMLPID,=X'0A21'                                                  
         MVC   CMLPAM,BAGYMD       AGENCY/MEDIA                                 
         MVC   CMLPCLT,BCLT        CLIENT                                       
         MVC   CMLKCML,0(R2)       COMML ID                                     
*                                                                               
         CLI   PACKFLG,C'Y'        ARE ALL COMMERCIALS PACKED?                  
         BNE   *+8                                                              
         MVI   CMLPID+1,X'C1'      READ X'0AC1'                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   RCOM220                                                          
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'        GET CMML DATA ELEMENT                        
         BRAS  RE,GETEL                                                         
         BNE   RCOM220                                                          
         USING CMLDTAEL,R6                                                      
         CLI   TLCMFLG,C'Y'        IF RUNNING TALCOM, DON'T EXCLUDE             
         BE    RCOM220                                                          
         MVC   SVTALEX,CMLTALEX    EXCLUDE FROM TALENT TRANSFER                 
         DROP  R6                                                               
*                                                                               
RCOM220  OC    8(L'CMLKCML,R2),8(R2)   IS THERE A COMMRECIAL 2?                 
         BZ    RCOM230                                                          
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CMLKEY,R6                                                        
         MVC   CMLPID,=X'0A21'                                                  
         MVC   CMLPAM,BAGYMD       AGENCY/MEDIA                                 
         MVC   CMLPCLT,BCLT        CLIENT                                       
         MVC   CMLKCML,8(R2)       COMML ID 2                                   
*                                                                               
         CLI   PACKFLG,C'Y'        ARE ALL COMMERCIALS PACKED?                  
         BNE   *+8                                                              
         MVI   CMLPID+1,X'C1'      READ X'0AC1'                                 
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   RCOM230                                                          
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'        GET CMML DATA ELEMENT                        
         BRAS  RE,GETEL                                                         
         BNE   RCOM230                                                          
         USING CMLDTAEL,R6                                                      
         CLI   TLCMFLG,C'Y'        IF RUNNING TALCOM, DON'T EXCLUDE             
         BE    RCOM230                                                          
         MVC   SVTALEX2,CMLTALEX   EXCLUDE FROM TALENT TRANSFER                 
         DROP  R6                                                               
*                                                                               
RCOM230  MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(L'SVBKEY),SVBKEY  RESTORE LAST PATTERN RECORD                
         GOTO1 HIGH                                                             
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        PUT COMMERCIALS TO WORKER FILE - REGULAR AND PIGGYBACK                 
*                                                                               
PUTCOM   NTR1  BASE=*,LABEL*                                                    
         BRAS  RE,READCML          GET PIGGYBACKS                               
*                                                                               
         OC    MYPIGGY,MYPIGGY     NO PIGGYBACKS?                               
         BNZ   PCOM10                                                           
         BRAS  RE,PUTWRKR          PUT SORT RECORD TO WORKER FILE               
         B     PCOMX                                                            
*                                                                               
PCOM10   LA    R0,4                CAN HAVE UP TO 4 PIGGYBACKS                  
         LA    R3,MYPIGGY                                                       
PCOM20   OC    0(L'TALCMID,R3),0(R3)                                            
         BZ    PCOMX                                                            
         MVC   TALCMID,0(R3)                                                    
         BRAS  RE,PUTWRKR          PUT SORT RECORD TO WORKER FILE               
         LA    R3,L'TALCMID(R3)                                                 
         BCT   R0,PCOM20                                                        
PCOMX    XIT1                                                                   
*                                                                               
* READ COMMERCIAL RECORD USING 8 CHARACTER COMMERCIAL ID                        
*                                                                               
READCML  NTR1  BASE=*,LABEL=*                                                   
         XC    MYPIGGY,MYPIGGY                                                  
         NI    SVFLAG,X'FF'-COMHDEF                                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CMLKEY,R6                                                        
         MVC   CMLKID,=X'0A21'                                                  
         MVC   CMLKAM,BAGYMD       AGENCY/MEDIA                                 
         MVC   CMLKCLT,HOLDALL     OLD CLIENT                                   
         MVC   CMLKCML,TALCMID     COMML ID                                     
*                                                                               
         CLI   TALCMID+8,C' '      IF COMML ID IS > 8 CHARS                     
         BNH   RCML05              MUST BE ADID                                 
         XC    WORK,WORK                                                        
         GOTO1 ATRPACK,DMCB,(C'P',TALCMID),WORK                                 
         MVI   CMLKID+1,X'C1'      READ X'0AC1'                                 
         MVC   CMLPADID,WORK       PACKED COMMERCIAL                            
         DROP  R6                                                               
*                                                                               
RCML05   MVC   FILENAME,=CL8'TRFDIR'                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    RCML10                                                           
         CLC   TALCMID(2),=CL2'**'   SKIP IF COMM SEQUENCE IS INVALID           
         BE    RCML30                                                           
*                                                                               
         CLI   TALCMID+8,C' '      DID WE CHECK ADID KEY?                       
         BH    *+6                                                              
         DC    H'00'               NO - DUMP, SHOULD BE THERE                   
*                                                                               
         XC    KEY,KEY             YES, ADID DIDN'T EXIST SO NOW CHECK          
         LA    R6,KEY              HIDEF KEY                                    
         USING CMLKEY,R6                                                        
         MVC   CMLKID,=X'0AC2'                                                  
         MVC   CMLKAM,BAGYMD       AGENCY/MEDIA                                 
         MVC   CMLKCLT,HOLDALL     OLD CLIENT                                   
         MVC   CMLHDFID,WORK       PACKED COMMERCIAL                            
         OI    SVFLAG,COMHDEF                                                   
         DROP  R6                                                               
*                                                                               
         MVC   FILENAME,=CL8'TRFDIR'                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    RCML10                                                           
         DC    H'00'               HIDEF KEY DOESN'T EXIST                      
*                                                                               
RCML10   MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'TRFFIL'                                            
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
*                                                                               
         LA    R0,4                UP TO 4 PIGGYBACKS ALLOWED                   
         LA    R4,MYPIGGY                                                       
         MVI   ELCODE,X'60'        PIGGYBACK COMMERCIALS?                       
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
RCML20   BRAS  RE,NEXTEL                                                        
         BNE   RCML30                                                           
         USING CMLACTEL,R6                                                      
         CLI   CMLACTLN,CMLACTL1  OLD LENGTH?                                   
         BH    RCML25                                                           
         MVC   0(L'CMLACTID,R4),CMLACTID                                        
         B     *+10                                                             
RCML25   MVC   0(L'CMLACTCM,R4),CMLACTCM   NEW LENGTH                           
         LA    R4,L'CMLACTCM(R4)   ALWAYS BUMP NEW LENGTH                       
         BCT   R0,RCML20                                                        
         DROP  R6                                                               
*                                                                               
RCML30   XC    FILENAME,FILENAME                                                
         MVC   AIO,AIO1                                                         
RCMLX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* VALIDATE COPY CODE                                                            
*                                                                               
         SPACE                                                                  
VALCPY   NTR1  BASE=*,LABEL=*                                                   
         USING BUYRECD,R5                                                       
         ZIC   RF,BUYKEST                                                       
         CLI   SVPROF11,C'E'      COPY CODE = ESTIMATE                          
         BE    VCPYYES                                                          
         LA    RF,SVESTAB(RF)                                                   
         SPACE                                                                  
         CLI   SVPROF11,C'P'      USING 1ST CHAR OF PROGRAM CODE                
         BE    VCPYYES            YES, ALL OKAY                                 
         SPACE                                                                  
         CLI   0(RF),X'FF'        COPY CODE FROM ESTIMATE                       
         BE    VCPY14              NO                                           
         CLI   OFFLINE,C'Y'                                                     
         BE    VCPYYES                                                          
         CLI   SVPROF11,C'D'      ERROR IF DPT=COPY CODE ON                     
         BE    VCPY10                                                           
         CLI   SVPROF11,C'Y'      ERROR IF DPT=COPY CODE ON                     
         BE    VCPY10                                                           
         SPACE                                                                  
VCPY10   CLC   EQVEST,BUYKEY+9    EQUIV TABLE FOR THIS EST                      
         BNE   VCPYYES                                                          
         B     VCPYNO             COPY CODE EST# HAS EQUIV TABLE                
*                                                                               
VCPY14   CLI   SVPROF11,C'A'       TEST ADJACENCY CODE                          
         BE    VCPY16                                                           
         CLI   SVPROF11,C'D'       TEST DPT=COPY CODE                           
         BE    VCPY16                                                           
         CLI   SVPROF11,C'Y'       TEST DPT=COPY CODE                           
         BNE   VCPYYES                                                          
*                                                                               
VCPY16   CLI   SVPROF11,C'A'       USE ADJACENCY CODE                           
         BNE   VCPYYES                                                          
         CLI   BDPROGT,C'9'                                                     
         BH    VCPYNO              ADJACENCY CODE NOT A-Z OR 0-9                
         BE    VCPYYES                                                          
         CLI   BDPROGT,C'0'                                                     
         BNH   VCPYYES                                                          
         CLI   BDPROGT,C'Z'                                                     
         BH    VCPYNO              ADJACENCY CODE NOT A-Z OR 0-9                
         BE    VCPYYES                                                          
         CLI   BDPROGT,C'A'                                                     
         BL    VCPYNO              ADJACENCY CODE NOT A-Z OR 0-9                
*                                                                               
VCPYYES  XR    RC,RC                                                            
VCPYNO   LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
* READ MARKET LIST RECORD & SAVE IT IN THE LAST 120 BYTES OF ELEM               
         EJECT                                                                  
* SEARCH FOR EQUAL ENTRY OR BUILD ONE *                                         
* R3 --> SORT RECORD                                                            
         SPACE                                                                  
         USING CMLISTD,R3                                                       
         USING INSKEY,R4                                                        
PUTCML   NTR1  BASE=*,LABEL=*                                                   
         CLI   UPDATE,C'N'                                                      
         BE    *+8                                                              
         BRAS  RE,OPENWRKR         OPEN WORKER FILE                             
*                                                                               
         L     R5,COUNT            TABLE COUNT                                  
         MVC   CMLCLI,SVBCLT       CLIENT                                       
         MVC   CMLMKT,INSKMKT                                                   
         MVC   CMLFTD(4),SVTCDTS                                                
         SPACE                                                                  
         MVI   CMLCBL,0                                                         
         MVC   CMLCSTA,INSKSTA                                                  
         CLI   INSKSTA,CABLESTA    IF THIS IS A CABLE STATION,                  
         BL    LR56                                                             
         MVI   CMLCBL,C'C'         SORT CABLE AFTER STATIONS                    
         MVC   CMLINDX,SVCBLIND                                                 
         MVC   SVCSTA,INSKSTA      CABLE STATION                                
*                                                                               
LR56     DS    0H                                                               
         TM    SVOPTSW,OPTENAME    PRINT ESTIMATE NAME                          
         BZ    *+8                                                              
         BRAS  RE,AESTLIST         ADD ESTIMATE TO ESTIMATE LIST                
         SPACE                                                                  
*NOP     TM    SVOPTSW,OPTESTP                                                  
******   BZ    LR56F                                                            
         MVC   CMLEST,INSKCOPY                                                  
         MVC   CMLPRD,SVPRD1       PRODUCT                                      
         MVC   CMLPRD2,SVPRD2      PARTNER                                      
         MVC   CMLSLN1,SVSLN1       AND THEIR LENGTHS                           
         MVC   CMLSLN2,SVSLN2                                                   
         SPACE                                                                  
LR56F    CLI   SVMGRPS,0           BY MARKET GROUP                              
         BE    LR58                 NO                                          
         BRAS  RE,RMG              READ & SAVE MARKET GROUP                     
         SPACE                                                                  
LR58     L     R3,CMLSTA                                                        
         AR    R3,R9                                                            
         SPACE                                                                  
         CLI   OFFLINE,C'Y'        IF OFFLINE, SORTING, NO TABLE                
         BE    LR66                                                             
         SPACE                                                                  
         LTR   R0,R5               COUNT OF EXISTING                            
         BZ    LR64                                                             
         SPACE                                                                  
LR60     CLC   CMLENT,WORK                                                      
         BE    PCMLX                                                            
         LA    R3,CMLNEXT                                                       
         BCT   R0,LR60                                                          
         C     R3,CMLSTX                                                        
         BNL   LSTSIZER                                                         
LR64     MVC   CMLENT,WORK                                                      
         LA    R3,CMLNEXT                                                       
         C     R3,CMLSTX                                                        
         BNL   LSTSIZER                                                         
         B     LR70                BYPASS TABLE BUILD                           
LR66     TM    SVOPTSW,OPTRACE                                                  
         BZ    LR68                                                             
         MVC   SVP,P                                                            
         MVC   P,SPACES                                                         
         MVC   P(28),WORK                                                       
         GOTO1 HEXOUT,DMCB,WORK,P+30,30,0,0                                     
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVC   P,SVP                                                            
         SPACE                                                                  
LR68     DS    0H                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',WORK                                     
         SPACE                                                                  
LR70     LA    R5,1(,R5)           ADD TO TABLE COUNT                           
         ST    R5,COUNT                                                         
PCMLX    XIT1                                                                   
         DROP  R3,R4                                                            
         EJECT                                                                  
LSTSIZER TM    WHEN,X'40'          THIS NOW                                     
         BZ    LSTSIZPT            NO PRINT MESSAGE                             
         MVC   CONHEAD,LSTSIZMS                                                 
         GOTO1 ERREX2                                                           
LSTSIZPT XC    SPECS,SPECS                                                      
         XC    HEADHOOK,HEADHOOK                                                
         MVC   P+20(33),=CL33'REQUEST REPORT FOR SMALLER PERIOD'                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
LSTSIZMS DC    C'* ERROR * LIST TOO LARGE FOR ONLINE, RUN SMALLER OR OVC        
                *'                                                              
         LTORG                                                                  
         EJECT                                                                  
* ADD PROD/ESTIMATE TO ESTIMATE LIST                                            
*  (5 BYTE ENTRIEST - 3 BYTE PROD/1-EST/1-BPRD)                                 
         SPACE                                                                  
AESTLIST NTR1  BASE=*,LABEL=*                                                   
         SPACE                                                                  
*  GET PRODUCT CODE                                                             
         SPACE                                                                  
         LA    R0,220                                                           
         L     R1,ASVCLIST         TABLE OF CLIENT PROD CODES                   
AEST10   CLC   SVPRD1,3(R1)        THIS A VALID PROD CODE                       
         BE    AEST30                                                           
         LA    R1,4(,R1)           BUMP PROD PTR                                
         CLI   0(R1),C' '          AT END OF TABLE?                             
         BNH   *+8                 YES, DEATH                                   
         BCT   R0,AEST10                                                        
         DC    H'0'                                                             
         SPACE                                                                  
AEST30   MVC   SVPROD,0(R1)                                                     
         SPACE                                                                  
         LA    R0,100              100 ESTIMATES                                
         LHI   R1,ESTLIST-SYSD                                                  
         AR    R1,R9               PROD/ESTIMATE LIST AREA                      
***      LA    R1,ESTLIST                                                       
         SPACE                                                                  
AEST50   OC    0(5,R1),0(R1)       EMPTY                                        
         BZ    AEST60                                                           
         SPACE                                                                  
         CLC   0(3,R1),SVPROD      SAME PROD                                    
         BNE   *+14                                                             
         CLC   3(1,R1),SVEST       AND ESTIMATE                                 
         BE    AESTX                                                            
         SPACE                                                                  
         LA    R1,5(R1)                                                         
         BCT   R0,AEST50                                                        
         B     AESTX                                                            
         SPACE                                                                  
AEST60   MVC   0(4,R1),SVPRDEST    SAVE PROD/ESTIMATE                           
         MVC   4(1,R1),SVPRD1      BPRD                                         
         SPACE                                                                  
AESTX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
* READ MARKET GROUP RECORDS TO FIND MARKETS FOR THIS MARKET GROUP *             
* R3 --> SORT RECORD                                                            
         SPACE                                                                  
         USING CMLISTD,R3                                                       
RMG      NTR1  BASE=*,LABEL=*                                                   
         SPACE                                                                  
         CLC   RMGMKT,CMLMKT                                                    
         BNE   RMG00                                                            
         MVC   CMLMGR,RMGMGR                                                    
         B     RMGX                                                             
         SPACE                                                                  
RMG00    MVC   RMGKEY,KEY                                                       
         SPACE                                                                  
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING MKGRECD,R4                                                       
         MVC   MKGPTYP,=X'0D82'                                                 
         MVC   MKGPAGMD(3),BAGYMD & BCLT                                        
         MVC   MKGPMID,SVMGRPS                                                  
*        MVC   MKGPMID(4),SVGRPKEY       MOVE IN PRD GRP/MKT GRP SCHEME         
*                                        FROM VMG (VALIDATE MARKET GRP)         
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWITCH TO SPOT MEDIA SYSTEM                
         GOTO1 HIGH                                                             
         SPACE                                                                  
         CLC   KEY(9),KEYSAVE                                                   
         BE    RMG14                                                            
         SPACE                                                                  
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLIENT                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         SPACE                                                                  
RMG10    CLC   KEY(9),KEYSAVE                                                   
         BNE   RMG40                                                            
         SPACE                                                                  
RMG14    CLC   INSKMKT-INSKEY+SVKEY,MKGPMKT       THIS THE MARKET               
         BE    RMG20                                                            
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         B     RMG10                                                            
         SPACE                                                                  
RMG20    MVC   CMLMGR,MKGPMGRP                                                  
         MVC   RMGMGR,MKGPMGRP                                                  
         MVC   RMGMKT,INSKMKT-INSKEY+SVKEY  SAVE THIS MARKET                    
         SPACE                                                                  
         SPACE                                                                  
RMG36    MVC   KEY(L'RMGKEY),RMGKEY                                             
         SPACE                                                                  
         XC    FILENAME,FILENAME                                                
         SPACE                                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    RMGX                                                             
         DC    H'0'                                                             
         SPACE                                                                  
RMG40    MVC   CMLMGR,=XL2'9999'  PUT IN UNKNOWN MARKET GROUP                   
         MVC   SVMGRP+1(2),=XL2'9999'                                           
         CLC   SVMGRPMK,INSKMKT-INSKEY+SVKEY                                    
         B     RMG36                                                            
RMGX     XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* OPEN WORKER FILE                                                              
*==================================================================             
                                                                                
OPENWRKR NTR1  BASE=*,LABEL=*                                                   
         TM    WRKRFLAG,WRKROPEN   WORKER FILE ALREADY OPEN?                    
         JO    OPENWRKX                                                         
         XC    ID,ID                                                            
         XC    IOWRKR,IOWRKR                                                    
         LA    R3,ID                                                            
         USING UKRECD,R3                                                        
         MVC   UKUSRID,TWAORIG                                                  
         MVC   UKSYSPRG,=C'STA'                                                 
         MVI   UKSUBPRG,C'L'                                                    
         GOTO1 DATCON,DMCB,(5,0),(1,COUNTER)       TODAY'S DATE                 
         MVC   UKDAY,COUNTER+2                                                  
         MVI   UKCLASS,C'T'               CLASS                                 
         OI    UKFLAG,X'01'               ALLOW DUPLICATE KEYS                  
         OI    UKFLAG,X'10'               RETENTION DAYS                        
         MVC   COMMAND,=CL6'OPEN'                                               
*                                                                               
         LA    R1,IOWRKR+28                                                     
         USING WKRECD,R1                                                        
         MVC   WKRETN,=H'7'                                                     
         DROP  R1                                                               
*                                                                               
         LA     R3,IOWRKR                                                       
         L      R4,=A(WRKRBUFF)                                                 
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,(R3)                                 
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         OI    WRKRFLAG,WRKROPEN                                                
OPENWRKX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
         DROP  R3                                                               
*                                                                               
MOVEWRKR NTR1  BASE=*,LABEL=*                                                   
         XC    IOWRKR,IOWRKR                                                    
         LHI   R4,MYWORK-SYSD                                                   
         AR    R4,R9                                                            
         MVC   IOWDATA,0(R4)       MOVE ELEMENTS TO WORKER OUT AREA             
* - ADD UP ELEM LENGTHS IN R1                                                   
         LA    R3,IOWDATA                                                       
         SR    R1,R1                                                            
MWRK10   ZIC   R2,1(R3)                                                         
         AR    R1,R2               ADD TO TOTAL ELEM LENGTH                     
         AR    R3,R2               BUMP TO NEXT ELEMENT                         
         CLI   1(R3),0             ANY MORE ELEMENTS                            
         BNE   MWRK10                                                           
         LA    R1,4(R1)            NO/ADD 4 BYTE IOWLEN HEADER                  
         STH   R1,IOWLEN       SET TOTAL ELEM LENGTH IN WORKER HEADER           
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
ADDWRKR  NTR1  BASE=*,LABEL=*                                                   
         MVC   COMMAND,=CL6'ADD'                                                
         LA    R3,IOWRKR                                                        
         L     R4,=A(WRKRBUFF)                                                  
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,(R3)                                 
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
CLOSWRKR NTR1  BASE=*,LABEL=*                                                   
         TM    WRKRFLAG,WRKROPEN   HAS WORKER FILE BEEN OPENED?                 
         JZ    CLOSWRKX                                                         
         MVC   COMMAND,=CL6'CLOSE'                                              
         LA    R3,IOWRKR                                                        
         L     R4,=A(WRKRBUFF)                                                  
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,(R3)                                 
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   COMMAND,=CL8'RETAIN'                                             
*                                                                               
         LA    R1,IOWRKR+28                                                     
         USING WKRECD,R1                                                        
         MVC   WKRETN,=H'7'                                                     
         DROP  R1                                                               
*                                                                               
         LA    R3,IOWRKR                                                        
         L     R4,=A(WRKRBUFF)                                                  
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,(R3)                                 
CLOSWRKX XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* SAVE COPY OF WORKER FILE TO MVS DATASET                                       
*==================================================================             
                                                                                
AUDITWRK NTR1  BASE=*,LABEL=*                                                   
         L     R9,ASYSD                                                         
         GOTO1 DATCON,DMCB,(5,0),(1,TODAYP)                                     
*                                                                               
         BAS   RE,RDFRSTW          READ FIRST WORKER FILE                       
         JNE   EXIT                IF NO WORKER FILES, SKIP AUDIT               
*                                                                               
         BAS   RE,OPENMVS          OPEN MVS CLONE DATASET                       
         BAS   RE,PUTMVS           READ WRKR FILES AND PUT TO DATASET           
         BRAS  RE,CLOSMVS          CLOSE MVS CLONE DATASET                      
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* READ FIRST WORKER FILE.  IF NONE EXIST, RETURN CCNEQ.                         
*==================================================================             
RDFRSTW  NTR1                                                                   
         L     R4,=A(WRKBUFF)  SET A(WORKER FILE BUFFER)                        
         ST    R4,AWRKBUFF                                                      
*                                                                               
         XC    INDEX,INDEX                                                      
         XC    WRKNM,WRKNM                                                      
         MVC   WRKFILEN,=CL8'WKFILE'  SET OLD WORKER FILE                       
         MVC   WRKNM,=C'STAL'  SET TO READ SPOT WRKR FILES                      
*                                                                               
         BAS   RE,GTWFILE      GET FIRST/(NEXT) WORKER FILE                     
         JNE   NO                                                               
*                                                                               
         J     YES                                                              
*                                                                               
         EJECT                                                                  
*==================================================================             
* DYNAMICALLY ALLOCATE MVS SEQUENTIAL DATASET                                   
*                                                                               
* SYSTEM RECORD ON TAL FILE WILL BE ADDED TO POINT TO THIS FILE                 
*==================================================================             
                                                                                
OPENMVS  NTR1                                                                   
         USING SSOOFF,R3                                                        
         GOTO1 DATAMGR,DMCB,=C'SSBAD'                                           
         ICM   R3,15,4(R1)         =A(SSB)                                      
         CLC   0(2,R3),=H'00'                                                   
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   BYTE,SSODSPAC-SSOOFF(R3)                                         
         DROP  R3                                                               
*                                                                               
         USING MYDSND,R3                                                        
         LA    R3,FILEDSN                                                       
         MVC   FILEDSN,MYDSN       DEFAULT TO ADV                               
         CLI   BYTE,C'A'                                                        
         JE    OPENW1                                                           
*                                                                               
         MVC   FILEDSN,MYDSN2                                                   
         CLI   BYTE,C'C'                                                        
         JNE   *+10                                                             
         MVC   FILEDSN(3),=C'CSC'                                               
         CLI   BYTE,C'Q'                                                        
         JNE   *+10                                                             
         MVC   FILEDSN(3),=C'FQA'                                               
         CLI   BYTE,C'T'                                                        
         JNE   *+10                                                             
         MVC   FILEDSN(3),=C'TST'                                               
*                                                                               
OPENW1   L     RF,TWAMASTC                                                      
         USING MASTD,RF                                                         
         CLI   MCTSTRUN,X'FF'                                                   
         BNE   *+8                                                              
         MVI   MYDSNTST,C'2'       SET TEST FILENUM IN NAME                     
         DROP  RF                                                               
*                                                                               
         MVC   MYDSNAGY(2),TWAAGY  SET ORIGINATING AGY                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,BTODAY),(X'20',MYDSNDAT)                          
*                                                                               
         TIME  DEC                 R0=HHMMSSHH                                  
         ST    R0,FULL                                                          
         GOTO1 HEXOUT,DMCB,FULL,DUB,4,=C'TOG'                                   
         MVC   MYDSNT1(4),DUB     MOVE HHMM                                     
         MVC   MYDSNT2(4),DUB+4   MOVE SSHH                                     
         DROP  R3                                                               
*                                                                               
         L     RF,TWADCONS                                                      
         USING TWADCOND,RF                                                      
         MVC   PRNTBL,TPRNTBL                                                   
         DROP  RF                                                               
         L     RF,TDYNALLO-TWADCOND(RF)                                         
         GOTO1 (RF),DMCB,(X'80',=C'FILEOUT '),(X'87',ALLOCS),          X        
               (X'80',FILEDSN)                                                  
*                                                                               
         TM    SVOPTSW,OPTRACE                                                  
         BZ    OPENW2                                                           
         MVC   P(13),=C'OPENW MVS DSN'                                          
         MVC   P+14(L'FILEDSN),FILEDSN                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
OPENW2   OPEN  (FILEOUT,(OUTPUT))                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ID,ID                                                            
         LA    R3,ID                                                            
         USING UKRECD,R3                                                        
         MVC   UKUSRID,TWAORIG                                                  
         MVC   UKSYSPRG,=C'STA'                                                 
         MVI   UKSUBPRG,C'L'                                                    
         GOTO1 DATCON,DMCB,(5,0),(1,COUNTER)       TODAY'S DATE                 
         MVC   UKDAY,COUNTER+2                                                  
         MVI   UKCLASS,C'T'               CLASS                                 
         OI    UKFLAG,X'01'               ALLOW DUPLICATE KEYS                  
         OI    UKFLAG,X'10'               RETENTION DAYS                        
         DROP  R3                                                               
*                                                                               
         MVC   IOWDATA(L'UKINDEX),ID      MOVE ID TO IO BUFFER                  
*                                                                               
         L     RF,TWAMASTC                         POINT TO MASTC               
*        MVC   IOWDATA+20(10),MCUSERID-MASTD(RF)   MOVE ALPHA ID                
         L     R6,AIO3                                                          
         USING CTIKEY,R6                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,MCORIGID-MASTD(RF)   SET ORIGIN ID NUMBER                
         DROP  R6                                                               
*MN                                                                             
         XC    FILENAME,FILENAME                                                
         MVC   SYSFIL,=CL8'CTFILE  '                                            
         MVC   SYSDIR,=CL8'CTFILE  '                                            
         MVC   DMFILE,=CL8'SPTFIL  '                                            
         MVC   COMMAND,=CL8'GETRECDM'                                           
*MN                                                                             
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',AIO3,AIO3               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO3                                                          
         MVC   HALF,DATADISP                                                    
         MVC   DATADISP,=H'28'                                                  
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST EXIST/ELSE BOMB                         
         ZIC   R1,1(R6)                                                         
         A     R1,=F'-3'                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IOWDATA+20(0),2(R6)   SAVE AGENCY ID                             
*                                                                               
         LA    RE,34               SET REC LEN                                  
         SLL   RE,16                                                            
         ST    RE,IOWLEN                                                        
*                                                                               
         L     R1,=A(FILEOUT)                                                   
         LA    R0,IOWRKR                                                        
         PUT   (1),(0)             PUT WRKR HDR AS FIRST FILE REC               
*                                                                               
         J     EXIT                                                             
*                                                                               
ALLOCS   DC    AL3(20),AL3(10)     PRIMARY/SECONDARY ALLOCATIONS                
*                                                                               
*                                                                               
         EJECT                                                                  
*=============================================================                  
* READ WORKER FILES AND PUT TO MVS DATASET                                      
*=============================================================                  
                                                                                
PUTMVS   NTR1                                                                   
*                                                                               
         L     R4,=A(WRKBUFF)  SET A(WORKER FILE BUFFER)                        
         ST    R4,AWRKBUFF                                                      
*                                                                               
         XC    INDEX,INDEX                                                      
         XC    WRKNM,WRKNM                                                      
         MVC   WRKFILEN,=CL8'WKFILE'  SET OLD WORKER FILE                       
         MVC   WRKNM,=C'STAL'  SET TO READ SPOT WRKR FILES                      
*                                                                               
PMVS10   BAS   RE,GTWFILE      GET FIRST/(NEXT) WORKER FILE                     
         BNE   PMVSX                                                            
*                                                                               
         BAS   RE,WKREAD       READ FIRST WORKER FILE RECORD                    
         BNE   PMVS10                                                           
         BAS   RE,PROCWF       READ WRKR FILE, SAVE TO DATASET                  
         BAS   RE,CLOSE        CLOSE WORKER FILE                                
         BAS   RE,INDRERD      RE-READ THIS INDEX RECORD                        
         B     PMVS10          LOOP FOR NEXT WORKER FILE                        
*                                                                               
PMVSX    J     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO GET A WORKER FILE                                     
         SPACE 1                                                                
GTWFILE  NTR1                                                                   
         XC    RECORD,RECORD                                                    
         LA    R2,INDEX                                                         
         USING UKRECD,R2                                                        
*                                                                               
GTWFL20  BAS   RE,INDEXRD          GET INDEX OF FIRST/NEXT FILE                 
         JNE   NO                                                               
*                                                                               
         TM    UKSTAT,X'08'        WORKER FILE MUST NOT BE ON KEEP              
         BO    GTWFL20                                                          
         CLC   UKUSRID,TWAORIG                                                  
         BNE   GTWFL20                                                          
*                                                                               
         CLC   UKSYSPRG,WRKNM      WORKER FILE NAME MUST MATCH                  
         BNE   GTWFL20                                                          
         CLI   UKCLASS,C'T'        MUST BE CLASS T                              
         BNE   GTWFL20                                                          
         CLC   UKDAY,TODAYP+2      MUST BE TODAY'S DATE                         
         BNE   GTWFL20                                                          
*                                                                               
         LA    R3,L'INDEX                                                       
         GOTO1 MYTRACE,DMCB,=C'GOOD INDEX',INDEX,(R3)                           
         J     YES                                                              
         EJECT                                                                  
*                                                                               
*              WORKER FILE I/O ROUTINES                                         
*                                                                               
         SPACE 3                                                                
*              ROUTINE TO READ A RECORD IN THE CURRENT WORKER FILE              
*                                  SAME ROUTINE FOR OLD & NEW STYLE             
WKREAD   NTR1                                                                   
         LA    RE,WREC             CLEAR WORKER FILE RECORD AREA                
         LH    RF,=AL2(L'WREC)                                                  
         XCEFL                                                                  
*                                                                               
         MVC   COMMAND2,=CL8'READ'                                              
         GOTO1 DATAMGR,DMCB,COMMAND2,WRKFILEN,INDEX,WREC,AWRKBUFF               
         CLI   DMCB+8,0            IF RECORD FOUND                              
         JNE   NO                                                               
*                                                                               
         LH    R2,WREC             LENGTH OF RECORD                             
         SH    R2,=H'4'                                                         
         GOTO1 MYTRACE,DMCB,=C'READ WORKER RECORD',WREC+4,(R2)                  
         J     YES                                                              
         SPACE 2                                                                
*              ROUTINE TO TRY TO FIND A WORKER FILE                             
*                                                                               
INDEXRD  NTR1                                                                   
         MVC   COMMAND2,=CL8'INDEX'                                             
INDRD10  GOTO1 DATAMGR,DMCB,COMMAND2,WRKFILEN,INDEX,WREC,AWRKBUFF               
         CLI   8(R1),0             IF NO ERROR CONDITION CODE                   
         JNE   NO                                                               
*                                                                               
         LA    R3,L'INDEX                                                       
         GOTO1 MYTRACE,DMCB,=C'POSSIBLE INDEX',INDEX,(R3)                       
         J     YES                                                              
*              ROUTINE REREADS INDEX OF CURRENT FILE                            
         SPACE 1                                                                
INDRERD  NTR1                                                                   
         MVC   COMMAND2,=CL8'INDEX'                                             
         GOTO1 DATAMGR,DMCB,COMMAND2,WRKFILEN,INDEX,WREC,AWRKBUFF               
*                                                                               
         LA    R3,L'INDEX                                                       
         GOTO1 MYTRACE,DMCB,=C'REREAD OF INDEX',INDEX,(R3)                      
         J     EXIT                                                             
         EJECT                                                                  
*              ROUTINE TO PROCESS CURRENT WORKER FILE                           
*              STAL LAYOUT - ONE HDR REC FOLW'D BY ONE OR MORE DTL RECS         
*                            DTL REC CONTAINS ALL WSP ELEMENTS                  
         SPACE 1                                                                
PROCWF   NTR1                                                                   
         B     *+12                                                             
*                                                                               
PROCWF5  BAS   RE,WKREAD           READ NEXT WORKER FILE RECORD                 
         JNE   PROCWFX                                                          
*                                                                               
         L     R1,=A(FILEOUT)                                                   
         LA    R0,IOWRKR                                                        
         MVC   IOWRKR,WREC                                                      
         PUT   (1),(0)             PUT TO DATASET                               
         B     PROCWF5                                                          
*                                                                               
* INSERT AN EOF RECORD BETWEEN WORKER FILE COPIES                               
                                                                                
PROCWFX  MVC   IOWDATA(2),=C'/*'                                                
         LA    RE,6                SET REC LEN                                  
         SLL   RE,16                                                            
         ST    RE,IOWLEN                                                        
         L     R1,=A(FILEOUT)                                                   
         LA    R0,IOWRKR                                                        
         PUT   (1),(0)                                                          
*                                                                               
         J      EXIT                                                            
         EJECT                                                                  
*              ROUTINE TO CLOSE THE CURRENT WORKER FILE (NETWORK ONLY)          
         SPACE 1                                                                
CLOSE    LA    R3,RECORD                                                        
         MVC   COMMAND2,=CL8'CLOSE'                                             
         B     GO                                                               
*                                                                               
GO       NTR1                                                                   
         GOTO1 DATAMGR,DMCB,COMMAND2,WRKFILEN,INDEX,(R3),AWRKBUFF               
         CLI   DMCB+8,0                                                         
         J     EXIT                                                             
         EJECT                                                                  
*              SET INFO FOR TRACE ROUTINE                                       
         SPACE                                                                  
MYTRACE  NTR1                                                                   
         TM    SVOPTSW,OPTRACE     IF TRACE IS ON                               
         JNO   EXIT                                                             
         L     R2,0(R1)            A(LITERAL)                                   
         L     RF,8(R1)            SET LENGTH OF RECORD                         
         L     R4,4(R1)            A(RECORD)                                    
         GOTO1 PRNTBL,DMCB,(R2),(R4),C'DUMP',(RF),=C'2D'                        
         J     EXIT                                                             
         LTORG                                                                  
         SPACE 1                                                                
*=============================================================                  
* CLOSE MVS DATASET THAT COPIES WORKER FILES                                    
*=============================================================                  
                                                                                
CLOSMVS  NTR1  BASE=*,LABEL=*                                                   
         CLOSE FILEOUT                                                          
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,=A(FILEOUT)                                                   
         FREEPOOL (1)                                                           
*                                                                               
         TM    SVOPTSW,OPTRACE                                                  
         BZ    CLOSMVS0                                                         
         MVC   P(13),=C'CLOSW MVS DSN'                                          
         MVC   P+14(L'FILEDSN),FILEDSN                                          
         GOTO1 SPOOL,DMCB,(R8)                                                  
                                                                                
*===============================================================                
* ADD A SYSTEM RECORD TO THE TALENT FILE FOR THIS MVS DATASET                   
* (1) OPEN TALDIR AND TALFIL FOR UPDATE!                                        
* (2) CHANGE THE UTL TO POINT TO THE TALENT SYSTEM                              
* (3) IF ASSIGNED DATASPACE IS TST, WRITE TO TAL2                               
*     OTHERWISE, WRITE TO TAL1                                                  
*===============================================================                
                                                                                
CLOSMVS0 L     RF,TWAMASTC              POINT TO MASTC                          
         L     RE,MCSSB-MASTD(RF)       POINT TO SSB                            
         LA    RE,SSODSPAC-SSOOFF(RE)   POINT TO DATASPACE ID                   
*                                                                               
         L     RF,MCUTL-MASTD(RF)  POINT TO UTL                                 
         MVC   HALF(1),4(RF)       SAVE THE PRESENT SYSTEM NUMBER               
*                                                                               
         MVI   4(RF),X'20'         SENUM FOR TAL2                               
         CLI   0(RE),C'T'          TEST RUNNING ON TST                          
         BE    *+8                                                              
         MVI   4(RF),X'10'         ELSE SET SENUM FOR TAL2                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMOPEN',=C'TALENT',TALFLIST,AIO1                 
                                                                                
* CALCULATE TRANSFER PICKUP DATE (NEXT BUSINESS DAY)                            
                                                                                
         GOTO1 DATCON,DMCB,(5,0),(3,TODAYB)                                     
         LA    R1,DMCB             NORMAL CALL FOR PQ RETAIN                    
         USING GETRETD,R1                                                       
         XC    GRDCB,GRDCB         CLEAR CONTROL BLOCK                          
         MVC   GRDHRS,=H'24'       SET 24 HOURS RETAIN PERIOD                   
         MVC   GRDIDYMD,TODAYB     SET YMD BINARY INPUT DATE                    
         MVC   GRDITHM,=X'0800'    SET HM BINARY INPUT TIME                     
         MVI   GRDFLAG,GRDFTAL+GRDFHLOK                                         
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGETRET-COMFACSD(RF)                                          
         GOTO1 (RF),(R1)                                                        
         CLI   GRDRETC,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
K        USING TLSYD,KEY                                                        
         XC    KEY,KEY                                                          
         MVI   K.TLSYCD,TLSYCDQ                                                 
         MVI   K.TLSYTYPE,TLSYSNFN    TYPE=SPOT/NET TRANSFER                    
*                                                                               
         XC    DUB,DUB                                                          
         MVC   DUB(3),GRDODYMD                                                  
         GOTO1 DATCON,DMCB,(3,DUB),(1,DUB+4)                                    
         MVC   K.TLSYSNDT,DUB+4                                                 
         DROP  R1,K                                                             
*                                                                               
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(X'80',=C'DMRDHI'),=C'TALDIR',KEYSAVE,KEY           
*                                                                               
         CLC   KEY(5),KEYSAVE      TEST ANY RECORDS THIS DATE                   
         BNE   CLOSMVS4            NO                                           
*                                                                               
CLOSMVS2 MVC   KEYSAVE,KEY         SAVE HIGH SEQNUM SO FAR                      
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 (RF),(R1),(X'80',=C'DMRSEQ')                                     
         CLC   KEY(5),KEYSAVE                                                   
         BE    CLOSMVS2                                                         
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE LAST KEY                             
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'TALFIL',KEY+34,AIO1, X        
               DMWORK                                                           
         B     CLOSMVS6                                                         
*                                                                               
CLOSMVS4 L     R6,AIO1             BUILD NEW RECORD                             
         USING TLSYD,R6                                                         
*                                                                               
         XC    0(256,R6),0(R6)                                                  
         MVC   0(32,R6),KEYSAVE                                                 
*                                                                               
         LLC   RE,TLSYSEQ          GET LAST SEQNUM USED                         
         LA    RE,1(RE)                                                         
         STC   RE,TLSYSEQ                                                       
*                                                                               
         LA    RE,TLSYELEM-TLSYD   LENGTH TO FIRST ELEM                         
         STCM  RE,3,TLSYLEN                                                     
         DROP  R6                                                               
*                                                                               
CLOSMVS6 LA    R6,ELEM                                                          
         USING TACMD,R6                                                         
*                                                                               
         MVI   TACMEL,TACMELQ                                                   
*        MVI   1(R6),L'MYDSN+3                                                  
         MVI   1(R6),L'FILEDSN+3                                                
         MVI   TACMTYPE,TACMTYPF   SET TYPE=FILENAME                            
         MVC   TACMCOMM(L'FILEDSN),FILEDSN                                      
         DROP  R6                                                               
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CHELLO-COMFACSD(RF) GET HELLO ADDRESS                         
         GOTO1 (RF),DMCB,(C'P',=C'TALFIL'),AIO1,(R6),(0,=C'ADD=END')            
*                                                                               
         CLI   12(R1),0                                                         
         BE    CLOSMVS8                                                         
*                                                                               
         CLI   12(R1),5            TEST REC TOO LONG                            
         BE    CLOSMVS4            GO BUILD NEW RECORD                          
         DC    H'0'                DIE ON ANY OTHER ERROR                       
*                                                                               
CLOSMVS8 L     R1,TWAMASTC         POINT TO MASTC                               
         USING MCBLOCK,R1                                                       
         CLI   MCWRITE,C'Y'        IF WRITE=NO SKIP PUT/ADD                     
         BNE   CLOSMVS9                                                         
         DROP  R1                                                               
*                                                                               
         LA    RE,=C'PUTREC'                                                    
         L     RF,AIO1                                                          
         CLC   KEY(32),0(RF)       TEST KEY MATCHES REC                         
         BE    *+8                                                              
         LA    RE,=C'ADDREC'                                                    
         ST    RE,DMCB                                                          
         MVI   DMCB,X'80'          SET READ FOR UPDATE                          
*                                                                               
         GOTO1 DATAMGR,DMCB,,=C'TALFIL',KEY+34,AIO1,DMWORK                      
*                                                                               
CLOSMVS9 GOTO1 DATAMGR,DMCB,=C'DMCLSE',=C'TALENT',TALFLIST,AIO1                 
*                                                                               
         L     RF,TWAMASTC         POINT TO MASTC                               
         L     RF,MCUTL-MASTD(RF)  POINT TO UTL                                 
         MVC   4(1,RF),HALF        RESTORE ORIGINAL SYSTEM NUMBER               
         J     EXIT                                                             
*                                                                               
TALFLIST DC    CL8'UTALDIR '                                                    
         DC    CL8'UTALFIL '                                                    
         DC    CL8'UTALRCV '                                                    
         DC    C'X'                                                             
*                                                                               
MYDSN    DC    CL44'TALDISK.TA0S1SJ.D100521.THHMM.TSSHH'                        
MYDSN2   DC    CL44'   .TAL.TA0S1SJ.D100521.THHMM.TSSHH'                        
FILEDSN  DS    CL44                                                             
*                                                                               
         LTORG                                                                  
*                                                                               
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=VB,LRECL=512,MACRF=PM              
*                                                                               
         DS    0D                  WORKER FILE RECORD                           
         DC    C'**WREC**'                                                      
WREC     DS    CL4096                                                           
         DS    0D                                                               
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'**WRK1**'                                                      
WRKBUFF  DS    14336C                                                           
         EJECT                                                                  
WRKRBUFF DS    0D                                                               
         DC    4500X'00'                                                        
         EJECT                                                                  
         SPACE 1                                                                
* - PUT SORT RECORDS TO WORKER FILE                                             
*   R3 --> HOLDALL (LAST SORT RECORD)                                           
*                                                                               
PUTWRKR  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,HOLDALL                                                       
         USING CMLISTD,R3                                                       
         CLI   UPDATE,C'N'                    IF TEST RUN                       
         BE    PWRKX                          NO WORKER                         
         SPACE                                                                  
* - SET UP TO WRITE TO WORKER FILE                                              
         SPACE 2                                                                
* - SET ELEMENTS TO MYWORK CREATING PRODUCT NAME ELEMENT                        
         SPACE                                                                  
PWRK4    LHI   RE,MYWORK-SYSD                                                   
         AR    RE,R9                                                            
         L     RF,=F'400'                                                       
         XCEF                                                                   
         LHI   R4,MYWORK-SYSD                                                   
         AR    R4,R9                                                            
*                                                                               
* - NOW INSERT COMMERCIAL ID ELEMENT                                            
         SPACE                                                                  
         USING TANXD,R4                                                         
         MVI   TANXEL,TANXELQ      SET ELEMENT CODE                             
         MVI   TANXLEN,TANXLNQ     SET LENGTH                                   
         MVC   TANXAGY,TALAGY      REQUESTED TALENT AGENCY                      
         MVC   TANXNCID,TALCMID    COMMERCIAL ID                                
         MVI   TANXMED,TANXMTV     MEDIA TV - DEFAULT                           
         CLI   TRAMED,C'R'         IS MEDIA RADIO?                              
         BNE   *+8                                                              
         MVI   TANXMED,TANXMRAD    MEDIA RADIO                                  
         LA    R5,TALCMID                                                       
         BRAS  RE,FCML             FIND COMML LENGTH                            
         MVC   TANXSEC,SVCMLSLN    LENGTH                                       
***      OC    SVADID,SVADID       IF THERE IS AD-ID                            
***      BZ    PWRK5                                                            
         CLI   PCKFLAG,C'Y'        IS COMMERCIAL PACKED?                        
         BE    PWRK3                                                            
         OC    SVCMML,SVCMML       IF THERE IS AD-ID                            
         BZ    PWRK5                                                            
         MVC   TANXADID,SVCMML     ADD AD-ID TO ELEMENT,                        
         MVI   TANXLEN,TANXLN2Q    AND USE LONGER LENGTH                        
         B     PWRK5                                                            
PWRK3    MVC   TANXNCID,SVCMMLP    PACKED COMMERCIAL ID                         
         MVC   TANXADID,TALCMID    ADD NEW AD-ID TO ELEMENT,                    
         MVI   TANXLEN,TANXLN2Q    AND USE LONGER LENGTH                        
         OI    TANXSTAT,TANXPACK   SET STATUS FOR PACKED COMM                   
PWRK5    ZIC   R1,TANXLEN                                                       
         AR    R4,R1               BUMP TO NEXT ELEM AREA IN MYWORK             
         DROP  R4                                                               
         SPACE                                                                  
*                                                                               
* - NOW INSERT CLIENT CODE ELEMENT                                              
         SPACE                                                                  
         USING TACTD,R4                                                         
         MVI   TACTEL,TACTELQ      CLIENT ELEMENT                               
         MVI   TACTLEN,TACTLNQ                                                  
         MVC   TACTCLI(3),QCLT     SET CLIENT CODE                              
         OC    TACTCLI,SPACES                                                   
         ZIC   R1,TACTLEN                                                       
         AR    R4,R1               BUMP TO NEXT ELEM AREA IN MYWORK             
         DROP  R4                                                               
         SPACE                                                                  
*                                                                               
* - NOW INSERT PRODUCT CODE ELEMENT                                             
         SPACE                                                                  
         CLI   CMLPRD,0            IF UNALLOCATED                               
         BE    PWRK10              SKIP PROD CODE ELEMENT                       
         USING TAPRD,R4                                                         
         MVI   TAPREL,TAPRELQ      PRODUCT CODE ELEMENT                         
         MVI   TAPRLEN,TAPRLNQ                                                  
         MVC   BYTE,CMLPRD         BINARY PRODUCT CODE                          
         BRAS  RE,FNDPRD           FIND EBCDIC PRODUCT CODE                     
         MVC   TAPRPRD(3),FULL     SET PROD CODE 1                              
         OC    TAPRPRD,SPACES                                                   
         ZIC   R1,TAPRLEN                                                       
         AR    R4,R1               BUMP TO NEXT ELEM AREA IN MYWORK             
         DROP  R4                                                               
         SPACE                                                                  
* - PRODUCT CODE ELEMENT - 2ND PRODUCT CODE                                     
         SPACE                                                                  
         CLI   CMLPRD2,0           IF UNALLOCATED                               
         BE    PWRK10              SKIP PROD CODE EELEMENT                      
         USING TAPRD,R4                                                         
         MVI   TAPREL,TAPRELQ      PRODUCT CODE ELEMENT                         
         MVI   TAPRLEN,TAPRLNQ                                                  
         MVC   BYTE,CMLPRD2        BINARY PRODUCT CODE                          
         BRAS  RE,FNDPRD           FIND EBCDIC PRODUCT CODE                     
         MVC   TAPRPRD(3),FULL     SET PROD CODE 2                              
         OC    TAPRPRD,SPACES                                                   
         ZIC   R1,TAPRLEN                                                       
         AR    R4,R1               BUMP TO NEXT ELEM AREA IN MYWORK             
         DROP  R4                                                               
         SPACE                                                                  
*                                                                               
* - NOW INSERT CLIENT NAME ELEMENT (VARIABLE LENGTH)                            
         SPACE                                                                  
         USING TAFND,R4                                                         
PWRK10   MVI   TAFNEL,TAFNELQ      SET ELEM CODE                                
         MVI   TAFNTYPE,C'C'       SET TYPE (CLIENT)                            
         MVC   WORK(20),CLTNM      GET CLIENT NAME                              
         LA    R1,19               AND DROP END BLANKS                          
         LA    R2,WORK+19                                                       
PWRK20   CLI   0(R2),X'40'                                                      
         BH    PWRK25                                                           
         BCTR  R1,0                                                             
         BCTR  R2,0                                                             
         B     PWRK20                                                           
PWRK25   EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TAFNNAME(0),WORK                                                 
         LA    R1,4(R1)            ADD FOR CORRECT ELEMENT LENGTH               
         STC   R1,TAFNLEN                                                       
         AR    R4,R1               BUMP TO NEXT ELEM AREA IN MYWORK             
         DROP  R4                                                               
         SPACE                                                                  
*                                                                               
* - NOW INSERT PRODUCT NAME ELEMENT                                             
         SPACE                                                                  
         MVC   BYTE,CMLPRD                                                      
         BRAS  RE,FNDPRD                                                        
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         MVC   KEY+4(3),FULL                                                    
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'               SERIOUS BUG                                  
         MVC   FILENAME,=CL8'SPTFIL'                                            
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PRDHDRD,R6                                                       
         MVC   PRODNAME,PNAME                                                   
         DROP  R6                                                               
         XC    FILENAME,FILENAME                                                
         OC    PRODNAME,PRODNAME   ...IF NO NAME(UNALLOC)                       
         BZ    PWRK40              ...SKIP THIS ELEMENT                         
         USING TAFND,R4                                                         
         MVI   TAFNEL,TAFNELQ                                                   
         MVI   TAFNTYPE,TAFNTPRD   PRODUCT NAME                                 
         LA    R1,19               DROP END BLANKS                              
         LA    RE,PRODNAME+19                                                   
PWRK30   CLI   0(RE),X'40'                                                      
         BH    PWRK35                                                           
         BCTR  R1,0                                                             
         BCTR  RE,0                                                             
         B     PWRK30                                                           
PWRK35   EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TAFNNAME(0),PRODNAME                                             
         LA    R1,4(R1)                                                         
         STC   R1,TAFNLEN                                                       
         AR    R4,R1               BUMP TO NEXT OUT AREA IN MYWORK              
         DROP  R4                                                               
*                                                                               
* - NOW INSERT COMM'L TITLE ELEMENT (VARIABLE LENGTH)                           
         SPACE                                                                  
PWRK40   LA    R5,TALCMID                                                       
         BRAS  RE,FCML             FIND COMML TITLE AND LENGTH                  
*                                                                               
         USING TAFND,R4                                                         
         MVI   TAFNEL,TAFNELQ      SET ELEM CODE                                
         MVI   TAFNTYPE,C'T'       SET TYPE (TITLE)                             
         MVC   WORK(56),SVCMLDSC   GET COMML DESCRIPTION                        
         LA    R1,55               AND DROP END BLANKS                          
         LA    R2,WORK+55                                                       
PWRK45   CLI   0(R2),X'40'                                                      
         BH    PWRK48                                                           
         BCTR  R1,0                                                             
         BCTR  R2,0                                                             
         B     PWRK45                                                           
PWRK48   EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TAFNNAME(0),WORK                                                 
         LA    R1,4(R1)            ADD FOR CORRECT ELEMENT LENGTH               
         STC   R1,TAFNLEN                                                       
         AR    R4,R1               BUMP TO NEXT ELEM AREA IN MYWORK             
         DROP  R4                                                               
         SPACE                                                                  
*                                                                               
* - NOW INSERT NETWORK/CLASS A PROGRAM DETAILS ELEMENT                          
         SPACE                                                                  
         USING TANPD,R4                                                         
         MVI   TANPEL,TANPELQ      ELEM CODE                                    
         CLI   CMLCSTA,CABLESTA    CHECK FOR CABLE                              
         BNL   PWRK80                                                           
***      MVI   TANPLEN,TANPLNQS    LENGTH FOR SPOT                              
         MVI   TANPTYP,TANPSPT     ELEMENT TYPE = SPOT                          
         MVC   TANPMKTN,MKTNM      MARKET NAME                                  
         MVC   TANPMKT,SVMTALM     TALENT MARKET                                
         OC    TANPMKT,TANPMKT     USE TALENT MARKET FIRST IF IT EXISTS         
         BNZ   PWRK90                                                           
         ZICM  R0,SVMNSI,2         USE NSI MARKET CODE IF THERE IS ONE          
         OC    SVMNSI,SVMNSI                                                    
         BNZ   PWRK49                                                           
         ZICM  R0,CMLMKT,2         USE MARKET CODE IF NO NSI                    
PWRK49   CVD   R0,DUB                                                           
         UNPK  FULL,DUB                                                         
         OI    FULL+3,X'F0'                                                     
*                                                                               
         CLI   TRAMED,C'R'         IS MEDIA RADIO?                              
         BNE   PWRK55                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),TRAMED                                                  
         MVC   KEY+2(4),FULL                                                    
         MVC   KEY+6(2),AGENCY                                                  
         MVC   AIO,AIO3                                                         
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO                      
         L     R6,AIO                                                           
         USING MKTRECD,R6                                                       
         CLC   KEY(13),0(R6)                                                    
         BE    *+6                                                              
         DC    H'00'               SERIOUS BUG                                  
         OC    MKTALST(3),MKTALST  IF THERE IS NO ALPHA MARKET CODE,            
         BZ    PWRK55              SEND NUMERIC CODE                            
         XC    FULL,FULL                                                        
         MVC   FULL(3),MKTALST     ALPHA MARKET CODE                            
         DROP  R6                                                               
         MVC   AIO,AIO1                                                         
         LA    R1,FULL                                                          
         LA    R0,4                                                             
PWRK50   CLI   0(R1),X'40'         LETTER OR SPACE?                             
         BNH   PWRK52                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,PWRK50                                                        
         B     PWRK53                                                           
PWRK52   MVI   0(R1),0             ONCE WE FIND A SPACE, PAD RIGHT              
         LA    R1,1(R1)            WITH ZEROES                                  
         BCT   R0,PWRK52                                                        
PWRK53   MVC   TANPMKT,FULL        ALPHA MARKET CODE                            
         B     PWRK90                                                           
*                                                                               
PWRK55   LA    R1,FULL                                                          
         L     R2,FULL                                                          
         LA    R0,4                                                             
PWRK60   CLI   0(R1),X'F0'         GET RID OF LEADING CHARACTER ZEROS           
         BNE   PWRK70                                                           
         SLL   R2,8                PAD RIGHT SIDE WITH ZEROS                    
         LA    R1,1(R1)                                                         
         BCT   R0,PWRK60                                                        
PWRK70   STCM  R2,15,TANPMKT       MARKET CODE - CONVERTED TO EBCDIC            
         B     PWRK90                                                           
*                                                                               
PWRK80   DS    0H                                                               
***K80   MVI   TANPLEN,TANPLNQC    LENGTH FOR CABLE                             
         MVI   TANPTYP,TANPCSYS    ELEMENT TYPE = CABLE                         
         XC    WORK(12),WORK                                                    
         XC    DUB(5),DUB                                                       
         MVC   DUB+2(3),CMLCSTA    STATION/SYSTEM CODE                          
         GOTO1 MSUNPK,DMCB,(X'80',DUB),WORK,WORK+4                              
         MVC   TANPSYS(4),WORK+4   SYSTEM CODE CONVERTED TO EBCDIC              
*                                                                               
         OC    CMLINDX,CMLINDX     MUST HAVE INDEX TO CABLE GROUP NAME          
         BNZ   *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE                                                            
         ICM   RE,3,CMLINDX                                                     
         BCTR  RE,R0                                                            
         MHI   RE,CBLNAML          LENGTH OF 1 ENTRY                            
         LR    RF,RE                                                            
         A     RF,CBLNAM           ADDR OF TABLE                                
         MVC   TANPMKTN,0(RF)      CABLE SYSTEM NAME                            
         OC    TANPMKTN,TANPMKTN                                                
         BNZ   PWRK90                                                           
         MVC   TANPMKTN,MKTNM      USE MARKET NAME IF NO CSYS NAME              
*                                                                               
PWRK90   MVC   TANPDATE,MYSTART    ACTIVE START DATE                            
         MVI   TANPLEN,TANPLNQ4    NEW LENGTH                                   
         ZIC   R1,TANPLEN                                                       
         AR    R4,R1               BUMP TO NEXT ELEM AREA IN MYWORK             
         DROP  R4                                                               
*                                                                               
* - ALL ELEMENTS NOW SIT IN MYWORK                                              
         BRAS  RE,MOVEWRKR         MOVE ELEMENTS INTO WORKER OUT AREA           
         BRAS  RE,ADDWRKR          AND ADD TO WORKER                            
*                                                                               
* - ADD ANOTHER WORKER FILE THE SAME EXCEPT WITH ACTIVE END DATE                
         LHI   R6,MYWORK-SYSD                                                   
         AR    R6,R9                                                            
         MVI   ELCODE,TANPELQ      FIND TANP ELEMENT IN MYWORK                  
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'00'                                                            
         USING TANPD,R6                                                         
         MVC   TANPDATE,MYEND      ACTIVE END DATE                              
         DROP  R6                                                               
* - ALL ELEMENTS NOW SIT IN MYWORK                                              
         BRAS  RE,MOVEWRKR         MOVE ELEMENTS INTO WORKER OUT AREA           
         BRAS  RE,ADDWRKR          AND ADD TO WORKER                            
PWRKX    XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
         SPACE                                                                  
* ON-LINE VALIDATION OF TALENT AGENCY FIELD                                     
*                                                                               
* VALIDATE TRATAL FIELD BY READING FOR ITS TALENT AGENCY REC                    
* AND MATCHING ITS NETWORK USERID ELEM TO REQUESTING AGENCY'S ID                
* TO SEE IF REQ AGY IS ALLOWED ACCESS TO TRATAL AGY                             
*                                                                               
READTAL  NTR1  BASE=*,LABEL=*                                                   
***      MVC   SAVEKEY,KEY                                                      
*                                                                               
* - READ CONTROL REC OF REQUESTING AGENCY TO GET AGENCY'S FULL ID               
         BAS   RE,SWCON                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,TWAORIG           SET ORIGIN ID NUMBER                   
         DROP  R4                                                               
         L     R6,AIO3                                                          
         ST    R6,AIO                                                           
*MN                                                                             
         XC    FILENAME,FILENAME                                                
         MVC   SYSFIL,=CL8'CTFILE  '                                            
         MVC   SYSDIR,=CL8'CTFILE  '                                            
         MVC   DMFILE,=CL8'SPTFIL  '                                            
         MVC   COMMAND,=CL8'GETRECDM'                                           
*MN                                                                             
         GOTO1 READ                                                             
*                                                                               
         MVC   HALF,DATADISP                                                    
         MVC   DATADISP,=H'28'                                                  
         MVI   ELCODE,X'02'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST EXIST/ELSE BOMB                         
         ZIC   R1,1(R6)                                                         
         A     R1,=F'-3'                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVAGYORG(0),2(R6)   SAVE AGENCY ID                               
*                                                                               
*        READ TALENT AGENCY RECORD                                              
*                                                                               
         BAS   RE,SWTAL         SWITCH TO TALENT SYSTEM                         
         XC    KEY,KEY          READ FOR TALENT AGENCY RECORD                   
         LA    R4,KEY                                                           
         USING TLAYD,R4                                                         
         MVI   TLAYCD,TLAYCDQ                                                   
         MVC   TLAYAGY,TRATAL       SCREEN TAL AGY FIELD                        
         OC    TLAYAGY,SPACES                                                   
         DROP  R4                                                               
*MN                                                                             
*        XC    FILENAME,FILENAME                                                
*        MVC   SYSFIL,=CL8'TALFIL  '                                            
*        MVC   SYSDIR,=CL8'TALDIR  '                                            
*        MVC   DMFILE,=CL8'SPTFIL  '                                            
         MVC   COMMAND,=CL8'DMREADDM'                                           
*MN                                                                             
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BE    *+14                                                             
         MVC   GERROR,=Y(TALAGYNF) TALENT AGENCY NOT FOUND                      
         B     TRAPERR2            GET OUT                                      
*                                                                               
*&&DO                              SPSUG-1239                                   
         L     R6,AIO2             YES/GET THE TALENT RECORD                    
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   DATADISP,=H'40'                                                  
*                                                                               
* - LOOK AT TALENT AGENCY REC AND SEE IF SX STATUS ON                           
*                                                                               
         MVI   ELCODE,TAAYELQ                                                   
         BRAS  RE,GETEL                                                         
         BNE   RDT30                                                            
         USING TAAYEL,R6                                                        
         TM    TAAYSTA5,TAAYSPTI   CAN USE SPOT INTERFACE?                      
         BO    RDT40                                                            
*                                                                               
* - LOOK AT TALENT AGENCY REC AND MATCH WITH REQUESTING AGENCY'S ID             
*                                                                               
RDT30    MVI   ELCODE,TANLELQ      FIND NETWORK USERID LIMIT ELEM               
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+14                                                             
         MVC   GERROR,=Y(TALNAUTH) NOT FOUND / INVALID ID                       
         B     TRAPERR2                                                         
*                                                                               
         USING TANLEL,R6                                                        
         ZIC   R3,TANLNUID         GET NUMBER OF USERIDS                        
         N     R3,=X'0000007F'     STRIP HOB                                    
         LA    R5,6                                                             
         TM    TANLNUID,TANLSPID   ARE WE USING 10 INSTEAD?                     
         BZ    *+8                                                              
         LA    R5,10                                                            
*                                                                               
         LA    R6,TANLUID          POINT TO USERID                              
*                                                                               
RDT35    LR    RF,R5                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R6),SVAGYORG    IS IT VALID USERID                           
         BE    RDT40               YES                                          
         AR    R6,R5               NO/TRY AGAIN                                 
         BCT   R3,RDT35                                                         
         MVC   GERROR,=Y(TALNAUTH) INVALID ID                                   
         B     TRAPERR2                                                         
*&&                                                                             
RDT40    DS    0H                                                               
         MVC   DATADISP,HALF       RESET DATADISP                               
         BAS   RE,SWBACK           SWITCH BACK TO ORIGINAL SYSTEM               
*&&DO                                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(13),SAVEKEY                                                  
         GOTO1 HIGH                RESTORE KEY                                  
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         MVC   DMWORK+4(4),KEY+14  SPOOF GENCON SAFETY @CHANGE2                 
*&&                                                                             
         XIT1                                                                   
         EJECT                                                                  
*              ROUTINES TO CONTROL SYSTEM SWITCHING                             
         SPACE 2                                                                
SWCON    DS    0H                                                               
         MVI   USEIO,C'Y'                                                       
         MVC   SYSDIR,=CL8'CTFILE'                                              
         MVC   SYSFIL,=CL8'CTFILE'                                              
         MVC   LKEY,=H'25'                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVI   DMCB,X'0A'          SET TO SWITCH TO CONTROL                     
         B     SWSYS                                                            
         SPACE 2                                                                
SWTAL    DS    0H                                                               
         MVC   SYSDIR,=CL8'TALDIR'                                              
         MVC   SYSFIL,=CL8'TALFIL'                                              
         MVC   LKEY,=H'32'                                                      
         MVC   LSTATUS,=H'2'                                                    
         MVI   DMCB,X'10'          SET TO SWITCH TO TAL1                        
         B     SWSYS                                                            
         SPACE 2                                                                
SWBACK   DS    0H                                                               
         MVI   USEIO,0                                                          
         MVC   SYSDIR,SVSYSDIR     RESTORE CALLER'S FILES, ETC.                 
         MVC   SYSFIL,SVSYSFIL                                                  
         MVC   LKEY,SVLKEY                                                      
         MVC   LSTATUS,=H'1'                                                    
         MVC   DMCB(1),SVSYS       SET TO SWITCH BACK TO ORIG. SYSTEM           
         B     SWSYS                                                            
         SPACE 2                                                                
SWSYS    NTR1                                                                   
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         GOTO1 SWITCH,DMCB,,0                                                   
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                SWITCH ERROR                                 
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
TRAPERR2 GOTO1 VTRAERR                                                          
         LTORG                                                                  
         EJECT                                                                  
* THIS ROUTINE FILLS PRODTBL WITH LIST OF PRODS VALID FOR TAL AGENCY            
*  - PRODUCT RECORD CARRIES VALID TALENT AGENCY CODE                            
*  - MATCH THIS CODE AGAINST REQUESTED TALENT AGENCY                            
PRDLIST  NTR1  BASE=*,LABEL=*                                                   
**       LA    R2,SVCLIST                                                       
*MN      LHI   R2,SVNCLIST-T216FFD                                              
*MN      AR    R2,RA                                                            
         L     R2,ASVNCLST                                                      
**       LA    R5,220                                                           
         LA    R5,253                                                           
         LHI   R4,PRODTBL-SYSD                                                  
         AR    R4,R9               R4 =A(PRODTBL)                               
         XC    0(255,R4),0(R4)                                                  
**       MVI   BYTE,0              SET CLIST2 FLAG                              
PRDL10   LA    R3,KEY                                                           
         USING PRDHDR,R3                                                        
         XC    POLTAGY,POLTAGY                                                  
         XC    KEY,KEY             GET POL TALENT AGY                           
         MVC   PKEYAM,BAGYMD                                                    
         MVC   PKEYCLT,BCLT                                                     
         MVC   PKEYPRD,=C'POL'                                                  
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      IF POL RECORD NOT FOUND                     
         BNE   PRDL15               SKIP                                        
         MVC   FILENAME,=CL8'SPTFIL'                                            
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
         L     R3,AIO                                                           
         OC    PTALAGY,PTALAGY                                                  
         BZ    *+10                                                             
         MVC   POLTAGY,PTALAGY                                                  
*                                                                               
PRDL15   LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   PKEYAM,BAGYMD                                                    
         MVC   PKEYCLT,BCLT                                                     
         MVC   PKEYPRD,0(R2)                                                    
         MVC   FILENAME,=CL8'SPTDIR'                                            
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      IF PROD RECORD NOT FOUND                    
         BNE   PRDL20               SKIP PROD                                   
         MVC   FILENAME,=CL8'SPTFIL'                                            
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
         L     R3,AIO                                                           
**NO-OP  ZIC   R1,TRATALH+5                                                     
**JAN11  BCTR  R1,0                                                             
**       EX    R1,*+8                                                           
**       B     *+10                                                             
**NO-OP  CLC   TRATAL(0),PTALAGY   IF TALENT AGENCY CODES MATCH                 
         OC    PTALAGY,PTALAGY     TEST PRODUCT HAS TALENT AGENCY               
         BNZ   *+10                                                             
         MVC   PTALAGY,POLTAGY     SET POL TALENT AGENCY                        
         CLC   =C'N/A',PTALAGY     TEST TAL AGY = N/A                           
         BE    PRDL20                                                           
*                                                                               
         OC    TRATAL,SPACES                                                    
         OC    PTALAGY,SPACES                                                   
         CLC   TRATAL,PTALAGY      IF TALENT AGENCY CODES MATCH                 
         BNE   PRDL20                                                           
         DROP  R3                                                               
*                                                                               
         MVC   0(1,R4),3(R2)       SET PRODUCT TO LIST                          
         LA    R4,1(R4)                                                         
PRDL20   LA    R2,4(R2)                                                         
         CLI   0(R2),0             ANY MORE PRODS                               
         BE    PRDLX                                                            
         BCT   R5,PRDL15                                                        
*&&DO                                                                           
         CLI   BYTE,2              CLIST2 READ?                                 
         BE    PRDLX               YES                                          
         MVI   BYTE,2                                                           
         LA    R5,35                                                            
         LA    R2,CLIST2                                                        
         CLI   0(R2),0                                                          
         BNE   PRDL10                                                           
*&&                                                                             
PRDLX    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
PRDOK    NTR1  BASE=*,LABEL=*                                                   
         LHI   R2,PRODTBL-SYSD                                                  
         AR    R2,R9              R2 =A(PRODTBL)                                
PRDOK5   CLC   BYTE,0(R2)         IS PRODUCT IN TABLE?                          
         BE    PRDOKX             PROD VALID CC=                                
         LA    R2,1(R2)                                                         
         CLI   0(R2),0                                                          
         BNE   PRDOK5                                                           
         LTR   RE,RE              NOT VALID CC NOT =                            
PRDOKX   XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
***********************************************************************         
*        GET SIZE OF REQUEST - LARGE OR MEDIUM                                  
*        LARGE REQUEST IF RUNNING FOR ALL CLIENTS                               
*        OR IF REQUEST PERIOD IS MORE THAN 3 MONTHS                             
***********************************************************************         
GETSIZE  NTR1  BASE=*,LABEL=*                                                   
         CLI   TWAWHEN,2           SOON JOBS ONLY                               
         BNE   GSIZE10                                                          
         CLI   ALLCLI,C'Y'                                                      
         BE    GSIZE10                                                          
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(3,PERSTART),(0,WORK)                                
         GOTO1 DATCON,DMCB,(3,PEREND),(0,WORK+6)                                
         GOTO1 ADDAY,DMCB,(C'M',WORK),WORK+12,3                                 
         CLC   WORK+6(6),WORK+12              3 MONTHS, OK                      
         BNH   GSIZEX                                                           
GSIZE10  MVI   REQSML,C'L'         MAKE IT A L(ARGE) REQUEST                    
GSIZEX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* SPGETBUY - INITIALIZE                                                         
*                                                                               
* ON ENTRY:    AGENCY              SET TO THE 2 CHAR AGENCY ALPHA               
*                                                                               
* ON EXIT:     SV1OR2              AGY USING 1 OR 2 BYTE BUYLINES               
***********************************************************************         
GBUYINIT NTR1  BASE=*,LABEL=*                                                   
         XC    GETBLK,GETBLK                                                    
         MVI   GBYACT,GBYINIT      INIT CALL                                    
         MVC   GBYAGY,TWAAGY                                                    
         MVC   GBYCOMF,ACOMFACS                                                 
*                                                                               
         GOTO1 AGETBUY,GETBLK                                                   
         MVC   SV1OR2,GBY1OR2                                                   
***************                                                                 
* SPECIAL FOR SJR                                                               
***************                                                                 
         CLC   AGENCY,=C'SJ'                                                    
         JNE   YES                                                              
         CLC   QCLT,=C'TBL'        ONLY CLT TBL ON SJR HAS 2 BYTES              
         JNE   YES                                                              
         MVI   SV1OR2,2            HARD CODE THIS                               
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SPGETBUY - READ HIGH                                                          
*                                                                               
* ON ENTRY:    SV1OR2              AGY USING 1 OR 2 BYTE BUYLINES               
*              KEY                 SET TO BUY KEY YOU WANT                      
*                                                                               
* ON EXIT:     KEY                 SET TO THE KEY YOU READ                      
*              KEYSAVE             SAVED OF WHAT YOU WANTED                     
***********************************************************************         
GBUYHIGH NTR1  BASE=*,LABEL=*                                                   
         XC    GETBLK,GETBLK                                                    
         MVI   GBYACT,GBYHIGH      READ HIGH FOR GBKEST/LIN                     
         MVC   GBYDMIN,DMINBTS                                                  
         MVC   GBY1OR2,SV1OR2                                                   
         MVC   GBYCOMF,ACOMFACS                                                 
*                                                                               
         LA    R1,KEY                                                           
         ST    R1,GBYKEYIN                                                      
         MVC   KEYSAVE,KEY         "GOTO1 HIGH" KEY GETS OVERWRITTEN            
*                                                                               
         GOTO1 AGETBUY,GETBLK                                                   
*                                                                               
         LA    R1,KEY+BUYKDA-BUYKEY                                             
         MVC   SVBUYDA,0(R1)                                                    
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SPGETBUY - READ SEQ                                                           
*                                                                               
* ON ENTRY:    SV1OR2              AGY USING 1 OR 2 BYTE BUYLINES               
*              KEY                 SET TO BUY KEY YOU WANT                      
*                                                                               
* ON EXIT:     KEY                 SET TO THE KEY YOU READ                      
*              KEYSAVE             SAVED OF WHAT YOU WANTED                     
***********************************************************************         
GBUYRSEQ NTR1  BASE=*,LABEL=*                                                   
         XC    GETBLK,GETBLK                                                    
         MVI   GBYACT,GBYSEQ       READ SEQUENTIAL                              
         MVC   GBYDMIN,DMINBTS                                                  
         MVC   GBY1OR2,SV1OR2                                                   
         MVC   GBYCOMF,ACOMFACS                                                 
*                                                                               
         LA    R1,KEY                                                           
         ST    R1,GBYKEYIN                                                      
         MVC   KEYSAVE,KEY         "GOTO1 SEQ" KEY GETS OVERWRITTEN             
*                                                                               
         GOTO1 AGETBUY,GETBLK                                                   
*                                                                               
         LA    R1,KEY+BUYKDA-BUYKEY                                             
         MVC   SVBUYDA,0(R1)                                                    
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SPGETBUY - GETREC                                                             
*                                                                               
* ON ENTRY:    SV1OR2              AGY USING 1 OR 2 BYTE BUYLINES               
*                                                                               
* ON EXIT:                                                                      
***********************************************************************         
GBUYGETR NTR1  BASE=*,LABEL=*                                                   
         XC    GETBLK,GETBLK                                                    
         MVI   GBYACT,GBYGET                                                    
         MVC   GBYDMIN,DMINBTS                                                  
         MVC   GBY1OR2,SV1OR2                                                   
         MVC   GBYCOMF,ACOMFACS                                                 
*                                                                               
         LA    RE,SVBUYDA                                                       
         ST    RE,GBYDA            GBYDA IS THE A(D/A)                          
         L     R1,AIO                                                           
         ST    R1,GBYIOA                                                        
         LA    R1,DMWORK                                                        
         ST    R1,GBYDMWRK                                                      
*                                                                               
         GOTO1 AGETBUY,GETBLK                                                   
         J     YES                                                              
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
*                                                                               
TR21REQ  DCB   DDNAME=TR21REQ,DSORG=PS,RECFM=FB,BLKSIZE=1600,          X        
               LRECL=80,MACRF=PM                                                
*                                                                               
REQCARD  DC    CL80' '                                                          
         ORG   REQCARD                                                          
         DC    C'EQXX 123456 '                                                  
         DC    C'0204ERMQ 0303REP 05**OV,DDS 0904STAL '                         
REQCDID  DC    C'10**ID=12345*'      CAN ADD TEST OR KEEP                       
         ORG                                                                    
*                                                                               
       ++INCLUDE SPTRINST                                                       
         EJECT                                                                  
       ++INCLUDE SPTRDPEQV                                                      
         EJECT                                                                  
       ++INCLUDE SPTRPAT                                                        
         EJECT                                                                  
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRBUY                                                        
         EJECT                                                                  
       ++INCLUDE SPTRPRDEQV                                                     
         EJECT                                                                  
       ++INCLUDE TAGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENMKG                                                       
         EJECT                                                                  
* INCLUDED DSECTS                                                               
* INCLUDE DDSPOOLD                                                              
* INCLUDE DDSPLWORKD                                                            
* INCLUDE DMWRKRK                                                               
* INCLUDE DMWRKRD                                                               
* INCLUDE DDSOFDATD                                                             
* INCLUDE DDCOMFACS                                                             
* INCLUDE FAFACTS                                                               
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE DMWRKRK                                                        
       ++INCLUDE DMWRKRD                                                        
       ++INCLUDE DDSOFDATD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAE0D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
* INCLUDED DSECTS                                                               
* INCLUDE SPTRAWORKD                                                            
         PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
       ++INCLUDE SPSTAPACKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
TSAROFF  DS    A                   TSAROFF CORE RESIDENT T00A7D                 
SVBUYDA  DS    A                   SAVED BUY D/A                                
AGETBUY  DS    A                   ADDRESS OF GETBUY                            
ATRPACK  DS    A                   ADDRESS OF TRPACK                            
SPTR49RR DS    F                                                                
SVR1R2   DS   2F                                                                
SVR6     DS    F                                                                
SVRE     DS    F                                                                
COUNT    DS    F                   COUNTER                                      
         SPACE                                                                  
CMLSTA   DS    F                   ADDRESS OF COMMERCIAL LIST TABLE             
CMLSTX   DS    F                                                                
CBLSTB   DS    F                                                                
CBLSTBX  DS    F                                                                
CBLNAM   DS    F                                                                
CBLNAMX  DS    F                                                                
AACLST   DS    A                   ADDRESS OF ACTIVITY LIST TABLE               
AACLSTX  DS    A                                                                
AWRKBUFF DS    A                   A(WORKER FILE BUFFER)                        
PRNTBL   DS    A                   A(PRNTBL)                                    
SVCBLIND DS    H                                                                
*                                                                               
TRBUYKEY DS    XL13                                                             
EQVEST   DS    XL1                 EQUIVALENCY EST FOR TABLE                    
EQVTAB   DS    CL28                UP TO 14 EQUIV DAY PART CODES                
*                                                                               
ROTDAYS  DS    H                                                                
ELDATE   DS    H                                                                
ELDATEX  DS    H                                                                
SVDATE   DS    H                                                                
SVDATEX  DS    H                                                                
ACLWORK  DS    XL24                                                             
TODAYB   DS    XL3                                                              
TODAYP   DS    XL3                 TODAY PACKED                                 
BUYDATE  DS    XL3                                                              
*                                                                               
FTDATE   DS    XL2                 FTD COVERED BY BOTH RECAP AND BUY            
LTDATE   DS    XL2                 LTD COVERED BY RECAP AND BUY                 
*                                                                               
DATELIST DS    372X                                                             
*                                                                               
SVT2PR05 DS    CL1                 COMBINE CABLE NETWORKS TO 1 STA              
OWRSDAY  DS    XL1                 EOWSDAY FROM EST=OUT OF WK ROT ST DY         
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
WRKRFLAG DS    XL1                                                              
WRKROPEN EQU   X'80'               WORKER FILE HAS BEEN OPENED                  
*                                                                               
SVTSRMKT DS    XL2                 SAVE MARKET                                  
*                                                                               
RMGMKT   DS    XL2                                                              
RMGMGR   DS    XL2                                                              
RMGKEY   DS    CL24                                                             
TOTUNTS  DS    F                                                                
MKTUNTS  DS    H                   UNITS FIELD FROM MARKET RECORD               
SVMKTUNT DS    H                   SAVED MKT UNIT                               
FLDH     DS    CL8                                                              
FLD      DS    CL32                                                             
MYPIGGY  DS    CL48                PIGGYBACK COMMERCIALS 12X4                   
         SPACE                                                                  
SVPRDEST DS    0XL4                PRD1/PRD2/EST                                
SVPROD   DS    XL3                                                              
SVEST    DS    XL1                                                              
*                                                                               
SVPRD1   DS    XL1                 PRD 1                                        
SVPRD2   DS    XL1                 PRD 2                                        
SVSLN1   DS    XL1                 LEN 1                                        
SVSLN2   DS    XL1                 LEN 2                                        
*                                  WHEN PRINTING SAVES LIST OF PRODS            
POLTAGY  DS    CL6                 POL'S TALENT AGENCY                          
TALAGY   DS    CL6                                                              
TALCMID  DS    CL12                                                             
TALLNTH  DS    CL1                                                              
TALDATE  DS    CL3                                                              
TITLEN   DS    CL1                                                              
SVINDT   DS    XL2                 SAVED INSTRUCTION DATE                       
INSTREC  DS    CL1                 RECORD TO READ, B=BUY, P=PATTERN             
         ORG   INSTREC                                                          
UNITMKT  DS    XL1                 MARKETS FOR UNITS                            
UNITMKNY EQU   X'80'               NEW YORK                                     
UNITMKCH EQU   X'40'               CHICAGO                                      
UNITMKLA EQU   X'20'               LOS ANGELES                                  
STATEST  DS    CL1                 STATUS OF COPY CODE = ESTIMATE               
SAVEKEY  DS    CL48                                                             
SVAGYORG DS    CL10                                                             
MYSTART  DS    CL3                 SAVED ACTIVE START                           
MYEND    DS    CL3                 SAVED ACTIVE END                             
MYPRD    DS    CL1                 SAVED PRODUCT                                
MYPRD2   DS    CL1                 SAVED PARTNER PRODUCT                        
TLCMFLG  DS    CL1                 TALCOM LIST FLAG                             
SVNUSER  DS    CL1                 SPOTTAL USER Y/N                             
MYDATE   DS    CL6                                                              
MYDATE2  DS    XL3                                                              
MYDATEB  DS    XL3                 MYDATE BINARY                                
ALLCLI   DS    CL1                 ALL CLIENT STATUS                            
*                                                                               
PRODNAME DS    CL20                                                             
ASRTREC  DS    A                   ADDRESS OF SORT RECORD                       
ID       DS    CL16                                                             
IOWRKR   DS    CL250                                                            
         ORG   IOWRKR                                                           
IOWLEN   DS    CL4                 LENGTH OF WORKER RECORD                      
IOWDATA  DS    CL246               DATA                                         
IOWRKND  DS    CL2                 SET TO ZERO                                  
COUNTER  DS    F                                                                
UPDATE   DS    CL1                                                              
MKTALPH  DS    CL3                 ALPHA MARKET CODE                            
SVMNSI   DS    CL2                 MARKET NSI CODE                              
SVMTALM  DS    XL4                 TALENT MARKET                                
SVTALEX  DS    XL1                 TALENT TRANSFER EXCLUDE (Y/N)                
SVTALEX2 DS    XL1                 TALENT TRANSFER EXCLUDE (Y/N) COMM 2         
PCKFLAG  DS    CL1                 PACKED COMMERCIAL FLAG                       
PACKFLG  DS    CL1                 PACKED COMMERCIAL FLAG                       
*                                                                               
         DS    0D                                                               
SVESTAB  DS    XL256                                                            
         ORG   SVESTAB                                                          
WRKNM    DS    CL4                 WRK FILE SYS/PROGRAM/SUB-PROGRAM             
INDEX    DS    CL42                WORKER FILE INDEX                            
COMMAND2 DS    CL8                 WORKER FILE COMMAND                          
WRKFILEN DS    CL8                 WORKER FILE FILE                             
RECORD   DS    CL150                                                            
         ORG                                                                    
         SPACE                                                                  
* SAVED DATES FROM INSTRUCTION RECAP SUBEL                                      
         SPACE                                                                  
SVTCDTS  DS    XL4                                                              
SVRFTD   DS    XL2                 RECAP FTD                                    
SVRLTD   DS    XL2                 RECAP LTD                                    
         SPACE                                                                  
* FILTERING ON ALL OR PART OF FOLLOWING COMMERCIAL CODE                         
         SPACE                                                                  
SVCMML   DS    CL12                                                             
SVCMMLN  DS    XL1                 LENGTH-1 OF COMPARE FOR CODE ABOVE           
SVCMMLP  DS    CL8                 PACKED COMML                                 
         SPACE                                                                  
* USED TO COMPARE LAST TO NEXT                                                  
         SPACE                                                                  
HOLDCOMP DS   0CL(L'CMLCOMP)                                                    
HOLDALL  DS    CL(L'CMLENT)                                                     
         SPACE                                                                  
* CURRENT MARKET GROUP/COMMERCIAL (OR PAIR) BEING LISTED                        
         SPACE                                                                  
SVCMLMGR DS    XL2                                                              
SVCMLCML DS    CL24                                                             
         SPACE                                                                  
SVCMLDSC DS    CL16                15 BYTES CMML TITLE, 1 DELETE IND            
SVCMLDS2 DS    CL20                20 BYTES CMML DESC 2                         
SVCMLDS3 DS    CL20                20 BYTES CMML DESC 3                         
SVCMLCLT DS    CL20                                                             
SVCMLSLN DS    XL1                                                              
         SPACE                                                                  
SVP      DS    CL132                                                            
SVMKT    DS    XL2                                                              
SVCSTA   DS    XL3                                                              
SVBASIC  DS    CL16                                                             
SVDATES  DS    XL4                 SAVE DATES                                   
         SPACE                                                                  
OPTEST   DS    XL1                                                              
SVOPTSW  DS    XL1                                                              
OPTESTP  EQU   X'80'              PRINT ALL ESTIMATES                           
OPTMGRP  EQU   X'40'              PRINT BY MARKET GROUP                         
OPTDOWN  EQU   X'20'              DOWNLOAD OPTION                               
OPTUNIT  EQU   X'10'              SHOW UNITS FROM MARKET RECORD                 
OPTCABLE EQU   X'08'              SHOW CABLE STATIONS ONLY                      
OPTSTA   EQU   X'04'              SHOW STD STATIONS ONLY                        
OPTRACE  EQU   X'02'              TRACE                                         
OPTENAME EQU   X'01'              PRINT ESTIMATE NAME                           
         SPACE                                                                  
SVFLAG   DS    XL1                                                              
BUYFSW   EQU   X'80'              AT LEAST ONE BUY FOUND FOR THIS RECAP         
DTECOVSW EQU   X'40'              BUY COVERED ALL DATES IN RECAP                
ERREPRT  EQU   X'20'              ERROR REPORT                                  
NORECAP  EQU   X'10'              NO RECAP FOR THIS BUY                         
ERRPRTD  EQU   X'08'              ERROR REPORT PRINTED                          
MKSTCHG  EQU   X'04'              MARKET CHANGED                                
BNFNDSW  EQU   X'02'              BUY NOT FOUND SWITCH                          
COMHDEF  EQU   X'01'              COMMERCIAL IS HIDEF                           
*                                                                               
         SPACE                                                                  
FIRSTSW  DS    XL1                                                              
         SPACE                                                                  
* PERIOD FILTER START AND END DATES                                             
         SPACE                                                                  
PERSTART DS    XL3                                                              
PEREND   DS    XL3                                                              
PERSTRP  DS    XL2                                                              
PERENDP  DS    XL2                                                              
         SPACE                                                                  
SVBCLT   DS    XL2                                                              
         SPACE                                                                  
SVMGRPS  DS    CL1                                                              
SVMGR    DS    CL1                                                              
SVGRPKEY DS   0XL8                                                              
SVPGRP1  DS    XL3                                                              
SVMGRP   DS    XL3                                                              
SVMGRPMK DS    XL2                                                              
         SPACE                                                                  
MGRPTITL DS    CL12                ROOM FOR MGRP TITLE                          
HOLDATES DS    CL17                                                             
SVBKEY   DS    CL24                SAVE BUY KEY                                 
CBLFLAG  DS    CL1                 PRINTING CABLE FLAG                          
CLRDATE  DS    CL1                 N = DO NOT CLEAR BUY DATES                   
SV1OR2   DS    XL1                 FROM SPGETBUY WHETHER AGY USE 1 OR 2         
         SPACE                                                                  
UNPKCML1 DS    CL12                                                             
UNPKCML2 DS    CL12                                                             
         SPACE                                                                  
MYWORK2  DS    CL17                                                             
         SPACE                                                                  
         DS    0F                                                               
GETBLK   DS    0XL(GETBUYL)                                                     
       ++INCLUDE SPGETBUYD                                                      
*                                                                               
***BDATELST AND SVBUYDTE TABLES MUST BE THE SAME SIZE!!!!!                      
BDATELSZ EQU   150                                                              
BDATELST DS    CL600               150 ENTRIES 4 BYTES EACH                     
         DS    XL2'0000'                                                        
SVBUYDTE DS    CL600               150 ENTRIES 4 BYTES EACH                     
         DS    XL2'0000'                                                        
         SPACE                                                                  
         DS    0D                                                               
TSARBLK  DS    CL(TSARDL2)                                                      
         SPACE                                                                  
MYWORK   DS    CL400                                                            
         SPACE                                                                  
PRODTBL  DS    CL300                                                            
         SPACE                                                                  
ESTLIST  DS     XL400               3 BYTE PRD/1 BYTE EST (100 ENTRIES)         
         SPACE                                                                  
*                                                                               
* LIST OF COMMERCIALS BY MARKET, FTD, LTD, AND STATION                          
* BUILT HERE FOR NOW, OR AT DUMMY OFFLINE                                       
*                                                                               
CMLIST   DS    0D                                                               
         SPACE                                                                  
* OFFLINE PRINT LINE LAYOUT *                                                   
         SPACE                                                                  
SPOOLD   DSECT                                                                  
         ORG   P                                                                
PSTART   DS    CL2                                                              
         DS    CL5                 WAS MARKET GROUP                             
         DS    CL6                                                              
PMKT     DS    CL4                                                              
         DS    CL6                                                              
PMKTALPH DS    CL4                                                              
         DS    CL3                                                              
PTALMKT  DS    CL4                                                              
         DS    CL3                                                              
PMKTNM   DS    CL24                                                             
         DS    CL3                                                              
PMKTMKTN DS    CL2                                                              
PMKTMKTC DS    CL2                                                              
PMKTMKTL DS    CL2                                                              
         DS    CL1                                                              
PMKTUNTS DS    CL6                                                              
         DS    CL4                                                              
PACTDTES DS    CL16                                                             
         DS    CL4                                                              
PESTS    DS    CL20                                                             
         SPACE                                                                  
* CML LIST *                                                                    
         SPACE                                                                  
CMLISTD  DSECT                                                                  
CMLENT   DS    0XL47                                                            
CMLCOMP  DS   0XL34                MGRP, BOTH CMLS, MKT, OPTIONAL CABLE         
CMLCOMPA DS   0XL40                ABOVE + DATES                                
CMLCLI   DS    XL2                 BINARY CLIENT                                
CMLMGR   DS    XL2                                                              
CMLCML   DS    CL12                                                             
CMLCML2  DS    CL12                                                             
CMLCBL   DS    CL1                 CABLE = C, STATION = 0                       
CMLMKT   DS    XL2                                                              
CMLCSTA  DS    XL3                 CABLE STATION IF OPT CABLE                   
         SPACE                                                                  
CMLFTD   DS    XL2                                                              
CMLLTD   DS    XL2                                                              
CMLEST   DS    XL1                                                              
CMLINDX  DS    XL2                 CABLE STATION GROUP INDEX                    
CMLPRD   DS    XL1                 PROD 1                                       
CMLPRD2  DS    XL1                 PROD 2                                       
CMLSLN1  DS    XL1                 LEN 1                                        
CMLSLN2  DS    XL1                 LEN 2                                        
CMLINST  DS    CL1                 P=PATTERN, B=BUYS                            
         DS    XL1                 SPARE                                        
CMLNEXT  EQU   *                                                                
         SPACE                                                                  
* BUY INFO TO TSAR                                                              
         SPACE                                                                  
TSRBUYD  DSECT                                                                  
TSRENT   DS    0XL17               FIXED ENTRY LENGTH                           
TSRKEY   DS    0XL16               EVERYTHING BUT THE FLAG IS THE KEY           
TSRCLI   DS    XL2                 CLIENT                                       
TSRMKT   DS    XL2                 MARKET                                       
TSRPRD   DS    XL1                 PROD 1                                       
TSRPRD2  DS    XL1                 PROD 2                                       
TSRSLN1  DS    XL1                 LEN 1                                        
TSRSLN2  DS    XL1                 LEN 2                                        
TSREST   DS    XL1                                                              
TSRFTD   DS    XL2                                                              
TSRLTD   DS    XL2                                                              
TSRCSTA  DS    XL3                 CABLE STATION IF OPT CABLE                   
TSRFLG   DS    XL1                 FLAG                                         
ENTBUY   EQU   X'80'               ENTIRE BUY COVERED BY RECAP                  
NOBUYFR  EQU   X'40'               NO BUY FOR THIS RECAP                        
*                                                                               
*  FOLLOWING IS VARIABLE PART OF THE ENTRY                                      
TSRRENT  DS    0XL4                ENTRY LEN                                    
TSRRFTD  DS    XL2                 RECAP FTD                                    
TSRRLTD  DS    XL2                 RECAP LTD                                    
         SPACE                                                                  
* CABLE STATION SYSTEM NAME TABLE                                               
         SPACE                                                                  
CBLSTAD  DSECT                                                                  
CBLENT   DS    0XL5                                                             
CBLSTA   DS    XL3                                                              
CBLINDEX DS    CL2                                                              
CBLNEXT  EQU   *                                                                
*                                                                               
* DSECT FOR ACTIVITY LIST ENTRIES *                                             
         SPACE                                                                  
ACLD     DSECT                                                                  
*                                                                               
ACLDATA  DS    0XL18                                                            
ACLEBCP1 DS    CL3                 EBCDIC PRD 1                                 
ACLEBCP2 DS    CL3                 EBCDIC PRD 2                                 
ACLCLI   DS    XL2                                                              
ACLPRD   DS    XL1                                                              
ACLSLN   DS    XL1                                                              
ACLPRD2  DS    XL1                                                              
ACLSLN2  DS    XL1                                                              
ACLCOPY  DS    XL1                                                              
ACLFTD   DS    XL2                                                              
ACLLTD   DS    XL2                                                              
         DS    XL1                 SPARE                                        
*                                                                               
MYDSND   DSECT                                                                  
         DS    CL12                                                             
MYDSNTST DS    CL1                 MYDSN+8,1                                    
MYDSNAGY DS    CL2                 MYDSN+9,2                                    
         DS    CL2                                                              
MYDSNDAT DS    CL6                 MYDSN+13,6                                   
         DS    CL2                                                              
MYDSNT1  DS    CL4                 MYDSN+21,4                                   
         DS    CL2                                                              
MYDSNT2  DS    CL4                 MYDSN+27,4                                   
*                                                                               
BLOCKD   DSECT                                                                  
SOFTDATE DS    V                                                                
SDBLOCK  DS    CL(SOFXTNL)         SOFTDATE BLOCK                               
OUTDATE  DS    CL12                                                             
TODAY0   DS    CL6                 TODAY'S DATE - EBCDIC                        
*                                                                               
       ++INCLUDE DDGETRETD                                                      
       ++INCLUDE FASSBOFF                                                       
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDTWADCOND                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048SPTRA21   10/22/18'                                      
         END                                                                    
