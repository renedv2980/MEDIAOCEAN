*          DATA SET SPSFM34    AT LEVEL 206 AS OF 02/17/21                      
*PHASE T21734A                                                                  
*INCLUDE SCINKEY                                                                
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE: SPSFM34<==>T21734 SPOT MARKET RECORDS MAINTENANCE.          *         
*                                                                     *         
*  COMMENTS: USING SFM TO HANDLE SPOT STATION-FILES.                  *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (T21700), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS: SCREEN SPSFMD2 (T217D2) -- MAINTENANCE                     *         
*                                                                     *         
*  OUTPUTS: UPDATED MARKET RECORDS                                    *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - WORK                                                  *         
*          R7 - SECOND BASE                                           *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM/WORK                                           *         
*          RF - SYSTEM/WORK                                           *         
*                                                                     *         
***********************************************************************         
         TITLE 'SPSFM34<==>T21734 SPOT MARKET RECORDS MAINTENANCE'              
***********************************************************************         
T21734   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21734*,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         MVI   ACTELOPT,C'N'                                                    
*                                                                               
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    MAIN05                                                           
         TM    12(RA),X'80'        CHECK IF AUTHORIZED FOR ADD/DEL/CHA          
         BZ    MAIN05                                                           
         MVI   ERROR,NOTAUTH       NOT AUTHORIZED FOR THIS FUNCTION             
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    NOTAUTHD                                                         
         CLI   ACTNUM,ACTCHA       ACTION CHANGE?                               
         BE    NOTAUTHD                                                         
         CLI   ACTNUM,ACTDEL       ACTION DELETE?                               
         BNE   MAIN05                                                           
*                                                                               
NOTAUTHD MVI   GETMSYS,2           CHANGE TO X'02' ERROR SYSTEM                 
         LA    R2,CONACTH          ACTION ERROR                                 
         B     ERREXGO                                                          
*                                                                               
MAIN05   CLI   SVAPROF+7,C'C'      TEST CANADIAN                                
         BNE   MAIN07                                                           
*                                                                               
         CLC   MKMRST2(3),=C'BBM'  TEST TITLE FIXED                             
         BE    MAIN06                                                           
         MVC   MKMRST2(3),=C'BBM'  FIX RTG SVC ID                               
         OI    MKMRST2H+6,X'80'    AND XMT THE FIELD                            
*                                                                               
MAIN06   CLI   QMED,C'T'           MEDIA T?                                     
         BE    MAIN06A             YES                                          
         XC    MKMRSTX(14),MKMRSTX NO - CLEAR RATING SERVICE                    
         OI    MKMRSTXH+6,X'80'    TRANSMIT                                     
         XC    MKMRSVC,MKMRSVC                                                  
         OI    MKMRSVCH+1,X'20'    PROTECT                                      
         OI    MKMRSVCH+6,X'80'    TRANSMIT                                     
         B     MAIN07                                                           
*                                                                               
MAIN06A  CLC   MKMRSTX(14),=C'Rating Service'                                   
         BE    MAIN07                                                           
         MVC   MKMRSTX(14),=C'Rating Service'                                   
         OI    MKMRSTXH+6,X'80'                                                 
         NI    MKMRSVCH+1,X'FF'-X'20'                                           
         OI    MKMRSVCH+6,X'80'                                                 
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK+16(4),=C'S000'                                              
         MVC   WORK+20(2),AGENCY    READ AGENCY LEVEL                           
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK+16,WORK,DATAMGR                                   
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
         MVC   SOFTDEM,WORK+15  SAVE USER DEMO OPTION                           
*                                                                               
MAIN07   CLI   MODE,SETFILE        SET FILE                                     
         BNE   MAIN10                                                           
         BAS   RE,SF                                                            
         B     EXIT                                                             
*                                                                               
MAIN10   CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,XRECPUT        AFTER RECORD HAS BEEN PLACED BACK.           
         BE    ZRP                                                              
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,RECDEL         DELETE RECORD.                               
         BE    DELR                                                             
         CLI   MODE,XRECREST       RESTORE RECORD.                              
         BE    RSTR                                                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*========================== SET FILE ROUTINE =========================*         
SF       DS    0H                                                               
         MVI   ACTELOPT,C'N'                                                    
         MVI   USEIO,C'Y'                                                       
*                                                                               
         MVC   SYSDIR,=C'STATION '     SET FILENAME.                            
         MVC   SYSFIL,=C'STATION '                                              
         SR    R1,R1                                                            
         LA    R1,MKTKEYLQ                                                      
         STH   R1,LKEY                                                          
*                                                                               
         BR    RE                                                               
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================== VALIDATE KEY ROUTINE =======================*         
*                                                                               
VK       DS    0H                                                               
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   LKEY,=H'13'         L(AGENCY RECORD)=13.                         
         MVI   USEIO,C'N'                                                       
*                                                                               
*--------------------------- MEDIA FIELD -----------------------------*         
*                                                                               
         LA    R2,MKMMEDIH         CHECK FOR ANY MEDIA INPUT.                   
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         CLI   SVAPROF+7,C'C'      FOR CANADIAN AGENCIES, DISALLOW              
         BNE   VK03                 MEDIAS 'C' & 'N'.                           
         MVI   ERROR,INVMED                                                     
         CLI   QMED,C'C'                                                        
         BE    ERREXGO                                                          
         CLI   QMED,C'N'                                                        
         BE    ERREXGO                                                          
*                                                                               
VK03     CLI   QMED,C'R'           MEDIA R?                                     
         BNE   VK06                NO, SHOW CABLE DEMOS FIELD                   
*                                                                               
         OI    MKMCABH+1,X'0C'       LOW INTENSITY                              
         OI    MKMCABH+6,X'80'       AND XMT THE FIELD                          
*                                                                               
         OI    MKMCDEMH+1,X'20'      PROTECT                                    
         XC    MKMCDEM(L'MKMCDEM),MKMCDEM                                       
         MVI   MKMCDEMH+5,0          NO INPUT                                   
         OI    MKMCDEMH+6,X'80'      AND XMT THE FIELD                          
         B     VK09                                                             
*                                                                               
VK06     NI    MKMCABH+1,X'FF'-X'0C' TURN OFF LOW INTENSITY                     
         OI    MKMCABH+6,X'80'       AND XMT THE FIELD                          
*                                                                               
         NI    MKMCDEMH+1,X'FF'-X'20' UNPROTECT                                 
         OI    MKMCDEMH+6,X'80'      AND XMT THE FIELD                          
*                                                                               
VK09     BAS   RE,TSTCOMS          TEST COMSCORE                                
*                                                                               
*-------------------------- MARKET FIELD -----------------------------*         
*                                                                               
VK10     LA    R2,MKMMRKTH          CHECK FOR ANY MARKET INPUT.                 
         GOTO1 ANY                                                              
*                                                                               
** VALIDATE SYNTAX OF MARKET CODE                                               
*                                                                               
         MVI   ERROR,NOTNUM        ASSUME INVALID NUMERICS.                     
         TM    4(R2),X'08'         TEST FOR VALID NUMERICS.                     
         BZ    ERREXGO                                                          
         MVI   ERROR,INVALID       ASSUME MARKET IS INVALID.                    
         CLI   5(R2),0             CHECK AGAIN FOR NO-INPUT.                    
         BE    ERREXGO                                                          
         CLI   5(R2),4             MAX OF 4 DIGITS IN MARKET CODE.              
         BH    ERREXGO                                                          
*                                                                               
** PUT MARKET CODE INTO QMKT                                                    
*                                                                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
         EDIT  (R0),(4,QMKT),FILL=0     RIGHT-PAD MKTCODE W/. ZEROES,           
         MVC   8(4,R2),QMKT              IF NECESSARY.                          
         OI    6(R2),X'80'                                                      
*                                                                               
*---------------------------- BUILD KEY ------------------------------*         
*                                                                               
         MVC   MKTCODE,QMKT        BUILD 'L'-KEY USES MKTCODE FIELD.            
         XC    KEY,KEY                                                          
         MVC   KEY,ZEROES                                                       
         LA    R4,KEY                                                           
         USING MKTRECD,R4                                                       
         MVI   MKTKTYPE,MKTKTYPQ   MARKET RECORDS ARE TYPE-'M'.                 
         MVC   MKTKMED,QMED        MEDIA.                                       
         MVC   MKTKMKT,QMKT        MARKET CODE.                                 
         MVC   MKTKAGY,AGENCY      AGENCY.                                      
         DROP  R4                                                               
*                                                                               
XVK      BAS   RE,SF               DO SET-FILES ROUTINE.                        
         XC    MYLSVKEY,MYLSVKEY   WE'RE NOT COMING IN FROM LISTING.            
         B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*====================== VALIDATE RECORD ROUTINE ======================*         
*                                                                               
VR       DS    0H                                                               
*                                                                               
         L     R6,AIO              YES, AND REREAD MKT RCRD INTO I/O.           
         CLI   0(R6),C'M'          IS MARKET RECORD IN I/O?                     
         BE    VR005                YEP, GO ON W/. VALREC.                      
*                                                                               
         USING ANMRECD,R6           NOPE, READ MKT RECORD INTO I/O.             
         MVC   KEY,ZEROES          GET SET TO BUILD MARKET-KEY.                 
*                                                                               
         LA    R4,KEY              BUILD MARKET-KEY HERE.                       
         USING MKTRECD,R4                                                       
         MVI   MKTKTYPE,MKTKTYPQ   MARKET RECORDS ARE TYPE-'M'.                 
         MVC   MKTKMED,ANMKMED     MEDIA.                                       
         MVC   MKTKMKT,ANMKNMRK    NUMERIC MARKET CODE.                         
         MVC   MKTKAGY,ANMKAGCY    AGENCY.                                      
         DROP  R4,R6                                                            
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(MKTKEYLQ),0(R6) R6=AIO-->MARKET RECORD.                      
         JNE   *+2                 MARKET-RECORD HAD BETTER BE FOUND.           
*                                                                               
VR005    XC    PRIMARK,PRIMARK                                                  
         CLI   ACTNUM,ACTADD                                                    
         BE    VR010                                                            
         L     R4,AIO                                                           
         USING MKTRECD,R4                                                       
         MVC   PRIMARK,MKTALST     PRIMARK=PRIMARY ALPHA MKT.                   
         DROP  R4                                                               
         OC    PRIMARK,PRIMARK                                                  
         BZ    VR010                                                            
         BAS   RE,LTEST                                                         
*                                                                               
*----------------------- VALIDATE MARKET NAME ------------------------*         
*                                                                               
VR010    LA    R2,MKMNAMEH                                                      
         GOTO1 ANY                                                              
         MVC   NAME,WORK                                                        
*                                                                               
*------------------------ VALIDATE TIME ZONE -------------------------*         
*                                                                               
         MVI   ZONE,0                                                           
         CLI   MKMTZONH+5,0                                                     
         BE    VR020                                                            
         MVC   ZONE,MKMTZON                                                     
*                                                                               
*--------------------------- VALIDATE RANK ---------------------------*         
*                                                                               
VR020    LA    R2,MKMRANKH                                                      
         XC    RANK,RANK                                                        
         CLI   5(R2),0                                                          
         BNE   VR020A                                                           
         CLC   AGENCY,=C'BO'                                                    
         BNE   VR030                                                            
         B     MISSERR                                                          
*                                                                               
VR020A   BAS   RE,PACKNUM                                                       
         CP    DUB,=P'0'           IF PACKNUM RETURNS WITH DUB=0,               
         BE    ERREXGO              THERE IS A MISTAKE SOMEWHERE.               
         OI    DUB+7,X'0F'                                                      
         UNPK  RANK,DUB                                                         
*                                                                               
*--------------------------- VALIDATE HOMES --------------------------*         
*                                                                               
VR030    LA    R2,MKMHOMSH                                                      
         XC    HOMES,HOMES                                                      
         CLI   5(R2),0                                                          
         BE    VR040                                                            
         BAS   RE,PACKNUM                                                       
         CP    DUB,=P'0'           IF PACKNUM RETURNS WITH DUB=0,               
         BE    ERREXGO              THERE IS A MISTAKE SOMEWHERE.               
         OI    DUB+7,X'0F'         GET SET TO UNPACK NUMBER.                    
         UNPK  HOMES,DUB                                                        
*                                                                               
*--------------------------- VALIDATE REGION -------------------------*         
*                                                                               
VR040    DS    0H                  REGION CODE GOES HERE.                       
*                                                                               
*----------------------------- VALIDATE NTA --------------------------*         
*                                                                               
VR050    LA    R2,MKMNTAH                                                       
         XC    NTA,NTA                                                          
         CLI   5(R2),0                                                          
         BNE   VR050A                                                           
         CLC   AGENCY,=C'BO'                                                    
         BNE   VR060                                                            
         B     MISSERR                                                          
*                                                                               
VR050A   BAS   RE,PACKNUM                                                       
         CP    DUB,=P'0'           IF PACKNUM RETURNS WITH DUB=0,               
         BE    ERREXGO              THERE IS A MISTAKE SOMEWHERE.               
         CP    DUB,=P'29'                                                       
         BH    INVLERR                                                          
         OI    DUB+7,X'0F'                                                      
         UNPK  NTA,DUB                                                          
*                                                                               
*---------------------- VALIDATE WEIGHTING FACTOR --------------------*         
*                                                                               
VR060    LA    R2,MKMWTH                                                        
         XC    WEIGHT,WEIGHT                                                    
         CLI   5(R2),0                                                          
         BE    VR070                                                            
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(2,MKMWT),(R3)                                      
         CLI   DMCB,X'FF'                                                       
         BE    INVLERR                                                          
         L     R3,DMCB+4                                                        
         CVD   R3,DUB                                                           
         CP    DUB,=P'9999'        MAX IS 99.99                                 
         BH    WTERR1                                                           
         CP    DUB,=P'0'                                                        
         BE    INVLERR                                                          
         OI    DUB+7,X'0F'                                                      
         UNPK  WEIGHT,DUB                                                       
*                                                                               
*------------------- VALIDATE COVERAGE PERCENTAGE --------------------*         
*                                                                               
VR070    LA    R2,MKMSHRH                                                       
         XC    SHR,SHR                                                          
         CLI   5(R2),0                                                          
         BE    VR080                                                            
         ZIC   R3,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(2,MKMSHR),(R3)                                     
         CLI   DMCB,X'FF'                                                       
         BE    INVLERR                                                          
         L     R3,DMCB+4                                                        
         CVD   R3,DUB                                                           
         CP    DUB,=P'0'                                                        
         BNH   INVLERR                                                          
         CP    DUB,=P'9999'        MAX IS 99.99                                 
         BH    INVLERR                                                          
         OI    DUB+7,X'0F'                                                      
         UNPK  SHR,DUB                                                          
*                                                                               
*--------------------- VALIDATE RATING SERVICE 1 ---------------------*         
*                                                                               
VR080    XC    RS1(3),RS1          CLEAR RATING SERVICE AND MKT                 
         MVI   CLAS1,0              AND SWEEP CLASS.                            
         MVI   RS1,C'0'            FORCE RTG SVC = NSI.                         
*                                                                               
         LA    R2,MKMRSM1H         RATING SERVICE MARKET.                       
         CLI   5(R2),0                                                          
         BE    VR085                                                            
*                                                                               
         BAS   RE,PACKNUM          EDIT MARKET NUMBER.                          
         STH   R0,HALF                                                          
         MVC   RSM1,HALF                                                        
*                                                                               
VR085    LA    R2,MKMSCL1H         EDIT SWEEP CLASS.                            
         CLI   5(R2),0                                                          
         BE    VR090                                                            
         TM    4(R2),X'08'         CHECK FOR VALID NUMERICS.                    
         BZ    NUMBERR                                                          
         MVC   CLAS1,8(R2)                                                      
*                                                                               
*--------------------- VALIDATE RATING SERVICE 2 ---------------------*         
*                                                                               
VR090    XC    RS2(3),RS2          CLEAR RATING SERVICE AND MKT                 
         MVI   CLAS2,0              AND SWEEP CLASS.                            
         MVI   RS2,C'1'            FORCE RTG SVC = ARB/BBM.                     
*                                                                               
         LA    R2,MKMRSM2H         RATING SERVICE MARKET.                       
         CLI   5(R2),0                                                          
         BE    VR095                                                            
*                                                                               
         TM    4(R2),X'08'         TEST FOR VALID NUMERICS.                     
         BZ    NUMBERR                                                          
         BAS   RE,PACKNUM          EDIT MARKET NUMBER.                          
         STH   R0,HALF                                                          
*                                                                               
         CLI   SVAPROF,C'1'        TEST ARB AGENCY                              
         BE    VR094                                                            
         CLI   SVAPROF,C'2'        OR BOTH                                      
         BE    VR094                                                            
         OC    HALF,HALF                                                        
         BNZ   ARBERR                                                           
*                                                                               
VR094    MVC   RSM2,HALF                                                        
*                                                                               
VR095    LA    R2,MKMSCL2H         EDIT SWEEP CLASS.                            
         CLI   5(R2),0                                                          
         BE    VR100                                                            
         TM    4(R2),X'08'         CHECK FOR VALID NUMERICS.                    
         BZ    NUMBERR                                                          
*                                                                               
         CLI   SVAPROF,C'1'        TEST ARB AGENCY                              
         BE    VR099                                                            
         CLI   SVAPROF,C'2'        OR BOTH                                      
         BE    VR099                                                            
         CLI   8(R2),X'F0'                                                      
         BH    ARBERR                                                           
*                                                                               
VR099    MVC   CLAS2,8(R2)                                                      
*                                                                               
*                                                                               
*-------------------- VALIDATE LIMIT ACCESS CODES --------------------*         
*                                                                               
VR100    LA    R2,MKMLOCKH                                                      
         XC    LTACC,LTACC         CLEAR RATING SERVICE AND MKT                 
         CLI   5(R2),0                                                          
         BE    VR105                                                            
         CLI   5(R2),3                                                          
         BH    LAERR1                                                           
*                                                                               
         MVC   LTACC,8(R2)                                                      
         LA    RE,LTACC                                                         
         LA    RF,3                                                             
VR100A   CLI   0(RE),0                                                          
         BE    VR100B                                                           
         CLI   0(RE),C'A'                                                       
         BL    LAERR2                                                           
         CLI   0(RE),C'9'                                                       
         BH    LAERR2                                                           
VR100B   LA    RE,1(RE)                                                         
         BCT   RF,VR100A                                                        
***********************************************************************         
* VALIDATE COMSCORE MARKET CODE                                                 
***********************************************************************         
VR105    TM    USRIDFLG,USRRNTKQ   USER HAS ACCESS TO COMSCORE DATA?            
         JZ    VR110               NO                                           
         CLI   QMED,C'T'           AND IT IS MEDIA T?                           
         JNE   VR110               NO                                           
*                                                                               
*                                                                               
         LA    R2,MKMCSMKH                                                      
         XC    CSMKT,CSMKT         CLEAR COMSCORE MARKET NUMBER                 
         CLI   5(R2),0                                                          
         JE    VR105A                                                           
         TM    4(R2),X'08'         TEST FOR VALID NUMERIC                       
         JZ    NUMBERR                                                          
*                                                                               
         LLC   R1,5(R2)            GET BINARY VALUE OF COMSCORE MKT             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         J     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,CSMKT                                                       
*                                                                               
VR105A   BRAS  RE,CKCSMKTS         CHECK AGAINST THE LIST OF COMSCORE           
         JE    VR110                   MARKETS                                  
         MVI   ERROR,INVALID                                                    
         J     ERREXGO                                                          
*                                                                               
*------------------ VALIDATE DAYLIGHT SAVINGS TIME ------------------*          
*                                                                               
*VR100    LA    R2,MKMDSTH         CODE COMMENTED OUT ON 12/24/92               
*****     MVI   NODST,0                                                         
*****     CLI   5(R2),0                                                         
*****     BE    VR110                                                           
*****     MVC   NODST,8(R2)                                                     
*****     CLI   NODST,C'Y'          IF DAYLIGHT SAVINGS TIME IS NOT             
*****     BNE   DSERR1               WANTED, JUST LEAVE FIELD BLANK.            
*                                                                               
*---------------------- VALIDATE ALPHA MARKETS ----------------------*          
*                                                                               
VR110    LA    R2,MKMMKTSH                                                      
         XC    ALST,ALST                                                        
         MVI   UPDTSD,C'N'                                                      
         CLI   5(R2),0                                                          
         BE    VR110A                                                           
         BAS   RE,VALMRKT                                                       
         BZ    ERREXGO                                                          
*                                                                               
* NOW CANADA USES ALPHAMKT FOR SOFT DEMO LOOKUP, THE MARKETS THIS               
* POINTS TO MUST MATCH ANY MARKET NUMBERS PREVIOUSLY INPUT FOR 'SPILL'          
*                                                                               
VR110A   CLI   SVAPROF+7,C'C'      CANADIAN TV ONLY                             
         BNE   VR120                                                            
         CLI   QMED,C'T'                                                        
         BNE   VR120                                                            
*                                                                               
         MVI   RSVC,0                                                           
         LA    R2,MKMRSVCH         RATING SERVICE                               
         CLI   MKMMKTSH+5,0        DO WE HAVE AN ALPHA MARKET?                  
         BNE   VR110B              YES                                          
         CLI   5(R2),0             HAVE A RATING SERVICE?                       
         BE    VR110D              NO                                           
         B     RTGERR2             YES - ERROR, WE ALSO NEED ALPHA MKT          
*                                                                               
VR110B   CLC   =C'***',MKMMKTS     HAVE *** AS ALPHA MARKET?                    
         BNE   VR110B1             NO                                           
         XC    8(3,R2),8(R2)       YES - CLEAR RATING SERVICE                   
         OI    6(R2),X'80'         AND XMIT                                     
         B     VR110D                                                           
*                                                                               
VR110B1  CLI   5(R2),0             HAVE A RATING SERVICE?                       
         BNE   VR110C              YES                                          
         CLI   SOFTDEM,C'Y'        00 PROFILE REQUIRE ALPHA MARKET?             
         BNE   VR110D              NO, WE DONT REQUIRE RATING SERVICE           
         B     RTGERR3             ERROR, 00 PROF REQUIRES RATING SVC           
*                                                                               
VR110C   MVI   RSVC,C'0'                                                        
*                                                                               
         CLC   8(3,R2),=C'NSI'                                                  
         BE    VR110D                                                           
         CLC   8(3,R2),=C'BBM'                                                  
         BNE   RTGERR                                                           
         MVI   RSVC,C'1'                                                        
*                                                                               
VR110D   MVI   BYTE,C'N'                                                        
         BAS   RE,GETRSMKT         NSI MKT FOR ALPHA FROM DEMFILES              
         OC    RSM1,RSM1                                                        
         BZ    *+14                NO NSI MKT# INPUT                            
         CLC   RSM1,HALF           CHECK NSI MKT NUM = ALPHA'S                  
         BNE   ERREXGO                                                          
         MVC   RSM1,HALF           SET NSI MKT NUM FROM ALPHA'S                 
*                                                                               
         MVI   BYTE,C'A'                                                        
         BAS   RE,GETRSMKT         NSI MKT FOR ALPHA FROM DEMFILES              
         OC    RSM2,RSM2                                                        
         BZ    *+14                NO BBM MKT# INPUT                            
         CLC   RSM2,HALF           CHECK BBM MKT NUM = ALPHA'S                  
         BNE   ERREXGO                                                          
         MVC   RSM2,HALF           SET BBM MKT NUM FROM ALPHA'S                 
*                                                                               
*---------------------- VALIDATE UNITS ------------------------------*          
*                                                                               
VR120    LA    R2,MKMUNITH                                                      
         XC    UNIT,UNIT                                                        
         ZICM  R1,5(R2)                                                         
         BZ    VR130                                                            
         TM    4(R2),X'08'                                                      
         BNO   NUMBERR                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,UNIT                                                        
         SPACE 1                                                                
*                                                                               
*------------------  VALIDATE LMP START DATE -------------------------*         
*                                                                               
VR130    LA    R2,MKMSDATH                                                      
         XC    LPMDATE,LPMDATE                                                  
         CLI   5(R2),0                                                          
         BE    VR140                                                            
         GOTO1 DATVAL,DMCB,(2,8(R2)),DATETEMP                                   
         CLC   DATETEMP,=C'000000'                                              
         BE    INVLERR                                                          
*                                                                               
         MVC   DMCB+4(4),=X'D9000A1D'                                           
         GOTO1 CALLOV,DMCB           CALL GETBRD                                
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(1,DATETEMP),WORK,GETDAY,ADDAY                         
         CLI   DMCB,X'FF'           ERROR FROM GETBROAD                         
         BE    INVLERR                                                          
         GOTO1 DATCON,DMCB,(0,WORK+6),(2,LPMDATE)                               
         XR    R1,R1                                                            
         ICM   R1,1,MKMMKTSH+5      NEED ALPHA MKT FOR LPM START DATE           
         BZ    LPMERR                                                           
         BCTR  R1,0                                                             
*                                                                               
         LA    R3,LPMTAB                                                        
*                                                                               
VR130A   CLI   0(R3),X'FF'          END OF TABLE?                               
         BE    VR140                YES                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   MKMMKTS(0),0(R3)     MATCH ON ALPHA MARKET?                      
         BE    VR130B               YES                                         
         LA    R3,5(R3)             BUMP                                        
         B     VR130A               CHECK NEXT TABLE ENTRY                      
*                                                                               
VR130B   CLC   LPMDATE,3(R3)        LPM DATE ON/AFTER DATE ALLOWED?             
         BL    LPMERRDT             NO, ERROR                                   
*                                                                               
*--------------------- VALIDATE CABLE DEMOS --------------------------*         
*                                                                               
VR140    LA    R2,MKMCDEMH                                                      
         MVI   CABLEDEM,0                                                       
         CLI   5(R2),0              FUSION/NSI INPUT?                           
         BE    VR150                NO                                          
*                                                                               
         CLI   8(R2),C'N'           NSI?                                        
         BNE   VR140A               NO                                          
         MVI   CABLEDEM,C'N'                                                    
         B     VR150                                                            
*                                                                               
VR140A   CLI   8(R2),C'F'           FUSION?                                     
         BNE   VR140B               NO                                          
         MVI   CABLEDEM,C'F'                                                    
         B     VR150                                                            
*                                                                               
VR140B   CLI   8(R2),C'0'           FUSION?                                     
         BNE   INVLERR              NO, ERROR                                   
         MVI   CABLEDEM,C'0'                                                    
*                                                                               
*------------------- VALIDATE TALENT MARKET --------------------------*         
*                                                                               
VR150    LA    R2,MKMTALMH         TALENT MARKET                                
         XC    TALM,TALM                                                        
         CLI   5(R2),0             DO WE HAVE A TALENT MARKET?                  
         BE    VR160               NO                                           
         BRAS  RE,VTALMKT          VALIDATE AGAINST TALENT SYSTEM               
         MVC   TALM,8(R2)                                                       
*                                                                               
*--------------------- VALIDATE BOOKTYPE -----------------------------*         
*                                                                               
VR160    LA    R2,MKMBTYPH         BOOKTYPE                                     
         MVI   BOOKTYP,X'00'                                                    
         CLI   5(R2),0                                                          
         BE    VR170                                                            
         OC    MKMBTYP,SPACES                                                   
*                                                                               
VR160A   L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB  GET A(BOOK TABLE)                            
         ICM   RF,15,0(R1)         A(TABLE)RETURNED IN P1                       
         JZ    *+2                                                              
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
*                                                                               
VR160B   CLI   0(RF),X'FF'                                                      
         BNE   *+12                                                             
         MVI   ERROR,BOOKERR                                                    
         B     INVLERR                                                          
         CLC   SPBKTYPA,MKMBTYP                                                 
         BE    *+10                                                             
         AR    RF,R0                                                            
         B     VR160B                                                           
         MVC   BOOKTYP,SPBKTYPN                                                 
         DROP  RF                                                               
         EJECT                                                                  
*                                                                               
*----------------- VALIDATE COMSCORE BOOKTYPE  ------------------------         
*                                                                               
VR170    LA    R2,MKMCBTYH         COMSCORE BOOKTYPE                            
         MVI   CBOKTYP,X'00'                                                    
         CLI   5(R2),0                                                          
         BE    VR200                                                            
         CLI   5(R2),1             CURRENT OPTION IS 'L' FOR LIVE               
         BNE   INVLERR                                                          
         CLI   8(R2),C'L'                                                       
         BNE   INVLERR                                                          
         MVC   CBOKTYP,8(R2)                                                    
         EJECT                                                                  
*                                                                               
*----------------------- PROCESSING 'L'-RECORDS ----------------------*         
*                                                                               
** ACTION = ADD                                                                 
*                                                                               
VR200    CLI   ACTNUM,ACTADD                                                    
         BNE   VR210                                                            
         OC    ALST,ALST           ANY ALPHA MARKETS?                           
         BZ    VR240               NO, EXIT 'L'-RECORD PROCESSING.              
*                                                                               
** ADD/YES PRIMARK                                                              
*                                                                               
         BAS   RE,BLDANKEY         BUILD KEY OF 'L'-RECORD.                     
         USING ANMRECD,R1          R1-->ANMKEY.                                 
         MVC   ANMKAMRK,ALST       PUT 1ST ALPHA MKT INTO KEY.                  
         DROP  R1                                                               
         L     R3,AIO2                                                          
         XC    0(256,R3),0(R3)                   CLEAR I/O2.                    
         MVC   0(ANMKEYLQ,R3),MYKEY              MOVE KEY INTO RECORD.          
         MVC   (ANMRCLEN-ANMRECD)(2,R3),=H'20'   L'RECORD=20.                   
         MVC   COMMAND,=C'DMADD   '                                             
         BAS   RE,VR230                                                         
*                                                                               
         CLI   QMED,C'T'           IF MEDIA 'T', THEN WORRY ABOUT               
         BNE   VR240                CANADIAN AGENCIES.                          
*                                                                               
         CLI   SVAPROF+7,C'C'      CHECK FOR CANADIAN AGENCY.                   
         BNE   VR240                                                            
         BAS   RE,MKCANADC         MAKE A MEDIA 'C' KEY.                        
         BAS   RE,VR230                                                         
         BAS   RE,MKCANADN         MAKE A MEDIA 'N' KEY.                        
         BAS   RE,VR230                                                         
*                                                                               
         MVC   MYKEY+(ANMKMED-ANMKEYD)(1),QMED   ORIGINAL MEDIA.                
         B     VR240                                                            
*                                                                               
** ACTION <> ADD                                                                
*                                                                               
VR210    CLC   PRIMARK,ALST        DID PRIMARY ALPHA MARKET CHANGE?             
         BE    VR240               NO, EXIT PROCESSING.                         
         BAS   RE,BLDANKEY         YES, PREPARE TO DELETE THE OLD               
*                                   'L'-RECORD OF MARKET.                       
         L     R3,AIO2                                                          
         OC    PRIMARK,PRIMARK     CHECK IF PRIMARK IS NULLS.  IF YES,          
         BZ    VR220                THEN THERE IS NOTHING TO DELETE.            
         USING ANMRECD,R1          R1-->MYKEY.                                  
         MVC   ANMKAMRK,PRIMARK    PUT OLD ALPHA MKT INTO KEY.                  
         DROP  R1                                                               
*                                                                               
         MVC   MEDIA(1),QMED                                                    
         BAS   RE,VRLSTEP1         DELETE FOR THAT MEDIA IN QMED.               
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   VR220               NOPE, DO NEXT THING.                         
*                                                                               
         CLI   QMED,C'T'           WORRY ABOUT MEDIAS 'C' & 'N'?                
         BNE   VR220                NOPE, DO NEXT THING.                        
         MVI   MEDIA,C'C'          MEDIA 'C' FOR CANADIAN AGENCY.               
         BAS   RE,VRLSTEP1         DELETE IT.                                   
         MVI   MEDIA,C'N'          MEDIA 'N' FOR CANADIAN AGENCY.               
         BAS   RE,VRLSTEP1         DELETE IT.                                   
*                                                                               
         MVC   MYKEY+(ANMKMED-ANMKEYD)(1),QMED   ORIGINAL MEDIA.                
*                                                                               
** GOTTEN RID OF OLD RECORDS W/. PRIMARK                                        
*                                                                               
VR220    OC    ALST,ALST           ANY ALPHA MARKETS?                           
         BZ    VR240               NO, DON'T CREATE 'L'-RECORD.                 
         LA    R1,MYKEY            BUILD THE NEW 'L'-RECORD TO BE               
         USING ANMRECD,R1           CONSISTENT W/. UPDATED                      
         MVC   ANMKAMRK,ALST        MARKET-RECORD.                              
         DROP  R1                                                               
*                                                                               
         MVC   MEDIA(1),QMED       MEDIA = 'T' OR 'R'.                          
         BAS   RE,VRLSTEP2         UPDATES FILE W/. NEW 'L'-RECORD.             
         CLI   SVAPROF+7,C'C'      TEST FOR CANADIAN AGENCY.                    
         BNE   VR240                                                            
         CLI   QMED,C'T'           WORRY ABOUT MEDIA 'C' AND 'N'?               
         BNE   VR240                NOPE, NO NEED TO.                           
         MVI   MEDIA,C'C'          MEDIA 'C' (CANADIAN AGENCY).                 
         BAS   RE,VRLSTEP2                                                      
         MVI   MEDIA,C'N'                                                       
         BAS   RE,VRLSTEP2                                                      
*                                                                               
         MVC   MYKEY+(ANMKMED-ANMKEYD)(1),QMED   ORIGINAL MEDIA.                
         B     VR240                                                            
         SPACE 4                                                                
VR230    NTR1                                                                   
         GOTO1 DATAMGR,DMCB,COMMAND,=C'STATION ',MYKEY,(R3)                     
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*---------------------------------------------------------------------*         
*                                                                               
VR240    LA    R2,ALST                                                          
         ST    R2,AALST                                                         
         BAS   RE,DRAMKT           DISPLAY ALPHA-MARKETS.                       
*                                                                               
*-------------------------- BUILD RECORD -----------------------------*         
*                                                                               
         L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         XC    SVALST,SVALST                                                    
         MVI   SVRSVC,0                                                         
         USING MKTRECD,R4                                                       
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    VR250               YES                                          
         MVC   SVALST,MKTALST      SAVE THE RECORDS ALPHA MARKET                
         MVC   SVRSVC,MKTRSVC      SAVE THE RECORDS RATING SERVICE              
*                                                                               
VR250    XC    0(256,R4),0(R4)                                                  
*                                                                               
         MVC   MKTKEY(MKTKEYLQ),KEY  KEY OF RECORD.                             
         MVC   MKTRECL,=H'144'       L(MARKET RECORD).                          
         MVC   MKTNAME,NAME        MARKET NAME.                                 
         MVC   MKTZONE,ZONE        TIME ZONE.                                   
         MVC   MKTCLASS,CLASS      SWEEP CODE.                                  
         MVC   MKTRTG,RTG          RATING SERVICE.                              
         MVC   MKTRANK,RANK        MARKET RANK.                                 
         MVC   MKTHOMES,HOMES      MARKET HOMES.                                
         MVC   MKTREG,REG          REGION CODE.                                 
         MVC   MKTNTA,NTA          NTA                                          
         MVC   MKTWT,WEIGHT        MARKET WEIGHTING FACTOR.                     
         MVC   MKTSHR,SHR          MARKET COVERAGE %.                           
         MVC   MKTRS1,RS1          RATING SERVICE.                              
         MVC   MKTRSM1,RSM1        RATING SERVICE MARKET.                       
         MVC   MKTRS2,RS2          RATING SERVICE.                              
         MVC   MKTRSM2,RSM2        RATING SERVICE MARKET.                       
         MVC   MKTLTACC,LTACC      LIMIT ACCESS CODES.                          
         MVC   MKTCSMKN,CSMKT      COMSCORE MARKET.                             
         MVC   MKTCLAS1,CLAS1      SWEEP CLASS FOR RATING SRVC 1.               
         MVC   MKTCLAS2,CLAS2      SWEEP CLASS FOR RATING SRVC 2.               
*****    MVC   MKTNODST,NODST      NO DAYLIGHT SAVINGS TIME?                    
         MVC   MKTALST,ALST        ALPHA MARKETS.                               
         MVC   MKTUNIT,UNIT        UNIT                                         
         MVC   MKTLPMDT,LPMDATE                                                 
         MVC   MKTCDEM,CABLEDEM    NSI=N, FUSION=F, NO DEMOS=C'0'               
         MVC   MKTTALM,TALM        TALENT MARKET                                
         MVC   MKTBKTYP,BOOKTYP    BOOKTYPE                                     
         MVC   MKTCSBTY,CBOKTYP    COMSCORE BOOKTYPE                            
*                                                                               
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   VR260               NO                                           
         CLI   QMED,C'T'           T.V.?                                        
         BNE   VR260               NO                                           
         MVC   MKTRSVC,RSVC        RATING SERVICE                               
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    VR260               YES, SPILLDEF CAN'T EXIST FOR MKT            
*                                                                               
         BAS   RE,SPILLDEF         SCAN SPILLDEF AND APPLY CHANGES              
*                                                                               
         MVC   AIO,AIO2            DON'T KILL I/O BUFFER W/. CHANGES.           
         BAS   RE,SF               DO SET-FILES ROUTINE.                        
         GOTO1 READ                RSTRE DTMGR'S RD (KEY HASN'T CHANGE)         
         MVC   AIO,AIO1            WRITE DATA IN I/O#1 TO FILE.                 
*                                                                               
VR260    CLI   ACTNUM,ACTADD       NO NEED TO RESTORE DATAMGR'S                 
         BE    XVR10                READ WHEN ADDING.                           
*                                                                               
         GOTO1 CNRECS,DMCB,C'C'    CHANGES FOR CANADIAN AGENCY.                 
         MVC   AIO,AIO2            DON'T KILL I/O BUFFER W/. CHANGES.           
         BAS   RE,SF               DO SET-FILES ROUTINE.                        
         GOTO1 READ                RSTRE DTMGR'S RD (KEY HASN'T CHANGE)         
         MVC   AIO,AIO1            WRITE DATA IN I/O#1 TO FILE.                 
         B     XVR20                                                            
*                                                                               
         DROP  R4                                                               
*                                                                               
XVR10    GOTO1 CNRECS,DMCB,C'A'    ADD FOR CANADIAN AGENCY.                     
*                                                                               
XVR20    BAS   RE,REQREC                                                        
*                                                                               
XVR      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*==================== PROCEDURES FOR 'L' RECORDS ====================*          
*                                                                               
*---------------------- BUILD 'L'-RECORD'S KEY ----------------------*          
*                                                                               
BLDANKEY DS    0H                                                               
         MVC   MYKEY,ZEROES                                                     
         LA    R1,MYKEY                                                         
         USING ANMRECD,R1                                                       
         MVI   ANMKTYPE,ANMKTYPQ   TYPE-'L'.                                    
         MVC   ANMKAGCY,AGENCY     AGENCY.                                      
         MVC   ANMKMED,QMED        MEDIA.                                       
         MVC   ANMKNMRK,MKTCODE    NUMERIC MARKET.                              
         DROP  R1                                                               
         BR    RE                  RETURN.                                      
         SPACE 2                                                                
*                                                                               
*----------------------------- MKCANADC ------------------------------*         
*                                                                               
MKCANADC DS    0H                                                               
*         MYKEY = KEY OF 'L'-RECORD.                                            
*         R3-->'L'-RECORD.                                                      
*                                                                               
         MVI   MYKEY+(ANMKMED-ANMKEYD),C'C'                                     
         MVI   (ANMKMED-ANMKEYD)(R3),C'C'                                       
*                                                                               
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
*----------------------------- MKCANADN ------------------------------*         
*                                                                               
MKCANADN DS    0H                                                               
*         MYKEY = KEY OF 'L'-RECORD.                                            
*         R3-->'L'-RECORD.                                                      
*                                                                               
         MVI   MYKEY+(ANMKMED-ANMKEYD),C'N'                                     
         MVI   (ANMKMED-ANMKEYD)(R3),C'N'                                       
*                                                                               
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* test if comscore - display fields                                             
*                                                                               
TSTCOMS  NTR1                                                                   
         TM    USRIDFLG,USRRNTKQ   USER HAS ACCESS TO COMSCORE DATA?            
         JZ    *+12                NO                                           
         CLI   QMED,C'T'           MEDIA T?                                     
         JE    TCOMS10             YES, SHOW COMSCORE?                          
*                                                                               
         OI    MKMCSMTH+1,X'0C'    NO, DON'T SHOW COMSCORE MARKET LABEL         
         OI    MKMCSMTH+6,X'80'       LOW INTENSITY AND TRANSMIT                
         XC    MKMCSMK,MKMCSMK        CLEAR COMSCORE MARKET                     
         OI    MKMCSMKH+1,X'20'       PROTECTED FIELD                           
         OI    MKMCSMKH+6,X'80'       TRANSMIT                                  
*                                                                               
         OI    MKMCBTXH+1,X'0C'       DON'T SHOW COMSCORE BKTYP LABEL           
         OI    MKMCBTXH+6,X'80'       LOW INTENSITY AND TRANSMIT                
         XC    MKMCBTY,MKMCBTY        CLEAR COMSCORE BOOKTYPE                   
         OI    MKMCBTYH+1,X'20'       PROTECTED FIELD                           
         OI    MKMCBTYH+6,X'80'       TRANSMIT                                  
         J     TSTCOMSX                                                         
*                                                                               
TCOMS10  NI    MKMCSMTH+1,X'FF'-X'0C' SHOW COMSCORE MARKET LABEL                
         OI    MKMCSMTH+6,X'80'       LOW INTENSITY AND TRANSMIT                
         NI    MKMCSMKH+1,X'FF'-X'20' UNPROTECT FIELD                           
         OI    MKMCSMKH+6,X'80'       TRANSMIT                                  
*                                                                               
         NI    MKMCBTXH+1,X'FF'-X'0C' SHOW COMSCORE BOOKTYPE LABEL              
         OI    MKMCBTXH+6,X'80'       REG INTENSITY AND TRANSMIT                
         NI    MKMCBTYH+1,X'FF'-X'20' UNPROTECT FIELD                           
         OI    MKMCBTYH+6,X'80'       TRANSMIT                                  
*                                                                               
TSTCOMSX J     EXIT                                                             
*                                                                               
*----------------------------- VRLSTEP1 ------------------------------*         
*                                                                               
VRLSTEP1 NTR1                                                                   
*         DELETES THE 'L'-RECORD W/. OLD ALPHA-MARKET                           
*         R3=AIO2--> 'L'-RECORD.                                                
*                                                                               
         CLC   MEDIA,QMED          MEDIA 'T' OR 'R'?                            
         BE    VRL100                                                           
         CLI   MEDIA,C'C'          MEDIA 'C'? (CANADIAN)                        
         BNE   *+12                                                             
         BAS   RE,MKCANADC                                                      
         B     VRL100                                                           
         CLI   MEDIA,C'N'          MEDIA 'N'? (CANADIAN)                        
         JNE   *+2                                                              
         BAS   RE,MKCANADN                                                      
*                                                                               
VRL100   GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI  '),=C'STATION ',MYKEY,(R3+        
               )                                                                
         CLC   MYKEY(ANMKEYLQ),0(R3)  CHECK IF RECORD EXISTS.                   
         BE    VRL110              RECORD EXISTS.                               
VRL105   MVI   ERROR,NOTFOUND      RECORD DOESN'T EXIST => ERROR.               
         B     ERREXGO                                                          
VRL110   OI    (ANMCNTL-ANMRECD)(R3),X'80'  MARKED FOR DELETION.                
         GOTO1 DATAMGR,DMCB,=C'DMWRT   ',=C'STATION ',MYKEY,(R3)                
*                                                                               
XVRLSTP1 B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*----------------------------- VRLSTEP2 ------------------------------*         
*                                                                               
VRLSTEP2 NTR1                                                                   
*         ADDS/RESTORES THE 'L'-RECORD W/. PRIMARK                              
*                                                                               
         CLC   MEDIA,QMED          MEDIA 'T' OR 'R'?                            
         BE    VRL200                                                           
         CLI   MEDIA,C'C'          MEDIA 'C'? (CANADIAN)                        
         BNE   *+12                                                             
         BAS   RE,MKCANADC                                                      
         B     VRL200                                                           
         CLI   MEDIA,C'N'          MEDIA 'N'? (CANADIAN)                        
         JNE   *+2                                                              
         BAS   RE,MKCANADN                                                      
*                                                                               
VRL200   GOTO1 DATAMGR,DMCB,(X'08',=C'DMRDHI  '),=C'STATION ',MYKEY,(R3+        
               )                                                                
*                          CHECK IF THE RECORD WAS MARKED FOR DELETION.         
         CLC   MYKEY(ANMKEYLQ),0(R3)                                            
         BE    VRL210              YES, SO RESTORE IT.                          
         XC    0(256,R3),0(R3)     NO, SO BUILD NEW RECORD AND ADD IT.          
         MVC   0(ANMKEYLQ,R3),MYKEY                                             
         MVC   (ANMRCLEN-ANMRECD)(2,R3),=H'20'                                  
         MVC   COMMAND,=C'DMADD   '                                             
         BAS   RE,VR230                                                         
         B     XVRLSTP2                                                         
VRL210   NI    (ANMCNTL-ANMRECD)(R3),X'FF'-X'80'   RESTORE RECORD.              
         MVC   (ANMRCLEN-ANMRECD)(2,R3),=H'20'                                  
         MVC   COMMAND,=C'DMWRT   '                                             
         BAS   RE,VR230                                                         
*                                                                               
XVRLSTP2 B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*============================== PACKNUM ==============================*         
*                                                                               
PACKNUM  NTR1                                                                   
*         CHECKS AND PACKS FIELD.                                               
*         R2-->MKMRANKH.                                                        
*         R0=BINARY VALUE OF FIELD ON EXIT.                                     
*                                                                               
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         CLI   5(R2),0             WAS THERE ANY INPUT?                         
         BE    XPACK1               NO, EXIT WITH AN ERROR CODE.                
         TM    4(R2),X'08'         IS IT VALID NUMERICS?                        
         BZ    XPACK2               NO, EXIT WITH AN ERROR CODE.                
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R0,DUB                                                           
*                                                                               
         MVI   ERROR,INVALID       MAYBE IT'S INVALID.                          
         B     XPACKNUM                                                         
*                                                                               
XPACK1   MVI   ERROR,MISSING                                                    
         B     XPACKNUM                                                         
*                                                                               
XPACK2   MVI   ERROR,NOTNUM                                                     
*                                                                               
XPACKNUM XIT1  REGS=(R0,R1)                                                     
         EJECT                                                                  
*                                                                               
*========================== VALIDATE MARKET ==========================*         
*                                                                               
VALMRKT  NTR1                                                                   
*         VALIDATES THE ALPHA-MARKET LIST.                                      
*                                                                               
         XC    TEMP,TEMP           BUILD BASIC ELEMENT                          
         XC    ALST,ALST           CLEAR PREV VALUES                            
*                                                                               
** VALIDATE AN INPUT FIELD                                                      
*                                                                               
VALM2    LA    R2,MKMMKTSH                                                      
         GOTO1 SCANNER,DMCB,(R2),(X'80',SCANTBL)                                
         CLI   4(R1),0                                                          
         BE    XVAL1               EXIT ON NO-INPUT ERROR                       
         MVC   NLINES,4(R1)        SAVE NUMBER OF -INPUT FIELDS                 
         CLI   SVAPROF+7,C'C'      CANADIAN TV ONLY ALLOWED 1 ALPHA MKT         
         BNE   VALM3                                                            
         CLI   QMED,C'T'                                                        
         BNE   VALM3                                                            
         CLI   NLINES,1            1 ALPHA MARKET?                              
         BE    VALM3               YES                                          
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BNE   XVAL2               NO, ERROR                                    
         CLI   NLINES,2            MORE THAN 2 ALPHA MARKETS?                   
         BH    XVAL2               NO, ERROR                                    
*                                                                               
         LA    R5,SCANTBL          R5=A(SCAN BLOCK ENTRY)                       
         CLC   =C'AKAT',12(R5)     FORCE AN UPDATE ON SPILLDEF RECORDS?         
         BE    *+10                YES                                          
         CLC   =C'ABEA',12(R5)     FORCE AN UPDATE ON SPILLDEF RECORDS?         
         BE    *+10                YES                                          
         CLC   =C'EJOR',12(R5)     FORCE AN UPDATE ON SPILLDEF RECORDS?         
         BNE   XVAL2               NO, ERROR                                    
*                                                                               
         MVI   UPDTSD,C'Y'         UPDATE SPILLDEF                              
         MVI   FNDX,1                                                           
         B     VALM14                                                           
*                                                                               
VALM3    MVI   FNDX,1                                                           
         LA    R5,SCANTBL          R5=A(SCAN BLOCK ENTRY)                       
*                                                                               
VALM4    CLC   FNDX,NLINES                                                      
         BH    VALOK                                                            
         CLI   0(R5),0             L'FLD                                        
         BE    XVAL1                                                            
         CLI   0(R5),3             L'FLD                                        
         BH    XVAL2               EXIT ON INVALID-INPUT ERROR.                 
*                                                                               
** VALIDATE MARKET NUMBERS                                                      
*                                                                               
VALM8    CLI   SVAPROF+7,C'C'      CANADIAN AGENCY?                             
         BNE   VALM8A              NO                                           
         CLI   QMED,C'T'           MEDIA T?                                     
         BNE   VALM8A              NO                                           
         CLC   =C'***',12(R5)      HAVE *** TO INDICATE NO MARKET?              
         BE    VALM9A              YES, SKIP VALIDATION OF ALPHA                
*                                                                               
VALM8A   TM    2(R5),X'40'         C'LHS (ALPHA)- MUST BE CHAR                  
         BZ    XVAL3               EXIT ON NON-ALPHABETIC ERROR.                
*                                                                               
** ENSURE VALID MARKET                                                          
*                                                                               
         LA    R1,12(R5)           R1=PTS TO MKT CODE                           
         BRAS  RE,FNDMKT           FND MKT ON FILE, IF NOT ON FILE,             
         BZ    XVAL4                EXIT ON INVALID MARKET ERROR.               
*                                                                               
** ENSURE MARKET NOT PREVIOUSLY DEFINED                                         
*                                                                               
VALM9A   LA    RE,ALST                                                          
         LA    RF,L'ALST(RE)                                                    
VALM10   OC    0(3,RE),0(RE)                                                    
         BZ    VALM12                                                           
         CLC   12(3,R5),0(RE)    IS MARKET ALREADY ENTERRED?  IF YES,           
         BE    XVAL2              EXIT ON DUPLICATE (INVLD) ENTRY ERR.          
         LA    RE,3(RE)                                                         
         CR    RE,RF                                                            
         BL    VALM10                                                           
*        B     VALERR              TOO MANY MKTS                                
*                                                                               
VALM12   MVC   0(3,RE),12(R5)      ADD MARKET TO LIST                           
*                                  BUMP TO NEXT SCAN BLOCK ENTRY                
VALM14   ZIC   R1,FNDX                                                          
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R5,32(R5)                                                        
         B     VALM4                                                            
*                                                                               
** SET ERROR CODES AND/OR EXIT                                                  
*                                                                               
XVAL1    MVI   ERROR,MISSING                                                    
         B     VALERR                                                           
XVAL2    MVI   ERROR,INVALID                                                    
         B     VALERR                                                           
XVAL3    MVI   ERROR,NOTALPHA      ALPHA DATA ONLY ALLOWED IN FIELD             
         B     VALERR                                                           
XVAL4    MVI   ERROR,INVMKT        INVALID MARKET                               
         B     VALERR                                                           
XVAL5    MVI   ERROR,DUPLICAT      ERROR DUPLICATE ENTRY                        
*                                                                               
VALERR   SR    R1,R1               ERROR OCCURED                                
         B     XVAL                                                             
*                                                                               
VALOK    LA    R1,1                CLEAN EXIT                                   
*                                                                               
XVAL     LTR   R1,R1                                                            
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* GET RTG SVC MARKET NAME VIA DEMAND FOR CANADIAN TV                            
* ENTRY  HALF=RTG SVC NUMBER / BYTE=RTG SVC ID / R2=A(FLDHDR)                   
* EXIT   RTG SVC MARKET NAME SCREEN FIELD UPDATED                               
**********************************************************************          
*                                                                               
* *** ONLY CALLED FOR CANADA ***                                                
GETRSNAM NTR1  ,                                                                
         MVC   8(L'MKMRSN1,R2),SPACES                                           
         USING DBLOCKD,R3                                                       
         LA    R3,BLOCK                                                         
         XC    DBLOCK,DBLOCK                                                    
*                                                                               
         L     R1,AIO2                                                          
         ST    R1,DBAREC                                                        
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBFUNCT,DBGETMK                                                  
         MVC   DBSELRMK,HALF       SAVE THE RATING SRVC MKT #                   
         MVI   DBSELMED,C'C'       CANADA                                       
         MVC   DBSELSRC,BYTE                                                    
         MVC   DBSELAGY,AGENCY                                                  
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,0                                               
         CLI   DBERROR,0                                                        
         BNE   GRSMNMX                                                          
*                                                                               
         L     RF,DBAREC                                                        
         LA    RF,DMFRSTEL-DMKEY(RF)                                            
         USING DMELEM,RF                                                        
         SR    R1,R1                                                            
         IC    R1,DMLEN                                                         
         SH    R1,=H'5'                                                         
         CHI   R1,L'MKMRSN1-1                                                   
         BNH   *+8                                                              
         LHI   R1,L'MKMRSN1-1                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),DMMNAME     MOVE IT IN                                   
         DROP  RF                                                               
*                                                                               
* DURING TRANSITION TO SOFT-DEMOS GIVE HINT OF ALPHAMKT TO ENTER...             
* GET ALPHAMKT FOR RTG SVC MARKET NUMBER VIA DEMAND FOR CANADIAN TV             
*                                                                               
         L     RF,AIO              SEE IF ALPHA EXISTS                          
         CLC   MKTALST-MKTRECD(3,RF),=X'000000'                                 
         BNE   GRSMNMX             YEP - NO HINT NEEDED                         
         MVI   DBFUNCT,DBCNVN2A    CONVERTING NUMERIC TO ALPHA                  
         L     RF,ACOMFACS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,0                                               
         CLI   DBERROR,0                                                        
         BNE   *+10                                                             
         MVC   8+(L'MKMRSN1-4)(3,R2),DBSELALF PUT HINT AT END                   
         DROP  R3                                                               
GRSMNMX  XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
* GET RTG SVC MARKET NUMBER FOR ALPHAMKT VIA DEMAND FOR CANADIAN TV             
* ENTRY  BYTE=RTG SVC ID                                                        
* EXIT   HALF=RTG SVC MARKET NUMBER                                             
**********************************************************************          
*                                                                               
GETRSMKT NTR1  ,                                                                
         XC    HALF,HALF                                                        
         USING DBLOCKD,R3                                                       
         LA    R3,BLOCK                                                         
         XC    DBLOCK,DBLOCK                                                    
         L     R1,AIO2                                                          
         ST    R1,DBAREC                                                        
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '                                                   
         MVI   DBFUNCT,DBCNVA2N    CONVERTING ALPHA TO NUMERIC                  
         MVC   DBSELALF,ALST                                                    
         MVI   DBSELMED,C'C'       CANADA                                       
         MVC   DBSELSRC,BYTE                                                    
         MVC   DBSELAGY,AGENCY                                                  
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CDEMAND-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,DBLOCK,0                                               
         CLI   DBERROR,0                                                        
         BNE   *+10                                                             
         MVC   HALF,DBSELRMK                                                    
         XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*=============================== LTEST ===============================*         
*                                                                               
LTEST    NTR1                                                                   
*         WHEN PRIMARK<>NULLS, THERE SHOULD BE AN 'L'-RECORD                    
*          OUT THERE CONTAINING PRIMARK AND MKTCODE.                            
*                                                                               
         BAS   RE,BLDANKEY                                                      
         USING ANMRECD,R1                                                       
         MVC   ANMKAMRK,PRIMARK                                                 
         DROP  R1                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD  ',=C'STATION ',MYKEY,AIO2                
         CLI   8(R1),0             TEST FOR ANY ERRORS.                         
         JNE   *+2                 DIE IF ANY ERROR.                            
*                                                                               
         MVC   AIO,AIO2            DON'T KILL I/O BUFFER W/. CHANGES.           
         BAS   RE,SF               DO SET-FILES ROUTINE.                        
         GOTO1 READ                RESTORE GENCON'S DATAMGR READ.               
         MVC   AIO,AIO1            WRITE DATA IN I/O#1 TO FILE.                 
*                                                                               
XLTEST   B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*========================== XRECPUT ROUTINE ==========================*         
*                                                                               
ZRP      DS    0H                  SEE IF WE NEED TO RESTORE                    
*                                   GENCON'S STATUS.                            
         OC    MYLSVKEY,MYLSVKEY   TEST IF DISPREC IS FROM LISTING.             
         BZ    XZRP                 NOPE, IT ISN'T.                             
*                                                                               
         MVC   KEY,MYLSVKEY                                                     
         GOTO1 HIGH                RESTORE STATUS FOR GENCON.                   
*                                                                               
XZRP     B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================== DISPLAY KEY ROUTINE ========================*         
*                                                                               
DK       DS    0H                                                               
*                                                                               
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    DK5                                                              
         TM    12(RA),X'80'        CHECK IF AUTHORIZED FOR ADD/DEL/CHA          
         BZ    DK5                                                              
         MVI   ERROR,NOTAUTH       NOT AUTHORIZED FOR THIS FUNCTION             
         CLI   THISLSEL,C'C'       SELECT FOR CHANGE?                           
         BE    NOTAUTHD                                                         
*                                                                               
DK5      XC    MYLSVKEY,MYLSVKEY                                                
         TM    OPTFLAG,OPTLANMQ    ARE WE LISTING VIA A/N RECORDS?              
         BZ    DK10                 NO, AIO HAS NORMAL MARKET.                  
*                                                                               
         L     R6,AIO              WE'LL NEED SOME DATA FROM BUFFER.            
         USING ANMRECD,R6                                                       
         MVC   MYLSVKEY,KEY        KEY IS SAVED=>CAN DESTROY BUFFER.            
         MVC   KEY,ZEROES          GET SET TO BUILD MARKET-KEY.                 
*                                                                               
         LA    R4,KEY              BUILD MARKET-KEY HERE.                       
         USING MKTRECD,R4                                                       
         MVI   MKTKTYPE,MKTKTYPQ   MARKET RECORDS ARE TYPE-'M'.                 
         MVC   MKTKMED,ANMKMED     MEDIA.                                       
         MVC   MKTKMKT,ANMKNMRK    NUMERIC MARKET CODE.                         
         MVC   MKTKAGY,ANMKAGCY    AGENCY.                                      
         DROP  R4,R6                                                            
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(MKTKEYLQ),0(R6) R6=AIO-->MARKET RECORD.                      
         JNE   *+2                 MARKET-RECORD HAD BETTER BE FOUND.           
*                                                                               
DK10     L     R4,AIO              AIO NOW POINTS TO CORRECT BUFFER             
         USING MKTRECD,R4           W/. MARKET RECORD.                          
*                                                                               
*------------------------------- MEDIA -------------------------------*         
*                                                                               
         MVC   MKMMEDI,MKTKMED                                                  
         OI    MKMMEDIH+6,X'80'                                                 
*                                                                               
*----------------------------- MARKET CODE ---------------------------*         
*                                                                               
         MVC   MKMMRKT,BLANKS      CLEAR FIELD OUT FIRST.                       
         MVC   MKMMRKT,MKTKMKT                                                  
         OI    MKMMRKTH+6,X'80'                                                 
         MVC   MKTCODE,MKTKMKT     BUILD 'L'-KEY USES MKTCODE FIELD.            
*                                                                               
         DROP  R4                                                               
*                                                                               
         BAS   RE,TSTCOMS          TEST COMSCORE                                
*                                                                               
XDK      B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================= DISPLAY RECORD ROUTINE ======================*         
*                                                                               
*-------------------- CLEAR OUT DATA FIELDS FIRST --------------------*         
*                                                                               
DR       DS    0H                                                               
         TWAXC MKMNAMEH                                                         
*                                                                               
*----------------------- FILL UP DATA FIELDS ------------------------*          
*                                                                               
         L     R4,AIO                                                           
         USING MKTRECD,R4                                                       
         MVC   MKMNAME,MKTNAME     MARKET NAME.                                 
         MVC   MKMTZON,MKTZONE     TIME ZONE.                                   
         MVC   MKMRANK,MKTRANK     MARKET RANK.                                 
         MVC   MKMHOMS,MKTHOMES    MARKET HOMES.                                
         MVC   MKMNTA,MKTNTA       NTA (1-29).                                  
*                                                                               
         XC    MKMWT,MKMWT                                                      
         OC    MKTWT,MKTWT                                                      
         BZ    DR10                                                             
         EDIT  (C4,MKTWT),(5,MKMWT),2,ALIGN=LEFT                                
*                                                                               
DR10     XC    MKMSHR,MKMSHR                                                    
         OC    MKTSHR,MKTSHR                                                    
         BZ    DR20                                                             
         EDIT  (C4,MKTSHR),(5,MKMSHR),2,ALIGN=LEFT                              
*                                                                               
DR20     XC    DUB,DUB                                                          
         MVC   DUB(1),MKTRS1       MOVE RTG SVC ID                              
         MVC   DUB+1(2),MKTRSM1    MOVE RTG SVC MKT NUM                         
         MVC   DUB+3(1),MKTCLAS1   MOVE RTG SVC SWEEP CLASS                     
         OI    DUB+3,X'F0'                                                      
         MVC   DUB+4(1),MKTRS2                                                  
         MVC   DUB+5(2),MKTRSM2                                                 
         MVC   DUB+7(1),MKTCLAS2                                                
         OI    DUB+7,X'F0'                                                      
*                                                                               
         CLI   DUB,C'0'                                                         
         BNE   DR20A                                                            
         MVI   DUB+4,C'1'                                                       
         B     DR30                                                             
*                                                                               
DR20A    CLI   DUB,C'1'                                                         
         BNE   DR20B                                                            
         MVI   DUB+4,C'0'                                                       
         B     DR30                                                             
*                                                                               
DR20B    CLI   DUB+4,C'0'                                                       
         BNE   DR20C                                                            
         MVI   DUB,C'1'                                                         
         B     DR30                                                             
*                                                                               
DR20C    CLI   DUB+4,C'1'                                                       
         BNE   DR20D                                                            
         MVI   DUB,C'0'                                                         
         B     DR30                                                             
*                                                                               
DR20D    MVI   DUB,C'0'                                                         
         MVI   DUB+4,C'1'                                                       
*                                                                               
DR30     XC    ELEM(8),ELEM                                                     
*                                                                               
         CLI   SVAPROF,C'0'        TEST NSI AGENCY                              
         BE    *+12                                                             
         CLI   SVAPROF,C'2'        OR BOTH                                      
         BNE   DR40                                                             
*                                                                               
         MVC   ELEM(4),DUB                                                      
         CLI   DUB,C'0'            TEST NSI DATA                                
         BE    DR40                                                             
         MVC   ELEM(4),DUB+4                                                    
*                                                                               
DR40     CLI   SVAPROF,C'1'        TEST ARB AGENCY                              
         BE    *+12                                                             
         CLI   SVAPROF,C'2'        OR BOTH                                      
         BNE   DR50                                                             
*                                                                               
         MVC   ELEM+4(4),DUB+4                                                  
         CLI   DUB+4,C'1'                                                       
         BE    DR50                                                             
         MVC   ELEM+4(4),DUB                                                    
*                                                                               
DR50     LA    R2,MKMRSM1H                                                      
         LA    R3,ELEM                                                          
         BAS   RE,DRRTSRV          GO DIPLAY RATING SERVICE.                    
*                                                                               
         LA    R2,MKMRSM2H                                                      
         LA    R3,ELEM+4                                                        
         BAS   RE,DRRTSRV          GO DISPLAY RATING SERVICE.                   
*                                                                               
FMT10    MVC   MKMLOCK,MKTLTACC    LIMIT ACCESS CODES.                          
*                                                                               
         XR    R1,R1               ANY COMSCORE MARKET NUMBER?                  
         ICM   R1,3,MKTCSMKN                                                    
         JZ    FMT15               NONE, SKIP IT                                
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MKMCSMK,DUB                                                      
         OI    MKMCSMKH+6,X'80'    TRANSMIT                                     
*                                                                               
FMT15    LA    R2,MKTALST          R2-->ALPHA-MARKETS.                          
         ST    R2,AALST            AALST=A(ALPHA-MARKETS).                      
         LA    R2,MKMMKTSH                                                      
         BAS   RE,DRAMKT           DISPLAY ALPHA MKTS.                          
         CLI   SVAPROF+7,C'C'      CANADIAN AGENCY                              
         BNE   FMT30               NO                                           
         CLI   QMED,C'T'           MEDIA T?                                     
         BNE   FMT30               NO                                           
         CLI   MKTRSVC,C'0'        NSI?                                         
         BE    FMT20               YES                                          
         CLI   MKTRSVC,C'1'        BBM?                                         
         BNE   FMT30               NO                                           
         MVC   MKMRSVC,=C'BBM'                                                  
         B     FMT30                                                            
*                                                                               
FMT20    MVC   MKMRSVC,=C'NSI'                                                  
*                                                                               
FMT30    EDIT  (B2,MKTUNIT),(4,MKMUNIT),ALIGN=LEFT    DISPLAY UNIT              
*                                                                               
         GOTO1 DATCON,DMCB,(2,MKTLPMDT),(9,MKMSDAT)                             
*                                                                               
         CLI   MKTCDEM,C'N'        NSI?                                         
         BNE   *+8                 NO                                           
         MVI   MKMCDEM,C'N'                                                     
         CLI   MKTCDEM,C'F'        FUSION?                                      
         BNE   *+8                 NO                                           
         MVI   MKMCDEM,C'F'                                                     
         CLI   MKTCDEM,C'0'        NO DEMOS?                                    
         BNE   *+8                 NO                                           
         MVI   MKMCDEM,C'0'                                                     
*                                                                               
         CLC   MKTTALM,SPACES      TALENT MARKET                                
         BNH   *+10                                                             
         MVC   MKMTALM,MKTTALM                                                  
*                                                                               
         OC    MKTBKTYP,MKTBKTYP                                                
         BZ    DR150                                                            
*                                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB  GET A(BOOK TABLE)                            
         ICM   RF,15,0(R1)         A(TABLE) RETURNED IN P1                      
         JZ    *+2                 DIE IF NO TABLE RETURNED                     
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
*                                                                               
DR100    CLI   0(RF),X'FF'                                                      
         JE    *+2                 DIE IF EOT                                   
*                                                                               
         CLC   MKTBKTYP,SPBKTYPN                                                
         BE    *+10                                                             
         AR    RF,R0                                                            
         B     DR100                                                            
*                                                                               
         MVC   MKMBTYP,SPBKTYPA    BOOK TYPE                                    
         OI    MKMBTYPH+6,X'80'                                                 
         DROP  RF                                                               
*                                                                               
DR150    OC    MKTCSBTY,MKTCSBTY   Any comScore Booktype?                       
         BZ    DR200               None                                         
         MVC   MKMCBTY(1),MKTCSBTY Yes, don't need Demtabs yet                  
         OI    MKMCBTYH+6,X'80'      as we only have LIVE right now             
         DROP  R4                                                               
*                                                                               
DR200    OC    MYLSVKEY,MYLSVKEY   TEST IF DISPREC IS FROM LISTING.             
         BZ    XDR                  NOPE, IT ISN'T.                             
*                                                                               
         MVC   KEY,MYLSVKEY                                                     
         GOTO1 READ                RESTORE STATUS FOR GENCON.                   
*                                                                               
XDR      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*======================= DISPLAY RATING SERVICE ======================*         
*                                                                               
* ENTRY  R2=A(NSI OR ARB/BBM FLDHDR)                                            
*        R3=A(4 BYTE DATA VALUES - RTG SVC ID/1, MKT/2, SWEEP/1)                
*                                                                               
DRRTSRV  NTR1  ,                                                                
         XC    8(4,R2),8(R2)       CLEAR MARKET NUMBER                          
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)          GET MKT NUM                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(4,R2),DUB                                                      
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO SWEEP CLASS                         
         MVI   8(R2),0             CLEAR IT                                     
         OI    6(R2),X'80'         AND XMT                                      
         MVC   8(1,R2),3(R3)       MOVE SWEEP CLASS                             
         OI    8(R2),X'F0'         GUARANTEE EBCDIC                             
*                                                                               
         CLI   SVAPROF+7,C'C'      SHOW MARKET NAME FOR CANADIAN TV             
         BNE   DRRTSRVX                                                         
         CLI   QMED,C'T'                                                        
         BNE   DRRTSRVX                                                         
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO NAME                                
         MVC   HALF,1(R3)                                                       
         MVI   BYTE,C'N'                                                        
         CLI   0(R3),C'0'                                                       
         BE    *+8                                                              
         MVI   BYTE,C'A'                                                        
         BAS   RE,GETRSNAM                                                      
         OI    6(R2),X'80'         AND XMT                                      
*                                                                               
DRRTSRVX B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*======================= DISPLAY ALPHA MARKETS =======================*         
*                                                                               
DRAMKT   NTR1                                                                   
*                                                                               
         XC    MKMMKTS,MKMMKTS     CLEAR SCREEN FIELD                           
         SR    R6,R6               R6=N'ENTRIES IN TABLE                        
         L     R5,AALST            R5-->ALPHA-MARKETS LIST.                     
         LR    R4,R5                                                            
*                                                                               
DRAMKT10 CH    R6,=H'9'            MAX 9 MKTS                                   
         BNL   DRAMKT15                                                         
         OC    0(3,R5),0(R5)       NON ZERO?                                    
         BZ    DRAMKT15                                                         
         LA    R6,1(R6)                                                         
         LA    R5,3(R5)                                                         
         B     DRAMKT10                                                         
*                                  FORMAT SCAN BLOCK INTO TWA                   
DRAMKT15 DS    0H                                                               
         LTR   R6,R6                                                            
         BZ    DRAMKTX                                                          
*                                                                               
         BCTR  R6,0                DON'T INCLUDE PRIMARY (1ST) ALPHA            
         LTR   R6,R6                MARKET INTO SORT.                           
         BZ    DRAMKT20                                                         
*                                                                               
         LA    R5,3(R4)                                                         
         GOTO1 XSORT,DMCB,(0,(R5)),(R6),3,3,0  SORT BEFORE DIS-                 
*                                    PLAY, LEAVE 1ST ALPHA MKT ALONE.           
*                                                                               
DRAMKT20 LA    R6,1(R6)            DISPLAY ALL ALPHA MARKETS.                   
         GOTO1 =V(SCINKEY),DMCB,(1,MKMMKTSH),(3,(R4)),(R6),RR=RELO              
         OI    MKMMKTSH+6,X'80'                                                 
*                                                                               
DRAMKTX  B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=========================== DELETE RECORD ===========================*         
DELR     DS    0H                  DELETE THE 'L'-RECORDS.                      
*                                                                               
         L     R4,AIO                                                           
         USING MKTRECD,R4                                                       
         OC    MKTALST,MKTALST                                                  
         BZ    XDELR                                                            
*                                                                               
         BAS   RE,BLDANKEY                                                      
         USING ANMRECD,R1                                                       
         MVC   ANMKAMRK,MKTALST                                                 
         DROP  R1,R4                                                            
*                                                                               
         L     R4,AIO2                                                          
         LR    R3,R4               FOR MKCANADC & MKCANADN.                     
         BAS   RE,DELRGO                                                        
*                                                                               
         CLI   SVAPROF+7,C'C'      TEST IF CANADIAN AGENCY.                     
         BNE   DELR100                                                          
*                                                                               
         BAS   RE,MKCANADC                                                      
         BAS   RE,DELRGO                                                        
         BAS   RE,MKCANADN                                                      
         BAS   RE,DELRGO                                                        
*                                                                               
** 'M' RECORDS W/. CANADIAN AGENCY                                              
*                                                                               
DELR100  GOTO1 CNRECS,DMCB,C'D'    DELETE FOR CANADIAN AGENCY.                  
         MVC   AIO,AIO2            DON'T KILL I/O BUFFER W/. CHANGES.           
         BAS   RE,SF               DO SET-FILES ROUTINE.                        
         GOTO1 READ                RSTRE DTMGR'S RD (KEY HASN'T CHANGE)         
         MVC   AIO,AIO1            WRITE DATA IN I/O#1 TO FILE.                 
*                                                                               
XDELR    B     EXIT                                                             
         SPACE 4                                                                
*                                                                               
*---------------------------------------------------------------------*         
*                                                                               
DELRGO   NTR1                                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMREAD  ',=C'STATION ',MYKEY,(R4)                
         CLI   8(R1),0                                                          
         JNE   *+2                 SHOULD CHANGE THIS ERROR CHECK.              
         USING ANMRECD,R4                                                       
         OI    ANMCNTL,X'80'       MARK FOR DELETION.                           
         DROP  R4                                                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT   ',=C'STATION ',MYKEY,(R4)                
*                                                                               
XDELRGO  B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=========================== RESTORE RECORD ==========================*         
RSTR     DS    0H                  RESTORING 'L'-RECORD.                        
*                                                                               
         L     R4,AIO                                                           
         USING MKTRECD,R4                                                       
         OC    MKTALST,MKTALST     IS THERE AN 'L'-RECORD TO RESTORE?           
         BZ    XDELR               NO, EXIT.                                    
*                                                                               
         BAS   RE,BLDANKEY                                                      
         USING ANMRECD,R1                                                       
         MVC   ANMKAMRK,MKTALST                                                 
         DROP  R1,R4                                                            
*                                                                               
         L     R4,AIO2                                                          
         LR    R3,R4               FOR MKCANADC & MKCANADN.                     
         BAS   RE,RSTRGO                                                        
         CLI   SVAPROF+7,C'C'      TEST FOR CANADIAN AGENCY.                    
         BNE   RSTR100                                                          
         BAS   RE,MKCANADC         MEDIA 'C' (CANADIAN AGENCY).                 
         BAS   RE,RSTRGO                                                        
         BAS   RE,MKCANADN         MEDIA 'N' (CANADIAN AGENCY).                 
         BAS   RE,RSTRGO                                                        
*                                                                               
RSTR100  GOTO1 CNRECS,DMCB,C'R'    RESTORE FOR CANADIAN AGENCY.                 
         MVC   AIO,AIO2            DON'T KILL I/O BUFFER W/. CHANGES.           
         BAS   RE,SF               DO SET-FILES ROUTINE.                        
         GOTO1 READ                RSTRE DTMGR'S RD (KEY HASN'T CHANGE)         
         MVC   AIO,AIO1            WRITE DATA IN I/O#1 TO FILE.                 
*                                                                               
XRSTR    B     EXIT                                                             
         SPACE 4                                                                
*                                                                               
*---------------------------------------------------------------------*         
*                                                                               
RSTRGO   NTR1                                                                   
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'08',=C'DMREAD  '),=C'STATION ',MYKEY,(R4+        
               )                                                                
         CLI   8(R1),0                                                          
         BE    RSTR10                                                           
         TM    8(R1),X'10'         CHECK FOR NOT FOUND ERROR.                   
         BZ    RSTR14                                                           
         MVI   ERROR,NOTFOUND                                                   
         B     ERREXGO                                                          
RSTR14   TM    8(R1),X'80'         TEST FOR END OF FILE.                        
         BZ    RSTR16                                                           
         MVI   ERROR,EOF                                                        
         B     ERREXGO                                                          
RSTR16   TM    8(R1),X'40'         TEST FOR DISK ERROR.                         
         BZ    RSTR10                                                           
         MVI   ERROR,DISKERR                                                    
         B     ERREXGO                                                          
*                                                                               
         USING ANMRECD,R4                                                       
RSTR10   NI    ANMCNTL,X'FF'-X'80'     UNMARK DELETION.                         
         DROP  R4                                                               
         GOTO1 DATAMGR,DMCB,=C'DMWRT   ',=C'STATION ',MYKEY,(R4)                
*                                                                               
XRSTRGO  B     EXIT                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=============================== REQREC ==============================*         
REQREC   NTR1                                                                   
*                                                                               
         L     R3,AIO2                                                          
         XC    0(110,R3),0(R3)         GENERATE REQUEST RECORD                  
         MVI   10(R3),45                                                        
         MVI   14(R3),106                                                       
         MVC   26(80,R3),SPACES                                                 
         MVC   26(2,R3),=C'45'                                                  
         MVC   28(2,R3),AGENCY                                                  
         MVC   30(1,R3),QMED                                                    
         MVC   31(3,R3),=C'ALL'                                                 
         MVI   36(R3),C'M'                                                      
         L     R4,AIO                                                           
         USING MKTRECD,R4                                                       
         MVC   40(4,R3),MKTKMKT                                                 
         DROP  R4                                                               
         MVC   94(7,R3),=C'CONTROL'                                             
         MVI   93(R3),C'A'                                                      
         CLI   ACTNUM,ACTADD                                                    
         BE    *+8                                                              
         MVI   93(R3),C'C'                                                      
         GOTO1 DATAMGR,DMCB,=C'DMADD   ',=C'REQUEST ',(R3),(R3)                 
         TM    8(R1),X'50'                                                      
         BZ    EXIT                                                             
*                                                                               
         DC    H'0'                                                             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
* SPILLDEF - IF THE ALPHA MKT OR RATING SERVICE WAS CHANGED FOR THIS  *         
* MARKET, THE CHANGES NEED TO BE REFLECTED IN THE SPILLDEF RECORDS    *         
***********************************************************************         
SPILLDEF NTR1                                                                   
*                                                                               
         CLI   UPDTSD,C'Y'         FORCE UPDATE ON SPILLDEF?                    
         BE    SPILL10                                                          
         CLC   SVALST,ALST         ALPHA MARKET CHANGED?                        
         BNE   SPILL10             YES                                          
         CLC   SVRSVC,RSVC         RATING SERVICE CHANGED                       
         BE    SPILLX              NO                                           
*                                                                               
SPILL10  MVC   SYSDIR,=C'SPTDIR  '                                              
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   LKEY,=H'13'         L(SPILLDEF RECORD)=13                        
*                                                                               
         PACK  DUB,MKTCODE                                                      
         CVB   R0,DUB                                                           
         STCM  R0,3,BMKT                NEED THIS FOR SPILLDEF                  
*                                                                               
         MVC   MYSVKEY,KEY         SAVE THE MKT RECORD KEY                      
         MVC   AIO,AIO2            DO NOT CLOBBER MKT RECORD                    
         XC    KEY,KEY                                                          
         LA    R2,KEY                                                           
         USING SDEFKEY,R2                                                       
         MVC   SDEFKTYP,=X'0D13'   SPILLDEF RECORD                              
         MVC   SDEFKAGY,AGENCY     AGENCY                                       
*                                                                               
         GOTO1 HIGH                                                             
         B     SPILL20                                                          
*                                                                               
SPILL15  GOTO1 SEQ                                                              
*                                                                               
SPILL20  CLC   KEY(4),KEYSAVE      HAVE SPILLDEF WITH THIS AGENCY?              
         BNE   SPILL50             NO, DONE                                     
         CLI   SDEFKSTA+4,0        HAVE MEDIA T STATION?                        
         BNE   SPILL15             NO, NOT CHANGING MEDIA R!                    
         DROP  R2                                                               
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL            HAVE X'05' ELEMENT?                          
         B     *+8                                                              
*                                                                               
SPILL25  BAS   RE,NEXTEL                                                        
         BNE   SPILL35             NO, READ NEXT SPILLDEF RECORD                
*                                                                               
         USING SDEFEL05,R6                                                      
         CLC   SDEFAMKT,BMKT       MATCH ON MARKET?                             
         BNE   SPILL25             NO, LOOK AT NEXT ELEMENT                     
*                                                                               
SPILL30  XC    ELEM,ELEM           BUILD NEW X'05' ELEMENT HERE                 
         ZIC   R1,1(R6)            LENGTH OF CURRENT ELEMENT                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R6)       GET A CURRENT COPY OF X'05' ELEMENT          
         MVI   ELEM+1,14           LENGTH WITH RATING SERVICE                   
         MVC   ELEM+13(1),RSVC     NEW RATING SERVICE                           
         CLI   UPDTSD,C'Y'         FORCE UPDATE ON ALPHA MARKET?                
         BE    SPILL31             YES                                          
         CLC   SVALST,ALST         ALPHA MARKET CHANGED?                        
         BE    *+10                NO, LEAVE IT AS IT WAS                       
SPILL31  MVC   ELEM+10(3),ALST     NEW ALPHA MARKET                             
         MVC   ELEM+4(2),RSM1      NSI MARKET                                   
         CLC   RSVC,RS1            ADDING NSI MARKET TO SPILLDEF?               
         BE    *+10                YES                                          
         MVC   ELEM+4(2),RSM2      BBM MARKET                                   
         GOTO1 RECUP,DMCB,(0,AIO),(R6)       DELETE THE OLD X'05' ELEM          
         GOTO1 RECUP,DMCB,(0,AIO),ELEM,(R6)  ADD THE NEW X'05' ELEMENT          
         B     SPILL25                                                          
*                                                                               
SPILL35  GOTO1 PUTREC                                                           
         B     SPILL15                                                          
*                                                                               
SPILL50  MVC   KEY,MYSVKEY                                                      
         MVC   AIO,AIO1                                                         
*                                                                               
SPILLX   B     EXIT                                                             
*                                                                               
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
*============================== MESSAGES =============================*         
MISSERR  MVI   ERROR,MISSING       TELL USER OF MISSING INPUT.                  
         GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
INVLERR  MVI   ERROR,INVALID       TELL USER OF INVALID INPUT.                  
         GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
NUMBERR  MVI   ERROR,NOTNUM        TELL USER WE NEED NUMERIC INPUT.             
         GOTO1 ERREX                                                            
         B     EXIT                                                             
*                                                                               
ERREXGO  GOTO1 ERREX                                                            
*                                                                               
DSERR1   MVC   CONHEAD(L'DSERR1MS),DSERR1MS                                     
         B     GENERR                                                           
DSERR1MS DC    C'** ERROR ** ENTER "Y" FOR YES, NOTHING FOR NO'                 
*                                                                               
LAERR1   MVC   CONHEAD(L'LAERR1MS),LAERR1MS                                     
         B     GENERR                                                           
LAERR1MS DC    C'** ERROR ** MAXIMUM OF 3-LETTER INPUT'                         
*                                                                               
LAERR2   MVC   CONHEAD(L'LAERR2MS),LAERR2MS                                     
         B     GENERR                                                           
LAERR2MS DC    C'** ERROR ** INVALID CHARACTER INPUTTED'                        
*                                                                               
RCERR1   MVC   CONHEAD(L'RCERR1MS),RCERR1MS                                     
         B     GENERR                                                           
RCERR1MS DC    C'PLEASE ENTER "U" FOR U.S. OR "C" FOR CANADA'                   
*                                                                               
RCERR2   MVC   CONHEAD(L'RCERR2MS),RCERR2MS                                     
         B     GENERR                                                           
RCERR2MS DC    C'** ERROR ** U.S. STATE MUST HAVE 2 LETTERS'                    
*                                                                               
RCERR3   MVC   CONHEAD(L'RCERR3MS),RCERR3MS                                     
         B     GENERR                                                           
RCERR3MS DC    C'** ERROR ** U.S. ZIP CODE MUST BE NUMERIC'                     
*                                                                               
RCERR4   MVC   CONHEAD(L'RCERR4MS),RCERR4MS                                     
         B     GENERR                                                           
RCERR4MS DC    C'** ERROR ** ZIP CODE DOES NOT MATCH COUNTRY'                   
*                                                                               
WTERR1   MVC   CONHEAD(L'WTERR1MS),WTERR1MS                                     
         B     GENERR                                                           
WTERR1MS DC    C'** ERROR ** MAX VALUE IS 99.99'                                
*                                                                               
LPMERR   MVC   CONHEAD(L'LPMERRMS),LPMERRMS                                     
         B     GENERR                                                           
LPMERRMS DC    C'** ERROR ** MUST HAVE ALPHA MARKET'                            
*                                                                               
LPMERRDT MVC   CONHEAD(11),=C'** ERROR **'                                      
         MVC   CONHEAD+12(3),0(R3)                                              
         LA    R4,CONHEAD+16                                                    
         CLI   2(R3),C' '                                                       
         BH    *+6                                                              
         BCTR  R4,0                                                             
         MVC   0(19,R4),=C'MUST BE ON OR AFTER'                                 
         GOTO1 DATCON,DMCB,(2,3(R3)),(9,20(R4))                                 
         B     GENERR                                                           
*                                                                               
ARBERR   MVC   CONHEAD(L'ARBERRC),ARBERRC                                       
         B     GENERR                                                           
ARBERRC  DC   C'* ERR * RATING SVC ON AGY REC MUST BE 1 OR 2 TO CHANGE'         
*                                                                               
RTGERR   MVC   CONHEAD(L'RTGERRC),RTGERRC                                       
         B     GENERR                                                           
RTGERRC  DC    C'** ERROR ** PLEASE ENTER BBM OR NSI'                           
*                                                                               
RTGERR2  MVC   CONHEAD(L'RTGERRC2),RTGERRC2                                     
         B     GENERR                                                           
RTGERRC2 DC    C'** ERROR ** ALPHA MARKET REQUIRED'                             
*                                                                               
RTGERR3  MVC   CONHEAD(L'RTGERRC3),RTGERRC3                                     
         B     GENERR                                                           
RTGERRC3 DC    C'** ERROR ** 00 PROFILE REQUIRES RATING SERVICE'                
*                                                                               
GENERR   OC    CONHEAD,BLANKS                                                   
         OI    CONHEADH+6,X'80'                                                 
         GOTO1 ERREX2                                                           
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*=========================== MISCELLANEOUS ===========================*         
         DS    0F                                                               
ZEROES   DC    (L'KEY)CL1'0'                                                    
BLANKS   DC    CL132' '                                                         
LPMTAB   DC    C'NY ',X'D081'     ON/AFTER APR01/2004                           
         DC    C'LA ',X'D0A1'     ON/AFTER MAY01/2004                           
         DC    C'CHI',X'D0E1'     ON/AFTER JUL01/2004                           
         DC    C'SF ',X'D141'     ON/AFTER OCT01/2004                           
         DC    C'PHI',X'D2E1'     ON/AFTER JUL01/2005                           
         DC    C'WAS',X'D2E1'     ON/AFTER JUL01/2005                           
         DC    C'DF ',X'D421'     ON/AFTER JAN01/2006                           
         DC    C'DET',X'D421'     ON/AFTER JAN01/2006                           
         DC    C'ATL',X'D4E1'     ON/AFTER JUL01/2006                           
         DC    X'FF'                                                            
*=========================== LITERAL POOL ===========================*          
         LTORG                                                                  
         EJECT                                                                  
* COPIED FROM SPREPFXCSM, THE PROGRAM THAT SEEDS THE COMSCORE MARKET #          
* TO THE SPOT MARKET RECORDS                                                    
         DS    0H                                                               
CSMKTS   DS    0XL4                                                             
* 1ST 2 BYTES ARE THE NIELSEN MARKET                                            
* 2ND 2 BYTES ARE THE COMSCORE MARKET                                           
* TABLE ENDS WITH X'0000'                                                       
         DC    AL2(100,2079)                                                    
         DC    AL2(101,2001)                                                    
         DC    AL2(102,2161)                                                    
         DC    AL2(103,2122)                                                    
         DC    AL2(104,2004)                                                    
         DC    AL2(105,2014)                                                    
         DC    AL2(106,2009)                                                    
         DC    AL2(107,2088)                                                    
         DC    AL2(108,2025)                                                    
         DC    AL2(109,2110)                                                    
         DC    AL2(110,2019)                                                    
         DC    AL2(111,2010)                                                    
         DC    AL2(112,2028)                                                    
         DC    AL2(113,2071)                                                    
         DC    AL2(114,2052)                                                    
         DC    AL2(115,2036)                                                    
         DC    AL2(116,2148)                                                    
         DC    AL2(117,2023)                                                    
         DC    AL2(118,2047)                                                    
         DC    AL2(119,2093)                                                    
         DC    AL2(120,2111)                                                    
         DC    AL2(121,2053)                                                    
         DC    AL2(122,2127)                                                    
         DC    AL2(123,2095)                                                    
         DC    AL2(124,2008)                                                    
         DC    AL2(125,2153)                                                    
         DC    AL2(126,2160)                                                    
         DC    AL2(127,2026)                                                    
         DC    AL2(128,2017)                                                    
         DC    AL2(129,2050)                                                    
         DC    AL2(130,2106)                                                    
         DC    AL2(131,2099)                                                    
         DC    AL2(132,2058)                                                    
         DC    AL2(133,2031)                                                    
         DC    AL2(134,2018)                                                    
         DC    AL2(135,2032)                                                    
         DC    AL2(136,2117)                                                    
         DC    AL2(137,2156)                                                    
         DC    AL2(138,2076)                                                    
         DC    AL2(139,2013)                                                    
         DC    AL2(140,2118)                                                    
         DC    AL2(141,2064)                                                    
         DC    AL2(142,2063)                                                    
         DC    AL2(143,2116)                                                    
         DC    AL2(144,2046)                                                    
         DC    AL2(145,2100)                                                    
         DC    AL2(146,2077)                                                    
         DC    AL2(147,2078)                                                    
         DC    AL2(148,2039)                                                    
         DC    AL2(149,2178)                                                    
         DC    AL2(150,2129)                                                    
         DC    AL2(151,2114)                                                    
         DC    AL2(152,2205)                                                    
         DC    AL2(153,2181)                                                    
         DC    AL2(154,2163)                                                    
         DC    AL2(155,2090)                                                    
         DC    AL2(156,2056)                                                    
         DC    AL2(157,2061)                                                    
         DC    AL2(158,2187)                                                    
         DC    AL2(159,2158)                                                    
         DC    AL2(160,2024)                                                    
         DC    AL2(161,2044)                                                    
         DC    AL2(163,2042)                                                    
         DC    AL2(164,2065)                                                    
         DC    AL2(165,2177)                                                    
         DC    AL2(166,2041)                                                    
         DC    AL2(167,2035)                                                    
         DC    AL2(169,2171)                                                    
         DC    AL2(170,2098)                                                    
         DC    AL2(171,2060)                                                    
         DC    AL2(173,2073)                                                    
         DC    AL2(174,2105)                                                    
         DC    AL2(175,2087)                                                    
         DC    AL2(176,2137)                                                    
         DC    AL2(177,2055)                                                    
         DC    AL2(181,2157)                                                    
         DC    AL2(182,2189)                                                    
         DC    AL2(183,2207)                                                    
         DC    AL2(184,2188)                                                    
         DC    AL2(188,2097)                                                    
         DC    AL2(192,2159)                                                    
         DC    AL2(196,2209)                                                    
         DC    AL2(197,2195)                                                    
         DC    AL2(198,2172)                                                    
         DC    AL2(200,2130)                                                    
         DC    AL2(202,2003)                                                    
         DC    AL2(203,2151)                                                    
         DC    AL2(204,2135)                                                    
         DC    AL2(205,2136)                                                    
         DC    AL2(206,2175)                                                    
         DC    AL2(209,2022)                                                    
         DC    AL2(210,2141)                                                    
         DC    AL2(211,2155)                                                    
         DC    AL2(212,2084)                                                    
         DC    AL2(213,2015)                                                    
         DC    AL2(216,2033)                                                    
         DC    AL2(217,2037)                                                    
         DC    AL2(218,2007)                                                    
         DC    AL2(219,2072)                                                    
         DC    AL2(222,2051)                                                    
         DC    AL2(223,2005)                                                    
         DC    AL2(224,2152)                                                    
         DC    AL2(225,2082)                                                    
         DC    AL2(226,2203)                                                    
         DC    AL2(227,2146)                                                    
         DC    AL2(228,2139)                                                    
         DC    AL2(230,2043)                                                    
         DC    AL2(231,2201)                                                    
         DC    AL2(232,2085)                                                    
         DC    AL2(233,2150)                                                    
         DC    AL2(234,2132)                                                    
         DC    AL2(235,2038)                                                    
         DC    AL2(236,2083)                                                    
         DC    AL2(237,2091)                                                    
         DC    AL2(238,2199)                                                    
         DC    AL2(239,2182)                                                    
         DC    AL2(240,2049)                                                    
         DC    AL2(241,2034)                                                    
         DC    AL2(242,2125)                                                    
         DC    AL2(243,2174)                                                    
         DC    AL2(244,2180)                                                    
         DC    AL2(247,2191)                                                    
         DC    AL2(248,2086)                                                    
         DC    AL2(249,2107)                                                    
         DC    AL2(250,2045)                                                    
         DC    AL2(251,2145)                                                    
         DC    AL2(252,2075)                                                    
         DC    AL2(256,2149)                                                    
         DC    AL2(257,2164)                                                    
         DC    AL2(258,2074)                                                    
         DC    AL2(259,2029)                                                    
         DC    AL2(261,2198)                                                    
         DC    AL2(262,2168)                                                    
         DC    AL2(269,2081)                                                    
         DC    AL2(270,2101)                                                    
         DC    AL2(271,2062)                                                    
         DC    AL2(273,2133)                                                    
         DC    AL2(275,2121)                                                    
         DC    AL2(276,2142)                                                    
         DC    AL2(278,2069)                                                    
         DC    AL2(279,2070)                                                    
         DC    AL2(282,2102)                                                    
         DC    AL2(286,2059)                                                    
         DC    AL2(287,2144)                                                    
         DC    AL2(291,2080)                                                    
         DC    AL2(292,2143)                                                    
         DC    AL2(293,2057)                                                    
         DC    AL2(298,2120)                                                    
         DC    AL2(302,2128)                                                    
         DC    AL2(305,2140)                                                    
         DC    AL2(309,2104)                                                    
         DC    AL2(310,2169)                                                    
         DC    AL2(311,2192)                                                    
         DC    AL2(316,2094)                                                    
         DC    AL2(317,2173)                                                    
         DC    AL2(318,2096)                                                    
         DC    AL2(322,2108)                                                    
         DC    AL2(324,2115)                                                    
         DC    AL2(325,2112)                                                    
         DC    AL2(334,2179)                                                    
         DC    AL2(336,2176)                                                    
         DC    AL2(337,2200)                                                    
         DC    AL2(340,2208)                                                    
         DC    AL2(343,2134)                                                    
         DC    AL2(344,2066)                                                    
         DC    AL2(345,2202)                                                    
         DC    AL2(346,2154)                                                    
         DC    AL2(347,2206)                                                    
         DC    AL2(349,2183)                                                    
         DC    AL2(351,2016)                                                    
         DC    AL2(352,2089)                                                    
         DC    AL2(353,2012)                                                    
         DC    AL2(354,2184)                                                    
         DC    AL2(355,2193)                                                    
         DC    AL2(356,2167)                                                    
         DC    AL2(357,2103)                                                    
         DC    AL2(358,2162)                                                    
         DC    AL2(359,2196)                                                    
         DC    AL2(360,2190)                                                    
         DC    AL2(362,2165)                                                    
         DC    AL2(364,2170)                                                    
         DC    AL2(365,2092)                                                    
         DC    AL2(366,2204)                                                    
         DC    AL2(367,2197)                                                    
         DC    AL2(370,2030)                                                    
         DC    AL2(371,2166)                                                    
         DC    AL2(373,2185)                                                    
         DC    AL2(389,2068)                                                    
         DC    AL2(390,2048)                                                    
         DC    AL2(398,2210)                                                    
         DC    AL2(400,2113)                                                    
         DC    AL2(401,2119)                                                    
         DC    AL2(402,2194)                                                    
         DC    AL2(403,2002)                                                    
         DC    AL2(404,2147)                                                    
         DC    AL2(407,2006)                                                    
         DC    AL2(410,2124)                                                    
         DC    AL2(411,2109)                                                    
         DC    AL2(413,2138)                                                    
         DC    AL2(419,2011)                                                    
         DC    AL2(420,2021)                                                    
         DC    AL2(421,2186)                                                    
         DC    AL2(425,2027)                                                    
         DC    AL2(428,2126)                                                    
         DC    AL2(439,2040)                                                    
         DC    AL2(455,2123)                                                    
         DC    AL2(462,2020)                                                    
         DC    AL2(466,2054)                                                    
         DC    AL2(468,2131)                                                    
         DC    AL2(481,2067)                                                    
CSMKTSX  DC    AL2(0,0)                                                         
**********************************************************************          
**********************************************************************          
         EJECT                                                                  
***********************************************************************         
* VALIDATES THE COMSCORE MARKET NUMBER                                          
*                                                                               
* ON EXIT:    EQ                  VALID COMSCORE MARKET                         
*             NEQ                 INVALID COMSCORE MARKET                       
***********************************************************************         
CKCSMKTS NTR1  BASE=*,LABEL=*                                                   
         OC    CSMKT,CSMKT         ANY INPUT FOR COMSCORE MARKET?               
         JNZ   CKCSM050            YES                                          
         OC    RSM1,RSM1           NO, ANY NSI MARKET #?                        
         JZ    CKCSMYES                NO, WE CAN LEAVE IT NULLS                
*                                                                               
         LARL  R2,CSMKTS                                                        
CKCSM010 OC    0(2,R2),0(R2)       EOT?                                         
         JZ    CKCSMNO             YES, WE DON'T HAVE A MATCH                   
*                                                                               
         CLC   0(L'RSM1,R2),RSM1   MATCH ON NSI MKT?                            
         JE    CKCSM020                                                         
         LA    R2,L'CSMKTS(R2)     NO, CHECK THE NEXT ENTRY                     
         J     CKCSM010                                                         
*                                                                               
CKCSM020 MVC   CSMKT,2(R2)         YES, COPY MATCHING COMSCORE MKT#             
         XR    R1,R1                                                            
         ICM   R1,3,CSMKT                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  MKMCSMK,DUB                                                      
         OI    MKMCSMKH+6,X'80'    TRANSMIT THE COMSCORE MKT FIELD              
         J     CKCSMYES                                                         
***************                                                                 
* COMSCORE MARKET NUMBER WAS FILLED IN                                          
***************                                                                 
CKCSM050 OC    RSM1,RSM1           ANY NSI MARKET?                              
         JZ    CKCSMNO             NONE, CAN'T VALIDATE COMSCORE MKT #          
*                                                                               
         LARL  R2,CSMKTS                                                        
CKCSM060 OC    0(2,R2),0(R2)       EOT?                                         
         JZ    CKCSMNO             YES, WE DON'T HAVE A MATCH                   
*                                                                               
         CLC   0(L'RSM1,R2),RSM1   MATCH ON NSI MKT?                            
         JE    CKCSM070                                                         
         LA    R2,L'CSMKTS(R2)     NO, CHECK THE NEXT ENTRY                     
         J     CKCSM060                                                         
*                                                                               
CKCSM070 CLC   CSMKT,2(R2)         YES, CHECK AGAINST INPUT                     
         JNE   CKCSMNO                  NOT THE SAME, ERROR                     
*                                                                               
CKCSMYES J     YES                                                              
*                                                                               
CKCSMNO  J     NO                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*============================ FIND MARKET ============================*         
*                                                                               
FNDMKT   NTR1  BASE=*,LABEL=*                                                   
*         VALIDATES ALPHA-MARKET CODES.  ON EXIT, CC=0 IF NOT VALID.            
*         R1-->MARKET CODE ON ENTRY.                                            
*                                                                               
         XC    MYKEY,MYKEY                                                      
         LA    R3,MYKEY                                                         
         USING CTDMREC,R3                                                       
         MVI   CTDMKTYP,CTDMKTEQ                                                
         MVI   CTDMKTY2,CTDMKT2E                                                
         MVC   CTDMKMED,QMED                                                    
         CLI   SVAPROF+7,C'C'      CANADIAN TV ALPHAMKT MEDIA IS 'C'            
         BNE   *+8                                                              
         CLI   QMED,C'T'                                                        
         BNE   *+8                                                              
         MVI   CTDMKMED,C'C'                                                    
         MVC   CTDMKMKT,0(R1)                                                   
         LA    R4,1                FOUND- CC= NON-ZERO                          
         L     R5,AIO2                                                          
*                                                                               
         MVI   CTDMKSRC,C'A'                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI  ',=C'CTFILE ',MYKEY,AIO2                 
         CLC   MYKEY(22),0(R5)                                                  
         BE    FNDMKX                                                           
*                                                                               
         CLI   SVAPROF+7,C'C'      CANADIAN                                     
         BNE   FNDMK5                                                           
         MVI   CTDMKSRC,C'M'       TRY BBM RADIO                                
         GOTO1 DATAMGR,DMCB,=C'DMRDHI  ',=C'CTFILE ',MYKEY,AIO2                 
         CLC   MYKEY(22),0(R5)                                                  
         BE    FNDMKX                                                           
*                                                                               
FNDMK5   MVI   CTDMKSRC,C'N'       NEILSON?                                     
         GOTO1 DATAMGR,DMCB,=C'DMRDHI  ',=C'CTFILE ',MYKEY,AIO2                 
         CLC   MYKEY(22),0(R5)                                                  
         BE    FNDMKX                                                           
*                                                                               
         DROP  R3                                                               
         SR    R4,R4               NOT FOUND                                    
*                                                                               
FNDMKX   LTR   R4,R4                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VTALMKT - VALIDATE IF THE TALENT MARKET EXISTS IN THE TALENT SYSTEM *         
***********************************************************************         
VTALMKT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   ERROR,0                                                          
***      GOTO1 SWITCH,DMCB,=C'TAL',0                                            
         MVI   DMCB,TAL1SE         SET TO SWITCH TO TAL1                        
         MVC   DMCB+1(3),=X'FFFFFF'                                             
         GOTO1 SWITCH,DMCB,,0                                                   
         CLI   DMCB+4,2            TEST TAL SYSTEM STARTED?                     
         BE    VTM20               YES                                          
         CLI   DMCB+4,0            TEST SWITCH WAS SUCCESSFUL                   
         BE    VTM30               YES                                          
VTM20    MVI   ERROR,255                                                        
         B     VTM90                                                            
*                                                                               
VTM30    XC    MYKEY,MYKEY                                                      
         LA    RF,MYKEY                                                         
         USING TLMTD,RF                                                         
         MVI   TLMTCD,TLMTCDQ      SEARCH MARKET RECORDS                        
         MVC   TLMTTYPE,QMED                                                    
         MVC   TLMTCODE(L'MKMTALM),MKMTALM                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI  ',=C'TALDIR ',MYKEY,AIO2                 
         L     R5,AIO2                                                          
         CLC   MYKEY(TLMTINUM-TLMTD),0(R5)    FIND IT?                          
         BE    VTM90                                                            
         CLI   QMED,C'T'    IF MEDIA TV, COULD BE A CSYS CODE                   
         BNE   VTM50                                                            
*                                                                               
         XC    MYKEY,MYKEY                                                      
         LA    RF,MYKEY                                                         
         MVI   TLMTCD,TLMTCDQ      SEARCH CSYS RECORDS                          
         MVI   TLMTTYPE,C'S'       MEDIA=S                                      
         MVC   TLMTCODE(L'MKMTALM),MKMTALM                                      
         GOTO1 DATAMGR,DMCB,=C'DMRDHI  ',=C'TALDIR ',MYKEY,AIO2                 
         L     R5,AIO2                                                          
         CLC   MYKEY(TLMTINUM-TLMTD),0(R5)    FIND IT?                          
         BE    VTM90                                                            
*                                                                               
         XC    MYKEY,MYKEY                                                      
         LA    RF,MYKEY                                                         
         USING TLMTPD,RF                                                        
         MVI   TLMTPCD,TLMTALDQ     SEARCH CSYS RECORDS                         
         MVC   TLMTALAL,MKMTALM                                                 
         GOTO1 DATAMGR,DMCB,=C'DMRDHI  ',=C'TALDIR ',MYKEY,AIO2                 
         L     R5,AIO2                                                          
         CLC   MYKEY(TLMTALAL-TLMTPD),0(R5)    FIND IT?                         
         BE    VTM90                                                            
*                                                                               
VTM50    MVI   ERROR,255           NO                                           
         DROP  RF                                                               
*                                                                               
VTM90    GOTO1 SWITCH,DMCB,=C'SPOT',0                                           
         CLI   DMCB+4,0            TEST SWITCH WAS SUCCESSFUL                   
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         CLI   ERROR,0                                                          
         BE    VTMX                                                             
         MVC   CONHEAD(L'LAERRTM),LAERRTM                                       
         J     GENERR                                                           
LAERRTM  DC    C'** ERROR ** TALENT MARKET NOT FOUND'                           
*                                                                               
VTMX     XIT1                                                                   
*                                                                               
TAL1SE   EQU   X'10'                                                            
TAL2SE   EQU   X'20'                                                            
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*======================= MARKET RECORDS DSECT =======================*          
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*======================== 'L' RECORDS DSECT =========================*          
       ++INCLUDE SPGENANMK                                                      
       ++INCLUDE SPGENSDEF                                                      
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*========================== USEFUL DSECTS ===========================*          
*       ++INCLUDE CTGENFILE                                                     
*       ++INCLUDE DDBIGBOX                                                      
*       ++INCLUDE DDCOMFACS                                                     
*       ++INCLUDE DDSPLWORKD                                                    
*       ++INCLUDE DDSPOOLD                                                      
*       ++INCLUDE FASYSFAC                                                      
*       ++INCLUDE FAPGMLST                                                      
*       ++INCLUDE FASELIST                                                      
*       ++INCLUDE FAFACTS                                                       
*       ++INCLUDE FALANG                                                        
*       ++INCLUDE FATIOB                                                        
*       ++INCLUDE FASYSLSTD                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FASYSFAC                                                       
       ++INCLUDE FAPGMLST                                                       
       ++INCLUDE FASELIST                                                       
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FALANG                                                         
       ++INCLUDE FATIOB                                                         
       ++INCLUDE FASYSLSTD                                                      
       ++INCLUDE DDFH                                                           
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DEDEMTABD                                                      
*                                                                               
       ++INCLUDE TAGENFILE                                                      
         PRINT ON                                                               
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*=============================== TWA ================================*          
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
*                                                                               
*--------------------------------------------------------------------*          
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMD2D          TWA DSECT FOR RECORD MAINTENANCE.            
*                                                                               
*--------------------------------------------------------------------*          
*                                                                               
       ++INCLUDE DDGENTWA                                                       
**********************************************************************          
         EJECT                                                                  
**********************************************************************          
*========================= SPSFM WORK AREA ==========================*          
       ++INCLUDE SPSFMWORKD                                                     
         SPACE 5                                                                
*                                                                               
*--------------- PUT MY STORAGE DSECT HERE IF NEEDED ----------------*          
*                                                                               
         ORG   SYSSPARE                                                         
*                                                                               
** STORAGE USED BY SPSFM33 AS WELL                                              
*                                                                               
MYSVKEY  DS    CL(L'KEY)                                                        
OPTFLAG  DS    CL1                                                              
OPTLANMQ EQU   X'80'               LIST BY A/N RECORDS.                         
*                                                                               
** CAN PUT ANY SAVE-STORAGE HERE                                                
*                                                                               
MKTCODE  DS    CL4                 MARKET CODE.                                 
MYAIO    DS    CL(L'AIO)           MY AIO SAVE AREA.                            
AMKT     DS    CL3                 ALPHA MARKET.                                
MYLSVKEY DS    CL(L'KEY)           MY LIST SAVE-KEY.                            
SOFTDEM  DS    CL1                 FOR CANADIAN SOFT DEMOS                      
*                                                                               
** WORKING STORAGE FOR MARKET-RECORD DATA                                       
*                                                                               
NAME     DS    CL24                                                             
ZONE     DS    CL1                                                              
CLASS    DS    CL1                                                              
RTG      DS    CL1                 RATING SERVICE OVERRIDE.                     
RANK     DS    CL3                                                              
HOMES    DS    CL8                                                              
REG      DS    CL3                                                              
NTA      DS    CL2                                                              
WEIGHT   DS    CL4                                                              
SHR      DS    CL4                                                              
RS1      DS    CL1                                                              
RSM1     DS    CL2                                                              
RS2      DS    CL1                                                              
RSM2     DS    CL2                                                              
CSMKT    DS    XL2                                                              
LTACC    DS    CL3                                                              
AMKTC    DS    CL6                                                              
CLAS1    DS    CL1                                                              
CLAS2    DS    CL1                                                              
NODST    DS    CL1                                                              
ALST     DS    CL27                                                             
UNIT     DS    XL2                                                              
LPMDATE  DS    XL2                                                              
DATETEMP DS    CL6                 TEMP DATE FOR DATVAL                         
CABLEDEM DS    CL1                 NSI=N, FUSION=F, NO DEMOS=C'0'               
RSVC     DS    XL1                 RATING SERVICE FOR CANADA                    
SVRSVC   DS    XL1                 SAVED RATING SERVICE FROM MKT REC            
SVALST   DS    XL3                 SAVED ALPHA MKT FROM MKT REC                 
UPDTSD   DS    XL1                 FORCE UPDATE ON SPILLDEF                     
TALM     DS    CL4                 TALENT MARKET                                
BOOKTYP  DS    XL1                 BOOKTYPE                                     
CBOKTYP  DS    XL1                 COMSCORE BOOKTYPE                            
*                                                                               
** MISCELLANEOUS WORKING STORAGE                                                
*                                                                               
AALST    DS    A                   A(ALPHA-MARKET LIST).                        
FNDX     DS    X                                                                
MEDIA    DS    CL1                                                              
NLINES   DS    X                   FOR USE W/. SCANNER.                         
PRIMARK  DS    CL3                 PRIMARY MARKET.                              
RELO     DS    F                   RELOCATION FACTOR.                           
TEMP     DS    X                                                                
MYKEY    DS    CL48                                                             
SCANTBL  DS    XL256                                                            
NOTAUTH  EQU   0175                ERROR-NOT AUTHORIZED FOR THIS FUNCTN         
**********************************************************************          
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'206SPSFM34   02/17/21'                                      
         END                                                                    
