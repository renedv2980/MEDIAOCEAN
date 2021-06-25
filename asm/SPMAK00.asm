*          DATA SET SPMAK00    AT LEVEL 073 AS OF 02/23/21                      
*PHASE T22800A                                                                  
*INCLUDE TWABLD                                                                 
*INCLUDE NSIWEEK     ---   NEEDED FOR METERED MKT WKLY                          
*                                                                               
***********************************************************************         
* USER     JIRA      DATE                 CHANGE LOG                  *         
* ---- ----------- -------- ----------------------------------------- *         
* AKAT SPEC-48438  02/23/20 SUPPORT 2-DECIMAL RADIO RATINGS FOR CANADA*         
* AKAT SPEC-31824  07/15/19 TWO DECIMAL IMPRESSIONS SUPPORT           *         
***********************************************************************         
*===============================================================*               
*                                                               *               
* HISTORY                                                       *               
* -------                                                       *               
*                                                               *               
* WHEN    LEV WHAT                                              *               
* ----    --- ----                                              *               
* 21NOV13 071 SUPPORT CROSS NETWORK MAKEGOODS                   *               
* 07OCT13 070 LOAD AND STORE ADDRESS OF BLDMGN                  *               
* 24JAN12 069 FLAG TO NOT OVERRIDE SPOT COST (FOR TOLERANCE)    *               
* 02NOV11 068 CHANGE PROFILE TYPE FROM BIN TO CHR               *               
* 09SEP11 067 SUPPORT PROGRAM NAME FEATURE AND COST TOLERANCE   *               
*         --- NEW DOWNLOAD OF SUBSTITUTE BRANDS FROM CMML REC   *               
* 01MAR11 066 2-BYTE BUYLINE SUPPORT                            *               
* 01DEC10 065 SUPPORT OVERNIGHT PROGRAM NAMES                   *               
*         --- NEW PROFILE VALUE                                 *               
* 19FEB10 064 SUPPORT MATCH INV SLN TO CMML SLN                 *               
* 03JUN09 063 FIX SEARCH SPOTS FOR $0 COSTS                     *               
* 13FEB09 062 SUPPORT FOR RATE TYPES                            *               
* 24OCT08 061 ADD CMLSEQ NUMBER AND STATUS TO DOWNLOAD          *               
* 05AUG08 060 SEND PROFILE VALUES 33 & 34 AS CHAR, NOT BIN      *               
* 12JAN08 059 SUPPORT NEW OVERRIDE FOR INTERVAL CHECKING        *               
* 13AUG07 058 READ I2N PROFILE                                  *               
* 28OCT06 057 SUPPORT SEARCH FOR INVOICES                       *               
* 05MAY06 056 ADD ORB DEMO TO H07 RECORD                        *               
*         --- NEW AUTO I2 PROFILE                               *               
*         --- NEW MAPS FOR INVOICE SEARCH FUNCTIONS             *               
*         --- SUPPORT IGNORE SPOT LENGTH FLAG                   *               
*         --- SUPPORT NEW ERROR FLAGS                           *               
* 06DEC05 055 PROGRAM EXCHANGE FEATURES                         *               
* 30AUG05 054 ADDED VALUE PROFILES                              *               
* 27JUN05 052 2 DECIMAL DEMOS ONLY FOR USTV                     *               
* 25JAN05 051 NEW GETPROF OPTION TO REALLY GET AGY LEVEL PROF   *               
* 11NOV04 050 SUPPORT CHANGE RNO COST                           *               
* 10NOV04 049 ADD NET COST IN H06                               *               
* 09JUL04 048 EARNED DISCOUNT/COST2                             *               
*         --- 2 DECIMAL DEMOS                                   *               
* 21JUN04 047 SUPPORT AD-ID                                     *               
* 26APR04 046 FIX BOOK=ACT CODE                                 *               
* 18SEP03 045 NO MORE PRISONERS ON BAD PRD CODES!               *               
*         --- REMOVE ALL *+4                                    *               
* 15JUL03 044 FIX SPECIAL XOTO CODE                             *               
* 13MAY03 043 VERSION 3.1                                       *               
* 16JAN03 042 USE PROFMK+4 ONLY FROM AGY LEVEL REC              *               
* 17DEC02 041 FIX BOOK VALIDATION                               *               
* 24JUN02 040 VERSION 3.0                                       *               
*         --- MAP CODE FOR MAKEGOOD WITH NO AFFID DATA          *               
*         --- MAP CODES FOR APPROVE/UN-APPROVE                  *               
*         --- MAP CODES MAKEGOOD ANLYSIS (BLDMGA)               *               
*         --- ADD START/END PERIOD TO H31                       *               
*         --- MAP CODES FOR GET PROGRAM NAME                    *               
* 18JUN02 039 SKIP LIMIT ACCES VALIDATION FOR CLT/MKT           *               
* 21MAY02 038 GET I2Z PROFILE                                   *               
* 20NOV01 037 ALWAYS LEAVE ROOM FOR PRD2 IN BDATA 'T' ELEMS     *               
*         --- NEW PROFILE MAP CODES                             *               
*         --- I2 DATE AND TIME MAP CODES                        *               
*         --- H0E RECORD FOR REASON CODES                       *               
*         --- DON'T CALL INPUT RTNS FOR 0 LEN INPUT             *               
* 30OCT01 036 NOP CODE FROM LEVELS 33-35                        *               
* 29OCT01 034 GET UTL TO BLOW ON TRATE > 250                    *               
* 29OCT01 033 COUNT CALLS TO BUY AND DIE OVER 50 (SPEC. DEBUG)  *               
* 20SEP01 032 MAKE SURE NETWORK DOESTN'T CHANGE!                *               
*         --- CHECK GLOBBER RETURN CODES                        *               
*         --- FIX LENGTH OF H06/16 (NPW) ITEM                   *               
* 07SEP01 031 CHANGE VSTA ERROR MESSAGE FOR UNAUTH MKT          *               
* 29MAY01 029 USE RIGHT PRD RTNS TO GET PIGGY STUFF TO WORK     *               
*         --- CLEAR QBOOK AT INIT                               *               
* 22MAY01 028 DMFILEGO SHOULD GO TO DMCHECK, NOT EXIT           *               
*             ALWAYS RE-READ PROFILES ON WAY IN                 *               
*                                                               *               
*===============================================================*               
*                                                                               
T22800   TITLE 'SPMAK00 - SPOT MATCHMAKER - BASE'                               
T22800   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 WORKX-WORKD,T22800,RR=R2,CLEAR=YES                               
         USING WORKD,RC                                                         
         SPACE 1                                                                
*=================================================================*             
* INITIALIZATION CODE *                                                         
*=================================================================*             
         SPACE 1                                                                
         ST    R2,BASERELO                                                      
         ST    RD,BASERD                                                        
         ST    R1,ASYSPARM                                                      
         MVC   ATIOB,0(R1)                                                      
         MVC   ATWA,4(R1)                                                       
         MVC   ASYSFACS,8(R1)                                                   
         MVC   ATIA,12(R1)                                                      
         MVC   ACOMFACS,16(R1)                                                  
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
*                                                                               
         MVI   DDS,C'N'                                                         
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,C'Y'                                                         
*                                                                               
         LR    RE,RC                                                            
         AHI   RE,IOAREA1-WORKD                                                 
         ST    RE,AIO                                                           
         ST    RE,AIO1                                                          
         AHI   RE,LENIO                                                         
         ST    RE,AIO2                                                          
         AHI   RE,LENIO                                                         
         ST    RE,AIO3                                                          
         AHI   RE,LENIO                                                         
         ST    RE,AIO4                                                          
*                                                                               
         L     RE,=V(TWABLD)                                                    
         A     RE,BASERELO                                                      
         ST    RE,VTWABLD                                                       
*                                                                               
         L     RE,ASYSPARM                                                      
         L     RE,8(RE)                                                         
         USING SPSYSFAC,RE                                                      
         MVC   VRECUP,SRECUP                                                    
         DROP  RE                                                               
*                                                                               
         L     R1,ACOMFACS                                                      
         USING COMFACSD,R1                                                      
         MVC   VDATAMGR,CDATAMGR                                                
         MVC   VCALLOV,CCALLOV                                                  
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
         MVC   VGETFACT,CGETFACT                                                
         DROP  R1                                                               
*                                                                               
         SPACE 1                                                                
*===================================================================*           
* SET UP ADDRESSES FOR CORE-RESIDENT PHASES                                     
*===================================================================*           
         SPACE 1                                                                
         L     R2,=A(PHASES)       R2=A(PHASE LIST)                             
         A     R2,BASERELO                                                      
         LA    R3,APHASES          R3=A(ADDRESS LIST)                           
         LA    R4,PHASESN          R4=MAX NUMBER OF PHASES (CORERES)            
         SR    R0,R0                                                            
         ICM   R0,14,=X'D9000A'                                                 
         LA    R1,DMCB                                                          
         L     RF,VCALLOV                                                       
*                                                                               
INIT02   ICM   R0,1,0(R2)          ANY ENTRY HERE?                              
         BZ    INIT04              NONE, SKIP TO THE NEXT ENTRY                 
*                                                                               
         GOTO1 (RF),(R1),0,(R0)                                                 
         MVC   0(4,R3),0(R1)                                                    
*                                                                               
INIT04   LA    R2,1(R2)            BUMP TO THE NEXT ENTRY                       
         LA    R3,4(R3)                                                         
         BCT   R4,INIT02                                                        
*                                                                               
         LR    RE,RB                                                            
         AHI   RE,VCOMMON-T22800                                                
         LA    R0,VCOMBASN                                                      
         SR    RF,RF                                                            
         LA    R1,READ                                                          
INIT10   DS    0H                                                               
         ST    RE,0(R1)                                                         
         STC   RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,INIT10                                                        
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         EJECT                                                                  
*                                                                               
         BRAS  RE,GETPROF                                                       
         XC    SVRCVEL,SVRCVEL     CLEAR LAST RECEIVE ELEMENT                   
         XC    CMTCOUNT,CMTCOUNT   CLEAR COMMENT COUNTER                        
         XC    QDEST,QDEST                                                      
*                                                                               
         CLI   SVXFROV,0           TEST RETURN FROM GLOBBER                     
         BNE   INIT30               YES - DON'T RESET THESE!                    
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'CLEAR'  MAKE SURE NOTHING IN GLOBBER            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
         XC    QMGCD,QMGCD                                                      
         XC    QCOST,QCOST                                                      
         XC    QCOST2,QCOST2                                                    
         MVI   BNOGOL,C'N'         DEFAULT NOGOAL OPTION=N                      
         MVI   BNODEM,C'N'         DEFAULT NODEM OPTION=N                       
         MVI   BSKED,0             DON'T FORCE SKED                             
         XC    BYCBLNET,BYCBLNET                                                
         XC    IVCBLNET,IVCBLNET                                                
         MVI   INVBCNET,0                                                       
         XC    SVDPNTRD,SVDPNTRD                                                
         XC    QREP,QREP                                                        
         XC    FLAGS,FLAGS                                                      
         XC    QBUYID,QBUYID                                                    
         XC    BBCBLNET,BBCBLNET                                                
         XC    APRD2,APRD2                                                      
         XC    QBOOK,QBOOK                                                      
         XC    BPER,BPER                                                        
         XC    XOTOCODE,XOTOCODE                                                
         MVI   AFCOST,FF           SET AFFID COST NOT SENT                      
         XC    OVFLAGS,OVFLAGS                                                  
         XC    STAT1,STAT1                                                      
*                                                                               
INIT30   LA    R0,RH6                                                           
         LA    R1,RH6L                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    RE,BDATA                                                         
         ST    RE,DPOINTER                                                      
*                                                                               
         BAS   RE,INIFALNK         INITIALIZE FALINK BLOCK                      
         GOTO1 VFALINK,DMCB,FABLK  GIVE FALINK CONTROL                          
         MVI   BUY1OR2,0           NOT INITIALIZED YET                          
*                                                                               
EXIT     DS    0H                  JUST EXIT                                    
         XIT1                                                                   
         EJECT                                                                  
*====================================================================*          
* INITIALIZE FALINK                                                             
*====================================================================*          
         SPACE 1                                                                
INIFALNK NTR1                                                                   
         CLI   SVXFROV,0           TEST RETURN FROM GLOBBER                     
         BNE   INI2                                                             
         LA    R0,FABLK                                                         
         LA    R1,FALINKDL                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,BDATA                                                         
         LHI   R1,L'BDATA                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    SVOLDRCV,SVOLDRCV                                                
         XC    SVREASON,SVREASON                                                
*                                                                               
INI2     MVC   SVRESUME,SVXFROV    SAVE THE GLOBBING OVERLAY NUMBER             
         MVI   SVXFROV,0           AND CLEAR THIS FLAG NOW !                    
*                                                                               
         OI    MAKSERVH+1,X'01'    SERVICE REQUEST ALWAYS MODIFIED              
         OI    MAKSERVH+6,X'80'                                                 
*                                                                               
                                                                                
         LA    R2,FABLK                                                         
         ST    R2,AFABLK           FOR OTHER OVERLAYS                           
         USING FALINKD,R2                                                       
         LA    R1,MAKINPH          SET A(FIRST SCREEN POSITION)                 
         ST    R1,FALABLD                                                       
         MVC   FALTBLD,VTWABLD     A(TWABLD)                                    
*                                                                               
         L     R1,ACOMFACS             A(SWITCH)                                
         L     R1,CSWITCH-COMFACSD(R1)                                          
         ST    R1,FALASWCH                                                      
*                                                                               
         L     R0,=A(RECEIVE)                                                   
         A     R0,BASERELO                                                      
         ST    R0,FALARCV                                                       
*                                                                               
         L     R0,=A(SEND)                                                      
         A     R0,BASERELO                                                      
         ST    R0,FALASND                                                       
*                                                                               
         L     R0,=A(BREAK)                                                     
         A     R0,BASERELO                                                      
         ST    R0,FALASTP                                                       
*                                                                               
         L     R0,=A(RESUME)                                                    
         A     R0,BASERELO                                                      
         ST    R0,FALARSM                                                       
*                                                                               
         XC    FAMSGBLK,FAMSGBLK                                                
         LA    R1,FAMSGBLK             A(MESSAGE BLOCK)                         
         ST    R1,FALAMSG                                                       
*                                                                               
         LA    R1,FACON                A(CONTROL FIELD BUFFER)                  
         ST    R1,FALACON                                                       
         L     R1,ATWA                                                          
         AH    R1,=Y(SVFALINK-TWAD) A(FALINK SAVED STORAGE)                     
         ST    R1,FALASVE                                                       
*                                                                               
         L     R0,=A(FAMAP)            A(MAP TABLE)                             
         A     R0,BASERELO                                                      
         ST    R0,FALAMAP                                                       
         ST    R0,AMAPTAB          FOR OTHER OVERLAYS                           
*                                                                               
         MVC   FALAPGS,TWAPGS                                                   
         B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
TWAPGS   DC    AL4(FALATMS)                                                     
         EJECT                                                                  
*====================================================================           
* LITERALS AND CONSTANTS                                                        
*====================================================================           
FF       EQU   X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
BREAK    NTR1  BASE=*,LABEL=*                                                   
         CR    RB,RB                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
* ON RETURN FROM AN OVERLAY THAT WANTS TO EXIT VIA GLOBBER,         *           
* WE RETURNED TO FALINK WITH AN FAGLB, FAGLB CALL.                  *           
* WHEN CALLED PROGRAM RETURNS TO US, THIS EXIT IS CALLED.           *           
* ON EXIT FROM HERE, AND SVRESUME IS THE OVERLAY TO BE CALLED.      *           
*====================================================================           
         SPACE 1                                                                
RESUME   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',BLOCK,24,GLVXCTL                          
         TM    DMCB+8,X'10'                                                     
         BZ    RSM2                NO CONTROL ELEM                              
         CLI   *,FF                SET CC LOW                                   
         BRAS  RE,EXIT                                                          
         DC    H'0'                NO RETURN HERE                               
*                                                                               
RSM2     GOTO1 VGLOBBER,DMCB,=C'DELE',,,GLVXCTL                                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DCHO                                                                   
*                                                                               
G        USING GLVXFRSY,BLOCK                                                   
         CLC   =C'SPOBUY',G.GLVXFRSY  FROM SPOT BUY ?                           
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  G                                                                
*                                                                               
         GOTO1 (RF),(R1),=C'GETD',BLOCK,78,GLVBUY1                              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    BLOCK(4),BLOCK      ERROR RETURNED                               
         BZ    RSM10               NO                                           
*                                                                               
* MAKE SURE DELETE ALL GLOBBER ELEMS ON ERROR                                   
         GOTO1 (RF),(R1),=C'CLEAR'   MAKE SURE NOTHING IN GLOBBER               
*                                                                               
         LA    R4,FAMSGBLK                                                      
         USING FAMSGD,R4                                                        
         MVC   FAMSGNO,BLOCK       MESSAGE NUMBER                               
         MVC   FAMSGTYP,BLOCK+2    TYPE                                         
         MVC   FAMSGSYS,BLOCK+3    SYSTEM                                       
         MVC   FAMSGXTR(7),BLOCK+4                                              
         CLI   *,FF                SET CC LOW                                   
         B     RSMX                                                             
*                                                                               
RSM10    CR    RB,RB               EXIT WITH CC =                               
*                                                                               
RSMX     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
SEND     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   ASETELEM,0(R1)      SAVE FALINK ROUTINE ADDRESSES                
         MVC   AADDDATA,4(R1)                                                   
*                                                                               
         CLI   SVRESUME,0          TEST XFRCTL RETURN                           
         BE    SEND10                                                           
         LA    R4,SVRESUME-2       POINT 2 BYTES BEFORE OVERLAY NUM             
         B     SEND20                                                           
*                                                                               
SEND10   LA    R4,SOVTAB                                                        
         LA    R5,(SOVTABX-SOVTAB)/L'SOVTAB                                     
SEND12   CLC   SVRCVEL,0(R4)       MATCH FIRST RCV ELCODE3                      
         BE    SEND20                                                           
         LA    R4,L'SOVTAB(R4)                                                  
         BCT   R5,SEND12                                                        
         DC    H'0'                                                             
*                                                                               
SEND20   CLI   2(R4),X'FF'         TEST NOT TO CALL ANYTHING                    
         BE    SEND40              ALAN SAYS A ZED WILL GO OUT !                
*                                                                               
*  FOR ACTION APPROVE/UN-APPROVE, WE NEED TO FAKE OUT SENDING                   
*  THE INVOICE DETAILS SO THAT SVNVKEY GETS SET                                 
         CLI   SVRCVEL+1,H41Q      APPROVE OR UN-APPROVE?                       
         BE    *+12                                                             
         CLI   SVRCVEL+1,H42Q                                                   
         BNE   SEND25                                                           
         XC    DMCB(4),DMCB                                                     
         MVI   DMCB,X'10'                                                       
         GOTO1 VCALLOV,DMCB,,ATWA                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DCHO                                                                   
         L     RF,0(R1)                                                         
         LR    R1,RC                                                            
         BASR  RE,RF                                                            
*                                                                               
SEND25   XC    DMCB(4),DMCB                                                     
         MVC   DMCB(1),2(R4)       MOVE OVERLAY NUMBER                          
         GOTO1 VCALLOV,DMCB,,ATWA                                               
*                                                                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,0(R1)                                                         
         LR    R1,RC                                                            
         BASR  RE,RF                                                            
*                                                                               
         MVC   SVRESUME,SVXFROV    SAVE THIS OVERLAY NUMBER                     
         CLI   SVXFROV,0           TEST OVLY REQUESTED GLOBBER CALL             
         BE    SEND30                                                           
         GOTO1 AADDDATA,DMCB,AFABLK,FALAGLB,FALAGLB                             
         B     SENDX                                                            
*                                                                               
SEND30   CLI   ANYDATA,C'Y'        ANY DATA IN BUFFER?                          
         BZ    SENDX               NO - DON'T CLOSE                             
*                                                                               
SEND40   GOTO1 AADDDATA,DMCB,AFABLK,FALADNE,FALADNE,0                           
*                                                                               
SENDX    CR    RB,RB                                                            
         XIT1                                                                   
         SPACE 1                                                                
*==================================================================*            
* SEND OVERLAY LOOKUP TABLE                                        *            
* ENTRIES ARE                                                      *            
*        DS    AL2(FIRST RCV EL CODE)                              *            
*        DS    XL1(SEND OVERLAY NUMBER)                            *            
*        DS    XL1(SPARE)                                          *            
*==================================================================*            
         SPACE 1                                                                
SOVTAB   DS    0AL4                                                             
         DC    AL2(H30Q),X'01',X'00' SEARCH FOR HEADERS REQUEST                 
         DC    AL2(H31Q),X'01',X'00' DOWNLOAD HEADERS REQUEST                   
         DC    AL2(H32Q),X'10',X'00' MATCH INFO REQUEST                         
         DC    AL2(H33Q),X'20',X'00' CHANGE TIME/COST                           
         DC    AL2(H34Q),X'20',X'00' COMMENT REQUEST                            
         DC    AL2(H35Q),X'20',X'00' MAKEGOOD REQUEST                           
         DC    AL2(H36Q),X'20',X'00' MOVE SPOT                                  
         DC    AL2(H37Q),X'20',X'00' NEW BUY                                    
         DC    AL2(H38Q),X'20',X'00' +OTO                                       
         DC    AL2(H39Q),X'20',X'00' -OTO                                       
         DC    AL2(H3AQ),X'30',X'00' REQUEST/COMMENTS                           
         DC    AL2(H3BQ),X'20',X'00' MATCHED                                    
         DC    AL2(H3CQ),X'20',X'00' UNMATCH                                    
         DC    AL2(H3DQ),X'20',X'00' EMAIL RNO SPOTS                            
         DC    AL2(H3EQ),X'20',X'00' CHANGE SPOT COST                           
         DC    AL2(H3FQ),X'10',X'00' SEARCH FOR SPOTS REQUEST                   
         DC    AL2(H40Q),X'20',X'00' BUY HISTORY REQUEST                        
         DC    AL2(H41Q),X'20',X'00' APPROVE                                    
         DC    AL2(H42Q),X'20',X'00' UN-APPROVE                                 
         DC    AL2(H43Q),X'20',X'00' MAKEGOOD ANALYSIS                          
         DC    AL2(H44Q),X'20',X'00' MAKEGOOD ACROSS MONTHS                     
         DC    AL2(H45Q),X'20',X'00' CHANGE COMMENTS                            
         DC    AL2(H46Q),X'30',X'00' PROGRAM NAME LOOKUP                        
         DC    AL2(H47Q),X'20',X'00' CHANGE RNO DATA                            
         DC    AL2(H48Q),X'20',X'00' SAME AS H33Q BUT USED OVERRIDES            
*                                                                               
         DC    AL2(H99Q),X'40',X'00' TEST PHASE                                 
*                                                                               
* FD & FE CURRENTLY GET IGNORED                                                 
         DC    AL2(HFDQ),X'FF',X'00' VERSION CODES                              
         DC    AL2(HFEQ),X'FF',X'00' VERSION CODES                              
SOVTABX  EQU   *                                                                
         LTORG                                                                  
         EJECT                                                                  
*===================================================================*           
* CONTROL RECEIVED HERE WHEN FALINK HAS RECEIVED DATA               *           
*===================================================================*           
         SPACE 1                                                                
RECEIVE  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   AGETDATA,0(R1)      SAVE FALINK ROUTINE ADDRESS                  
*                                                                               
RCV10    GOTO1 AGETDATA,DMCB,FABLK,FPARMS                                       
         BL    RCVERR              FALINK ERROR                                 
         BH    RCVX                END OF DATA                                  
*                                                                               
         CLI   FPARMS,0            HEADER?                                      
         BNE   RCV20                                                            
         SPACE 1                                                                
*====================================================================           
* PRCHDR - PROCESS HEADER ELEMENT                                               
*====================================================================           
         SPACE 1                                                                
         L     R6,FPARMS                                                        
         USING MHELD,R6            R6=A(HEADER ENTRY)                           
*                                                                               
         OC    SVRCVEL,SVRCVEL     TEST FIRST ELEMENT                           
         BNZ   RCV10                                                            
         CLC   MHCODE,=X'00FE'     IGNORE FE/FD ELEMS                           
         BE    RCV10                                                            
         CLC   MHCODE,=X'00FD'                                                  
         BE    RCV10                                                            
         MVC   SVRCVEL,MHCODE      SAVE FIRST RECEIVE ELEMENT                   
         B     RCV10                                                            
*                                                                               
* THIS CODE TO PREVENT TRAUMA WHEN PC GETS LOST AFTER A DUMP AND TRIES          
* TO CONTINUE WHERE IT LEFT OFF                                                 
*                                                                               
* CODE NOP'D 09JAN07 BECAUSE IT DOESN'T WORK VERY WELL WITH THE NEW             
* SEARCH FEATURE (QGRP WILL NEVER HAVE A VALUE).  THIS PROBLEM WAS              
* CORRECTED ON THE PC A FEW YEARS AGO.  IF THE PROGRAM EVER BEGINS              
* TO DUMP 2X FOR SOME DUMPS, PUT THIS BACK IN (IN SOME OTHER FORM)              
*                                                                               
*&&DO                                                                           
         CLI   SVRCVEL+1,H31Q      DOWNLOAD HEADERS REQUEST?                    
         BE    RCV10                YES                                         
         CLC   QGRP,SPACES                                                      
         BH    RCV10                                                            
         MVC   ERROR,=Y(INTERR)                                                 
         B     RCVERR                                                           
*&&                                                                             
*                                                                               
         EJECT                                                                  
*====================================================================           
* PROCESS DATA FIELD                                                            
*====================================================================           
         SPACE 1                                                                
RCV20    L     R6,FPARMS                                                        
         USING MDELD,R6            R6=A(DATA ENTRY)                             
*                                                                               
         L     R4,FPARMS+4         GET DATA ADDRESS                             
         ICM   R5,15,FPARMS+8      GET DATA LENGTH                              
         BNZ   RCV30                                                            
* SOME ROUTINES EXPECT ELEMS WITH NO DATA, SUCH AS INQCOM AND INH6CMT           
* FOR THEM, THE TEXT IDENTIFIER IS A FLAG TO ALLOW ZERO DATA LENGTH             
         CLC   MDTEXT,=C'ZZCOM'                                                 
         BNE   RCV10               DON'T CALL IF 0 LENGTH INPUT                 
*                                                                               
RCV30    BCTR  R5,0                SET FOR EX                                   
*                                                                               
         ICM   RF,15,MDUSER        GET PROCESS DATA ROUTINE ADDRESS             
         A     RF,BASERELO                                                      
         BASR  RE,RF               NO RETURN EXPECTED - TRACE USE ONLY          
         DC    H'0'                                                             
*                                                                               
RCVX     CR    RB,RB                                                            
         XIT1                                                                   
*                                                                               
RCVERR   GOTO1 SENDMSG             RETURN FALINK ERROR                          
         DROP  R6                                                               
         EJECT                                                                  
*=================================================================*             
* DATA RECEIVE ROUTINES                                           *             
*=================================================================*             
         SPACE 1                                                                
DUMMY    B     RCV10               DUMMY ROUTINE - DO NOTHING                   
*                                                                               
INVERFD  MVC   VERSION,0(R4)       VERSION DATA (HFDQ)                          
         OI    FLAGS,FLSKVRSN                                                   
         B     RCV10                                                            
*                                                                               
INVERSN  MVC   VERSION,0(R4)       VERSION DATA                                 
         B     RCV10                                                            
*                                                                               
INBUYID  MVC   QBUYID,SPACES                                                    
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   QBUYID(0),0(R4)                                                  
         B     RCV10                                                            
*                                                                               
INGRP    MVC   QGRP,SPACES                                                      
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   QGRP(0),0(R4)                                                    
         B     RCV10                                                            
*                                                                               
INCMNT   CLI   0(R4),C'Y'          SEND COMMENTS WITH BUY DATA?                 
         BNE   RCV10                NO                                          
         OI    FLAGS,FLSNDCMT                                                   
         B     RCV10                                                            
*                                                                               
INMATCH  CLI   0(R4),C'Y'          SEND MATCHED DATA?                           
         BNE   RCV10                NO                                          
         OI    FLAGS,FLSNDMAT                                                   
         B     RCV10                                                            
*                                                                               
INNOMAT  CLI   0(R4),C'Y'          DON'T PERFORM MATCH                          
         BNE   RCV10                                                            
         OI    FLAGS,FLNOMAT                                                    
         B     RCV10                                                            
*                                                                               
INBYR    MVC   QBYR,SPACES                                                      
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   QBYR(0),0(R4)                                                    
         OC    QBYR,SPACES                                                      
         B     RCV10                                                            
*                                                                               
INMED    MVC   QMED,0(R4)                                                       
         GOTO1 VALIMED                                                          
         BRAS  RE,GETPROF          GET AGY LEVEL PROFILES                       
         B     RCV10                                                            
*                                                                               
INMKT    SR    R0,R0                                                            
         ICM   R0,3,0(R4)                                                       
         STCM  R0,3,BMKT                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QMKT,DUB                                                         
         B     RCV10                                                            
*                                                                               
INSTA    EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   QSTA(0),0(R4)                                                    
         OC    QSTA,SPACES                                                      
         GOTO1 VALISTA                                                          
         B     RCV10                                                            
*                                                                               
INMOS    GOTO1 VDATVAL,DMCB,(2,(R4)),DUB                                        
         GOTO1 VDATCON,DMCB,DUB,(3,FULL)                                        
         MVC   BMOS(2),FULL                                                     
*                                                                               
         GOTO1 VGETBROD,DMCB,(1,DUB),QPER,VGETDAY,VADDAY                        
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 VDATCON,DMCB,QPER,(2,BPER)                                       
         GOTO1 (RF),(R1),,(3,QSTARTB)                                           
         GOTO1 (RF),(R1),QPER+6,(2,BPER+2)                                      
         GOTO1 (RF),(R1),,(3,QENDB)                                             
         B     RCV10                                                            
*                                                                               
INSTMOS  GOTO1 VDATVAL,DMCB,(2,(R4)),DUB                                        
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 VDATCON,DMCB,DUB,(2,BPER)                                        
         B     RCV10                                                            
*                                                                               
INEDMOS  GOTO1 VDATVAL,DMCB,(2,(R4)),DUB                                        
         MVC   DUB+4(2),=C'01'                                                  
         GOTO1 VDATCON,DMCB,DUB,(2,BPER+2)                                      
         B     RCV10                                                            
*                                                                               
INCLT    EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   QCLT,0(R4)                                                       
         OC    QCLT,SPACES                                                      
         GOTO1 VALICLT                                                          
*                                                                               
         CLI   SVRCVEL+1,H30Q      IS THIS A SEARCH?                            
         BNE   INCLT10              NO                                          
         XC    QSTA,QSTA                                                        
         XC    BSTA,BSTA                                                        
         MVC   QMKT,=C'0000'                                                    
         XC    BMKT,BMKT                                                        
         B     *+8                                                              
*                                                                               
INCLT10  BRAS  RE,CKSTA                                                         
         BRAS  RE,GETPROF                                                       
         B     RCV10                                                            
*                                                                               
INPRD    EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   QPRD,0(R4)                                                       
         OC    QPRD,SPACES                                                      
         GOTO1 VALIPRD                                                          
         XC    QPRD2,QPRD2                                                      
         MVI   BPRD2,0                                                          
         MVI   BPOLPRD,0           CLEAR FLAG                                   
         XC    SVPNAME2,SVPNAME2                                                
         B     RCV10                                                            
*                                                                               
INPR2    EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   QPRD2,0(R4)                                                      
         OC    QPRD2,SPACES                                                     
         GOTO1 VALIPR2                                                          
         B     RCV10                                                            
*                                                                               
INFPRD   EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   FPRD,0(R4)                                                       
         OC    FPRD,SPACES                                                      
         GOTO1 VALIPRD                                                          
         B     RCV10                                                            
*                                                                               
INFPR2   EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   FPRD2,0(R4)                                                      
         OC    FPRD2,SPACES                                                     
         GOTO1 VALIPR2                                                          
         B     RCV10                                                            
*                                                                               
* *** NOTE *** NOTE *** NOTE ***                                                
*                                                                               
* APRD & APRD2 CONTAIN THE ORIGINAL SPOT PRD ON A MOVE SPOT                     
*                                                                               
INA2PRD  LA    R1,APRD2            PIGGY AFFID PRD                              
         B     *+8                                                              
*                                                                               
INAPRD   LA    R1,APRD                                                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R4)                                                    
         OC    0(L'APRD,R1),SPACES                                              
*                                                                               
         CLI   SVRCVEL+1,X'36'     IS THIS A MOV SPOT REQ?                      
         BE    RCV10                YES - DONT PUT PRD IN BDATA BUFFER          
*                                                                               
         LR    RF,RA                                                            
         AHI   RF,(SVCLTREC-TWAD)+(CLIST-CLTHDRD)  RF=A(CLIST)                  
         LHI   R0,220              MAX PRODUCTS                                 
*                                                                               
         CLC   0(L'APRD,R1),0(RF)  MATCH ON PRODUCT?                            
         BE    INAPRD2              YES                                         
         AHI   RF,4                                                             
         BCT   R0,*-14                                                          
*                                                                               
         MVC   ERROR,=Y(BADPRD)                                                 
         GOTO1 SENDMSG                                                          
*                                                                               
INAPRD2  LA    R4,3(RF)            POINT TO PRDNUM                              
         LA    R5,1                                                             
*                                                                               
         LA    RF,APRD                                                          
         CR    R1,RF               INAPRD OR INA2PRD ?                          
         BNE   INAPRD4              INA2PRD                                     
*                                                                               
         BAS   RE,MVDATA                                                        
         L     RE,DPOINTER         ALWAYS LEAVE A SPACE FOR PRD2                
         AHI   RE,1                                                             
         ST    RE,DPOINTER                                                      
         B     INAPRDX                                                          
*                                                                               
INAPRD4  L     RE,DPOINTER         BACK UP TO 2ND PRD FLD                       
         AHI   RE,-1                                                            
         ST    RE,DPOINTER                                                      
         BAS   RE,MVDATA                                                        
*                                                                               
INAPRDX  B     RCV10                                                            
         SPACE 1                                                                
*==============================================================*                
* THIS ROUTINE ONLY CALLED TO IDENTIFY BUYLINE                                  
*==============================================================*                
         SPACE 1                                                                
INEST    MVC   BUYEST,0(R4)        1 BYTE BINARY EST                            
         B     RCV10                                                            
*                                                                               
* ESTIMATE RANGE                                                                
*                                                                               
INESTRNG CLI   SVRCVEL+1,H3FQ      FIND SPOTS REQUEST?                          
         BE    *+10                 YES                                         
         MVC   QEST,0(R4)          EST INPUT IS XXX(-YYY) OR ALL                
         MVC   FULL(3),0(R4)                                                    
         GOTO1 VALIEST                                                          
*                                                                               
         CLI   SVRCVEL+1,H3FQ      FIND SPOTS REQUEST?                          
         BNE   *+14                 NO                                          
         MVC   FBEST,FULL+3                                                     
         B     RCV10                                                            
*                                                                               
         MVC   BEST,FULL+3                                                      
         MVC   BEST2,BEST          SET EST2=EST1                                
*                                                                               
         XC    QEST2,QEST2                                                      
*                                                                               
         CLI   3(R4),C'-'          TEST RANGE SPECIFIED                         
         BNE   RCV10                                                            
*                                                                               
         MVC   QEST2,4(R4)                                                      
         PACK  DUB,QEST2                                                        
         CVB   R0,DUB                                                           
         STC   R0,BEST2                                                         
         B     RCV10                                                            
*                                                                               
INCBLNET CLC   VERSION,=X'04000034' BEFORE VERSION 4.0.0.52?                    
         BL    *+14                NO - DON'T SUPPORT CROSS NETWORK             
         XC    BYCBLNET,BYCBLNET   IN CASE NTWK IS 2 BYTES & LAST WAS 3         
         B     INCBL10             YES - SUPPORT CROSS NTWK MAKEGOODS           
*                                                                               
INCBL05  OC    BYCBLNET,BYCBLNET   DID WE ALREADY GET ONE?                      
         BZ    INCBL10             NO                                           
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   BYCBLNET(0),0(R4)                                                
         BE    INCBL10                                                          
         MVC   ERROR,=Y(ONENTWK)                                                
         GOTO1 SENDMSG                                                          
*                                                                               
INCBL10  EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   BYCBLNET(0),0(R4)                                                
*                                                                               
* GET THE CABLE NETWORK CODE                                                    
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
         USING STAPACKD,R4                                                      
*                                                                               
         MVI   STAPACT,C'P'                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPAGY,QAGY                                                     
         MVC   STAPMED,QMED                                                     
*                                                                               
         MVI   STAPCTRY,C'U'                                                    
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   *+8                                                              
         MVI   STAPCTRY,C'C'                                                    
         MVC   STAPQMKT,QMKT                                                    
         MVC   STAPQSTA,QSTA                                                    
         MVC   STAPQNET,BYCBLNET                                                
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   BBCBLNET,STAPSTA+2                                               
         CLI   SVAPROF+7,C'C'      CANADA?                                      
         BE    *+8                  YES                                         
         NI    BBCBLNET,X'7F'      TURN OFF HOB                                 
         CLC   VERSION,=X'04000034' ON/AFTER VERSION 4.0.0.52?                  
         BNL   RCV10               YES - SUPPORT CROSS NTWK MKGDS!              
         MVC   IVCBLNET,BYCBLNET   SET FOR SPMAK20                              
         MVC   INVBCNET,BBCBLNET   SET FOR SPMAK20                              
         B     RCV10                                                            
*                                                                               
INCBLNT2 CLC   VERSION,=X'04000034' BEFORE VERSION 4.0.0.52?                    
         BL    INCBL05             NO - DON'T SUPPORT CROSS NTWK MKGDS!         
         XC    IVCBLNET,IVCBLNET   IN CASE NTWK IS 2 BYTES & LAST WAS 3         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   IVCBLNET(0),0(R4)                                                
*                                                                               
* GET THE CABLE NETWORK CODE                                                    
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
         USING STAPACKD,R4                                                      
*                                                                               
         MVI   STAPACT,C'P'                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPAGY,QAGY                                                     
         MVC   STAPMED,QMED                                                     
*                                                                               
         MVI   STAPCTRY,C'U'                                                    
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   *+8                                                              
         MVI   STAPCTRY,C'C'                                                    
         MVC   STAPQMKT,QMKT                                                    
         MVC   STAPQSTA,QSTA                                                    
         MVC   STAPQNET,IVCBLNET   NO - GET THE INVOICE CABLE NETWORK           
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   INVBCNET,STAPSTA+2  BINARY NETWORK FOR INVOICE                   
         CLI   SVAPROF+7,C'C'      CANADA?                                      
         BE    *+8                 YES                                          
         NI    INVBCNET,X'7F'      TURN OFF HOB                                 
         B     RCV10                                                            
*                                                                               
INBUYLN  MVI   WORK,C'B'                                                        
         MVC   WORK+1(1),BUYEST    MOVE CURRENT ESTIMATE NUMBER                 
         MVC   WORK+2(2),0(R4)     MOVE BINARY LINENUM                          
         MVC   WORK+4(3),BYCBLNET  MOVE BUY CABLE NETWORK                       
         MVC   WORK+7(1),BBCBLNET  MOVE BINARY BUY CABLE NETWORK                
         LA    R4,WORK                                                          
         LA    R5,8                                                             
         BAS   RE,MVDATA                                                        
         B     RCV10                                                            
*                                                                               
INSPDAT  MVI   FULL,C'D'           INDICATE SPOT DATE                           
         B     *+8                                                              
INSPADAT MVI   FULL,C'A'           INDICATE AFFID DATE                          
         GOTO1 VDATCON,DMCB,(3,(R4)),(2,FULL+1)                                 
         LA    R4,FULL                                                          
         LA    R5,3                                                             
         BAS   RE,MVDATA                                                        
         B     RCV10                                                            
*                                                                               
ININVNUM MVI   WORK,C'I'           INDICATE INVOICE NUMBER                      
         MVC   WORK+1(10),0(R4)                                                 
         LA    R4,WORK                                                          
         LA    R5,11                                                            
         BAS   RE,MVDATA                                                        
         B     RCV10                                                            
*                                                                               
INSPTIM  XC    WORK,WORK                                                        
         EX    R5,*+8                                                           
         B     *+10                                                             
         OC    0(0,R4),SPACES                                                   
         AHI   R5,1                                                             
         GOTO1 VTIMVAL,DMCB,((R5),(R4)),WORK+1                                  
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   WORK,C'T'           INDICATE AFFID TIME                          
         LA    R4,WORK                                                          
         LA    R5,3                                                             
         BAS   RE,MVDATA                                                        
         B     RCV10                                                            
*                                                                               
INOCOST  AHI   R5,1                R5 BCTR'D                                    
         BAS   RE,MVDATA           ADD AFFID COST AFTER AFFID TIME              
         B     RCV10                                                            
*                                                                               
INNOOVR  CLI   0(R4),C'Y'                                                       
         BNE   RCV10                                                            
         OI    FLAGS,FLNOOVR                                                    
         B     RCV10                                                            
*                                                                               
INAFCOST MVC   AFCOST,0(R4)                                                     
         B     RCV10                                                            
*                                                                               
INOVFLM  MVC   OVFLM,0(R4)                                                      
         B     RCV10                                                            
*                                                                               
INOVCST  MVC   OVCST,0(R4)                                                      
         B     RCV10                                                            
*                                                                               
INOVTIM  MVC   OVTIM,0(R4)                                                      
         B     RCV10                                                            
*                                                                               
INOVSLN  MVC   OVSLN,0(R4)                                                      
         B     RCV10                                                            
*                                                                               
INOVINT  MVC   OVINT,0(R4)                                                      
         B     RCV10                                                            
*                                                                               
INSETST  MVC   STAT1,0(R4)                                                      
         B     RCV10                                                            
*                                                                               
INSPNUM  MVI   FULL,C'N'           SET FOR SPOT NUMBER                          
         MVC   FULL+1(1),0(R4)                                                  
         LA    R4,FULL                                                          
         LA    R5,2                                                             
         BAS   RE,MVDATA                                                        
         B     RCV10                                                            
*                                                                               
INOTO    L     RF,DPOINTER         SET LAST SPOT REC'D WAS OTO                  
         AHI   RF,-2                                                            
         MVI   0(RF),C'O'                                                       
         B     RCV10                                                            
*                                                                               
INSDATE  MVC   BSDATE,0(R4)                                                     
         CLI   SVRCVEL+1,H3FQ      SEARCH SPOT REQUEST?                         
         BNE   RCV10                NO                                          
         GOTO1 VDATCON,DMCB,(3,BSDATE),(2,BPER)    YES - USE AS PERIOD          
         B     RCV10                                                            
*                                                                               
INEDATE  MVC   BEDATE,0(R4)                                                     
         CLI   SVRCVEL+1,H3FQ      SEARCH SPOT REQUEST?                         
         BNE   RCV10                NO                                          
         GOTO1 VDATCON,DMCB,(3,BEDATE),(2,BPER+2)  YES - USE AS PERIOD          
         B     RCV10                                                            
*                                                                               
INNOGOL  MVC   BNOGOL,0(R4)        Y/N                                          
         B     RCV10                                                            
*                                                                               
INNODEM  MVC   BNODEM,0(R4)        Y/N                                          
         B     RCV10                                                            
*                                                                               
INSKED   MVC   BSKED,0(R4)         O=OTO, S=SKED                                
         B     RCV10                                                            
*                                                                               
INXOTO   MVC   XOTOCODE,0(R4)      SPECIAL -OTO CHARACTER                       
         OI    XOTOCODE,C' '       MAKE SURE UPPERCASE!                         
         B     RCV10                                                            
*                                                                               
INBUYER  MVC   BBUYER,SPACES       BUYER NAME                                   
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   BBUYER,0(R4)                                                     
         B     RCV10                                                            
*                                                                               
INCOST   LTR   R5,R5               ANY INPUT?                                   
         BNM   INCOST10             NO                                          
         CLI   SVRCVEL+1,H3FQ      FIND SPOTS REQ?                              
         BNE   RCV10                                                            
         MVC   QCOST(4),=C'ZERO'   FLAG TO SEARCH FOR $0 SPOTS                  
         B     RCV10                                                            
*                                                                               
INCOST10 EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   QCOST(0),0(R4)                                                   
         B     RCV10                                                            
*                                                                               
INCOST2  EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   QCOST2(0),0(R4)                                                  
         B     RCV10                                                            
*                                                                               
INMGCD   EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   QMGCD(0),0(R4)                                                   
         B     RCV10                                                            
*                                                                               
INPERD   DS    0H                  START/END DATES                              
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   RH6DATE,0(R4)                                                    
         CHI   R5,4                TEST MORE THAN 5 CHARS INPUT                 
         BH    RCV10               YES - MUST HAVE TWO DATES                    
         LA    RE,RH6DATE(R5)                                                   
         MVC   1(3,RE),=C'-1W'                                                  
         B     RCV10                                                            
         EJECT                                                                  
*                                                                               
INRSNCD  DS    0H                  GET REASON CODE                              
         LA    R1,RH6RSNCD                                                      
         B     INHMOV                                                           
*                                                                               
INRSNTX  DS    0H                  GET REASON TEXT                              
         LA    R1,RH6RSNTX                                                      
         B     INHMOV                                                           
*                                                                               
INH6MG   DS    0H                  GET MAKEGOOD CODE                            
         LA    R1,RH6MG                                                         
         B     INHMOV                                                           
*                                                                               
INH6DAYS DS    0H                                                               
         LA    R1,RH6DAYS                                                       
         B     INHMOV                                                           
*                                                                               
INTIME   DS    0H                  START/END TIMES                              
         LA    R1,RH6TIME                                                       
         B     INHMOV                                                           
*                                                                               
INH6NPW  DS    0H                                                               
         LA    R1,RH6NPW                                                        
         B     INHMOV                                                           
*                                                                               
INH6SLN  DS    0H                                                               
         LA    R1,RH6SLN                                                        
         B     INHMOV                                                           
*                                                                               
INH6DPT  DS    0H                                                               
         LA    R1,RH6DPT                                                        
         B     INHMOV                                                           
*                                                                               
INH6PROG DS    0H                                                               
         STC   R5,RH6PROGL                                                      
         LA    R1,RH6PROG                                                       
         B     INHMOV                                                           
*                                                                               
INH6REP  DS    0H                                                               
         LA    R1,QREP                                                          
         B     INHMOV                                                           
*                                                                               
INH6ADJ  DS    0H                                                               
         LA    R1,RH6ADJ                                                        
         B     INHMOV                                                           
*                                                                               
INH6DEM  DS    0H                                                               
         LA    R1,RH6DEM                                                        
         B     INHMOV                                                           
*                                                                               
INH6CMT  DS    0H                                                               
         OI    FLAGS,FLCMTRCV      SET COMMENT ELEMENT RECEIVED                 
         XR    R1,R1                                                            
         IC    R1,CMTCOUNT                                                      
         LA    RF,1(R1)                                                         
         STC   RF,CMTCOUNT                                                      
         LTR   R5,R5               ANY INPUT?                                   
         BM    RCV10                NO                                          
         MHI   R1,L'RH6COM1                                                     
         LA    R1,RH6COM1(R1)                                                   
         B     INHMOV                                                           
*                                                                               
INH6PRD  LA    R1,RH6PRD                                                        
         B     INHMOV                                                           
*                                                                               
INH6PR2  LA    R1,RH6PR2                                                        
         B     INHMOV                                                           
*                                                                               
INQSTR   XC    QUESTOR,QUESTOR                                                  
         LA    R1,QUESTOR                                                       
         B     INHMOV                                                           
*                                                                               
INQWHEN  MVC   QWHEN,0(R4)                                                      
         B     RCV10                                                            
*                                                                               
INQDEST  LA    R1,QDEST                                                         
         B     INHMOV                                                           
*                                                                               
INQBOOK  MVC   QHUT,=C'NO'         SET DEFAULT HUT VALUE                        
         XC    WORK,WORK           CREATE DUMMY FLDHDR                          
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK+8(0),0(R4)                                                  
*                                                                               
         MVC   QBOOK,WORK+8                                                     
         CHI   R5,2                3 CHARS INPUT? (R5 BCTR'D)                   
         BNE   *+14                 NO                                          
         CLC   WORK+8(3),=C'ACT'                                                
         BE    IQBX                                                             
*                                                                               
         CHI   R5,3                4 CHARS INPUT? (R5 BCTR'D)                   
         BNE   *+14                 NO                                          
         CLC   WORK+8(3),=C'LATE'                                               
         BE    IQBX                                                             
*                                                                               
IQB10    TM    FLAGS,FLOVNITE      TEST OVERNIGHTS OPTION                       
         BNZ   IQB20                YES                                         
         TM    FLAGS,FLWEEKLY      TEST WEEKLY METERED OPTION                   
         BNZ   IQB20                YES                                         
         AHI   R5,1                                                             
         STC   R5,WORK+5                                                        
         GOTO1 VCALLOV,DMCB,0,X'D9000A00'   GET BOOKVAL ADDRESS                 
         L     RF,0(R1)                                                         
         GOTO1 (RF),(R1),(C'N',WORK),(1,DUB),VSCANNER                           
*                                                                               
         TM    DUB,X'BF'           TEST ANY GARBAGE OPTIONS INPUT               
         BNZ   INBKERR                                                          
         CLI   4(R1),0                                                          
         BE    INBKERR                                                          
         GOTO1 VDATCON,DMCB,(3,DUB+1),WORK+32                                   
         MVC   QBOOK,WORK+32       SAVE EBCDIC YYMM                             
         B     IQBX                                                             
*                                                                               
IQB20    GOTO1 VDATVAL,DMCB,(0,WORK+8),WORK+32                                  
         OC    0(4,R1),0(R1)                                                    
         BZ    INBKERR                                                          
*                                                                               
                                                                                
         MVI   BYTE,0                 SET START DAY TO DEFAULT                  
         TM    FLAGS,FLOVNITE         TEST OVERNIGHTS OPTION                    
         BZ    *+8                                                              
         MVI   BYTE,1                 MONDAY START OF WEEK                      
         GOTO1 =V(NSIWEEK),DMCB,WORK+32,(BYTE,VGETDAY),VADDAY,VDATCON, X        
               RR=BASERELO                                                      
         MVC   QBOOK(1),DMCB+4     YEAR                                         
         MVC   QBOOK+1(1),DMCB     WEEK                                         
*                                                                               
IQBX     B     RCV10                                                            
*                                                                               
INBKERR  MVC   ERROR,=Y(BADBOOK)                                                
         GOTO1 SENDMSG                                                          
*                                                                               
INQHUT   CLC   =C'NO',0(R4)                                                     
         BE    INQHUTX                                                          
*                                                                               
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB(0),0(0,R4)                                                   
         CVD   R0,DUB                                                           
         UNPK  QHUT,DUB            SAVE NUMERIC MONTH                           
INQHUTX  B     RCV10                                                            
*                                                                               
INWKLY   CLI   0(R4),C'O'          SEND OVERNIGHTS PROGRAM NAME                 
         BNE   *+12                                                             
         OI    FLAGS,FLOVNITE                                                   
         B     RCV10                                                            
         CLI   0(R4),C'W'          SEND WEEKLY METERED PROGRAM NAME             
         BNE   RCV10                                                            
         OI    FLAGS,FLWEEKLY                                                   
         B     RCV10                                                            
*                                                                               
INQCOM   DS    0H                  I2 REQUEST COMMENTS                          
         L     R6,AIO2                                                          
         CLI   CMTCOUNT,0                                                       
         BE    INQCOM4                                                          
* FIND END OF BUFFER                                                            
         SR    R0,R0                                                            
INQCOM2  ICM   R0,1,0(R6)                                                       
         BZ    INQCOM4                                                          
         AR    R6,R0                                                            
         B     INQCOM2                                                          
*                                                                               
INQCOM4  IC    R0,CMTCOUNT                                                      
         AHI   R0,1                                                             
         STC   R0,CMTCOUNT                                                      
*                                                                               
         LTR   R5,R5               ANY INPUT?                                   
         BNM   INQCOM6             YES                                          
         MVI   0(R6),2             CREATE COMMENT WITH NO DATA                  
         MVI   1(R6),0                                                          
         MVI   2(R6),0                                                          
         B     INQCOMX                                                          
*                                                                               
INQCOM6  LA    R0,2(R5)            GET L'COMMENT + 1 (FOR LENGTH)               
         STC   R0,0(R6)                                                         
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R6),0(R4)       MOVE COMMENT DATA                            
         LA    RE,2(R5,R6)         POINT PAST COMMENT                           
         MVI   0(RE),0             AND SET EOD FLAG                             
*                                                                               
INQCOMX  B     RCV10                                                            
         EJECT                                                                  
*                                                                               
INHMOV   EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R4)                                                    
         B     RCV10                                                            
*                                                                               
PACK     PACK  DUB,0(0,R4) *EXECUTED*                                           
*                                                                               
MVDATA   L     RF,DPOINTER                                                      
         AR    RF,R5                                                            
         LA    R0,BDATA+(BDATAX-BDATA) TEST PAST END OF BUFFER                  
         CR    RF,R0                                                            
         BL    MV10                                                             
         MVC   ERROR,=Y(MAXDATA)                                                
         GOTO1 SENDMSG                                                          
*                                                                               
MV10     L     RF,DPOINTER                                                      
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R4)                                                    
         LA    RF,1(R5,RF)         ADVANCE POINTER                              
         ST    RF,DPOINTER         AND SAVE                                     
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*================================================================*              
* SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY                 *              
* ON ENTRY, CALLER MUST HAVE RC = A(WORK)                        *              
*================================================================*              
         SPACE 1                                                                
         DS    0D                                                               
VCOMMON  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
*                                                                               
         SRL   RF,24                                                            
         L     RF,VBRANCH(RF)                                                   
         AR    RF,RB                                                            
         BASR  RE,RF               *** NO RETURN EXPECTED HERE ***              
         DC    H'0'                                                             
VCOMMONX XIT1                                                                   
*                                                                               
VCOMERR  DS    0H                                                               
*                                                                               
VMSG     LA    R4,FAMSGBLK                                                      
         USING FAMSGD,R4                                                        
         OC    ERROR,ERROR         FAMSGNO CAN BE SET BY FALINK                 
         BZ    *+10                                                             
         MVC   FAMSGNO,ERROR                                                    
         CLI   *,FF                SET CC LOW                                   
         L     RD,BASERD           MONITOR TO SPMAK00                           
         L     RD,8(RD)            SPMAK00 TO FALINK                            
         L     RD,8(RD)            FALINK TO SPMAK00                            
         XIT1                                                                   
         DROP  R4                                                               
         SPACE 1                                                                
VBRANCH  DS    0A                                                               
         DC    A(DMREAD-VCOMMON)                                                
         DC    A(DMSEQ-VCOMMON)                                                 
         DC    A(DMHIGH-VCOMMON)                                                
         DC    A(DMADD-VCOMMON)                                                 
         DC    A(DMWRITE-VCOMMON)                                               
         DC    A(DMGETREC-VCOMMON)                                              
         DC    A(DMPUTREC-VCOMMON)                                              
         DC    A(DMADDREC-VCOMMON)                                              
         DC    A(DMRDSTA-VCOMMON)                                               
         DC    A(DMHISTA-VCOMMON)                                               
         DC    A(VBYR-VCOMMON)                                                  
         DC    A(VMED-VCOMMON)                                                  
         DC    A(VCLT-VCOMMON)                                                  
         DC    A(VPRD-VCOMMON)                                                  
         DC    A(VEST-VCOMMON)                                                  
         DC    A(VSTA-VCOMMON)                                                  
         DC    A(VPR2-VCOMMON)                                                  
         DC    A(VGETHDR-VCOMMON)                                               
         DC    A(VGETDATA-VCOMMON)                                              
         DC    A(VMSG-VCOMMON)     ERROR MESSAGES USE THIS                      
         DC    17A(0)              SPARE                                        
VCOUNT   EQU   (*-VBRANCH)/4                                                    
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*===================================================================*           
* DIRECTORY AND FILE ROUTINES                                       *           
*===================================================================*           
         SPACE 1                                                                
DMREAD   MVC   COMMAND,=C'DMREAD'                                               
         B     DIRCTRY                                                          
*                                                                               
DMSEQ    MVC   COMMAND,=C'DMRSEQ'                                               
         B     DIRCTRY                                                          
*                                                                               
DMHIGH   MVC   COMMAND,=C'DMRDHI'                                               
         B     DIRCTRY                                                          
*                                                                               
DMADD    MVC   COMMAND,=C'DMADD '                                               
         B     DIRCTRY                                                          
*                                                                               
DMWRITE  MVC   COMMAND,=C'DMWRT '                                               
         B     DIRCTRY                                                          
*                                                                               
DIRCTRY  CLI   RDUPDATE,C'Y'                                                    
         BNE   *+8                                                              
         OI    DMINBTS,X'80'                                                    
         MVC   KEYSAVE,KEY                                                      
         MVC   DIRECTRY,=C'SPTDIR'                                              
         CLI   XSP,C'Y'                                                         
         BNE   *+10                                                             
         MVC   DIRECTRY,=C'XSPDIR'                                              
         CLI   XSP,C'T'                                                         
         BNE   *+10                                                             
         MVC   DIRECTRY,=C'TRFDIR'                                              
         GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),DIRECTRY,KEYSAVE,       X        
               KEY,0                                                            
         B     DMCHECK                                                          
*                                                                               
DMGETREC MVC   COMMAND,=C'GETREC'                                               
         B     DMFILE                                                           
*                                                                               
DMPUTREC MVC   COMMAND,=C'PUTREC'                                               
         B     DMFILE                                                           
*                                                                               
DMADDREC MVC   COMMAND,=C'ADDREC'                                               
         B     DMFILE                                                           
*                                                                               
DMFILE   CLI   RDUPDATE,C'Y'                                                    
         BNE   *+8                                                              
         OI    DMINBTS,X'80'                                                    
         LA    R0,KEY+14                                                        
         MVC   FILE(8),=CL8'SPTFILE'                                            
         CLI   XSP,C'T'                                                         
         BNE   *+10                                                             
         MVC   FILE(8),=CL8'TRFFILE'                                            
         CLI   XSP,C'Y'                                                         
         BNE   *+14                                                             
         LA    R0,KEY+36                                                        
         MVC   FILE(8),=CL8'XSPFILE'                                            
*                                                                               
DMFILEGO GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),FILE,(R0),              X        
               AIO,(0,DMWORK)                                                   
         B     DMCHECK                                                          
*                                                                               
DMRDSTA  MVC   COMMAND,=C'DMREAD'                                               
         B     DMSTA                                                            
*                                                                               
DMHISTA  MVC   COMMAND,=C'DMRDHI'                                               
         B     DMSTA                                                            
*                                                                               
DMSTA    GOTO1 VDATAMGR,DMCB,(DMINBTS,COMMAND),=C'STATION',KEY,AIO              
         SPACE 1                                                                
DMCHECK  DS    0H                                                               
         MVI   DMINBTS,X'00'                                                    
         MVI   RDUPDATE,C'N'                                                    
         MVC   DMBYTE,DMCB+8                                                    
         NC    DMBYTE,DMOUTBTS                                                  
         B     VCOMMONX                                                         
         EJECT                                                                  
*================================================================*              
* RETURN ADDRESS OF MAP HEADER ELEMENT IN R1                     *              
*================================================================*              
         SPACE 1                                                                
VGETHDR  LA    RE,FAMAP                                                         
         USING MHELD,RE                                                         
         SR    RF,RF                                                            
*                                                                               
GHDR2    CLM   R1,3,MHCODE         MATCH EL                                     
         BE    GHDRX                                                            
         ICM   RF,3,MHDISP                                                      
         AR    RE,RF                                                            
         CLI   MHLEN,0                                                          
         BNE   GHDR2                                                            
         DC    H'0'                                                             
*                                                                               
GHDRX    ST    RE,HDRADDR                                                       
         B     VCOMMONX                                                         
         DROP  RE                                                               
         EJECT                                                                  
*================================================================*              
* RETURN DATA ITEM ADDRESS FOR HEADER  IN HDRADDR                *              
* R1 CONTAINS DATA ITEM NUMBER                                   *              
*================================================================*              
         SPACE 1                                                                
VGETDATA ICM   RE,15,HDRADDR                                                    
         BNZ   *+6                                                              
         DC    H'0'                TAKING NO PRISONERS                          
         USING MDELD,RE                                                         
         SR    RF,RF                                                            
         IC    RF,MHLEN-MHELD(RE)  DSPL TO FIRST DATA ITEM                      
         AR    RE,RF                                                            
*                                                                               
GDAT2    CLM   R1,3,MDCODE         MATCH EL                                     
         BE    GDATX                                                            
         ICM   RF,1,MDLEN                                                       
         AR    RE,RF                                                            
         CLI   MDLEN,0                                                          
         BNE   GDAT2                                                            
         DC    H'0'                                                             
*                                                                               
GDATX    ST    RE,DATADDR                                                       
         B     VCOMMONX                                                         
         DROP  RE                                                               
         EJECT                                                                  
*================================================================*              
* VALIDATE MEDIA CODE                                            *              
*================================================================*              
         SPACE 1                                                                
VMED     XC    KEY,KEY             GET AGENCY RECORD                            
         LA    R4,KEY                                                           
         USING AGYKEY,R4                                                        
         MVI   AGYKTYPE,X'06'                                                   
         MVC   AGYKAGY,QAGY                                                     
         DROP  R4                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         USING AGYKEY,R6                                                        
         MVC   SVAGYNAM,AGYNAME                                                 
         MVC   SVAGYADR,AGYADDR                                                 
         MVC   SVAPROF,AGYPROF     SAVE AGENCY PROFILE                          
*                                                                               
         CLI   QMED,C'N'           NETWORK TV?                                  
         BNE   VMED1                NO                                          
         CLI   SVAPROF+7,C'C'      BETTER BE CANADA...                          
         BE    VMED1                                                            
         MVC   ERROR,=Y(BADMED)                                                 
         BRAS  RE,VCOMERR                                                       
*                                                                               
VMED1    MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         B     VMED4                                                            
         SPACE 1                                                                
VMED2    BAS   RE,NEXTEL                                                        
         BE    *+6                                                              
         DCHO                                                                   
VMED4    CLC   2(1,R6),QMED        MATCH MEDIA CODE                             
         BNE   VMED2                                                            
         MVC   BAGYMD,3(R6)        DIG OUT AGENCY/MEDIA                         
         MVC   SVMEDNM,4(R6)       MEDIA NAME                                   
         B     VCOMMONX                                                         
         DROP  R6                                                               
         EJECT                                                                  
*================================================================*              
* VALIDATE BUYER                                                 *              
*================================================================*              
         SPACE 1                                                                
VBYR     B     VCOMMONX                                                         
         EJECT                                                                  
*================================================================*              
* VALIDATE CLIENT                                                *              
*================================================================*              
         SPACE 1                                                                
VCLT     MVC   ERROR,=Y(BADCLT)                                                 
         GOTO1 VCLPACK,DMCB,QCLT,BCLT                                           
         CLI   0(R1),0                                                          
         BE    *+8                                                              
         BRAS  RE,VCOMERR                                                       
         SPACE 1                                                                
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+8                                                              
         BRAS  RE,VCOMERR                                                       
                                                                                
         LR    R6,RA                                                            
         AHI   R6,(SVCLTREC-TWAD)                                               
         ST    R6,AIO                                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         USING CLTHDRD,R6                                                       
*                                                                               
         MVC   SVCPROF,CPROF       SAVE CLIENT PROFILES                         
         MVC   SVCXTRA,CEXTRA                                                   
         MVC   SVCNAME,CNAME       AND CLIENT NAME                              
         MVC   SVOFFC,COFFICE      AND OFFICE! (SINCE IT'S USED)                
         MVI   SVCOST2,C'N'                                                     
*                                                                               
         TM    COPT3,COP3COSQ      COS2 OPTIONAL                                
         BNO   *+12                 NO                                          
         MVI   SVCOST2,C'O'                                                     
         B     VCLTX                                                            
*                                                                               
         TM    COPT4,COP4TRD       TRADE?                                       
         BNO   *+12                 NO                                          
         MVI   SVCOST2,C'T'                                                     
         B     VCLTX                                                            
*                                                                               
         TM    COPT1,COP1COSQ                                                   
         BNO   *+8                                                              
         MVI   SVCOST2,C'Y'                                                     
*                                                                               
VCLTX    B     VCOMMONX                                                         
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*===============================================================*               
* VALIDATE PRODUCT CODE                                         *               
*===============================================================*               
*                                                                               
VPR2     MVI   MYFLAG,2            INDICATE PRD2                                
         B     VPRD2                                                            
*                                                                               
VPRD     MVI   MYFLAG,1            INDICATE PRD1                                
*                                                                               
VPRD2    XC    KEY,KEY                                                          
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),BCLT                                                    
         CLI   MYFLAG,1                                                         
         BNE   VPRD4                                                            
         MVC   KEY+4(3),QPRD                                                    
         CLI   SVRCVEL+1,H3FQ      FIND SPOTS REQ?                              
         BNE   VPRD6                                                            
         MVC   KEY+4(3),FPRD                                                    
         B     VPRD6                                                            
*                                                                               
VPRD4    MVC   KEY+4(3),QPRD2                                                   
         CLI   SVRCVEL+1,H3FQ      FIND SPOTS REQ?                              
         BNE   *+10                                                             
         MVC   KEY+4(3),FPRD2                                                   
*                                                                               
VPRD6    MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         USING PRDHDRD,R6                                                       
*                                                                               
         CLI   MYFLAG,2                                                         
         BE    VPRD10                                                           
         MVC   FBPRD,PCODE+1                                                    
         CLI   SVRCVEL+1,H3FQ      FIND SPOTS REQ?                              
         BE    VCOMMONX             YES                                         
         MVC   BPRD,PCODE+1                                                     
         MVC   SVPNAME,PNAME                                                    
         B     VCOMMONX                                                         
*                                                                               
VPRD10   MVC   FBPRD2,PCODE+1                                                   
         CLI   SVRCVEL+1,H3FQ      FIND SPOTS REQ?                              
         BE    VCOMMONX             YES                                         
         MVC   BPRD2,PCODE+1                                                    
         MVC   SVPNAME2,PNAME                                                   
         B     VCOMMONX                                                         
         DROP  R6                                                               
         EJECT                                                                  
*===============================================================*               
* VALIDATE EBCDIC ESTIMATE NUMBER IN FULL(3)                    *               
* RETURN BINARY VALUE IN FULL+3(1)                              *               
*===============================================================*               
         SPACE 1                                                                
VEST     CLC   =C'ALL',QEST                                                     
         BE    VCOMMONX                                                         
*                                                                               
         MVC   ERROR,=Y(BADEST)                                                 
         PACK  DUB,FULL(3)                                                      
         CVB   R0,DUB                                                           
         STC   R0,FULL+3           SET BINARY ESTIMATE                          
         CHI   R0,255                                                           
         BNH   *+8                                                              
         BRAS  RE,VCOMERR                                                       
         B     VCOMMONX                                                         
         EJECT                                                                  
*==================================================================*            
* LOOK UP MARKET NUMBER AND GET 3 BYTE PACKED STA                  *            
*==================================================================*            
         SPACE 1                                                                
VSTA     DS    0H                                                               
         CLC   QMKT,=C'0000'       IF THERE IS NO MKT (SEARCH FEATURE),         
         BNE   VSTA10               GET IT FROM THE STATION REC                 
         CLI   SVAPROF+7,C'C'      IF CANADIAN NETWORK, MKT 0 LEGIT             
         BNE   *+12                                                             
         CLI   QMED,C'N'                                                        
         BE    VSTA10                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING STARECD,R6                                                       
         MVI   STAKTYPE,STAKTYPQ                                                
         MVC   STAKMED,QMED                                                     
         MVC   STAKCALL,QSTA                                                    
         CLI   STAKCALL+4,C' '                                                  
         BH    *+10                                                             
         MVC   STAKCALL+4(1),QMED                                               
         MVC   STAKAGY,QAGY                                                     
         MVC   STAKCLT,QCLT                                                     
         MVC   STAKFILL,=C'000'                                                 
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         GOTO1 READSTA                                                          
         MVC   QMKT,SMKT                                                        
*                                                                               
VSTA10   MVC   ERROR,=Y(BADSTA)                                                 
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
         USING STAPACKD,R4                                                      
*                                                                               
         MVI   STAPACT,C'P'                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPAGY,QAGY                                                     
         MVC   STAPMED,QMED                                                     
*                                                                               
         MVI   STAPCTRY,C'U'                                                    
         CLI   SVAPROF+7,C'C'      TEST CANADA                                  
         BNE   *+8                                                              
         MVI   STAPCTRY,C'C'                                                    
         MVC   STAPQMKT,QMKT                                                    
         MVC   STAPQSTA,QSTA                                                    
         MVC   STAPQNET,QSTA+5                                                  
         GOTO1 VSTAPACK,(R4)                                                    
         CLI   STAPERR,0                                                        
         BE    *+8                                                              
         BRAS  RE,VCOMERR                                                       
         MVC   BSTA,STAPSTA                                                     
         MVC   BMKT,STAPMKT                                                     
         EJECT                                                                  
* READ MARKET RECORD TO IO2                                                     
         SPACE 1                                                                
         L     R6,AIO2                                                          
         ST    R6,AIO                                                           
         USING MKTRECD,R6                                                       
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),QMKT                                                    
         MVC   KEY+6(2),QAGY                                                    
*                                                                               
         MVC   ERROR,=Y(BADMKT)                                                 
         GOTO1 HIGHSTA                                                          
*                                                                               
         CLC   KEY(8),0(R6)        TEST MARKET FOUND                            
         BE    VSTA40                                                           
         CLI   QMED,C'C'           ERROR EXCEPT FOR MEDIA=C, MKT=0000           
         BE    *+8                                                              
         BRAS  RE,VCOMERR                                                       
         CLC   QMKT,=C'0000'                                                    
         BE    *+8                                                              
         BRAS  RE,VCOMERR                                                       
         XC    SVMKTNM,SVMKTNM                                                  
         MVC   SVMKTNM(7),=C'NETWORK'                                           
         B     VCOMMONX                                                         
*                                                                               
VSTA40   MVC   SVMKTNM,MKTNAME     RETURN MARKET NAME TO USER                   
         B     VCOMMONX                                                         
         EJECT                                                                  
         GETEL R6,24,ELCODE                                                     
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* CKSTA - CHECK THAT STATIONS MARKET AGREES WITH QMKT                 *         
*=====================================================================*         
         SPACE 1                                                                
CKSTA    NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY             GET STATION RECORD                           
         LA    R6,KEY                                                           
         USING STARECD,R6                                                       
         MVI   STAKTYPE,STAKTYPQ                                                
         MVC   STAKMED,QMED                                                     
         MVC   STAKCALL,QSTA                                                    
         CLI   STAKCALL+4,C' '                                                  
         BH    *+10                                                             
         MVC   STAKCALL+4(1),QMED                                               
         MVC   STAKAGY,QAGY                                                     
         MVC   STAKCLT,QCLT                                                     
         MVC   STAKFILL,=C'000'                                                 
*                                                                               
         MVC   ERROR,=Y(BADMKT)                                                 
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 READSTA                                                          
*                                                                               
         CLC   SMKT,QMKT                                                        
         BE    CKSTAX                                                           
         BRAS  RE,VCOMERR                                                       
         DC    H'0'                NO RETURN EXPECTED HERE                      
*                                                                               
CKSTAX   BRAS  RE,EXIT                                                          
         DC    H'0'                NO RETURN EXPECTED HERE                      
         DROP  R6                                                               
*                                                                               
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* GETPROF - READ REQUIRED PROFILES *                                            
*=====================================================================*         
         SPACE 1                                                                
GETPROF  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R4,=C'S0A0'         READ A0 PROFILE                              
         LA    R5,PROFA0                                                        
         BAS   RE,GETIT                                                         
         SPACE 1                                                                
         LA    R4,=C'S0A0'         READ AGY LEVEL A0 PROFILE                    
         O     R4,=X'80000000'     SET FLAG FOR GETIT                           
         LA    R5,WORK2                                                         
         BAS   RE,GETIT                                                         
         MVC   PROFA0(1),WORK2     SET GROSS/NET FIELD ONLY                     
*                                                                               
         LA    R4,=C'S0MK'                                                      
         LA    R5,PROFMK                                                        
         BAS   RE,GETIT                                                         
         CLI   PROFMK+1,C'A'       TEST PROFILE VALUE SET                       
         BNL   *+8                                                              
         MVI   PROFMK+1,C'Y'       SET DEFAULT VALUE                            
         SPACE 1                                                                
         LA    R4,=C'S0MK'         READ AGY LEVEL MK PROFILE                    
         O     R4,=X'80000000'     SET FLAG FOR GETIT                           
         LA    R5,WORK2                                                         
         BAS   RE,GETIT                                                         
         MVC   PROFMK+4(1),WORK2+4   ONLY LOOK AT AGY LEV FOR BUYER             
         MVC   PROFMK+8(1),WORK2+8   ONLY LOOK AT AGY LEV FOR APPROVED          
*                                                                               
         LA    R4,=C'S000'                                                      
         LA    R5,PROF00                                                        
         BAS   RE,GETIT                                                         
         CLI   SVAPROF+7,C'C'      CANADA AGENCY?                               
         BNE   GETPRF1             NO                                           
         CLC   VERSION,=X'04010029' BEFORE VERSION 4.1.0.41?                    
         BL    GETPRF1             YES - NO CANADIAN 2-DEC RADIO RTGS           
         CLI   QMED,C'R'           MEDIA R?                                     
         BE    *+8                 YES - HONOR 2 DECIMAL RATINGS                
GETPRF1  CLI   QMED,C'T'           ONLY MEDIA T GETS 2 DECIMAL                  
         BE    *+12                                                             
         MVI   PROF00+9,C'N'                                                    
         B     GETPRF6                                                          
         LA    R4,=C'S000'         READ AGY LEVEL 00 PROFILE                    
         O     R4,=X'80000000'     SET FLAG FOR GETIT                           
         LA    R5,WORK2                                                         
         BAS   RE,GETIT                                                         
         MVC   PROF00+9(1),WORK2+9   ONLY LOOK AT AGY LEVEL FOR 2 DEC           
*                                                                               
GETPRF6  MVC   FULL,=C'S00A'       READ AGY LEVEL 00A PROFILE                   
         NI    FULL,X'BF'          CHANGE 'S' TO LOWER CASE                     
         LA    R4,FULL             READ AGY LEVEL 00A PROFILE                   
         O     R4,=X'80000000'     SET AGENCY LEVEL FLAG FOR GETIT              
         LA    R5,WORK2            SET PROFILE HERE                             
         BAS   RE,GETIT            READ PROFILE                                 
         MVC   DEC2IMPS,WORK2+6    AGY LEVEL 2 DECIMAL IMPRESSIONS FLD          
         CLI   QMED,C'T'           MEDIA T?                                     
         BNE   *+12                NO - SET TO AN N                             
         CLI   DEC2IMPS,C'Y'       2 DEC IMPS SET TO A "Y"?                     
         BE    *+8                 YES                                          
         MVI   DEC2IMPS,C'N'       SET TO AN "N"                                
*                                                                               
         LA    R4,=C'S0B0'                                                      
         LA    R5,PROFB0                                                        
         BAS   RE,GETIT                                                         
*                                                                               
         LA    R4,=C'S0TI'                                                      
         LA    R5,PROFTI                                                        
         BAS   RE,GETIT                                                         
*                                                                               
         LA    R4,=C'S0I2'                                                      
         LA    R5,PROFI2                                                        
         BAS   RE,GETIT                                                         
*                                                                               
         MVC   FULL,=C'SI2X'                                                    
         NI    FULL,X'BF'          CHANGE 'S' TO LOWER CASE                     
         LA    R4,FULL                                                          
         LA    R5,PROFI2X                                                       
         BAS   RE,GETIT                                                         
*                                                                               
         MVC   FULL+1(3),=C'I2Y'                                                
         LA    R5,PROFI2Y                                                       
         BAS   RE,GETIT                                                         
*                                                                               
         MVC   FULL+1(3),=C'I2S'                                                
         LA    R5,PROFI2S                                                       
         BAS   RE,GETIT                                                         
*                                                                               
         MVC   FULL+1(3),=C'I2Z'                                                
         LA    R5,PROFI2Z                                                       
         BAS   RE,GETIT                                                         
*                                                                               
         MVC   FULL+1(3),=C'I2A'                                                
         LA    R5,PROFI2A                                                       
         BAS   RE,GETIT                                                         
*                                                                               
         MVC   FULL+1(3),=C'I2B'                                                
         LA    R5,PROFI2B                                                       
         BAS   RE,GETIT                                                         
*                                                                               
         MVC   FULL+1(3),=C'I2C'                                                
         LA    R5,PROFI2C                                                       
         BAS   RE,GETIT                                                         
*                                                                               
         MVC   FULL+1(3),=C'I2N'                                                
         LA    R5,PROFI2N                                                       
         BAS   RE,GETIT                                                         
         O     R4,=X'80000000'     SET FLAG FOR GETIT                           
         LA    R5,WORK2                                                         
         BAS   RE,GETIT                                                         
         N     R4,=X'7FFFFFFF'     RESET AGY LEVEL FLAG                         
         MVC   PROFI2N+3(1),WORK2+3  FIELDS ONLY VALID AT AGENCY LEVEL          
         MVC   PROFI2N+5(1),WORK2+5                                             
*                                                                               
         MVC   FULL+1(3),=C'DAR'                                                
         LA    R5,PROFDAR                                                       
         BAS   RE,GETIT                                                         
*                                                                               
* CONVERT PROFILE DATES TO Y2K FORMAT                                           
         LA    R5,PROFA0+5         SPECIAL OFFICE START                         
         BRAS  RE,Y2KPROF                                                       
*                                                                               
         LA    R5,PROFA0+7         MAKEGOOD AS MISSED EFFECTIVE Y/M             
         BRAS  RE,Y2KPROF                                                       
*                                                                               
         LA    R5,PROFI2Z+8        TIME LEEWAY EFFECTIVE MON                    
         BRAS  RE,Y2KPROF                                                       
*                                                                               
         LA    R5,PROFI2A+6        TIME LEEWAY EFFECTIVE MON                    
         BRAS  RE,Y2KPROF                                                       
*                                                                               
         LA    R5,PROFI2B+1        CHK CMML PRD EFFECTIVE MON                   
         BRAS  RE,Y2KPROF                                                       
*                                                                               
         LA    R5,PROFI2B+4        CHK CMML FLTS EFFECTIVE MON                  
         BRAS  RE,Y2KPROF                                                       
*                                                                               
         LA    R5,PROFI2B+7        CHK CMML TIMES EFFECTIVE MON                 
         BRAS  RE,Y2KPROF                                                       
*                                                                               
         LA    R5,PROFI2B+11       SPT LEN LEEWAY EFFECTIVE MON                 
         BRAS  RE,Y2KPROF                                                       
*                                                                               
         LA    R5,PROFTI+11        TI PROF #9 & #11 EFFECTIVE MON               
         BRAS  RE,Y2KPROF                                                       
*                                                                               
GETPRFX  J     EXIT                                                             
         DC    H'0'                NO RETURN EXPECTED HERE                      
*                                                                               
GETIT    NTR1                                                                   
         XC    WORK,WORK                                                        
         MVC   WORK(4),0(R4)                                                    
         MVC   WORK+4(2),QAGY                                                   
         LA    R1,WORK             SET UP P1                                    
         ST    R1,DMCB                                                          
         MVI   DMCB,X'C0'          RETURN DEFALT PROFILE IF NONE                
         LTR   R4,R4               IF NEGATIVE, STOP AT AGY                     
         BNM   *+12                                                             
         OI    DMCB,X'10'          SET AGY LEVEL PROFILE ONLY                   
         B     GETIT2                                                           
*                                                                               
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVOFFC                                                
*                                                                               
GETIT2   XC    0(16,R5),0(R5)                                                   
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,,(R5),VDATAMGR                                         
         J     EXIT                                                             
         EJECT                                                                  
*                                                                               
Y2KPROF  LR    R0,RE               CONVERT BIN YY/MM TO DDS MM/YY               
         OC    0(2,R5),0(R5)                                                    
         BZR   RE                                                               
         MVC   DUB(2),0(R5)                                                     
         MVI   DUB+2,1                                                          
         GOTO1 VDATCON,DMCB,(3,DUB),WORK    MAKE EBCDIC                         
         GOTO1 (RF),(R1),WORK,(3,DUB)       MAKE BINARY                         
         MVC   0(2,R5),DUB                                                      
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
PHASES   DS    0X                  ** LOADED PHASE LIST **                      
         DC    AL1(QFALINK)                                                     
         DC    AL1(QOFFICER)                                                    
         DC    AL1(QSTAPACK)                                                    
         DC    AL1(QDEMOCON)                                                    
         DC    AL1(QDAYUNPK)                                                    
         DC    AL1(QUNTIME)                                                     
         DC    AL1(QGETBROD)                                                    
         DC    AL1(QCLPACK)                                                     
         DC    AL1(QSTAVAL)                                                     
         DC    AL1(QTSAR)                                                       
         DC    AL1(QSPOTIO)                                                     
         DC    AL1(QTIMVAL)                                                     
         DC    AL1(QGETRATE)                                                    
         DC    AL1(QCLUNPK)                                                     
         DC    AL1(QSPGETBU)                                                    
         DC    AL1(QGETBUY)                                                     
         DC    AL1(QBLDMGN)                                                     
PHASESN  EQU   *-PHASES                                                         
         EJECT                                                                  
*====================================================================*          
* FALINK MAP TABLE                                                              
*====================================================================*          
         SPACE 1                                                                
         DS    0D                                                               
         DC    C'**FAMAP*'         EYE CATCHER                                  
FAMAP    DS    0D                                                               
         SPACE 1                                                                
*====================================================================*          
* RECEIVE HEADERS (FROM PC)                                          *          
*====================================================================*          
         SPACE 1                                                                
*====================================================================*          
* 30 - SEARCH FOR HEADERS REQUEST                                    *          
*====================================================================*          
         SPACE 1                                                                
H30      DC    AL1(H30X-H30)       HEADER LENGTH                                
         DC    AL2(H30Q)           HEADER CODE                                  
         DC    AL2(H30XX-H30)      DISP TO NEXT HEADER                          
H30X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(01),CL5'MEDIA',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INMED)                                                       
         DC    AL1(14),AL2(02),CL5'CLT  ',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INCLT)                                                       
         DC    AL1(14),AL2(03),CL5'MKT  ',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INMKT)                                                       
         DC    AL1(14),AL2(04),CL5'STA  ',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INSTA)                                                       
         DC    AL1(14),AL2(05),CL5'STMOS',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INSTMOS)                           MMM/YY                    
         DC    AL1(14),AL2(06),CL5'EDMOS',AL1(MDTCHQ),AL1(6)                    
         DC    AL4(INEDMOS)                           MMM/YY                    
         DC    X'00'                                                            
H30XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 31 - DOWNLOAD HEADERS REQUEST                                      *          
*====================================================================*          
         SPACE 1                                                                
H31      DC    AL1(H31X-H31)       HEADER LENGTH                                
         DC    AL2(H31Q)           HEADER CODE                                  
         DC    AL2(H31XX-H31)      DISP TO NEXT HEADER                          
H31X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(01),CL5'GROUP',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INGRP)                                                       
         DC    AL1(14),AL2(02),CL5'BUYER',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INBYR)                                                       
         DC    AL1(14),AL2(03),CL5'MEDIA',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INMED)                                                       
         DC    AL1(14),AL2(04),CL5'MATCH',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INMATCH)                                                     
         DC    AL1(14),AL2(05),CL5'STMOS',AL1(MDTCHQ),AL1(6)                    
         DC    AL4(INSTMOS)                           MMM/YY                    
         DC    AL1(14),AL2(06),CL5'EDMOS',AL1(MDTCHQ),AL1(6)                    
         DC    AL4(INEDMOS)                           MMM/YY                    
         DC    X'00'                                                            
H31XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 32 - MATCH INFORMATION REQUEST (BUYS/AFFIDS)                                  
*====================================================================*          
         SPACE 1                                                                
H32      DC    AL1(H32X-H32)       HEADER LENGTH                                
         DC    AL2(H32Q)           HEADER CODE                                  
         DC    AL2(H32XX-H32)      DISP TO NEXT HEADER                          
H32X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(01),CL5'CMNTS',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INCMNT)                                                      
         DC    X'00'                                                            
H32XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 33 - CHANGE BUY                                                               
*====================================================================*          
         SPACE 1                                                                
H33      DC    AL1(H33X-H33)       HEADER LENGTH                                
         DC    AL2(H33Q)           HEADER CODE                                  
         DC    AL2(H33XX-H33)      DISP TO NEXT HEADER                          
H33X     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(01),CL5'TIME ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INTIME)                                                      
         DC    AL1(14),AL2(02),CL5'COST ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INCOST)                                                      
         DC    AL1(14),AL2(03),CL5'NOMAT',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INNOMAT)                                                     
         DC    X'00'                                                            
H33XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 34 - BUYLINE COMMENTS REQUEST                                                 
*====================================================================*          
         SPACE 1                                                                
H34      DC    AL1(H34X-H34)       HEADER LENGTH                                
         DC    AL2(H34Q)           HEADER CODE                                  
         DC    AL2(H34XX-H34)      DISP TO NEXT HEADER                          
H34X     EQU   *                                                                
*                                                                               
         DC    X'00'                                                            
H34XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 35 - MAKEGOOD REQUEST                                                         
*====================================================================*          
         SPACE 1                                                                
H35      DC    AL1(H35X-H35)       HEADER LENGTH                                
         DC    AL2(H35Q)           HEADER CODE                                  
         DC    AL2(H35XX-H35)      DISP TO NEXT HEADER                          
H35X     EQU   *                                                                
*                                                                               
         DC    AL1(14),AL2(01),CL5'BUYER',AL1(MDTCHQ),AL1(12)                   
         DC    AL4(INBUYER)                                                     
         DC    AL1(14),AL2(02),CL5'PERD ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INPERD)                                                      
         DC    AL1(14),AL2(03),CL5'COST ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INCOST)                                                      
         DC    AL1(14),AL2(04),CL5'MGCOD',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INMGCD)                                                      
         DC    AL1(14),AL2(05),CL5'NOMAT',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INNOMAT)                                                     
         DC    AL1(14),AL2(06),CL5'COST2',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INCOST2)                                                     
         DC    X'00'                                                            
H35XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 36 - MOVE SPOT                                                                
*====================================================================*          
         SPACE 1                                                                
H36      DC    AL1(H36X-H36)       HEADER LENGTH                                
         DC    AL2(H36Q)           HEADER CODE                                  
         DC    AL2(H36XX-H36)      DISP TO NEXT HEADER                          
H36X     EQU   *                                                                
*                                                                               
         DC    AL1(14),AL2(01),CL5'SDATE',AL1(MDTBDQ),AL1(6)                    
         DC    AL4(INSDATE)                           YYMMDD                    
         DC    AL1(14),AL2(02),CL5'EDATE',AL1(MDTBDQ),AL1(6)                    
         DC    AL4(INEDATE)                           YYMMDD                    
         DC    AL1(14),AL2(03),CL5'NOGOL',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INNOGOL)                                                     
         DC    AL1(14),AL2(04),CL5'SKED ',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INSKED)                                                      
         DC    X'00'                                                            
H36XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 37 - NEW BUY REQUEST                                                          
*====================================================================*          
         SPACE 1                                                                
H37      DC    AL1(H37X-H37)       HEADER LENGTH                                
         DC    AL2(H37Q)           HEADER CODE                                  
         DC    AL2(H37XX-H37)      DISP TO NEXT HEADER                          
H37X     EQU   *                                                                
*                                                                               
         DC    AL1(14),AL2(01),CL5'BUYER',AL1(MDTCHQ),AL1(12)                   
         DC    AL4(INBUYER)                                                     
         DC    AL1(14),AL2(02),CL5'PERD ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INPERD)                            MMDDYYMMDDYY              
         DC    AL1(14),AL2(03),CL5'COST ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INCOST)                                                      
         DC    AL1(14),AL2(04),CL5'NOMAT',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INNOMAT)                                                     
         DC    AL1(14),AL2(06),CL5'COST2',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INCOST2)                                                     
         DC    X'00'                                                            
H37XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 38 - PLUS OTO                                                                 
*====================================================================*          
         SPACE 1                                                                
*                                                                               
H38      DC    AL1(H38X-H38)       HEADER LENGTH                                
         DC    AL2(H38Q)           HEADER CODE                                  
         DC    AL2(H38XX-H38)      DISP TO NEXT HEADER                          
H38X     EQU   *                                                                
*                                                                               
         DC    AL1(14),AL2(01),CL5'SKED ',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INSKED)                                                      
         DC    AL1(14),AL2(02),CL5'COST ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INCOST)                                                      
         DC    X'00'                                                            
H38XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 39 - MINUS OTO                                                                
*====================================================================*          
         SPACE 1                                                                
H39      DC    AL1(H39X-H39)       HEADER LENGTH                                
         DC    AL2(H39Q)           HEADER CODE                                  
         DC    AL2(H39XX-H39)      DISP TO NEXT HEADER                          
H39X     EQU   *                                                                
*                                                                               
         DC    AL1(14),AL2(01),CL5'SKED ',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INSKED)                                                      
         DC    AL1(14),AL2(02),CL5'XOTO ',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INXOTO)                                                      
         DC    X'00'                                                            
H39XX EQU      *                                                                
         SPACE 1                                                                
*====================================================================*          
* 3A - REQUEST OLREPORT                                                         
*====================================================================*          
         SPACE 1                                                                
H3A      DC    AL1(H3AX-H3A)       HEADER LENGTH                                
         DC    AL2(H3AQ)           HEADER CODE                                  
         DC    AL2(H3AXX-H3A)      DISP TO NEXT HEADER                          
H3AX     EQU   *                                                                
*                                                                               
         DC    AL1(14),AL2(01),CL5'QSTR ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INQSTR)                                                      
         DC    AL1(14),AL2(02),CL5'ZZCOM',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INQCOM)                                                      
         DC    AL1(14),AL2(03),CL5'QWHEN',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INQWHEN)                                                     
         DC    AL1(10),AL2(04),CL5'RPTID',AL1(MDTCHQ),AL1(9)                    
         DC    AL1(14),AL2(05),CL5'QDEST',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INQDEST)                                                     
         DC    AL1(14),AL2(06),CL5'QBOOK',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INQBOOK)                                                     
         DC    AL1(14),AL2(07),CL5'QHUT ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INQHUT)                                                      
         DC    X'00'                                                            
H3AXX EQU      *                                                                
         SPACE 1                                                                
*====================================================================*          
* 3B - MATCHED                                                                  
*====================================================================*          
         SPACE 1                                                                
H3B      DC    AL1(H3BX-H3B)       HEADER LENGTH                                
         DC    AL2(H3BQ)           HEADER CODE                                  
         DC    AL2(H3BXX-H3B)      DISP TO NEXT HEADER                          
H3BX     EQU   *                                                                
*                                                                               
         DC    AL1(14),AL2(01),CL5'MATCH',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(DUMMY)                                                       
         DC    X'00'                                                            
H3BXX EQU      *                                                                
         SPACE 1                                                                
*====================================================================*          
* 3C - UNMATCH                                                                  
*====================================================================*          
         SPACE 1                                                                
H3C      DC    AL1(H3CX-H3C)       HEADER LENGTH                                
         DC    AL2(H3CQ)           HEADER CODE                                  
         DC    AL2(H3CXX-H3C)      DISP TO NEXT HEADER                          
H3CX     EQU   *                                                                
*                                                                               
         DC    X'00'                                                            
H3CXX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 3D - EMAIL RNO SPOTS                                                          
*====================================================================*          
         SPACE 1                                                                
H3D      DC    AL1(H3DX-H3D)       HEADER LENGTH                                
         DC    AL2(H3DQ)           HEADER CODE                                  
         DC    AL2(H3DXX-H3D)      DISP TO NEXT HEADER                          
H3DX     EQU   *                                                                
*                                                                               
         DC    X'00'                                                            
H3DXX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 3E - CHANGE SPOT COST                                                         
*====================================================================*          
         SPACE 1                                                                
H3E      DC    AL1(H3EX-H3E)       HEADER LENGTH                                
         DC    AL2(H3EQ)           HEADER CODE                                  
         DC    AL2(H3EXX-H3E)      DISP TO NEXT HEADER                          
H3EX     EQU   *                                                                
*                                                                               
         DC    AL1(14),AL2(02),CL5'COST ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INCOST)                                                      
         DC    AL1(14),AL2(03),CL5'NOMAT',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INNOMAT)                                                     
         DC    AL1(14),AL2(04),CL5'COST2',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INCOST2)                                                     
         DC    X'00'                                                            
H3EXX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 3F - SEARCH FOR SPOTS REQUEST                                                 
*====================================================================*          
         SPACE 1                                                                
H3F      DC    AL1(H3FX-H3F)       HEADER LENGTH                                
         DC    AL2(H3FQ)           HEADER CODE                                  
         DC    AL2(H3FXX-H3F)      DISP TO NEXT HEADER                          
H3FX     EQU  *                                                                 
*                                                                               
         DC    AL1(14),AL2(01),CL5'PRD  ',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INFPRD)                                                      
         DC    AL1(14),AL2(10),CL5'PR2  ',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INFPR2)                                                      
         DC    AL1(14),AL2(02),CL5'ESRNG',AL1(MDTCHQ),AL1(7)                    
         DC    AL4(INESTRNG)                                                    
         DC    AL1(14),AL2(03),CL5'SDATE',AL1(MDTBDQ),AL1(6)                    
         DC    AL4(INSDATE)                           YYMMDD                    
         DC    AL1(14),AL2(04),CL5'EDATE',AL1(MDTBDQ),AL1(6)                    
         DC    AL4(INEDATE)                           YYMMDD                    
         DC    AL1(14),AL2(05),CL5'DAYS ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INH6DAYS)                                                    
         DC    AL1(14),AL2(06),CL5'TIME ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INTIME)                                                      
         DC    AL1(14),AL2(07),CL5'DPT  ',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INH6DPT)                                                     
         DC    AL1(14),AL2(08),CL5'ZZCOM',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INCOST)                                                      
         DC    AL1(14),AL2(09),CL5'PROG ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INH6PROG)                                                    
         DC    X'00'                                                            
H3FXX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 40 - BUY HISTORY REQUEST                                                      
*====================================================================*          
         SPACE 1                                                                
H40      DC    AL1(H40X-H40)       HEADER LENGTH                                
         DC    AL2(H40Q)           HEADER CODE                                  
         DC    AL2(H40XX-H40)      DISP TO NEXT HEADER                          
H40X     EQU   *                                                                
*                                                                               
         DC    AL1(10),AL2(01),CL5'HSTRY',AL1(MDTCHQ),AL1(78)                   
         DC    X'00'                                                            
H40XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 41 - APPROVE                                                                  
*====================================================================*          
         SPACE 1                                                                
H41      DC    AL1(H41X-H41)       HEADER LENGTH                                
         DC    AL2(H41Q)           HEADER CODE                                  
         DC    AL2(H41XX-H41)      DISP TO NEXT HEADER                          
H41X     EQU   *                                                                
*                                                                               
         DC    X'00'                                                            
H41XX EQU      *                                                                
         SPACE 1                                                                
*====================================================================*          
* 42 - UN-APPROVE                                                               
*====================================================================*          
         SPACE 1                                                                
H42      DC    AL1(H42X-H42)       HEADER LENGTH                                
         DC    AL2(H42Q)           HEADER CODE                                  
         DC    AL2(H42XX-H42)      DISP TO NEXT HEADER                          
H42X     EQU   *                                                                
*                                                                               
         DC    X'00'                                                            
H42XX EQU      *                                                                
         SPACE 1                                                                
*====================================================================*          
* 43 - MAKEGOOD ANALYSIS                                                        
*====================================================================*          
         SPACE 1                                                                
H43      DC    AL1(H43X-H43)       HEADER LENGTH                                
         DC    AL2(H43Q)           HEADER CODE                                  
         DC    AL2(H43XX-H43)      DISP TO NEXT HEADER                          
H43X     EQU   *                                                                
*                                                                               
         DC    AL1(10),AL2(01),CL5'MGA  ',AL1(MDTCHQ),AL1(123)                  
         DC    X'00'                                                            
H43XX EQU      *                                                                
         SPACE 1                                                                
*====================================================================*          
* 44 - MAKEGOOD REQUEST                                                         
*====================================================================*          
         SPACE 1                                                                
H44      DC    AL1(H44X-H44)       HEADER LENGTH                                
         DC    AL2(H44Q)           HEADER CODE                                  
         DC    AL2(H44XX-H44)      DISP TO NEXT HEADER                          
H44X     EQU   *                                                                
*                                                                               
         DC    AL1(14),AL2(01),CL5'BUYER',AL1(MDTCHQ),AL1(12)                   
         DC    AL4(INBUYER)                                                     
         DC    AL1(14),AL2(02),CL5'PERD ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INPERD)                                                      
         DC    AL1(14),AL2(03),CL5'COST ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INCOST)                                                      
         DC    AL1(14),AL2(04),CL5'MGCOD',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INMGCD)                                                      
         DC    AL1(14),AL2(05),CL5'NOMAT',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INNOMAT)                                                     
         DC    AL1(14),AL2(06),CL5'COST2',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INCOST2)                                                     
         DC    X'00'                                                            
H44XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 45 - CHANGE COMMENTS                                                          
*====================================================================*          
         SPACE 1                                                                
H45      DC    AL1(H45X-H45)       HEADER LENGTH                                
         DC    AL2(H45Q)           HEADER CODE                                  
         DC    AL2(H45XX-H45)      DISP TO NEXT HEADER                          
H45X     EQU   *                                                                
*                                                                               
         DC    X'00'                                                            
H45XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 46 - PROGRAM NAME LOOKUP                                                      
*====================================================================*          
         SPACE 1                                                                
H46      DC    AL1(H46X-H46)       HEADER LENGTH                                
         DC    AL2(H46Q)           HEADER CODE                                  
         DC    AL2(H46XX-H46)      DISP TO NEXT HEADER                          
H46X     EQU   *                                                                
         DC    AL1(14),AL2(01),CL5'QBOOK',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INQBOOK)                                                     
         DC    AL1(14),AL2(02),CL5'TIME ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INTIME)                                                      
         DC    AL1(14),AL2(03),CL5'DAYS ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INH6DAYS)                                                    
         DC    AL1(10),AL2(04),CL5'PROG ',AL1(MDTCHQ),AL1(16)                   
         DC    AL1(14),AL2(05),CL5'WKLY ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INWKLY)                                                      
*                                                                               
         DC    X'00'                                                            
H46XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 47 - CHANGE RNO DATA                                                          
*====================================================================*          
         SPACE 1                                                                
H47      DC    AL1(H47X-H47)       HEADER LENGTH                                
         DC    AL2(H47Q)           HEADER CODE                                  
         DC    AL2(H47XX-H47)      DISP TO NEXT HEADER                          
H47X     EQU   *                                                                
*                                                                               
         DC    X'00'                                                            
H47XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 48 - SAME AS 33, BUT USED FOR OVERRIDE MATCH CRITERIA DIALOGUE                
*      SO THAT WE DON'T CHANGE THE SPOT COST                                    
*====================================================================*          
         SPACE 1                                                                
H48      DC    AL1(H48X-H48)       HEADER LENGTH                                
         DC    AL2(H48Q)           HEADER CODE                                  
         DC    AL2(H48XX-H48)      DISP TO NEXT HEADER                          
H48X     EQU   *                                                                
*                                                                               
         DC    X'00'                                                            
H48XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 01 - PROFILE INFORMATION                                                      
*====================================================================*          
         SPACE 1                                                                
H01      DC    AL1(H01X-H01)       HEADER LENGTH                                
         DC    AL2(H01Q)           HEADER CODE                                  
         DC    AL2(H01XX-H01)      DISP TO NEXT HEADER                          
H01X     EQU   *                                                                
*                                                                               
         DC    AL1(10),AL2(01),CL5'PA0 1',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(02),CL5'PAG 6',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(03),CL5'PMK 1',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(04),CL5'PCL10',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(05),CL5'PI2X7',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(06),CL5'PI2XD',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(07),CL5'PI2Y5',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(09),CL5'PAG 8',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(10),CL5'PB0 4',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(11),CL5'PI2 0',AL1(MDTBIQ),AL1(01)                   
         DC    AL1(10),AL2(12),CL5'PI2XD',AL1(MDTBIQ),AL1(01)                   
         DC    AL1(10),AL2(13),CL5'PI2 4',AL1(MDTBIQ),AL1(01)                   
         DC    AL1(10),AL2(14),CL5'PI2X9',AL1(MDTBIQ),AL1(01)                   
         DC    AL1(10),AL2(15),CL5'PI2S0',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(16),CL5'PI2S1',AL1(MDTBIQ),AL1(01)                   
         DC    AL1(10),AL2(17),CL5'PI2S2',AL1(MDTBIQ),AL1(01)                   
         DC    AL1(10),AL2(18),CL5'PTI A',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(19),CL5'PB0 8',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(20),CL5'PB010',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(21),CL5'PMK 9',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(22),CL5'PE   ',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(23),CL5'DAR 7',AL1(MDTCHQ),AL1(03)                   
         DC    AL1(10),AL2(24),CL5'I2A 6',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(25),CL5'P00 A',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(26),CL5'PCLC2',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(27),CL5'PMK 9',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(28),CL5'I2A15',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(29),CL5'I2A16',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(30),CL5'PMK10',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(31),CL5'I2B10',AL1(MDTBIQ),AL1(01)                   
         DC    AL1(10),AL2(32),CL5'I2B11',AL1(MDTBIQ),AL1(01)                   
         DC    AL1(10),AL2(33),CL5'I2N4 ',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(34),CL5'I2N6 ',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(35),CL5'DAR16',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(36),CL5'PCL15',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(37),CL5'I2C07',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(38),CL5'I2C08',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(39),CL5'I2C06',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(40),CL5'I2C11',AL1(MDTBIQ),AL1(01)                   
         DC    AL1(10),AL2(41),CL5'MK 14',AL1(MDTCHQ),AL1(01)                   
         DC    AL1(10),AL2(42),CL5'P00A7',AL1(MDTCHQ),AL1(01)                   
*                                                                               
         DC    AL1(10),AL2(99),CL5'RATE ',AL1(MDTCHQ),AL1(0)                    
         DC    X'00'                                                            
H01XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 02 (OFFICE/BUYER NAMES)                                                       
*====================================================================*          
         SPACE 1                                                                
H02      DC    AL1(H02X-H02)       HEADER LENGTH                                
         DC    AL2(H02Q)           HEADER CODE                                  
         DC    AL2(H02XX-H02)      DISP TO NEXT HEADER                          
H02X     EQU   *                                                                
*                                                                               
         DC    AL1(10),AL2(01),CL5'OFCNM',AL1(MDTCHQ),AL1(24)                   
         DC    AL1(10),AL2(02),CL5'BYRNM',AL1(MDTCHQ),AL1(24)                   
         DC    X'00'                                                            
H02XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 03 - MATCH HEADER DATA                                                        
*====================================================================*          
         SPACE 1                                                                
H03      DC    AL1(H03X-H03)       HEADER LENGTH                                
         DC    AL2(H03Q)           HEADER CODE                                  
         DC    AL2(H03XX-H03)      DISP TO NEXT HEADER                          
H03X     EQU   *                                                                
*                                                                               
         DC    AL1(14),AL2(01),CL5'MED  ',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INMED)                                                       
         DC    AL1(14),AL2(02),CL5'MKT  ',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INMKT)                                                       
         DC    AL1(14),AL2(03),CL5'STA  ',AL1(MDTCHQ),AL1(8)                    
         DC    AL4(INSTA)                                                       
         DC    AL1(14),AL2(04),CL5'CLT  ',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INCLT)                                                       
         DC    AL1(14),AL2(05),CL5'PRD  ',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INPRD)                                                       
         DC    AL1(14),AL2(06),CL5'PR2  ',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INPR2)                                                       
         DC    AL1(14),AL2(07),CL5'MOS  ',AL1(MDTCHQ),AL1(6)                    
         DC    AL4(INMOS)                             YYMM                      
         DC    AL1(14),AL2(08),CL5'ESRNG',AL1(MDTCHQ),AL1(7)                    
         DC    AL4(INESTRNG)                                                    
         DC    AL1(10),AL2(10),CL5'CNAME',AL1(MDTCHQ),AL1(20)                   
         DC    AL1(10),AL2(11),CL5'PNAME',AL1(MDTCHQ),AL1(20)                   
         DC    AL1(10),AL2(14),CL5'FLMSQ',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(16),CL5'FLAGS',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(17),CL5'I2COM',AL1(MDTCHQ),AL1(0)                    
         DC    AL1(10),AL2(18),CL5'PIGNM',AL1(MDTCHQ),AL1(20)                   
         DC    AL1(10),AL2(19),CL5'STBYT',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(20),CL5'SPOTS',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(21),CL5'DOLS ',AL1(MDTPKQ),AL1(8)                    
         DC    AL1(10),AL2(22),CL5'PAID ',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(23),CL5'CONT ',AL1(MDTCHQ),AL1(12)                   
         DC    AL1(10),AL2(24),CL5'CBLST',AL1(MDTCHQ),AL1(0)                    
         DC    X'00'                                                            
H03XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 04 - MARKET NAMES                                                             
*====================================================================*          
         SPACE 1                                                                
H04      DC    AL1(H04X-H04)       HEADER LENGTH                                
         DC    AL2(H04Q)           HEADER CODE                                  
         DC    AL2(H04XX-H04)      DISP TO NEXT HEADER                          
H04X     EQU   *                                                                
*                                                                               
         DC    AL1(10),AL2(01),CL5'MKT  ',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(02),CL5'MKTNM',AL1(MDTCHQ),AL1(24)                   
         DC    X'00'                                                            
H04XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 05 - ESTIMATE DATA                                                            
*====================================================================*          
         SPACE 1                                                                
H05      DC    AL1(H05X-H05)       HEADER LENGTH                                
         DC    AL2(H05Q)           HEADER CODE                                  
         DC    AL2(H05XX-H05)      DISP TO NEXT HEADER                          
H05X     EQU   *                                                                
*                                                                               
         DC    AL1(10),AL2(01),CL5'EST  ',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(02),CL5'ENAME',AL1(MDTCHQ),AL1(20)                   
         DC    AL1(10),AL2(03),CL5'SDATE',AL1(MDTBDQ),AL1(3)                    
         DC    AL1(10),AL2(04),CL5'EDATE',AL1(MDTBDQ),AL1(3)                    
         DC    AL1(10),AL2(05),CL5'DEMO ',AL1(MDTCHQ),AL1(7)                    
         DC    AL1(10),AL2(06),CL5'OOWST',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(07),CL5'ERATE',AL1(MDTCHQ),AL1(1)                    
         DC    X'00'                                                            
H05XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 06 - BUY INFO                                                                 
*====================================================================*          
         SPACE 1                                                                
H06      DC    AL1(H06X-H06)       HEADER LENGTH                                
         DC    AL2(H06Q)           HEADER CODE                                  
         DC    AL2(H06XX-H06)      DISP TO NEXT HEADER                          
H06X     EQU   *                                                                
*                                                                               
         DC    AL1(10),AL2(01),CL5'BDATA',AL1(MDTCHQ),AL1(H6DX-H6D)             
         DC    AL1(10),AL2(02),CL5'BDEM ',AL1(MDTBIQ),AL1(6)                    
         DC    AL1(10),AL2(03),CL5'BSVI ',AL1(MDTBIQ),AL1(1)                    
*                                                                               
         DC    AL1(14),AL2(07),CL5'PRD  ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INH6PRD)                                                     
         DC    AL1(14),AL2(08),CL5'PR2  ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INH6PR2)                                                     
         DC    AL1(14),AL2(09),CL5'CBL  ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INCBLNET)                                                    
         DC    AL1(14),AL2(10),CL5'EST  ',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INEST)                                                       
         DC    AL1(14),AL2(11),CL5'BUYLN',AL1(MDTBIQ),AL1(2)                    
         DC    AL4(INBUYLN)                                                     
         DC    AL1(14),AL2(12),CL5'ZZCOM',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INH6CMT)                                                     
         DC    AL1(14),AL2(13),CL5'MGC  ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INH6MG)                                                      
         DC    AL1(10),AL2(14),CL5'BSTDT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(15),CL5'BEDDT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(14),AL2(16),CL5'NPW  ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INH6NPW)                                                     
         DC    AL1(10),AL2(17),CL5'BCOST',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(18),CL5'RTYPE',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(14),AL2(20),CL5'DAYS ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INH6DAYS)                                                    
         DC    AL1(14),AL2(21),CL5'TIMES',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INTIME)                                                      
         DC    AL1(14),AL2(22),CL5'SLN  ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INH6SLN)                                                     
         DC    AL1(14),AL2(23),CL5'PROG ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INH6PROG)                                                    
         DC    AL1(14),AL2(24),CL5'DPT  ',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INH6DPT)                                                     
         DC    AL1(14),AL2(25),CL5'ADJ  ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INH6ADJ)                                                     
         DC    AL1(14),AL2(26),CL5'DEMOS',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INH6DEM)                                                     
         DC    AL1(14),AL2(27),CL5'BUYID',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INBUYID)                                                     
         DC    AL1(14),AL2(28),CL5'REP  ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INH6REP)                                                     
         DC    AL1(10),AL2(29),CL5'UDEM ',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(30),CL5'COST2',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(31),CL5'NETC2',AL1(MDTBIQ),AL1(4)                    
         DC    AL1(10),AL2(32),CL5'BNET ',AL1(MDTBIQ),AL1(4)                    
*                                                                               
         DC    X'00'                                                            
H06XX    EQU   *                                                                
*                                                                               
         EJECT                                                                  
*====================================================================*          
* 07 - BUYLINE ORBIT DATA                                                       
*====================================================================*          
         SPACE 1                                                                
H07      DC    AL1(H07X-H07)       HEADER LENGTH                                
         DC    AL2(H07Q)           HEADER CODE                                  
         DC    AL2(H07XX-H07)      DISP TO NEXT HEADER                          
H07X     EQU   *                                                                
*                                                                               
         DC    AL1(10),AL2(01),CL5'ORDAY',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(02),CL5'ORSTM',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(03),CL5'ORETM',AL1(MDTBIQ),AL1(2)                    
         DC    AL1(10),AL2(04),CL5'ORPRG',AL1(MDTCHQ),AL1(7)                    
         DC    AL1(10),AL2(05),CL5'ORDEM',AL1(MDTBIQ),AL1(4)                    
*                                                                               
         DC    X'00'                                                            
H07XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 08 - SPOT INFO                                                                
*====================================================================*          
         SPACE 1                                                                
H08      DC    AL1(H08X-H08)       HEADER LENGTH                                
         DC    AL2(H08Q)           HEADER CODE                                  
         DC    AL2(H08XX-H08)      DISP TO NEXT HEADER                          
H08X     EQU   *                                                                
*                                                                               
         DC    AL1(10),AL2(01),CL5'SPDET',AL1(MDTCHQ),AL1(S8DX-S8D)             
         DC    AL1(14),AL2(02),CL5'SPDAT',AL1(MDTBDQ),AL1(0)                    
         DC    AL4(INSPDAT)                                                     
         DC    AL1(14),AL2(03),CL5'SPNUM',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INSPNUM)                                                     
         DC    AL1(14),AL2(04),CL5'SPOTO',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INOTO)                                                       
         DC    AL1(14),AL2(05),CL5'PRD  ',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INAPRD)                                                      
         DC    AL1(14),AL2(06),CL5'PRD2 ',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INA2PRD)                                                     
*                                                                               
         DC    X'00'                                                            
H08XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 09 - AFFIDAVIT DATA                                                           
*====================================================================*          
         SPACE 1                                                                
H09      DC    AL1(H09X-H09)       HEADER LENGTH                                
         DC    AL2(H09Q)           HEADER CODE                                  
         DC    AL2(H09XX-H09)      DISP TO NEXT HEADER                          
H09X     EQU   *                                                                
*                                                                               
         DC    AL1(10),AL2(01),CL5'AFFID',AL1(MDTCHQ),AL1(S9DX-S9D)             
         DC    AL1(14),AL2(02),CL5'SPDAT',AL1(MDTBDQ),AL1(0)                    
         DC    AL4(INSPADAT)                                                    
         DC    AL1(14),AL2(03),CL5'TIME ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INSPTIM)                                                     
         DC    AL1(14),AL2(04),CL5'PRD  ',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INAPRD)                                                      
         DC    AL1(14),AL2(05),CL5'CNET ',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INCBLNT2)                                                    
         DC    AL1(14),AL2(06),CL5'PR2  ',AL1(MDTCHQ),AL1(3)                    
         DC    AL4(INA2PRD)                                                     
         DC    AL1(14),AL2(07),CL5'OCOST',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INOCOST)                                                     
         DC    AL1(14),AL2(08),CL5'INVNO',AL1(MDTCHQ),AL1(10)                   
         DC    AL4(ININVNUM)                                                    
         DC    AL1(14),AL2(09),CL5'EST  ',AL1(MDTBIQ),AL1(10)                   
         DC    AL4(DUMMY)                                                       
         DC    AL1(10),AL2(21),CL5'RCREL',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(22),CL5'EMAIL',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(23),CL5'NOSEP',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(14),AL2(24),CL5'AFCST',AL1(MDTBIQ),AL1(4)                    
         DC    AL4(INAFCOST)                                                    
         DC    AL1(10),AL2(25),CL5'CSTCH',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(26),CL5'CMLFL',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(27),CL5'CMLPR',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(14),AL2(28),CL5'OVFLM',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INOVFLM)                                                     
         DC    AL1(14),AL2(29),CL5'OVCST',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INOVCST)                                                     
         DC    AL1(14),AL2(30),CL5'OVTIM',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INOVTIM)                                                     
         DC    AL1(10),AL2(31),CL5'CMLTM',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(14),AL2(32),CL5'OVSLN',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INOVSLN)                                                     
         DC    AL1(14),AL2(33),CL5'SETST',AL1(MDTBIQ),AL1(1)                    
         DC    AL4(INSETST)                                                     
         DC    AL1(14),AL2(34),CL5'OVINT',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INOVINT)                                                     
         DC    AL1(10),AL2(35),CL5'CMSLN',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(14),AL2(36),CL5'NOOVR',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INNOOVR)                                                     
         DC    X'00'                                                            
H09XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 0A -  COMMENT DATA                                                            
*====================================================================*          
         SPACE 1                                                                
H0A      DC    AL1(H0AX-H0A)       HEADER LENGTH                                
         DC    AL2(H0AQ)           HEADER CODE                                  
         DC    AL2(H0AXX-H0A)      DISP TO NEXT HEADER                          
H0AX     EQU   *                                                                
*                                                                               
         DC    AL1(10),AL2(01),CL5'COM  ',AL1(MDTCHQ),AL1(0)                    
*                                                                               
         DC    X'00'                                                            
H0AXX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 0B - VARIOUS BUY HEADLINE OPTIONS                                             
*====================================================================*          
         SPACE 1                                                                
H0B      DC    AL1(H0BX-H0B)       HEADER LENGTH                                
         DC    AL2(H0BQ)           HEADER CODE                                  
         DC    AL2(H0BXX-H0B)      DISP TO NEXT HEADER                          
H0BX     EQU   *                                                                
*                                                                               
         DC    AL1(14),AL2(01),CL5'NOGOL',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INNOGOL)                                                     
         DC    AL1(14),AL2(02),CL5'NODEM',AL1(MDTCHQ),AL1(1)                    
         DC    AL4(INNODEM)                                                     
         DC    X'00'                                                            
H0BXX EQU      *                                                                
         SPACE 1                                                                
*====================================================================*          
* 0C - INVOICE DATA                                                             
*====================================================================*          
         SPACE 1                                                                
H0C      DC    AL1(H0CX-H0C)       HEADER LENGTH                                
         DC    AL2(H0CQ)           HEADER CODE                                  
         DC    AL2(H0CXX-H0C)      DISP TO NEXT HEADER                          
H0CX     EQU  *                                                                 
*                                                                               
         DC    AL1(10),AL2(01),CL5'INV  ',AL1(MDTCHQ),AL1(10)                   
         DC    AL1(10),AL2(02),CL5'BOOK ',AL1(MDTCHQ),AL1(6)                    
         DC    AL1(10),AL2(03),CL5'BLRNM',AL1(MDTCHQ),AL1(6)                    
         DC    AL1(10),AL2(04),CL5'EASI ',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(05),CL5'CONT ',AL1(MDTCHQ),AL1(12)                   
         DC    AL1(10),AL2(06),CL5'FILM ',AL1(MDTCHQ),AL1(12)                   
         DC    AL1(10),AL2(07),CL5'INVFL',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(10),AL2(08),CL5'I2DAT',AL1(MDTCDQ),AL1(2)                    
         DC    AL1(10),AL2(09),CL5'I2TIM',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(10),CL5'REP  ',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(11),CL5'IMKEY',AL1(MDTCHQ),AL1(SNVIMLQ-2)            
         DC    AL1(10),AL2(12),CL5'FLMSQ',AL1(MDTHXQ),AL1(2)                    
         DC    AL1(10),AL2(13),CL5'FLMST',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(14),CL5'SORCE',AL1(MDTCHQ),AL1(4)                    
         DC    AL1(10),AL2(15),CL5'USER ',AL1(MDTHXQ),AL1(2)                    
         DC    AL1(10),AL2(16),CL5'STA  ',AL1(MDTCHQ),AL1(5)                    
         DC    AL1(10),AL2(17),CL5'BTCDT',AL1(MDTCDQ),AL1(2)                    
         DC    X'00'                                                            
H0CXX EQU      *                                                                
         SPACE 1                                                                
*====================================================================*          
* 0D - LOW EST DEMO DATA                                                        
*====================================================================*          
         SPACE 1                                                                
H0D      DC    AL1(H0DX-H0D)       HEADER LENGTH                                
         DC    AL2(H0DQ)           HEADER CODE                                  
         DC    AL2(H0DXX-H0D)      DISP TO NEXT HEADER                          
H0DX     EQU   *                                                                
*                                                                               
         DC    AL1(10),AL2(01),CL5'PRD  ',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(02),CL5'EST  ',AL1(MDTBIQ),AL1(1)                    
         DC    AL1(10),AL2(03),CL5'PDEMO',AL1(MDTCHQ),AL1(7)                    
         DC    X'00'                                                            
H0DXX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 0E - REASON CODES                                                             
*====================================================================*          
         SPACE 1                                                                
H0E      DC    AL1(H0EX-H0E)       HEADER LENGTH                                
         DC    AL2(H0EQ)           HEADER CODE                                  
         DC    AL2(H0EXX-H0E)      DISP TO NEXT HEADER                          
H0EX     EQU   *                                                                
*                                                                               
         DC    AL1(14),AL2(01),CL5'RSNCD',AL1(MDTCHQ),AL1(6)                    
         DC    AL4(INRSNCD)                                                     
         DC    AL1(10),AL2(02),CL5'INPRQ',AL1(MDTCHQ),AL1(1)                    
         DC    AL1(14),AL2(03),CL5'RSNTX',AL1(MDTCHQ),AL1(0)                    
         DC    AL4(INRSNTX)                                                     
         DC    X'00'                                                            
H0EXX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 11 - COMMERCIAL SUBSTITUTION BRANDS                                           
*====================================================================*          
         SPACE 1                                                                
H11      DC    AL1(H11X-H11)       HEADER LENGTH                                
         DC    AL2(H11Q)           HEADER CODE                                  
         DC    AL2(H11XX-H11)      DISP TO NEXT HEADER                          
H11X     EQU   *                                                                
*                                                                               
         DC    AL1(10),AL2(01),CL5'CMSEQ',AL1(MDTHXQ),AL1(2)                    
         DC    AL1(10),AL2(02),CL5'CMPRD',AL1(MDTCHQ),AL1(3)                    
         DC    AL1(10),AL2(03),CL5'CMSDT',AL1(MDTBDQ),AL1(3)                    
         DC    AL1(10),AL2(04),CL5'CMEDT',AL1(MDTBDQ),AL1(3)                    
         DC    X'00'                                                            
H11XX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* FD - VERSION DATA                                                             
*====================================================================*          
         SPACE 1                                                                
HFD      DC    AL1(HFDX-HFD)       HEADER LENGTH                                
         DC    AL2(HFDQ)           HEADER CODE                                  
         DC    AL2(HFDXX-HFD)      DISP TO NEXT HEADER                          
HFDX     EQU   *                                                                
*                                                                               
         DC    AL1(14),AL2(01),CL5'VERSN',AL1(MDTHXQ),AL1(4)                    
         DC    AL4(INVERFD)                                                     
         DC    X'00'                                                            
HFDXX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* FE - VERSION DATA                                                             
*====================================================================*          
         SPACE 1                                                                
HFE      DC    AL1(HFEX-HFE)       HEADER LENGTH                                
         DC    AL2(HFEQ)           HEADER CODE                                  
         DC    AL2(HFEXX-HFE)      DISP TO NEXT HEADER                          
HFEX     EQU   *                                                                
*                                                                               
         DC    AL1(14),AL2(01),CL5'VERSN',AL1(MDTHXQ),AL1(4)                    
         DC    AL4(INVERSN)                                                     
         DC    X'00'                                                            
HFEXX    EQU   *                                                                
         SPACE 1                                                                
*====================================================================*          
* 99 - TEST PHASE                                                               
*====================================================================*          
         SPACE 1                                                                
H99      DC    AL1(H99X-H99)       HEADER LENGTH                                
         DC    AL2(H99Q)           HEADER CODE                                  
         DC    AL2(H99XX-H99)      DISP TO NEXT HEADER                          
H99X     EQU   *                                                                
*                                                                               
         DC    X'00'                                                            
H99XX    EQU   *                                                                
*                                                                               
         DC    X'00'               EOFT                                         
         EJECT                                                                  
       ++INCLUDE SPMAKWRK                                                       
       ++INCLUDE SPSYSFAC                                                       
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE SPSTABLK                                                       
       ++INCLUDE SPSTAPACKD                                                     
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE SPGENSNV                                                       
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
*                                                                               
       ++INCLUDE FAUTL                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073SPMAK00   02/23/21'                                      
         END                                                                    
