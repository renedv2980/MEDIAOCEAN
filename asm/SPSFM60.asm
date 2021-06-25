*          DATA SET SPSFM60    AT LEVEL 015 AS OF 05/27/10                      
*PHASE T21760A                                                                  
*                                                                               
***********************************************************************         
* AKAT 015 26MAY10 ADD NE,NF,KP AND KW TO EWSEQTAB                              
* AKAT 014 05MAY10 DON'T ABEND IF X'01' ELEM NOT FOUND CBLDEF CHANGED!          
* MCHO 013 10APR07 ADDING KO - KOOTENAY TO EWSEQTAB                             
* PWES 012 19JUL04 ACTION PFKEY AFTER ANY UPDATE / REMOVE PF11 FOR NEXT         
* PWES 011 22JUN04 COPY ACTION                                                  
* PWES 010 22MAY02 ADD CE TO EWSEQTAB                                           
* PWES 009 14MAR02 ADD SS TO EWSEQTAB                                           
* PWES 008 06FEB02 ADD KA/OK TO EWSEQTAB                                        
* PWES 007 07JAN02 VALIDATE NETWORK ON ADD (MUST HAVE MASTER/BE CABLE)          
* PWES 006 17DEC01 ALLOW E-W IN MARKET NUMBER ORDER                             
* PWES 005 12DEC01 ALLOW *DEF RECORD SO CAN ALTER SEQUENCE OF MARKETS           
*                  + ALLOW RESEQUENCE OF ALL MARKETS TO EAST-WEST               
* PWES 004 21NOV01 'PF11 FOR NEXT' MESSAGE ON DISPLAY                           
* PWES 003 30OCT01 PRE-RELEASE REFINEMENTS                                      
* PWES 002 24OCT01 PRE-RELEASE REFINEMENTS                                      
* PWES 001 27SEP01 NEW - LIVE FOR DDS/TESTING                                   
***********************************************************************         
*                                                                     *         
*  TITLE: T21760 - CABLE NETWORK PRORATION MAINTENANCE                *         
*                                                                     *         
*  CALLED FROM: SPOT CONTROLLER (T21700), WHICH CALLS                 *         
*               GEGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  INPUTS:  SCREEN SCSFM55  (T29755)  - MAINTENANCE                   *         
*           SCREEN SCSFM54  (T29754)  - LIST                          *         
*                                                                     *         
*  OUTPUTS: UPDATED NETWORK (X'0D11') RECORDS (CBLPRO RECORDS)        *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - GETEL REGISTER                                        *         
*          R4 - DEFINITION RECORD                                     *         
*          R5 - WORK                                                  *         
*          R6 - WORK                                                  *         
*          R7 - SECOND BASE REGISTER                                  *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'T21760 NETWORK MAINTENANCE'                                     
T21760   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21760*,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD           GENERAL PRINT AREAS                         
         USING SPOOLD,R8                                                        
*                                                                               
         BAS   RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BE    LR                                                               
         CLI   MODE,PRINTREP       ADD REQUEST FOR NSL REP (44)                 
         BE    REQREC                                                           
         CLI   MODE,XRECADD        AFTER ADDREC                                 
         BE    REQREC                                                           
         CLI   MODE,XRECPUT        AFTER PUTREC                                 
         BE    REQREC                                                           
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
                                                                                
**********************************************************************          
*  VALIDATE KEY ROUTINE                                              *          
**********************************************************************          
                                                                                
VK       LA    R4,MYKEY                                                         
         USING NDEFRECD,R4                                                      
         XC    MYKEY,MYKEY                                                      
*                                                                               
         MVC   NDEFKTYP,=AL2(NDEFRECQ)    NETWORK RECORD ID                     
         MVC   NDEFKAGY,AGENCY     AGENCY ID                                    
*                                                                               
         BAS   RE,VMED             *** VALIDATE (SET) MEDIA ***                 
*                                                                               
         BAS   RE,VNTWK            *** VALIDATE NETWORK (STATION) ***           
*                                                                               
         LA    R2,MNTCLTH          CLIENT                                       
         CLI   5(R2),0                                                          
         BE    *+12                                                             
         BAS   RE,VCLT             *** VALIDATE CLIENT ***                      
         B     VK10                                                             
         CLI   ACTNUM,ACTLIST                                                   
         BE    VK10                                                             
         CLI   MNTESTH+5,0         REQUIRED IF ESTIMATE INPUT                   
         BNE   MIS                 ERROR                                        
         B     VK20                SKIP CLIENT, ESTIMATE                        
*                                                                               
VK10     CLI   ACTNUM,ACTLIST      BOTTOM LINE FOR LIST                         
         BE    VKEX                                                             
*                                                                               
         LA    R2,MNTESTH          ESTIMATE                                     
         CLI   5(R2),0                                                          
         BE    VK20                                                             
         BAS   RE,VEST             *** VALIDATE ESTIMATE ***                    
*                                                                               
VK20     CLI   ACTNUM,ACTCOPY                                                   
         BE    VKCOPY                                                           
         BAS   RE,VPAGE            *** VALIDATE 'PAGE' ***                      
         B     VKEX                                                             
*                                                                               
* COPY FROM KEY IS THAT FOR REC LAST DISPLAYED WHICH IS SET IN COPYKEY          
* BY DISPREC. ENSURE CONTENT OF COPYKEY WAS SET IMMEDIATLY PRIOR TO             
* COPY ACTION ELSE CONTENT COULD BE DEBRIS OR A HANGOVER FROM EARLIER           
VKCOPY   LA    R2,CONACTH                                                       
         CLC   TWALREC,RECNUM                                                   
         BNE   *+8                 BEEN ELSEWHERE, COPYKEY MAY BE DUF           
         CLI   TWALACT,ACTDIS                                                   
         BE    *+14                ENSURE WAS DISP FOR EXTRA SAFETY             
         MVC   ERRNUM,=AL2(1247)                                                
         B     SPERREX                                                          
*                                                                               
         CLC   NDEFKEY,COPYKEY     ENSURE NOT EXACT SAME KEY                    
         BNE   *+14                                                             
         MVC   ERRNUM,=AL2(398)                                                 
         B     SPERREX                                                          
         CLC   NDEFKEY(NDEFKCLT-NDEFRECD),COPYKEY   MUST BE SAME NWK            
         BE    *+14                                                             
         MVC   ERRNUM,=AL2(836)                                                 
         B     SPERREX                                                          
* CLIENT/ESTIMATE HEIRARCHICAL ORDER MUST BE ADHERED TO                         
         CLI   NDEFKEST,0          IF CREATING AN ESTIMATE LEVEL                
         BE    VKEX                                                             
         CLC   NDEFKEY(NDEFKEST-NDEFRECD),COPYKEY   MUST BE SAME CLT            
         BE    VKEX                ALSO CLT LVL REC MUST EXIST - AS WE          
         MVC   ERRNUM,=AL2(423)    JUST DISPD FROM REC IT MUST BE THERE         
         LA    R2,MNTCLTH                                                       
         B     SPERREX                                                          
*                                                                               
VKEX     CLI   ACTNUM,ACTADD       NO RECORD DATA IS REQUIRED FOR ADD           
         BNE   *+8                 SO PRETEND SOME HAS BEEN INPUT AND           
         MVI   MNTDATAH+5,1        THE RECORD WILL BE ADDED IN ONE HIT          
         MVC   KEY,MYKEY                                                        
         B     EXIT                                                             
*                                                                               
VKERR    B     INV                 ERROR                                        
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*  VALIDATE MEDIA                                                    *          
**********************************************************************          
                                                                                
VMED     NTR1  ,                                                                
         XC    TEMPFLD,TEMPFLD     FORCE FAKE INPUT                             
         MVC   TEMPFLD,=XL9'0900000000010000E3'  T FOR NETWORK                  
         LA    R2,TEMPFLD                                                       
         GOTO1 VALIMED                                                          
         XIT1  ,                                                                
         EJECT                                                                  
                                                                                
**********************************************************************          
*  VALIDATE NETWORK                                                  *          
**********************************************************************          
                                                                                
VNTWK    NTR1  ,                                                                
         LA    R4,MYKEY                                                         
         USING NDEFRECD,R4                                                      
*                                                                               
         LA    R2,MNTNTWKH         NETWORK                                      
         CLI   5(R2),4             CHECK FOR AGENCY LEVEL DEFAULT               
         BNE   VN5                                                              
         CLC   8(4,R2),=C'*DEF'    DEFAULT RECORD IS SPECIAL                    
         BNE   VN5                                                              
         CLI   ACTNUM,ACTADD                                                    
         BE    INV                 CANNOT ADD HERE                              
         B     VN20                LEAVE NDEFKNET NULL                          
*                                                                               
VN5      CLI   ACTNUM,ACTLIST                                                   
         BNE   *+12                NOT REQUIRED IF LIST                         
         CLI   LSTCLT,C'>'                                                      
         BNE   VN10                                                             
         GOTO1 ANY                 REQUIRED                                     
*                                                                               
         CLI   ACTNUM,ACTADD       NEEDS VALIDATION WHEN ADD CBLPRO/MKT         
         BNE   VN10                                                             
         MVI   USEIONUM,2          USE AIO2                                     
         GOTO1 VALISTA             VALIDATE STATION (SPSFM00 LEVEL)             
*                                  (VALID THUS MUST HAVE MASTER REC)            
         CLC   QMKT,=C'0000'                                                    
         BNE   INV                 MUST BE A (CABLE) NETWORK STATION            
         MVI   USEIONUM,0          RESTORE AIO1                                 
         MVC   AIO,AIO1                                                         
*                                                                               
VN10     SR    R5,R5                                                            
         IC    R5,5(R2)                                                         
         LTR   R5,R5                                                            
         BNP   VNEX                                                             
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   NDEFKNET(0),MNTNTWK MOVE NETWORK INTO KEY                        
         OC    NDEFKNET,SPACES     SPACEPAD                                     
VN20     MVC   SVNTWK,NDEFKNET                                                  
*                                                                               
VNEX     XIT1  ,                                                                
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*  VALIDATE CLIENT                                                   *          
**********************************************************************          
                                                                                
VCLT     NTR1  ,                                                                
         LA    R4,MYKEY                                                         
         USING NDEFRECD,R4                                                      
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   VCLT10                                                           
*                                                                               
         CLI   8(R2),C'>'          START AT FILTER                              
         BNE   VCLT20              NO                                           
         MVC   TEMPCLT,8(R2)       SAVE TO RESTORE IT LATER                     
         ICM   R5,15,8(R2)                                                      
         SLL   R5,8                GET RID OF C'>'                              
         STCM  R5,15,8(R2)                                                      
         OC    8(4,R2),SPACES                                                   
         ZIC   RF,5(R2)            DECRIMENT LENGTH                             
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BP    *+8                                                              
         B     INV                 ERROR                                        
         STC   RF,5(R2)                                                         
         OI    FLAG,CLSTART        START AT FILTER                              
         B     VCLT25                                                           
*                                                                               
VCLT10   MVC   KEY,NDEFKEY         CHECK FOR NETWORK KEY ON FILE                
         GOTO1 HIGH                READ THE KEY                                 
         CLC   KEY(13),KEYSAVE     SAME KEY (NETWORK LEVEL ON FILE)?            
         JE    VCLT20              YES - SO CONTINUE                            
*                                                                               
         LA    R2,MNTNTWKH         ELSE - GET A(NETWORK FIELD)                  
         B     NFND                ERROR                                        
*                                                                               
VCLT20   NI    FLAG,X'FF'-CLSTART                                               
         GOTO1 VALICLT                                                          
*                                                                               
VCLT25   TM    FLAG,CLSTART                                                     
         BZ    VCLT30                                                           
         CLI   5(R2),2                                                          
         BL    VCERR                                                            
*                                                                               
         MVC   QCLT,SPACES                                                      
         MVI   QCLT+2,C'A'                                                      
         ZIC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   QCLT(0),8(R2)                                                    
         GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
         CLI   0(R1),0                                                          
         BNE   VCERR                                                            
         MVC   8(L'TEMPCLT,R2),TEMPCLT  RESTORE ORIGINAL VALUE                  
         ZIC   RF,5(R2)            INCREMENT LENGTH                             
         LA    RF,1(RF)                                                         
         STC   RF,5(R2)                                                         
*                                                                               
VCLT30   MVC   NDEFKCLT,BCLT       MOVE CLIENT INTO THE KEY                     
         MVC   SVCLT,BCLT                                                       
         XIT1  ,                                                                
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*  VALIDATE ESTIMATE                                                 *          
**********************************************************************          
                                                                                
VEST     NTR1  ,                                                                
         LA    R4,MYKEY                                                         
         USING NDEFRECD,R4                                                      
*                                                                               
         TM    4(R2),X'08'         TEST VALID NUMERIC                           
         BZ    VKERR                                                            
         CLI   5(R2),3                                                          
         BH    VKERR                                                            
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         CH    RE,=H'1'            TEST IN RANGE 1-255                          
         BL    VKERR                                                            
         CH    RE,=H'255'                                                       
         BH    VKERR                                                            
         STC   RE,NDEFKEST         SET BINARY ESTIMATE                          
*                                                                               
VESTEX   XIT1  ,                                                                
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*  DISPLAY KEY                                                       *          
**********************************************************************          
                                                                                
DK       L     R4,AIO                                                           
         USING NDEFRECD,R4         NETWORK DEFINITION RECORD                    
         MVC   MNTNTWK,NDEFKNET    NETWORK                                      
         OC    NDEFKNET,NDEFKNET                                                
         BNZ   *+10                                                             
         MVC   MNTNTWK,=C'*DEF'    DEFAULT RECORD IS SPECIAL                    
         OI    MNTNTWKH+6,X'80'                                                 
         OC    NDEFKCLT,NDEFKCLT   CLIENT IN PACKED FORMAT                      
         BZ    DK10                                                             
         GOTO1 CLUNPK,DMCB,NDEFKCLT,QCLT                                        
         MVC   MNTCLT(L'QCLT),QCLT CLIENT UNPACKED FORMAT                       
         OI    MNTCLTH+6,X'80'     CLIENT                                       
*                                                                               
DK10     EDIT  NDEFKEST,(3,MNTEST),ALIGN=LEFT ESTIMATE NUMBER                   
         OI    MNTESTH+6,X'80'                                                  
         BAS   RE,VPAGE                                                         
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
*  VALIDATE PAGE                                                     *          
*  DETERMINE WHAT DATA (COST/OFFSET) TO SHOW ON THIS DISPLAY         *          
*  NOTE: HAD TO DO THIS AND NOT SHOW ALL DATA ON ONE SCREEN AS THE   *          
*        SCREEN WOULD EXCEED 3270 OUTPUT TRANSLATOR MAX OF 2K        *          
**********************************************************************          
                                                                                
VPAGE    NTR1  ,                                                                
         LA    R2,MNTPAGEH         PAGE                                         
         CLI   5(R2),0                                                          
         BE    VPAGE10             DEFAULT TO COST                              
         CLI   8(R2),C'C'                                                       
         BE    VPAGE10                                                          
         CLI   8(R2),C'O'                                                       
         BE    VPAGE20                                                          
         B     INV                                                              
VPAGE10  CLI   PFKEY,5                                                          
         BE    VPAGE25                                                          
VPAGE15  MVC   8(6,R2),=C'COST% '                                               
         B     VPAGE30                                                          
*                                                                               
VPAGE20  CLI   PFKEY,5                                                          
         BE    VPAGE15                                                          
VPAGE25  MVC   8(6,R2),=C'OFFSET'                                               
*                                                                               
VPAGE30  OI    6(R2),X'80'                                                      
VPAGEX1  XIT1  ,                                                                
         EJECT                                                                  
                                                                                
**********************************************************************          
*  DISPLAY RECORD                                                    *          
**********************************************************************          
                                                                                
DR       TWAXC MNTMOVEH,MNTMVTOH        CLEAR MOVE DETAILS                      
         TWAXC MNTMKTH,MNTCOSLH,PROT=Y  CLEAR RECORD DETAILS                    
*                                                                               
         LA    R2,MNTNTWKH                                                      
         L     R3,AIO              DON'T SHOW IF NETDEF RECORD                  
         MVI   ELCODE,NDEFNELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   INV                                                              
         CLI   NDEFNET-NDEFEL02(R3),NDEFCABQ                                    
         BNE   INV                                                              
*                                                                               
         MVC   COPYKEY,MYKEY       COPY IS BASED ON LAST DISPD REC/KEY          
         L     R3,AIO                                                           
         SR    R5,R5               INITIALIZE TOTAL PERCENTAGE COST             
         MVI   ELCODE,NDEFELQ      X'01' ELEMENT                                
         BAS   RE,GETEL                                                         
         LA    R2,MNTMKTH                                                       
*                                                                               
DR10     BNE   DREX                                                             
         USING NDEFEL01,R3                                                      
                                                                                
*------- MARKET ----->                                                          
                                                                                
         MVC   8(L'NDEFMSUF,R2),NDEFMSUF                                        
         OI    6(R2),X'80'                                                      
         BAS   RE,NEXT             BUMP TO NEXT FIELD                           
                                                                                
*------- OFFSET ------>                                                         
                                                                                
         CLI   MNTPAGE,C'C'                                                     
         BE    DRCOST                                                           
*                                  R2 POINTS TO HEADER                          
         EDIT  NDEFOSET,(4,8(R2)),0,FLOAT=-,ZERO=BLANK,                >        
               ALIGN=LEFT                                                       
         CLC   NDEFPCT,=F'-1'      NOT BOUGHT                                   
         BE    DR20                                                             
         B     DR15                STILL ACCUM TOTAL COST                       
                                                                                
*------- COST & TOTAL COST ------->                                             
                                                                                
DRCOST   CLC   NDEFPCT,=F'-1'      NOT BOUGHT                                   
         BNE   *+14                                                             
         MVC   8(L'MNTDATA,R2),=CL7'NB'                                         
         B     DR20                                                             
         EDIT  NDEFPCT,(L'MNTDATA,8(R2)),3,ALIGN=LEFT                           
*                                                                               
DR15     ICM   R4,15,NDEFPCT       NDEFPCT IS NOT ALIGNED                       
         AR    R5,R4               TOTAL PERCENTAGE COST                        
*                                                                               
DR20     OI    6(R2),X'80'                                                      
         BAS   RE,NEXT                                                          
         BAS   RE,NEXTEL           FIND NEXT ELEMENT                            
         B     DR10                                                             
*                                                                               
DREX     EDIT  (R5),(L'MNTTOT,MNTTOT),3    PERCENTAGE OF NETWORK BUY            
         OI    MNTTOTH+6,X'80'                                                  
*                                                                               
         CLI   MODE,PRINTREP       <<< ??? >>>                                  
         BNE   EXIT                                                             
         LA    R2,CONACTH                                                       
         B     REQOV               REPORT WILL BE GENERATED OV                  
         DROP  R3                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*  VALIDATE RECORD                                                   *          
**********************************************************************          
                                                                                
VR       XC    MYKEY,MYKEY                                                      
         MVC   MYKEY,KEY                                                        
         MVC   AIO,AIO1                                                         
         L     R3,AIO                                                           
         MVC   0(13,R3),MYKEY                                                   
*        OI    GENSTAT2,RETEQSEL   REDISPLAY SELECTION FROM LIST                
         SR    R5,R5               INITIALIZE TOTAL PERCENTAGE COST             
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    ADDNEW              ADD NEW RECORD                               
         CLI   ACTNUM,ACTCOPY                                                   
         BNE   VR10                                                             
         BRAS  RE,COPYREC                                                       
         B     EXIT                                                             
*                                                                               
VR10     CLI   MNTMVTOH+5,0        DELETE INDICATED IN 'MOVE TO' (DDS)          
         BE    VR20                                                             
         CLC   =C'$DEL',MNTMVTO    DDS WANTS TO DELETE THE RECORD?              
         BE    DELEST              DELETE ESTIMATE LEVEL RECORD                 
*                                                                               
VR20     CLI   MNTMOVEH+5,0                                                     
         BE    VR30                                                             
         LA    R2,MNTMOVEH         CHECK STD SINGLE MOVE OR RESEQ               
         CLI   5(R2),2                                                          
         BL    INV                                                              
         BE    VR25                - SINGLE MARKET MOVE                         
         CLC   MNTMOVE(3),=C'ALL'  (CAN ONLY BE MAX 3 CHR INPUT)                
         BNE   INV                                                              
         LA    R2,MNTMVTOH         - ALL MARKET RESEQUENCE                      
         GOTO1 ANY                                                              
         CLC   MNTMVTO(2),=C'EW'   CHECK CONFIRMED BY SEQ KEYWORD               
         BNE   INV                 MUST CONFIRM RESEQUENCE                      
         BAS   RE,MOVEEW           RESEQUENCE ALL MARKETS IN RECORD             
         B     VR30                                                             
VR25     LA    R2,MNTMVTOH                                                      
         GOTO1 ANY                                                              
         CLC   MNTMOVE(2),MNTMVTO                                               
         BE    INV                 CAN'T MOVE TO ITSELF                         
         BAS   RE,MOVE             MOVE MARKET POSITION IN RECORD               
*                                  CAN MOVE & CHANGE IN ONE TRANSACTION         
VR30     EQU   *                                                                
* LET THEM HAVE COSTS/OFFSETS ON DEFAULT RECORD IF THEY WANT !                  
*        CLC   MNTNTWK,=C'*DEF'    NO COSTS/OFFSETS ON DEFAULT RECORD           
*        BE    *+8                                                              
         BAS   RE,CHANGE           USER WANTS TO CHANGE DATA                    
*                                                                               
VREX     CLI   PFKEY,5             HANDLE PF5 - AFTER DOING CHANGES             
         BNE   *+8                                                              
         BAS   RE,VPAGE                                                         
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
**********************************************************************          
*  ADD A CBLPRO RECORD (AND HENCE A CBLMKT RECORD) BASED ON MOST     *          
*  APPROPRIATE CBLMKT RECORD                                         *          
*  HEIRARCHICAL ORDER MUST BE ADHERED TO, EACH NEW RECORD IS         *          
*  EFFECTIVELY A 'COPY' OF HIGHER LEVEL RECORD                       *          
*  ESTIMATE LEVEL MUST ALWAYS HAVE SAME MARKET/SUFFIX DEFINITIONS AS *          
*  THE CORRESPONDING CLIENT LEVEL                                    *          
**********************************************************************          
                                                                                
ADDNEW   EQU   *                                                                
*  CHECK NETWORK IS APPROPRIATE TO ADD AS SPECIALTY CABLE                       
*  - CANNOT HAVE ANY SPILL (SPILL RECORDS ARE ADDED BEFORE NETDEF ETC           
*    RECS DEFINED SO CANNOT CROSS CHECK STATUS AT TIME OF SPILL ADD             
         LA    RF,KEY                                                           
         USING SDEFRECD,RF                                                      
         XC    KEY,KEY                                                          
         MVC   SDEFKTYP,=X'0D13'                                                
         MVC   SDEFKAGY,AGENCY                                                  
         MVI   SDEFKRSV,C'0'                 CIS                                
         MVC   SDEFKSTA(4),MYKEY+4                                              
         DROP  RF                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BE    ERRSPL                                                           
         MVC   KEY,KEYSAVE                                                      
         MVI   KEY+(SDEFKRSV-SDEFRECD),C'1'  BBM                                
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BE    ERRSPL                                                           
*                                                                               
         XC    KEY,KEY             NETWORK LEVEL BASED ON DEFAULT REC           
         MVC   KEY(4),MYKEY        BASIC (DEFAULT) RECORD KEY                   
         CLI   MNTCLTH+5,0         CLIENT LEVEL BASED ON NETWORK REC            
         BE    ADD10                                                            
         MVC   KEY+4(4),MYKEY+4    NETWORK                                      
         CLI   MNTESTH+5,0         ESTIMATE LEVEL BASED ON CLIENT REC           
         BE    *+10                                                             
         MVC   KEY+8(2),MYKEY+8    CLIENT                                       
ADD10    GOTO1 HIGH                FIND HIGHER LEVEL RECORD                     
*                                                                               
         LA    R2,MNTNTWKH                                                      
         CLC   KEY(8),KEYSAVE                                                   
         BNE   NFND                NETWORK / DEFAULT ERROR                      
         LA    R2,MNTCLTH                                                       
         CLC   KEY(10),KEYSAVE                                                  
         BNE   NFND                CLIENT ERROR                                 
*                                                                               
         GOTO1 GETREC              GET HIGHER LEVEL RECORD                      
*                                                                               
         USING NDEFRECD,R3                                                      
         MVC   NDEFKEY,MYKEY       SET NEW KEY                                  
         XC    NDEFLINK,NDEFLINK   CLEAR LINKAGE (?)                            
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*  DELETE AN ESTIMATE SPECIFIC CBLPRO RECORD                         *          
*  NOTE  - NO PASSIVE POINTERS FOR CBLMKT/CBLPRO RECORDS             *          
**********************************************************************          
                                                                                
DELEST   LA    R2,MNTMVTOH         SET INCASE ERROR                             
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   INV                                                              
         CLI   MNTESTH+5,0         ESTIMATE SPECIFIC?                           
         BE    INV                                                              
*                                                                               
         USING NDEFRECD,R3                                                      
         OI    NDEFCNTL,X'80'      MARK FILE RECORD FOR DELETION                
         OI    KEY+13,X'80'        MARK DIR ACTIVE KEY FOR DELETION             
         GOTO1 WRITE                                                            
         GOTO1 PUTREC                                                           
         MVI   IOOPT,C'Y'          GENCON DOESN'T NEED TO DO ANYTHING           
         B     EXIT                                                             
         DROP  R3                                                               
                                                                                
**********************************************************************          
*  BUMP SCREEN FIELD POINTER                                         *          
**********************************************************************          
                                                                                
NEXT     NTR1  ,                                                                
         SR    R4,R4                                                            
         IC    R4,0(R2)                                                         
         AR    R2,R4               BUMP TO NEXT FIELD                           
*                                                                               
NEXTEX   XIT1  REGS=(R2)           DON'T RESTORE OLD VALUE OF R2                
         EJECT                                                                  
                                                                                
**********************************************************************          
*  MOVE A MARKET                                                     *          
**********************************************************************          
                                                                                
MOVE     NTR1  ,                                                                
         LA    R2,MNTMOVEH                                                      
         L     R3,AIO                                                           
*                                                                               
         MVI   ELCODE,NDEFELQ      X'01'                                        
         BAS   RE,GETEL            FIRST ELEMENT                                
         B     *+8                                                              
MOVE10   BAS   RE,NEXTEL                                                        
         BNE   INV                                                              
         CLC   NDEFMSUF-NDEFEL01(L'NDEFMSUF,R3),MNTMOVE                         
         BNE   MOVE10                                                           
         XC    ELEM,ELEM           ELEMENT FOUND                                
         MVC   ELEM(16),0(R3)              COPY IT                              
         GOTO1 RECUP,DMCB,(0,AIO),(R3)     REMOVE IT                            
*                                                                               
         LA    R2,MNTMVTOH         DETERMINE NEW POSITION                       
         L     R3,AIO                                                           
         MVI   ELCODE,NDEFELQ      X'01'                                        
         CLI   5(R2),0                                                          
         BE    MIS                                                              
         CLI   5(R2),2                                                          
         BE    MOVE30                                                           
         CLI   5(R2),4                                                          
         BNE   INV                                                              
         CLC   MNTMVTO,=C'FRST'                                                 
         BNE   *+12                                                             
MOVE15   LA    R3,NDEFEL-NDEFRECD(R3)  DISP TO 1ST ELEMENT                      
         B     MOVE50                                                           
*                                                                               
         CLC   8(4,R2),=C'LAST'                                                 
         BNE   INV                                                              
         BAS   RE,GETEL            FIND LAST ELEM                               
         BNE   MOVE15              (ONLY ELEM WAS ONE MOVING = FRST)            
         B     *+8                                                              
MOVE20   BAS   RE,NEXTEL                                                        
         BNE   MOVE25                                                           
         ST    R3,ALAST01                                                       
         B     MOVE20                                                           
MOVE25   L     R3,ALAST01          (R3=EOR) POINT TO LAST 01 FOUND              
         B     MOVE40                                                           
*                                                                               
MOVE30   BAS   RE,GETEL            FIND MATCHING ELEM                           
         B     *+8                                                              
MOVE35   BAS   RE,NEXTEL                                                        
         BNE   NFND                                                             
         CLC   NDEFMSUF-NDEFEL01(L'NDEFMSUF,R3),MNTMVTO                         
         BNE   MOVE35                                                           
*                                                                               
MOVE40   SR    RF,RF               BUMP PAST THIS ELEM                          
         IC    RF,1(R3)                                                         
         AR    R3,RF                                                            
MOVE50   GOTO1 RECUP,DMCB,(0,AIO),ELEM,(R3)   ADD AN ELEMENT                    
*                                                                               
MOVEEX   XIT1  ,                                                                
                                                                                
**********************************************************************          
*  MOVE ALL MERKETS TO EAST-WEST SEQUENCE                            *          
**********************************************************************          
                                                                                
MOVEEW   NTR1  ,                                                                
         LA    R2,MNTMOVEH                                                      
         L     R4,AIO3             USE IO3 TO STORE AND SORT ELEMS              
         MVI   0(R4),X'FF'                                                      
         MVI   ELCOUNT,0                                                        
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,NDEFELQ      X'01'                                        
         BAS   RE,GETEL            FIRST ELEMENT                                
         B     *+8                                                              
MVEW10   BAS   RE,NEXTEL                                                        
         BNE   MVEW40              DONE ALL ELEMS IN RECORD                     
MVEW15   SR    RF,RF                                                            
         CLC   MNTMVTO(3),=C'EWM'  E-W BY MARKET NUMBER                         
         BE    MVEW30                                                           
         LA    R1,EWSEQTAB                                                      
MVEW20   CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                UNKNOWN SUFFIX                               
         CLC   NDEFMSUF-NDEFEL01(L'NDEFMSUF,R3),0(R1)                           
         BE    MVEW30                                                           
         LA    R1,L'EWSEQTAB(R1)                                                
         AHI   RF,1                                                             
         B     MVEW20                                                           
*                                                                               
MVEW30   STC   RF,0(R4)            SET SORT SEQ NUMBER                          
         MVC   1(16,R4),0(R3)      COPY ELEMENT                                 
         LA    R4,17(R4)           NEXT SAVE SLOT                               
         MVI   0(R4),X'FF'         NEW TERMINATOR                               
         SR    RF,RF                                                            
         IC    RF,ELCOUNT          COUNT NUMBER OF ELEMENTS IN SORT             
         AHI   RF,1                                                             
         STC   RF,ELCOUNT                                                       
         GOTO1 RECUP,DMCB,(0,AIO),(R3)     REMOVE IT                            
         CLI   0(R3),NDEFELQ       (R3 NOW POINT TO NEXT ELEM)                  
         BE    MVEW15                                                           
         B     MVEW10                                                           
*                                                                               
MVEW40   L     R4,AIO3                                                          
         SR    RF,RF                                                            
         IC    RF,ELCOUNT          (COUNT NOT INCLUDE TERMINATOR X'FF')         
         C     RF,=AL4(1)                                                       
         BNH   MVEW50                                                           
* SORT DESCEND BUT AS ALWAYS ADD AT A(1ST ELEM), END UP IN REC ASCEND           
         CLC   MNTMVTO(3),=C'EWM'  E-W BY MARKET                                
         BNE   MVEW45                                                           
         GOTO1 XSORT,DMCB,(X'FF',(R4)),(RF),17,2,5   SORT THE ELEMENTS          
         B     MVEW50                                                           
MVEW45   GOTO1 XSORT,DMCB,(X'FF',(R4)),(RF),17,3,0   SORT THE ELEMENTS          
MVEW50   L     R3,AIO              REPOINT TO RECORD                            
         LA    R3,NDEFEL-NDEFRECD(R3)  DISP TO 1ST ELEMENT IN REC               
         L     R4,AIO3                                                          
MVEW55   CLI   0(R4),X'FF'                                                      
         BE    MVEWEX                                                           
         XC    ELEM,ELEM                                                        
         MVC   ELEM(16),1(R4)                                                   
         GOTO1 RECUP,DMCB,(0,AIO),ELEM,(R3)   ADD ELEMENT FROM SORT             
         LA    R4,17(R4)                                                        
         B     MVEW55                                                           
*                                                                               
MVEWEX   XIT1  ,                                                                
         EJECT                                                                  
                                                                                
**********************************************************************          
* CHANGE EXISTING RECORD                                             *          
* ONLY ALLOWABLE CHANGES ARE OFFSET AND COST%                        *          
**********************************************************************          
                                                                                
CHANGE   NTR1  ,                                                                
         LA    R2,MNTMKTH          START POSITION                               
         LA    R6,MNTLN24H         LIMIT                                        
         SR    R4,R4               INITIALIZE TOTAL PERCENTAGE COST             
*                                                                               
CH10     CR    R2,R6                                                            
         BNL   CHANGEX             DONE SCREEN                                  
         CLI   7(R2),0             CHECK FOR SUFFIX                             
         BE    CHANGEX             DONE ALL MARKETS                             
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,NDEFELQ                                                   
         BAS   RE,GETEL            FIND MATCHING ELEM                           
         B     *+8                                                              
CH20     BAS   RE,NEXTEL           HAVE MATCHING ELEMENT?                       
         BNE   ERRCBLD             NO - CBLDEF MUST HAVE CHANGED!               
         CLC   NDEFMSUF-NDEFEL01(L'NDEFMSUF,R3),8(R2)                           
         BNE   CH20                                                             
*                                                                               
         BAS   RE,NEXT             BUMP TO DATA FIELD (COST/OFFSET)             
         CLI   MNTPAGE,C'C'                                                     
         BE    CHCOST                                                           
*                                                                               
         BAS   RE,VALOFF           VALIDATE AND SET OFFSET                      
         ICM   R5,15,NDEFPCT-NDEFEL01(R3)   MUST STILL ACCUM COST               
         BNM   *+6                                                              
         SR    R5,R5                                                            
         B     *+8                                                              
CHCOST   BAS   RE,VALCOST          VALIDATE AND SET %AGE OF NWK BUY             
         AR    R4,R5                                                            
*                                                                               
         BAS   RE,NEXT             BUMP TO NEXT MKT                             
         B     CH10                                                             
*                                                                               
CHANGEX  C     R4,=F'100000'       MUST ACCOUNT FOR 100 PERCENT                 
         BE    *+12                                                             
         C     R4,=F'0'            ALLOW ZERO (NOT BOUGHT AT LEVEL)             
         BNE   CHANGER                                                          
         XIT1  ,                                                                
*                                                                               
CHANGER  EDIT  (R4),(L'MNTTOT,MNTTOT),3    PERCENTAGE OF NETWORK BUY            
         OI    MNTTOTH+6,X'80'                                                  
         LA    R2,MNTDATAH         POINT TO FIRST MARKET COST                   
         B     PCTERR                                                           
         EJECT                                                                  
                                                                                
**********************************************************************          
*  VALIDATE HOUR OFFSET FIELD                                        *          
*  ENTRY - R2=A(OFFSET FIELD)                                        *          
*          R3=A(CORRESPONDING 01 ELEM)                               *          
*  EXIT  - VALUE IN 01 ELEM RESET                                    *          
**********************************************************************          
                                                                                
VALOFF   NTR1  ,                                                                
         USING NDEFEL01,R3                                                      
         XC    NDEFOSET,NDEFOSET   CLEAR ANY CURRENT VALUE                      
         SR    R5,R5                                                            
         ICM   R5,1,5(R2)                                                       
         BZ    VOEX                INPUT NOT REQUIRED                           
*                                                                               
         GOTO1 CASHVAL,DMCB,8(R2),(R5)                                          
         CLI   DMCB,0                                                           
         BNE   INV                                                              
*                                                                               
         L     R5,DMCB+4                                                        
         C     R5,=F'21000'        3.5 HOURS MAX                                
         BH    INV                                                              
         C     R5,=F'-21000'                                                    
         BL    INV                                                              
         CVD   R5,DUB                                                           
         DP    DUB,=P'1500'                                                     
         CP    DUB+5(3),=P'0'      MUST BE 15 MIN MULTIPLES                     
         BNE   INV                                                              
         M     R4,=F'1'                                                         
         D     R4,=F'100'                                                       
         STH   R5,NDEFOSET         HOUR OFFSET (SIGNED) FIELD                   
*                                                                               
VOEX     XIT1  ,                                                                
         DROP  R3                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*  VALIDATE COST                                                     *          
*  ENTRY - R2=A(OFFSET FIELD)                                        *          
*          R3=A(CORRESPONDING 01 ELEM)                               *          
*  EXIT  - VALUE IN 01 ELEM RESET                                    *          
*          R5=PERCENTAGE                                                        
**********************************************************************          
                                                                                
VALCOST  NTR1  ,                                                                
         SR    R5,R5               CLEAR RETURN % INCASE NB                     
         USING NDEFEL01,R3                                                      
         CLC   8(2,R2),=C'NB'      NOT BOUGHT                                   
         BNE   *+14                                                             
VCOS10   MVC   NDEFPCT,=F'-1'                                                   
         B     VCEX                                                             
*                                                                               
         ICM   R5,1,5(R2)                                                       
         BZ    VCOS10              BLANK, DEFAULT TO NOT BOUGHT                 
         GOTO1 CASHVAL,DMCB,(3,8(R2)),(R5)                                      
         CLI   DMCB,0                                                           
         BNE   INV                                                              
*                                                                               
         L     R5,DMCB+4                                                        
         C     R5,=F'0'            CAN'T BE NEGATIVE                            
         BL    COSTERR                                                          
         C     R5,=F'100000'       CAN'T EXCEED 100 PERCENT                     
         BH    PCTERR                                                           
         MVC   NDEFPCT,DMCB+4                                                   
*                                                                               
VCEX     XIT1  REGS=(R5)           DON'T RESTORE OLD VALUE OF R5                
         DROP  R3                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*  LIST RECORDS                                                      *          
**********************************************************************          
                                                                                
LR       LA    R4,KEY                                                           
         USING NDEFRECD,R4                                                      
*                                                                               
         OI    GENSTAT2,DISTHSPG   RETURN TO SAME LIST PAGE                     
*                                                                               
         OC    KEY,KEY             IS IT FIRST TIME THROUGH                     
         BNZ   LR10                - NO                                         
         MVC   NDEFKTYP,=AL2(NDEFRECQ)                                          
         MVC   NDEFKAGY,AGENCY                                                  
*                                                                               
         CLI   LSTNTWKH+5,0        LIST ALL NETWORKS?                           
         BE    *+10                                                             
         MVC   NDEFKNET,SVNTWK     NETWORK                                      
*                                                                               
         CLI   LSTCLTH+5,0         LIST CLIENTS FOR SPECIFIC NETWORK?           
         BE    LR10                                                             
         MVC   NDEFKCLT,SVCLT      CLIENT                                       
*                                                                               
LR10     MVC   SAVEKEY,KEY                                                      
         GOTO1 HIGH                GET FIRST RECORD                             
*                                                                               
LR20     CLC   KEY(4),SAVEKEY                                                   
         BNE   LREND                                                            
         CLC   KEY+2(2),AGENCY     SINCE THIS WAS WRITTEN BY                    
         BNE   LREND               SOMEONE NOT TOO GOOD                         
         OC    NDEFKNET,NDEFKNET   DON'T SHOW THE DEFAULT CBLMKT RECORD         
         BZ    LR30                                                             
*                                                                               
         CLI   LSTCLTH+5,0         SHOW ALL CLIENTS                             
         BE    LR25                                                             
         TM    FLAG,CLSTART        START AT FILTER?                             
         BO    LR24                - YES                                        
         CLC   NDEFKCLT,SVCLT                                                   
         BNE   LR30                PURE CLIENT FILTER                           
         B     LR25                                                             
LR24     CLC   NDEFKCLT,SVCLT                                                   
         BL    LR30                                                             
*                                                                               
LR25     EQU   *                                                                
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         L     R3,AIO              DON'T SHOW IF NETDEF RECORD                  
         MVI   ELCODE,NDEFNELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   LR30                                                             
         CLI   NDEFNET-NDEFEL02(R3),NDEFCABQ                                    
         BNE   LR30                                                             
*                                                                               
*        GOTO1 GETREC                                                           
*                                                                               
         XC    LISTAR,LISTAR                                                    
         MVC   LISTNET,NDEFKNET    NETWORK                                      
         OC    NDEFKNET,NDEFKNET                                                
         BNZ   *+10                                                             
         MVC   LISTNET(4),=C'*DEF' DEFAULT RECORD IS SPECIAL                    
*                                                                               
         GOTO1 CLUNPK,DMCB,NDEFKCLT,QCLT  UNPACK CLIENT FOR DISPLAY             
         MVC   LISTCLT,QCLT        CLIENT UNPACKED FORMAT                       
*                                                                               
         EDIT  NDEFKEST,(3,LISTEST),ALIGN=LEFT  ESTIMATE NUMBER                 
*                                                                               
         GOTO1 LISTMON                                                          
*                                                                               
LR30     MVC   SAVEKEY,KEY                                                      
         GOTO1 SEQ                                                              
         B     LR20                                                             
*                                                                               
LREND    B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*  REQUEST REPORT                                                    *          
*  AFTER ADD/CHANGE SUBMIT A TURNAROUND REPORT REQUEST THEN RE-DISP  *          
**********************************************************************          
                                                                                
REQREC   CLI   MODE,PRINTREP                                                    
         BNE   *+16                                                             
         LA    R2,CONWHENH                                                      
         CLI   CONWHENH+5,0        NO PRINT OPTIONS                             
         BNE   INVPRT                                                           
*                                                                               
         CLC   =C'$DEL',MNTMVTO    DDS DELETED THE RECORD?                      
         BE    EXIT                                                             
*                                                                               
         L     R3,AIO2                                                          
         XC    0(110,R3),0(R3)                                                  
         MVI   10(R3),44           REQUEST HEADER - BINARY PROGRAM              
         MVI   14(R3),106                                                       
*                                  (OFFSETS, SEE QRECORD IN SPREPWORKD)         
         MVI   26(R3),X'40'        REQUEST CARD                                 
         MVC   27(79,R3),26(R3)    SPACE FILL                                   
         MVC   26(2,R3),=C'44'     PROGRAM (QCODE)                              
         MVC   28(2,R3),AGENCY     AGENCY  (QAGY)                               
         MVC   30(1,R3),QMED       MEDIA   (QMED)                               
         MVC   31(3,R3),=C'ALL'    CLIENT  (QCLT)                               
         OC    BCLT,BCLT                                                        
         BZ    *+10                                                             
         MVC   31(3,R3),QCLT                                                    
         MVC   40(4,R3),MYKEY+4    NETWORK IN MKT (QMKT)                        
         CLI   MYKEY+10,0          SEE IF ESTIMATE EXCEPTION                    
         BE    REQREC5                                                          
         EDIT  (B1,MYKEY+10),(3,49(R3)),0,FILL=0  ESTIMATE (QEST)               
*                                                                               
REQREC5  MVI   87(R3),C'N'                                                      
         MVC   94(7,R3),=C'CONTROL'                                             
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',AIO2,AIO2                     
         TM    8(R1),X'50'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   KEY,MYKEY                                                        
         GOTO1 HIGH                RESTORE ORIGINAL REC FOR DISPLAY             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
* HANDLE ANY OUTSTANDING PFKEYS                                                 
         CLI   MODE,XRECADD        AFTER ADDREC                                 
         BE    *+12                                                             
         CLI   MODE,XRECPUT        AFTER PUTREC                                 
         BNE   REQRECX                                                          
         BAS   RE,DOPFK                                                         
REQRECX  B     DR                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
*  SETUP                                                              *         
***********************************************************************         
                                                                                
SETUP    NTR1  ,                                                                
         OI    GENSTAT4,NODELLST   NO DELETE FROM THE LIST                      
         OI    GENSTAT1,APPLIC                                                  
* SPECIAL SELECT STUFF                                                          
         CLI   ACTNUM,ACTSEL       CLEAR PREVIOUS SELECT KEY IF NOT SEL         
         BE    *+10                                                             
         XC    SELKEY,SELKEY                                                    
* SPECIAL COPY STUFF                                                            
         CLI   ACTNUM,ACTCOPY      CLEAR COPY KEY IF NOT COPY ACTION            
         BE    SETUPXPF            NO PFKEYS ON COPY EITHER                     
         XC    COPYKEY,COPYKEY                                                  
* PFKEY STUFF                                                                   
         CLI   ACTNUM,ACTLIST      NO PFKEYS ON LIST                            
         BE    SETUPXPF                                                         
         CLI   ACTNUM,ACTSEL       ACTION SEL?                                  
         BNE   SETUP05                                                          
*                                FORCE REDISP & VALREC SO PF5 WILL WORK         
         OI    GENSTAT2,RETEQSEL   REDISPLAY SELECTION FROM LIST                
         OI    GENSTAT5,GENSELVR   ALWAYS GET VALREC - EVEN IF NO INPUT         
         CLI   MODE,DISPKEY                                                     
         BNE   SETUP05                                                          
         CLC   SELKEY,KEY                                                       
         MVC   SELKEY,KEY                                                       
         BNE   SETUP05                                                          
         CLI   PFKEY,0                                                          
         BNE   SETUP05             PFKEY HIT                                    
         OI    GENSTAT2,NEXTSEL    RELEASE CURRENT SELECTION/GET NEXT           
         NI    GENSTAT2,X'FF'-RETEQSEL                                          
         B     SETUPX                                                           
*                                                                               
SETUP05  OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
*                                                                               
* SET LINE24 PFKEY HELP TEXT & COLUMN HEADINGS                                  
         MVC   MNTLN24,SPACES      CLEAR PFKEY TEXT                             
         OI    MNTLN24H+FHOID,FHOITR                                            
         OI    MNTHDR1H+FHOID,FHOITR                                            
         OI    MNTHDR2H+FHOID,FHOITR                                            
         OI    MNTHDR3H+FHOID,FHOITR                                            
         OI    MNTHDR4H+FHOID,FHOITR                                            
         MVC   MNTLN24(L'PF02CBLM),PF02CBLM                                     
         MVC   MNTLN24+12(4),=CL4'PF5='                                         
         CLI   MNTPAGE,C'C'                                                     
         BNE   SETUP10                                                          
         MVC   MNTLN24+16(L'OSETTXT),OSETTXT                                    
         MVC   MNTHDR1,COSTTXT                                                  
         MVC   MNTHDR2,COSTTXT                                                  
         MVC   MNTHDR3,COSTTXT                                                  
         MVC   MNTHDR4,COSTTXT                                                  
         B     SETUP11                                                          
SETUP10  MVC   MNTLN24+16(L'COSTTXT),COSTTXT                                    
         MVC   MNTHDR1,OSETTXT                                                  
         MVC   MNTHDR2,OSETTXT                                                  
         MVC   MNTHDR3,OSETTXT                                                  
         MVC   MNTHDR4,OSETTXT                                                  
*                                                                               
* HANDLE PFKEY SPECIFICS                                                        
SETUP11  MVC   PF02ACT,SPACES      SWAP TO SAME ACTION                          
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    *+12                                                             
         CLI   ACTNUM,ACTSEL       ACTION SEL?                                  
         BNE   SETUP12                                                          
         MVC   PF02ACT(4),=CL4'DISP'           SWAP TO DISP ACTION              
*                                                                               
SETUP12  CLI   CALLSP,0            ANYTHING TO RETURN TO ?                      
         BE    *+10                                                             
         MVC   MNTLN24+65(L'PF12RET),PF12RET                                    
*                                                                               
* HANDLE ANY PFKEYS                                                             
         OC    PFKEY,PFKEY                                                      
         BZ    SETUPX                                                           
* PREVALIDATE - DON'T HAVE INVALID PFKEYS, JUST FAKE THEM AS <ENTER>            
         LA    RF,PFTABLE                                                       
         USING PFTABD,RF                                                        
SETUP50  CLI   0(RF),X'FF'                                                      
         BE    SETUPXPF            PFKEY INVALID, FAKE AS <ENTER>               
         CLC   PFKEY,PFTAID        MATCH ON NUMBER                              
         BE    SETUP60                                                          
         SR    RE,RE                                                            
         IC    RE,PFTLEN           BUMP TO NEXT ENTRY IN TABLE                  
         AR    RF,RE                                                            
         B     SETUP50                                                          
SETUP60  TM    PFTSTAT,PFTRPROG                                                 
         BZ    SETUP70                                                          
         CLI   CALLSP,0            ANYTHING TO RETURN TO ?                      
         BE    SETUPXPF            - NOPE, STOP THAT ERROR TOO                  
* PASSTHRU ALL PFKEYS ON UPDATIVE ACTIONS, HANDLE POST ADD/CHANGE DONE          
SETUP70  CLI   ACTNUM,ACTADD                                                    
         BE    SETUPX                                                           
         CLI   ACTNUM,ACTCHA                                                    
         BE    SETUPX                                                           
         CLI   ACTNUM,ACTSEL                                                    
         BNE   SETUP80                                                          
         CLI   THISLSEL,C'C'                                                    
         BE    SETUPX                                                           
         CLI   THISLSEL,X'10'      (GENCON 'CHASELQ')                           
         BE    SETUPX                                                           
SETUP80  BAS   RE,DOPFK                                                         
         B     SETUPX                                                           
*                                                                               
SETUPXPF MVI   PFKEY,0             CLEAR PFKEY EXIT                             
SETUPX   XIT1  ,                                                                
         EJECT                                                                  
* ACTION ANY PFKEYS                                                             
DOPFK    NTR1  ,                                                                
         OC    PFKEY,PFKEY                                                      
         BZ    DOPFKX                                                           
         GOTO1 INITPFKY,DMCB,PFTABLE      MAINT PF TABLE                        
DOPFKX   B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*  ERROR EXITS                                                       *          
**********************************************************************          
                                                                                
REQOV    MVC   ERRNUM,=AL2(REPGEN)                                              
         B     SPINFEX                                                          
COSTERR  MVC   ERRNUM,=AL2(265)    INVALID COST                                 
         B     SPERREX                                                          
PCTERR   MVC   ERRNUM,=AL2(NOT100Q)                                             
         B     SPERREX                                                          
ERRSPL   MVC   ERRNUM,=AL2(842)    'HAS SPILL DEFINED'                          
         B     SPERREX                                                          
ERRCBLD  MVC   ERRNUM,=AL2(1353)   DISP REC TO REFRESH CBLDEF CHANGED           
         B     SPERREX                                                          
*                                                                               
VCERR    LA    R2,MNTCLTH          INVALID CLIENT                               
INV      MVI   ERROR,INVALID                                                    
         B     ERRX                                                             
MIS      MVI   ERROR,MISSING                                                    
         B     ERRX                                                             
NFND     MVI   ERROR,NOTFOUND                                                   
         B     ERRX                                                             
INVPRT   MVI   ERROR,INVPRINT                                                   
         B     ERRX                                                             
*                                                                               
         USING GETTXTD,RF                                                       
SPINFEX  LA    RF,GETTXTCB                                                      
         MVI   GTMTYP,GTMINF                                                    
         B     *+12                                                             
SPERREX  LA    RF,GETTXTCB                                                      
         MVI   GTMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMSYS,2                                                         
         DROP  RF                                                               
ERRX     GOTO1 ERREX                                                            
                                                                                
* ERROR EQUATES                                                                 
                                                                                
REPGEN   EQU   411                                                              
NOT100Q  EQU   238                                                              
         EJECT                                                                  
                                                                                
**********************************************************************          
*  GETEL ROUTINE                                                     *          
**********************************************************************          
                                                                                
         GETEL R3,DATADISP,ELCODE                                               
                                                                                
**********************************************************************          
*  LOCAL TABLES AND CONSTANTS                                        *          
**********************************************************************          
                                                                                
* TABLE OF 2CHR MARKET SUFFIXES IN EAST-WEST ORDER                              
* (E-W BY PROVINCE, ALPHA WITHIN EACH PROVINCE)                                 
                                                                                
EWSEQTAB DS    0CL2           (IF EVER NEED IN GEOG ORDER - LATITUDE)           
* NEWFOUNDLAND                                                                  
         DC    C'CR'          CORNER BROOK                57° 57' W             
         DC    C'SJ'          ST. JOHN'S                  52° 45' W             
* NOVA SCOTIA                                                                   
         DC    C'HA'          HALIFAX                     63° 34' W             
         DC    C'SY'          SYDNEY                      60° 3'  W             
* PEI                                                                           
         DC    C'CH'          CHARLOTTETOWN               63° 8'  W             
* NEW BRUNSWICK                                                                 
         DC    C'SM'          SAINT JOHN/MONCTON ENG.  65°53'/64°41' W          
         DC    C'SF'          SAINT JOHN/MONCTON FR.                            
* QUEBEC                                                                        
         DC    C'CL'          CARLETON                                          
         DC    C'CJ'          CHICOUTIMI                  71° 5' W              
         DC    C'MO'          MONTREAL ENG.               73° 45' W             
         DC    C'MF'          MONTREAL FR.                                      
         DC    C'QU'          QUEBEC CITY                 71° 23' W             
         DC    C'RI'          RIMOUSKI/MATANE/SEPT-ILES 68°32/?/66°16'W         
         DC    C'RO'          RIVIERE-DU-LOUP                                   
         DC    C'RN'          ROUYN/NORANDA                                     
         DC    C'SH'          SHERBROOKE                  71° 54' W             
         DC    C'TR'          TROIS RIVIERES              72° 35' W             
* ONTARIO                                                                       
         DC    C'BA'          BARRIE                                            
         DC    C'HM'          HAMILTON                    79° 54' W             
         DC    C'KN'          KENORA                      94° 22' W             
         DC    C'KI'          KINGSTON                    76° 30' W             
         DC    C'KT'          KITCHENER                   80° 30' W             
         DC    C'LO'          LONDON                      81° 9' W              
         DC    C'KL'          KITCHENER/LONDON                                  
         DC    C'NB'          NORTH BAY                   79° 25' W             
         DC    C'OT'          OTTAWA/HULL ENG.        75° 40'/75° 44' W         
         DC    C'OF'          OTTAWA/HULL FR.                                   
         DC    C'PM'          PEMBROKE                                          
         DC    C'PE'          PETERBOROUGH                78° 19' W             
         DC    C'TB'          THUNDER BAY                 89° 19' W             
         DC    C'TM'          TIMMINS                     81° 22' W             
         DC    C'TO'          TORONTO                     79° 38' W             
         DC    C'ST'          SAULT STE. MARIE            84° 30' W             
         DC    C'SU'          SUDBURY                     80° 48' W             
         DC    C'SN'          SUDBURY/TIMMINS/NTH BAY                           
         DC    C'SS'          SUDBURY/TIMMINS/NTH BAY/SSM                       
         DC    C'WI'          WINDSOR                     82° 58' W             
* MANITOBA                                                                      
         DC    C'BR'          BRANDON                     99° 59' W             
         DC    C'WP'          WINNIPEG                    97° 14' W             
* SASKATCHEWAN                                                                  
         DC    C'PA'          PRINCE ALBERT               105° 41' W            
         DC    C'RE'          REGINA/MOOSE JAW     104° 40'/105° 33' W          
         DC    C'SA'          SASKATOON                   106° 41' W            
         DC    C'SW'          SWIFT CURRENT               107° 41' W            
         DC    C'YO'          YORKTON                     102° 28' W            
* ALBERTA                                                                       
         DC    C'CA'          CALGARY                     114° 1' W             
         DC    C'CE'          CALGARY/LETHBRIDGE          114° 1' W             
         DC    C'ED'          EDMONTON                    113° 31' W            
         DC    C'LE'          LETHBRIDGE                  112° 48' W            
         DC    C'LL'          LLOYDMINSTER                                      
         DC    C'ME'          MEDICINE HAT                110° 43' W            
         DC    C'RD'          RED DEER                    113° 54' W            
* BRITISH COLOMBIA                                                              
         DC    C'DA'          DAWSON CREEK                120° 11' W            
         DC    C'KA'          KAMLOOPS                    120° 25'W             
         DC    C'KE'      KELOWNA (OKANAGAN/KAMLOOPS)(119°20'/120°25'W)         
         DC    C'KO'          KOOTENAY                                          
         DC    C'KP'          PRINCE GEORGE/KAMLOOPS                            
         DC    C'KW'          KELOWNA                                           
         DC    C'OK'          OKANAGAN                    119° 20' W            
         DC    C'PG'          PRINCE GEORGE               122° 41' W            
         DC    C'TK'          TERRACE/KITIMAT                                   
         DC    C'PK'          PR. GEORGE/TERR./KITIMAT                          
         DC    C'VA'          VANCOUVER                   123° 10' W            
         DC    C'VI'          VICTORIA                    123° 19' W            
* NUNAVUT                                                                       
* NORTH WEST TERRITORIES                                                        
         DC    C'YE'          YELLOW KNIFE                114° 27' W            
* YUKON TERRITORY                                                               
         DC    C'NE'          NATIONAL ENGLISH                                  
         DC    C'NF'          NATIONAL FRENCH                                   
         DC    C'WH'          WHITEHORSE                  135° 4' W             
*                                                                               
         DC    C'ZZ'          ZZZZ                                              
         DC    X'FF'          EOT                                               
                                                                                
* PFKEY TABLE                                                                   
*                                                                               
PFTABLE  DS    0H                                                               
*        CBLMKT MAINT DISPLAY                                                   
         DC   AL1(PF02X-*,02,PFTCPROG,(PF02X-PF02)/KEYLNQ,0)                    
         DC   CL3' '                   NOT SELECTABLE                           
         DC   CL8'CBLM'                RECORD: CBLMKT                           
PF02ACT  DC   CL8' '                   ACTION: AS CURRENT                       
PF02     DC   AL1(KEYTYTWA,L'MNTNTWK-1),AL2(MNTNTWK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'MNTCLT-1),AL2(MNTCLT-T217FFD)                      
PF02X    EQU  *                                                                 
*                                                                               
*        PAGE                                                                   
PF05     DC   AL1(PF05X-*,05,0,0,PFTRETRN)                                      
         DC   3C' ',8C' ',8C' '                                                 
PF05X    EQU  *                                                                 
*                                                                               
*        RETURN CALLER                                                          
         DC    AL1(PF12X-*,12,PFTRPROG,0,0)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
                                                                                
* PFKEY TEXT MIXCASE CONSTANTS                                                  
PF12RET  DC    X'D7C6F1F27ED985A3A49995'  PF12=RETURN                           
PF02CBLM DC    X'D7C6F27EC38293D492A3'    PF2=CBLMKT                            
COSTTXT  DC    X'C396A2A36C40'            COST%                                 
OSETTXT  DC    X'D68686A285A3'            OFFSET                                
         EJECT                                                                  
**********************************************************************          
*  LITERAL POOL AND LOCAL EQUATES                                    *          
**********************************************************************          
                                                                                
ELEMLEN  EQU   16                  ELEMENT LENGTH                               
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* COPY FUNCTION - COPIES CBLPRO/CBLMKT RECORD WHOSE KEY IS IN COPYKEY*          
* USER MUST HAVE PREVIOUSLY DISPLAYED THE RECORD THEY WANT TO COPY   *          
**********************************************************************          
*  ADD A CBLPRO RECORD (AND HENCE A CBLMKT RECORD) BASED ON MOST     *          
*  APPROPRIATE CBLMKT RECORD                                         *          
*  HEIRARCHICAL ORDER MUST BE ADHERED TO, EACH NEW RECORD IS         *          
*  EFFECTIVELY A 'COPY' OF HIGHER LEVEL RECORD                       *          
*  ESTIMATE LEVEL MUST ALWAYS HAVE SAME MARKET/SUFFIX DEFINITIONS AS *          
*  THE CORRESPONDING CLIENT LEVEL                                    *          
COPYREC  NTR1  BASE=*,LABEL=*                                                   
         MVC   MYKEY,KEY                                                        
         MVC   AIO,AIO1                                                         
         L     R3,AIO                                                           
*                                                                               
         MVI   RDUPDATE,C'N'       CHECK FOR COPY TO RECORD                     
         OI    DMINBTS,X'08'       MAY HAVE BE DELETED                          
         GOTO1 HIGH                                                             
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(L'COPYKEY),KEYSAVE                                           
         BNE   COPYR10                                                          
         LA    R2,MNTCLTH                                                       
         MVI   ERROR,49            'RECORD EXISTS'                              
         TM    DMCB+8,X'02'                                                     
         JZ    ERRX                                                             
         MVC   ERRNUM,=AL2(941)    'DELETED RECORD EXISTS'                      
         J     SPERREX                                                          
*                                                                               
COPYR10  XC    KEY,KEY                                                          
         MVC   KEY(L'COPYKEY),COPYKEY                                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                FIND COPY FROM RECORD                        
         CLC   KEY(L'COPYKEY),KEYSAVE                                           
         BE    *+12                                                             
         LA    R2,MNTNTWKH                                                      
         J     NFND                ERROR (WE JUST DISPLAYED IT!)                
*                                                                               
         GOTO1 GETREC              GET COPY FROM RECORD                         
*                                                                               
         MVC   KEY,MYKEY           SET NEW KEY                                  
         USING NDEFRECD,R3                                                      
         MVC   NDEFKEY,MYKEY       SET NEW KEY                                  
         XC    NDEFLINK,NDEFLINK   CLEAR LINKAGE (?)                            
         GOTO1 ADDREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1  ,                                                                
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*  INCLUDED DSECTS                                                              
**********************************************************************          
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD               SPOOL DSECT                             
         EJECT                                                                  
       ++INCLUDE SPGENNDEF              NETWORK DEFINITION DSECT                
       ++INCLUDE SPGENSDEF                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FACTRY                                                         
       ++INCLUDE DDFH                                                           
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FATIOB                                                         
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SCSFM54D                                                       
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SCSFM55D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
                                                                                
**********************************************************************          
*  LOCAL WORKING STORAGE                                             *          
**********************************************************************          
                                                                                
         ORG   SYSSPARE                                                         
ERRNUM   DS    XL2                                                              
TEMPFLD  DS    CL9                 WORK AREA                                    
MYKEY    DS    CL48                                                             
SAVEKEY  DS    XL48                                                             
SVNTWK   DS    CL4                                                              
SVCLT    DS    CL2                                                              
TEMPCLT  DS    CL4                                                              
FLAG     DS    X                                                                
ELCOUNT  DS    X                                                                
CLSTART  EQU   X'80'               START AT CLIENT FILTER                       
ALAST01  DS    A                   A(LAST 01 ELEM FOUND)                        
COPYKEY  DS    XL(L'NDEFKEY)                                                    
SELKEY   DS    XL(L'NDEFKEY)                                                    
                                                                                
**********************************************************************          
*  REDEFINITION OF LIST LINE AT LISTAR                               *          
**********************************************************************          
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LISTNET  DS    CL4                                                              
         DS    CL7                                                              
LISTCLT  DS    CL3                                                              
         DS    CL7                                                              
LISTEST  DS    CL3                                                              
         DS    CL2                                                              
**********************************************************************          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015SPSFM60   05/27/10'                                      
         END                                                                    
