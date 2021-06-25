*          DATA SET ACBIL00    AT LEVEL 063 AS OF 07/31/15                      
*PHASE T60E00C                                                                  
*INCLUDE VATICAN                                                                
*INCLUDE PRORATA                                                                
*INCLUDE EXTEDIT                                                                
*INCLUDE BMONVAL                                                                
*INCLUDE CADET                                                                  
         TITLE 'T60E00 - CREATIVE BILLING - ROOT'                               
ACBIL00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 DWSX-DWSD,**BIL0**,RA,R7,R4,RR=R3,CLEAR=YES                      
         USING LWSD,RC             RC=A(BASE LOCAL W/S)                         
*                                                                               
         L     R9,=AL4(L'DWSL)                                                  
         AR    R9,RC                                                            
*                                                                               
         USING GWS,R9              R9=A(GLOBAL W/S)                             
         LR    R2,R9                                                            
         A     R2,=AL4(L'DWSG)                                                  
         ST    R2,ATWA1            A(TWA1)                                      
*                                                                               
         L     RE,20(R1)                                                        
         MVC   FACFLAG,7(RE)       SAVE CONNECT FLAG                            
         MVC   FACUPD,10(RE)       UPDATIVE FACPAK ID                           
         MVC   FACPID,20(RE)       AND PERSON ID                                
*                                                                               
         USING TWAD,R8             R8=A(TWA)                                    
         L     R8,4(,R1)                                                        
         ST    R3,RELO                                                          
*                                  INITIALIZE STANDARD W/S VALUES               
         ST    RB,ABASE1                                                        
         ST    RA,ABASE2                                                        
         ST    R7,ABASE3                                                        
         ST    R4,ABASE4                                                        
         ST    RD,AWORK                                                         
         ST    R8,ATWA                                                          
         ST    RC,ABASEC                                                        
         MVC   ATIA,12(R1)                                                      
         MVC   ACOMFACS,16(R1)                                                  
         MVC   COMPANY,0(R1)                                                    
         L     RF,20(,R1)                                                       
         ST    RF,AXTRA                                                         
         MVC   AGYOPTS,0(RF)       AGENCY OPTIONS                               
         MVC   AGYCTRY,1(RF)       AGENCY COUNTRY                               
         MVC   AGYLANG,3(RF)       AGENCY LANGUAGE                              
         MVC   AGYCURR,4(RF)       AGENCY CURRENCY                              
         CLI   AGYCTRY,0                                                        
         BNE   *+8                                                              
         MVI   AGYCTRY,CTRYUSA                                                  
         CLI   AGYLANG,0                                                        
         BNE   *+8                                                              
         MVI   AGYLANG,LANGEUS                                                  
         MVI   RUNOPT,X'00'                                                     
         CLI   AGYCTRY,CTRYCAN                                                  
         BNE   *+8                                                              
         OI    RUNOPT,NEEDGST+NEEDPST                                           
*                                                                               
*                                  INITIALIZE DATA MANAGER CONSTANTS            
         MVC   DMVARS(DMCONSQ),DMCONS                                           
*                                                                               
         USING COMFACSD,RE                                                      
         L     RE,ACOMFACS                                                      
         MVC   VCALLOV,CCALLOV     BUILD EXTERNAL DIRECTORY                     
         MVC   VDATAMGR,CDATAMGR                                                
         MVC   VDATCON,CDATCON                                                  
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VDICTAT,CDICTATE                                                 
         MVC   VGETFACT,CGETFACT                                                
         MVC   VGETMSG,CGETMSG                                                  
         MVC   VGETPROF,CGETPROF                                                
         MVC   VGETTXT,CGETTXT                                                  
         MVC   VHELLO,CHELLO                                                    
         MVC   VSCANNER,CSCANNER                                                
         MVC   VUNSCAN,CUNSCAN                                                  
         MVC   VXSORT,CXSORT                                                    
         MVC   VSECRET,CSECRET                                                  
*                                                                               
         USING ACCFACSD,RE                                                      
         L     RE,8(,R1)                                                        
         MVC   VADDAY,AADDAY                                                    
         MVC   VCASHVAL,ACASHVAL                                                
         MVC   VGETDAY,AGETDAY                                                  
         DROP  RE                                                               
*                                                                               
         LA    RE,IOAREAS          SET  A(IOAREAS 1-2)                          
         ST    RE,AIOAREA1                                                      
*                                                                               
         LA    RE,RECLNQ(,RE)                                                   
         ST    RE,AIOAREA2                                                      
*                                                                               
         LA    RE,RECLNQ(,RE)                                                   
         ST    RE,AIOAREA3                                                      
*                                                                               
         LA    RE,RECLNQ(,RE)                                                   
         ST    RE,ADCMP            SET  UP A(COMPANY RECORD)                    
*                                                                               
         LH    RE,=Y(OFFBLK-GWS)                                                
         LA    RE,GWS(RE)          SET  A(OFFAL BLOCK)                          
         ST    RE,AOFFBLK                                                       
*                                                                               
         LH    RE,=Y(PROBLK-GWS)                                                
         LA    RE,GWS(RE)          SET  A(PROBLK)                               
         ST    RE,APROBLK                                                       
*                                                                               
         LH    RE,=Y(PROLST-GWS)                                                
         LA    RE,GWS(RE)          SET  A(PROLST)                               
         ST    RE,APROLST                                                       
*                                                                               
         LH    RE,=Y(GOPBLK-GWS)                                                
         LA    RE,GWS(RE)          SET  A(GOBLOCK)                              
         ST    RE,AGOBLOCK                                                      
*                                                                               
         LH    RE,=Y(VATBUFF-GWS)                                               
         LA    RE,GWS(RE)          SET  A(VATBUFF)                              
         ST    RE,AVATBUFF                                                      
*                                                                               
         LH    RE,=Y(CADETIO-GWS)                                               
         LA    RE,GWS(RE)          SET  A(CADETIO)                              
         ST    RE,ACADETIO                                                      
*                                                                               
         LH    RE,=Y(SECBLK-GWS)                                                
         LA    RE,GWS(RE)          SET  A(SECBLK)                               
         ST    RE,ASECBLK                                                       
*                                                                               
         LA    RE,ACCORFST                                                      
         STH   RE,DATADISP         SET  DISP TO FIRST ELEMENT                   
*                                                                               
         GOTO1 VGETFACT,DMCB,0,0   CALL GETFACT                                 
*                                                                               
         USING FACTSD,R2           MAP  GETFACT BLOCK                           
         L     R2,0(,R1)           ->   GETFACT BLOCK                           
         MVC   CULANG,FALANG       GET  CURRENT LANGUAGE                        
         MVC   ASSYSO,FAOVSYS      GET  SYSTEM  NUMBER                          
         DROP  R2                                                               
*                                                                               
         L     R3,=A(CURSIGNT)     ->   CURR    SIGN     TABLE                  
         A     R3,RELO                                                          
         ZIC   R2,CULANG           GET  CURRENT LANGUAGE                        
         MH    R2,=AL2(L'CURSIGNT) GET  TABLE   ENTRY                           
         AR    R3,R2                                                            
         MVC   CUSIGN,0(R3)        GET  CURRENT SIGN                            
         MVC   PCSIGN,0(R3)        GET  PRINT   SIGN                            
*                                                                               
         LA    RF,L'SECBLK                                                      
         L     R2,ASECBLK                                                       
         USING SECD,R2                                                          
         GOTO1 VSECRET,DMCB,('SECPINIT',SECD),(RF)                              
         ORG   *-2                                                              
         TM    SECINDS,SECIINIT          TEST SECRET BLOCK INITIALISED          
         BNZ   *+12                                                             
         BASR  RE,RF                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R2                                                               
*                                                                               
         EJECT ,                                                                
***********************************************************************         
*  BUILD INTERNAL/EXTERNAL DIRECTORY                                  *         
***********************************************************************         
         SPACE  1                                                               
         LA    R2,ADTABLES                                                      
         L     R3,=A(TABLES)                                                    
         A     R3,RELO                                                          
         LA    RF,NTABLS                                                        
*                                                                               
SYS3     L     R1,0(,R3)           RELOCATE TABLE ENTRIES                       
         A     R1,RELO                                                          
         ST    R1,0(,R2)                                                        
         LA    R2,4(,R2)                                                        
         LA    R3,4(,R3)                                                        
         BCT   RF,SYS3                                                          
*                                                                               
         LA    R2,ACOMMON          SET UP COMMON ENTRIES                        
         SR    R3,R3                                                            
         LA    RF,AROUTINE                                                      
         LA    R5,ACOUNT                                                        
*                                                                               
SYS4     ST    R2,0(,RF)                                                        
         STC   R3,0(,RF)                                                        
         LA    R3,4(,R3)                                                        
         LA    RF,4(,RF)                                                        
         BCT   R5,SYS4                                                          
*                                                                               
*              FIND ADDRESSES OF CORE RESIDENT MODULES                          
*                                                                               
         L     R3,=A(CORELIST)                                                  
         A     R3,RELO                                                          
         LA    R2,CORERES                                                       
*                                                                               
SYS5     XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A00'                                           
         MVC   DMCB+7(1),0(R3)                                                  
         GOTO1 VCALLOV,DMCB                                                     
         MVC   0(4,R2),DMCB                                                     
         LA    R2,4(,R2)                                                        
         LA    R3,1(,R3)                                                        
         CLI   0(R3),X'FF'                                                      
         BNE   SYS5                                                             
*                                  SET OTHER FIELDS                             
         LA    R0,ZEROS#           NUMBER OF ZEROS                              
         LA    R2,ZEROS                                                         
*                                                                               
*                                  INITIALIZE ZEROS                             
SYS6     ZAP   0(PLAMTLNQ,R2),=P'0'                                             
         LA    R2,PLAMTLNQ(,R2)                                                 
         BCT   R0,SYS6                                                          
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         MVC   MSG,SPACES                                                       
         MVC   XTRAMESS,SPACES                                                  
         MVC   FVFOK,=XL2'FFFF'                                                 
         MVC   FVMSGNO,FVFOK                                                    
         MVI   FVMTYPE,C' '                                                     
*                                  READ SAVED ENTRIES FROM TEMPSTR              
         MVC   DMCB+10(2),TWATRM   TERMINAL NUMBER                              
         MVI   DMCB+8,1            PAGE 1                                       
         MVI   DMCB+9,0                                                         
         MVC   DMCB+20(2),=C'L='                                                
         L     RF,=AL4(L'DWST)     LENGTH OF USED TWA1                          
         STH   RF,DMCB+22                                                       
*                                  GET  6K RECORD                               
         GOTO1 VDATAMGR,DMCB,DMREAD,TEMPSTR,,ATWA1                              
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                CAN'T READ TEMPSTR                           
*                                                                               
*        USING ISDTF,RE                                                         
*        GOTO1 VDATAMGR,DMCB,=C'DTFADD ',ACCOUNT                                
*        L     RE,12(,R1)          RE=A(DTF)                                    
*        TM    ISFTYPE,ISFTEMU     IS THIS AN EMULATED FILE ?                   
*        BZ    *+8                 NO                                           
         MVI   EMULATE,C'Y'        YES, SET INDICATOR                           
         B     INIT02                                                           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  SYSTEM ROUTINES ENTERABLE FROM BASE OR OVERLAY                     *         
***********************************************************************         
         SPACE 1                                                                
ACOMMON  NTR1  BASE=ABASE1                                                      
         L     RA,ABASE2                                                        
         L     R7,ABASE3                                                        
         L     R4,ABASE4                                                        
         L     RC,ABASEC                                                        
         SRL   RF,24                                                            
         B     ABRANCH(RF)                                                      
         SPACE 1                                                                
*                                                                               
ABRANCH  B     FVAL                                                             
         B     GETNAME                                                          
         B     ACADD                                                            
         B     ACRDHI                                                           
         B     ACRDHIL                                                          
         B     ACREAD                                                           
         B     ACREADL                                                          
         B     ACSEQ                                                            
         B     ACSEQL                                                           
         B     ACWRITE                                                          
         B     WRKBUF                                                           
         B     WRKBUR                                                           
         B     WRKBUS                                                           
         B     WRKIND                                                           
         B     WRKOPE                                                           
         B     WRKADD                                                           
         B     WRKCLO                                                           
         B     WRKDEL                                                           
         B     WRKSEQ                                                           
         B     WRKLADD                                                          
         B     WRKLGET                                                          
         B     WRKLGTL                                                          
         B     WRKLPUT                                                          
         B     WRKLDEL                                                          
         B     WRKLCLO                                                          
         B     WRKRET                                                           
         B     WRKRAN                                                           
         B     FINDBIL                                                          
         B     GETWC                                                            
         B     NARRDIS                                                          
         B     REAMED                                                           
         B     REAMEDL                                                          
         B     WRKID                                                            
         B     PARNOFM                                                          
         B     EXTBIL                                                           
         B     REALED                                                           
         B     REALEDL                                                          
         B     WKDCODE                                                          
         B     BLDCHR                                                           
         B     PARCHR                                                           
         B     BILCHR                                                           
         B     DISTOT                                                           
         B     ERRORX                                                           
         B     ERRORX2                                                          
         B     GETMAN                                                           
         B     ADDELM                                                           
         B     TRXELM                                                           
         B     LDADTRN             LOAD  ADDTRN AND ACBIL40                     
         B     BADDLAST            BATCH RECORD ADD ELEMENT LAST                
         B     BBLDKHD             BUILD KEY FOR BATCH HEADER RECORD            
         B     BADDHDR             BUILD ADD HEADER RECORD                      
         B     BADDRFT             BATCH ADD DRAFT TRANS ELEMENT                
         B     BADDREC             CALL  ADDREC/PUTREC FOR BATCH RCDS           
         B     BDELDPT             BATCH DELETE DIRECTORY POINTERS              
         B     BITEMADD            BATCH ITEM RECORD ADDREC/PUTREC              
         B     VALMLK              CHECK MOS FOR SECURITY LOCKOUTS              
*                                                                               
ACOUNT   EQU   (*-ABRANCH)/4                                                    
         EJECT ,                                                                
***********************************************************************         
*  FIRST TIME INITIALIZATION CODE                                     *         
***********************************************************************         
         SPACE 1                                                                
*                                  TRANSLATE LOCAL DICTIONARY ITEMS             
INIT02   DS    0H                                                               
*                                                                               
         L     R2,=A(DICTB00)      ->   DICTATE   LIST                          
         A     R2,RELO                                                          
         GOTO1 VDICTAT,DMCB,C'LL  ',(R2),DICLS00                                
*                                                                               
         XC    BILCLIN,BILCLIN     CLEAR CLIENT/PROD/JOB                        
         OI    BILCLINH+6,FVOXMT   TRANSMIT                                     
         XC    BILPRON,BILPRON                                                  
         OI    BILPRONH+6,FVOXMT   TRANSMIT                                     
         XC    BILJOBN,BILJOBN                                                  
         OI    BILJOBNH+6,FVOXMT   TRANSMIT                                     
         LA    R1,BILACTH          SET FADR TO FIRST INPUT FIELD                
         ST    R1,FADR                                                          
*                                                                               
         XC    CURCOD,CURCOD       CLEAR CURRENCY CODE                          
         MVI   PLANG,X'00'         CLEAR INTERNAL PRINT LANGUAGE                
*                                                                               
         L     R3,=A(CURSIGNT)     ->    CURRENCY  SIGN TABLE                   
         A     R3,RELO                                                          
         ZIC   R2,CULANG           GET   DEFAULT   LANGUAGE                     
         MH    R2,=AL2(L'CURSIGNT) GET   TABLE     ENTRY                        
         AR    R3,R2                                                            
         MVC   PCSIGN,0(R3)        GET   PRINT     SIGN                         
*                                                                               
         MVI   WCCOMMA,COMMA       WORK  CODE LIST USES COMMA                   
         CLI   CULANG,2            DEFAULT    LANGUAGE  ENGLISH ?               
         BNH   *+8                 YES,  SKIP                                   
         MVI   WCCOMMA,SEMICOLN    WORK  CODE LIST USES SEMI-COLON              
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         GOTO1 AREAD,ADCMP         GET COMPANY RECORD                           
         BNE   ERROR                                                            
         L     RE,ADCMP                                                         
         SR    RF,RF                                                            
         ICM   RF,3,ACCORLEN(RE)                                                
         LA    RF,4(RF,RE)                                                      
         ST    RF,ADCLI            SET A(CLIENT RECORD)                         
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
*                                                                               
INIT04   CLI   0(RE),0             LOCATE COMPANY ELEMENT                       
         BE    INIT08                                                           
         CLI   0(RE),CPYELQ                                                     
         BE    INIT06                                                           
         IC    RF,1(,RE)                                                        
         AR    RE,RF                                                            
         B     INIT04                                                           
*                                                                               
         USING CPYELD,RE                                                        
INIT06   MVC   PRODUL,CPYPROD                                                   
         MVI   COSTING,C'N'                                                     
         TM    CPYSTAT1,X'10'      MAKE COSTING POSTINGS                        
         BNO   *+8                                                              
         MVI   COSTING,C'Y'                                                     
         MVC   COMPSTA1,CPYSTAT1                                                
         MVC   COMPSTA2,CPYSTAT2                                                
         MVC   COMPSTA3,CPYSTAT3                                                
         MVC   COMPSTA4,CPYSTAT4                                                
         MVC   COMPSTA5,CPYSTAT5                                                
         MVC   COMPSTA6,CPYSTAT6                                                
         MVC   COMPSTA7,CPYSTAT7                                                
         MVC   COMPSTA8,CPYSTAT8                                                
*                                                                               
         XC    COMPGMOA,COMPGMOA                                                
         CLI   CPYLN,CPYLN3Q       LONG ENOUGH FOR THESE FIELDS?                
         BL    INIT07              NO                                           
         MVC   COMPTMSD,CPYTMSSD                                                
         MVC   COMPSTA9,CPYSTAT9                                                
         MVC   COMPSTAA,CPYSTATA                                                
         MVC   COMPGMOA,CPYGLMOA                                                
*                                                                               
INIT07   CLI   CPYLN,CPYLN2Q                                                    
         BL    *+10                                                             
         MVC   CURCOD,CPYCURR                                                   
*                                                                               
INIT08   OC    CURCOD,CURCOD       DO WE HAVE A CODE?                           
         BNZ   INIT10              YES                                          
         GOTO1 AGETCUR                                                          
*                                                                               
INIT10   MVC   KEY+1(2),PRODUL                                                  
         GOTO1 AREAD,AIOAREA1      GET PRODUCTION LEDGER RECORD                 
         BE    INIT12                                                           
         MVC   XTRAMESS(6),=C'LEDGER'                                           
         MVC   XTRAMESS+7(2),PRODUL                                             
         B     ERROR                                                            
*                                                                               
INIT12   LA    R1,PRODHEIR                                                      
         BAS   RE,INITHEIR         GET HEIRARCHY LENGTHS                        
*                                  GET TODAY'S DATE                             
         GOTO1 VDATCON,DMCB,(5,0),(0,TODAY)       TODAY YYMMDD                  
         GOTO1 VDATCON,DMCB,(0,TODAY),(1,TODAYP)  PACKED,                       
         GOTO1 VDATCON,DMCB,(0,TODAY),(2,TODAYC)  COMPRESSED                    
         MVC   PMOS,TODAYP                                                      
         MVC   MOS(1),TODAY+1                                                   
         MVC   MOS+1(1),TODAY+3                                                 
         CLI   TODAY+2,C'1'                                                     
         BNE   INIT15                                                           
         NI    MOS+1,X'C3'         ('0=A,11=B,12=C)                             
         ZIC   R1,MOS+1                                                         
         LA    R1,1(,R1)                                                        
         STC   R1,MOS+1                                                         
*                                                                               
INIT15   MVC   YM,MOS              SAVE AS YEAR/MONTH                           
         MVC   MY+1(1),YM          AND MONTH/YEAR                               
         MVC   MY(1),YM+1                                                       
*                                                                               
INIT16   DS    0H                                                               
*                                                                               
         USING OFFALD,R1                                                        
         L     R1,AOFFBLK                                                       
         MVC   OFFACOMF,ACOMFACS                                                
         MVC   OFFAALPH,TWAAGY                                                  
         MVC   OFFACPY,COMPANY                                                  
         MVC   OFFACST1(OFFAOPOS-OFFACST1),COMPSTA1                             
         MVC   OFFALIMA,TWAACCS                                                 
         MVC   OFFAAUTH,TWAAUTH                                                 
         MVI   OFFAINDS,OFFAIOFF                                                
         MVI   OFFAACT,OFFAINI                                                  
         OC    OFFASAV(OFFASAVL),SAVEOFFA                                       
         BZ    *+8                                                              
         MVI   OFFAACT,OFFARES                                                  
         GOTO1 VOFFAL                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SAVEOFFA,OFFASAV                                                 
         DROP  R1                                                               
*                                                                               
         GOTO1 AGETAGCY                                                         
*                                                                               
INITX    B     VALACTN                                                          
         EJECT ,                                                                
***********************************************************************         
* EXTRACT HEIRARCHY LENGTHS FROM A LEDGER RECORD.                     *         
*                                                                     *         
* LENGTHS ARE EXTRACTED FROM RECORD ADDRESSED BY AIOAREA INTO 3-BYTE  *         
* FIELD ADDRESSED BY R1.                                              *         
***********************************************************************         
         SPACE 1                                                                
INITHEIR NTR1                                                                   
         MVC   LEDGBILL,SPACES     SET NO-COMPANY-BILL NOS                      
         L     RE,AIOAREA                                                       
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
*                                                                               
INITH2   CLI   0(RE),0                                                          
         BE    EXIT                                                             
         CLI   0(RE),X'16'                                                      
         BE    INITH6                                                           
         CLI   0(RE),X'11'                                                      
         BE    INITH8                                                           
*                                                                               
INITH4   IC    RF,1(,RE)                                                        
         AR    RE,RF                                                            
         B     INITH2                                                           
*                                                                               
         USING ACLELD,RE                                                        
INITH6   MVC   0(1,R1),ACLVALS                                                  
         MVC   1(1,R1),ACLVALS+(L'ACLVALS)                                      
         MVC   2(1,R1),ACLVALS+(L'ACLVALS*2)                                    
         B     INITH4                                                           
*                                                                               
         USING PMDELD,RE                                                        
INITH8   MVC   LEDGBILL,PMDLBILL                                                
         MVC   LEDGRSET,PMDRBILL                                                
         B     INITH4                                                           
         DROP  RE                                                               
         EJECT ,                                                                
***********************************************************************         
* VALIDATE ACTION CODE                                                *         
***********************************************************************         
         SPACE 1                                                                
VALACTN  MVI   ANYKEY,C'N'                                                      
         TM    BILACTH+4,X'20'                                                  
         BO    *+8                                                              
         MVI   LACTION,0                                                        
         GOTO1 AFVAL,BILACTH                                                    
         BZ    ERROR               ACTION IS REQUIRED FIELD                     
         XC    WORK,WORK           GET  COMPANY   PROFILE                       
         MVC   WORK(4),=C'ABIL'                                                 
         NI    WORK,X'BF'                                                       
         MVC   WORK+4(1),COMPANY                                                
         MVC   WORK+12(2),TWAAGY                                                
         GOTO1 VGETPROF,DMCB,WORK,PROGPROF,VDATAMGR                             
         LA    R0,SCANRHSL                                                      
         LA    R2,SCANBLK                                                       
         GOTO1 VSCANNER,DMCB,((R0),FLDH),(10,(R2))                              
         MVC   FLAG1,4(R1)         NUMBER IN ACTION FIELD                       
         MVI   FERN,INVALID                                                     
         CLI   FLAG1,0             ACTION IS KNOWN TO BE PRESENT SO             
         BE    ERROR               ZERO FIELDS MUST BE AN ERROR                 
         ZIC   RE,0(R2)                                                         
         BCTR  RE,0                FIRST FIELD IS ACTION VERB                   
         MVI   FERN,TOOSMALL       MIN 2 CHARS                                  
         CLI   0(R2),2                                                          
         BL    ERROR                                                            
         MVI   FERN,INVACTN                                                     
*                                                                               
         USING ACTD,R1                                                          
         L     R1,AACTNTAB                                                      
*                                  LOOK UP ACTION IN ACTION TABLE               
VALACT02 CLI   0(R1),X'FF'                                                      
         BE    ERROR                                                            
         CLI   0(R2),2             MATCH ON SHORT IF 2 CHARS                    
         BH    VALACT06                                                         
         CLC   ACTDSHT,12(R2)                                                   
         BE    VALACT08                                                         
*                                                                               
VALACT04 LA    R1,ACTDLEN(,R1)                                                  
         B     VALACT02                                                         
*                                                                               
VALACT06 EXCLC RE,ACTDNAME,12(R2)  OTHERWISE FULL                               
         BNE   VALACT04                                                         
*                                                                               
VALACT08 MVC   ACTNVALS,0(R1)      MATCH FOUND                                  
*                                                                               
         TM    ACTINDS,READOK      IS READ ONLY ACCESS ALLOWED?                 
         BO    VALACT10            YES                                          
         TM    FACFLAG,XIROSYS+XIROMODE+XIWRONGF                                
         BZ    VALACT10                                                         
         L     R1,FADR                                                          
         OI    6(R1),X'40'         SET CURSOR                                   
                                                                                
         LHI   R2,360                                                           
         XC    DMCB,DMCB                                                        
         TM    FACFLAG,XIROMODE    CONNECTED IN READ ONLY MODE?                 
         BO    VALACT09            YES                                          
                                                                                
         LHI   R2,357                                                           
         LA    RE,FACUPD                                                        
         ST    RE,DMCB+8                                                        
         MVI   DMCB+8,L'FACUPD                                                  
         TM    FACFLAG,XIWRONGF    CONNECTED TO WRONG FACPAK?                   
         BO    VALACT09            YES                                          
                                                                                
         LHI   R2,358              CONNECTED TO READ ONLY SYSTEM                
         XC    DMCB,DMCB                                                        
*                                                                               
VALACT09 GOTOR ATXTGET,DMCB,(C'E',(R2)),0,,0                                    
         B     EXIT                                                             
*                                                                               
VALACT10 CLI   ACTION,DIS          DISPLAY WITHOUT A PARA IS SUMMARY            
         BNE   VALACT12                                                         
         CLI   BILPARAH+5,0                                                     
         BE    VALACT12            DISPLAY SUMMARY                              
         DROP  R1                                                               
*                                                                               
         L     R1,AACTNTAB                                                      
         MVC   ACTNVALS,DDETNTRY-ACTNTAB(R1)  DISPLAY PARAGRAPH                 
*                                                                               
VALACT12 CLI   ACTION,HLP                                                       
         BE    GOINIT                                                           
         TM    ACTINDS,DDSONLY     CHECK FOR DDS ONLY                           
         BZ    VALACT14                                                         
         CLI   TWAOFFC,C'*'                                                     
         BNE   VALACT04                                                         
*                                                                               
VALACT14 CLI   ACTION,BIL          IF ACTION IS BILL                            
         BNE   VALACT16                                                         
         TM    TWAAUTH,X'10'       THEN 10 BIT MUST BE ON                       
         BNO   VALACT04                                                         
*                                                                               
VALACT16 CLC   ACTION,LACTION                                                   
         BE    VALACT18                                                         
         XC    LCLI(18),LCLI                                                    
         MVI   ANYKEY,C'Y'                                                      
*                                                                               
VALACT18 CLI   ACTION,AUT                                                       
         BE    VALACT20                                                         
         CLI   ACTION,WRT                                                       
         BNE   VALOPT                                                           
*                                                                               
VALACT20 CLI   1(R2),0            AUT=1,2 ETC                                   
         BE    VALOPT              USE DEFAULT                                  
         MVI   FERN,INVALID                                                     
         MVC   BILFORM,11(R2)      BINARY FORMAT NUMBER                         
         CLI   BILFORM,0                                                        
         BE    ERROR                                                            
         EJECT ,                                                                
***********************************************************************         
* VALIDATE OPTIONS                                                    *         
***********************************************************************         
         SPACE 1                                                                
VALOPT   CLI   ACTION,EDIT                                                      
         BE    VALOPTX                                                          
         LA    R0,BILACTH                                                       
         LA    R2,SCANBLK+22+SCANRHSL                                           
         MVI   FNDX,2                                                           
         ST    R0,FADR                                                          
*                                                                               
VALOPT2  CLC   FNDX,FLAG1          ALL PROCESSED - EXIT                         
         BH    VALOPTX                                                          
         MVI   FERN,NOINPUT        CHECK L'KEYWORD                              
         ZIC   RE,0(,R2)                                                        
         SH    RE,=H'1'                                                         
         BM    ERROR                                                            
*                                                                               
         USING PARMD,RF                                                         
         L     RF,AOPTNTAB                                                      
         MVI   FERN,INVALID                                                     
         SR    R0,R0                                                            
*                                                                               
VALOPT4  CLI   PARMLEN,0           CHECK FOR VALID KEYWORD                      
         BE    ERROR                                                            
         EXCLC RE,PARMWORD,12(R2)                                               
         BE    VALOPT6                                                          
*                                                                               
VALOPT5  IC    R0,PARMLEN                                                       
         AR    RF,R0                                                            
         B     VALOPT4                                                          
*                                                                               
VALOPT6  LA    R1,PARMACTS         COMPATIBLE WITH ACTION                       
*                                                                               
VALOPT7  CLI   0(R1),0                                                          
         BE    VALOPT5             END OF LIST                                  
         CLI   0(R1),255                                                        
         BE    VALOPT8             VALID FOR ALL ACTIONS                        
         CLC   ACTION,0(R1)                                                     
         BE    VALOPT8                                                          
         LA    R1,1(,R1)                                                        
         B     VALOPT7                                                          
*                                                                               
VALOPT8  TM    PARMINDS,DDSONLY    OTHER CHECKS                                 
         BZ    VALOPT8A                                                         
         CLI   TWAOFFC,C'*'                                                     
         BNE   VALOPT5                                                          
*                                                                               
VALOPT8A MVI   FERN,TOOSHORT                                                    
         CLC   1(1,R2),PARMMIN                                                  
         BL    ERROR                                                            
         MVI   FERN,TOOLONG                                                     
         CLC   1(1,R2),PARMMAX                                                  
         BH    ERROR                                                            
*                                                                               
VALOPT9  SR    RE,RE               TEST FOR DUPLICATE                           
         ICM   RE,3,PARMDEST                                                    
         AR    RE,R9               RE = A(FNDX+OUTPUT VALUE OF PARAM)           
         CLI   0(RE),0                                                          
         MVI   FERN,DUPED                                                       
         BNE   ERROR                                                            
         MVC   0(1,RE),FNDX                                                     
*                                                                               
VALOPT10 CLI   PARMSR,0            CALL VALIDATE/CONVERT SR IF ANY              
         BNE   VALOPT12                                                         
         MVI   FERN,INVALID                                                     
         ICM   RF,15,PARMSR                                                     
         A     RF,RELO                                                          
         GOTO1 (RF),DMCB,(R2),(RE)                                              
         BNE   ERROR                                                            
         B     VALOPT20                                                         
*                                                                               
VALOPT12 CLI   PARMSR,C'Y'         OR MOVE IN C'Y' TO OUTPUT VALUE              
         BNE   VALOPT14                                                         
         MVI   0(RE),C'Y'                                                       
         B     VALOPT20                                                         
*                                                                               
VALOPT14 ZIC   R1,PARMSR           OR MOVE INPUT TO OUTPUT                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),22(R2)                                                   
*                                                                               
VALOPT20 ZIC   R1,FNDX             BUMP TO NEXT FIELD                           
         LA    R1,1(,R1)                                                        
         STC   R1,FNDX                                                          
         LA    R2,22+SCANRHSL(R2)                                               
         B     VALOPT2                                                          
*                                                                               
VALOPTX  MVI   FNDX,0                                                           
         MVI   FERN,OK                                                          
         OI    BILACTH+4,X'20'                                                  
         CLI   GSTOPT,C'N'         STOP GST CALCULATION ?                       
         BNE   *+8                 NO                                           
         NI    RUNOPT,X'FF'-(NEEDGST+NEEDPST)                                   
         CLI   PSTOPT,C'N'         STOP PST CALCULATION ?                       
         BNE   *+8                 NO                                           
         NI    RUNOPT,X'FF'-NEEDPST                                             
         B     VALBIL                                                           
         DROP  RF                                                               
         EJECT ,                                                                
***********************************************************************         
* ROUTINES FOR ACTION PARAMETER (OPTION) VALIDATION                   *         
*                                                                     *         
* ON ENTRY P1 CONTAINS A(SCANNER ENTRY) AND P2 A(OUTPUT VALUE)        *         
* ON EXIT CC=NEQ IF ERROR AND FERN IS SET                             *         
*                                                                     *         
* VALIDATE EXTENSION DAYS                                             *         
***********************************************************************         
         SPACE 1                                                                
VALDAY   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVI   FERN,INVNUM                                                      
         TM    3(R2),X'80'         SCANNER NUMERIC BIT FOR 2ND HALF             
         BZ    VALDAYX                                                          
         CLI   11(R2),X'15'        MAX IS 15 DAYS EXTRA                         
         BH    VALDAYX                                                          
         MVC   0(1,R3),11(R2)      BINARY VALUE OF 2ND HALF                     
         MVI   FERN,OK                                                          
*                                                                               
VALDAYX  CLI   FERN,OK                                                          
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE BILLING DATE                                              *         
*    R2 = INPUT FIELD SCAN FIELD                                      *         
*    R3 = RESULT                                                      *         
***********************************************************************         
         SPACE 1                                                                
VALBDTE  NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVI   FERN,INVDATE        FORMAT IS MMMDD/YY                           
         GOTO1 VDATVAL,DMCB,(0,22(R2)),WORK                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    VALBDTEX            DATE INVALID                                 
         GOTO1 VDATCON,DMCB,(0,WORK),(1,0(R3))                                  
         MVI   FERN,OK                                                          
*                                                                               
VALBDTEX CLI   FERN,OK                                                          
         B     EXIT                                                             
*                                                                               
*        VALIDATE AMOUNT=$$$$                                                   
*                                                                               
VALAMT   NTR1                                                                   
         MVI   FERN,INVAMNT                                                     
         LM    R2,R3,0(R1)                                                      
         SR    R0,R0                                                            
         IC    R0,1(,R2)                                                        
         GOTO1 VCASHVAL,DMCB,22(R2),(R0)                                        
         CLI   0(R1),X'FF'         SEE IF AMOUNT WAS INVALID                    
         BE    VALAMTX                                                          
         MVC   0(4,R3),4(R1)       SAVE VALID CASH AMOUNT                       
         MVI   FERN,OK                                                          
*                                                                               
VALAMTX  CLI   FERN,OK                                                          
         B     EXIT                                                             
*                                                                               
* VALIDATE BILL MOS                                                             
*                                                                               
VALBMOS  NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVI   FERN,INVDATE        FORMAT IS MMM/YY                             
         GOTO1 VDATVAL,DMCB,(2,22(R2)),WORK                                     
         OC    0(4,R1),0(R1)                                                    
         BZ    VALBMOSX            DATE INVALID                                 
*                                                                               
         CLC   WORK(2),=C'80'      DON'T ALLOW BEFORE 1980                      
         BL    VALBMOSX                                                         
*                                                                               
         GOTO1 VDATCON,(R1),WORK,(1,BILMNTH)                                    
         GOTO1 VDATCON,(R1),WORK,(1,WORK+6)                                     
         MVC   PBILMOS,WORK+6      SAVE PACKED YYMM                             
         MVC   0(1,R3),WORK+1      OUTPUT(WORK) IS YYMM                         
         MVC   1(1,R3),WORK+3                                                   
         CLI   WORK+2,C'1'                                                      
         BNE   VALBMOS2                                                         
         NI    1(R3),X'C3'         ('0=A,11=B,12=C)                             
         ZIC   R1,1(,R3)                                                        
         LA    R1,1(,R1)                                                        
         STC   R1,1(,R3)                                                        
*                                                                               
VALBMOS2 MVI   FERN,OK                                                          
*                                                                               
VALBMOSX CLI   FERN,OK                                                          
         B     EXIT                                                             
*                                                                               
* VALIDATE TYPE                                                                 
*                                                                               
VALTYPE  NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVI   FERN,INVTYPE        MUST BE 'P' OR 'B'                           
         CLI   22(R2),C'P'         IS IT PRODUCTIVITY ?                         
         BE    VALTYPE2            OK                                           
         CLI   22(R2),C'B'         NO, IS IT BUDGET ?                           
         BNE   VALTYPEX            NO, ERROR                                    
*                                                                               
VALTYPE2 MVI   FERN,OK             YES, INDICATE OK                             
*                                                                               
VALTYPEX CLI   FERN,OK                                                          
         B     EXIT                                                             
*                                                                               
* VALIDATE WRITE-OFF ACCOUNT OVERRIDE                                           
*                                                                               
VALOWO   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVI   FERN,INVALID                                                     
         MVC   WOAOVR(2),=C'SI'    SET UP FOR SI ACCOUNT                        
         LA    R1,WOAOVR                                                        
         CLC   22(2,R2),=C'SI'     DID THEY ENTER SI ?                          
         BE    *+8                 YES, MOVE WHOLE THING                        
         LA    R1,2(,R1)           NO, SHIFT PAST SI                            
         SR    RF,RF                                                            
         IC    RF,1(,R2)                                                        
         EXMVC RF,0(R1),22(R2)                                                  
         OC    WOAOVR,SPACES       WRITE OFF ACCOUNT OVERRIDE                   
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(L'WOAOVR),WOAOVR                                           
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   VALOWOX                                                          
         MVI   FERN,OK                                                          
*                                                                               
VALOWOX  CLI   FERN,OK                                                          
         B     EXIT                                                             
*                                                                               
* VALIDATE WORKCODE TYPE FILTER                                                 
*                                                                               
VALWTYP  NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVI   FERN,INVTYPE                                                     
         MVC   BYTE,22(R2)                                                      
         CLI   22(R2),C'*'         ALL EXCEPT ?                                 
         BNE   *+10                                                             
         MVC   BYTE,23(R2)                                                      
         MVC   0(1,R3),BYTE                                                     
         CLI   BYTE,C'O'           MUST BE O, P, T OR M                         
         BE    VALWTYP2                                                         
         CLI   BYTE,C'P'                                                        
         BE    VALWTYP2                                                         
         CLI   BYTE,C'T'                                                        
         BE    VALWTYP2                                                         
         CLI   BYTE,C'M'                                                        
         BNE   VALWTYPX                                                         
*                                                                               
VALWTYP2 CLI   22(R2),C'*'                                                      
         BNE   *+8                                                              
         NI    0(R3),X'FF'-X'40'    MAKE IT AN ALL EXCEPT                       
         MVI   FERN,OK                                                          
*                                                                               
VALWTYPX CLI   FERN,OK                                                          
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE BATCH TYPE FILTER                                         *         
***********************************************************************         
         SPACE 1                                                                
VALBTYP  NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVI   FERN,INVTYPE                                                     
         MVC   0(2,R3),=C'00'      INITIALIZE                                   
         ZIC   RE,1(,R2)           GET  LENGTH    OF   INPUT                    
         BCTR  RE,0                MINUS     ONE  FOR  EXECUTE   INST           
         LA    RF,22(,R2)          ->   INPUT DATA                              
         CLI   0(RF),C'*'          ALL  EXCEPT    ENTERED ?                     
         BE    VALBT10             YES, PROCESS   C'*' INPUT                    
         CLI   1(R2),3             3    OR   MORE CHARS     ENTERED ?           
         BNL   VALBTYPX            YES, INVALID                                 
         B     VALBT20             CONTINUE                                     
*                                                                               
VALBT10  DS    0H                  ALL  EXCEPT    ENTERED                       
         LA    RF,1(,RF)           BUMP TO   INPUT     DATA                     
         BCTR  RE,0                MINUS     ONE  FROM INPUT     LENGTH         
         CLI   1(R2),1             ONLY C'*' ENTERED ?                          
         BE    VALBTYPX            YES, INVALID                                 
*                                                                               
VALBT20  DS    0H                  MOVE THE  DATA                               
         LTR   RE,RE               ONLY ONE  CHAR OF   BATCH     TYPE ?         
         BNZ   *+8                 NO,  MOVE TWO  CHARACTERS                    
         LA    R3,1(,R3)           MOVE INTO 2ND  BYTE OF   BATYP               
         EXMVC RE,0(R3),0(RF)      MOVE THE  BATCH     TYPE                     
*                                                                               
         L     R3,4(,R1)           ->   REINITIALIZE   BATCH     TYPE           
         CLI   0(R3),C'0'          1ST  CHAR <    C'0' ?                        
         BL    VALBTYPX            YES, INVALID                                 
         CLI   0(R3),C'9'          1ST  CHAR >    C'9' ?                        
         BH    VALBTYPX            YES, INVALID                                 
         CLI   1(R3),C'0'          2ND  CHAR <    C'0' ?                        
         BL    VALBTYPX            YES, INVALID                                 
         CLI   1(R3),C'9'          2ND  CHAR >    C'9' ?                        
         BH    VALBTYPX            YES, INVALID                                 
         CLC   0(2,R3),=C'00'      C'00' ?                                      
         BE    VALBTYPX            YES, INVALID                                 
*                                                                               
         MVI   FERN,OK                                                          
         CLI   22(R2),C'*'         ALL  EXCEPT ?                                
         BNE   *+8                 NO,  SKIP                                    
         NI    0(R3),X'FF'-X'40'   MAKE IT   ALL  EXCEPT                        
*                                                                               
VALBTYPX CLI   FERN,OK             SET  COND CODE                               
         B     EXIT                RETURN    TO   CALLER                        
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE WORKCODE FILTER                                           *         
***********************************************************************         
         SPACE 1                                                                
VALWCF   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVI   FERN,INVWC                                                       
         MVC   HALF,22(R2)         SAVE WORK CODE                               
         CLI   HALF,C'*'           ALL EXCEPT                                   
         BNE   *+10                                                             
         MVC   HALF,23(R2)                                                      
         GOTO1 AGETWC,HALF                                                      
         BNE   VALWCFX             INVALID WORK CODE                            
         MVC   0(2,R3),HALF                                                     
         CLI   22(R2),C'*'                                                      
         BNE   *+8                                                              
         NI    0(R3),X'FF'-X'40'   MAKE IT AN ALL EXCEPT                        
         MVI   FERN,OK                                                          
*                                                                               
VALWCFX  CLI   FERN,OK                                                          
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE Y OR N                                                    *         
***********************************************************************         
         SPACE 1                                                                
VALYORN  NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVI   FERN,INVALID                                                     
         MVC   0(1,R3),22(R2)      SAVE INPUT VALUE                             
         CLI   0(R3),C'Y'                                                       
         BE    VALYORN2                                                         
         CLI   0(R3),C'N'                                                       
         BNE   VALYORN4                                                         
*                                                                               
VALYORN2 MVI   FERN,OK                                                          
*                                                                               
VALYORN4 CLI   FERN,OK                                                          
         B     EXIT                                                             
*                                                                               
* VALIDATE Y                                                                    
*                                                                               
VALY     NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVI   FERN,INVALID                                                     
         MVC   0(1,R3),22(R2)      SAVE INPUT VALUE                             
         CLI   0(R3),C'Y'                                                       
         BNE   *+8                                                              
         MVI   FERN,OK                                                          
         CLI   FERN,OK                                                          
         B     EXIT                                                             
*                                                                               
* VALIDATE N                                                                    
*                                                                               
VALN     NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVI   FERN,INVALID                                                     
         MVC   0(1,R3),22(R2)      SAVE INPUT VALUE                             
         CLI   0(R3),C'N'                                                       
         BNE   *+8                                                              
         MVI   FERN,OK                                                          
         CLI   FERN,OK                                                          
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
* VALIDATE PRINT LANGUAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
VALPLANG NTR1                                                                   
         CLI   ACTION,ADD          ACTION     ADD ?                             
         BNE   VALPLN20            NO,   OKAY LANG  OPTION                      
         CLI   FIRST,C'Y'          ADD   FIRST ?                                
         BE    VALPLN20            YES,  OKAY LANG  OPTION                      
         LA    R2,SCANBLK          CHECK FOR  ADD,LANG=X,FIRST                  
         ZIC   R0,FLAG1            NUM   OF   ENTRIES    IN   SCANBLK           
         SR    RF,RF               CLEAR REGISTER                               
*                                                                               
         USING PARMD,R3            MAP   OPTION     TABLE                       
         L     R3,AOPTNTAB         ->    OPTION     TABLE                       
*                                  ->    FIRST      ENTRY                       
         LA    R3,OPTFI-OPTNTAB(,R3)                                            
*                                                                               
VALPLN10 LA    R2,22+SCANRHSL(,R2) GET   NEXT ENTRY                             
         IC    RF,0(,R2)           LEN   OF   TBL   ENTRY                       
         BCTR  RF,0                MINUS ONE  FOR   EXECUTE                     
         EXCLC RF,12(R2),PARMWORD  FIRST ?                                      
         BE    VALPLN20            YES,  OKAY LANG  OPTION                      
         BCT   R0,VALPLN10         NO,   TRY  NEXT  ENTRY                       
         B     VALPLNEX            FIRST NOT  FOUND                             
         DROP  R3                                                               
*                                                                               
VALPLN20 MVC   PLANG,CULANG        DEFAULT    LANGUAGE                          
         LM    R2,R3,0(R1)         R2=   SCAN BLOCK ENTRY                       
         MVC   0(1,R3),22(R2)      R3=   SAVE ENTRY ADDR                        
         L     RF,=A(LANGTBL)      ->    LANGUAGE   TABLE                       
         A     RF,RELO                                                          
*                                                                               
VALPLN30 CLI   0(RF),EOT           END   OF   TABLE ?                           
         BE    VALPLNEX            YES,  EXIT                                   
         CLC   22(1,R2),0(RF)      VALID LANGUAGE ?                             
         BE    VALPLN40            YES,  SAVE LANGUAGE                          
         LA    RF,2(,RF)           TRY   NEXT LANGUAGE                          
         B     VALPLN30            TEST  NEXT LANGUAGE                          
*                                                                               
VALPLN40 MVC   PLANG,1(RF)         SAVE  LANG NUMBER                            
*                                                                               
         L     R3,=A(CURSIGNT)     ->    CURRENCY  SIGN TABLE                   
         A     R3,RELO                                                          
         ZIC   R2,PLANG            GET   PRINT     LANGUAGE                     
         MH    R2,=AL2(L'CURSIGNT) GET   TABLE     ENTRY                        
         AR    R3,R2                                                            
         MVC   PCSIGN,0(R3)        GET   PRINT     SIGN                         
*                                                                               
         MVI   WCCOMMA,COMMA       WORK  CODE LIST USES COMMA                   
         CLI   PLANG,2             LANGUAGE   ENGLISH ?                         
         BNH   *+8                 YES,  SKIP                                   
         MVI   WCCOMMA,SEMICOLN    WORK  CODE LIST USES SEMI-COLON              
*                                                                               
         MVI   FERN,OK             SAY   VALID     OPTION                       
*                                                                               
VALPLNEX CLI   FERN,OK             SET   CONDITION CODE                         
         B     EXIT                RETURN                                       
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE BILL NUMBER                                               *         
***********************************************************************         
         SPACE 1                                                                
VALBIL   GOTO1 AFVAL,BILPARAH      FIX ACTION VALUES IF NEEDED                  
         TM    FLDH+4,8                                                         
         BZ    VALBIL02                                                         
         CLC   FLDH+2(2),=H'35'                                                 
         BH    VALBIL02                                                         
         MVC   PARAHEX,FLDH+3                                                   
*                                                                               
VALBIL02 CLI   ACTION,ADD          ADD,FIRST OR ADD WITH PARA=1 IMPLIES         
         BNE   VALBIL04            A NEW BILL                                   
         CLI   FIRST,C'Y'                                                       
         BE    VALBIL03                                                         
         CLI   NEXT,C'Y'                                                        
         BE    VALBIL08                                                         
         CLI   PARAHEX,1                                                        
         BNE   VALBIL08                                                         
*                                                                               
VALBIL03 MVI   ACTION,NEWBILL                                                   
         NI    ACTINDS,X'FF'-HASBILNO                                           
         B     VALBIL08                                                         
*                                                                               
VALBIL04 CLI   ACTION,DID          DISPLAY PARAGRAPH                            
         BE    VALBIL08                                                         
         CLI   ACTION,DIS          DISPLAY SUMMARY                              
         BE    VALBIL08                                                         
*                                                                               
VALBIL06 CLI   ACTION,DEL          DELETE ALL HAS DIFFERENT INDICATORS          
         BNE   VALBIL08            FROM DELETE PARA                             
         CLC   BILPARA,=C'ALL'                                                  
         BNE   VALBIL08                                                         
         NI    ACTINDS,X'FF'-HASPARNO-DISPARA-TWOSTAGE                          
         MVI   SCREEN,0                                                         
*                                                                               
VALBIL08 TM    BILNUMH+4,X'20'     FIELD PREVIOUSLY VALIDATED ?                 
         BO    *+8                 YES, SKIP                                    
         MVI   ANYKEY,C'Y'                                                      
         GOTO1 AFVAL,BILNUMH       NOW CHECK BILL NUMBER                        
         BNE   VALBIL09                                                         
         TM    ACTINDS,HASBILNO    DOES ACTION NEED BILL NUMBER                 
         BNO   VALBIL18                                                         
         MVI   FERN,NOINPUT                                                     
         LA    R0,BILNUMH                                                       
         ST    R0,FADR                                                          
         B     ERROR               MISSING BILL NUMBER                          
*                                                                               
VALBIL09 MVI   FERN,INVALID                                                     
         CLI   ACTION,AUT          NO BILL NUMBER FOR AUTO                      
         BE    ERROR                                                            
         CLI   ACTION,ALL          AND ALLOCATION                               
         BE    ERROR                                                            
         CLI   ACTION,WRT          AND WRITE                                    
         BE    ERROR                                                            
                                                                                
         TM    FLDH+4,8            NUMERIC FIELD ?                              
         BO    VALBIL10            YES, SKIP                                    
         MVI   FERN,INVNUM                                                      
         CLI   ACTION,DRA                                                       
         BNE   VALBIL1A                                                         
         CLC   FLD(6),=C'LINEUP'                                                
         BNE   VALBIL10                                                         
         MVI   PHASE,OVPHPRT       FOR LINEUP GO DIRECT TO PRINT PHASE          
         B     GO                                                               
*                                                                               
VALBIL10 OC    FLDH(4),FLDH        IF NUMERIC                                   
         MVI   FERN,INVALID                                                     
         BZ    ERROR               MUST BE NONZERO                              
VALBIL1A L     R1,FLDH                                                          
         CVD   R1,DUB                                                           
         ZAP   BILNUMP,DUB                                                      
*                                                                               
         CLI   ACTION,REV          IF REVERSE BILL NO WORKER FILE               
         BNE   VALBIL11                                                         
         CLI   BILNUMH+5,6                                                      
         BE    VALCLI                                                           
         MVI   FERN,INVALID                                                     
         B     ERROR                                                            
*                                                                               
VALBIL11 MVC   BILNUM,SPACES                                                    
         EDIT  BILNUMP,(6,BILNUM),ALIGN=LEFT                                    
         OI    BILNUMH+6,FVOXMT    TRANSMIT                                     
         CLI   ACTION,REC                                                       
         BE    VALPARA                                                          
         GOTO1 AWRKID              CONVERT TO WORKER ID FORMAT IN WRKEY         
         CLC   WRKEY(8),WRKSAVE    IS IT IN PROCESS                             
         BNE   VALBIL13                                                         
         MVI   FERN,DUPLICT        YES                                          
         CLI   ACTION,NEWBILL                                                   
         BE    ERROR               ERROR IF NEW BILL                            
         GOTO1 AWRKBUR,AIOAREA1    OTHERWISE RESTORE BUFFER                     
         BNE   ERROR                                                            
         MVI   WRKPARA,0           AND GET HEADER                               
         L     RF,AWRKLGTL                                                      
         CLI   ACTION,BIL                                                       
         BE    VALBIL12                                                         
         CLI   ACTION,UNB                                                       
         BE    VALBIL12                                                         
         L     RF,AWRKLGET                                                      
*                                                                               
VALBIL12 BASR  RE,RF               LOCK IT IF BILL/UNBILL                       
         BNE   ERROR                                                            
         B     VALBIL16                                                         
*                                                                               
VALBIL13 MVI   ANYKEY,C'Y'                                                      
         MVI   LPARA,0             IT ISN'T IN PROCESS                          
         GOTO1 AFINDBIL,AIOAREA1   DOES IT EXIST (IF SO GET HDR)                
         BE    VALBIL15                                                         
         CLI   FERN,NOTFOUND       NO                                           
         BNE   ERROR                                                            
         CLI   ACTION,NEWBILL      ERROR UNLESS NEW BILL/ALLOC                  
         BE    VALBIL18                                                         
         B     ERROR                                                            
*                                                                               
VALBIL15 CLI   ACTION,NEWBILL      YES                                          
         MVI   FERN,DUPLICT        ERROR IF NEW BILL                            
         BE    ERROR                                                            
*                                                                               
VALBIL16 CLI   ACTION,LIST         DISPLAY CLI/PRO/JOB CODES FROM HDR           
         BE    VALCLI              UNLESS ACTION IS LIST                        
*                                                                               
         USING HEADERD,R6                                                       
         L     R6,AIOAREA1                                                      
         MVC   SAVHSTAT,HSTAT                                                   
         LA    R1,HCLI                                                          
         LA    R2,BILCLIH                                                       
         LA    RF,DISCODE                                                       
         BASR  RE,RF                                                            
         LA    R1,HPRO                                                          
         LA    R2,BILPROH                                                       
         BASR  RE,RF                                                            
         LA    R1,HJOB                                                          
         LA    R2,BILJOBH                                                       
         LA    RE,VALBIL17                                                      
*                                                                               
DISCODE  OC    8(L'BILJOB,R2),SPACES                                            
         CLC   8(L'BILJOB,R2),0(R1)                                             
         BER   RE                                                               
         MVC   8(L'BILJOB,R2),0(R1)                                             
         OI    6(R2),FVOXMT        TRANSMIT                                     
         LA    R1,L'BILJOB         SET LEN FOR FVAL LATER                       
*                                                                               
DISCODE2 LA    R3,7(R1,R2)                                                      
         CLI   0(R3),C' '                                                       
         BH    DISCODE4                                                         
         BCT   R1,DISCODE2                                                      
*                                                                               
DISCODE4 STC   R1,5(,R2)                                                        
         BR    RE                                                               
*                                                                               
VALBIL17 MVC   PLANG,HPLANG        GET   PRINT     LANGUAGE                     
         CLI   PLANG,0             ANY   DEFINED ?                              
         BNE   *+8                 YES,  SKIP                                   
         MVI   PLANG,LANGEUS       USE   ENGLISH  SINCE ALREADY STARTED         
*                                                                               
         L     RE,=A(CURSIGNT)     ->    CURRENCY  SIGN TABLE                   
         A     RE,RELO                                                          
         ZIC   RF,PLANG            GET   PRINT     LANGUAGE                     
         MH    RF,=AL2(L'CURSIGNT) GET   TABLE     ENTRY                        
         AR    RE,RF                                                            
         MVC   PCSIGN,0(RE)        GET   PRINT     SIGN                         
*                                                                               
         MVI   WCCOMMA,COMMA       WORK  CODE LIST USES COMMA                   
         CLI   PLANG,2             LANGUAGE   ENGLISH ?                         
         BNH   *+8                 YES,  SKIP                                   
         MVI   WCCOMMA,SEMICOLN    WORK  CODE LIST USES SEMI-COLON              
*                                                                               
         CLI   HBILL,C'N'          CHECK FOR  BILLED    STATUS                  
         MVI   FERN,NTBILLED                                                    
         BNE   VALBL17A                                                         
         CLI   ACTION,UNB                                                       
         BE    ERROR                                                            
         B     VALBIL18                                                         
         DROP  R6                                                               
*                                                                               
VALBL17A MVI   FERN,BILLED                                                      
         TM    ACTINDS,UNBILLED                                                 
         BO    ERROR                                                            
*                                                                               
VALBIL18 CLI   PLANG,0             ANY   PRINT     LANGUAGE ?                   
         BNE   VALBL18B            YES,  CONTINUE                               
*                                                                               
*                                  WHEN  WE   CREATE    A   NEW  BILL           
*                                  WE    NEED A    PRINT    LANGUAGE            
         CLI   ACTION,AUT          ACTION     AUTO ?                            
         BE    VALBL18A            YES,  ASSIGN    PRINT    LANGUAGE            
         CLI   ACTION,ADD          ACTION     ADD ?                             
         BNE   VALBL18B            NO,   SKIP PRINT     LANGUAGE                
         CLI   FIRST,C'Y'          ADD,FIRST ?                                  
         BNE   VALBL18B            NO,   SKIP PRINT     LANGUAGE                
*                                                                               
VALBL18A DS    0H                  SET   PRINT     LANGUAGE TO                  
         MVC   PLANG,CULANG              CURRENT   LANGUAGE                     
         CLI   CULANG,0            ANY   DEFINED ?                              
         BNE   *+8                 YES,  SKIP                                   
         MVI   PLANG,LANGEUS       NO,   USE  ENGLISH                           
*                                                                               
         L     RE,=A(CURSIGNT)     ->    CURRENCY  SIGN TABLE                   
         A     RE,RELO                                                          
         ZIC   RF,PLANG            GET   PRINT     LANGUAGE                     
         MH    RF,=AL2(L'CURSIGNT) GET   TABLE     ENTRY                        
         AR    RE,RF                                                            
         MVC   PCSIGN,0(RE)        GET   PRINT     SIGN                         
*                                                                               
         MVI   WCCOMMA,COMMA       WORK  CODE LIST USES COMMA                   
         CLI   PLANG,2             LANGUAGE   ENGLISH ?                         
         BNH   *+8                 YES,  SKIP                                   
         MVI   WCCOMMA,SEMICOLN    WORK  CODE LIST USES SEMI-COLON              
*                                                                               
VALBL18B DS    0H                                                               
         GOTO1 VGETFACT,DMCB,0,0                                                
*                                                                               
         USING FACTSD,R2           MAP  GETFACT   BLOCK                         
         L     R2,DMCB             ->   GETFACT   BLOCK                         
         ZIC   R1,PLANG            GET  OVERRIDE  MESSAGE   LANGUAGE            
         MHI   R1,16               ->   TABLE     OF   ADDRESSES OF             
         A     R1,FAXLATES              SYSTEM    TRANSLATE TABLES              
         L     R1,8(,R1)           ->   UPPER     CASE TABLE                    
         ST    R1,AUPPERCT         SAVE THIS      ADDRESS                       
         DROP  R2                                                               
*                                                                               
         CLI   FOOT,C'Y'           ONLY  ONE  FOOTLINE                          
         BNE   VALBIL19                                                         
         TM    SAVHSTAT,FOOTLINE                                                
         BNO   VALBIL19                                                         
         MVI   FERN,DUPLICT                                                     
         LA    R1,BILACTH                                                       
         ST    R1,FADR                                                          
         B     ERROR                                                            
*                                                                               
VALBIL19 CLI   FIRST,C'Y'          PRE-SET PARA FOR FIRST OR NEXT               
         BNE   VALBL19A                                                         
         LA    RF,1                                                             
         B     VALBIL20                                                         
*                                                                               
VALBL19A CLI   NEXT,C'Y'                                                        
         BNE   VALPARA                                                          
         ZIC   RF,LPARA                                                         
         CLI   ACTION,DID          DISPLAY,NEXT                                 
         BNE   VALBL19B            NO,  ADD,NEXT                                
         CLC   LPARA,WRKLPARA      CURR PARAGRAPH # < LAST PARA # ?             
         BL    VALBL19C            YES, BUMP PARAGRAPH NUMBER                   
         B     VALBIL20            NO,  KEEP PARAGRAPH NUMBER                   
*                                                                               
VALBL19B IC    RF,WRKLPARA         ADD,NEXT                                     
*                                                                               
VALBL19C LA    RF,1(,RF)                                                        
*                                                                               
VALBIL20 MVC   BILPARA,SPACES                                                   
         EDIT  (RF),(2,BILPARA),FILL=0                                          
         MVI   BILPARAH+4,8                                                     
         MVI   BILPARAH+5,2                                                     
         OI    BILPARAH+6,FVOXMT   TRANSMIT                                     
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE PARAGRAPH FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
VALPARA  TM    ACTINDS,HASPARNO                                                 
         BZ    VALCLI                                                           
         GOTO1 AFVAL,BILPARAH                                                   
         BZ    ERROR                                                            
         MVI   FERN,INVNUM                                                      
         TM    FLDH+4,8                                                         
         BZ    ERROR                                                            
         OC    FLDH(4),FLDH                                                     
         BZ    ERROR                                                            
         MVI   FERN,TOOBIG                                                      
         CLC   FLDH+2(2),=H'35'                                                 
         BH    ERROR                                                            
         MVC   PARAHEX,FLDH+3                                                   
         CLI   ACTION,NEWBILL                                                   
         BE    VALCLI                                                           
         CLI   ACTION,ADD          ADD MUST BE PARA NUMBER AFTER LAST           
         BNE   VALPARA2            ON WORKER FILE                               
         ZIC   R1,WRKLPARA                                                      
         LA    R1,1(,R1)                                                        
         CH    R1,FLDH+2                                                        
         BL    ERROR                                                            
         MVI   FERN,DUPLICT                                                     
         BH    ERROR                                                            
         TM    SAVHSTAT,FOOTLINE   CANT ADD AFTER FOOTLINE                      
         BNO   VALCLI                                                           
         MVC   FVMSGNO,=AL2(2222)  ADD AFTER FOOTLINE NOT ALLOWED               
         B     ERRORX2A                                                         
*                                                                               
VALPARA2 CLC   WRKLPARA,PARAHEX    OTHERWISE PARA NUM MUST EXIST                
         BNL   VALCLI                                                           
         MVI   FERN,TOOBIG                                                      
         CLI   ACTION,INS                                                       
         BE    ERROR               EVEN FOR INSERT                              
         MVI   FERN,NOTFOUND                                                    
         B     ERROR                                                            
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE CLIENT CODE                                               *         
***********************************************************************         
         SPACE 1                                                                
VALCLI   OI    BILNUMH+4,X'20'                                                  
         GOTO1 AFVAL,BILCLIH                                                    
         BNZ   VALCLI0             CLIENT REQUIRED IF NOT LIST                  
         CLI   ACTION,LIST                                                      
         BNE   ERROR                                                            
         B     VALCLI1                                                          
*                                                                               
VALCLI0  MVI   FERN,TOOLONG                                                     
         CLC   FLDH+5(1),PRODHEIR  CHECK L'CLIENT CODE                          
         BH    ERROR                                                            
*                                                                               
         MVC   KEY,SPACES          BUILD CLIENT KEY                             
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),PRODUL                                                  
         MVC   KEY+3(L'LCLI),FLD                                                
         CLC   LCLI,FLD                                                         
         BE    VALCLI2                                                          
*                                                                               
VALCLI1  XC    LCLI(18),LCLI       CLEAR CLI/PRO/JOB DATA                       
         MVI   ANYKEY,C'Y'                                                      
         CLI   FLDH+5,0            (LIST WITHOUT CLIENT FILTER)                 
         BE    GOINIT                                                           
*                                                                               
VALCLI2  GOTO1 AREAD,ADCLI         READ CLIENT & CHECK OK                       
         BNE   ERROR                                                            
         L     RE,ADCLI                                                         
         SR    RF,RF                                                            
         ICM   RF,3,ACCORLEN(RE)                                                
         LA    RF,4(RF,RE)                                                      
         ST    RF,ADPRD            SET A(PRODUCT RECORD)                        
         GOTO1 AGETNAME,ADCLI                                                   
         MVC   BILCLIN,WORK        DISPLAY CLIENT NAME                          
         XC    ADDR,ADDR                                                        
         LA    R1,CSAVE            EXTRACT CLIENT PROFILES                      
         BAS   RE,EXTPROF                                                       
         MVC   LCLI,FLD            SAVE CLIENT CODE                             
         XC    WORK,WORK           GET CLIENT-LEVEL PROGRAM PROFILE             
         MVC   WORK(4),=C'ABIL'                                                 
         NI    WORK,X'BF'          LOWER CASE SYSTEM FOR ONLINE CALL            
         MVC   WORK+4(1),COMPANY                                                
         MVC   WORK+5(2),PRODUL                                                 
         MVC   WORK+7(3),LCLI                                                   
*                                                                               
         USING JOBD,R1                                                          
         LA    R1,CSAVE                                                         
         CLC   JOFFC,SPACES        TEST FOR CLIENT OFFICE                       
         BNH   VALCLI4             NONE                                         
         MVC   WORK+12(2),TWAAGY                                                
         TM    COMPSTA4,X'01'      TEST NEW OFFICES                             
         BO    VALCLI3             YES                                          
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),JOFFC                                                 
         B     VALCLI4                                                          
*                                                                               
VALCLI3  MVI   WORK+10,C'+'                                                     
         MVC   WORK+14(2),JOFFC                                                 
*                                                                               
VALCLI4  GOTO1 VGETPROF,DMCB,WORK,PROGPROF,VDATAMGR                             
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE PRODUCT CODE                                              *         
***********************************************************************         
         SPACE 1                                                                
VALPRO   GOTO1 AFVAL,BILPROH                                                    
         BNZ   VALPRO0             PROD REQUIRED IF NOT LIST                    
         CLI   ACTION,LIST                                                      
         BNE   ERROR                                                            
         B     VALPRO1                                                          
*                                                                               
VALPRO0  MVI   FERN,TOOLONG                                                     
         ZIC   RE,PRODHEIR                                                      
         ZIC   RF,PRODHEIR+1                                                    
         SR    RF,RE                                                            
         STC   RF,DUB              DUB(1)=MAX L'PRODUCT                         
         CLC   FLDH+5(1),DUB       CHECK L'PRODUCT CODE                         
         BH    ERROR                                                            
*                                                                               
         LA    RE,KEY+3(RE)        BUILD PRODUCT KEY                            
         MVC   0(L'LPRO,RE),FLD                                                 
         CLC   LPRO,FLD                                                         
         BE    VALPRO2                                                          
*                                                                               
VALPRO1  XC    LPRO(12),LPRO       CLEAR PRO/JOB DATA                           
         MVI   ANYKEY,C'Y'                                                      
         CLI   FLDH+5,0            (LIST WITHOUT PRODUCT FILTER)                
         BE    GOINIT                                                           
*                                                                               
VALPRO2  GOTO1 AREAD,ADPRD         READ PRODUCT & CHECK OK                      
         BNE   ERROR                                                            
         L     RE,ADPRD                                                         
         SR    RF,RF                                                            
         ICM   RF,3,ACCORLEN(RE)                                                
         LA    RF,4(RF,RE)                                                      
         ST    RF,ADJOB            SET A(JOB RECORD)                            
         GOTO1 AGETNAME,ADPRD                                                   
         MVC   BILPRON,WORK        DISPLAY PRODUCT NAME                         
         LA    R1,PSAVE            EXTRACT PRODUCT PROFILES                     
         BAS   RE,EXTPROF                                                       
         MVC   LPRO,FLD                                                         
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE JOB                                                       *         
***********************************************************************         
         SPACE 1                                                                
VALJOB   GOTO1 AFVAL,BILJOBH                                                    
         BNZ   VALJOB0             JOB REQUIRED IF NOT LIST                     
         CLI   ACTION,LIST                                                      
         BNE   ERROR                                                            
         B     VALJOB1                                                          
*                                                                               
VALJOB0  MVI   FERN,TOOLONG                                                     
         ZIC   RE,PRODHEIR+1                                                    
         ZIC   RF,PRODHEIR+2                                                    
         SR    RF,RE                                                            
         STC   RF,DUB                                                           
         CLC   FLDH+5(1),DUB       CHECK L'JOB CODE                             
         BH    ERROR                                                            
*                                                                               
         LA    RE,KEY+3(RE)                                                     
         MVC   0(L'LJOB,RE),FLD     BUILD LJOB KEY                              
         CLC   LJOB,FLD                                                         
         BE    VALJOB2                                                          
*                                                                               
VALJOB1  XC    LJOB,LJOB           CLEAR JOB DATA                               
         MVI   ANYKEY,C'Y'                                                      
         CLI   FLDH+5,0            (LIST WITHOUT JOB FILTER)                    
         BE    GOINIT                                                           
*                                                                               
VALJOB2  GOTO1 AREAD,ADJOB         READ JOB RECORD                              
         BNE   ERROR                                                            
*                                                                               
         MVC   FVMSGNO,=AL2(819)                                                
         L     RE,ADJOB                                                         
         USING ACTRECD,RE                                                       
         TM    ACTRSTAT,ACTSDRFT                                                
         BNZ   ERRORX2A                                                         
         DROP  RE                                                               
*                                                                               
         GOTO1 AGETNAME,ADJOB                                                   
         MVC   BILJOBN,WORK                                                     
*                                                                               
         CLI   ACTION,LIST                                                      
         BNE   VALJOB3                                                          
         MVC   LJOB,FLD                                                         
         B     GOINIT                                                           
*                                                                               
VALJOB3  L     RE,ADJOB                                                         
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
         MVI   FERN,46             CHECK FOR X-JOB                              
*                                                                               
VALJOB4  CLI   0(RE),0                                                          
         BE    ERROR                                                            
         CLI   0(RE),JOBELQ                                                     
         BE    VALJOB5                                                          
         IC    RF,1(,RE)                                                        
         AR    RE,RF                                                            
         B     VALJOB4                                                          
*                                                                               
VALJOB5  TM    (JOBSTA1-JOBELD)(RE),X'10'                                       
         BO    ERROR                                                            
         L     RE,ADJOB                                                         
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
         MVI   FERN,CLOSED         CHECK FOR ACCOUNT NOT CLOSED                 
*                                                                               
VALJOB6  CLI   0(RE),0                                                          
         BE    ERROR                                                            
         CLI   0(RE),RSTELQ                                                     
         BE    VALJOB7                                                          
         IC    RF,1(,RE)                                                        
         AR    RE,RF                                                            
         B     VALJOB6                                                          
*                                                                               
VALJOB7  TM    (RSTSTAT-RSTELD)(RE),X'40'                                       
         BO    ERROR                                                            
         LA    R1,JSAVE                                                         
         BAS   RE,EXTPROF                                                       
         MVC   LJOB,FLD                                                         
         MVC   JOBKEY,KEY                                                       
         BAS   RE,JOBOPT           GET OPTIONS                                  
         EJECT ,                                                                
***********************************************************************         
*  MERGE PROFILES ETC                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING JOBD,R1                                                          
         SPACE 1                                                                
PRFMERGE MVC   JOBINFO,CSAVE       DEFAULT IS CLIENT                            
         LA    R0,2                                                             
         LA    R1,PSAVE                                                         
         LA    RE,JOBINFO                                                       
*                                                                               
PMRGE2   OC    JRECV,JRECV                                                      
         BZ    *+10                                                             
         MVC   JRECV-JOBD(L'JRECV,RE),JRECV                                     
         OC    JCOST,JCOST                                                      
         BZ    *+10                                                             
         MVC   JCOST-JOBD(L'JCOST,RE),JCOST                                     
         OC    JBTYPE,JBTYPE                                                    
         BZ    *+10                                                             
         MVC   JBTYPE-JOBD(L'JBTYPE,RE),JBTYPE                                  
         CLC   JINFO,SPACES                                                     
         BNH   *+10                                                             
         MVC   JINFO-JOBD(L'JINFO,RE),JINFO                                     
         CLI   JUNIT,C' '                                                       
         BNH   *+10                                                             
         MVC   JUNIT-JOBD(L'JUNIT,RE),JUNIT                                     
         CLC   JOFFC,SPACES                                                     
         BNH   *+10                                                             
         MVC   JOFFC-JOBD(L'JOFFC,RE),JOFFC                                     
         LA    R1,JLEN(,R1)                                                     
         BCT   R0,PMRGE2           MERGE IN THE JOB                             
*                                                                               
         LA    R2,BILCLIH                                                       
         ST    R2,FADR                                                          
         LA    R1,JOBINFO                                                       
         MVC   HALF,JOFFC                                                       
*                                                                               
         USING OFFALD,R1                                                        
         L     R1,AOFFBLK                                                       
         MVC   OFFAOFFC,HALF                                                    
         MVI   OFFAACT,OFFAPST                                                  
         GOTO1 VOFFAL                                                           
         BE    PMRG4                                                            
         MVI   FERN,SECLOCK                                                     
         B     ERROR                                                            
         DROP  R1                                                               
*                                                                               
PMRG4    TM    RUNOPT,NEEDGST      ARE WE DOING GST LOGIC ?                     
         BZ    PMRG6               NO                                           
*                                                                               
         USING VTCD,R2                                                          
         L     R2,AVATBUFF                                                      
         XC    VTCACTN(VTCLNQ),VTCACTN    CLEAR VATICAN BUFFER                  
         MVI   VTCACTN,VTCALOOK           LOOK FOR RATES                        
         MVC   VTCLANG,PLANG              USE  PRINT LANGUAGE                   
         MVC   VTCCOMF,ACOMFACS           A(COMFACS)                            
         MVC   VTCCPY,JOBKEY              COMPANY CODE                          
         MVC   VTCOFFC,HALF               OFFICE                                
         MVC   VTCINVD,BILDATE            USE BILLDATE IF THERE                 
         CLI   BILDATE,X'00'              ELSE, USE TODAY                       
         BNE   *+10                                                             
         MVC   VTCINVD,TODAYP                                                   
         MVI   VTCTYPE,C'S'               ALWAYS NEED A TYPE                    
         XC    VTCPRV,VTCPRV                                                    
         GOTO1 VVATICAN,(R2)                                                    
         TM    VTCINDS,VTCINA             IS TAX APPLICABLE ?                   
         BZ    *+8                        YES                                   
         NI    RUNOPT,X'FF'-(NEEDGST+NEEDPST)   NO, TURN OFF OPTION             
*                                                                               
         TM    RUNOPT,NEEDPST                                                   
*        BZ    PMRG6                                                            
         B     PMRG6                                                            
*                                                                               
         CLC   SVPSTCOD,SPACES     IS A PST CODE DEFINED FOR THIS JOB           
         BNH   PMRG5               NO                                           
*                                                                               
         CLC   SVPSTPRV,SPACES     IS A PROVINCE DEFINED FOR THIS JOB           
         BNH   PMRG5               NO                                           
*                                                                               
         MVC   VTCTYPE,SVPSTCOD                                                 
         MVC   VTCPRV,SVPSTPRV                                                  
         GOTO1 VVATICAN,(R2)                                                    
         TM    VTCINDS,VTCINA             IS PST TAX APPLICABLE                 
         BZ    PMRG6                      YES                                   
*                                                                               
PMRG5    NI    RUNOPT,X'FF'-NEEDPST       NO, TURN OFF OPTION THEN              
         DROP  R2                                                               
*                                                                               
PMRG6    TM    ACTINDS,DISPARA     READ PARA RECORD                             
         BZ    MEDCHK                                                           
         MVC   WRKPARA,PARAHEX                                                  
         L     RF,AWRKLGET                                                      
         LA    R1,AIOAREA1                                                      
         TM    ACTINDS,TWOSTAGE    TWO-STAGE ACTIONS CAN NOW GO TO              
         BZ    PMRGE8              OVERLAY IF SAME PARA AS LAST                 
         CLC   WRKEY(8),WRKSAVE                                                 
         BNE   PMRGE8                                                           
         CLC   PARAHEX,LPARA                                                    
         BNE   PMRGE8                                                           
         L     RF,AWRKLGTL                                                      
*                                                                               
PMRGE8   BASR  RE,RF                                                            
         BNE   ERROR                                                            
         C     RF,AWRKLGTL                                                      
         BE    GO                                                               
         B     MEDCHK                                                           
         EJECT ,                                                                
***********************************************************************         
*  EXTRACT PROFILE INFORMATION ETC FROM CLI/PROD/JOB RECORDS          *         
*                                                                     *         
*    RECORD IS ADDRESSED BY AIOAREA,R1 POINTS TO RECEIVING AREA       *         
***********************************************************************         
         SPACE 1                                                                
         USING JOBD,R1              CLEAR AREA                                  
         SPACE 1                                                                
EXTPROF  NTR1                                                                   
         L     RE,AIOAREA                                                       
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
         XC    0(JLEN,R1),0(R1)                                                 
         MVC   JOFFC,SPACES        SET OFFICE TO SPACES                         
*                                                                               
EXTP2    CLI   0(RE),0                                                          
         BE    EXIT                                                             
         CLI   0(RE),PPRELQ                                                     
         BNE   EXTP4                                                            
*                                                                               
         USING PPRELD,RE                                                        
         OC    PPRRECV,PPRRECV                                                  
         BZ    *+10                                                             
         MVC   JRECV,PPRRECV                                                    
         OC    PPRCOST,PPRCOST                                                  
         BZ    *+10                                                             
         MVC   JCOST,PPRCOST                                                    
         OC    PPRBILLP,PPRBILLP                                                
         BZ    *+10                                                             
         MVC   JINFO,PPRBILLP                                                   
         CLC   PPRGAOFF,SPACES                                                  
         BNH   EXTPX                                                            
         MVC   JOFFC,PPRGAOFF                                                   
         MVC   JUNIT,PPRGAOFF                                                   
         B     EXTPX                                                            
*                                                                               
EXTP4    CLI   0(RE),X'22'                                                      
         BNE   EXTPX                                                            
         MVC   ADDR,0(RE)                                                       
*                                                                               
EXTPX    IC    RF,1(,RE)                                                        
         AR    RE,RF                                                            
         B     EXTP2                                                            
         DROP  R1,RE                                                            
         EJECT ,                                                                
***********************************************************************         
*  GET OPTIONS FROM OPTION RECORDS                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING GOBLOCKD,R3                                                      
         SPACE 1                                                                
JOBOPT   NTR1                                                                   
         L     R3,AGOBLOCK                                                      
         MVC   GOADM,VDATAMGR      SET-UP GOBLOCK                               
         MVC   GOSELCUL,JOBKEY                                                  
*        MVC   GOSELCLI,SPACES                                                  
         MVC   GOSELCLI,LCLI                                                    
*        MVC   GOSELPRO,SPACES                                                  
         MVC   GOSELPRO,LPRO                                                    
*        MVC   GOSELJOB,SPACES                                                  
         MVC   GOSELJOB,LJOB                                                    
         MVC   GOACOMP,ADCMP                                                    
         MVC   GOACLI,ADCLI                                                     
         MVC   GOAPRO,ADPRD                                                     
         MVC   GOAJOB,ADJOB                                                     
         MVI   GOWHICH,0                                                        
         MVI   GOANYMED,0                                                       
         MVI   GOSELLEV,0                                                       
         GOTO1 VGETOPT,DMCB,AGOBLOCK                                            
         MVC   DUEDAYS,GODUEDAY     DUE DAYS                                    
         MVC   SVGSTCOD,GOTAXCOD                                                
         MVC   SVPSTCOD,GOPSTCOD                                                
         MVC   SVPSTPRV,GOPSTPRV                                                
*                                                                               
         USING JOBD,R1                                                          
         LA    R1,JSAVE                                                         
         MVC   JBTYPE,GOBILTYP                                                  
         B     EXIT                                                             
         DROP  R1,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  CHECK MEDIA                                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING GOBLOCKD,R3                                                      
         SPACE 1                                                                
MEDCHK   L     R3,AGOBLOCK                                                      
         MVI   LPARA,0             READ MEDIA REC IF WE HAVEN'T                 
         CLC   MEDIA,BILJOB                                                     
         BE    MEDCHK2                                                          
         GOTO1 AREAMED,AIOAREA2                                                 
         BNE   ERROR                                                            
*                                                                               
MEDCHK2  MVC   ANALYSIS,MEDANAL                                                 
         MVC   INCOME,MEDINC                                                    
         OC    GOTBC,GOTBC         TEST FOR A TIME BILLING A/C OVR              
         BZ    *+10                                                             
         MVC   INCOME,GOTBC        YES-USE IT OVER THE MEDIA A/C                
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'INCOME),INCOME                                             
         GOTO1 AREAD,AIOAREA2                                                   
         BNE   ERROR                                                            
*                                                                               
         SR    RF,RF                                                            
         L     RE,AIOAREA                                                       
         MVI   FERN,INVALID                                                     
         AH    RE,DATADISP                                                      
*                                                                               
         USING SPAELD,RE                                                        
MEDCHK4  CLI   0(RE),0                                                          
         BE    MEDCHKX                                                          
         CLI   0(RE),SPAELQ       GET SPECIAL POSTING ELEMENT                   
         BE    MEDCHK8                                                          
*                                                                               
MEDCHK6  IC    RF,1(,RE)                                                        
         AR    RE,RF                                                            
         B     MEDCHK4                                                          
*                                                                               
MEDCHK8  CLI   SPATYPE,SPATANAL     LOOK FOR ANALYSIS POINTER                   
         BNE   MEDCHK6                                                          
         MVC   ANALACCT(L'SPAAULA),SPAAULA                                      
*                                                                               
MEDCHKX  B     GOINIT                                                           
         DROP  R3,RE                                                            
         EJECT ,                                                                
***********************************************************************         
*  READ MEDIA RECORD FOR INCOME ACCOUNT ETC.                          *         
*                                                                     *         
*  I/O AREA IS ADDRESSED BY WORD AT R1                                *         
***********************************************************************         
         SPACE 1                                                                
REAMEDL  L     RF,AREADL           WITH LOCK                                    
         B     READM1                                                           
*                                                                               
REAMED   L     RF,AREAD            WITHOUT LOCK                                 
*                                                                               
READM1   MVC   KEY,SPACES                                                       
         MVI   KEY,X'09'                                                        
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(1),BILJOB                                                  
         BASR  RE,RF                                                            
         BNE   READMERR                                                         
         SR    RF,RF                                                            
         L     RE,AIOAREA                                                       
         MVI   FERN,INVALID                                                     
         AH    RE,DATADISP                                                      
*                                                                               
READM2   CLI   0(RE),0                                                          
         BE    READMERR                                                         
         CLI   0(RE),PMDELQ        X'11' - PRODUCTION MEDIA ELEMENT             
         BE    READM4                                                           
         IC    RF,1(,RE)                                                        
         AR    RE,RF                                                            
         B     READM2                                                           
*                                                                               
         USING PMDELD,RE           MAP  PRODUCTION MEDIA ELEMENT                
READM4   MVC   MEDINC,PMDCOM1                                                   
         MVC   MEDNAME,PMDDESC                                                  
         MVC   MEDBILL,PMDLBILL                                                 
         MVC   MEDRSET,PMDRBILL                                                 
         MVC   MEDIA,PMDCODE                                                    
         MVC   MEDANAL,PMDANAL                                                  
         MVC   ANALACCT,SPACES                                                  
         MVC   ANALACCT(L'PMDANAL),PMDANAL                                      
         CLI   PMDLN,PMDLN2Q       IS THIS A NEW ELEMENT ?                      
         BL    OKXIT               NO                                           
         CLI   PMDCOST,X'41'       YES, ANY COST ANALYSIS ACCOUNT ?             
         BL    OKXIT               NO                                           
         MVC   ANALACCT,PMDCOST    YES, SAVE IT                                 
         B     OKXIT                                                            
         DROP  RE                                                               
*                                                                               
READMERR DS    0H                  ERROR                                        
         LA    R0,BILJOBH                                                       
         ST    R0,FADR                                                          
         MVC   XTRAMESS(5),AC$MED  MEDIA                                        
         MVC   XTRAMESS+6(1),BILJOB                                             
         B     ERRXIT                                                           
         EJECT ,                                                                
***********************************************************************         
*  PREPARE TO GO TO OVERLAY (DISPLAY PARAGRAPH IF REQUIRED)           *         
***********************************************************************         
         SPACE 1                                                                
GOINIT   DS    0H                                                               
         MVC   LACTION,ACTION                                                   
         ZAP   PNET,=P'0'                                                       
         ZAP   PCOM,=P'0'                                                       
         ZAP   PGST,=P'0'                                                       
         ZAP   PPST,=P'0'                                                       
         ZAP   PNON,=P'0'                                                       
         ZAP   PCSD,=P'0'                                                       
         TM    ACTXIND,JOBTOT      DO I NEED JOB TOTALS                         
         BZ    GOINIT1             NO                                           
         CLI   ANYKEY,C'Y'         WAS THERE A KEY CHANGE                       
         BNE   GOINIT1             TABLE IS ALREADY SET-UP                      
         GOTO1 ABLDCHR             BUILD TABLE OF ALLOCATED CHARGES             
         CLI   ACTION,NEWBILL      IF NEWBILL NO NEED TO TOTALS                 
         BE    GOINIT1                                                          
         GOTO1 ABILCHR             GET TOTALS OF ALL PARAGRAPHS                 
*                                                                               
GOINIT1  CLI   SCREEN,0            LOAD SCREEN IF REQUIRED                      
         BE    GOINIT2                                                          
         CLC   SCREEN,LSCRN                                                     
         BE    GOINIT2                                                          
         ICM   R0,14,=X'D9060E'                                                 
         ICM   R0,1,SCREEN                                                      
         GOTO1 VCALLOV,DMCB,BILTABH,(R0)                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,BILMSGH                                                       
         SR    RE,RE                                                            
         LA    RF,BILTABH                                                       
*                                                                               
GOINIT1A OI    6(R1),FVOXMT        RE-TRANSMIT TOP OF SCREEN                    
         IC    RE,0(,R1)                                                        
         BXLE  R1,RE,GOINIT1A                                                   
         MVC   LSCRN,SCREEN                                                     
         MVI   LPARA,0                                                          
         TM    ACTINDS,NEWPARA     IF ITS ADD/INSERT EXIT FOR MORE              
         BZ    GOINIT2                                                          
         GOTO1 ADISTOT             DISPLAY ALLOCATED TOTALS                     
         LA    R0,BLPWRKH                                                       
         ST    R0,FADR                                                          
         MVC   FVMSGNO,=AL2(93)    ENTER PARAGRAPH DETAILS                      
         MVI   FVMTYPE,FVMINFO                                                  
         B     ERRORX2A                                                         
*                                                                               
GOINIT2  TM    ACTINDS,DISPARA     PARAGRAPH DISPLAY REQUIRED                   
         BZ    GO                                                               
*                                                                               
         USING PARAD,R6                                                         
         L     R6,AIOAREA1                                                      
         MVC   BLPWRK,PARAWRK      OPTIONAL W/C                                 
         OC    BLPWRK,SPACES                                                    
         OI    BLPWRKH+6,FVOXMT    TRANSMIT                                     
         MVC   BLPDSC,PARADSC      OPTIONAL DESCRIPTION                         
*        OC    BLPDSC,SPACES                                                    
         OI    BLPDSCH+6,FVOXMT    TRANSMIT                                     
         ZAP   PNET,PARANET                                                     
         ZAP   PCOM,PARACOM                                                     
         ZAP   PGST,PARAGST                                                     
         ZAP   PPST,PARAPST                                                     
         ZAP   PNON,PARANON                                                     
         ZAP   PCSD,PARACSD                                                     
         GOTO1 ADISTOT             DISPLAY PARAGRAPH TOTALS                     
*                                                                               
GOINIT11 GOTO1 ANARRDIS,AIOAREA1   NARRATIVE                                    
         MVC   LPARA,PARAHEX       SAVE PARA NUMBER                             
*                                                                               
         LA    R0,BILACTH          SOME ACTIONS STOP HERE                       
         LA    R2,94               MSG # FOR 'ENTER NEXT ACTION'                
         CLI   ACTION,DID                                                       
         BNE   GOINIT17                                                         
         CLC   PARAHEX,WRKLPARA                                                 
         BE    GOINIT23            DISPLAY OF LAST PARA                         
         B     GOINIT19                                                         
*                                                                               
GOINIT17 TM    ACTINDS,TWOSTAGE                                                 
         BZ    GO                                                               
*                                                                               
GOINIT19 LA    R0,BLPWRKH                                                       
         LA    R2,4                MSG # FOR 'NOW ENTER CHANGES'                
         CLI   ACTION,CHA                                                       
         BE    GOINIT23            CHANGE                                       
         LA    R0,BLPJOBTH                                                      
         OI    BILACTH+6,X'81'                                                  
         LA    R2,95               MSG # FOR 'HIT ENTER TO DELETE'              
         CLI   ACTION,DEL                                                       
         BE    GOINIT23            DELETE                                       
         CLI   ACTION,DID                                                       
         BNE   GOINIT21                                                         
         LA    R2,50               MSG # FOR 'HIT ENTER FOR NEXT'               
         MVC   BILACT(12),=C'DISPLAY,NEXT'                                      
         B     GOINIT23            DISPLAY NOT LAST PARA                        
*                                                                               
GOINIT21 DC    H'0'                UNRECOGNISED ACTION                          
*                                                                               
GOINIT23 ST    R0,FADR             SAVE CURSOR ADDRESS                          
         ST    R2,SAVEMSG#         SAVE MSG NUMBER                              
         GOTO1 APARNOFM,MSG        SET UP MESSAGE                               
         LR    R2,R1                                                            
         MVC   0(9,R2),AC$DSPD     DISPLAYED                                    
         LA    R1,9                                                             
         BAS   RE,FINDFREE                                                      
*                                                                               
         MVI   1(R2),C'-'          -                                            
*                                                                               
         LA    R2,3(,R2)           WHERE TO START THE MESSAGE                   
         LA    R3,MSG+L'MSG        ->   JUST PAST THE MESSAGE                   
         SR    R3,R2               GET  AVAILABLE IN  MSG                       
         GOTO1 ATXTGET,DMCB,('FVMINFO',SAVEMSG#),((R3),(R2)),0,0                
         B     OKEND                                                            
         EJECT ,                                                                
***********************************************************************         
*  HANDLE INTERFACE TO OVERLAYS                                       *         
***********************************************************************         
         SPACE 1                                                                
GO       LA    R0,BILACTH                                                       
         ST    R0,FADR                                                          
         CLI   PHASE,OVPHBASE                                                   
         BNE   GO4                                                              
         CLI   ACTION,EXT          EXTENDING RETENTION DAYS OF WRKER FL         
         BNE   GO2                                                              
         GOTO1 AEXTBIL                                                          
         B     SAVBUF                                                           
*                                                                               
GO2      CLI   ACTION,HLP          SWITCH ACTIONS FOR HELP                      
         BNE   SAVBUF                                                           
         XC    BILACT,BILACT                                                    
         OI    BILACTH+6,FVOXMT    TRANSMIT                                     
*        CLI   SCREEN,X'F9'        CONTINUATION SCREEN                          
*        BE    SAVBUF                                                           
         B     SAVBUF                                                           
*                                                                               
GO4      GOTO1 VCALLOV,DMCB,(PHASE,0),0,0                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   APHASE,0(R1)                                                     
*                                                                               
GOPHASE  MVI   FERN,OK                                                          
         MVC   FVMSGNO,FVFOK                                                    
         MVI   FVMTYPE,C' '                                                     
         GOTO1 APHASE                                                           
         CLI   FERN,OK             ANY  ERRORS ?                                
         BNE   ERROR               YES, PROCESS THE ERROR                       
         CLC   FVMSGNO,FVFOK                                                    
         BE    GOPHASE2            NO,  CONTINUE                                
         CLI   FVMTYPE,FVMINFO     INFORMATIONAL MESSAGES ONLY ?                
         BNE   ERRORX2A            NO,  PROCESS THE ERROR                       
*                                                                               
GOPHASE2 CLI   ACTION,DRA          DRAFT & BILL GO TO PRINT PHASE               
         BE    GOPHASE4            AFTER BILL/UNBILL PHASE                      
         CLI   ACTION,BIL                                                       
         BE    GOPHASE4                                                         
         CLC   FVMSGNO,FVFOK                                                    
         BE    OKEND                                                            
         B     ERRORX2A            NO,  PROCESS THE ERROR                       
*                                                                               
GOPHASE4 CLI   PHASE,OVPHPRT                                                    
         BE    OKEND                                                            
         MVI   PHASE,OVPHPRT       PRINT PHASE                                  
         B     GO                                                               
         EJECT ,                                                                
***********************************************************************         
*  EXTRACT AND PRE-VALIDATE AN INPUT FIELD.                           *         
*                                                                     *         
*  ADDRESS OF FIELD HEADER IS PASSED IN R1. RETURN WITH:-             *         
*                                                                     *         
*              FADR     = A(INPUT FIELD HEADER)                       *         
*              FERN     = MISSING INPUT FIELD IF NO INPUT             *         
*              FNDX     = ZERO                                        *         
*              FLDH     = INPUT FIELD HEADER (FLDH(4) = BINARY VALUE  *         
*                                             FOR NUMERIC FIELD)      *         
*              FLD      = EXTRACTED & SPACE FILLED INPUT FIELD        *         
*                                                                     *         
*  RETURN WITH CC=EQU IF NO INPUT IN FIELD                            *         
***********************************************************************         
         SPACE 1                                                                
FVAL     MVI   FNDX,0                                                           
         MVI   FERN,NOINPUT                                                     
         ST    R1,FADR                                                          
         XC    FLDH,FLDH                                                        
         MVC   FLDH+4(2),4(R1)                                                  
         MVC   FLD,SPACES                                                       
         ZIC   RE,FLDH+5                                                        
         SH    RE,=H'1'                                                         
         BM    FVALX                                                            
         EXMVC RE,FLD,8(R1)                                                     
         MVI   FERN,OK                                                          
         TM    FLDH+4,X'08'                                                     
         BZ    FVALX                                                            
         CLI   FLDH+5,9                                                         
         BNH   FVAL2                                                            
         NI    FLDH+4,X'F7'                                                     
         B     FVALX                                                            
*                                                                               
FVAL2    EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   RE,DUB                                                           
         ST    RE,FLDH                                                          
*                                                                               
FVALX    CLI   FERN,NOINPUT                                                     
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  EXTRACT NAME FROM A RECORD INTO WORK.                              *         
*                                                                     *         
*  RECORD IS ADDRESSED BY WORD AT R1.                                 *         
***********************************************************************         
         SPACE 1                                                                
GETNAME  L     R1,0(,R1)                                                        
         ST    R1,AIOAREA                                                       
         MVC   WORK,SPACES                                                      
         AH    R1,DATADISP                                                      
         SR    RF,RF                                                            
*                                                                               
GETNAME2 CLI   0(R1),0                                                          
         BE    GETNAMEX                                                         
         CLI   0(R1),NAMELQ        X'20' - GENERAL NAME ELEMENT ?               
         BE    GETNAME4                                                         
         IC    RF,1(,R1)                                                        
         AR    R1,RF                                                            
         B     GETNAME2                                                         
*                                                                               
GETNAME4 IC    RF,1(,R1)                                                        
         SH    RF,=H'3'                                                         
         EXMVC RF,WORK,2(R1)                                                    
*                                                                               
GETNAMEX B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  GET WORK CODE DESCRIPTION INTO WORK                                *         
*                                                                     *         
*  ADDRESS OF WORK CODE IS PASSED IN R1.                              *         
*  RETURN WITH CC=NEQ IF NOT FOUND AND FERN IS SET                    *         
***********************************************************************         
         SPACE 1                                                                
GETWC    MVC   WORK,SPACES                                                      
         LA    R2,LWCTAB           R2 = A(SAVED W/C TAB ENTRY)                  
         LA    R0,4                     CONTAINS CODE(CL2)/DESC(CL15)           
*                                                                               
GETWC2   OC    0(2,R2),0(R2)                                                    
         BZ    GETWC4                                                           
         CLC   0(2,R2),0(R1)                                                    
         BE    GETWC9              WE HAVE W/C IN SAVE STORAGE                  
         LA    R2,L'LWCTAB(,R2)                                                 
         BCT   R0,GETWC2                                                        
         LA    R2,LWCTAB                                                        
         XC    0(4*L'LWCTAB,R2),0(R2)                                           
*                                                                               
GETWC4   MVC   KEY,SPACES          BUILD KEY TO READ                            
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(2),PRODUL                                                  
         MVC   KEY+4(2),0(R1)                                                   
         GOTO1 AREAD,AIOAREA2                                                   
         BE    GETWC6                                                           
         TM    DMCB+8,X'10'                                                     
         BZ    ERRXIT                                                           
*                                                                               
GETWC5   MVI   FERN,INVWC                                                       
         B     ERRXIT                                                           
*                                                                               
GETWC6   L     R1,0(,R1)           FIND ANALYSIS ELEMENT AND SAVE               
         AH    R1,DATADISP         CODE/DESC                                    
         SR    RF,RF                                                            
*                                                                               
GETWC7   CLI   0(R1),0                                                          
         BE    GETWC5                                                           
         CLI   0(R1),WCOELQ        X'12' - WORK CODE ELEMENT ?                  
         BE    GETWC8                                                           
         IC    RF,1(,R1)                                                        
         AR    R1,RF                                                            
         B     GETWC7                                                           
*                                                                               
         USING WCOELD,R1           MAP  WORK CODE ELEMENT                       
GETWC8   MVC   0(L'LWCTAB,R2),WCOCODE                                           
*                                                                               
GETWC9   MVC   WORK(15),2(R2)      PASS DESC BACK IN   WORK                     
         SR    R0,R0               CC=EQU FOR OK                                
         B     EXIT                                                             
*                                                                               
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY BILL PARAGRAPH NARRATIVE                                   *         
*                                                                     *         
*  RECORD IS ADDRESSED BY WORD AT R1                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING PARAD,R6                                                         
         SPACE 1                                                                
NARRDIS  L     R6,0(,R1)                                                        
         LA    R6,PARATXT                                                       
         LA    R3,BLPFRSTH                                                      
         LA    R5,BLPFINLH                                                      
         SR    R0,R0               R0 = COUNT OF UNSCAN ENTRIES PENDING         
*                                                                               
NARRDIS2 CLI   0(R6),0             SEARCH FOR COMMENT ELEMENTS                  
         BE    NARRDISE                                                         
         CLI   0(R6),SCMELQ        X'3E' - STANDARD COMMENT ELEMENT             
         BE    NARRDIS4                                                         
*                                                                               
NARRDIS3 ZIC   R1,1(,R6)                                                        
         AR    R6,R1                                                            
         B     NARRDIS2                                                         
*                                                                               
         USING SCMELD,R6           MAP  STANDARD COMMENT ELEMENT                
NARRDIS4 CLI   SCMTYPE,0                                                        
         BNE   NARRDIS6                                                         
         LTR   R0,R0               ANY N=1-6 ENTRIES PENDING ?                  
         BZ    NARRDIS5                                                         
         BAS   RE,NARREQ           IF SO DISPLAY THEM AS A STRING               
         BNZ   EXIT                (NO MORE ROOM)                               
*                                                                               
NARRDIS5 ZIC   RE,SCMLN            THEN DISPLAY STANDARD COMMENT                
         SH    RE,=H'5'                                                         
         MVC   CRD,SPACES                                                       
         EXMVC RE,CRD,SCMNARR                                                   
         BAS   RE,NARRTWA          MOVE TWA IF DIFFERENT & BUMP TWA             
         BNZ   EXIT                                                             
         B     NARRDIS3                                                         
*                                                                               
NARRDIS6 LTR   R0,R0               NON-STANDARD COMMENT - ADD TO UNSCAN         
         BNZ   NARRDIS7            BLOCK                                        
*                                  SUPPORTS  UP   TO   16   N= STRINGS,         
*                                  WE   MUST SUPPORT   ONLY 8  STRINGS.         
         MVC   TEMP,SPACES                                                      
         MVC   TEMPOV,SPACES                                                    
*                                                                               
NARRDIS7 LA    RF,20                                                            
         MR    RE,R0                                                            
         LA    RF,TEMP(RF)                                                      
         MVI   0(RF),C'N'                                                       
         AH    R0,=H'1'            BUMP ENTRY COUNT                             
         LA    RE,SCMNARR          MOVE IN COMMENT NUM LEFT-ALIGNED             
         LA    R1,5                                                             
*                                                                               
NARRDIS8 CLI   0(RE),C' '                                                       
         BH    NARRDIS9                                                         
         LA    RE,1(,RE)                                                        
         BCT   R1,NARRDIS8                                                      
*                                                                               
NARRDIS9 EXMVC R1,10(RF),0(RE)                                                  
         B     NARRDIS3                                                         
         DROP  R6                                                               
*                                                                               
NARRDISE LTR   R0,R0               AT END                                       
         BZ    NARRDISF                                                         
         BAS   RE,NARREQ           DISPLAY ANY N=1-6 ENTRIES PENDING            
         BNZ   EXIT                                                             
*                                                                               
NARRDISF MVC   CRD,SPACES          AND CLEAR REMAINING LINES                    
*                                                                               
NARRDISG BAS   RE,NARRTWA                                                       
         BZ    NARRDISG                                                         
         B     EXIT                                                             
*                                                                               
NARREQ   NTR1                      ROUTINE TO UNSCAN (R0), ENTRIES FROM         
         L     RF,VUNSCAN          TEMP INTO TWA FIELD(S)-R3=A(1ST HDR)         
         GOTO1 ,DMCB,((R0),TEMP),CRDH                                           
         SR    R0,R0                                                            
*                                                                               
NARREQ2  MVC   CRDH(1),0(R3)                                                    
         MVC   CRD,SPACES                                                       
         BASR  RE,RF                                                            
         BAS   RE,NARRTWA                                                       
         BNZ   EXIT                                                             
         CLI   0(R1),0             ANY MORE TO DISPLAY                          
         BNE   NARREQ2                                                          
         XIT1  REGS=(R0,R3)        PASS BACK A(NEXT HDR)                        
*                                                                               
NARRTWA  ZIC   R2,0(,R3)           ROUTINE TO MOVE CONTENTS OF CRD TO           
         SH    R2,=H'9'            FLD AT 8(R3) IF DIFFERENT, TO                
*        EX    R2,NARROC           TRANSMIT,AND BUMP R3 TO NEXT UNPROT          
         EX    R2,NARRCLC          HDR                                          
         BE    NARRTWA2            CC = NEQ IF WE REACH END OF NARRATVE         
         EX    R2,NARRMVC               AREA (R5 IS A OF LAST HDR)              
         OI    6(R3),FVOXMT        TRANSMIT                                     
*                                                                               
NARRTWA2 IC    R2,0(,R3)                                                        
         LA    R3,0(R2,R3)                                                      
         CR    R3,R5                                                            
         BNH   NARRTWA4                                                         
         LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
NARRTWA4 TM    1(R3),X'20'                                                      
         BZR   RE                                                               
         B     NARRTWA2                                                         
*                                                                               
*NARROC  OC    8(0,R3),SPACES                                                   
NARRCLC  CLC   8(0,R3),CRD                                                      
NARRMVC  MVC   8(0,R3),CRD                                                      
         EJECT ,                                                                
***********************************************************************         
* ACCOUNT FILE I/O EXECUTIVE.                                         *         
*                                                                     *         
* I/O IS EXECUTED ON KEY INTO I/O AREA ADDRESSED BY R1. COMMAND IS    *         
* PASSED IN DMBYTE FOLLOWS:-                                          *         
*                                                                     *         
*               BITS 0-3  = COMMAND NUMBER (1-5) (SEE IOCMNDST)       *         
*                    5 ON = PASS BACK DELETED RECORDS                 *         
*                    6 ON = READ KEY WITH LOCK                        *         
*                    7 ON = SAVE KEY IN KEYSAVE BEFORE I/O            *         
*                                                                     *         
*        NOTE ACCIO HAS BEEN REMOVED FROM ADDRESSABLE STORAGE         *         
*                                                                     *         
* RETURN WITH CC=NEQ ON I/O ERROR WITH FERN SET TO ERROR MESSAGE NUM. *         
***********************************************************************         
         SPACE 1                                                                
ACADD    MVI   DMBYTE,X'10'                                                     
         B     CALLACIO                                                         
*                                                                               
ACRDHI   MVI   DMBYTE,X'25'                                                     
         B     CALLACIO                                                         
*                                                                               
ACRDHIL  MVI   DMBYTE,X'27'                                                     
         B     CALLACIO                                                         
*                                                                               
ACREAD   MVI   DMBYTE,X'34'                                                     
         B     CALLACIO                                                         
*                                                                               
ACREADL  MVI   DMBYTE,X'36'                                                     
         B     CALLACIO                                                         
*                                                                               
ACSEQ    MVI   DMBYTE,X'40'                                                     
         B     CALLACIO                                                         
*                                                                               
ACSEQL   MVI   DMBYTE,X'42'                                                     
         B     CALLACIO                                                         
*                                                                               
ACWRITE  MVI   DMBYTE,X'50'                                                     
*                                                                               
CALLACIO L     RF,0(,R1)           SAVE A(IO AREA TO USE)                       
         ST    RF,DMCB                                                          
         GOTO1 =A(ACCIO),DMCB,,RR=RELO                                          
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
* WORKER FILE I/O EXECUTIVE                                           *         
*                                                                     *         
* WORKER ID IS PASSED IN WRKEY, PARA NUMBER WHERE RELEVANT IN WRKPARA *         
* ADDRESS OF I/O AREA IS ADDRESSED BY R1                              *         
* ACTION IS PASSED IN WRKBYTE WITH ACCIO EXCEPT THAT                  *         
* ALL 8 BITS ARE USED FOR ACTION NUMBER (SEE WRKCMNDS)                *         
*                                                                     *         
* RETURN WITH CC=NEQ ON I/O ERROR AND FERN SET                        *         
* WRKSTAT=INUSE IF BUFFER IS IN USE, OTHERWISE WRKSAVE IS CLEARED     *         
* WRKLPARA CONTAINS PARA NUMBER OF LAST PARA FOR THIS ID              *         
* AWRKIO CONTAINS ADDRESS OF I/O AREA USED                            *         
***********************************************************************         
         SPACE 1                                                                
WRKBUF   MVI   WRKBYTE,X'01'                                                    
         B     WRKIO                                                            
*                                                                               
WRKBUR   MVI   WRKBYTE,X'02'                                                    
         B     WRKIO                                                            
*                                                                               
WRKBUS   MVI   WRKBYTE,X'03'                                                    
         B     WRKIO                                                            
*                                                                               
WRKIND   MVI   WRKBYTE,X'04'                                                    
         B     WRKIO                                                            
*                                                                               
WRKOPE   MVI   WRKBYTE,X'05'                                                    
         B     WRKIO                                                            
*                                                                               
WRKADD   MVI   WRKBYTE,X'06'                                                    
         B     WRKIO                                                            
*                                                                               
WRKCLO   MVI   WRKBYTE,X'07'                                                    
         B     WRKIO                                                            
*                                                                               
WRKDEL   MVI   WRKBYTE,X'08'                                                    
         B     WRKIO                                                            
*                                                                               
WRKSEQ   MVI   WRKBYTE,X'09'                                                    
         B     WRKIO                                                            
*                                                                               
WRKLADD  MVI   WRKBYTE,X'0A'                                                    
         B     WRKIO                                                            
*                                                                               
WRKLGET  MVI   WRKBYTE,X'0B'                                                    
         B     WRKIO                                                            
*                                                                               
WRKLGTL  MVI   WRKBYTE,X'0C'                                                    
         B     WRKIO                                                            
*                                                                               
WRKLPUT  MVI   WRKBYTE,X'0D'                                                    
         B     WRKIO                                                            
*                                                                               
WRKLDEL  MVI   WRKBYTE,X'0E'                                                    
         B     WRKIO                                                            
*                                                                               
WRKLCLO  MVI   WRKBYTE,X'0F'                                                    
         B     WRKIO                                                            
*                                                                               
WRKRET   MVI   WRKBYTE,X'10'                                                    
         B     WRKIO                                                            
*                                                                               
WRKRAN   MVI   WRKBYTE,X'11'                                                    
*                                                                               
WRKIO    L     RF,0(,R1)           SAVE A(IO AREA TO USE)                       
         ST    RF,DMCB                                                          
         GOTO1 =A(NWRKIO),DMCB,,RR=RELO                                         
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
* FIND A WORKER FILE FOR A BILL                                       *         
*                                                                     *         
* BILL NUMBER IN WORKER ID FORMAT IS PASSED IN WRKEY                  *         
* ADDRESS OF I/O AREA TO CONTAIN HEADER REC IF FOUND IS ADDRESSED BY R1         
*                                                                     *         
* RETURN WITH CC=NEQ IF NOT FOUND AND FERN SET                        *         
***********************************************************************         
         SPACE 1                                                                
FINDBIL  GOTO1 AWRKBUF             CLEAR BUFFER                                 
         MVC   DUB,WRKEY                                                        
         XC    WRKEY,WRKEY                                                      
*                                                                               
FINDBIL1 GOTO1 AWRKIND             INDEX READ                                   
         BE    FINDBIL4                                                         
         MVC   WRKEY(8),DUB                                                     
         B     ERRXIT                                                           
*                                                                               
FINDBIL4 CLC   DUB,WRKEY                                                        
         BNE   FINDBIL1                                                         
*                                                                               
         MVI   WRKPARA,0           GET HDR REC (PARA # 0)                       
         L     RF,AWRKLGTL                                                      
         CLI   ACTION,BIL                                                       
         BE    FINDBIL6                                                         
         CLI   ACTION,UNB                                                       
         BE    FINDBIL6                                                         
         L     RF,AWRKLGET                                                      
*                                                                               
FINDBIL6 BASR  RE,RF               LOCK IF BILL/UNBILL                          
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  SET UP A WORKER ID FOR A BILL IN WRKEY USING 6 DIGIT BILL #        *         
*  IN BILNUM                                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING UKRECD,RF                                                        
         SPACE 1                                                                
WRKID    LA    RF,WRKEY                                                         
         MVC   UKUSRID,TWAUSRID                                                 
         MVI   UKSYSPRG,C'B'                                                    
         EDIT  BILNUMP,(6,TEMP),FILL=0                                          
         MVC   UKSYSPRG+1(3),TEMP                                               
         PACK  DUB(2),TEMP+3(3)                                                 
         MVC   UKDAY,DUB                                                        
         MVC   UKCLASS,TEMP+5                                                   
         B     EXIT                                                             
         DROP  RF                                                               
         EJECT ,                                                                
***********************************************************************         
*  SET UP DISPLAY IN FORM 'PARAGRAPH N OF M'                          *         
*                                                                     *         
*  ON INPUT:                                                          *         
*    R1       = ADDRESS OF DISPLAY AREA                               *         
*    PARAHEX  = N                                                     *         
*    WRKLPARA = M                                                     *         
*                                                                     *         
*  ON OUTPUT:                                                         *         
*    R1       = ADDRESS OF NEXT AVAILABLE POS IN DISPLAY AREA         *         
***********************************************************************         
         SPACE 1                                                                
PARNOFM  ST    R2,SAVER2           SAVE R2                                      
         LR    R2,R1               USE  R2                                      
         MVC   0(10,R2),AC$PRGRP                                                
         LA    R1,10                                                            
         BAS   RE,FINDFREE                                                      
         LA    R2,1(,R2)                                                        
*                                                                               
         ZIC   R0,PARAHEX          N                                            
         BAS   RE,PARNOFM2                                                      
*                                                                               
         TM    SAVPSTAT,FOOTLINE                                                
         BNO   PARNOFM1                                                         
         MVC   0(7,R2),AC$PFOOT    (FOOT)                                       
         LA    R1,7                                                             
         BAS   RE,FINDFREE                                                      
         LA    R2,1(,R2)                                                        
*                                                                               
PARNOFM1 MVC   0(3,R2),AC$OF       OF                                           
         LA    R1,3                                                             
         BAS   RE,FINDFREE                                                      
         LA    R2,1(,R2)                                                        
*                                                                               
         ZIC   R0,WRKLPARA         M                                            
         BAS   RE,PARNOFM2                                                      
*                                                                               
         LR    R1,R2                                                            
         L     R2,SAVER2           RESTORE R2                                   
         XIT1  REGS=(R1)                                                        
*                                                                               
PARNOFM2 EDIT  (R0),(2,0(R2)),ALIGN=LEFT                                        
         AR    R2,R0                                                            
         LA    R2,1(,R2)                                                        
         BSM   0,RE                                                             
         EJECT ,                                                                
***********************************************************************         
*  READ LEDGER RECORD FOR BILL NUMBER                                 *         
*                                                                     *         
*  I/O AREA IS ADDRESSED BY WORD AT R1                                *         
***********************************************************************         
         SPACE 1                                                                
REALEDL  L     RF,AREADL                                                        
         B     READL1                                                           
*                                                                               
REALED   L     RF,AREAD                                                         
*                                                                               
READL1   MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),PRODUL                                                  
         BASR  RE,RF                                                            
         BNE   READLERR                                                         
         SR    RF,RF                                                            
         L     RE,AIOAREA                                                       
         MVI   FERN,INVALID                                                     
         AH    RE,DATADISP                                                      
*                                                                               
READL2   CLI   0(RE),0                                                          
         BE    READLERR                                                         
         CLI   0(RE),PMDELQ        X'11' - PRODUCTION MEDIA ELEMENT             
         BE    READL4                                                           
         IC    RF,1(,RE)                                                        
         AR    RE,RF                                                            
         B     READL2                                                           
*                                                                               
         USING PMDELD,RE           MAP  PRODUCTION MEDIA ELEMENT                
READL4   MVC   LEDGBILL,PMDLBILL                                                
         MVC   LEDGRSET,PMDRBILL                                                
         B     OKXIT                                                            
         DROP  RE                                                               
*                                                                               
READLERR DS    0H                  ERROR                                        
         LA    R0,BILJOBH                                                       
         ST    R0,FADR                                                          
         MVC   XTRAMESS(6),AC$LGRC LEDGER                                       
         MVC   XTRAMESS+6(1),PRODUL+1                                           
         B     ERRXIT                                                           
         EJECT ,                                                                
***********************************************************************         
*  EXTEND RETENTION DAYS VALUE OF WORKER FILE                         *         
***********************************************************************         
         SPACE 1                                                                
EXTBIL   GOTO1 AWRKBUF                                                          
         GOTO1 AWRKID                                                           
         GOTO1 AWRKIND,AIOAREA1                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIOAREA1                                                      
         XC    0(4,R6),0(R6)       SET RECORD NUMBER TO ZERO                    
         LA    R6,28(,R6)                                                       
*                                                                               
         USING WKRECD,R6                                                        
         GOTO1 AWRKRAN,AIOAREA1    AND DO RANDOM READ TO GET IT                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING UKRECD,RF                                                        
         LA    RF,WRKEY                                                         
         OI    UKFLAG,X'10'        SETTING RETENTION DAYS BIT                   
         ZIC   RE,EXTND                                                         
         SR    R0,R0                                                            
         ICM   R0,3,WKRETN         ADD NEW DAYS TO PREVIOUS                     
         AR    RE,R0                                                            
         STCM  RE,3,WKRETN                                                      
         GOTO1 AWRKRET,AIOAREA1                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  RF                                                               
*                                                                               
         EDIT  (B1,EXTND),(2,LWSWORK)                                           
*                                  BILL EXPIRATION EXTENDED BY &1 DAYS          
         GOTO1 ATXTGET,DMCB,('FVMINFO',192),(L'MSG,MSG),(2,LWSWORK),0           
         MVI   FERN,SPECIAL                                                     
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO DECODE A WORKCODE LINE                                  *         
*    FORMAT IS: AA=10N,AB,AC=10C,AC=20N,BB=N,DD=C                     *         
*            OR AA=$10.20,AB=$9.75C                                   *         
*                                                                     *         
*    PARM 1    BYTE   0     LENGTH OF INPUT DATA FIELD                *         
*              BYTE 1-3     A(INPUT DATA FIELD)                       *         
***********************************************************************         
         SPACE 1                                                                
WKDCODE  DS    0H                                                               
         LA    R2,BLPWRKH                                                       
         ST    R2,FADR                                                          
*                                                                               
         ZIC   R3,0(,R1)           GET  LENGTH                                  
         L     R1,0(,R1)           GET  A(DATA)                                 
         MVC   WORK,SPACES                                                      
         MVC   WORK1,SPACES                                                     
         XC    WORK(8),WORK                                                     
         BCTR  R3,0                                                             
         EXMVC R3,WORK+8,0(R1)     DATA TO   WORK+8                             
         LA    R2,WORK+8(R3)       R2   TO   END  OF   FIELD                    
         LA    R3,1(,R3)                                                        
*                                                                               
WKDCV2   CLI   0(R2),C' '          FIND LAST NON-BLANK                          
         BNE   WKDCV3                                                           
         BCTR  R2,0                                                             
         BCT   R3,WKDCV2           GET  REAL LENGTH                             
*                                                                               
WKDCV3   STC   R3,WORK+5           FOR  DUMMY     HEADER                        
*                                                                               
         MVI   WKCNT,0                                                          
         CLI   WORK+5,1                                                         
         BNE   WKDCV4                                                           
         CLC   WORK+8(1),WCCOMMA   ONE  COMMA     IS   CONTINUATION             
         BE    WKDCXIT                                                          
*                                                                               
WKDCV4   LA    R0,SCANRHSL                                                      
         LA    RF,WKCMX            MAXIMUM   NUM  OF   WORKCODES                
         MVI   DMCB+8,COMMA        CHANGE    C',='     DELIMITERS               
         MVI   DMCB+9,EQUALSGN     BASED     ON        PLANG                    
         MVC   DMCB+10(1),WCCOMMA                                               
         MVI   DMCB+11,EQUALSGN                                                 
         GOTO1 VSCANNER,DMCB,((R0),WORK),((RF),SCANBLK)                         
         MVC   WKCNT,4(R1)                                                      
         MVI   FERN,INVWC          INVALID   WORK CODE                          
         CLI   WKCNT,0             ANY  WORK CODES ?                            
         BE    WKDCERR             NO,  ERROR                                   
         MVI   FNDX,1              1ST  WORK CODE                               
         LA    R3,SCANBLK                                                       
         LA    R6,WKCAREA                                                       
*                                                                               
         USING WKCD,R6                                                          
WKDCV5   MVC   WKCSV,SPACES                                                     
*                                  DEFAULT   IS   100.00    PERCENT             
         OI    WKCTYPE,X'80'+X'40'+X'20'                                        
         ZAP   WKCAMNT,=P'10000'                                                
         XC    MANAMT,MANAMT                                                    
         MVI   FERN,INVWC                                                       
         ZIC   RE,0(,R3)           LENGTH    OF   WORK CODE                     
         CH    RE,=H'2'                                                         
         BNH   WKDCV8              MUST BE   A    WORK CODE                     
         CH    RE,=H'3'                                                         
         BH    WKDCERR                                                          
         LA    R5,PNET                                                          
         CLC   12(3,R3),=C'NET'                                                 
         BE    WKDCV6                                                           
         LA    R5,PCOM                                                          
         CLC   12(3,R3),=C'COM'                                                 
         BE    WKDCV6                                                           
         LA    R5,PCSD                                                          
         CLC   12(3,R3),=C'DIS'                                                 
         BE    WKDCV6                                                           
         LA    R5,PGST                                                          
         CLC   12(3,R3),=C'GST'                                                 
         BE    WKDCV6                                                           
         LA    R5,PPST                                                          
         CLC   12(3,R3),=C'PST'                                                 
         BNE   WKDCERR                                                          
*                                                                               
WKDCV6   ST    R5,MANAMT           SAVE ADDR OF   AMOUNT    FIELD               
         TM    SAVHSTAT,MANBILL    ALREADY   MANUAL ?                           
         BO    WKDCV7              YES, SKIP                                    
         CLI   ACTION,NEWBILL      NOT  YET  MANUAL                             
         BNE   WKDCERR             AND  NOT  1ST  PARAGRAPH                     
         OI    SAVHSTAT,MANBILL    SAY  MANUAL                                  
         MVI   MANB,C'Y'                                                        
*                                                                               
WKDCV7   MVI   WKCTYPE,0                                                        
         ZAP   WKCAMNT,=P'0'                                                    
         LA    R2,22(,R3)          ->   RIGHT     SIDE                          
         B     WKDCV9A             VALIDATE  AMOUNT                             
*                                                                               
WKDCV8   BCTR  RE,0                                                             
         EXMVC RE,WKCSV,12(R3)     SAVE WORK CODE                               
         CLC   WKCSV,=C'99'                                                     
         BE    WKDCERR                                                          
         TM    SAVHSTAT,MANBILL    MANUAL    BILL ?                             
         BO    WKDCERR             YES, ERROR                                   
         GOTO1 AGETWC,WKCSV        VALIDATE  WORK CODE                          
         BNE   WKDCERR             INVALID,  ERROR     EXIT                     
*                                                                               
         LA    R2,BILCLIH                                                       
         ST    R2,FADR                                                          
*                                                                               
         USING JOBD,R1                                                          
         LA    R1,JOBINFO                                                       
         MVI   FERN,INVBTYPE       INVALID   BILLING   TYPE                     
*                                  INPUT     BY   WORK CODE -                   
         CLI   JBTYPE,C'C'         BILL TYPE =    CLIENT ?                      
         BNE   WKDCERR             NO,  ERROR                                   
*                                                                               
         MVI   FERN,INVWC                                                       
         CLI   1(R3),0                                                          
         BE    WKDCV30             NO   PERCENT   SPECIFIED                     
         LA    R2,22(,R3)          OR   (N)  OR   (C)                           
*                                                                               
         CLI   0(R2),C'N'                                                       
         BE    WKDCV15N                                                         
         CLI   0(R2),C'C'                                                       
         BE    WKDCV15C                                                         
         CLC   0(1,R2),CUSIGN      SKIP CURRENCY  SIGN                          
         BE    WKDCV9                                                           
         CLC   0(1,R2),PCSIGN      SKIP PRINT     CURRENCY  SIGN                
         BNE   WKDCV9A                                                          
*                                                                               
WKDCV9   NI    WKCTYPE,X'FF'-X'20' TURN-OFF  PERCENT   BIT                      
         LA    R2,1(,R2)                                                        
*                                                                               
WKDCV9A  LA    RE,WRKAMT           ->   AMOUNT    WORK AREA                     
         SR    RF,RF               FIND LEN  OF   AMOUNT    FIELD               
*                                                                               
WKDCV10  CLI   0(R2),DECPOINT      DECIMAL   POINT                              
         BE    WKDCV10B                                                         
         CLI   0(R2),COMMA         COMMA                                        
         BE    WKDCV10B                                                         
         CLI   0(R2),C'-'                                                       
         BE    WKDCV12                                                          
         CLC   0(1,R2),CUSIGN      SKIP CURRENCY  SIGN                          
         BE    WKDCV10A                                                         
         CLC   0(1,R2),PCSIGN      SKIP PRINT     CURRENCY  SIGN                
         BNE   WKDCV11                                                          
*                                                                               
WKDCV10A NI    WKCTYPE,X'FF'-X'20' TURN-OFF  PERCENT   BIT                      
         B     WKDCV12B                                                         
*                                                                               
WKDCV10B MVI   0(RE),DECPOINT      DEFAULT   TO   DECIMAL   POINT               
         CLI   CULANG,LANGEUS      CURRENT   LANGUAGE  ENGLISH ?                
         BNH   WKDCV12A            YES, DONE                                    
         MVI   0(RE),COMMA         NO,  USE  COMMA                              
         B     WKDCV12A                                                         
*                                                                               
WKDCV11  CLI   0(R2),C'0'          CHECK     PERCENT   IS   NUMERIC             
         BL    WKDCV12C                                                         
         CLI   0(R2),C'9'                                                       
         BH    WKDCV12C                                                         
*                                                                               
WKDCV12  MVC   0(1,RE),0(R2)                                                    
*                                                                               
WKDCV12A LA    RE,1(,RE)                                                        
         LA    RF,1(,RF)                                                        
*                                                                               
WKDCV12B LA    R2,1(,R2)           NEXT CHARACTER IN   STRING                   
         B     WKDCV10                                                          
*                                                                               
WKDCV12C CLI   0(R2),X'40'         CHARACTER BLANK ?                            
         BE    WKDCV13             YES, CONTINUE                                
         CLI   0(R3),2             WORK CODE SPECIFIED ?                        
         BNE   WKDCERR1            NO,  INVALID   WORK CODE AMOUNT              
*                                                                               
WKDCV13  LTR   RF,RF               ANY  AMOUNT    INPUT ?                       
         BZ    WKDCERR             NO,  ERROR                                   
         GOTO1 VCASHVAL,DMCB,WRKAMT,(RF)                                        
         CLI   0(R1),X'FF'                                                      
         BE    WKDCERR1                                                         
         L     R1,DMCB+4                                                        
         CVD   R1,DUB                                                           
         ZAP   WKCAMNT,DUB         AMOUNT    TO   TABLE                         
         OC    MANAMT,MANAMT       ADDR OF   MANUAL    AMOUNT                   
         BZ    WKDCV13D                                                         
         L     R5,MANAMT                                                        
         CP    0(PLAMTLNQ,R5),=P'0'                                             
         BNE   WKDCERR1            ALREADY   HAVE AMOUNT                        
*                                  SAVE MANUAL    AMOUNT                        
         ZAP   0(PLAMTLNQ,R5),WKCAMNT                                           
         B     WKDCV30                                                          
*                                                                               
WKDCV13D TM    WKCTYPE,X'20'       IS   IT   A    PERCENT ?                     
         BZ    WKDCV14             NO,  SKIP                                    
         CP    WKCAMNT,=P'10000'   YES, >    100 ?                              
         BH    WKDCERR1            YES, ERROR                                   
*                                                                               
WKDCV14  CLI   0(R2),C'N'                                                       
         BE    WKDCV15N                                                         
         CLI   0(R2),C'C'                                                       
         BE    WKDCV15C                                                         
         CLI   0(R2),C' '                                                       
         BE    WKDCV30                                                          
         B     WKDCERR1                                                         
*                                                                               
WKDCV15C NI    WKCTYPE,X'FF'-X'40' TURN-OFF: NON  COMM                          
         B     WKDCV30                                                          
*                                                                               
WKDCV15N NI    WKCTYPE,X'FF'-X'80' TURN-OFF: COMMISSIONABLE                     
*                                                                               
WKDCV30  LA    R3,22+SCANRHSL(,R3) NEXT SCAN ENTRY                              
         LA    R6,WKCLEN(,R6)                                                   
         ZIC   R1,FNDX                                                          
         LA    R1,1(,R1)                                                        
         STC   R1,FNDX                                                          
         CLC   FNDX,WKCNT          LAST ENTRY ?                                 
         BL    WKDCV5              NO,  TEST NEXT WORK CODE                     
         BH    WKDCXIT             ALREADY   PROCESSED LAST,     OKAY           
         OC    0(2,R3),0(R3)       IS   LAST JUST A    ',' ?                    
         BNZ   WKDCV5              YES, OKAY                                    
         ZIC   R1,WKCNT                                                         
         SH    R1,=H'1'            REDUCE    COUNT                              
         BNP   WKDCERR                                                          
         STC   R1,WKCNT                                                         
*                                                                               
WKDCXIT  MVI   FERN,OK                                                          
         B     EXIT                                                             
         DROP  R1,R6                                                            
*                                                                               
WKDCERR  GOTO1  AERRORX                                                         
*                                                                               
*                                  INVALID   WORK CODE AMOUNT                   
WKDCERR1 MVC    FVMSGNO,=AL2(INVWCAMT)                                          
         MVC    XTRAMESS(1),FNDX   GET  WORK CODE NUMBER                        
         OI     XTRAMESS,C'0'                                                   
         CLI    FNDX,10            WORK CODE NUM  >    9 ?                      
         BL     WKDCERR2                                                        
         ZIC    RE,FNDX                                                         
         CVD    RE,DUB                                                          
         UNPK   XTRAMESS(2),DUB+6(2)                                            
         OI     XTRAMESS+1,X'F0'                                                
*                                                                               
WKDCERR2 GOTO1  AERRORX2                                                        
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO BUILD TABLE OF ALLOCATED CHARGES                        *         
***********************************************************************         
         SPACE 1                                                                
         USING CHRD,R5                                                          
         SPACE 1                                                                
BLDCHR   DS    0H                                                               
         MVI   SKREVSW,C'N'                                                     
         MVI   EPREVSW,C'N'                                                     
         ZAP   TOTNET,=P'0'        JOB TOTAL DEBITS                             
         ZAP   TOTBIL,=P'0'        JOB TOTAL BILLED                             
         ZAP   TOTALL,=P'0'        JOB TOTAL ALLOCATED                          
         ZAP   TOTCOM,=P'0'        JOB TOTAL COMMISSION                         
*                                                                               
         LA    R5,CHRTOT           INITIALIZE TOTAL ACCUMS                      
         LA    R1,CHRANET                                                       
         LA    RF,CHRBKCNT                                                      
*                                                                               
BLDCH1   ZAP   0(PLAMTLNQ,R1),=P'0'                                             
         LA    R1,PLAMTLNQ(,R1)                                                 
         BCT   RF,BLDCH1                                                        
*                                                                               
         USING TWA1D,RF                                                         
         L     RF,ATWA1                                                         
         LA    R5,CHARGES                                                       
         XC    CHRWK,CHRWK                                                      
         XC    CHRCODE,CHRCODE     GST CODE                                     
         XC    CHRPCODE,CHRPCODE   PST CODE                                     
         XC    CHRRATE,CHRRATE     GST RATE                                     
         XC    CHRINDS,CHRINDS     GST RATE DECIMALS                            
         XC    CHRPRATE,CHRPRATE   PST RATE                                     
         XC    CHRPINDS,CHRPINDS   PST RATE DECIMALS                            
         XC    CHRDATE,CHRDATE                                                  
         MVC   CHRACCT,SPACES      GST ACCOUNT                                  
         MVC   CHRPACCT,SPACES     PST ACCOUNT                                  
*                                                                               
         USING SVTABD,R2                                                        
         LA    R0,VATNUM                                                        
         LA    R2,SVTABLE                                                       
*                                                                               
BLDCH2   XC    SVTYPE(SVDATAL),SVTYPE                                           
         MVC   SVBK(SVBKLNQ),ZEROS                                              
         LA    R2,SVTABLNQ(,R2)                                                 
         BCT   R0,BLDCH2                                                        
         DROP  R2,RF                                                            
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),JOBKEY      READ JOB RECORD                              
         SR    R0,R0               COUNT ENTRIES                                
         GOTO1 AREAD,AIOAREA3                                                   
         BE    *+6                                                              
         DC    H'0'                CAN'T READ JOB RECORD                        
*                                                                               
BLDCH4   GOTO1 ASEQ,AIOAREA3                                                    
*                                                                               
         USING TRNRECD,R2          MAP  TRANSACTION RECORD                      
         L     R2,AIOAREA3                                                      
         CLC   JOBKEY,TRNKCULA     SAME ACCOUNT ?                               
         BNE   BLDCH20             END OF JOB RECORDS                           
         LA    R3,ACCORFST(,R2)    ->   1ST ELEMENT                             
         CLI   0(R3),TRNELQ        X'44' - TRANSACTION ELEMENT ?                
         BNE   BLDCH4              ONLY WANT TRANSACTIONS                       
*                                                                               
         USING TRNELD,R3           MAP  TRANSACTION ELEMENT                     
         CLC   TRNANAL,=C'99'      ELIMINATE BILLING                            
         BE    BLDCH4                                                           
         CLC   TRNANAL,=C'**'      AND OTHER FUNNIES                            
         BE    BLDCH4                                                           
         CLC   TRNANAL,SPACES                                                   
         BE    BLDCH4                                                           
         TM    TRNSTAT,TRNSDR      DEBIT ?                                      
         BZ    BLDCH4              ONLY DEBITS                                  
*                                                                               
         TM    TRNRSTAT,TRNSDRFT                                                
         BO    BLDCH4              NO DRAFTS                                    
         TM    TRNRSTAT,TRNSREVS                                                
         BO    BLDCH4              NO REVERSALS                                 
         DROP  R2                                                               
*                                                                               
         ZAP   PNET,=P'0'           INITIALIZE ACCUMS                           
         ZAP   PNON,=P'0'                                                       
         ZAP   PCSD,=P'0'                                                       
         XC    PHOURS,PHOURS                                                    
*                                                                               
         GOTO1 VPRORATA,DMCB,(X'C0',(R2)),0,ACOMFACS,0,APROBLK,APROLST          
*                                                                               
         USING PRORATAD,R2                                                      
         L     R2,APROBLK                                                       
*                                                                               
         AP    TOTNET,PA$NET       GET TRANSACTION NET                          
         SP    TOTNET,PA$WOFAM     SUBTRACT W/O'S  AND TRANSFERS                
         SP    TOTNET,PA$XFRAM                                                  
*                                                                               
         AP    TOTBIL,PA$NETBL     ADD TO TOTAL BILLED                          
         AP    TOTALL,PP$AALLO     ADD TO TOTAL ALLOCATED                       
         AP    TOTCOM,PP$ACOMM     ADD TO TOTAL ALLOCATED COMMISSION            
         AP    PCSD,PP$ADSCB       ADD TO ALLOCATED DISCOUNT                    
*                                                                               
         ZAP   DUB,PP$HRSB         ADD TO ALLOCATED HOURS                       
         CVB   R1,DUB                                                           
         A     R1,PHOURS                                                        
         ST    R1,PHOURS                                                        
*                                                                               
         LA    R1,PNET             COMMISSIONABLE                               
         TM    TRNSTAT,X'01'                                                    
         BZ    *+8                                                              
         LA    R1,PNON             OR NON                                       
         AP    0(PLAMTLNQ,R1),PP$AALLO                                          
         AP    0(PLAMTLNQ,R1),PP$ADSCB                                          
*                                                                               
         CP    PNET,=P'0'                                                       
         BNE   BLDCH6                                                           
         CP    PNON,=P'0'                                                       
         BNE   BLDCH6                                                           
         OC    PHOURS,PHOURS                                                    
         BZ    BLDCH4              NOTHING GOOD TO ADD                          
         DROP  R2                                                               
*                                                                               
         USING TRNRECD,R2                                                       
BLDCH6   L     R2,AIOAREA3                                                      
         CLI   TRNTYPE,47          ESTIMATED PRODUCTION                         
         BNE   *+8                                                              
         MVI   EPREVSW,C'Y'        NEEDS TO BE REVERSED                         
         CLI   TRNTYPE,49          TIMESHEETS                                   
         BNE   *+8                                                              
         MVI   SKREVSW,C'Y'                                                     
         CLC   TRNKCUNT(2),=C'SK'  AND  CONTRA SK                               
         BNE   *+8                                                              
         MVI   SKREVSW,C'Y'        ALSO NEED SPECIAL REVERSE                    
*                                                                               
         CLC   TRNANAL,CHRWK                                                    
         BE    BLDCH8              FOUND A MATCH                                
         LA    R1,CHRGMX                                                        
         CR    R0,R1                                                            
         BE    TOOCHR              THE CHARGES TABLE IS FULL                    
         OC    CHRWK,CHRWK                                                      
         BZ    *+8                 FIRST TIME                                   
         LA    R5,CHRLEN(,R5)                                                   
         MVC   CHRWK,TRNANAL       INITIALIZE NEW ENTRY                         
         LA    R1,CHRANET                                                       
         LA    RF,CHRBKCNT                                                      
*                                                                               
BLDCH7   ZAP   0(PLAMTLNQ,R1),=P'0'                                             
         LA    R1,PLAMTLNQ(,R1)                                                 
         BCT   RF,BLDCH7                                                        
         AH    R0,=H'1'                                                         
*                                                                               
BLDCH8   AP    CHRANET,PNET                                                     
         AP    CHRANON,PNON                                                     
         AP    CHRACSD,PCSD                                                     
         B     BLDCH4              AND  GET NEXT RECORD                         
*                                                                               
BLDCH20  STC   R0,CHRCNT           NUMBER OF ENTRIES                            
         LTR   R0,R0                                                            
         BZ    BLDCH24             NO   ALLOCATED CHARGES                       
*                                                                               
         GOTO1 AGETRULE            GET  COMMISSION RATES                        
*                                                                               
         USING TWA1D,RF                                                         
         L     RF,ATWA1            CALCULATE COMMISSION                         
         LA    R5,CHARGES                                                       
         LA    R3,CHRTOT                                                        
         ZIC   R0,CHRCNT                                                        
*                                                                               
BLDCH22  ZAP   PK16,CHRANET        COMMISSIONABLE                               
         MP    PK16,CHRRULE        X RATE                                       
         SRP   PK16,64-6,5         RATE HAS 4 DP                                
         ZAP   CHRACOM,PK16        RESULT IS COMMISSION AMOUNT                  
*                                                                               
         GOTO1 AGETRATE            GET GST RATE AND AMOUNT                      
*                                                                               
         AP    CHRANET-CHRD(L'CHRANET,R3),CHRANET  ADD TO TOTALS                
         AP    CHRACOM-CHRD(L'CHRACOM,R3),CHRACOM                               
         AP    CHRANON-CHRD(L'CHRANON,R3),CHRANON                               
         AP    CHRACSD-CHRD(L'CHRACSD,R3),CHRACSD                               
         AP    CHRAGST-CHRD(L'CHRAGST,R3),CHRAGST                               
         AP    CHRAPST-CHRD(L'CHRAPST,R3),CHRAPST                               
*                                                                               
         LA    R5,CHRLEN(,R5)                                                   
         BCT   R0,BLDCH22                                                       
*                                                                               
BLDCH24  ZAP   PNET,=P'0'                                                       
         ZAP   PCOM,=P'0'                                                       
         ZAP   PGST,=P'0'                                                       
         ZAP   PPST,=P'0'                                                       
         ZAP   PNON,=P'0'                                                       
         ZAP   PCSD,=P'0'                                                       
         MVI   FERN,OK                                                          
         B     EXIT                                                             
*                                                                               
         DROP  R2,R3,R5,RF                                                      
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO ALLOCATE CHARGES FOR A PARAGRAPH                        *         
***********************************************************************         
         SPACE 1                                                                
PARCHR   DS    0H                                                               
         LA    R2,BLPWRKH                                                       
         ST    R2,FADR                                                          
*                                                                               
         ZAP   PNET,=P'0'           INITIALIZE PARAGRAPH TOTALS                 
         ZAP   PNON,=P'0'                                                       
         ZAP   PCSD,=P'0'                                                       
         ZAP   PCOM,=P'0'                                                       
         ZAP   PGST,=P'0'                                                       
         ZAP   PPST,=P'0'                                                       
*                                                                               
         USING WKCD,R6                                                          
         LA    R6,WKCAREA          TABLE OF CODES AND PERCENTS                  
         ZIC   R0,WKCNT            NUMBER IN TABLE                              
         LTR   R0,R0                                                            
         BZ    PARCHRX                                                          
*                                                                               
         USING TWA1D,RF                                                         
         USING CHRD,R5                                                          
PARCHR3  L     RF,ATWA1                                                         
         LA    R5,CHARGES          TABLE OF CHARGES                             
         ZIC   R1,CHRCNT           NUMBER OF ITEMS IN TABLE                     
         LTR   R1,R1                                                            
         BZ    NOCHR                                                            
         ZAP   DUB1,=P'0'          GET TOTAL NET INTO DUB1                      
         ZAP   DUB2,=P'0'          AND TOTAL COMMISSIONABLE INTO DUB2           
         ZAP   COMMAMT,=P'0'                                                    
         ZAP   CDAMT,=P'0'                                                      
*                                                                               
PARCHR4  CLC   CHRWK,WKCSV         DOES WORKCODE MATCH                          
         BE    PARCHR5             IT DOES, SO ALLOCATE                         
         LA    R5,CHRLEN(,R5)      IF NOT LOOK AT NEXT CODE                     
         BCT   R1,PARCHR4          UNTIL END OF CHARGES                         
         B     NOALLOC             ERROR NO CHARGES FOR PARAGRAPH               
*                                                                               
PARCHR5  TM    WKCTYPE,X'20'                                                    
         BO    PARCHR7             AMOUNT FIELD IS PERCENT                      
         AP    DUB1,WKCAMNT        TOTAL NET                                    
         TM    WKCTYPE,X'80'                                                    
         BZ    PARCHR11            AMOUNT IS NON-COMMISSIONABLE                 
         AP    DUB2,WKCAMNT        COMMISSIONABLE NET                           
         B     PARCHR11                                                         
*                                                                               
PARCHR7  TM    WKCTYPE,X'80'       DO WE WANT PCT OF COMMISSIONABLE             
         BZ    PARCHR9             BRANCH TO EXCLUDE COMMISSIONABLE             
         ZAP   DUB,CHRANET                                                      
         BAS   RE,COMPUTE          GET AMOUNT APPLIED TO THIS CODE              
         AP    DUB1,DUB            ADD TO TOTAL NET                             
         AP    DUB2,DUB            COMMISSIONABLE NET                           
*                                                                               
PARCHR9  TM    WKCTYPE,X'40'       DO WE WANT ONLY NON-COMMISSIONABLE           
         BZ    PARCHR11            BRANCH IF WE DON'T                           
         ZAP   DUB,CHRANON                                                      
         BAS   RE,COMPUTE          GET AMOUNT APPLIED TO THIS CODE              
         AP    DUB1,DUB            TOTAL NET                                    
*                                                                               
PARCHR11 AP    PNET,DUB2           COMMISSIONABLE NET                           
         AP    PNON,DUB1           TOTAL                                        
         SP    PNON,DUB2           LESS COMMISSIONABLE EQ NON                   
*                                                                               
         ZAP   PK16,DUB2           COMMISSIONABLE                               
         MP    PK16,CHRRULE        X RATE                                       
         SRP   PK16,64-6,5         RATE HAS 4DP                                 
         AP    PCOM,PK16           COMMISSION FOR PARAGRAPH                     
         ZAP   COMMAMT,PK16        SAVE COMMISSION AMOUNT                       
*                                                                               
         ZAP   CDAMT,=P'0'         CLEAR CD HOLD AREA                           
         CP    CHRACSD,=P'0'                                                    
         BE    PARCHR16            NO CASH DISCOUNT                             
         CP    DUB2,=P'0'                                                       
         BE    PARCHR16            NO COMMISSIONABLE ALLOCATED                  
         CP    CHRANET,DUB2        TOTAL COMMISSIONABLE NET VS THIS             
         BNE   PARCHR15            COMMISSIONABLE NET - IF EQUAL ALL            
         AP    PCSD,CHRACSD        CD TO THIS PARA                              
         ZAP   CDAMT,CHRACSD                                                    
         B     PARCHR16                                                         
*                                                                               
PARCHR15 DS    0H                                                               
*                                  NOTE: THE COMMISSIONABLE AMOUNT              
*                                        THAT THE USER ENTERS ALWAYS            
*                                        INCLUDES THE CASH DISCOUNT             
         ZAP   PK16,DUB2           COMMISSIONABLE (INCLUDING THE CD)            
         MP    PK16,CHRACSD        TIMES THE ALLOCATED CASH DISCOUNT            
         MP    PK16,=P'10'         TIMES 10 FOR ROUNDING                        
*                                  DIVIDED BY ALLOCATED NET (ALLOCATED          
         DP    PK16,CHRANET              COMMISIONABLE+ALLOCATED CD)            
         SRP   PK16(8),64-1,5      ROUNDED                                      
         AP    PCSD,PK16(8)        ADD TO CASH DISCOUNT FOR PARAGRAPH           
         ZAP   CDAMT,PK16(8)       AND SAVE                                     
*                                                                               
PARCHR16 SR    RE,RE               CONVERT RATE TO DECIMAL                      
         ICM   RE,3,CHRRATE                                                     
         CVD   RE,DUB                                                           
*                                                                               
         ZAP   PK16,COMMAMT                                                     
         AP    PK16,DUB1           ADD TOTAL TO COMMISSION                      
         SP    PK16,CDAMT          SUBTRACT CD                                  
         ZAP   PSTBASIS,PK16                                                    
         MP    PK16,DUB            X GST PERCENT                                
*                                                                               
         TM    CHRINDS,VBIIDC3     DOES RATE HAVE 3 DECIMALS?                   
         BZ    *+14                NO                                           
         SRP   PK16,64-5,5         YES, SHIFT ONE MORE DIGIT                    
         B     *+10                                                             
         SRP   PK16,64-4,5         RATE HAS 4DP                                 
         AP    PGST,PK16           GST FOR PARAGRAPH                            
         AP    PSTBASIS,PK16       PST BASIS IS GRS-CD+GST                      
*                                                                               
         CLC   SVPSTPRV,=C'PQ'     IS PROVINCE PQ?                              
         BNE   PARCHR17            NO, BASIS INCLUDES GST                       
*        CLC   BILDATE,=X'B30101'  YES, IS THIS BILL PRIOR TO 1/01/13?          
*        BL    PARCHR17            YES, INCLUDE THE GST IN THE BASIS            
         SP    PSTBASIS,PK16       NO, PST BASIS IS LESS GST THEN               
*                                                                               
PARCHR17 SR    RE,RE               CONVERT PSTRATE TO DECIMAL                   
         ICM   RE,3,CHRPRATE                                                    
         CVD   RE,DUB                                                           
*                                                                               
         ZAP   PK16,PSTBASIS                                                    
         MP    PK16,DUB            X PST PERCENT                                
*                                                                               
         TM    CHRPINDS,PBIIDC3    DOES RATE HAVE 3 DECIMALS?                   
         BZ    *+14                NO                                           
         SRP   PK16,64-5,5         YES, SHIFT ONE MORE DIGIT                    
         B     *+10                                                             
         SRP   PK16,64-4,5         RATE HAS 4DP                                 
*                                                                               
         AP    PPST,PK16           PST FOR PARAGRAPH                            
         LA    R6,WKCLEN(,R6)      NEXT WORK-CODE FOR TABLE                     
         BCT   R0,PARCHR3                                                       
*                                                                               
         LA    R5,CHRTOT           ADD TO BILL TOTALS                           
         LA    R1,CHRANET          R1 TO TOTAL CHARGES                          
         LA    R2,CHRBNET          R2 TO PREVIOUSLY ALLOCATED TOTAL             
         LA    R3,PNET             R3 TO PARAGRAPH TOTALS                       
         LA    R0,PTOT#            NET/COMMISSION/NON/C.D./GST/PST              
*                                                                               
PARCHR19 CP    0(PLAMTLNQ,R3),=P'0'          NOTHING ALLOCATED                  
         BE    PARCHR21                      SKIP, TOTALS & ROUNDING            
         AP    0(PLAMTLNQ,R2),0(PLAMTLNQ,R3) PARA AMOUNT TO ALLOCATED           
         ZAP   DUB,0(PLAMTLNQ,R1)            TOTAL                              
         SP    DUB,0(PLAMTLNQ,R2)            LESS ALLOCATED                     
         CP    DUB,=P'02'                                                       
         BH    PARCHR21                      MORE THAN 2 CENTS                  
         CP    DUB,=P'-02'                   EITHER WAY                         
         BL    PARCHR21                      IS NOT ROUNDING                    
         AP    0(PLAMTLNQ,R2),DUB            ADD ROUNDING TO ALLOCATED          
         AP    0(PLAMTLNQ,R3),DUB            AND PARAGRAPH                      
*                                                                               
PARCHR21 LA    R1,PLAMTLNQ(,R1)                                                 
         LA    R2,PLAMTLNQ(,R2)                                                 
         LA    R3,PLAMTLNQ(,R3)                                                 
         BCT   R0,PARCHR19                                                      
*                                                                               
PARCHRX  MVI   FERN,OK                                                          
         B     EXIT                                                             
         DROP  R5,R6,RF                                                         
         EJECT ,                                                                
         SPACE 1                                                                
         USING WKCD,R6                                                          
         SPACE 1                                                                
COMPUTE  ZAP   PK16,DUB                                                         
         MP    PK16,WKCAMNT        AMOUNT * PCT                                 
         SRP   PK16,64-4,5         ROUND RESULT                                 
         ZAP   DUB,PK16            RETURN AMOUNT IN DUB                         
         BR    RE                                                               
         DROP  R6                                                               
         EJECT ,                                                                
***********************************************************************         
*  READ THRU BILL TO ADD UP PARAGRAPH TOTALS                          *         
***********************************************************************         
         SPACE 1                                                                
         USING CHRD,R5                                                          
         SPACE 1                                                                
BILCHR   DS    0H                                                               
         LA    R5,CHRTOT                                                        
         ZAP   CHRBNET,=P'0'                                                    
         ZAP   CHRBCOM,=P'0'                                                    
         ZAP   CHRBGST,=P'0'                                                    
         ZAP   CHRBPST,=P'0'                                                    
         ZAP   CHRBNON,=P'0'                                                    
         ZAP   CHRBCSD,=P'0'                                                    
         LA    R0,1                                                             
*                                                                               
BILCHR3  STC   R0,WRKPARA                                                       
         CLC   WRKPARA,WRKLPARA                                                 
         BH    EXIT                END OF BILL                                  
         GOTO1 AWRKLGET,AIOAREA2   GET FIRST/NEXT PARAGRAPH                     
*                                                                               
         USING PARAD,R6                                                         
         L     R6,AIOAREA2                                                      
         CLI   PARALEN,PARALNQ                                                  
         BNE   BILRFMTD            BILL MUST BE REFORMATTED                     
*                                                                               
         AP    CHRBNET,PARANET                                                  
         AP    CHRBCOM,PARACOM                                                  
         AP    CHRBGST,PARAGST                                                  
         AP    CHRBPST,PARAPST                                                  
         AP    CHRBNON,PARANON                                                  
         AP    CHRBCSD,PARACSD                                                  
*                                                                               
         AH    R0,=H'1'                                                         
         B     BILCHR3              GET NEXT PARAGRAPH                          
         DROP  R5,R6                                                            
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO DISPLAY PARAGRAPH, BILL AND JOB TOTALS                  *         
***********************************************************************         
         SPACE 1                                                                
         USING DTD,R6                                                           
         SPACE 1                                                                
DISTOT   DS    0H                                                               
         LA    R6,BLPTITL                                                       
*                                  SET-UP HEADLINE                              
         MVC   DTTITL(DTTITLNQ),SPACES                                          
         MVC   DTNET+4(7),AC$NET        NET        (RIGHT)                      
         MVC   DTCOM+3(7),AC$CMN        COMMISSION (RIGHT)                      
         MVC   DTGROSS+4(7),AC$GROSS    GROSS      (RIGHT)                      
         MVC   DTGST+3(7),AC$VAT        GST        (RIGHT)                      
         MVC   DTPST+3(7),AC$PST        PST        (RIGHT)                      
         MVC   DTTOT+4(7),AC$TOTAL      TOTAL      (RIGHT)                      
*                                                                               
         CLI   PFCOMM,C'F'                                                      
         BNE   DISTOT05                                                         
         MVC   DTCOM+3(7),AC$FEE        FEE        (CENTERED)                   
*                                                                               
DISTOT05 TM    RUNOPT,NEEDGST      ARE  WE   DOING  GST ?                       
         BO    *+10                YES                                          
*                                  CLEAR GST HEADERS                            
         MVC   DTGST(DTGSTLN),SPACES                                            
*                                                                               
         OI    BLPTITLH+6,FVOXMT   TRANSMIT TITLE                               
         LA    R5,PNET                                                          
         LA    R2,BLPPARTH                                                      
         MVC   BLPPART,SPACES                                                   
         OI    BLPPARTH+6,FVOXMT   TRANSMIT                                     
         BAS   RE,DISEDIT          DISPLAY PARAGRAPH TOTALS                     
         CLI   BYTE,C'N'                                                        
         BE    DISTOT10                                                         
*                                  PARAGRAPH                                    
         MVC   BLPPART(9),AC$9PRGR                                              
*                                                                               
         USING CHRD,R5                                                          
DISTOT10 LA    R5,CHRTOT                                                        
         LA    R5,CHRBNET          BILL TOTALS                                  
         LA    R2,BLPBILTH                                                      
         MVC   BLPBILT,SPACES                                                   
         OI    BLPBILTH+6,FVOXMT   TRANSMIT                                     
         BAS   RE,DISEDIT                                                       
         CLI   BYTE,C'N'                                                        
         BE    DISTOT20                                                         
         MVC   BLPBILT(9),AC$BIL   BILL                                         
*                                                                               
DISTOT20 MVC   BLPJOBT,SPACES                                                   
         OI    BLPJOBTH+6,FVOXMT   TRANSMIT                                     
         TM    SAVHSTAT,MANBILL    NO   JOB TOTALS FOR MANUAL                   
         BO    EXIT                                                             
         LA    R5,CHRTOT           JOB  TOTALS                                  
         LA    R5,CHRANET                                                       
         LA    R2,BLPJOBTH                                                      
         BAS   RE,DISEDIT                                                       
         CLI   BYTE,C'N'                                                        
         BE    BILTOT30                                                         
         MVC   BLPJOBT(9),AC$JOB   JOB                                          
*                                                                               
BILTOT30 B     EXIT                                                             
         DROP  R5,R6                                                            
         EJECT ,                                                                
         SPACE 1                                                                
DISEDIT  MVI   BYTE,C'Y'                                                        
         LA    R0,PTOT#                                                         
         LR    R1,R5                                                            
*                                                                               
DISEDIT2 CP    0(PLAMTLNQ,R1),=P'0'                                             
         BNE   DISEDIT3                                                         
         LA    R1,PLAMTLNQ(,R1)                                                 
         BCT   R0,DISEDIT2                                                      
         MVI   BYTE,C'N'           NO   DATA TO DISPLAY                         
         BR    RE                                                               
*                                                                               
         USING CHRD,R5                                                          
         USING DTD,R6                                                           
DISEDIT3 ST    RE,SAVERE           SAVE RETURN     REGISTER                     
         LA    R6,8(,R2)           ->   FIELD DATA AREA                         
         ZAP   DUB2,0(PLAMTLNQ,R5)                 NET-COMMISSIONABLE           
         AP    DUB2,2*PLAMTLNQ(PLAMTLNQ,R5)        PLUS NON                     
*                                                  **** NET                     
         EXTED (P8,DUB2),(L'DTNET+1,DTNET),2,MINUS=YES                          
*                                                                               
*                                                  **** COMMISSION              
         EXTED (P8,PLAMTLNQ(R5)),(L'DTCOM+1,DTCOM),2,MINUS=YES                  
*                                                                               
         AP    DUB2,PLAMTLNQ(PLAMTLNQ,R5)                                       
*                                                  **** GROSS                   
         EXTED (P8,DUB2),(L'DTGROSS+1,DTGROSS),2,MINUS=YES                      
*                                                                               
         TM    RUNOPT,NEEDGST      ARE  WE   DOING GST ?                        
         BZ    DISEDIT9            NO,  RETURN                                  
*                                                                               
*                                                  **** GST                     
         EXTED (P8,4*PLAMTLNQ(R5)),(L'DTGST+1,DTGST),2,MINUS=YES                
         AP    DUB2,4*PLAMTLNQ(PLAMTLNQ,R5)                                     
*                                                                               
*                                                  **** PST                     
         EXTED (P8,5*PLAMTLNQ(R5)),(L'DTPST+1,DTPST),2,MINUS=YES                
         AP    DUB2,5*PLAMTLNQ(PLAMTLNQ,R5)                                     
*                                                                               
*                                                  **** GST  GROSS              
         EXTED (P8,DUB2),(L'DTTOT+1,DTTOT),2,MINUS=YES      (TOTAL)             
*                                                                               
DISEDIT9 L     RE,SAVERE           RESTORE   REGISTER                           
         BR    RE                  RETURN                                       
         DROP  R5,R6                                                            
         EJECT ,                                                                
***********************************************************************         
*  NOTE - CHRB SET IN BILCHR ROUTINE (GET TOTAL OF PARAGRAPHS)        *         
***********************************************************************         
         SPACE 1                                                                
         USING VTCD,R2                                                          
         USING SVTABD,R3                                                        
         USING CHRD,R5                                                          
         USING TWA1D,RF                                                         
         SPACE 1                                                                
GETMAN   L     RF,ATWA1                                                         
         L     R2,AVATBUFF                                                      
         LA    R3,SVTABLE                                                       
         LA    R5,CHRTOT                                                        
         LA    R0,VATNUM                                                        
*                                                                               
GETM02   XC    SVTYPE(SVDATAL),SVTYPE                                           
         MVC   SVBK(SVBKLNQ),ZEROS                                              
         LA    R3,SVTABLNQ(R3)                                                  
         BCT   R0,GETM02                                                        
*                                                                               
         LA    R3,SVTABLE                                                       
         OC    SVGSTCOD,SVGSTCOD   DO WE HAVE A VAT CODE ?                      
         BZ    GETMX               NO, EXIT                                     
         TM    RUNOPT,NEEDGST      ARE WE DOING GST LOGIC ?                     
         BZ    GETMX               NO                                           
*                                                                               
         MVI   VTCACTN,VTCALOOK    LOOK FOR RATES                               
         MVC   VTCLANG,PLANG       USE  PRINT LANGUAGE                          
         MVC   VTCCOMF,ACOMFACS    A(COMFACS)                                   
         MVC   VTCCPY,JOBKEY       COMPANY CODE                                 
         MVC   VTCOFFC,HALF        OFFICE                                       
         MVC   VTCINVD,BILDATE     USE BILLDATE IF THERE                        
         CLI   BILDATE,X'00'       ELSE, USE TODAY                              
         BNE   *+10                                                             
         MVC   VTCINVD,TODAYP                                                   
         MVC   VTCTYPE,SVGSTCOD    PASS VAT TYPE CODE                           
         XC    VTCPRV,VTCPRV                                                    
         XC    VTCPRVD,VTCPRVD     PROVINCE DESCRIPTION??                       
         GOTO1 VVATICAN,(R2)                                                    
         TM    VTCINDS,VTCINA      IS TAX APPLICABLE ?                          
         BO    GETMX               NO                                           
         CLI   VTCERR,X'00'        YES, FIND TYPE ?                             
         BNE   GETMX               NO                                           
*                                                                               
         MVI   SVTYPE,SVT_GST      BUILD GST SV TABLE ENTRY                     
         MVC   SVCODE,VTCTYPE      CODE                                         
         MVC   SVRATE,VTCRATE      RATE                                         
         MVC   SVINDS,VTCINDS      INDICATOR                                    
         MVC   SVACCT,VTCACT       ACCOUNT,                                     
         MVC   SVDATE,VTCEFFD      AND EFFECTIVE DATE IN TABLE                  
*                                                                               
         ZAP   SVNET,CHRBNET                                                    
         ZAP   SVNON,CHRBNON                                                    
         ZAP   SVCOMM,CHRBCOM                                                   
         ZAP   SVDISC,CHRBCSD                                                   
         ZAP   SVGST,CHRBGST                                                    
*                                                                               
         ZAP   PK16,CHRBNET                                                     
         AP    PK16,CHRBCOM                                                     
         AP    PK16,CHRBNON                                                     
         SP    PK16,CHRBCSD                                                     
         ZAP   SVBASE,PK16                                                      
*                                                                               
         TM    RUNOPT,NEEDPST                                                   
         BZ    GETMX                                                            
*                                                                               
         MVC   VTCTYPE,SVPSTCOD    PASS PST TYPE CODE                           
         MVC   VTCPRV,SVPSTPRV     PASS THE PROVIDENCE                          
         GOTO1 VVATICAN,(R2)                                                    
         TM    VTCINDS,VTCINA      IS TAX APPLICABLE ?                          
         BO    GETMX               NO                                           
         CLI   VTCERR,X'00'        YES, FIND TYPE ?                             
         BNE   GETMX               NO                                           
*                                                                               
         LA    R3,SVTABLNQ(,R3)    BUMP TABLE ENTRY                             
         MVI   SVTYPE,SVT_PST      SAVE PST DATA                                
         MVC   SVRATE,VTCRATE                                                   
         MVC   SVINDS,VTCINDS                                                   
         MVC   SVACCT,VTCACT                                                    
         MVC   SVCODE,SVPSTCOD                                                  
         MVC   SVPRVNCE,VTCPRV                                                  
         MVC   SVPRDESC,VTCPRVD                                                 
*                                                                               
         ZAP   SVNET,CHRBNET                                                    
         ZAP   SVNON,CHRBNON                                                    
         ZAP   SVCOMM,CHRBCOM                                                   
         ZAP   SVDISC,CHRBCSD                                                   
         ZAP   SVPST,CHRBPST                                                    
*                                                                               
         ZAP   PK16,CHRBNET                                                     
         AP    PK16,CHRBCOM                                                     
         AP    PK16,CHRBNON                                                     
         SP    PK16,CHRBCSD                                                     
         AP    PK16,CHRBGST                                                     
         ZAP   SVBASE,PK16                                                      
*                                                                               
         CLC   SVPRVNCE,=C'PQ'                                                  
         BNE   GETMX                                                            
*        CLC   BILDATE,=X'B30101'                                               
*        BL    GETMX                                                            
         SP    SVBASE,CHRBGST                                                   
*                                                                               
GETMX    B     EXIT                                                             
         DROP  R2,R3,R5,RF                                                      
         EJECT ,                                                                
         SPACE 1                                                                
ADDELM   DS    0H                                                               
         L     RF,0(,R1)                                                        
         GOTO1 VHELLO,DMCB,(C'P',=C'ACCFIL'),(RF),TEMP,0                        
         CLI   DMCB+12,0           EVERYTHING OK?                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     EXIT                                                             
         EJECT ,                                                                
         SPACE 1                                                                
TRXELM   DS    0H                  UPDATE TRXEL X'75'                           
         L     R5,0(,R1)           A(RECORD)                                    
         GOTO1 =A(TRXELEM),RR=RELO                                              
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  LOAD ADDTRN AND INITIALIZE ADDTRN I/O AREAS                        *         
***********************************************************************         
         SPACE 1                                                                
LDADTRN  DS    0H                  LOAD ADDTRN    AND  ACBIL40                  
         GOTO1 =A(LDADTRAN),RR=RELO                                             
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*  BATCH RECORD ADD ELEMENT LAST                                      *         
***********************************************************************         
         SPACE 1                                                                
BADDLAST DS    0H                  BATCH     RCD  ADD  ELEMENT   LAST           
         GOTO1 =A(BTADDLST),(R1),RR=RELO                                        
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*  BUILD HEADER KEY FOR BATCH RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
BBLDKHD  DS    0H                  BUILD     KEY  FOR  BATCH     HDR            
         GOTO1 =A(BTBLDKHD),RR=RELO                                             
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*  ADD BATCH HEADER RECORD                                          *           
***********************************************************************         
         SPACE 1                                                                
BADDHDR  DS    0H                  ADD  BATCH     HDR  RECORD                   
         GOTO1 =A(BTADDHDR),RR=RELO                                             
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  BATCH ADD DRAFT TRANSACTION ELEMENT                                *         
***********************************************************************         
         SPACE 1                                                                
BADDRFT  DS    0H                  FOR  BATCH     RECORDS   ADD                 
*                                       DRAFT     TRANSACTION    EL             
         GOTO1 =A(BADDRAFT),RR=RELO                                             
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*  CALL ADDREC/PUTREC FOR BATCH RECORDS                               *         
***********************************************************************         
         SPACE 1                                                                
BADDREC  DS    0H                  CALL ADDREC/PUTREC  FOR  BATCH  RCDS         
         GOTO1 =A(BTADDREC),RR=RELO                                             
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*  BATCH DELETE DIRECTORY POINTERS                                    *         
***********************************************************************         
         SPACE 1                                                                
*                                  BATCH     DELETE    DIRECTORY                
BDELDPT  DS    0H                       POINTERS  FOR  UNBILLING                
         GOTO1 =A(BTDELDPT),RR=RELO                                             
         B     EXIT                                                             
         SPACE 2                                                                
***********************************************************************         
*  BATCH ITEM RECORD ADDREC/PUTREC                                    *         
***********************************************************************         
         SPACE 1                                                                
*                                  BATCH     ITEM RECORD                        
BITEMADD DS    0H                       ADDREC/PUTREC                           
         GOTO1 =A(BITMADD),RR=RELO                                              
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  CHECK FOR SECURITY LOCKOUT                                         *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
VALMLK   DS    0H                                                               
         STC   R1,BYTE             GET THE TYPE                                 
         MVI   FERN,MOSLOCK        PREPARE FOR ERROR                            
         CLI   BILMNTH,0           ANY MOS OVERRIDE?                            
         BNE   *+10                YES                                          
         MVC   BILMNTH,TODAYP      NO, USE TODAY                                
         GOTO1 VDATCON,DMCB,(1,BILMNTH),(6,WORK)                                
         GOTO1 VBMONVAL,DMCB,(6,WORK),(BYTE,ACOMFACS),(0,WORK+6),      X        
               (COMPANY,0)                                                      
         LA    R1,WORK+6                                                        
         USING BMONVALD,R1                                                      
         CLI   BMOERR,BMOEOKQ      EVERYTHING OK?                               
         BNE   VALMLKX             NO                                           
         MVI   FERN,OK                                                          
*                                                                               
VALMLKX  CLI   FERN,OK                                                          
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* CALL DICTATE                                                        *         
*                                                                     *         
* INPUT:                                                              *         
*   R1   = ADDRESS OF FIELD TO BE TRANSLATED                          *         
*          NOTE: THE FIELD MUST HAVE BEEN PREVIOUSLY INITIALIZED WITH *         
*                AN MVCDD INSTRUCTION.                                *         
*                                                                     *         
* USES:                                                               *         
*   DMCB = PARM AREA                                                  *         
***********************************************************************         
         SPACE 1                                                                
CALLDICT DS    0H                                                               
         ST    R6,SAVER6           SAVE     R6                                  
         ST    RE,SAVERE           SAVE     RE                                  
         LR    R6,R1                                                            
         GOTO1 VDICTAT,DMCB,C'SL  ',(R6),0                                      
         LR    R1,R6               RESTORE  R1                                  
         L     R6,SAVER6           RESTORE  R6                                  
         L     RE,SAVERE           RESTORE  RE                                  
         BSM   0,RE                RETURN                                       
         EJECT ,                                                                
***********************************************************************         
* FIND FIRST AVAILABLE (BYTE)                                         *         
*                                                                     *         
*   INPUT:                                                            *         
*     R1 = NUMBER  OF BYTES TO CONSIDER                               *         
*     R2 = ADDRESS OF FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
FINDFREE AR    R2,R1               FIND LAST BYTE IN   FIELD                    
         BCTR  R2,0                                                             
*                                                                               
FINDFR10 CLI   0(R2),C' '          FIND LAST CHARACTER                          
         BH    FINDFR20                                                         
         BCTR  R2,0                                                             
         BCT   R1,FINDFR10                                                      
*                                                                               
FINDFR20 LA    R2,1(,R2)           1ST  FREE BYTE                               
         BSM   0,RE                RETURN    TO   CALLER                        
         EJECT ,                                                                
***********************************************************************         
*  ERROR ROUTINES MESSAGES                                            *         
***********************************************************************         
         SPACE 1                                                                
TOOCHR   MVC   FVMSGNO,=AL2(2208)  TOO MANY ALLOCATED CHARGES ON JOB            
         B     SETERR                                                           
*                                                                               
NOCHR    MVC   FVMSGNO,=AL2(2202)  NO ALLOCATED CHARGES ON JOB                  
         B     SETERR                                                           
*                                                                               
         USING WKCD,R6                                                          
*                                  NO CHARGES FOR CODE XX                       
NOALLOC  MVC   FVMSGNO,=AL2(2209)                                               
         MVC   XTRAMESS(2),WKCSV                                                
         LA    R1,BLPWRKH                                                       
         B     SETERR2                                                          
         DROP  R6                                                               
*                                                                               
BILRFMTD MVC   FVMSGNO,=AL2(2221)  BILL MUST BE REFORMATTED                     
         LA    R1,BILACTH                                                       
         B     SETERR2                                                          
*                                                                               
SETERR   LA    R1,BILJOBH                                                       
*                                                                               
SETERR2  ST    R1,FADR                                                          
         GOTO1 AERRORX2                                                         
         EJECT ,                                                                
***********************************************************************         
*  FORMAT OUTPUT MESSAGE/EXTRA MESSAGE INTO MSG & EXIT.               *         
***********************************************************************         
         SPACE 1                                                                
ERRORX2  L     RD,AWORK            RESTORE     BASE  RD FOR QUICK EXIT          
         MVI   WRKSTAT,0           SINCE ERROR DON'T CLOSE FILE                 
*                                                                               
ERRORX2A MVI   FERN,SPECIAL                                                     
         LH    R2,FVMSGNO          GET   MSG   NUMBER                           
         XC    DMCB+8(4),DMCB+8    CLEAR EXTRA OUTPUT                           
         CLI   FVMTYPE,C' '        ANY   TEXT  MSG   TYPE ?                     
         BNE   *+8                 YES,  SKIP                                   
         MVI   FVMTYPE,FVMERR      MSG   TYPE  ERROR                            
         CLC   XTRAMESS,SPACES     ANY   EXTRA MESSAGE ?                        
         BE    ERRORX2B            NO,   SKIP  EXTRA MESSAGE LOGIC              
         LA    RE,XTRAMESS         ->    EXTRA MESSAGE                          
         ST    RE,DMCB+8           SAVE  IN    PARM  LIST                       
         MVI   DMCB+8,L'XTRAMESS   SAVE  MAX   LENGTH                           
*                                                                               
ERRORX2B GOTO1 ATXTGET,DMCB,(FVMTYPE,(R2)),(L'MSG,MSG),,0                       
         B     SAVBUF              CONTINUE    ERROR PATH                       
*                                                                               
ERRORX   L     RD,AWORK            RESTORE     BASE  RD FOR QUICK EXIT          
         MVI   WRKSTAT,0           SINCE ERROR DON'T CLOSE FILE                 
*                                                                               
ERROR    CLI   FERN,SPECIAL                                                     
         BE    SAVBUF                                                           
         GOTO1 VGETMSG,DMCB1,(FERN,MSG),(FNDX,DMCB),0                           
         CLC   XTRAMESS,SPACES                                                  
         BE    SAVBUF                                                           
         LA    R1,XTRAMESS+L'XTRAMESS-1                                         
         LA    RE,XTRAMESS-1                                                    
         ZIC   RF,DMCB1                                                         
*                                                                               
ERROR2   CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,ERROR2                                                        
         SR    R1,RE               R1=L'XTRAMESS                                
         LA    R1,1(RF,R1)         R1=TOTAL MESSAGE LENGTH                      
         LA    RE,L'MSG                                                         
         CR    R1,RE               CHECK MESSAGE FITS                           
         BH    SAVBUF                                                           
         LA    RF,MSG+1(RF)        AND IF SO TACK ON EXTRA MESSAGE              
         MVC   0(L'XTRAMESS,RF),XTRAMESS                                        
         B     SAVBUF                                                           
*                                                                               
OKEND    DS    0H                                                               
*                                                                               
SAVBUF   DS    0H                                                               
         CLI   WRKSTAT,0           SAVE WORKER BUFFER DETAILS IF IN USE         
         BE    OMSG                                                             
         TM    WRKSTAT,WRITPEND                                                 
         BZ    SAVBUF2                                                          
         GOTO1 AWRKLCLO,AWRKIO     LIB CLOSE FOR WRITE PENDING IF ANY           
*                                                                               
SAVBUF2  GOTO1 AWRKBUS             BUFFER SAVE                                  
         BE    OMSG                                                             
         XC    WRKSAVE,WRKSAVE                                                  
*                                  MOVE MESSAGE TO TWA & TRANSMIT               
OMSG     MVC   BILMSG,MSG                                                       
         OI    BILMSGH+6,FVOXMT    TRANSMIT                                     
         L     R1,FADR                                                          
         OI    6(R1),X'40'         SET CURSOR TO FIELD                          
*                                                                               
         MVC   DMCB+10(2),TWATRM   TERMINAL NO                                  
         MVI   DMCB+8,1            PAGE 1                                       
         MVI   DMCB+9,0                                                         
         L     RE,ATWA1            STAMP TWA1 AS OURS                           
*                                  REWRITE ON TEMPSTR                           
         GOTO1 VDATAMGR,DMCB,DMWRT,TEMPSTR,,ATWA1                               
         CLI   SIOSW,C'Y'                                                       
         BNE   EXIT                                                             
         GOTO1 VGETFACT,DMCB,0,0                                                
*                                                                               
         USING FACTSD,R2                                                        
         L     R2,0(,R1)                                                        
         LA    R3,BILMSG+55                                                     
         EDIT  (B2,FATIOCNT),(5,0(R3)),ALIGN=LEFT                               
         MVC   BILMSG+51(4),=C'SIO='                                            
         B     EXIT                                                             
         DROP  R2                                                               
*                                                                               
OKXIT    SR    RB,RB                                                            
*                                                                               
ERRXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT ,                                                                
***********************************************************************         
*  CONSTANTS                                                          *         
***********************************************************************         
         SPACE 1                                                                
*                                  BEGIN     KEEP IN   SEQUENCE  WITH           
*                                       DMVARS    IN   ACBILWORK                
DMCONS   DS    0C                  DATA MGR  CONSTANTS                          
         DC    CL8'ACCDIR  '                                                    
         DC    CL8'ACCMST  '                                                    
         DC    CL8'ACCOUNT '                                                    
         DC    CL8'CTFILE  '                                                    
         DC    CL8'TEMPSTR '                                                    
*                                                                               
         DC    CL8'ADDREC  '                                                    
         DC    CL8'GETREC  '                                                    
         DC    CL8'PUTREC  '                                                    
*                                                                               
         DC    CL8'DMADD   '                                                    
         DC    CL8'DMRDH   '                                                    
         DC    CL8'DMREAD  '                                                    
         DC    CL8'DMRSEQ  '                                                    
         DC    CL8'DMWRT   '                                                    
         DC    CL8'DMUNLK  '                                                    
*                                                                               
         DC    CL7'ADD=END'                                                     
*                                                                               
DMCONSQ  EQU   *-DMCONS                                                         
*                                  END  OF   KEEP IN   SEQUENCE  WITH           
*                                       DMVARS    IN   ACBILWORK                
         EJECT ,                                                                
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
         DROP  R4,R7,RA,RC                                                      
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO GET COMMISSION RULES (THE NEW WAY)                      *         
***********************************************************************         
         SPACE  1                                                               
         USING GOBLOCKD,R3                                                      
         USING LWSD,RC             MAP  LOCAL     WORKING   STORAGE             
         SPACE  1                                                               
GETRULE  NMOD1 0,**GETRU**                                                      
         L     RC,ABASEC                                                        
         L     R3,AGOBLOCK                                                      
         MVC   GOADM,VDATAMGR      SET-UP    GOBLOCK                            
         MVC   GOSELCUL,JOBKEY                                                  
         MVC   GOSELCLI,LCLI                                                    
         MVC   GOSELPRO,LPRO                                                    
         MVC   GOSELJOB,LJOB                                                    
         MVC   GOACOMP,ADCMP                                                    
         MVC   GOACLI,ADCLI                                                     
         MVC   GOAPRO,ADPRD                                                     
         MVC   GOAJOB,ADJOB                                                     
         MVI   GOWHICH,0                                                        
         MVI   GOANYMED,0                                                       
         MVI   GOSELLEV,0                                                       
*                                                                               
         USING TWA1D,RE                                                         
         USING CHRD,R5                                                          
         L     RE,ATWA1                                                         
         LA    R5,CHARGES          GET RATE FOR EACH WORKCODE IN TABLE          
         ZIC   R0,CHRCNT                                                        
*                                                                               
GETRU02  MVC   GOSELWC,CHRWK                                                    
         GOTO1 VGETOPT,DMCB,AGOBLOCK                                            
         ZAP   CHRRULE,GOAGYCOM                                                 
         MVC   CHRCODE,GOTAXCOD                                                 
         MVC   CHRPCODE,GOPSTCOD                                                
         MVC   CHRPRV,GOPSTPRV                                                  
         LA    R5,CHRLEN(,R5)                                                   
         BCT   R0,GETRU02                                                       
         B     GETRX                                                            
*                                                                               
         DROP  R3,R5,RC,RE                                                      
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  FILL IN GST FIELDS OF THE CHRD RECORD PASSED IN 0(R5)              *         
*  CREATE/UPDATE SVTAB ENTRIES FOR THIS TYPE (PST AND GST)            *         
*  CHRCODE/CHRPCODE/CHRPRV SET IN GETRULE                             *         
***********************************************************************         
         SPACE  1                                                               
         USING VTCD,R2                                                          
         USING SVTABD,R3                                                        
         USING CHRD,R5                                                          
         USING TWA1D,RF                                                         
         SPACE  1                                                               
GETRATE  NMOD1 0,**GETRA**                                                      
         L     RF,ATWA1                                                         
         L     R2,AVATBUFF                                                      
         LA    R3,SVTABLE                                                       
         LA    R6,VATNUM                                                        
         XC    CHRRATE,CHRRATE     CLEAR RATE                                   
         XC    CHRPRATE,CHRPRATE   AND PST RATE                                 
         OC    CHRCODE,CHRCODE     DO WE HAVE A VAT CODE ?                      
         BZ    GETRX               NO, EXIT                                     
         TM    RUNOPT,NEEDGST      ARE WE DOING GST LOGIC ?                     
         BZ    GETRX               NO                                           
         DROP  RF                                                               
*                                                                               
GETR02   CLI   SVTYPE,X'00'        NOTHING IN TABLE OR END                      
         BE    GETR04                                                           
         CLI   SVTYPE,SVT_GST      IS THIS A GST ENTRY                          
         BNE   GETR03              NO                                           
         CLC   CHRCODE,SVCODE      FIND CODE IN TABLE                           
         BE    GETR06                                                           
*                                                                               
GETR03   LA    R3,SVTABLNQ(,R3)                                                 
         BCT   R6,GETR02                                                        
         DC    H'0'                SHOULD NOT HAVE MORE THAN 10 CODES           
*                                                                               
GETR04   MVI   VTCACTN,VTCALOOK    LOOK FOR RATES                               
         MVC   VTCLANG,PLANG       USE  PRINT LANGUAGE                          
         MVC   VTCCOMF,ACOMFACS    A(COMFACS)                                   
         MVC   VTCCPY,JOBKEY       COMPANY CODE                                 
         MVC   VTCOFFC,HALF        OFFICE                                       
         MVC   VTCINVD,BILDATE     USE BILLDATE IF THERE                        
         CLI   BILDATE,X'00'       ELSE, USE TODAY                              
         BNE   *+10                                                             
         MVC   VTCINVD,TODAYP                                                   
         MVC   VTCTYPE,CHRCODE     PASS VAT TYPE CODE                           
         XC    VTCPRV,VTCPRV                                                    
         GOTO1 VVATICAN,(R2)                                                    
         TM    VTCINDS,VTCINA      IS TAX APPLICABLE ?                          
         BO    GETRX               NO                                           
         CLI   VTCERR,X'00'        YES, FIND TYPE ?                             
         BNE   GETRX               NO                                           
*                                                                               
         MVI   SVTYPE,SVT_GST      CREATE A NEW SVTABLE RECORD                  
         MVC   SVRATE,VTCRATE      PUT RATE,                                    
         MVC   SVINDS,VTCINDS       DECIMALS,                                   
         MVC   SVACCT,VTCACT         ACCOUNT,                                   
         MVC   SVCODE,VTCTYPE           CODE                                    
         MVC   SVDATE,VTCEFFD             EFFECTIVE DATE                        
         MVC   SVREG,VTCREG                  AND REGISTRATION NUMBER            
         XC    SVPRVNCE,SVPRVNCE                                                
*                                                                               
GETR06   MVC   CHRRATE,SVRATE      SAVE RATE                                    
         MVC   CHRINDS,SVINDS      INDICATOR                                    
         MVC   CHRDATE,SVDATE      DATE                                         
         MVC   CHRACCT,SVACCT      AND ACCOUNT                                  
         AP    SVNET,CHRANET                                                    
         AP    SVNON,CHRANON                                                    
         AP    SVCOMM,CHRACOM                                                   
         AP    SVDISC,CHRACSD                                                   
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,CHRRATE                                                     
         CVD   RE,DUB                                                           
*                                                                               
         ZAP   PK16,CHRANET                                                     
         AP    PK16,CHRACOM                                                     
         AP    PK16,CHRANON                                                     
         SP    PK16,CHRACSD                                                     
         AP    SVBASE,PK16                                                      
         MP    PK16,DUB                                                         
*                                                                               
         TM    CHRINDS,VBIIDC3     DOES RATE HAVE 3 DECIMALS?                   
         BZ    *+14                NO                                           
         SRP   PK16,64-5,5         YES, SHIFT ONE MORE DIGIT                    
         B     *+10                                                             
         SRP   PK16,64-4,5                                                      
*                                                                               
         AP    SVGST,PK16                                                       
         ZAP   CHRAGST,PK16                                                     
*                                                                               
         TM    RUNOPT,NEEDPST      ARE WE DOING PST LOGIC ?                     
         BZ    GETRX               NO                                           
         OC    CHRPCODE,CHRPCODE   DO WE HAVE A PST CODE ?                      
         BZ    GETRX               NO, EXIT                                     
*                                                                               
         USING TWA1D,RF                                                         
         L     RF,ATWA1                                                         
         LA    R3,SVTABLE                                                       
         LA    R6,VATNUM                                                        
         DROP  RF                                                               
*                                                                               
GETR20   CLI   SVTYPE,X'00'        NOTHING IN TABLE OR END                      
         BE    GETR40                                                           
         CLI   SVTYPE,SVT_PST      IS THIS A PST ENTRY                          
         BNE   GETR30              NO                                           
         CLC   CHRPCODE,SVCODE     FIND CODE IN TABLE                           
         BNE   GETR30              NO                                           
         CLC   CHRPRV,SVPRVNCE     FIND CODE IN TABLE                           
         BE    GETR50                                                           
*                                                                               
GETR30   LA    R3,SVTABLNQ(,R3)                                                 
         BCT   R6,GETR20                                                        
         DC    H'0'                SHOULD NOT HAVE MORE THAN 10 CODES           
*                                                                               
GETR40   MVC   VTCTYPE,CHRPCODE    BUILD A NEW PST SVTABLE ENTYY                
         MVC   VTCPRV,CHRPRV       PROVINCE                                     
         GOTO1 VVATICAN,(R2)                                                    
         TM    VTCINDS,VTCINA      IS   TAX APPLICABLE ?                        
         BO    GETRX               NO                                           
         CLI   VTCERR,X'00'        YES, FIND TYPE ?                             
         BNE   GETRX               NO                                           
         MVI   SVTYPE,SVT_PST                                                   
         MVC   SVCODE,VTCTYPE      PST  CODE                                    
         MVC   SVPRVNCE,VTCPRV                                                  
         MVC   SVPRDESC,VTCPRVD                                                 
         MVC   SVRATE,VTCRATE                                                   
         MVC   SVINDS,VTCINDS                                                   
         MVC   SVACCT,VTCACT                                                    
         MVC   SVDATE,VTCEFFD                                                   
         MVC   SVREG,VTCREG                                                     
*                                                                               
GETR50   DS    0H                                                               
         MVC   CHRPRATE,SVRATE     SAVE RATE,                                   
         MVC   CHRPINDS,SVINDS        DECIMALS                                  
         MVC   CHRPDATE,SVDATE     .    DATE AND                                
         MVC   CHRPACCT,SVACCT     .    ACCOUNT                                 
         AP    SVNET,CHRANET                                                    
         AP    SVNON,CHRANON                                                    
         AP    SVCOMM,CHRACOM                                                   
         AP    SVDISC,CHRACSD                                                   
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,CHRPRATE                                                    
         CVD   RE,DUB                                                           
*                                                                               
         ZAP   PK16,CHRANET                                                     
         AP    PK16,CHRACOM                                                     
         AP    PK16,CHRANON                                                     
         SP    PK16,CHRACSD                                                     
         AP    PK16,CHRAGST                                                     
*                                                                               
         CLC   SVPSTPRV,=C'PQ'     IS THIS PROVINCE PQ?                         
         BNE   GETR52              NO, BASIS INCLUDES GST                       
*        CLC   BILDATE,=X'B30101'  YES, IS THIS BILL PRIOR TO 1/01/13?          
*        BL    GETR52              YES, BASIS INCLUDES GST                      
         SP    PK16,CHRAGST        NO, PST BASIS IS LESS GST THEN               
*                                                                               
GETR52   AP    SVBASE,PK16         ACCUMULATE PST BASE                          
         MP    PK16,DUB                                                         
*                                                                               
         TM    CHRPINDS,PBIIDC3    DOES RATE HAVE 3 DECIMALS?                   
         BZ    *+14                NO                                           
         SRP   PK16,64-5,5         YES, SHIFT ONE MORE DIGIT                    
         B     *+10                                                             
         SRP   PK16,64-4,5                                                      
*                                                                               
         AP    SVPST,PK16                                                       
         ZAP   CHRAPST,PK16                                                     
*                                                                               
GETRX    XIT1                                                                   
         DROP  R2,R3,R5                                                         
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  INITIALIZE A LIST OF SCREEN FIELDS WITH STANDARD SCREEN MESSAGES   *         
*  DEFINED IN A SCREEN FIELDS TABLE                                   *         
*                                                                     *         
*  INPUT:                                                             *         
*    R1  -     ADDRESS OF SCREEN FIELDS TABLE                         *         
*                                                                     *         
*    SCREEN FIELDS ENTRY FORMAT:                                      *         
*        A(SCREEN FIELD HEADER)                                       *         
*        F'MESSAGE NUMBER'                                            *         
***********************************************************************         
         SPACE 1                                                                
INITSCR  NMOD1 0,**ISCR**                                                       
         LR    R2,R1               ->   TEXT TABLE                              
*                                                                               
INITSC10 CLI   0(R2),X'FF'         END  OF   TABLE ?                            
         BE    INITSCEX            YES, EXIT                                    
         L     R4,0(,R2)           ->   HEADER                                  
         A     R4,ATWA             RELOCATE                                     
         OI    6(R4),FVOXMT        TRANSMIT  FIELD                              
         L     R3,4(,R2)           GET  MESSAGE   NUMBER                        
         GOTO1 ATXTGET,DMCB,(R3),(R4),0,0                                       
         LA    R2,8(,R2)           BUMP TO   NEXT ENTRY                         
         B     INITSC10            TEST THE  NEXT ENTRY                         
*                                                                               
INITSCEX XIT1  ,                   RETURN    TO   CALLER                        
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  STANDARDIZE TEXT GET ROUTINE                                       *         
*                                                                     *         
*  INPUT:                                                             *         
*    PARM1     AL1 TYPE (DEFAULT "S" SCREEN TYPE)                     *         
*              AL3 TEXT NUMBER                                        *         
*    PARM2     AL1 LENGTH  OF OUTPUT AREA                             *         
*                  OR ZERO IF HEADER FIELD                            *         
*              AL3 ADDRESS OF OUTPUT AREA                             *         
*    PARM3     AL1 LENGTH  OF EXTRA OUTPUT DATA                       *         
*                  OR ZERO IF NO EXTRA DATA                           *         
*              AL3 ADDRESS OF EXTRA OUTPUT DATA                       *         
*    PARM4     AL1 OVERRIDE MESSAGE LANGUAGE                          *         
*                  OR ZERO IF NO MESSAGE OVERRIDE                     *         
*              AL3 ADDRESS OF SUBSTITUTION TABLE                      *         
*                  OR ZERO IF NO SUBSTITUTION TABLE                   *         
***********************************************************************         
         SPACE 1                                                                
         USING GETTXTD,R2                                                       
         SPACE 1                                                                
TXTGET   NMOD1 0,**TGET**                                                       
         LA    R2,DMCB1                                                         
         LR    R3,R1               ->   PARAMETER LIST                          
         XC    GTBLOCK,GTBLOCK     CLEAR     WORK BLOCK                         
         MVC   GTMSGNO,2(R3)       MESSAGE   NUMBER                             
         MVI   GTMTYP,GTMSCR       SCREEN    TYPE (DEFAULT)                     
         CLI   0(R3),C' '          ANY  MESSAGE   TYPE ?                        
         BNH   *+10                NO,  SKIP                                    
         MVC   GTMTYP,0(R3)        GET  MESSAGE   TYPE                          
         MVC   GTMSYS,ASSYSO       ACCOUNT   SYSTEM                             
         CLI   8(R3),0             ANY  EXTRA     OUTPUT    AREA ?              
         BE    TXTGET10            NO,  SKIP                                    
         MVC   GTLTXT,8(R3)        LENGTH    OF   EXTRA     OUTPUT              
         MVC   GTATXT,9(R3)        A(EXTRA   OUTPUT    DATA)                    
*                                                                               
TXTGET10 CLI   12(R3),0            ANY  OVERRIDE  MESSAGE   LANGUAGE ?          
         BE    TXTGET20            NO,  SKIP                                    
         MVC   GTMLANG,12(R3)      GET  OVERRIDE  MESSAGE   LANGUAGE            
*                                                                               
TXTGET20 OC    13(3,R3),13(R3)     ANY  SUBSTITUTION   TABLE ?                  
         BZ    TXTGET30            NO,  SKIP                                    
         MVC   GTASUBST,13(R3)     YES, SUBSTITUTION   TABLE                    
*                                                                               
TXTGET30 OC    4(4,R3),4(R3)                                                    
         BZ    TXTGET50                                                         
         MVC   GTMAXL(4),4(R3)     OUTPUT    LENGTH                             
         SR    RF,RF                                                            
         IC    RF,4(,R3)           GET  LENGTH    OF   OUTPUT    AREA           
         L     RE,4(,R3)           GET  ADDRESS   OF   OUTPUT    AREA           
         CLI   4(R3),0             A    HEADER    FIELD ?                       
         BNE   TXTGET40            NO,  SKIP                                    
         IC    RF,0(,RE)           YES, CALCULATE LENGTH    OF   AREA           
         SH    RF,=H'08'                                                        
         TM    1(RE),X'02'         EXTENDED  FIELD     HEADER ?                 
         BZ    *+8                 NO,  SKIP                                    
         SH    RF,=H'08'                                                        
         LA    RE,8(,RE)           BUMP TO   ACTUAL    FIELD                    
         STCM  RE,7,GTAOUT                                                      
         STC   RF,GTMAXL                                                        
*                                                                               
TXTGET40 OI    GT1INDS,GT1OWRK+GT1NOREF                                         
         LR    R4,RE               SAVE THE  FIELD     ADDRESS                  
         LR    R5,RF               SAVE THE  FIELD     LEN  -    1              
         SH    R5,=H'01'                                                        
         BM    TXTGETEX                                                         
         EXMVC R5,0(R4),SPACES     CLEAR     FIELD                              
*                                                                               
TXTGET50 GOTO1 VGETTXT,GETTXTD     GET  THE  MESSAGE   TEXT                     
*                                                                               
         CLI   12(R3),0            ANY  OVERRIDE  MESSAGE   LANGUAGE ?          
         BE    TXTGETEX            NO,  SKIP                                    
         CLI   PFUPPER,C'M'        MIXED     CASE DATA ?                        
         BE    TXTGETEX            YES, SKIP                                    
*                                                                               
         GOTO1 VGETFACT,DMCB,0,0                                                
*                                                                               
         USING FACTSD,R2           MAP  GETFACT   BLOCK                         
         L     R2,DMCB             ->   GETFACT   BLOCK                         
         ZIC   R1,12(,R3)          GET  OVERRIDE  MESSAGE   LANGUAGE            
         MHI   R1,16               ->   TABLE     OF   ADDRESSES OF             
         A     R1,FAXLATES              SYSTEM    TRANSLATE TABLES              
         L     R1,8(,R1)           ->   UPPER     CASE TABLE                    
         EX    R5,TXTGETTR         UPPER     CASE THE  MESSAGE                  
*                                                                               
TXTGETEX XIT1  ,                   RETURN    TO   CALLER                        
*                                                                               
TXTGETTR TR    0(0,R4),0(R1)       TRANSLATE TO   UPPER     CASE                
*                                                                               
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  GET A LANGUAGE SOFT PRINT TEXT FIELD FROM THE DICTIONARY.          *         
*                                                                     *         
*  INPUT:                                                             *         
*    R1  -     ADDRESS OF FIELD THAT CONTAINS THE FIELD THAT IS TO BE *         
*              TRANSLATED.  THIS FIELD SHOULD HAVE BEEN INITIALIZED   *         
*              VIA A MVCDD STATEMENT.                                 *         
*                                                                     *         
*  OUTPUT:                                                            *         
*              THE TRANSLATED DATA WILL BE IN THE INPUT FIELD.        *         
***********************************************************************         
         SPACE 1                                                                
         USING DICTATED,RE                                                      
         SPACE 1                                                                
GETPFLD  NMOD1 0,**GPFL**                                                       
         LR    R2,R1               ->   FIELD                                   
         LA    RE,DMCB             ->   DMCB                                    
         MVC   DMCB(4),=C'SU  '    UPPER     CASE                               
         CLI   PFUPPER,C'M'        MIXED     CASE FOR  COMPANY ?                
         BNE   *+8                 NO,  SKIP                                    
         MVI   DDRETN,DDCASEL      LOWER     CASE                               
         MVC   DDLANG,PLANG        PRINT     LANGUAGE                           
         CLI   PLANG,X'00'         ANY  PRINT     LANGUAGE ?                    
         BNE   *+8                 YES, SKIP                                    
         MVI   DDLANG,C' '         CLEAR     THE  LANGUAGE                      
         DROP  RE                                                               
*                                  TRANSLATE THE  FIELD                         
         GOTO1 VDICTAT,DMCB,,(R2),0                                             
         XIT1  ,                   RETURN    TO   CALLER                        
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
         SPACE 1                                                                
ACCIO    NMOD1 0,**ACCIO*                                                       
         L     RF,0(,R1)                                                        
         ST    RF,AIOAREA          SAVE A(I/O AREA)                             
         ZIC   RF,DMBYTE           SAVE COMMAND BYTE                            
         SRL   RF,4                                                             
         BCTR  RF,0                MINUS ONE                                    
         SLL   RF,3                TIMES EIGHT                                  
         LA    RE,IOCMNDST                                                      
         AR    RF,RE                                                            
         ST    RF,DMCB             SET  A(COMMAND)                              
         TM    DMBYTE,X'04'                                                     
         BZ    *+8                                                              
         OI    DMCB,X'08'          SET  TO PASS BACK DELETES                    
         TM    DMBYTE,X'02'                                                     
         BZ    *+8                                                              
         OI    DMCB,X'80'          SET  TO READ WITH LOCK                       
         TM    DMBYTE,X'01'                                                     
         BZ    *+10                                                             
         MVC   KEYSAVE,KEY         SAVE KEY                                     
*                                                                               
         GOTO1 VDATAMGR,DMCB,,ACCOUNT,KEY,AIOAREA                               
         MVI   FERN,OK             SET FIELD ERROR NUMBER                       
         CLI   DMCB+8,0                                                         
         BE    ACCIOX                                                           
         MVI   FERN,NOTFOUND                                                    
         TM    DMCB+8,X'10'        TEST N/F                                     
         BO    ACCIOX                                                           
         MVI   FERN,DELETED                                                     
         TM    DMCB+8,X'02'        TEST DELETED                                 
         BO    ACCIOX                                                           
         MVI   FERN,IOERROR        EOF/ERR/DUP/LOCKS                            
*                                                                               
ACCIOX   CLI   FERN,OK             EXIT WITH CC=EQ IF I/O OK                    
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
         SPACE 1                                                                
NWRKIO   NMOD1 0,**WRKIO*                                                       
         ZIC   RF,WRKBYTE                                                       
         SLL   RF,3                                                             
         LA    R2,WRKCMNDS-8(RF)   P1                                           
         LA    R5,WRKSAVE          P4                                           
*                                                                               
         CLI   0(R2),C'B'          BUF/BUR/BUS DONT USE I/O AREA                
         BE    WRKIO1                                                           
         L     R5,0(,R1)                                                        
         ST    R5,AWRKIO                                                        
*                                                                               
WRKIO1   LA    R3,WRKFILE          P2                                           
         LA    RF,WRKEY            P3                                           
         L     R6,ATIA             P5                                           
*                                                                               
         STM   R2,R3,DMCB          P1 AND P2                                    
         ST    RF,DMCB+8           P3                                           
         STM   R5,R6,DMCB+12       P4 AND P5                                    
*                                                                               
         USING UKRECD,R3                                                        
WRKIO2   LA    R3,WRKEY                                                         
         CLC   0(3,R2),LIBRARY     LIBRARY CALLS HAVE SUB-ACTIONS               
         BNE   WRKIO3                                                           
         XC    UKLPARM,UKLPARM                                                  
         MVC   UKLACTN,3(R2)                                                    
         CLC   0(6,R2),LIBRARY     ADD HAS NO REC NUM                           
         BNE   WRKIO2A                                                          
         CLC   WRKPARA,WRKLPARA                                                 
         BH    WRKIO4                                                           
*                                                                               
WRKIO2A  ZIC   R0,WRKPARA                                                       
         AH    R0,NUMHDRS                                                       
         ST    R0,UKLREC1                                                       
*                                                                               
WRKIO3   CLC   0(3,R2),INDEX       CLEAR FOR INDEX CALL                         
         BNE   WRKIO4                                                           
         XC    UKLPARM,UKLPARM                                                  
*                                                                               
WRKIO4   NC    WRKSTAT,6(R2)       PRE-SET STATUS FOR ACTION                    
         CLI   6(R2),0                                                          
         BNE   WRKIO6                                                           
         XC    WRKSAVE,WRKSAVE                                                  
*                                                                               
WRKIO6   TM    7(R2),X'80'         READ FOR UPDATE                              
         BZ    *+8                                                              
         OI    DMCB,X'80'                                                       
         GOTO1 VDATAMGR,DMCB       CALL & ERROR CHECKS                          
         MVI   FERN,OK                                                          
         CLI   DMCB+8,0                                                         
         BE    WRKIO7                                                           
         MVI   FERN,NOTFOUND                                                    
         TM    DMCB+8,X'10'                                                     
         BO    WRKIOX                                                           
         MVI   FERN,IOERROR                                                     
         B     WRKIOX                                                           
*                                                                               
WRKIO7   OC    WRKSTAT,7(R2)       UPDATE STATUS FOR ACTION                     
*                                                                               
         USING PARAD,R5                                                         
         CLI   PARAEL,X'01'                                                     
         BNE   WRKIO8                                                           
*                                                                               
*                                  USING THE OLD PARAGRAPH RECS ?               
         CLI   PARALEN,PARALNQ-WRKIOFLQ                                         
         BNE   WRKIO8              NO                                           
         BAS   RE,WRKIOFIX         INSERT PST BUCKET                            
*                                                                               
WRKIO8   CLC   0(3,R2),LIBRARY     UPDATE LAST PARA NUM FOR LIBRARY             
         BNE   WRKIOX              CALLS AND SAVE PARA STATUS                   
         CLI   PARAEL,X'01'                                                     
         BNE   *+10                                                             
         MVC   SAVPSTAT,PARASTAT                                                
         L     R0,UKLRECS                                                       
         SH    R0,NUMHDRS                                                       
         STC   R0,WRKLPARA                                                      
         BP    WRKIOX                                                           
         CLI   ACTION,DEL          UNLESS ACTION IS NOT DELETE                  
         BNE   WRKIOX              (EG CHANGE ALTERING REC LEN)                 
*                                                                               
         LA    R2,DELETE           DELETE BOOK IF NO PARAS LEFT                 
         B     WRKIO1                                                           
*                                                                               
WRKIOX   CLI   FERN,OK             EXIT CC=EQU IF OK                            
         XIT1                                                                   
         DROP  R3,R5                                                            
         EJECT ,                                                                
         SPACE 1                                                                
WRKCMNDS DS    0CL8                ACTION/SUBACTION/WRKSTAT INIT & SET          
         DC    C'BUF   ',X'0000'   1ST STATUS VALUE IS FOR AND-ING BFRE         
         DC    C'BUR   ',X'FF01'   2ND STATUS VALUE IS FOR OR-ING AFTER         
         DC    C'BUS   ',X'FF00'   X'01' = BUFFER IS IN USE                     
INDEX    DC    C'IND   ',X'0001'   X'02' = WRITE PENDING                        
         DC    C'OPE   ',X'0000'   X'80' = READ FOR UPDATE                      
         DC    C'ADD   ',X'FF03'                                                
         DC    C'CLO   ',X'FD00'                                                
DELETE   DC    C'DEL   ',X'0000'                                                
         DC    C'SEQ   ',X'0000'                                                
LIBRARY  DC    C'LIBADD',X'FF02'                                                
         DC    C'LIBGET',X'FF00'                                                
         DC    C'LIBGET',X'FF80'   READ FOR UPDATE                              
         DC    C'LIBPUT',X'FF02'                                                
         DC    C'LIBDEL',X'FF02'                                                
         DC    C'LIBCLO',X'FD00'                                                
         DC    C'RETAIN',X'0000'                                                
         DC    C'RANDOM',X'0000'                                                
*                                                                               
WRKFILE  DC    C'ACCWRK  '                                                      
*                                                                               
NUMHDRS  DC    H'1'                NUMBER OF HDR RECORDS PER WORKFILE           
         EJECT ,                                                                
***********************************************************************         
*  FIX THE PARAGRAPH RECORD IN 0(R5)                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING PARAD,R5                                                         
         SPACE 1                                                                
WRKIOFIX NTR1  WORK=(R2,WKFXLN)                                                 
*                                                                               
         OI    WRKSTAT2,WRKFIXED   WORKER FILE FIXED                            
*                                                                               
         LR    R8,R2               SAVE   R2   IN   R8                          
         XR    R1,R1                                                            
         LA    R3,WKFXLN                                                        
         MVCL  R2,R0                                                            
*                                                                               
         LR    R2,R8               RESTORE     R2                               
         MVC   0(PARANET-PARAD,R2),0(R5)  COPY GOOD PART OF RECORD              
*                                                                               
         LA    R2,PARANET-PARAD(,R2)      ->   WORK COPY OF PARAGRAPH           
         LA    R6,PARANET-PARAD(,R5)      ->   OLD  COPY OF PARAGRAPH           
         LA    R3,PTOT#                   NUMBER    OF   TOTALS                 
*                                                                               
WRKIOF10 ZAP   0(PLAMTLNQ,R2),0(6,R6)     MOVE PARAGRAPH AMOUNT                 
         LA    R2,PLAMTLNQ(,R2)           ->   NEXT WORK AMOUNT                 
         LA    R6,6(,R6)                  ->   NEXT OLD  AMOUNT                 
         BCT   R3,WRKIOF10                FIX  NEXT AMOUNT                      
*                                         NOTE:     R6   POINTS   TO            
*                                                   REST OF   OLD DATA          
*                                                                               
WRKIOFLQ EQU   (PLAMTLNQ-6)*PTOT#         NUMBER    OF   BYTES    ADDED         
*                                                                               
         LR    R2,R8               RESTORE     R2                               
         XR    R3,R3               GET    OLD  RECORD    LENGTH                 
         ICM   R3,3,PARARLEN                                                    
         LA    R7,WRKIOFLQ(,R3)    GET    NEW  RECORD    LENGTH                 
         STH   R7,PARARLEN-PARAD(,R2)     SAVE NEW  RECORD    LENGTH            
*                                                                               
         MVI   PARALEN-PARAD(R2),PARALNQ  SAVE NEW  PARAGRAPH LENGTH            
*                                                                               
*                                  GET    LENGTH    OF   REMAINDER              
         SH    R3,=Y(PARANET-PARAD+6*PTOT#)                                     
         LR    R7,R3               AMOUNT LEFT TO   MOVE                        
*                                  R6     SET  EARLIER                          
         LA    R2,PARASTAT-PARAD(,R2)     ADDR OF   STAT IN   WORK              
         MVCL  R2,R6                                                            
*                                  R8     ->   WORK RECORD                      
         LH    R9,PARARLEN-PARAD(,R8)     GET  NEW  LENGTH                      
         LR    R3,R9                      GET  NEW  LENGTH                      
         LR    R2,R5               ->     OLD  RECORD                           
         MVCL  R2,R8               UPDATE OLD  RECORD                           
*                                                                               
         XIT1                                                                   
         DROP  R5                                                               
*                                                                               
WKFXLN   EQU   2048                                                             
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  ROUTINE TO GET AGENCY NAME AND ADDRESS                             *         
***********************************************************************         
         SPACE 1                                                                
GETAGCY  NMOD1 0,**GETAG**                                                      
         OC    USERNAME,USERNAME   TEST IF I HAVE BEEN HERE BEFORE              
         BNZ   GETAGCYX            YES                                          
         XC    KEY,KEY                                                          
*                                                                               
         USING CTIKEY,R3                                                        
         LA    R3,KEY                                                           
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,TWAUSRID    READ ID RECORD                               
*                                                                               
         GOTO1 VDATAMGR,DMCB,=CL8'DMREAD',CTFILE,KEY,AIOAREA3                   
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',CTFILE),('CTORGELQ',AIOAREA3),0                
         CLI   12(R1),0            TEST IF FOUND                                
         BNE   GETAGCYX            NO                                           
*                                                                               
         USING CTORGD,R3                                                        
         L     R3,12(,R1)                                                       
         MVC   USERNAME,CTORGNAM                                                
         MVC   USERADDR,CTORGADD                                                
         DROP  R3                                                               
*                                                                               
GETAGCYX XIT1                                                                   
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  GET CURRENCY CODE FROM TABLE OF ALL COUNTRIES                      *         
***********************************************************************         
         SPACE 1                                                                
GETCUR   NMOD1 0,**GETCUR**                                                     
         L     R1,=A(CURTAB)                                                    
         A     R1,RELO                                                          
*                                                                               
GETC02   CLI   0(R1),0             END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   AGYCTRY,0(R1)       MATCH ON COUNTRY CODE                        
         BE    GETC04                                                           
         LA    R1,L'CURTAB(,R1)                                                 
         B     GETC02                                                           
*                                                                               
GETC04   MVC   CURCOD,1(R1)                                                     
         XIT1                                                                   
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  UPDATE TRXEL BILL PENDING STATUS                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING TRXELD,R2                                                        
         USING PTAELD,R3                                                        
         SPACE 1                                                                
TRXELEM  DS    0H                  UPDATE TRXEL X'75'                           
         NMOD1 0,**TRXELEM*                                                     
         MVI   BYTE,0                                                           
*                                                                               
         LR    R2,R5               A(RECORD)                                    
         AH    R2,DATADISP                                                      
         SR    RF,RF                                                            
*                                                                               
TRX10    CLI   0(R2),0                                                          
         BE    TRX20                                                            
         CLI   0(R2),TRXELQ        X'75' TRXEL                                  
         BE    TRX30                                                            
         IC    RF,1(,R2)                                                        
         AR    R2,RF                                                            
         B     TRX10                                                            
*                                                                               
TRX20    OI    BYTE,X'80'          NEED TO ADD ELEM                             
         XC    TEMP(TRXLN1Q),TEMP  BUILD IN TEMP                                
         LA    R2,TEMP                                                          
         MVI   TRXEL,TRXELQ        X'75'                                        
         MVI   TRXLN,TRXLN1Q                                                    
*                                                                               
TRX30    NI    TRXSTA2,X'FF'-TRXSBILP   NO TRANS BILL ACTIVITY PENDING          
         LR    R3,R5                    A(RECORD)                               
         AH    R3,DATADISP              LOOK FOR X'77' ELEMS                    
         SR    RF,RF                                                            
*                                                                               
TRX40    CLI   0(R3),0                                                          
         BE    TRX60                                                            
         CLI   0(R3),PTAELQ        X'77'                                        
         BNE   TRX50                                                            
         TM    PTASTAT1,PTASPEND   ACTIVITY PENDING                             
         BNO   *+8                                                              
         OI    TRXSTA2,TRXSBILP                                                 
*                                                                               
TRX50    IC    RF,1(,R3)                                                        
         AR    R3,RF                                                            
         B     TRX40                                                            
*                                                                               
TRX60    TM    BYTE,X'80'          ADD ELEM                                     
         BNO   TRXX                                                             
         ST    R5,FULL                                                          
         GOTO1 AADDELM,FULL                                                     
*                                                                               
TRXX     DS    0H                                                               
         XIT1  ,                                                                
         DROP  R2,R3                                                            
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  LOAD ADDTRN AND INITIALIZE ADDTRN I/O AREAS                        *         
***********************************************************************         
         SPACE 1                                                                
         USING LWSD,RC             MAP  LOCAL     WORKING   STORAGE             
LDADTRAN DS    0H                  LOAD ADDTRN    AND  ACBIL40                  
         NMOD1 0,**LADT**                                                       
         L     RC,ABASEC                                                        
*                                                                               
         USING COMFACSD,R5                                                      
         L     R5,ACOMFACS                                                      
         XC    DMCB(12),DMCB                                                    
*                                  READ FROM ACCPAK                             
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QADDTRN      ADDTRN    PHASE     NUMBER                   
         GOTO1 CCALLOV,DMCB        LOAD ADDTRN                                  
         MVC   ADDTRN,0(R1)        SAVE A(ADDTRN)                               
*                                                                               
*                                  LOAD ACBIL40                                 
         GOTO1 VCALLOV,DMCB,('OVPHIADT',0),0,0                                  
         CLI   4(R1),X'FF'         LOADED OKAY ?                                
         BNE   *+6                 YES, CONTINUE                                
         DC    H'0'                NO,  DUMP                                    
*                                                                               
         L     R1,0(,R1)           R1=  A(ACBIL40)                              
         LA    R1,0(,R1)           CLEAR HIGH-ORDER BYTE                        
         LM    R2,R3,0(R1)         R2=  1ST  ADDRESS                            
         AR    R2,R1               R3=  NUM  OF   ADDRESSES                     
*                                                                               
LD4010   LM    RE,RF,0(R2)         RE=  ADDR OF   AREA                          
*                                  RF=  ADDR IN   GWS                           
         AR    RE,R1               ->   AREA                                    
         ST    RE,0(RF,R9)         SAVE IN   GWS                                
         LA    R2,8(,R2)           NEXT I/O  ADDR SET                           
         BCT   R3,LD4010           GET  NEXT I/O  ADDR SET                      
*                                                                               
         MVC   TRNACC,AACTIO       SET  I/O  ADDRESSES                          
         MVC   TRNBUK,ABUKIO                                                    
         MVC   TRNCAC,ACNTIO                                                    
         MVC   TRNOFA,AOFFIO                                                    
*                                                                               
         MVC   TRNREC,ATIO         INIT ADDTRNS   BLOCK                         
         MVC   TRNCOMF,ACOMFACS                                                 
         MVC   TRNCPYS1,COMPSTA1                                                
         MVC   TRNCPYS2,COMPSTA2                                                
         MVC   TRNCPYS3,COMPSTA3                                                
         MVC   TRNCPYS4,COMPSTA4                                                
         MVC   TRNCPYS5,COMPSTA5                                                
         MVC   TRNCPYS6,COMPSTA6                                                
         MVC   TRNCPYS7,COMPSTA7                                                
         MVC   TRNCPYS8,COMPSTA8                                                
         MVC   TRNCPYS9,COMPSTA9                                                
         MVC   TRNCPYSA,COMPSTAA                                                
         MVC   TRNPUSER,TWAUSRID                                                
         MVC   TRNGLMOA,COMPGMOA                                                
*                                                                               
         LA    R0,PALAREA                                                       
         STCM  R0,15,TRNPAL                                                     
*                                  DRAFT/NEW TRANSACTIONS                       
         MVI   TRNINDS,TRNIDRFT+TRNICONV                                        
         MVI   TRNMODE,TRNMONLN+TRNMEMUY                                        
         MVI   TRNINDS2,TRNIADDG                                                
*                                                                               
         GOTO1 CGETFACT,DMCB,(X'80',0),F#UTLD                                   
         L     R1,0(,R1)                                                        
*                                                                               
         USING F@UTLD,R1                                                        
         TM    F@TTEST,X'80'       TEST UPD=NO                                  
         BNO   *+8                                                              
         OI    TRNINDS,TRNINDIR    SET DO NOT RE-READ DIRECTORY                 
*                                                                               
         GOTO1 CGETFACT,DMCB,(X'80',0),F#SELISD                                 
         L     R1,0(,R1)                                                        
*                                                                               
         USING F@SELISD,R1                                                      
         TM    F@SEIND,X'04'       TEST READ ONLY                               
         BNO   *+8                                                              
         OI    TRNINDS,TRNINDIR    SET DO NOT RE-READ DIRECTORY                 
         XIT1                                                                   
         DROP  R1,R5,RC                                                         
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  BATCH RECORD ADD ELEMENT LAST                                      *         
*                                                                     *         
*  INPUT:                                                             *         
*    R1       - ADDRESS     OF   ELEMENT                              *         
*    TIO      - TRANSACTION I/O  AREA                                 *         
*                                                                     *         
*  ADDRESS OF THIS SUBROUTINE IN:                                     *         
*    ABADDLST IN GWS                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
BTADDLST DS    0H                  BATCH     RCD  ADD  ELEMENT   LAST           
         NMOD1 0,**BADL**                                                       
*                                                                               
         LR    R3,R1               SAVE A(ELEMENT)                              
*                                  ADD  EL   TO   END  OF   RECORD              
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),ATIO,(R3),ADDEND,0                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XIT1                      RETURN    TO   CALLER                        
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  BATCH DELETE DIRECTORY POINTERS FOR UNBILLING                      *         
*                                                                     *         
*  INPUT:                                                             *         
*    BTKEY - BATCH KEY AREA                                           *         
*    BTPPK - PASSIVE POINTER KEY AREA                                 *         
*                                                                     *         
*  OUTPUT:                                                            *         
*    BTKEY - BATCH KEY AREA                                           *         
*                                                                     *         
*  USES:                                                              *         
*    BTDIO - DIRECTORY I/O KEY                                        *         
*                                                                     *         
*  CALLS:                                                             *         
*    BTBLDKHD - BUILD KEY FOR BATCH HDR RCD                           *         
*    BTDELDRF - BATCH DELETE DRAFT TRANSACTION ITEMS                  *         
*                                                                     *         
*  ADDRESS OF THIS SUBROUTINE IN:                                     *         
*    ABDELDPT IN GWS                                                  *         
*                                                                     *         
***********************************************************************         
                                                                                
         USING TBARECD,R2                                                       
         USING LWSD,RC                                                          
BTDELDPT DS    0H                  DELETES FOR UNBILLING                        
         NMOD1 0,**BDDP**                                                       
         L     RC,ABASEC                                                        
         L     R2,ABTKEY           BATCH KEY                                    
         LR    R3,R2                                                            
         L     R4,ABTDIO                                                        
*                                  READ BATCH HEADER DIRECTORY                  
         GOTO1 =A(BTBLDKHD),RR=RELO                                             
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),ACCDIR,TBAKEY,(R4)                  
         CLI   8(R1),0             TEST BATCH KEY FOUND                         
         BNE   BTDELPEX            NO, NOT BILLED TODAY                         
         LR    R2,R4                                                            
*                                                                               
BTDELP20 OI    TBAKSTA,TBAHSDEL    MARK DELETED & REWRITE                       
         GOTO1 VDATAMGR,DMCB,DMWRT,ACCDIR,(R4),(R4)                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,ABTDIO           READ BATCH HEADER RECORD                     
         MVC   DA,TBAKDA-TBAKEY(R2)                                             
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),ACCMST,DA,AIOAREA2,ADMWORK          
         TM    8(R1),0                                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIOAREA2                                                      
         OI    TBARSTA,TBAHSDEL    MARK THIS DELETED ALSO                       
         GOTO1 VDATAMGR,DMCB,(0,PUTREC),ACCMST,DA,AIOAREA2,ADMWORK              
         TM    8(R1),0                                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                  READ NEXT HEADER DIRECTORY RECORD            
         GOTO1 VDATAMGR,DMCB,DMRSEQ,ACCDIR,(R4),(R4)                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  SAME BATCH ?                                 
         CLC   0(TBAKTSEQ-TBAKEY,R4),0(R3)                                      
         BNE   BTDELP40            NO, READ PASSIVE POINTERS                    
         BAS   RE,BTDELDRF         DELETE DRAFT TRANSACTIONS                    
                                                                                
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),ACCDIR,(R4),(R4)                    
         CLI   8(R1),0             RE-READ LAST RECORD                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R2,R4                                                            
         B     BTDELP20            AND MARK IT DELETED                          
*                                                                               
BTDELP40 L     R2,ABTPP            READ PASSIVE POINTER                         
*                                  READ FOR UPDATE ACCDIR                       
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),ACCDIR,TBAPAS,(R4)                  
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LR    R2,R4                                                            
         OI    TBAKSTA,TBAHSDEL    DELETE    PASSIVE                            
*                                  REWRITE   ACCDIR                             
         GOTO1 VDATAMGR,DMCB,DMWRT,ACCDIR,(R4),(R4)                             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BTDELPEX XIT1                      RETURN    TO   CALLER                        
         EJECT ,                                                                
***********************************************************************         
*  BATCH DELETE DRAFT TRANSACTION ITEMS                               *         
*                                                                     *         
*  INPUT:                                                             *         
*    BTDIO   - DIRECTORY I/O KEY                                      *         
*                                                                     *         
*  USES:                                                              *         
*    DA      - DIRECTORY ADDRESS                                      *         
*    BTDIO2  - DIRECTORY I/O KEY 2                                    *         
*    DMWORK  - DATA MANAGER WORK AREA                                 *         
*    IOAREA3 - I/O AREA 3                                             *         
*    KEY     - I/O KEY                                                *         
*                                                                     *         
***********************************************************************         
                                                                                
         USING TBARECD,R2                                                       
BTDELDRF DS    0H                                                               
         NTR1  ,                                                                
         L     R2,ABTDIO           GET BATCH ITEM RECORD                        
         MVC   DA,TBAKDA-TBAKEY(R2)                                             
         GOTO1 VDATAMGR,DMCB,(0,GETREC),ACCMST,DA,AIOAREA3,ADMWORK              
         TM    8(R1),0                                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R2,AIOAREA3                                                      
         LA    R3,TBARFST          GET ACCOUNT SYSTEM KEY ELEMENT               
         USING ASKELD,R3                                                        
         SR    R0,R0                                                            
                                                                                
BTDELD20 CLI   ASKEL,0             END OF RECORD ?                              
         BE    BTDELDEX            YES, RETURN                                  
         CLI   ASKEL,ASKELQ        FOUND IT?                                    
         BE    BTDELD40            YES, PROCESS IT                              
                                                                                
BTDELD30 SR    R0,R0                                                            
         IC    R0,ASKLN            GET NEXT ELEMENT                             
         AR    R3,R0                                                            
         B     BTDELD20                                                         
                                                                                
BTDELD40 MVC   KEY,ASKKEY          GET KEY OF DRAFT TRANSACTION                 
*                                  READ FOR UPDATE ACCDIR                       
         GOTO1 VDATAMGR,DMCB,(X'80',DMREAD),ACCDIR,KEY,BTDIO2                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TRNRECD,R2                                                       
         LA    R2,BTDIO2           READ DIRECTORY RECORD                        
         TM    TRNKSTAT,TRNSDRFT   MUST BE DRAFT                                
         BO    *+6                                                              
         DC    H'0'                                                             
         OI    TRNKSTAT,TRNSDELT   MARK IT DELETED                              
                                                                                
         GOTO1 VDATAMGR,DMCB,DMWRT,ACCDIR,BTDIO2,BTDIO2                         
         CLI   8(R1),0             WRITE BACK DIRECTORY                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
                                                                                
         USING ACTRECD,R2                                                       
         LA    R2,KEY                                                           
         MVC   KEY,SPACES                                                       
         MVC   ACTKCULA,ASKKEY     READ ACCOUNT DIRECTORY RECORD                
                                                                                
         GOTO1 VDATAMGR,DMCB,(0,DMREAD),ACCDIR,KEY,BTDIO2                       
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DA,BTDIO2+(ACTKDA-ACTKEY)                                        
                                                                                
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),ACCMST,DA,AIOAREA2,ADMWORK          
         TM    8(R1),0             READ ACCOUNT RECORD NOW                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ASTELD,R4                                                        
         L     R2,AIOAREA2                                                      
         LA    R4,ACTRFST          GET STATUS ELEMENT                           
         SR    R0,R0                                                            
*                                                                               
BTDELD60 CLI   ASTEL,0             EOR?                                         
         BE    BTDELD30            YES, EXIT                                    
         CLI   ASTEL,ASTELQ        FOUND STATUS EL ?                            
         BE    BTDELD80            YES, PROCESS IT                              
         IC    R0,ASTLN            GET NEXT ELEMENT                             
         AR    R4,R0                                                            
         B     BTDELD60                                                         
                                                                                
BTDELD80 SR    RF,RF                                                            
         ICM   RF,7,ASTDRAFT       UPDATE DRAFT RECORD COUNT                    
         BCTR  RF,0                                                             
         STCM  RF,7,ASTDRAFT                                                    
                                                                                
         GOTO1 VDATAMGR,DMCB,(0,PUTREC),ACCMST,DA,AIOAREA2,ADMWORK              
         TM    8(R1),0            REWRITE RECORD AND GET NEXT ASKEL             
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     BTDELD30                                                         
                                                                                
BTDELDEX XIT1                      RETURN TO CALLER                             
         DROP  R2,R3,R4,RC                                                      
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*  BUILD KEY FOR BATCH HEADER RECORD                                  *         
*                                                                     *         
*  INPUT:                                                             *         
*    BILNUMP  - BILL    NUMBER  PACKED    FOR  BILLING                *         
*    BTTYPE   - BATCH   TYPE                                          *         
*    PHASE    - OVERLAY PHASE   NUMBER                                *         
*    WONUMCHR - WRITE-OFF       NUMBER    FOR  WRITE-OFF              *         
*                                                                     *         
*  OUTPUT:                                                            *         
*    BATCHTOT - BATCH   TOTAL   SET  TO   ZERO                        *         
*    BTCHREF  - BATCH   REFERENCE    ID                               *         
*    BTDRTOT  - BATCH   DR      TOTAL     SET  TO   ZERO              *         
*    BTITEMC  - ITEM    COUNT   SET  TO   ZERO                        *         
*    BTKEY    - BATCH   KEY     AREA                                  *         
*    BTPPK    - PASSIVE POINTER KEY  AREA                             *         
*                                                                     *         
*  WORK AREA:                                                         *         
*    DUB      - DOUBLE  WORD    WORK AREA                             *         
*                                                                     *         
*  CALLS:                                                             *         
*    BINITRCD - INITIALIZE      1ST/NEXT  BATCH ITEM RECORD           *         
*                                                                     *         
*  ADDRESS OF THIS SUBROUTINE IN:                                     *         
*    ABBLDKHD IN GWS                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TBARECD,R2          MAP  TRANSACTION    BATCH     RCDS           
         SPACE 1                                                                
BTBLDKHD DS    0H                  BUILD     KEY  FOR  BATCH    HDR RCD         
         NMOD1 0,**BBKY**                                                       
*                                                                               
         L     R2,ABTKEY           ->   BATCH     KEY  I/O  AREA                
*                                  CLEAR     THE  AREA                          
         XC    0(L'TBAKEY,R2),0(R2)                                             
         XC    BTITEMC,BTITEMC     AND  SEQUENCE  NUMBER                        
         ZAP   BATCHTOT,=P'0'                                                   
         ZAP   BTDRTOT,=P'0'                                                    
*                                                                               
         MVI   TBAKTYP,TBAKTYPQ                                                 
         MVC   TBAKCPY,COMPANY     COMPANY                                      
         MVC   TBAKUSER,TWAUSRID   USER                                         
         MVC   TBAKADDT,TODAYC     TODAY                                        
         XC    TBAKADDT,=X'FFFF'                                                
         MVI   TBAKGRUP,TBAGPRDQ   GROUP=P                                      
         MVC   TBAKBTYP,BTTYPE     INPUT     TYPE                               
         MVC   TBAKBMOS,PMOS       MOS  (PACKED)                                
         MVC   TBAKBCHR,FACPID     PERSON ID                                    
         CLI   PBILMOS,0           ANY  MOS  OVERRIDE ?                         
         BE    BTBLDK10            NO,  SKIP                                    
         MVC   TBAKBMOS,PBILMOS    USE  MOS  OVERRIDE                           
*                                                                               
BTBLDK10 DS    0H                                                               
         CLI   PHASE,OVPHBIL       BILL OVERLAY ?                               
         BNE   BTBLDK20            NO,  SKIP                                    
         OI    BILNUMP+3,X'0F'     BILLING,  USE  BILNUMP   FOR                 
         UNPK  DUB,BILNUMP                   LAST FOUR CHARACTERS               
         MVC   TBAKBREF,DUB+4                OF   CONTROL   NUMBER              
         B     BTBLDK40            CONTINUE                                     
*                                                                               
*                                  WRITE-OFF,     USE  WONUMCHR  FOR            
*                                            LAST FOUR CHARACTERS               
BTBLDK20 MVC   TBAKBREF,WONUMCHR             OF   CONTROL   NUMBER              
*                                                                               
BTBLDK40 MVC   BTCHREF(2),BILMOS   MOS  (CHARACTER)                             
*                                  TRANSACTION    BATCH     REFERENCE           
         MVC   BTCHREF+2(4),TBAKBREF    NUMBER                                  
*                                                                               
P        USING TBAPAS,R3           MAP  BATCH     HDR  PASSIVE POINTERS         
         L     R3,ABTPP            ->   BATCH     HDR  PASSIVE POINTERS         
         XC    0(TBAKDA-TBARECD+L'TBAKDA,R3),0(R3)                              
         MVI   P.TBAPTYP,TBAPTYPQ  RECORD    TYPE                               
         MVC   P.TBAPCPY,TBAKCPY   COMPANY   CODE                               
         MVC   P.TBAPUSER,TBAKUSER USER                                         
         MVC   P.TBAPEFDT,TODAYC   EFFECTIVE      DATE                          
         MVC   P.TBAPGRUP,TBAKGRUP GROUP                                        
         MVC   P.TBAPBTYP,TBAKBTYP BATCH     TYPE                               
         MVC   P.TBAPBMOS,TBAKBMOS BATCH     MOS                                
         MVC   P.TBAPBREF,TBAKBREF BATCH     REFERENCE                          
         MVC   P.TBAPBCHR,TBAKBCHR BATCHER                                      
         MVC   P.TBAPSEQN,TBAKSEQN BATCH     SEQUENCE                           
         MVC   P.TBAPOFFC,TBAKOFFC OFFICE                                       
         MVC   P.TBAPTSEQ,TBAKTSEQ TRANSACTION   (ITEM)  SEQ (HEADER=0)         
*                                                                               
*                                  INIT 1ST/NEXT  BATCH     ITEM RECORD         
         GOTO1 =A(BINITRCD),RR=RELO                                             
         XIT1                      RETURN    TO   CALLER                        
*                                                                               
         DROP  R2,P                                                             
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  ADD BATCH HEADER RECORD                                            *         
*                                                                     *         
*  INPUT:                                                             *         
*    BATCHTOT - BATCH   TOTAL                                         *         
*    BTITEMC  - BATCH   ITEM    COUNT                                 *         
*    BTKEY    - BATCH   KEY     AREA                                  *         
*                                                                     *         
*  OUTPUT:                                                            *         
*    BIO      - BATCH   HEADER  I/O  AREA                             *         
*    BTDIO    - DIRECTORY       I/O  KEY                              *         
*    BTPPK    - PASSIVE POINTER KEY  AREA                             *         
*                                                                     *         
*  USES:                                                              *         
*    ELEMENT  - ELEMENT BUILD        AREA                             *         
*                                                                     *         
*  CALLS:                                                             *         
*    BHADDLST - BATCH   HEADER       RECORD         ADD ELEMENT LAST  *         
*    BTADDREC - CALL    DATAMGR'S    ADDREC/PUTREC  FOR BATCH RECORDS *         
*                                                                     *         
*  ADDRESS OF THIS SUBROUTINE IN:                                     *         
*    ABADDHDR IN GWS                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
BTADDHDR DS    0H                  ADD  BATCH     HEADER    RECORD              
         NMOD1 0,**BADH**                                                       
*                                                                               
         L     R2,ABIO             ->   BATCH     HDR  I/O  AREA                
         LR    RE,R2                                                            
         LA    RF,RECLNQ           CLEAR     I/O  AREA                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING TBARECD,R2          MAP  TRANSACTION    BATCH     RCDS           
         L     R2,ABIO             ->   BATCH     HDR  I/O  AREA                
         L     R3,ABTKEY           ->   BATCH     KEY       AREA                
         MVC   TBAKEY,0(R3)        MOVE BATCH     KEY                           
*                                                                               
*                                  STATUS   (ENDED     &    APPROVED)           
         MVI   TBARHSTA,TBAHSEND+TBAHSAPR                                       
         MVC   TBAHRADT,TODAYC     BATCH     ADDED                              
         MVC   TBAHREDT,TODAYC     EFFECTIVE DATE (TODAY)                       
*                                                                               
         USING BHDELD,R3           MAP       BATCH     HEADER    EL             
         LA    R3,ELEMENT          CLEAR     THE  ELEMENT                       
         XC    ELEMENT,ELEMENT                                                  
*                                                                               
*                                  BUILD     BATCH     HEADER    EL             
         MVI   BHDEL,BHDELQ                                                     
         MVI   BHDLN,BHDLN2Q                                                    
         MVC   BHDNAME,CLIBIL      CLI/BIL   AS   DEFAULT                       
         CLI   ACTION,WRT          WRITE-OFF ?                                  
         BNE   BTADDH10            NO,  TRY  RECOVERY                           
         MVC   BHDNAME,WRITEOFF    WRITE-OFF                                    
         B     BTADDH20                                                         
*                                                                               
BTADDH10 CLI   ACTION,REC          RECOVERY ?                                   
         BNE   BTADDH20            NO,  KEEP DEFAULT                            
         MVC   BHDNAME,RECOVERY    RECOVERY                                     
*                                                                               
BTADDH20 SR    RF,RF               GET  NUMBER    OF   ITEMS                    
         MVC   BHDITEMC,BTITEMC    NUMBER    OF   ITEMS                         
         MVC   BHDITEMA,BTITEMC                                                 
         ZAP   BHDCASHC,BATCHTOT   TOTAL     CASH                               
         ZAP   BHDCASHA,BATCHTOT                                                
         ZAP   BHDTOTDR,BATCHTOT                                                
         ZAP   BHDTOTCR,BATCHTOT                                                
         OI    BHDSTAT1,BHDSNREV   NO   REVERSALS                               
*                                                                               
*                                  ADD  EL   TO   END  OF   RECORD              
         GOTO1 =A(BHADDLST),(R3),RR=RELO                                        
         DROP  R3                                                               
*                                                                               
*                                  ADDREC/PUTREC                                
         GOTO1 =A(BTADDREC),RR=RELO                                             
*                                                                               
P        USING TBAPAS,R3           MAP  BATCH     HDR  PASSIVE   PTRS           
         L     R3,ABTPP            BATCH     HEADER    PASSIVE   PTRS           
         L     R4,ABTDIO           BATCH     HDR  DIRECTORY I/O  AREA           
         MVC   P.TBAKSTA,TBARSTA   RECORD    STATUS                             
         MVC   P.TBAKDA,DA         DISK ADDRESS                                 
*                                                                               
*                                  READ FOR  UPDATE    AND  READ DEL'ED         
         GOTO1 VDATAMGR,DMCB,(X'88',DMREAD),ACCDIR,P.TBAPAS,(R4)                
         LA    RF,DMADD                                                         
         TM    8(R1),X'10'         TEST RECORD    NOT  FOUND ?                  
         BO    BTADDH50            YES, OK   TO   ADD                           
*                                  TEST DELETED ?                               
         TM    TBAKSTA-TBARECD(R4),TBAHSDEL                                     
         BO    *+6                                                              
         DC    H'0'                DUPLICATE RECORD                             
*                                                                               
         LA    RF,DMWRT            RECORD    ON   FILE -    REWRITE             
*                                                                               
*                                  WRITE     OR   REWRITE   ACCDIR              
BTADDH50 GOTO1 VDATAMGR,DMCB,(RF),ACCDIR,P.TBAPAS,P.TBAPAS                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XIT1                      RETURN    TO   CALLER                        
*                                                                               
         DROP  R2,P                                                             
*                                                                               
CLIBIL   DC    CL(L'BHDNAME)'CLI/BIL'                                           
WRITEOFF DC    CL(L'BHDNAME)'WRITE-OFF'                                         
RECOVERY DC    CL(L'BHDNAME)'RECOVERY'                                          
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  BATCH ADD DRAFT TRANSACTION ELEMENT TO BATCH ITEM RECORD           *         
*                                                                     *         
*  INPUT:                                                             *         
*    ACCSYSK# - ACCESS  SYSTEM  KEY  SEQUENCE  NUMBER                 *         
*    BATCHTOT - BATCH   TOTAL                                         *         
*    BTDRTOT  - BATCH   DR      TOTAL                                 *         
*    TIO      - TRANSACTION     I/O  AREA                             *         
*    TRNBLK   - TRANSACTION     I/O  AREA FOR  ADDTRN                 *         
*                                                                     *         
*  OUTPUT:                                                            *         
*    ACCSYSK# - ACCESS  SYSTEM  KEY  SEQUENCE  NUMBER                 *         
*    BATCHTOT - BATCH   TOTAL                                         *         
*    BTDRTOT  - BATCH   DR      TOTAL                                 *         
*                                                                     *         
*  USES:                                                              *         
*    ELEMENT  - ELEMENT BUILD        AREA                             *         
*                                                                     *         
*  CALLS:                                                             *         
*    ADDTRN   - ADD     TRANSACTION  I/O                              *         
*    BHADDLST - BATCH   HEADER       RECORD    ADD  ELEMENT LAST      *         
*                                                                     *         
*  ADDRESS OF THIS SUBROUTINE IN:                                     *         
*    ABADDRFT IN GWS                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
BADDRAFT NMOD1 0,**BADF**          ADD DRAFT TRANSACTION FOR BATCH              
         OI    TRNINDS2,TRNIADDG                                                
         NI    TRNINDS2,X'FF'-TRNIUPDG                                          
         GOTO1 ADDTRN,TRNBLK                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TRNRECD,R4                                                       
         USING TRNELD,R5                                                        
         L     R4,ATIO             A(TRANSACTION I/O AREA)                      
         LA    R5,TRNRFST          A(1ST ELEMENT)                               
         CLI   TRNKUNT,C'S'        UNIT S?                                      
         BNE   BADDRF10            NO, SKIP                                     
         TM    TRNSTAT,TRNSDR      DEBITS?                                      
         BNO   BADDRF10            NO, SKIP                                     
         AP    BATCHTOT,TRNAMNT    ADD TO BATCH TOTAL                           
         AP    BTDRTOT,TRNAMNT     ADD TO BATCH DEBIT TOTAL                     
*                                                                               
         USING ASKELD,R3                                                        
BADDRF10 LA    R3,ELEMENT          CLEAR ELEMENT BUILD AREA                     
         XC    ELEMENT,ELEMENT                                                  
         MVI   ASKEL,ASKELQ        ACCOUNT SYSTEM KEY ELEMENT                   
         MVI   ASKLN,ASKLNQ        LENGTH                                       
         ZIC   R2,ACCSYSK#         BUMP UP THE ACCESS SYSTEM SEQ#               
         LA    R2,1(,R2)                                                        
         STC   R2,ACCSYSK#                                                      
         STC   R2,ASKSEQN          STORE ACCESS SYSTEM SEQ#                     
         L     RF,ATIO             A(TRANSACTION I/O AREA)                      
         MVC   ASKKEY,0(RF)        ADD TRANSACTION KEY                          
*                                                                               
         GOTO1 =A(BHADDLST),(R3),RR=RELO                                        
         XIT1                      RETURN    TO   CALLER                        
         DROP  R3,R4,R5                                                         
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  BATCH ITEM RECORD ADDREC/PUTREC                                    *         
*                                                                     *         
*  INPUT:                                                             *         
*    BIO      - BATCH   HDR     I/O  AREA                             *         
*    BTDRTOT  - BATCH   DR      TOTAL                                 *         
*    BTITEMC  - BATCH   ITEM         COUNT                            *         
*    TIO      - TRANSACTION     I/O  AREA                             *         
*                                                                     *         
*  OUTPUT:                                                            *         
*    BIO      - BATCH   HDR     I/O  AREA                             *         
*    BTDRTOT  - BATCH   DR      TOTAL                                 *         
*    BTITEMC  - BATCH   ITEM         COUNT                            *         
*                                                                     *         
*  USES:                                                              *         
*    ELEMENT  - ELEMENT BUILD        AREA                             *         
*                                                                     *         
*  CALLS:                                                             *         
*    BHADDLST - BATCH   HEADER       RECORD ADD    ELEMENT    LAST    *         
*    BINITRCD - INIT    NEXT         BATCH  ITEM   RECORD             *         
*    BTADDREC - CALL    DATAMGR'S    ADDREC/PUTREC FOR  BATCH RECORDS *         
*                                                                     *         
*  ADDRESS OF THIS SUBROUTINE IN:                                     *         
*    ABITMADD IN GWS                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
BITMADD  DS    0H                  BATCH     ITEM RCD  ADDREC/PUTREC            
         NMOD1 0,**BITM**                                                       
*                                                                               
         USING TRNRECD,R4          MAP  TRANSACTION    RECORD                   
         USING TRNELD,R5           MAP  TRANSACTION    ELEMENT                  
         L     R4,ATIO             ->   TRANSACTION    I/O  AREA                
         LA    R5,TRNRFST          ->   1ST  ELEMENT                            
*                                                                               
         USING BIAELD,R3           MAP  BATCH     ITEM AMOUNT    EL             
         LA    R3,ELEMENT          CLEAR     EL   BUILD     AREA                
         XC    ELEMENT,ELEMENT                                                  
         MVI   BIAEL,BIAELQ        BATCH     ITEM AMOUNT    ELEMENT             
         MVI   BIALN,BIALNQ        LENGTH                                       
         ZAP   BIAAMT,BTDRTOT      DEBIT     AMOUNT                             
         MVC   BIAREF,TRNREF       REFERENCE                                    
*                                                                               
         ZAP   BTDRTOT,=P'0'       CLEAR     DEBIT     AMOUNT                   
*                                                                               
         USING TBARECD,R2          MAP  TRANSACTION    BATCH     RCDS           
         L     R2,ABIO             ->   BATCH     I/O  AREA                     
         SR    RF,RF                                                            
         ICM   RF,3,BTITEMC        UPDATE                                       
         LA    RF,1(,RF)                ITEM                                    
         STCM  RF,3,BTITEMC                  SEQUENCE  NUMBER                   
         MVC   TBAKTSEQ,BTITEMC    SET  SEQUENCE  FOR  ITEM RECORD              
*                                                                               
*                                  ADD  EL   TO   END  OF   RECORD              
         GOTO1 =A(BHADDLST),(R3),RR=RELO                                        
*                                                                               
*                                  ADDREC    OR   PUTREC                        
         GOTO1 =A(BTADDREC),RR=RELO                                             
*                                                                               
*                                  INIT NEXT BATCH     ITEM RECORD              
         GOTO1 =A(BINITRCD),RR=RELO                                             
*                                                                               
         XIT1                      RETURN    TO   CALLER                        
*                                                                               
         DROP  R3,R4,R5                                                         
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  INITIALIZE FIRST/NEXT BATCH ITEM RECORD                            *         
*                                                                     *         
*  INPUT:                                                             *         
*    BTKEY    - BATCH   KEY     AREA                                  *         
*                                                                     *         
*  OUTPUT:                                                            *         
*    ACCSYSK# - ACCESS  SYSTEM  KEY  SEQUENCE  NUMBER                 *         
*    BIO      - BATCH   HDR     I/O  AREA                             *         
*                                                                     *         
*  USES:                                                              *         
*                                                                     *         
*  CALLS:                                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
BINITRCD DS    0H                  INITIALIZE     1ST/NEXT  BATCH               
*                                       ITEM RECORD                             
         NMOD1 0,**BINT**                                                       
*                                                                               
         L     R2,ABIO             CLEAR                                        
         LR    RE,R2                    BATCH                                   
         LA    RF,RECLNQ                     HEADER                             
         SR    R1,R1                              I/O                           
         MVCL  RE,R0                                   AREA                     
*                                                                               
         USING TBARECD,R2          MAP  TRANSACTION    BATCH     RCDS           
         L     R2,ABIO             ->   BATCH     I/O  AREA                     
         L     R3,ABTKEY           ->   BATCH     KEY  AREA                     
         MVC   TBAKEY,0(R3)        SET  BATCH     KEY                           
*                                                                               
         MVI   ACCSYSK#,0          INIT ACCESS    SYSTEM    KEY  SEQ  #         
*                                                                               
         XIT1                      RETURN    TO   CALLER                        
*                                                                               
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  CALL DATAMGR'S ADDREC/PUTREC FOR BATCH RECORDS                     *         
*                                                                     *         
*  INPUT:                                                             *         
*    BIO      - BATCH HDR     I/O  AREA                               *         
*                                                                     *         
*  USES:                                                              *         
*    BTDIO    - DIRECTORY     I/O  KEY                                *         
*    DA       - DIRECTORY          ADDRESS                            *         
*    DMWORK   - DATA  MANAGER WORK AREA                               *         
*                                                                     *         
*  ADDRESS OF THIS SUBROUTINE IN:                                     *         
*    ABADDREC IN GWS                                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TBARECD,R2          MAP  TRANSACTION    BATCH     RCDS           
         SPACE 1                                                                
BTADDREC DS    0H                  CALL DATAMGR'S ADDREC/PUTREC  RTNS           
         NMOD1 0,**BADR**                                                       
*                                                                               
         L     R2,ABIO             ->   BATCH     HDR  I/O  AREA                
         L     R4,ABTDIO           ->   DIRECTORY      I/O  KEY                 
*                                  READ FOR  UPDATE    AND  READ DEL'ED         
         GOTO1 VDATAMGR,DMCB,(X'88',DMREAD),ACCDIR,ABIO,(R4)                    
         TM    8(R1),X'10'         TEST RECORD    NOT  FOUND ?                  
         BO    BTADDR20            OK   TO   ADD                                
*                                  TEST DELETED ?                               
         TM    TBAKSTA-TBARECD(R4),TBAHSDEL                                     
         BO    *+6                                                              
         DC    H'0'                DUPLICATE RECORD                             
*                                                                               
         MVC   TBAKSTA-TBARECD(L'TBAKSTA,R4),TBARSTA                            
*                                  REWRITE   RECORD    ON   ACCDIR              
         GOTO1 VDATAMGR,DMCB,DMWRT,ACCDIR,(R4),(R4)                             
         TM    8(R1),0                                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                  READ FOR  UPDATE    SO   THAT                
*                                       WE   CAN  REPLACE   THE  RCD            
         MVC   DA,TBAKDA-TBAKEY(R4)                                             
*                                  READ FOR  UPDATE    ACCMST                   
         GOTO1 VDATAMGR,DMCB,(X'80',GETREC),ACCMST,DA,AIOAREA2,ADMWORK          
         TM    8(R1),0                                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*                                  REWRITE   ACCMST                             
         GOTO1 VDATAMGR,DMCB,PUTREC,ACCMST,DA,ABIO,ADMWORK                      
         TM    8(R1),0                                                          
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     BTADDREX                                                         
*                                                                               
*                                  WRITE     ACCMST                             
BTADDR20 GOTO1 VDATAMGR,DMCB,ADDREC,ACCMST,DA,ABIO,ADMWORK                      
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BTADDREX XIT1                      RETURN    TO   CALLER                        
*                                                                               
         DROP  R2                                                               
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*  BATCH HEADER RECORD ADD ELEMENT LAST                               *         
*                                                                     *         
*  INPUT:                                                             *         
*    R1       - ADDRESS OF      ELEMENT                               *         
*    BIO      - BATCH   HEADER  I/O  AREA                             *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
BHADDLST DS    0H                  BATCH     HDR  RCD  ADD  ELEMENT             
*                                            LAST                               
         NMOD1 0,**BHAD**                                                       
*                                                                               
         LR    R3,R1               SAVE A(ELEMENT)                              
*                                  ADD  EL   TO   END  OF   RECORD              
         GOTO1 VHELLO,DMCB,(C'P',ACCMST),ABIO,(R3),ADDEND,0                     
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XIT1                      RETURN    TO   CALLER                        
*                                                                               
         LTORG                                                                  
         EJECT ,                                                                
         EJECT ,                                                                
***********************************************************************         
*  TABLES                                                             *         
***********************************************************************         
         SPACE 1                                                                
LANGTBL  DS    0H                  LANGUAGE TABLE                               
         DC    C'E',AL1(LANGEUS)   ENGLISH                                      
         DC    C'G',AL1(LANGGER)   GERMAN                                       
         DC    C'F',AL1(LANGFRE)   FRENCH                                       
         DC    C'S',AL1(LANGSPA)   SPANISH                                      
         DC    C'I',AL1(LANGITA)   ITALIAN                                      
         DC    C'D',AL1(LANGDUT)   DUTCH                                        
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
CURTAB   DS    0XL4                COUNTRY CURRENCY TABLE                       
       ++INCLUDE ACCURTAB                                                       
CURTABX  DC    AL1(0)                                                           
         SPACE 2                                                                
CURSIGNT DS    0CL2                CURRENCY SIGN TABLE                          
         DC    CL2'$ '             0 - ENGLISH                                  
         DC    CL2'# '             1 - ENGLISH UK                               
         DC    CL2'$ '             2 - ENGLISH US                               
         DC    CL2'DM'             3 - DEUTSCH                                  
*&&UK*&& DC    CL2'FF'             4 - FRANCAIS                                 
*&&US*&& DC    CL2'$ '             4 - FRENCH CANADA                            
         DC    CL2'PT'             5 - ESPANOL                                  
         DC    CL2'L '             6 - ITALIANO                                 
         DC    CL2'G '             7 - DUTCH                                    
         EJECT ,                                                                
         SPACE 1                                                                
DICTB00  DS    0X                                                               
         DCDDL AC#BIL,9            BILL                                         
         DCDDL AC#CMN,7,R          COMMISSION (RIGHT)                           
         DCDDL AC#DSPD,9           DISPLAYED                                    
         DCDDL AC#FEE,7,C          FEE        (CENTERED)                        
         DCDDL AC#GROSS,7,R        GROSS      (RIGHT)                           
         DCDDL AC#VAT,7,R          GST        (RIGHT)                           
         DCDDL AC#JOB,9            JOB                                          
         DCDDL AC#LGRC,6           LEDGER                                       
         DCDDL AC#MED,5            MEDIA                                        
         DCDDL AC#NET,7,R          NET        (RIGHT)                           
         DCDDL AC#OF,3             OF                                           
         DCDDL AC#PRGRP,10         PARAGRAPH                                    
*                                  PARAGRAPH                                    
         DCDDL AC#PRGRP,9,LABEL=AC@PRGR9                                        
         DCDDL AC#PST,7,R          PST        (RIGHT)                           
         DCDDL AC#TOTAL,7,R        TOTAL      (RIGHT)                           
         DCDDL AC#PFOOT,7          (FOOT)                                       
*                                                                               
DICTB00X DC    AL1(EOT)                                                         
         EJECT ,                                                                
         SPACE 1                                                                
CORELIST DC    AL1(QCENTER)        CENTER                                       
         DC    AL1(QCHOPPER)       CHOPPER                                      
         DC    AL1(QSQUASH)        SQUASHER                                     
         DC    AL1(QXSORT)         XSORT                                        
         DC    AL1(QUNDRLIN)       UNDERLIN                                     
         DC    AL1(QGETOPT)        GETOPT                                       
         DC    AL1(QOFFAL)         OFFAL                                        
         DC    X'FF'                                                            
*                                                                               
TABLES   DS    0F                                                               
         DC    A(ACTNTAB)          ACTION TABLE                                 
         DC    A(OPTNTAB)          OPTION TABLE                                 
         DC    A(GETRULE)                                                       
         DC    A(GETRATE)                                                       
         DC    A(INITSCR)          INITIALIZE SCREEN    MESSAGES                
         DC    A(TXTGET)           GET  STANDARD   MSG  TEXT                    
         DC    A(GETPFLD)          GET  PRINT FLD  FROM DICTIONARY              
         DC    A(GETAGCY)                                                       
         DC    A(GETCUR)                                                        
         DC    V(VATICAN)                                                       
         DC    V(PRORATA)                                                       
         DC    V(EXTEDIT)                                                       
         DC    V(BMONVAL)                                                       
         DC    V(CADET)                                                         
NTABLS   EQU   (*-TABLES)/4                                                     
         EJECT ,                                                                
***********************************************************************         
* TABLE OF INPUT ACTIONS.                                             *         
*                                                                     *         
*              BYTE 0-7 = ACTION NAME                                 *         
*                   8-9 = SHORT ACTION NAME                           *         
*                   10  = ACTION NUMBER                               *         
*                   11  = OVERLAY PHASE NUMBER                        *         
*                   12  = OVERRIDE SCREEN NUMBER                      *         
*                   13  = INDICATORS - BIT 0 ON=DDS-ONLY ACTION       *         
*                                      BIT 1 ON=NEEDS A BILL NUMBER   *         
*                                      BIT 2 ON=NEEDS A PARA NUMBER   *         
*                                      BIT 3 ON=TWO-STAGE ACTION      *         
*                                      BIT 4 ON=DISPLAYS A PARA       *         
*                                      BIT 5 ON=ADDS A PARA TO WKFILE *         
*                                      BIT 6 ON=MUST BE UNBILLED      *         
*                                      BIT 7 ON=ALLOC                 *         
*                   14  = INDICATORS - BIT 0 ON=GET ALLOCATED TOTALS  *         
***********************************************************************         
         SPACE 1                                                                
ACTNTAB  DS    0CL15                                                            
*                                                                               
         DC    C'ADD     AD'                                                    
         DC    AL1(ADD,OVPHEDIT),X'FD'                                          
         DC    AL1(HASBILNO+HASPARNO+NEWPARA+UNBILLED)                          
         DC    AL1(JOBTOT)                                                      
*                                                                               
DSUMNTRY DC    C'DISPLAY DI'                                                    
         DC    AL1(DIS,OVPHDIS),X'FE'                                           
         DC    AL1(READOK)                                                      
         DC    AL1(0)                                                           
*                                                                               
DDETNTRY DC    C'DISPLAY DI'                                                    
         DC    AL1(DID,OVPHBASE),X'FD'                                          
         DC    AL1(HASBILNO+HASPARNO+DISPARA+READOK)                            
         DC    AL1(JOBTOT)                                                      
*                                                                               
         DC    C'CHANGE  CH'                                                    
         DC    AL1(CHA,OVPHEDIT),X'FD'                                          
         DC    AL1(HASBILNO+HASPARNO+TWOSTAGE+DISPARA+UNBILLED)                 
         DC    AL1(JOBTOT)                                                      
*                                                                               
         DC    C'EDIT    ED'                                                    
         DC    AL1(EDIT,OVPHEDIT),X'FD'                                         
         DC    AL1(HASBILNO+HASPARNO+DISPARA+UNBILLED)                          
         DC    AL1(JOBTOT)                                                      
*                                                                               
         DC    C'INSERT  IN'                                                    
         DC    AL1(INS,OVPHEDIT),X'FD'                                          
         DC    AL1(HASBILNO+HASPARNO+NEWPARA+UNBILLED)                          
         DC    AL1(JOBTOT)                                                      
*                                                                               
         DC    C'DELETE  DE'                                                    
         DC    AL1(DEL,OVPHEDIT),X'FD'                                          
         DC    AL1(HASBILNO+HASPARNO+TWOSTAGE+DISPARA+UNBILLED)                 
         DC    AL1(JOBTOT)                                                      
*                                                                               
         DC    C'DRAFT   DR'                                                    
         DC    AL1(DRA,OVPHBIL),X'00'                                           
         DC    AL1(HASBILNO+UNBILLED)                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    C'BILL    BI'                                                    
         DC    AL1(BIL,OVPHBIL),X'00'                                           
         DC    AL1(HASBILNO+UNBILLED)                                           
         DC    AL1(0)                                                           
*                                                                               
DALLNTRY DC    C'ALLOCATEAL'                                                    
         DC    AL1(ALL,OVPHALL),X'FA'                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    C'REVERSE RE'                                                    
         DC    AL1(REV,OVPHALL),X'FA'                                           
         DC    AL1(HASBILNO)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    C'UNBILL  UB'                                                    
         DC    AL1(UNB,OVPHBIL),X'00'                                           
         DC    AL1(HASBILNO)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    C'LIST    LI'                                                    
         DC    AL1(LIST,OVPHLIST),X'FB'                                         
         DC    AL1(READOK)                                                      
         DC    AL1(0)                                                           
*                                                                               
         DC    C'EXTEND  EX'                                                    
         DC    AL1(EXT,OVPHBASE),X'00'                                          
         DC    AL1(HASBILNO)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    C'AUTO    AB'                                                    
         DC    AL1(AUT,OVPHAUT),X'FD'                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    C'WRITEOFFWO'                                                    
         DC    AL1(WRT,OVPHWRT),X'F8'                                           
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
*                                                                               
         DC    C'RECOVER RC'                                                    
         DC    AL1(REC,OVPHWRT),X'F8'                                           
         DC    AL1(HASBILNO)                                                    
         DC    AL1(0)                                                           
*                                                                               
         DC    X'FF'                                                            
         EJECT ,                                                                
***********************************************************************         
*  TABLE OF INPUT OPTIONS.                                            *         
*                                                                     *         
*              BYTE 0    =ENTRY LENGTH                                *         
*              BYTE 1-8  =OPTION KEYWORD                              *         
*              BYTE 9    =INDICATORS - BIT 0 ON=DDS-ONLY OPTION       *         
*              BYTE 10   =MIN LENGTH OF DATA VALUE                    *         
*              BYTE 11   =MAX LENGTH OF DATA VALUE                    *         
*              BYTE 12-13=DISPLACEMENT FROM START OF W/S OF ADDRESS   *         
*                         OF PROCESSED PARAMETER VALUE                *         
*              BYTE 14-17=B0 = 0    - A(VALIDATE/CONVERT SR)          *         
*                              C'Y' - MOVE C'Y' TO OUTPUT             *         
*                              ELSE - MOVE IN TO OUT FOR L'B0         *         
*              BYTE 18-  =STRING OF COMPATIBLE ACTION NUMBERS ENDED   *         
*                         BY ZERO                                     *         
***********************************************************************         
         SPACE 1                                                                
OPTNTAB  DS    0C                                                               
OPTFI    DC    AL1(OPTFIX-*),CL8'FIRST   ',X'000000',AL2(FIRST-GWS)             
         DC    C'Y',AL3(0),AL1(ADD,DID,0,0)                                     
OPTFIX   DS    0H                                                               
*                                                                               
OPTNE    DC    AL1(OPTNEX-*),CL8'NEXT    ',X'000000',AL2(NEXT-GWS)              
         DC    C'Y',AL3(0),AL1(ADD,DID,0,0)                                     
OPTNEX   DS    0H                                                               
*                                                                               
OPTFO    DC    AL1(OPTFOX-*),CL8'FOOTLINE',X'000000',AL2(FOOT-GWS)              
         DC    C'Y',AL3(0),AL1(ADD,0)                                           
OPTFOX   DS    0H                                                               
*                                                                               
OPTDA    DC    AL1(OPTDAX-*),CL8'DATE    ',X'000608',AL2(BILDATE-GWS)           
         DC    AL4(VALBDTE),AL1(BIL,UNB,LIST,ALL,DRA,0,0)                       
OPTDAX   DS    0H                                                               
*                                                                               
OPTGST   DC    AL1(OPTGSTX-*),CL8'GST     ',X'000102',AL2(GSTOPT-GWS)           
         DC    AL4(VALN),AL1(BIL,DRA,0)                                         
OPTGSTX  DS    0H                                                               
*                                                                               
OPTPST   DC    AL1(OPTPSTX-*),CL8'PST     ',X'000102',AL2(PSTOPT-GWS)           
         DC    AL4(VALN),AL1(BIL,DRA,0)                                         
OPTPSTX  DS    0H                                                               
*                                                                               
OPTMO    DC    AL1(OPTMOX-*),CL8'MOS     ',X'000406',AL2(BILMOS-GWS)            
         DC    AL4(VALBMOS),AL1(BIL,UNB,ALL,WRT,REC,0)                          
OPTMOX   DS    0H                                                               
*                                                                               
OPTBI    DC    AL1(OPTBIX-*),CL8'BILLED  ',X'000103',AL2(BILFLT-GWS)            
         DC    AL1(1),AL3(0),AL1(LIST,0)                                        
OPTBIX   DS    0H                                                               
*                                                                               
OPTTWES  DC    AL1(OPTTWESX-*),CL8'TWOEST',X'000000',AL2(TWOES-GWS)             
         DC    C'Y',AL3(0),AL1(DID,DIS,0)                                       
OPTTWESX DS    0H                                                               
*                                                                               
OPTEXDY  DC    AL1(OPTEXDYX-*),CL8'DAYS',X'000102',AL2(EXTND-GWS)               
         DC    AL4(VALDAY),AL1(EXT,0)                                           
OPTEXDYX DS    0H                                                               
*                                                                               
OPTSDA   DC    AL1(OPTSDAX-*),CL8'SDATE   ',X'000608',AL2(SDATE-GWS)            
         DC    AL4(VALBDTE),AL1(ALL,0,0)                                        
OPTSDAX  DS    0H                                                               
*                                                                               
OPTEDA   DC    AL1(OPTEDAX-*),CL8'EDATE   ',X'000608',AL2(EDATE-GWS)            
         DC    AL4(VALBDTE),AL1(ALL,0,0)                                        
OPTEDAX  DS    0H                                                               
*                                                                               
OPTWCF   DC    AL1(OPTWCFX-*),CL8'WC      ',X'000203',AL2(WCF-GWS)              
         DC    AL4(VALWCF),AL1(ALL,0,0)                                         
OPTWCFX  DS    0H                                                               
*                                                                               
OPTALLC  DC    AL1(OPTALLCX-*),CL8'AL      ',X'000101',AL2(ALLCSW-GWS)          
         DC    AL4(VALYORN),AL1(ALL,0,0)                                        
OPTALLCX DS    0H                                                               
*                                                                               
OPTLAB   DC    AL1(OPTLABX-*),CL8'LABOR   ',X'000101',AL2(LABSW-GWS)            
         DC    AL4(VALYORN),AL1(ALL,0,0)                                        
OPTLABX  DS    0H                                                               
*                                                                               
OPTUNAL  DC    AL1(OPTUNALX-*),CL8'UNALL   ',X'000101',AL2(UNALSW-GWS)          
         DC    AL4(VALYORN),AL1(ALL,0,0)                                        
OPTUNALX DS    0H                                                               
*                                                                               
OPTSUMM  DC    AL1(OPTSUMMX-*),CL8'SUMMARY ',X'000101',AL2(SUMMSW-GWS)          
         DC    AL4(VALY),AL1(ALL,0,0)                                           
OPTSUMMX DS    0H                                                               
*                                                                               
OPTPART  DC    AL1(OPTPARTX-*),CL8'PARTIAL ',X'000101',AL2(PARTSW-GWS)          
         DC    AL4(VALYORN),AL1(ALL,0,0)                                        
OPTPARTX DS    0H                                                               
*                                                                               
OPTSIO   DC    AL1(OPTSIOX-*),CL8'SIO      ',X'800101',AL2(SIOSW-GWS)           
         DC    AL4(VALY),AL1(255)                                               
OPTSIOX  DS    0H                                                               
*                                                                               
OPTAMT   DC    AL1(OPTAMTX-*),CL8'AMOUNT   ',X'00020A',AL2(FLTAMT-GWS)          
         DC    AL4(VALAMT),AL1(ALL,0,0)                                         
OPTAMTX  DS    0H                                                               
*                                                                               
OPTOWO   DC    AL1(OPTOWOX-*),CL8'OWO      ',X'00030E',AL2(WOAOVR-GWS)          
         DC    AL4(VALOWO),AL1(WRT,0,0)                                         
OPTOWOX  DS    0H                                                               
*                                                                               
OPTTYP   DC    AL1(OPTTYPX-*),CL8'TYPE     ',X'000101',AL2(BILTYP-GWS)          
         DC    AL4(VALTYPE),AL1(WRT,0,0)                                        
OPTTYPX  DS    0H                                                               
*                                                                               
OPTWT    DC    AL1(OPTWTX-*),CL8'WT       ',X'000102',AL2(WOTYP-GWS)            
         DC    AL4(VALWTYP),AL1(ALL,0,0)                                        
OPTWTX   DS    0H                                                               
*                                                                               
OPTBT    DC    AL1(OPTBTX-*),CL8'BT       ',X'000103',AL2(BATYP-GWS)            
         DC    AL4(VALBTYP),AL1(ALL,0,0)                                        
OPTBTX   DS    0H                                                               
*                                                                               
OPTOFF   DC    AL1(OPTOFFX-*),CL8'OFF      ',X'000103',AL2(OFFOPT-GWS)          
         DC    AL4(VALYORN),AL1(ALL,0,0)                                        
OPTOFFX  DS    0H                                                               
*                                                                               
OPTPLN   DC    AL1(OPTPLNX-*),CL8'LANGUAGE',X'000101',AL2(PLANGOPT-GWS)         
         DC    AL4(VALPLANG),AL1(AUT,ADD,0,0)                                   
OPTPLNX  DS    0H                                                               
*                                                                               
         DC    X'00'                                                            
         EJECT ,                                                                
***********************************************************************         
*  DSECT TO DEFINE WORKING STORAGE REQUIREMENTS                       *         
***********************************************************************         
         SPACE 1                                                                
DWSD     DSECT                                                                  
DWSL     DS    CL(LWSX-LWS)        AREA FOR LOCAL WORKING STORAGE               
DWSG     DS    CL(GWSX-GWS)        AREA FOR GENERAL WORKING STORAGE             
DWST     DS    CL(TWA1LAST-TWA1D)  AREA FOR TWA1                                
DWSX     EQU   *                                                                
         EJECT ,                                                                
***********************************************************************         
*  DSECT FOR LOCAL (BASE) WORKING STORAGE                             *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
LWS      DS    0D                                                               
COMMAMT  DS    D                                                                
CDAMT    DS    D                                                                
PSTBASIS DS    D                                                                
*                                                                               
SAVER2   DS    A                   SAVE   AREA FOR  R2                          
SAVER6   DS    A                   SAVE   AREA FOR  R6                          
SAVERE   DS    A                   SAVE   AREA FOR  RE                          
SAVEMSG# DS    A                   SAVE   AREA FOR  MESSAGE   NUMBER            
MANAMT   DS    A                   A(MANUAL    AMOUNT    FIELD)                 
*                                                                               
BILGOWRK DS    CL100                                                            
LWSWORK  DS    CL60                LOCAL  RTNS WORK AREA                        
WRKAMT   DS    CL(SCANRHSL)        AMOUNT WORK AREA                             
*                                                                               
SCANBLK  DS    30CL(22+SCANRHSL)                                                
*                                                                               
*                                  DIRECTORY   I/O  KEY  AREA 2                 
*                                  .      FOR  BATCH     TRANSACTIONS           
BTDIO2   DS    CL(TBAKDA-TBARECD+L'TBAKDA)                                      
*                                                                               
DICLS00  DS    0X                                                               
AC$BIL   DS    CL9                 BILL                                         
AC$CMN   DS    CL7                 COMMISSION (RIGHT)                           
AC$DSPD  DS    CL9                 DISPLAYED                                    
AC$FEE   DS    CL7                 FEE        (CENTERED)                        
AC$GROSS DS    CL7                 GROSS      (RIGHT)                           
AC$VAT   DS    CL7                 GST        (RIGHT)                           
AC$JOB   DS    CL9                 JOB                                          
AC$LGRC  DS    CL6                 LEDGER                                       
AC$MED   DS    CL5                 MEDIA                                        
AC$NET   DS    CL7                 NET        (RIGHT)                           
AC$OF    DS    CL3                 OF                                           
AC$PRGRP DS    CL10                PARAGRAPH                                    
AC$9PRGR DS    CL9                 PARAGRAPH                                    
AC$PST   DS    CL7                 PST        (RIGHT)                           
AC$TOTAL DS    CL7                 TOTAL      (RIGHT)                           
AC$PFOOT DS    CL7                 (FOOT)                                       
*                                                                               
DICLS00X DS    AL1                 EOT                                          
*                                                                               
PROFDATA DS    CL16                KEY FOR LK PROFILE                           
LKDATA   DS    CL16                LK PROFILE DATA                              
*                                                                               
PALAREA  DS    XL20                P&L BUCKET AREA                              
*                                                                               
LWSX     DS    0D                                                               
         EJECT ,                                                                
***********************************************************************         
*  DSECT TO COVER LK PROFILE                                          *         
***********************************************************************         
PROFKD   DSECT                                                                  
PROFKEY  DS    0CL1                                                             
PROFKSYS DS    CL1                                                              
PROFKPGM DS    CL3                                                              
         DS    CL1                                                              
PROFKUNL DS    CL2                                                              
PROFKACC DS    CL3                                                              
PROFKAST DS    CL1                                                              
PROFKOFF DS    CL1                                                              
PROFKAGY DS    CL2                                                              
PROFKOFC DS    CL2                                                              
         SPACE 2                                                                
***********************************************************************         
*  DSECT TO COVER ACTION TABLE ENTRY                                  *         
***********************************************************************         
         SPACE 1                                                                
ACTD     DSECT                                                                  
ACTDNAME DS    CL8                 NAME                                         
ACTDSHT  DS    CL2                 SHORT NAME                                   
ACTDNUM  DS    CL1                 NUMBER (EQUATED)                             
ACTDOVER DS    CL1                 OVERLAY                                      
ACTDSCRN DS    CL1                 SCREEN NUMBER                                
ACTDINDS DS    CL1                 INDICATORS                                   
ACTDSTAT DS    CL1                 XTRA INDICATORS                              
ACTDLEN  EQU   *-ACTD              ENTRY LENGTH                                 
         SPACE 2                                                                
***********************************************************************         
*  DSECT TO COVER ACTION PARAMETER (OPTION) TABLE ENTRY               *         
***********************************************************************         
         SPACE 1                                                                
PARMD    DSECT                                                                  
PARMLEN  DS    CL1       B         LENGTH OF TABLE ENTRY                        
PARMWORD DS    CL8       C         PARAMETER KEYWORD                            
PARMINDS DS    XL1       X         INDICATORS                                   
PARMMIN  DS    XL1       B         MIN LENGTH OF RHS                            
PARMMAX  DS    XL1       B         MAX LENGTH OF RHS                            
PARMDEST DS    CL2       B         DISPLACEMENT FROM START OF W/S OF            
*                                  ADDRESS OF PROCESSED PARAMETER VALUE         
PARMSR   DS    CL4       B         B0 = 0    - A(VALIDATE/CONVERT SR)           
*                                  B0 = C'Y' - MOVE C'Y' TO OUTPUT              
*                                  ELSE      - MOVE IN TO OUT FOR L'B0          
PARMACTS DS    0C        B         STRING OF COMPATIBLE ACTNNUMS ENDED          
*                                  BY ZERO                                      
         EJECT ,                                                                
***********************************************************************         
*  DSECT TO COVER TOTAL LINE                                          *         
***********************************************************************         
         SPACE 1                                                                
DTD      DSECT                                                                  
DTTYPE   DS    CL9                 TYPE OF TOTAL (PARAGRAPH, BILL, JOB)         
         DS    CL1                                                              
DTTITL   DS    0C                                                               
DTNET    DS    CL11                NET AMOUNT                                   
         DS    CL1                                                              
DTCOM    DS    0CL10               COMMISSION                                   
DTFEE    DS    CL10                OR FEE  (WHEN PFCOM=F)                       
         DS    CL1                                                              
DTGROSS  DS    CL11                GROSS                                        
         DS    CL1                                                              
*                                                                               
DTGST    DS    CL10                GST (OPTIONAL)                               
         DS    CL1                                                              
DTPST    DS    CL10                PST (OPTIONAL)                               
         DS    CL1                                                              
DTTOT    DS    CL11                TOTAL                                        
         DS    CL1                                                              
DTGSTLN  EQU   *-DTGST                                                          
DTTITLNQ EQU   *-DTTITL                                                         
*                                                                               
DTLN     EQU   *-DTD                                                            
         EJECT                                                                  
* DDDICTATED                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDDICTATED                                                     
         PRINT ON                                                               
* ACBMONVALD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBMONVALD                                                     
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE ACBILWORK                                                      
         EJECT                                                                  
       ++INCLUDE ACBILFDD                                                       
         EJECT                                                                  
       ++INCLUDE ACBILDSECT                                                     
         EJECT                                                                  
       ++INCLUDE FAXTRAINF                                                      
         EJECT                                                                  
       ++INCLUDE FASECRETD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'063ACBIL00   07/31/15'                                      
         END                                                                    
