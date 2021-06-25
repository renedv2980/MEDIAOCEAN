*          DATA SET ACORD00    AT LEVEL 049 AS OF 10/17/18                      
*PHASE T60F00A                                                                  
*INCLUDE ACJOBCOL                                                               
*INCLUDE SRCHCALL                                                               
*INCLUDE ACSRCHC                                                                
         TITLE 'ACORD00 - PRODUCTION ORDERS - ROOT'                             
*SGAV 049 21SEP18 <SPEC-12211> BULK API EXTRACT CHANGES                         
ACORD00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (WORKX-ORDWORKD),**ORD**,RA,R5,RR=RF,CLEAR=YES                   
         LR    R9,RC                                                            
         USING ORDWORKD,R9         R9=A(GLOBAL W/S)                             
         ST    RF,RELO                                                          
                                                                                
         L     RE,20(R1)                                                        
         MVC   FACFLAG,7(RE)       SAVE CONNECT FLAG                            
         MVC   FACUPD,10(RE)       AND UPDATIVE FACPAK ID                       
                                                                                
         L     R8,4(R1)                                                         
         USING TWAD,R8             R8=A(TWA)                                    
         EJECT                                                                  
*---------------------------------------------------------                      
*        INITIALIZE WORKSTORAGE VALUES                                          
*---------------------------------------------------------                      
*                                                                               
         ST    RB,ABASE1                                                        
         ST    RA,ABASE2                                                        
         ST    R5,ABASE3                                                        
         ST    RD,AWORK                                                         
         ST    R8,ATWA                                                          
         MVC   ATIA,12(R1)                                                      
         MVC   ACOMFACS,16(R1)                                                  
         MVC   COMPANY,0(R1)                                                    
         MVC   ATIOB,0(R1)                                                      
*                                                                               
         MVI   DDS,C'N'                                                         
         CLI   TWAOFFC,C'*'        TEST FOR DDS TERMINAL                        
         BNE   *+8                                                              
         MVI   DDS,C'Y'                                                         
         L     RE,ATIOB            RE=A(TIOB AREA)                              
         USING TIOBD,RE                                                         
         MVC   MODFRST,TIOBFRST    EXTRACT TIOB VALUES                          
         MVC   MODLAST,TIOBLAST                                                 
         MVC   CURDISP,TIOBCURD                                                 
         ZIC   RF,TIOBAID                                                       
         LA    R0,12                                                            
         CR    RF,R0               TEST FOR PF13-24                             
         BNH   *+6                                                              
         SR    RF,R0               EQUIVALENCE TO PF1-PF12                      
         STC   RF,PFKEY                                                         
         DROP  RE                                                               
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         MVC   VCALLOV,CCALLOV     BUILD EXTERNAL DIRECTORY                     
         MVC   VDATAMGR,CDATAMGR                                                
         MVC   VDATCON,CDATCON                                                  
         MVC   VDATVAL,CDATVAL                                                  
         MVC   VGETFACT,CGETFACT                                                
         MVC   VGETMSG,CGETMSG                                                  
         MVC   VGETPROF,CGETPROF                                                
         MVC   VHELLO,CHELLO                                                    
         MVC   VPARSNIP,CPARSNIP                                                
         MVC   VSCANNER,CSCANNER                                                
         MVC   VUNSCAN,CUNSCAN                                                  
         MVC   VGETTXT,CGETTXT                                                  
         L     RE,8(R1)                                                         
         USING ACCFACSD,RE                                                      
         MVC   VADDAY,AADDAY                                                    
         MVC   VCASHVAL,ACASHVAL                                                
         MVC   VGETDAY,AGETDAY                                                  
         MVC   VACCEMU,AACCEMU                                                  
         DROP  RE                                                               
*                                                                               
         L     R2,=A(CORETAB)                                                   
         A     R2,RELO                                                          
         LA    R3,NCORES           GET ADCONS OF CORE-RESIDENT MODULES          
         LA    R4,COREFACS                                                      
         L     RF,VCALLOV                                                       
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
*                                                                               
INIT1    MVC   DMCB+7(1),0(R2)     SET MODULE NUMBER                            
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R4),0(R1)                                                    
         LA    R2,1(R2)            NEXT MODULE                                  
         LA    R4,4(R4)            NEXT ADDRESS POSITION                        
         BCT   R3,INIT1                                                         
*                                                                               
*                                  INITIALIZE IO AREAS AND ADDRESSES            
         LA    R0,4                LOOP COUNTER                                 
         LA    R1,C'1'             IO AREA NUMBER                               
         LA    RE,IOAREAS                                                       
         LA    RF,AIOAREA1                                                      
*                                                                               
INIT2    MVC   0(8,RE),=C'**IO0**'                                              
         STC   R1,4(RE)            SET IO NUMBER                                
         LA    RE,8(RE)            BUMP AHEAD PAST LABEL                        
         ST    RE,0(RF)            SET IO AREA ADDRESS                          
         LA    R1,1(R1)            INCREMENT IO AREA NUMBER                     
         LA    RE,LIOAREAS(RE)     POINT TO NEXT IO AREA                        
         LA    RF,4(RF)            NEXT ADCON                                   
         BCT   R0,INIT2                                                         
         EJECT                                                                  
*                                  BUILD INTERNAL/EXTERNAL DIRECTORY            
         LA    R1,AROUTINE                                                      
         L     RF,=A(ROUTTAB)                                                   
         A     RF,RELO                                                          
INIT3    ICM   RE,7,0(RF)                                                       
         LA    RE,0(RE)         RELOCATE A/V TYPE                               
         A     RE,RELO                                                          
         LA    RF,3(RF)                                                         
INIT4    ICM   RE,8,0(RF)                                                       
         ST    RE,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    RF,1(RF)                                                         
         CLI   0(RF),X'FF'         END OF SUB-LIST                              
         BNE   INIT4                                                            
         LA    RF,1(RF)                                                         
         CLI   0(RF),X'FF'         END OF LIST                                  
         BNE   INIT3                                                            
*                                                                               
         L     R1,=A(EXTTAB)                                                    
         A     R1,RELO                                                          
         LA    R0,NEXTTAB          SET ADCONS FOR EXTENDED STORAGE              
INIT5    LM    RE,RF,0(R1)                                                      
         LA    RE,ORDWORKD(RE)     COMPUTE ACTUAL ADDRESSES                     
         LA    RF,ORDWORKD(RF)                                                  
         ST    RE,0(RF)                                                         
         LA    R1,L'EXTTAB(R1)     NEXT TABLE ENTRY                             
         BCT   R0,INIT5                                                         
*                                  SET OTHER FIELDS                             
         LA    RE,ACRECORD-ACKEYD                                               
         STH   RE,DATADISP         SET DISP TO FIRST ELEMENT                    
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         MVC   MSG,SPACES                                                       
         MVC   XTRAMESS,SPACES                                                  
         MVI   DELETADD,C'N'                                                    
         MVI   EMULATE,C'Y'        YES                                          
*                                                                               
INIT6    LA    R1,APOACTH          SET FADR TO FIRST INPUT FIELD                
         ST    R1,FADR                                                          
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         GOTO1 AREAD,AIOAREA1      GET COMPANY RECORD                           
         BNE   ERROR                                                            
         L     RE,AIOAREA                                                       
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
*                                                                               
INIT7    CLI   0(RE),0             LOCATE COMPANY ELEMENT                       
         BE    INIT7A                                                           
         CLI   0(RE),ACMPELQ                                                    
         BE    *+14                                                             
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     INIT7                                                            
         USING ACCOMPD,RE                                                       
         MVC   PRODUL,ACMPJOB                                                   
         MVC   SUPPUL,ACMPSUPP                                                  
*                                                                               
         CLI   1(RE),CPYLN2Q       COMPANY SETTING(BYTE 9) FOR ACGENFIL         
         BNH   *+10                                                             
         MVC   COMPSTA9,X'57'(RE)  X'57(RE) : CPYSTAT9 IN ACGENFILE             
*                                                                               
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVC   OFFACOMF,ACOMFACS                                                
         MVC   OFFAALPH,TWAAGY                                                  
         MVC   OFFACPY,COMPANY                                                  
         MVC   OFFACST1,ACMPSTAT                                                
         MVC   OFFACST2,ACMPSTA2                                                
         MVC   OFFACST3,ACMPSTA3                                                
         MVC   OFFACST4,ACMPSTA4                                                
         MVC   OFFALIMA,TWAACCS                                                 
         MVC   OFFAAUTH,TWAAUTH                                                 
         MVI   OFFAINDS,OFFAIOFF                                                
         MVI   OFFAACT,OFFAINI                                                  
         CLI   MODE,INIT                                                        
         BE    *+14                                                             
         MVC   OFFASAV(OFFASAVL),SAVEOFFA                                       
         MVI   OFFAACT,OFFARES                                                  
         GOTO1 VOFFAL                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SAVEOFFA,OFFASAV                                                 
         DROP  R1                                                               
         EJECT                                                                  
*---------------------------------------------------------                      
*        FIRST TIME INITIALIZATION CODE                                         
*---------------------------------------------------------                      
*                                                                               
         CLI   MODE,INIT                                                        
         BNE   INITX                                                            
*                                                                               
INIT7A   MVC   KEY+1(2),PRODUL                                                  
         GOTO1 AREAD,AIOAREA1      GET PRODUCTION LEDGER RECORD                 
         BZ    INIT7B                                                           
         MVC   XTRAMESS(6),=C'LEDGER'                                           
         MVC   XTRAMESS+7(2),PRODUL                                             
         B     ERROR                                                            
INIT7B   LA    R1,PRODHEIR                                                      
         BAS   RE,INITHEIR         GET HEIRARCHY LENGTHS                        
*                                  GET TODAY'S DATE                             
         GOTO1 VDATCON,DMCB,(5,0),(0,WORK)                                      
         GOTO1 VDATCON,DMCB,(0,WORK),(1,TODAYP)                                 
         MVC   THISMON(1),WORK+1   SAVE MOS                                     
         MVC   THISMON+1(1),WORK+3                                              
         CLI   WORK+2,C'1'                                                      
         BNE   INIT8                                                            
         NI    THISMON+1,X'C3'     (10=A,11=B,12=C)                             
         ZIC   R1,THISMON+1                                                     
         LA    R1,1(R1)                                                         
         STC   R1,THISMON+1                                                     
*                                                                               
INIT8    XC    WORK,WORK           SAVE AGENCY-LEVEL PROGRAM PROFILE            
         MVC   WORK(4),=C'AORD'                                                 
         NI    WORK,X'FF'-X'40'    MAKE SYSTEM LOWER CASE                       
         MVC   WORK+4(1),COMPANY                                                
         MVC   WORK+12(2),TWAAGY                                                
         GOTO1 VGETPROF,DMCB,WORK,PROGPROF,VDATAMGR                             
         MVI   MODE,FIRST          SET SAVE W/S INITIALIZED                     
INITX    B     VALACT                                                           
         EJECT                                                                  
*---------------------------------------------------------                      
*        EXTRACT HEIRARCHY LENGTHS FROM A LEDGER RECORD.                        
*---------------------------------------------------------                      
*                                                                               
*        LENGTHS ARE EXTRACTED FROM RECORD ADDRESSED BY AIOAREA                 
*        INTO 3-BYTEFIELD ADDRESSED BY R1.                                      
*                                                                               
INITHEIR NTR1                                                                   
         L     RE,AIOAREA                                                       
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
INITH2   CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RE),ACHRELQ                                                    
         BE    *+14                                                             
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     INITH2                                                           
         USING ACHEIRD,RE                                                       
         MVC   0(1,R1),ACHRLEVA                                                 
         MVC   1(1,R1),ACHRLEVB                                                 
         MVC   2(1,R1),ACHRLEVC                                                 
         B     EXIT                                                             
         DROP  RE                                                               
         EJECT                                                                  
*---------------------------------------------------------                      
*        VALIDATE ACTION                                                        
*---------------------------------------------------------                      
*                                                                               
VALACT   GOTO1 AFVAL,APOACTH                                                    
         BZ    ERROR                                                            
         XC    ACTSCAN(256),ACTSCAN                                             
         XC    ACTSCAN+256(56),ACTSCAN+256                                      
         LA    R0,MAXESCAN         MAX=60 FOR POSSIBLE EMAIL                    
         GOTO1 VSCANNER,DMCB,((R0),FLDH),(5,ACTSCAN)                            
         MVC   FLAG1,4(R1)         SAVE NO OF INPUT FIELDS                      
         ZIC   RE,ACTSCAN                                                       
         BCTR  RE,0                FIRST FIELD IS ACTION                        
         MVI   FERN,TOOSMALL       MIN 2 CHARS                                  
         CLI   ACTSCAN,2                                                        
         BL    ERROR                                                            
         MVI   FERN,INVACTN                                                     
         L     R1,AACTNTAB                                                      
         USING ACTD,R1                                                          
*                                  LOOK UP ACTION IN ACTION TABLE               
VALACT2  CLI   0(R1),X'FF'                                                      
         BE    HELPER              IF INVALID GIVE HELP                         
         CLI   ACTSCAN,2           MATCH ON SHORT IF 2 CHARS                    
         BH    VALACT6                                                          
         CLC   ACTDSHT,ACTSCAN+12                                               
         BE    VALACT8                                                          
                                                                                
VALACT4  LA    R1,ACTDLEN(R1)                                                   
         B     VALACT2                                                          
                                                                                
VALACT6  EX    RE,*+8              OTHERWISE FULL                               
         B     *+10                                                             
         CLC   ACTDNAME(0),ACTSCAN+12                                           
         BNE   VALACT4                                                          
                                                                                
VALACT8  MVC   ACTNVALS,0(R1)      MATCH FOUND                                  
                                                                                
         TM    ACTINDS,READOK      IS READ ONLY ACCESS ALLOWED?                 
         BO    VALACT12            YES                                          
         TM    FACFLAG,XIROSYS+XIROMODE+XIWRONGF                                
         BZ    VALACT12                                                         
*                                                                               
         LHI   R2,360                                                           
         TM    FACFLAG,XIROMODE    CONNECTED IN READ ONLY MODE?                 
         BO    VALACT10            YES                                          
*                                                                               
         LHI   R2,357              MUST BE CONNECTED TO WRONG FACPAK            
         LA    RE,FACUPD                                                        
         ST    RE,DMCB+8                                                        
         MVI   DMCB+8,L'FACUPD                                                  
         TM    FACFLAG,XIWRONGF    CONNECTED TO WRONG FACPAK?                   
         BO    VALACT10            YES                                          
*                                                                               
         LHI   R2,358              CONNECTED TO READ ONLY SYSTEM                
         XC    DMCB+8(4),DMCB+8                                                 
                                                                                
VALACT10 GOTOR TXTGET,DMCB,(0,(R2)),0,,0                                        
         B     EXIT                                                             
                                                                                
VALACT12 MVI   FERN,INVACTN                                                     
         TM    ACTINDS,DDSONLY     CHECK FOR DDS ONLY                           
         BZ    *+12                                                             
         CLI   TWAOFFC,C'*'                                                     
         BNE   ERROR                                                            
         CLI   ACTDNUM,1           AMEND TYPE FUNCTION?                         
         BNH   VALACT14            NO, HELP OR DISPLAY                          
         CLI   PROGPROF+2,C'Y'     AUTH REQ'D?                                  
         BNE   VALACT14            NO                                           
         TM    TWAAUTH,X'10'       ARE YOU AUTH'D?                              
         BO    VALACT14            YES                                          
         MVI   FERN,SECLOCK        SECURITY LOCKOUT                             
         B     ERROR                                                            
                                                                                
VALACT14 TM    ACTINDS,PARMS_OK    ARE PARMS NOT NEEDED, BUT OK                 
         BO    VALACTX             YES                                          
         MVI   FNDX,2                                                           
         TM    ACTINDS,HASPARMS    ARE PARMS NEEDED                             
         BO    VALACT16            YES                                          
         MVI   FERN,INVALID        PARMS NOT ALLOWED                            
         CLI   FLAG1,1                                                          
         BH    ERROR                                                            
         B     VALACTX                                                          
                                                                                
VALACT16 MVI   FERN,NOINPUT        CHECK FOR PARMS (VALIDATE IN OV'LAY)         
         CLI   FLAG1,1                                                          
         BNH   ERROR                                                            
                                                                                
VALACTX  CLI   ACTION,HLP                                                       
         MVI   HELPFERN,OK                                                      
         BE    GO                                                               
         B     VALORD                                                           
         EJECT                                                                  
*---------------------------------------------------------                      
*        VALIDATE ORDER NUMBER                                                  
*---------------------------------------------------------                      
*                                                                               
VALORD   GOTO1 AFVAL,APONUMH                                                    
         BZ    ERROR               INPUT REQUIRED                               
         MVI   FERN,INVNUM                                                      
         CLI   ACTION,PRI          OR (PR ONLY) LINEUP                          
         BNE   *+14                                                             
         CLC   FLD(6),=C'LINEUP'                                                
         BE    GO                                                               
         MVI   FERN,INVALID                                                     
         ZIC   RF,FLDH+5                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),=CL6'AUTO'                                                
         BNE   VALORD1                                                          
         TM    ACTINDS,NEW                                                      
         BZ    ERROR                                                            
         B     VALORD8                                                          
*                                                                               
VALORD1  MVC   ORDNUMB(1),FLD      SAVE FIRST CHARACTER OF INPUT                
         MVC   WORK(5),=5X'F0'     TEST THE REST FOR NUMERIC                    
         BCTR  RF,0                AS FORMAT CAN BE 1 ALPHA PLUS                
         EX    RF,*+8              UP TO 5 NUMERIC                              
         B     *+10                                                             
         MVZ   WORK(0),FLD+1                                                    
         CLC   WORK(5),=5X'F0'                                                  
         BNE   ERROR                                                            
         CLI   FLDH+5,6            FOR SHORT INPUT WHICH STARTS WITH A          
         BE    VALORD1A            NUMBER,GO BACK TO THE OLD WAY                
         CLI   FLD,C'0'            IE PAD FROM LEFT WITH ZEROES                 
         BL    VALORD1A                                                         
         LA    RF,1(RF)                                                         
         MVI   ORDNUMB,C'0'                                                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         B     VALORD1B                                                         
VALORD1A EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD+1(0)                                                     
VALORD1B CVB   RF,DUB                                                           
         STCM  RF,7,ORDNUMB+1                                                   
         CLC   ORDNUMB,=X'F0000000' MUST NOT BE ZEROES                          
         BE    ERROR                                                            
         XC    KEY,KEY             OTHERWISE READ FOR ORDER REC                 
         LA    R7,KEY                                                           
         USING ACKEYD,R7                                                        
         MVI   ACOKCODE,ACOKCODQ                                                
         MVC   ACOKCOMP,COMPANY                                                 
         LA    RF,ORDNUMB+1                                                     
         LA    RE,ACOKNUM+1                                                     
         EDIT  (3,0(RF)),(5,0(RE)),FILL=0                                       
         MVC   ACOKNUM(1),ORDNUMB                                               
         OC    APONUM,SPACES                                                    
         CLC   APONUM,ACOKNUM      ENSURE 6 DIGIT NUMBER DISPLAYED              
         BE    *+14                                                             
         MVC   APONUM,ACOKNUM                                                   
         OI    APONUMH+6,X'80'                                                  
         LA    R1,AIOAREA1                                                      
         L     RF,AREADL                                                        
         CLI   ACTION,EDIT                                                      
         BE    VALORD2                                                          
         TM    ACTINDS,NEW         (LOCK IN CASE ITS DELETED)                   
         BO    VALORD2                                                          
         TM    ACTINDS,TWOSTAGE                                                 
         BZ    *+14                                                             
         CLC   ORDNUMB,LORDNUM                                                  
         BE    *+8                                                              
         L     RF,AREAD                                                         
VALORD2  BASR  RE,RF                                                            
         L     RE,AIOAREA1         RE=A(RECORD)                                 
         TM    ACTINDS,NEW                                                      
         BO    VALORDAD                                                         
*                                                                               
         MVI   FERN,NOTFOUND                                                    
         TM    DMCB+8,X'10'                                                     
         BNZ   ERROR                                                            
*                                                                               
         CLI   ACTION,RES                                                       
         BE    VALORD3            ALLOW RESTORE X'80' DELETED RECS              
*                                                                               
         MVI   FERN,DELETED                                                     
         TM    DMCB+8,2                                                         
         BNZ   ERROR                                                            
*                                                                               
VALORD3  BAS   RE,PREPOP           CHECK PRESTO/POP ORDER                       
         BNE   ERROR               DO NOT ALLOW ACTION                          
*                                                                               
         L     RE,AIOAREA1         RE=A(RECORD)                                 
*                                                                               
         CLI   ACTION,DEL                                                       
         BE    VALORDDE                                                         
*                                                                               
         CLI   ACTION,RES                                                       
         BE    VALORDRE                                                         
*                                                                               
         CLI   ACTION,CLO                                                       
         BE    VALORDCL                                                         
*                                                                               
         CLI   ACTION,OPE                                                       
         BE    VALORDOP                                                         
*                                                                               
         CLI   ACTION,DISP                                                      
         BE    VALORDDI                                                         
*                                                                               
         B     VALORDDI            ALL OTHER ACTIONS MUST MEET THE              
*                                  CRITERIA OF THE ORDER RECORD NOT             
*                                  BEING DELETED OR CLOSED.                     
*                                                                               
VALORDDE DS    0H                  DELETE AN ORDER RECORD                       
         CLI   ACSTATUS-ACKEYD(RE),X'20'                                        
         BE    ERRMSG                                                           
         CLI   ACSTATUS-ACKEYD(RE),X'80'                                        
         BE    ERRMSG                                                           
         B     VALORD6                                                          
*                                                                               
VALORDRE DS    0H                  RESTORE AN ORDER RECORD                      
         CLI   ACSTATUS-ACKEYD(RE),X'20'                                        
         BE    VALORD6                                                          
         CLI   ACSTATUS-ACKEYD(RE),X'80'                                        
         BE    VALORD6                                                          
         B     ERRMSG                                                           
*                                                                               
VALORDCL DS    0H                  CLOSE AN ORDER RECORD                        
         CLI   ACSTATUS-ACKEYD(RE),X'00'                                        
         BNE   ERRMSG                                                           
         B     VALORD6                                                          
*                                                                               
VALORDOP DS    0H                  OPEN AN ORDER RECORD                         
         CLI   ACSTATUS-ACKEYD(RE),X'40'                                        
         BNE   ERRMSG                                                           
         B     VALORD6                                                          
*                                                                               
VALORDDI DS    0H                  DISPLAY AN ORDER RECORD                      
         CLI   ACSTATUS-ACKEYD(RE),X'20'                                        
         BE    ERRMSG                                                           
         CLI   ACSTATUS-ACKEYD(RE),X'40'                                        
         BE    ERRMSG                                                           
         CLI   ACSTATUS-ACKEYD(RE),X'80'                                        
         BE    ERRMSG                                                           
         B     VALORD6                                                          
*                                                                               
VALORDAD DS    0H                                                               
         B     VALORD6                                                          
*                                                                               
VALORD6  TM    ACTINDS,TWOSTAGE     IF SAME AS LAST AND TWO-STATE GO            
         BZ    VALORD7              TO OVERLAY                                  
         CLC   ORDNUMB,LORDNUM     TEST IF ORDER NUMBER CHANGED                 
         BNE   VALORD7             YES                                          
         CLI   TYPE,EXPENSE        NO-TEST IF EXPENSE PO                        
         BE    GO                  YES-NO NEED FOR JOB LOOKUP                   
         BAS   RE,GETJOB           YES-GET JOB + DO LOOKUP FOR OVERLAY          
         B     GO                                                               
*                                                                               
VALORD7  TM    ACTINDS,NEW                                                      
         BZ    DISORD                                                           
*                                                                               
VALORD8  XC    LORDNUM,LORDNUM                                                  
         OC    APODATS,APODATS                                                  
         BZ    *+14                                                             
         XC    APODATS,APODATS                                                  
         OI    APODATSH+6,X'80'                                                 
         MVI   TYPE,PRODN          DEFAULT TYPE ON ADD                          
         B     VALCLI                                                           
         EJECT                                                                  
*---------------------------------------------------------                      
*        GET CORRECT STATUS MESSAGE                                             
*---------------------------------------------------------                      
*                                                                               
ERRMSG   MVI   FERN,ORDDEL         IS ORDER DELETED?                            
         CLI   ACSTATUS-ACKEYD(RE),X'20'                                        
         BE    ERROR                                                            
*                                                                               
         MVI   FERN,ORDCLO         IS ORDER RECORD CLOSED?                      
         CLI   ACSTATUS-ACKEYD(RE),X'40'                                        
         BE    ERROR                                                            
*                                                                               
         MVI   FERN,ORDOPN         IS ORDER RECORD OPEN?                        
         CLI   ACSTATUS-ACKEYD(RE),X'00'                                        
         BE    ERROR                                                            
*                                                                               
         MVI   FERN,INVSTAT                                                     
         B     ERROR                                                            
         EJECT                                                                  
*---------------------------------------------------------                      
*        CHECK IF $POP MUST BE USED FOR THIS ORDER                              
*        CHECK FOR UPDATIVE ACTIONS BESIDES CLOSE OR OPEN                       
*        ON A PRESTO ORDER.                                                     
*        ON EXIT, CC=EQ FOR OK, CC=NEQ AND FERN, MSG SET                        
*---------------------------------------------------------                      
*                                                                               
PREPOP   NTR1  ,                                                                
         L     RF,AIOAREA1                                                      
         AH    RF,DATADISP                                                      
         USING ORDELD,RF                                                        
*                                                                               
PREPOP2  CLI   ORDEL,0                                                          
         BE    OKXIT                                                            
         CLI   ORDEL,ORDELQ                                                     
         BE    PREPOP4                                                          
         ZIC   R1,ORDLN                                                         
         AR    RF,R1                                                            
         B     PREPOP2                                                          
*                                                                               
PREPOP4  CLI   ORDLN,ORDLN2Q       TEST ELEMENT IS LONG ENOUGH                  
         BL    OKXIT               NOT LONG ENOUGH FOR PRESTO FLAG              
         TM    ORDSTAT,ORDSPOP     MUST WE USE $POP?                            
         BO    PREPOP8             YES                                          
         TM    ORDSTAT,ORDSTY10    MUST THEY USE TYPE 10?                       
         BO    PREPOP8             YES                                          
         TM    ORDSTAT,ORDSPRES    TEST FOR PRESTO ORDER                        
         BZ    OKXIT               NO                                           
*                                                                               
         MVI   PRESFLAG,C'Y'       SET FLAG FOR PRESTO ORDER                    
*                                                                               
         CLI   ACTION,EDIT                                                      
         BE    PREPOP6                                                          
         CLI   ACTION,CHA                                                       
         BE    PREPOP6                                                          
         CLI   ACTION,DEL                                                       
         BE    PREPOP6                                                          
         CLI   ACTION,RES                                                       
         BE    PREPOP6                                                          
         B     OKXIT                                                            
*                                                                               
PREPOP6  MVI   FERN,PRESTORD                                                    
         B     ERRXIT                                                           
*                                                                               
PREPOP8  MVI   FERN,POPORDR                                                     
         B     ERRXIT                                                           
         DROP  RF                                                               
         EJECT                                                                  
*---------------------------------------------------------                      
*        DISPLAY ORDER                                                          
*---------------------------------------------------------                      
*                                                                               
DISORD   L     R7,AIOAREA1                                                      
         AH    R7,DATADISP                                                      
*                                                                               
DISORD2  CLI   0(R7),0             FIND ELEMENTS                                
         BE    DISORD30                                                         
         CLI   0(R7),X'67'                                                      
         BE    DISORD10                                                         
         CLI   0(R7),X'68'                                                      
         BE    DISORD20                                                         
DISORD3  ZIC   R0,1(R7)                                                         
         AR    R7,R0                                                            
         B     DISORD2                                                          
*                                                                               
DISORD10 DS    0H                  ORDER ELEMENT                                
         USING ACORDRD,R7                                                       
         MVI   TYPE,PRODN                                                       
         CLC   PRODUL,ACORJOB+1                                                 
         BE    *+8                                                              
         MVI   TYPE,EXPENSE                                                     
         MVC   CRD,SPACES                                                       
*                                                                               
         GOTO1 VDATCON,DMCB,(1,ACORDATE),(8,APODAT)                             
         OI    APODATH+6,X'80'                                                  
         MVC   LDATE,ACORDATE                                                   
         CLI   ACORAMNO,0                                                       
         BE    DISORD12                                                         
         MVC   CRD(14),=C'LAST AMENDMENT'                                       
         LA    R2,CRD                                                           
         EDIT  ACORAMNO,(3,15(R2)),ALIGN=LEFT                                   
         AR    R2,R0                                                            
         MVI   15(R2),C','                                                      
         GOTO1 VDATCON,DMCB,(1,ACORAMDT),(8,16(R2))                             
*                                                                               
DISORD12 OC    APODATS,SPACES                                                   
         CLC   APODATS,CRD                                                      
         BE    DISORD13                                                         
         MVC   APODATS,CRD                                                      
         OI    APODATSH+6,X'80'                                                 
*                                                                               
DISORD13 OC    APOAUT,SPACES       AUTHORISATION                                
         CLC   ACORAUTH,APOAUT                                                  
         BE    *+14                                                             
         MVC   APOAUT,ACORAUTH                                                  
         OI    APOAUTH+6,X'80'                                                  
*                                                                               
         OC    ACORDDTE,ACORDDTE                                                
         BZ    DISOR13A                                                         
         GOTO1 VDATCON,DMCB,(1,ACORDDTE),(5,APODDTE)                            
         OI    APODDTEH+6,X'80'                                                 
DISOR13A CLI   ACORLEN,ACORLNQ                                                  
         BNE   DISORD14                                                         
         OC    APOATTN,SPACES                                                   
         CLC   ACORATTN,APOATTN                                                 
         BE    *+14                                                             
         MVC   APOATTN,ACORATTN                                                 
         OI    APOATTNH+6,X'80'                                                 
         OC    APOTAX,SPACES                                                    
         CLC   ACORTAX,APOTAX                                                   
         BE    *+14                                                             
         MVC   APOTAX,ACORTAX                                                   
         OI    APOTAXH+6,X'80'                                                  
         B     DISORD14                                                         
*                                                                               
DISORD14 SR    R1,R1               SET UP CLI/PRO/JOB CODES & INPUT LEN         
         CLI   TYPE,EXPENSE        EXCEPT IF IT IS AN EXPENSE ORDER             
         BE    DISORD15                                                         
         GOTO1 AACSRCHC,DMCB,APOCLIH,ATWA,(PRODHEIR,0),                X        
               (X'C0',SDSPNAMQ),ACORJOB,0                                       
         GOTO1 (RF),(R1),APOPROH,,(PRODHEIR+1,PRODHEIR)                         
         GOTO1 (RF),(R1),APOJOBH,,(PRODHEIR+2,PRODHEIR+1)                       
         XC    APOEXP,APOEXP                                                    
         OI    APOEXPH+6,X'80'                                                  
         B     DISORD16                                                         
*                                                                               
DISORD15 GOTO1 AACSRCHC,DMCB,APOEXPH,ATWA,0,(X'C0',SDSPNAMQ),ACOREXP,0          
         XC    APOCLI,APOCLI                                                    
         OI    APOCLIH+6,X'80'                                                  
         XC    APOPRO,APOPRO                                                    
         OI    APOPROH+6,X'80'                                                  
         XC    APOJOB,APOJOB                                                    
         OI    APOJOBH+6,X'80'                                                  
*                                                                               
DISORD16 GOTO1 AACSRCHC,DMCB,APOSUPH,ATWA,(C'*',SUPPUL),               X        
               (X'C0',SDSPNAMQ),ACORSUPP,0                                      
         B     DISORD3                                                          
*                                                                               
         EJECT                                                                  
DISORD20 DS    0H                  ORDER AMOUNT ELEMENT                         
         USING ACOAMTD,R7                                                       
         CLI   TYPE,EXPENSE                                                     
         BE    DISORD27                                                         
         MVC   TEMP,SPACES         US - UP TO 4 COMM & 4 NON-COMM W/C'S         
DISORD21 LA    R3,TEMP             BUILD DISPLAY FLDS IN TEMP                   
         TM    ACOASTAT,X'80'                                                   
         BZ    *+8                                                              
         LA    R3,TEMP+114                                                      
         LR    R2,R3                                                            
         CLI   0(R2),C' '                                                       
         BE    DISORD22                                                         
         LA    R2,11-1(R2)         GET TO END OF WORKCODES                      
         CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),C','                                                       
         LA    R2,2(R2)                                                         
*                                                                               
DISORD22 MVC   0(2,R2),ACOAWC                                                   
         LA    R2,11(R3)           W/C DESCRIPTION                              
         GOTO1 AGETWC,ACOAWC                                                    
         BNZ   ERROR                                                            
         CLI   0(R2),C' '                                                       
         BE    DISORD25                                                         
         LA    R2,63-1(R2)         GET TO END OF DESCRIPTIONS                   
         CLI   0(R2),C' '                                                       
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         MVI   1(R2),C','                                                       
         LA    R2,2(R2)                                                         
*                                                                               
DISORD25 MVC   0(L'ACANDESC,R2),WORK                                            
         LA    R2,74(R3)           AMOUNT                                       
         CLI   0(R2),C' '                                                       
         BE    DISORD26                                                         
         LA    R2,1(R2)                                                         
         CLI   0(R2),C' '                                                       
         BNE   *-8                                                              
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
*                                                                               
DISORD26 EDIT  ACOAMT,(10,0(R2)),2,ALIGN=LEFT                                   
*                                                                               
         ZIC   R0,1(R7)            BUMP TO NEXT                                 
         AR    R7,R0                                                            
         CLI   0(R7),X'68'                                                      
         BE    DISORD21                                                         
         B     DISORD28                                                         
*                                                                               
DISORD27 MVC   APOWRKC,SPACES       DISPLAY ONE AMOUNT AND NO W-CODES           
         MVC   APOWNAC,SPACES                                                   
         OI    APOWRKCH+6,X'80'                                                 
         OI    APOWNACH+6,X'80'                                                 
         MVC   WORK1(L'APOVALC),SPACES                                          
         EDIT  ACOAMT,(10,WORK1),2,ALIGN=LEFT                                   
         OC    APOVALC,SPACES                                                   
         CLC   APOVALC(10),WORK1                                                
         BE    DISORD3                                                          
         MVC   APOVALC,WORK1                                                    
         OI    APOVALCH+6,X'80'                                                 
         B     DISORD3                                                          
*                                                                               
DISORD28 OC    APOWRKC,SPACES      AT END OF AMOUNT ELEMENTS, CHECK             
         CLC   APOWRKC,TEMP        FOR CHANGES TO DISPLAY FIELDS                
         BE    *+14                                                             
         MVC   APOWRKC,TEMP        COMMISSIONABLE WORKCODES                     
         OI    APOWRKCH+6,X'80'                                                 
*                                                                               
         OC    APOWNAC,SPACES      THEIR DESCRIPTIONS                           
         CLC   APOWNAC,TEMP+11                                                  
         BE    *+14                                                             
         MVC   APOWNAC,TEMP+11                                                  
         OI    APOWNACH+6,X'80'                                                 
*                                                                               
         OC    APOEXTC,SPACES      THE REST OF THEIR DESCRITPIONS               
         CLC   APOEXTC,TEMP+60                                                  
         BE    *+14                                                             
         MVC   APOEXTC,TEMP+60                                                  
         OI    APOEXTCH+6,X'80'                                                 
*                                                                               
         OC    APOVALC,SPACES      THEIR AMOUNTS                                
         CLC   APOVALC,TEMP+74                                                  
         BE    *+14                                                             
         MVC   APOVALC,TEMP+74                                                  
         OI    APOVALCH+6,X'80'                                                 
*                                                                               
         OC    APOWRKN,SPACES      NON-COMMISSIONABLE WORKCODES                 
         CLC   APOWRKN,TEMP+114                                                 
         BE    *+14                                                             
         MVC   APOWRKN,TEMP+114                                                 
         OI    APOWRKNH+6,X'80'                                                 
*                                                                               
         OC    APOWNAN,SPACES      THEIR DESCRIPTIONS                           
         CLC   APOWNAN,TEMP+125                                                 
         BE    *+14                                                             
         MVC   APOWNAN,TEMP+125                                                 
         OI    APOWNANH+6,X'80'                                                 
*                                                                               
         OC    APOEXTN,SPACES      THE REST OF THEIR DESCRITPIONS               
         CLC   APOEXTN,TEMP+174                                                 
         BE    *+14                                                             
         MVC   APOEXTN,TEMP+174                                                 
         OI    APOEXTNH+6,X'80'                                                 
*                                                                               
         OC    APOVALN,SPACES      THEIR AMOUNTS                                
         CLC   APOVALN,TEMP+188                                                 
         BE    *+14                                                             
         MVC   APOVALN,TEMP+188                                                 
         OI    APOVALNH+6,X'80'                                                 
*                                                                               
         MVC   LWCC1,APOWRKC                                                    
         MVC   LWCC2,APOWRKC+3                                                  
         MVC   LWCC3,APOWRKC+6                                                  
         MVC   LWCC4,APOWRKC+9                                                  
         MVC   LWCN1,APOWRKN                                                    
         MVC   LWCN2,APOWRKN+3                                                  
         MVC   LWCN3,APOWRKN+6                                                  
         MVC   LWCN4,APOWRKN+9                                                  
         B     DISORD2                                                          
*                                                                               
DISORD30 MVI   FLAG,0              DISPLAY NARRATIVE                            
         GOTO1 ANARRDIS,AIOAREA1                                                
         MVI   FLAG,FOOTLINE       AND FOOTLINE                                 
         BASR  RE,RF                                                            
DISORDX  B     VALCLI                                                           
         EJECT                                                                  
*---------------------------------------------------------                      
*        VALIDATE CLIENT CODE                                                   
*---------------------------------------------------------                      
*                                                                               
VALCLI   CLI   TYPE,EXPENSE        SKIP CLIENT IF WE KNOW                       
         BE    VALSUP              THIS IS EXPENSE                              
         GOTO1 AACSRCHC,DMCB,('STMPSTRQ',APOCLIH),ATWA,PRODUL,ACOMFACS,X        
               (X'11',0)                                                        
         CLI   APOCLIH+5,0                                                      
         BNE   VALCLI2                                                          
         MVI   TYPE,EXPENSE        IF NO CLIENT SET AS EXPENSE                  
         B     VALSUP              AND SKIP TO SUPPLIER                         
*                                                                               
VALCLI2  GOTO1 AFVAL,APOCLIH                                                    
         BZ    ERROR               CLIENT REQUIRED                              
         MVI   FERN,TOOLONG                                                     
         CLC   FLDH+5(1),PRODHEIR  CHECK L'CLIENT CODE                          
         BH    ERROR                                                            
         XC    APOEXP,APOEXP                                                    
         OI    APOEXPH+6,X'80'                                                  
*                                                                               
         MVC   KEY,SPACES          BUILD CLIENT KEY                             
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),PRODUL                                                  
         MVC   KEY+3(L'LCLI),FLD                                                
         CLC   LCLI,FLD                                                         
         BE    VALCLIX                                                          
*                                                                               
         XC    LCLI(18),LCLI       CLEAR CLI/PRO/JOB DATA                       
         XC    CLIUNIT(6),CLIUNIT  CLEAR CLI,PROD AND JOB UNITS                 
*                                                                               
         GOTO1 AREAD,AIOAREA2      READ CLIENT & CHECK OK                       
         BNE   ERROR                                                            
*                                                                               
         GOTO1 AGETNAME,AIOAREA2                                                
         MVC   LCLINAME,WORK       SAVE CLIENT NAME                             
         MVC   LCLI,FLD            SAVE CLIENT CODE                             
*                                                                               
         GOTO1 GETPROFL,AIOAREA2   GET CLIENT PROFILE ELEMENT                   
         MVC   CLIUNIT,WORK                                                     
         MVI   MODE,FIRST                                                       
*                                                                               
VALCLIX  GOTO1 AACSRCHC,DMCB,APOCLIH,ATWA,(PRODHEIR,0),                X        
               (X'C0',SDSPNAMQ),KEY,(L'LCLINAME,LCLINAME)                       
*                                                                               
         BAS   RE,SETFAXPR         GET FAX PROFILE OPTIONS                      
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'AORD'                                                 
         NI    WORK,X'FF'-X'40'    LOWER CASE SYS ON-LINE                       
         MVC   WORK+4(1),COMPANY                                                
         MVC   WORK+5(2),=C'SJ'                                                 
         MVC   WORK+12(2),TWAAGY                                                
*                                                                               
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         TM    OFFACST4,X'01'      TEST NEW OFFICES                             
         BO    VALCLI5             YES                                          
*                                                                               
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),CLIUNIT  OFFICE CODE                                  
         B     VALCLI6                                                          
*                                                                               
VALCLI5  MVI   WORK+10,C'+'        NEW OFFICES                                  
         MVC   WORK+14(2),CLIUNIT                                               
*                                                                               
VALCLI6  ZIC   RF,PRODHEIR         L'CLIENT                                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK+7(0),FLD       CLIENT CODE                                  
*                                                                               
         GOTO1 VGETPROF,DMCB,WORK,PROGPROF,VDATAMGR                             
         B     VALPRO                                                           
         DROP  R1                                                               
         EJECT                                                                  
*---------------------------------------------------------                      
*        VALIDATE PRODUCT CODE                                                  
*---------------------------------------------------------                      
*                                                                               
VALPRO   GOTO1 AACSRCHC,DMCB,('STMPSTRQ',APOPROH),ATWA,PRODUL,ACOMFACS,X        
               (X'22',KEY+3)                                                    
         GOTO1 AFVAL,APOPROH                                                    
         BZ    ERROR               PRODUCT REQUIRED                             
         MVI   FERN,TOOLONG                                                     
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
         XC    LPRO(12),LPRO       CLEAR PRO/JOB DATA                           
         XC    PROUNIT(4),PROUNIT  CLEAR PROD AND JOB UNITS                     
*                                                                               
         GOTO1 AREAD,AIOAREA2      READ PRODUCT & CHECK OK                      
         BNE   ERROR                                                            
*                                                                               
         GOTO1 AGETNAME,AIOAREA2                                                
         MVC   LPRONAME,WORK       SAVE PRODUCT NAME                            
         MVC   LPRO,FLD                                                         
*                                                                               
         GOTO1 GETPROFL,AIOAREA2   GET PRODUCT PROFILE ELEMENT                  
         MVC   PROUNIT,WORK                                                     
         MVI   MODE,FIRST                                                       
*                                                                               
VALPRO2  GOTO1 AACSRCHC,DMCB,APOPROH,ATWA,(PRODHEIR+1,PRODHEIR),       X        
               (X'C0',SDSPNAMQ),KEY,(L'LPRONAME,LPRONAME)                       
         B     VALJOB                                                           
         EJECT                                                                  
*---------------------------------------------------------                      
*        VALIDATE JOB                                                           
*---------------------------------------------------------                      
*                                                                               
VALJOB   GOTO1 AACSRCHC,DMCB,('STMPSTRQ',APOJOBH),ATWA,PRODUL,ACOMFACS,X        
               (X'33',KEY+3)                                                    
         GOTO1 AFVAL,APOJOBH                                                    
         BZ    ERROR                                                            
         MVI   FERN,TOOLONG                                                     
         ZIC   RE,PRODHEIR+1                                                    
         ZIC   RF,PRODHEIR+2                                                    
         SR    RF,RE                                                            
         STC   RF,DUB                                                           
         CLC   FLDH+5(1),DUB       CHECK L'JOB CODE                             
         BH    ERROR                                                            
*                                                                               
         LA    RE,KEY+3(RE)                                                     
         MVC   0(L'LJOB,RE),FLD                                                 
*                                                                               
         XC    LJOB,LJOB           CLEAR JOB DATA                               
         XC    JOBUNIT,JOBUNIT     CLEAR JOB UNIT                               
*                                                                               
         GOTO1 AREAD,AIOAREA2      READ JOB RECORD                              
         BNE   ERROR                                                            
*                                                                               
         MVI   FERN,JOBUNAP                                                     
         L     RE,AIOAREA2                                                      
         USING ACTRECD,RE                                                       
         TM    ACTRSTAT,ACTSDRFT                                                
         BNZ   ERROR                                                            
         DROP  RE                                                               
*                                                                               
         GOTO1 AGETNAME,AIOAREA2                                                
         MVC   LJOBNAME,WORK                                                    
*                                                                               
         GOTO1 AACSRCHC,DMCB,APOJOBH,ATWA,(PRODHEIR+2,PRODHEIR+1),     X        
               (X'C0',SDSPNAMQ),KEY,(L'LJOBNAME,LJOBNAME)                       
*                                                                               
         MVC   LJOB,FLD                                                         
*                                                                               
         L     RE,AIOAREA2                                                      
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
         MVI   FERN,CLOSED         CHECK FOR ACCOUNT NOT CLOSED                 
VALJOB1  CLI   0(RE),0                                                          
         BE    ERROR                                                            
         CLI   0(RE),ACSTELQ                                                    
         BE    *+14                                                             
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     VALJOB1                                                          
         TM    (ACSTSTAT-ACSTATD)(RE),X'40'                                     
         BO    ERROR                                                            
         MVI   FERN,LOCKED         OR LOCKED                                    
         TM    (ACSTSTAT-ACSTATD)(RE),X'20'                                     
         BO    ERROR                                                            
         GOTO1 GETPROFL,AIOAREA2   GET JOB PROFILE ELEMENT                      
         MVC   JOBUNIT,WORK                                                     
         MVI   MODE,FIRST                                                       
VALJOB2  MVC   JOBKEY,KEY                                                       
*                                                                               
VALJOB3  L     RE,AIOAREA2                                                      
         CLC   KEY(15),0(RE)       JOB MAY NOT BE IN BUFFER                     
         BE    VALJOB4                                                          
         MVI   FERN,NOTFOUND                                                    
         GOTO1 AREAD,AIOAREA2                                                   
         BNE   ERROR                                                            
*                                                                               
VALJOB4  BAS   RE,RDOPT            ALWAYS READ FOR THE OPTIONS                  
         CLI   PROGPROF+3,0                                                     
         BE    VALJOB10            ANY ESTIMATE ELEMENTS                        
         BAS   RE,GETTAB           GET JOBBER TABLES                            
         GOTO1 =A(LOOKUP),RR=RELO                                               
*                                                                               
         MVI   FERN,ESTERR                                                      
         L     R6,AJOBLOCK                                                      
         USING JBLOCKD,R6                                                       
         L     RE,JBACOLTB         RE=A(COLUMN TABLE)                           
         CLI   JBNEWEST,JBMCSQ     IS THIS A BO JOB?                            
         BNE   VALJOB7                                                          
*                                                                               
         USING MJETABD,RE                                                       
VALJOB5  CLI   MJETTYP,MJETTEQ     ARE WE AT THE END?                           
         BE    ERROR               YES                                          
         CLI   MJETTYP,MJETTWQ     LOOK FOR WORKCODES ONLY                      
         BNE   VALJOB6                                                          
         OC    MJET1RA,MJET1RA     BUT NOT 1R-LEVEL DETAILS                     
         BNZ   VALJOB6                                                          
         CP    MJETVAL+L'MJETVAL(L'MJETVAL),=P'0'                               
         BNE   VALJOB10                                                         
*                                                                               
VALJOB6  XR    R1,R1                                                            
         IC    R1,MJETLEN                                                       
         AR    RE,R1                                                            
         B     VALJOB5                                                          
*                                                                               
         USING JBCOLD,RE                                                        
VALJOB7  LH    R2,JBNROWS          R2=LOOP COUNTER                              
*                                                                               
VALJOB8  CLI   JBCOLTYP,JBCOLTWC   TEST FOR WORKCODE ENTRY                      
         BNE   VALJOB9             NO-SKIP IT                                   
*                                                                               
         CP    JBCOLVAL,=P'0'      TEST FOR NON-ZERO ESTIMATE                   
         BNE   VALJOB10            YES                                          
         CP    JBCOLVAL+L'JBCOLVAL(L'JBCOLVAL),=P'0'                            
         BNE   VALJOB10                                                         
*                                                                               
VALJOB9  AH    RE,JBLCOL           NEXT TABLE ENTRY                             
         BCT   R2,VALJOB8                                                       
         B     ERROR               ALL ZERO ENTRIES                             
*                                                                               
VALJOB10 B     VALOFFC                                                          
         DROP  R6,RE                                                            
         EJECT                                                                  
*---------------------------------------------------------                      
*        VALIDATE OFFICE                                                        
*---------------------------------------------------------                      
*                                                                               
VALOFFC  L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVC   OFFAREC,AIOAREA2                                                 
         MVI   OFFAOPOS,C'C'                                                    
         MVC   OFFAOFFC,CLIUNIT                                                 
         CLC   PROUNIT,SPACES                                                   
         BNH   *+10                                                             
         MVC   OFFAOFFC,PROUNIT                                                 
         CLC   JOBUNIT,SPACES                                                   
         BNH   *+10                                                             
         MVC   OFFAOFFC,JOBUNIT                                                 
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 VOFFAL                                                           
         BE    VALSUP                                                           
         MVI   FERN,SECLOCK                                                     
         B     ERROR                                                            
         DROP  R1                                                               
         EJECT                                                                  
*---------------------------------------------------------                      
*        VALIDATE SUPPLIER CODE                                                 
*---------------------------------------------------------                      
*                                                                               
VALSUP   GOTO1 AACSRCHC,DMCB,('STMPSTRQ',APOSUPH),ATWA,(C'*',SUPPUL),  X        
               ACOMFACS,0                                                       
         GOTO1 AFVAL,APOSUPH                                                    
         BZ    ERROR               SUPPLIER REQUIRED                            
*                                                                               
         XC    SUPADDEL,SUPADDEL   CLEAR SUPPLIER DATA                          
         XC    LSUPP,LSUPP                                                      
*                                                                               
         XC    FAXNUM,FAXNUM       CLEAR FAX NUMBER                             
*                                                                               
         MVC   KEY,SPACES          BUILD KEY                                    
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),FLD+1                                                  
         CLI   FLD,C'*'                                                         
         BE    *+16                                                             
         MVC   KEY+1(2),SUPPUL                                                  
         MVC   KEY+3(12),FLD                                                    
*                                                                               
         GOTO1 AREAD,AIOAREA2      READ SUPPLIER                                
         BNE   ERROR                                                            
*                                                                               
         GOTO1 AGETNAME,AIOAREA2                                                
         MVC   LSUPNAME,WORK       SAVE NAME                                    
         MVC   LSUPP,FLD           SAVE SUPPLIER CODE                           
*                                                                               
         GOTO1 AACSRCHC,DMCB,APOSUPH,ATWA,(C'*',SUPPUL),               X        
               (X'C0',SDSPNAMQ),AIOAREA2,(L'LSUPNAME,LSUPNAME)                  
*                                                                               
         L     R1,AIOAREA2                                                      
         AH    R1,DATADISP                                                      
         SR    RF,RF                                                            
*                                                                               
         MVI   FERN,INVPOST        CHECK FOR BALANCE ELEMENT                    
VALSUP1  CLI   0(R1),0                                                          
         BE    ERROR                                                            
         CLI   0(R1),ABLELQ                                                     
         BE    *+14                                                             
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     VALSUP1                                                          
*                                                                               
         L     R1,AIOAREA2         SAVE ELEMENT DATA                            
         AH    R1,DATADISP                                                      
         SR    RF,RF                                                            
*                                                                               
VALSUP2  CLI   0(R1),0                                                          
         BE    VALSUPX                                                          
*                                                                               
         CLI   0(R1),X'22'         ADDRESS ELEMENT?                             
         BE    VALSUP6                                                          
         CLI   0(R1),X'8C'         OVERRIDE ADDRESS?                            
         BE    VALSUP6                                                          
         CLI   0(R1),X'30'         STATUS ELEMENT?                              
         BE    VALSUP8                                                          
         CLI   0(R1),FFTELQ        FAX NUMBER, EMAIL ADDRESS?                   
         BE    VALSUP10                                                         
*                                                                               
VALSUP4  IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     VALSUP2                                                          
*                                                                               
VALSUP6  MVC   SUPADDEL,0(R1)                                                   
         B     VALSUP4                                                          
*                                                                               
VALSUP8  MVC   LSUPPSTA,ACSTSTAT-ACSTATD(R1)                                    
         B     VALSUP4                                                          
*                                                                               
         USING FFTELD,R1                                                        
VALSUP10 CLI   FFTTYPE,FFTTPFAX    FAX NUMBER?                                  
         BNE   VALSUP20            NO                                           
         IC    RF,FFTDLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FAXNUM(0),FFTDATA   SAVE FAX NUMBER                              
         B     VALSUP4                                                          
*                                                                               
VALSUP20 CLI   FFTTYPE,FFTTEML       EMAIL ADDRESS?                             
         BNE   VALSUP4                NO                                        
         IC    RF,FFTDLEN                                                       
         STC   RF,EMAILADL           SAVE EMAIL LENGTH                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   EMAILAD(0),FFTDATA    SAVE EMAIL ADDRESS                         
         B     VALSUP4                                                          
*                                                                               
VALSUPX  TM    ACTINDS,NEW           ARE WE ADDING AN ORDER?                    
         BZ    VALSUPX2               NO                                        
         TM    LSUPPSTA,X'20'         YES, IS SUPPLIER LOCKED?                  
         BZ    VALSUPX2                NO                                       
         MVI   FERN,LOCKED             YES, PRINT ERROR                         
         B     ERROR                                                            
*                                                                               
VALSUPX2 CLI   TYPE,PRODN                                                       
         BE    GOINIT              THAT IS ALL IF THIS IS PRODUCTION            
         B     VALEXP                                                           
         EJECT                                                                  
*---------------------------------------------------------                      
*        VALIDATE EXPENSE ACCOUNT                                               
*---------------------------------------------------------                      
*                                                                               
VALEXP   CLI   TYPE,EXPENSE                                                     
         BNE   VALEXPA                                                          
         OI    APOCLIH+6,X'80'                                                  
         XC    APOPRO,APOPRO                                                    
         OI    APOPROH+6,X'80'                                                  
         XC    APOJOB,APOJOB                                                    
         OI    APOJOBH+6,X'80'                                                  
         XC    APOWNAC,APOWNAC                                                  
         OI    APOWNACH+6,X'80'                                                 
         XC    APOWNAN,APOWNAN                                                  
         OI    APOWNANH+6,X'80'                                                 
         XC    APOWRKC,APOWRKC                                                  
         OI    APOWRKCH+6,X'80'                                                 
         XC    APOWRKN,APOWRKN                                                  
         OI    APOWRKNH+6,X'80'                                                 
*                                                                               
VALEXPA  GOTO1 AFVAL,APOEXPH                                                    
         BZ    ERROR               REQUIRED                                     
*                                                                               
         MVI   FERN,INVALID        ONLY CERTAIN U/LS ALLOWED                    
         CLC   =C'SE',FLD                                                       
         BE    VALEXPB                                                          
         CLC   =C'GP',FLD                                                       
         BE    VALEXPB                                                          
         CLC   =C'SQ',FLD                                                       
         BE    VALEXPB                                                          
         CLC   =C'SH',FLD                                                       
         BE    VALEXPB                                                          
         B     ERROR                                                            
*                                                                               
VALEXPB  GOTO1 AACSRCHC,DMCB,('STMPSTRQ',APOEXPH),ATWA,0,ACOMFACS,0             
         GOTO1 AFVAL,APOEXPH                                                    
         BZ    ERROR                                                            
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),FLD                                                    
         CLC   LEXP,FLD                                                         
         BE    VALEXPX                                                          
         XC    LEXP,LEXP                                                        
*                                                                               
         GOTO1 AREAD,AIOAREA2      READ EXPENSE ACCOUNT                         
         BNE   ERROR                                                            
         MVI   FERN,INVPOST                                                     
         L     RF,AIOAREA2                                                      
         AH    RF,DATADISP                                                      
*                                                                               
VALEXPC  CLI   0(RF),0             SEE IF IT IS A BOTTOM  LEVEL ACCT            
         BE    ERROR                                                            
         CLI   0(RF),X'32'         BY FINDING A BALANCE ELEMENT                 
         BE    VALEXPE                                                          
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     VALEXPC                                                          
*                                                                               
VALEXPE  L     RE,AIOAREA2                                                      
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
         MVI   FERN,LOCKED         CHECK FOR ACCOUNT NOT LOCKED                 
*                                                                               
VALEXPG  CLI   0(RE),0                                                          
         BE    ERROR                                                            
         CLI   0(RE),X'30'                                                      
         BE    *+14                                                             
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     VALEXPG                                                          
         TM    (ACSTSTAT-ACSTATD)(RE),X'20'                                     
         BO    ERROR                                                            
*                                                                               
         GOTO1 AGETNAME,AIOAREA2                                                
         MVC   LEXPNAME,WORK                                                    
         MVC   LEXP,FLD                                                         
*                                                                               
VALEXPX  MVC   EXPAKEY,KEY                                                      
         GOTO1 AACSRCHC,DMCB,APOEXPH,ATWA,0,(X'C0',SDSPNAMQ),EXPAKEY,  X        
               (L'LEXPNAME,LEXPNAME)                                            
*                                                                               
         BAS   RE,SETFAXPR         GET LEDGER LEVEL(SE) PROFILE FOR FAX         
         B     GOINIT                                                           
         EJECT                                                                  
*---------------------------------------------------------                      
*        TIDY UP BEFORE GOING TO APPLICATION                                    
*---------------------------------------------------------                      
*                                                                               
GOINIT   LA    R0,APOACTH          SOME ACTIONS STOP HERE - SO SET MSG          
         LA    R1,ENTERNXT         AND FADR AND EXIT                            
         CLI   ACTION,DISP         DISPLAY                                      
         BNE   GOINIT1                                                          
         CLI   PRESFLAG,C'Y'       TEST FOR PRESTO ORDER                        
         BNE   *+8                                                              
         LA    R1,PRESORD                                                       
         B     GOINIT2                                                          
*                                                                               
GOINIT1  TM    ACTINDS,TWOSTAGE    2-STAGE ONES (OPEN,CLOSE,CHANGE)             
         BZ    GO                                                               
         LA    R0,APOFRSTH                                                      
         LA    R1,NOWCHANG                                                      
         CLI   ACTION,CHA                                                       
         BE    GOINIT2                                                          
         OI    APOACTH+6,X'81'                                                  
         LA    R0,APOTABH                                                       
         LA    R1,HITOOPEN                                                      
         CLI   ACTION,OPE                                                       
         BE    GOINIT2                                                          
         LA    R1,HITOCLOS                                                      
         CLI   ACTION,CLO                                                       
         BE    GOINIT2                                                          
*                                                                               
         LA    R0,APOACTH                                                       
         LA    R1,HITODEL                                                       
         CLI   ACTION,DEL                                                       
         BE    GOINIT2                                                          
*                                                                               
         LA    R1,HITORES                                                       
         CLI   ACTION,RES                                                       
         BE    GOINIT2                                                          
         B     OKEND                                                            
*                                                                               
GOINIT2  MVC   MSG,SPACES                                                       
         MVC   MSG(17),=C'ORDER DISPLAYED -'                                    
         MVC   MSG+18(22),0(R1)                                                 
         ST    R0,FADR                                                          
         B     OKEND                                                            
*                                                                               
ENTERNXT DC    CL22'ENTER NEXT ACTION     '                                     
PRESORD  DC    CL22'THIS IS A PRESTO ORDER'                                     
HITOOPEN DC    CL22'HIT ENTER TO OPEN     '                                     
HITOCLOS DC    CL22'HIT ENTER TO CLOSE    '                                     
HITODEL  DC    CL22'HIT ENTER TO DELETE   '                                     
HITORES  DC    CL22'HIT ENTER TO RESTORE  '                                     
NOWCHANG DC    CL22'NOW ENTER CHANGES     '                                     
         EJECT                                                                  
*---------------------------------------------------------                      
*        HANDLE INTERFACE TO APPLICATION PROGRAMS.                              
*---------------------------------------------------------                      
*                                                                               
HELPER   L     R1,AACTNTAB         SET SCREEN FOR HELP CALL                     
         MVC   ACTNVALS,0(R1)                                                   
         MVC   HELPFERN,FERN                                                    
*                                                                               
GO       MVC   PHASE,ACTOVER       OVERLAY APPLICATION PHASE                    
GO1      GOTO1 VCALLOV,DMCB,(PHASE,0),0,0                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   APHASE,0(R1)                                                     
*                                  GO TO APPLICATION WITH ACTMODE               
GOPHASE  MVI   FERN,X'FF'                                                       
         LA    R0,APOACTH                                                       
         ST    R0,FADR                                                          
         GOTO1 APHASE                                                           
         CLI   FERN,X'FF'          CHECK FOR ERRORS                             
         BNE   ERROR                                                            
         CLI   ACTION,APR                                                       
         BNE   OKEND                                                            
         L     RE,=A(PRINTRN)                                                   
         A     RE,RELO                                                          
         MVC   ACTNVALS,0(RE)                                                   
         B     GO                                                               
         EJECT                                                                  
*---------------------------------------------------------                      
*        FIND PRODUCTION PROFILE ELEMENT ON CLI,PROD OR JOB RECORD              
*---------------------------------------------------------                      
*                                                                               
*        RECORD IS ADDRESSED BY WORD AT R1                                      
*                                                                               
GETPROFL NTR1                                                                   
         L     R7,0(R1)                                                         
         AH    R7,DATADISP                                                      
         XC    WORK(2),WORK                                                     
GETP2    CLI   0(R7),0                                                          
         BE    EXIT                                                             
         CLI   0(R7),ACPRELQ                                                    
         BE    GETP4                                                            
         ZIC   RF,1(R7)                                                         
         AR    R7,RF                                                            
         B     GETP2                                                            
         USING ACPROFD,R7                                                       
GETP4    MVC   WORK(2),ACPROFFC                                                 
         B     EXIT                                                             
         EJECT                                                                  
*----------------------------------------------------------------------         
*        SAVE THE PROFILE VALUES WHICH ARE USED TO CONTROL THE                  
*        CREATION OF FAX OR EMAIL                                               
*----------------------------------------------------------------------         
*                                                                               
SETFAXPR NTR1                                                                   
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'AORD'                                                 
         NI    WORK,X'FF'-X'40'    LOWER CASE SYS ON-LINE                       
         MVC   WORK+4(1),COMPANY                                                
         MVC   WORK+12(2),TWAAGY                                                
         MVC   WORK+5(2),=C'SE'                                                 
         CLI   TYPE,EXPENSE                                                     
         BE    SETFX50                                                          
*                                                                               
         MVC   WORK+5(2),=C'SJ'                                                 
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         TM    OFFACST4,X'01'      TEST NEW OFFICES                             
         BO    SETFX10             YES                                          
*                                                                               
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),CLIUNIT  OFFICE CODE                                  
         B     SETFX50                                                          
*                                                                               
SETFX10  MVI   WORK+10,C'+'        NEW OFFICES                                  
         MVC   WORK+14(2),CLIUNIT                                               
*                                                                               
SETFX50  GOTO1 VGETPROF,DMCB,WORK,PROFWORK,VDATAMGR                             
         MVC   PRAUTOFX,PROFWORK+10                                             
         MVC   PRFXNPRT,PROFWORK+11                                             
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*------------------------------------------------------------------             
* SUB-ROUTINE TO READ JOB AND GET JOB VALUES--CALLED FROM VALORD                
*------------------------------------------------------------------             
*                                                                               
GETJOB   NTR1  ,                                                                
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),PRODUL                                                  
         MVC   KEY+3(L'LCLI),LCLI  BUILD JOB KEY                                
         ZIC   RE,PRODHEIR                                                      
         LA    RE,KEY+3(RE)                                                     
         MVC   0(L'LPRO,RE),LPRO   SLOT IN PRODUCT                              
         ZIC   RE,PRODHEIR+1                                                    
         LA    RE,KEY+3(RE)                                                     
         MVC   0(L'LJOB,RE),LJOB                                                
*                                                                               
GETJOB2  GOTO1 AREAD,AIOAREA2                                                   
         BE    *+6                                                              
         DC    H'0'                DUMP IF CANNOT FIND IT                       
         BAS   RE,RDOPT            GET JOB OPTIONS                              
         BAS   RE,GETTAB                                                        
         GOTO1 =A(LOOKUP),RR=RELO                                               
         B     EXIT                                                             
         EJECT                                                                  
*-----------------------------------------------------------------              
*        READ THE OPTIONS FOR THE JOB--CALLED FROM VALJOB + GETJOB              
*-----------------------------------------------------------------              
*                                                                               
RDOPT    NTR1  ,                                                                
         L     R6,AGOBLOCK                                                      
         USING GOBLOCKD,R6                                                      
         MVC   GOADM,VDATAMGR                                                   
         MVC   GOSELCUL(1),COMPANY                                              
         MVC   GOSELCUL+1(2),PRODUL PRODUCTION UNIT/LEDGER                      
*                                                                               
         MVC   GOSELCLI,LCLI       SPACE PADDED CLIENT CODE                     
         MVC   GOSELPRO,LPRO                                                    
         MVC   GOSELJOB,LJOB                                                    
*                                                                               
         MVI   GOWHICH,0           BOTH OLD AND NEW OPTIONS                     
*                                                                               
         GOTO1 VGETOPT,DMCB,GOBLOCK                                             
*                                                                               
RDOPTX   B     EXIT                                                             
         DROP  R6                                                               
*---------------------------------------------------------                      
*        LOAD IN JOBBER TABLES AND TO SET THEIR ADDRESSES                       
*---------------------------------------------------------                      
*        --CALLED FROM VALJOB AND GETJOB--                                      
*                                                                               
GETTAB   ST    RE,FULL                                                          
         GOTO1 VCALLOV,DMCB,(X'50',0),0,0                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)            GET A(TABLES)                                
*                                                                               
         LM    R0,R1,0(RF)         GET DISP TO/LENGTH OF COLUMN TABLE           
         AR    R0,RF                                                            
         STM   R0,R1,ACOLTAB                                                    
*                                                                               
         LM    R0,R1,8(RF)                                                      
         AR    R0,RF                                                            
         STM   R0,R1,AOPVTAB                                                    
*                                                                               
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*---------------------------------------------------------                      
*        FORMAT OUTPUT MESSAGE/EXTRA MESSAGE INTO MSG & EXIT.                   
*---------------------------------------------------------                      
*                                                                               
ERROR    GOTO1 VGETMSG,DMCB1,(FERN,MSG),(X'FF',DMCB),(X'12',0)                  
         CLC   XTRAMESS,SPACES                                                  
         BE    OMSG                                                             
         LA    R1,XTRAMESS+L'XTRAMESS-1                                         
         LA    RE,XTRAMESS-1                                                    
         ZIC   RF,DMCB1                                                         
         CLI   0(R1),C' '                                                       
         BNE   *+8                                                              
         BCT   R1,*-8                                                           
         SR    R1,RE               R1=L'XTRAMESS                                
         LA    R1,1(RF,R1)         R1=TOTAL MESSAGE LENGTH                      
         LA    RE,L'MSG                                                         
         CR    R1,RE               CHECK MESSAGE FITS                           
         BH    OMSG                                                             
         LA    RF,MSG+1(RF)        AND IF SO TACK ON EXTRA MESSAGE              
         MVC   0(L'XTRAMESS,RF),XTRAMESS                                        
         B     OMSG                                                             
*                                                                               
OKEND    MVC   LORDNUM,ORDNUMB     SAVE LAST ORDER NUMBER                       
*                                  MOVE MESSAGE TO TWA & TRANSMIT               
OMSG     MVC   APOMSG,MSG                                                       
         OI    APOMSGH+6,X'80'                                                  
         L     R1,FADR                                                          
         OI    6(R1),X'40'         SET CURSOR TO FIELD                          
         B     EXIT                                                             
                                                                                
OKXIT    SR    RB,RB                                                            
ERRXIT   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* STANDARDIZED TEXT GET ROUTINE                                                 
***********************************************************************         
                                                                                
         USING GETTXTD,R2                                                       
TXTGET   NTR1                                                                   
         LA    R2,DMCB1                                                         
         LR    R3,R1                                                            
         XC    GTBLOCK,GTBLOCK     CLEAR WORK BLOCK                             
         MVC   GTMSGNO,2(R3)       MESSAGE NUMBER                               
         MVI   GTMTYP,GTMERR       ERROR TYPE (DEFAULT)                         
         CLI   0(R3),C' '          ANY MESSAGE TYPE ?                           
         BNH   *+10                NO, SKIP                                     
         MVC   GTMTYP,0(R3)        GET MESSAGE TYPE                             
         MVI   GTMSYS,6            ACCOUNT SYSTEM                               
         CLI   8(R3),0             ANY EXTRA OUTPUT AREA ?                      
         BE    TXTGET02            NO, SKIP                                     
         MVC   GTLTXT,8(R3)        LENGTH OF EXTRA OUTPUT                       
         MVC   GTATXT,9(R3)        A(EXTRA OUTPUT DATA)                         
                                                                                
TXTGET02 CLI   12(R3),0            ANY OVERRIDE MESSAGE LANGUAGE ?              
         BE    TXTGET04            NO, SKIP                                     
         MVC   GTMLANG,12(R3)      GET OVERRIDE MESSAGE LANGUAGE                
                                                                                
TXTGET04 OC    13(3,R3),13(R3)     ANY SUBSTITUTION TABLE ?                     
         BZ    TXTGET06            NO, SKIP                                     
         MVC   GTASUBST,13(R3)     YES, SUBSTITUTION TABLE                      
                                                                                
TXTGET06 GOTO1 VGETTXT,GETTXTD     GET THE MESSAGE TEXT                         
         DROP  R1                                                               
                                                                                
         L     R1,FADR                                                          
         OI    6(R1),X'40'         SET CURSOR TO FIELD                          
                                                                                
TXTGETEX B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*---------------------------------------------------------                      
*        EXTRACT AND PRE-VALIDATE AN INPUT FIELD.                               
*---------------------------------------------------------                      
*                                                                               
* ADDRESS OF FIELD HEADER IS PASSED IN R1. RETURN WITH:-                        
*                                                                               
*              FADR     = A(INPUT FIELD HEADER)                                 
*              FERN     = MISSING INPUT FIELD IF NO INPUT                       
*              FNDX     = ZERO                                                  
*              FLDH     = INPUT FIELD HEADER (FLDH(4) = BINARY VALUE            
*                                             FOR NUMERIC FIELD)                
*              FLD      = EXTRACTED & SPACE FILLED INPUT FIELD                  
*                                                                               
* RETURN WITH CC=EQU IF NO INPUT IN FIELD                                       
*                                                                               
FVAL     NTR1  BASE=ABASE1                                                      
         L     RA,ABASE2                                                        
         L     R5,ABASE3                                                        
         MVI   FNDX,0                                                           
         MVI   FERN,NOINPUT                                                     
         ST    R1,FADR                                                          
         XC    FLDH,FLDH                                                        
         MVC   FLDH+4(2),4(R1)                                                  
         MVC   FLD,SPACES                                                       
         CLI   FLDH+5,0                                                         
         BE    FVALX                                                            
         SR    RE,RE                                                            
         IC    RE,FLDH+5                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),8(R1)                                                     
         MVI   FERN,OK                                                          
         TM    FLDH+4,X'08'                                                     
         BZ    FVALX                                                            
         CH    RE,=H'8'                                                         
         BH    FVALX                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         CVB   RE,DUB                                                           
         ST    RE,FLDH                                                          
FVALX    CLI   FERN,NOINPUT                                                     
         B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------                      
*        EXTRACT NAME FROM A RECORD INTO WORK.                                  
*---------------------------------------------------------                      
*                                                                               
*        RECORD IS ADDRESSED BY WORD AT R1.                                     
*                                                                               
GETNAME  NTR1  BASE=ABASE1                                                      
         L     RA,ABASE2                                                        
         L     R5,ABASE3                                                        
         L     R1,0(R1)                                                         
         ST    R1,AIOAREA                                                       
         MVC   WORK,SPACES                                                      
         AH    R1,DATADISP                                                      
         SR    RF,RF                                                            
GETNAME2 CLI   0(R1),0                                                          
         BE    GETNAMEX                                                         
         CLI   0(R1),X'20'                                                      
         BE    *+14                                                             
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     GETNAME2                                                         
         IC    RF,1(R1)                                                         
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     GETNAMEX                                                         
         MVC   WORK(0),2(R1)                                                    
GETNAMEX B     EXIT                                                             
         EJECT                                                                  
*---------------------------------------------------------                      
*       DISPLAY ORDER NARRATIVE                                                 
*---------------------------------------------------------                      
*                                                                               
*       RECORD IS ADDRESSED BY WORD AT R1                                       
*       FLAG = NULL(NORMAL NARRATIVE) OR FOOTLINE                               
*                                                                               
NARRDIS  NTR1  BASE=ABASE1                                                      
         L     RA,ABASE2                                                        
         L     R5,ABASE3                                                        
         L     R7,0(R1)                                                         
         AH    R7,DATADISP                                                      
         LA    R3,APOFRSTH                                                      
         CLI   FLAG,FOOTLINE                                                    
         BNE   *+8                                                              
         LA    R3,APOFOOTH                                                      
         SR    R0,R0               R0 = COUNT OF UNSCAN ENTRIES PENDING         
*                                                                               
NARRDIS2 CLI   0(R7),0             SEARCH FOR COMMENT ELEMENTS                  
         BE    NARRDISE                                                         
         CLI   0(R7),X'3E'                                                      
         BE    NARRDIS4                                                         
*                                                                               
NARRDIS3 ZIC   R1,1(R7)                                                         
         AR    R7,R1                                                            
         B     NARRDIS2                                                         
*                                                                               
         USING ACOMMD,R7                                                        
NARRDIS4 MVI   FLAG1,FOOTLINE      CHECK FOR FOOTLINE REQUIRED                  
         NC    FLAG1,ACOMTYPE                                                   
         CLC   FLAG1,FLAG                                                       
         BNE   NARRDIS3                                                         
         TM    ACOMTYPE,4                                                       
         BO    NARRDIS6                                                         
         LTR   R0,R0               ANY N=123456 ENTRIES PENDING                 
         BZ    *+12                                                             
         BAS   RE,NARREQ           IF SO DISPLAY THEM AS A STRING               
         BNZ   EXIT                (NO MORE ROOM)                               
         ZIC   R4,ACOMLEN          THEN DISPLAY STANDARD COMMENT                
         SH    R4,=H'5'                                                         
         MVC   CRD,SPACES                                                       
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   CRD(0),ACOMMENT                                                  
         BAS   RE,NARRTWA          MOVE TWA IF DIFFERENT & BUMP TWA             
         BNZ   EXIT                                                             
         B     NARRDIS3                                                         
*                                                                               
NARRDIS6 LTR   R0,R0               NON-STANDARD COMMENT - ADD TO UNSCAN         
         BNZ   *+14                BLOCK                                        
         MVI   TEMP,C' '                                                        
         MVC   TEMP+1(L'TEMP-1),TEMP                                            
         LA    RF,20                                                            
         MR    RE,R0                                                            
         LA    RF,TEMP(RF)                                                      
         MVI   0(RF),C'N'                                                       
         AH    R0,=H'1'            BUMP ENTRY COUNT                             
         LA    RE,ACOMMENT         MOVE IN COMMENT NUM LEFT-ALIGNED             
         LA    R1,5                                                             
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         LA    RE,1(RE)                                                         
         BCT   R1,*-12                                                          
         EX    R1,*+8                                                           
         B     NARRDIS3                                                         
         MVC   10(0,RF),0(RE)                                                   
*                                                                               
NARRDISE LTR   R0,R0               AT END                                       
         BZ    *+12                                                             
         BAS   RE,NARREQ           DISPLAY ANY N=123456 ENTRIES PENDING         
         BNZ   EXIT                                                             
         MVC   CRD,SPACES          AND CLEAR REMAINING LINES                    
         BAS   RE,NARRTWA                                                       
         BZ    *-4                                                              
         B     EXIT                                                             
*                                                                               
*---------------------------------------------------------                      
*       UNSCAN ENTRIES                                                          
*---------------------------------------------------------                      
*                                                                               
NARREQ   NTR1                      ROUTINE TO UNSCAN (R0), ENTRIES FROM         
         L     RF,VUNSCAN          TEMP INTO TWA FIELD(S)-R3=A(1ST HDR)         
         GOTO1 ,DMCB,((R0),TEMP),CRDH                                           
         SR    R0,R0                                                            
NARREQ2  MVC   CRDH(1),0(R3)                                                    
         MVC   CRD,SPACES                                                       
         L     RF,VUNSCAN                                                       
         BASR  RE,RF                                                            
         BAS   RE,NARRTWA                                                       
         BNZ   EXIT                                                             
         CLI   0(R1),0             ANY MORE TO DISPLAY                          
         BNE   NARREQ2                                                          
         XIT1  REGS=(R0,R3)        PASS BACK A(NEXT HDR)                        
*                                                                               
NARRTWA  ZIC   R4,0(R3)            ROUTINE TO MOVE CONTENTS OF CRD TO           
         SH    R4,=H'9'            FLD AT 8(R3) IF DIFFERENT, TO                
         EX    R4,NARROC           TRANSMIT,AND BUMP R3 TO NEXT UNPROT          
         EX    R4,NARRCLC          HDR                                          
         BE    *+12                CC = NEQ IF WE REACH END OF NARRATVE         
         EX    R4,NARRMVC               AREA                                    
         OI    6(R3),X'80'                                                      
NARRTWA2 IC    R4,0(R3)                                                         
         AR    R3,R4                                                            
         LA    RF,APOFINLH                                                      
         CLI   FLAG,FOOTLINE                                                    
         BNE   *+8                                                              
         LA    RF,APOFOOTH                                                      
         CR    R3,RF                                                            
         BNH   *+8                                                              
         LTR   RB,RB                                                            
         BR    RE                                                               
         TM    1(R3),X'20'                                                      
         BZR   RE                                                               
         B     NARRTWA2                                                         
NARRDISX DS    0H                                                               
*                                                                               
NARROC   OC    8(0,R3),SPACES                                                   
NARRCLC  CLC   8(0,R3),CRD                                                      
NARRMVC  MVC   8(0,R3),CRD                                                      
         EJECT                                                                  
*---------------------------------------------------------                      
*       GET WORK CODE DESCRIPTION INTO WORK                                     
*---------------------------------------------------------                      
*                                                                               
*       ADDRESS OF WORK CODE IS PASSED IN R1.                                   
*       RETURN WITH CC=NEQ IF NOT FOUND AND FERN IS SET                         
*                                                                               
GETWC    NTR1  BASE=ABASE1                                                      
         L     RA,ABASE2                                                        
         L     R5,ABASE3                                                        
         MVC   WORK(15),SPACES                                                  
         LA    R2,LWCTAB           R2 = A(SAVED W/C TAB ENTRY)                  
         LA    R0,8                     CONTAINS CODE(CL2)/DESC(CL15)           
GETWC2   OC    0(2,R2),0(R2)                                                    
         BZ    GETWC4                                                           
         CLC   0(2,R2),0(R1)                                                    
         BE    GETWC9              WE HAVE W/C IN SAVE STORAGE                  
         LA    R2,L'LWCTAB(R2)                                                  
         BCT   R0,GETWC2                                                        
         LA    R2,LWCTAB                                                        
         XC    0(8*L'LWCTAB,R2),0(R2)                                           
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
GETWC5   MVI   FERN,INVWC                                                       
         B     ERRXIT                                                           
*                                                                               
GETWC6   L     R1,0(R1)            FIND ANALYSIS ELEMENT AND SAVE               
         AH    R1,DATADISP         CODE/DESC                                    
         SR    RF,RF                                                            
GETWC8   CLI   0(R1),0                                                          
         BE    GETWC5                                                           
         CLI   0(R1),X'12'                                                      
         BE    *+14                                                             
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     GETWC8                                                           
         USING ACANALD,R1                                                       
         MVC   0(L'LWCTAB,R2),ACANCODE                                          
*                                                                               
GETWC9   MVC   WORK(15),2(R2)      PASS DESC BACK IN WORK                       
         SR    R0,R0               CC=EQU FOR OK                                
         B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
*---------------------------------------------------------                      
*        ACCOUNT FILE I/O EXECUTIVE.                                            
*---------------------------------------------------------                      
*                                                                               
* I/O IS EXECUTED ON KEY INTO I/O AREA ADDRESSED BY R1. COMMAND IS              
* PASSED IN THE HIGH ORDER BYTE OF RF AS FOLLOWS:-                              
*                                                                               
*              BITS 0-3 = COMMAND NUMBER (1-6 SEE IOCMNDS)                      
*                   4ON = READ/WRITE FROM DIRECTORY/USE BIGKEY                  
*                   5ON = PASS BACK DELETED RECORDS                             
*                   6ON = READ KEY WITH LOCK                                    
*                   7ON = SAVE KEY IN KEYSAVE BEFORE I/O                        
*                                                                               
* RETURN WITH CC=NEQ ON I/O ERROR WITH FERN SET TO ERROR MESSAGE NUM.           
*                                                                               
ACCIO    NTR1  BASE=ABASE1                                                      
         L     RA,ABASE2                                                        
         L     R5,ABASE3                                                        
         STCM  RF,8,DUB            SAVE COMMAND BYTE                            
         L     R1,0(R1)                                                         
         ST    R1,AIOAREA          SAVE A(I/O AREA)                             
         SRL   RF,28                                                            
         SLL   RF,3                                                             
         LA    RF,IOCMNDS-8(RF)                                                 
         ST    RF,DMCB             SET A(COMMAND)                               
                                                                                
         TM    DUB,X'04'                                                        
         BZ    *+8                                                              
         OI    DMCB,X'08'          SET TO PASS BACK DELETES                     
                                                                                
         TM    DUB,X'02'                                                        
         BZ    *+8                                                              
         OI    DMCB,X'80'          SET TO READ WITH LOCK                        
                                                                                
         TM    DUB,X'01'                                                        
         BZ    ACCIO1                                                           
                                                                                
         LA    RE,KEY                                                           
         TM    DUB,X'08'           TEST DIRECTORY IN USE                        
         BZ    *+8                                                              
         LA    RE,BIGKEY                                                        
         MVC   KEYSAVE,0(RE)       SAVE KEY                                     
*                                                                               
ACCIO1   CLC   DMUNLK,0(RF)        TEST COMMAND=DMUNLK                          
         BNE   ACCIO2                                                           
         CLI   EMULATE,C'Y'        TEST EMULATING ACCOUNT FILE                  
         BNE   ACCIO2                                                           
*                                                                               
         GOTO1 VDATAMGR,DMCB,,ACCDIR,KEY,AIOAREA                                
         BE    *+6                 UNLOCK THE DIRECTORY/THEN FILE               
         DC    H'0'                                                             
         GOTO1 (RF),(R1),,ACCMST,KEY,AIOAREA                                    
         BE    ACCIO4                                                           
         DC    H'0'                                                             
*                                                                               
ACCIO2   TM    DUB,X'08'           TEST DIRECTORY BASED COMMAND                 
         BZ    ACCIO3              NO                                           
         GOTO1 VDATAMGR,DMCB,,ACCDIR,BIGKEY,BIGKEY,0                            
         MVC   DISKADD,BIGKEY+(ACCKDA-ACCRECD) GET DISK ADDRESS                 
         B     ACCIO4                                                           
*                                                                               
ACCIO3   GOTO1 VDATAMGR,DMCB,,IOFILE,KEY,AIOAREA                                
*                                                                               
ACCIO4   MVI   FERN,OK             SET FIELD ERROR NUMBER                       
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
         B     EXIT                                                             
         DROP  R5,RA,RB                                                         
         EJECT                                                                  
*---------------------------------------------------------                      
*        LIST OF I/O COMMANDS/FILES                                             
*---------------------------------------------------------                      
*                                                                               
IOCMNDS  DS    0CL8                                                             
         DC    C'DMADD   '                                                      
         DC    C'DMRDHI  '                                                      
         DC    C'DMREAD  '                                                      
         DC    C'DMRSEQ  '                                                      
         DC    C'DMWRT   '                                                      
DMUNLK   DC    C'DMUNLK  '                                                      
IOFILE   DC    C'ACCOUNT '                                                      
         SPACE 2                                                                
ACCDIR   DC    C'ACCDIR  '                                                      
ACCMST   DC    C'ACCMST  '                                                      
*TFADD   DC    C'DTFADD  '                                                      
         EJECT                                                                  
RELO     DS    F                                                                
*                                                                               
*---------------------------------------------------------                      
*        LITERAL DECLARATIONS                                                   
*---------------------------------------------------------                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*---------------------------------------------------------                      
*        LOOK UP THE JOB'S ORIGINAL AND CURRENT ESTIMATES                       
*---------------------------------------------------------                      
*        --CALLED FROM VALJOB AND GETJOB--                                      
*                                                                               
LOOKUP   NMOD1 0,**LOOK**                                                       
         GOTO1 VJOBCOL,DMCB,LOOKFLDH,ACOLIST,ACOMFACS                           
         CLI   4(R1),0             TEST FOR ERROR                               
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AJOBLOCK                                                      
         USING JBLOCKD,R6                                                       
         MVC   JBAJOB,AIOAREA2     A(JOB)                                       
         MVC   JBACOLS,ACOLIST                                                  
         MVC   JBACOM,ACOMFACS                                                  
         MVC   JBAGOBLK,AGOBLOCK                                                
         MVC   JBAIO,AIOAREA4                                                   
         MVC   JBGETOPT,VGETOPT                                                 
         MVC   JBACOLTB,ACOLTAB                                                 
         MVC   JBLCOLTB,LCOLTAB                                                 
         MVC   JBAOPVTB,AOPVTAB                                                 
         MVC   JBLOPVTB,LOPVTAB                                                 
*                                                                               
         MVI   JBSELFUN,JBGETDE    GET BO DETAILS                               
         LA    RE,LOOKFLDH                                                      
         ST    RE,JBORICLI                                                      
*                                                                               
LOOKUP2  GOTO1 VJOBBER,DMCB,AJOBLOCK                                            
         CLI   JBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LOOKUPX  XMOD1 1                                                                
         DROP  R6                                                               
*                                                                               
LOOKFLDH DC    AL1(L'LOOKFLD+8),4X'00',AL1(L'LOOKFLD),2X'00'                    
LOOKFLD  DC    C'OE,CE'                                                         
         EJECT                                                                  
*---------------------------------------------------------                      
*        TABLE OF ADCONS FOR EXTENDED WORKING STORAGE                           
*---------------------------------------------------------                      
*                                                                               
*              BYTE 0-3 = DISPLACMENT TO STORAGE AREA                           
*              BYTE 4-7 = DISPLACEMENT TO ADDRESSS OF STORAGE AREA              
*                                                                               
EXTTAB   DS    0D                                                               
         DC    AL4(GOBLOCKA-ORDWORKD),AL4(AGOBLOCK-ORDWORKD)                    
         DC    AL4(JOBLOCKA-ORDWORKD),AL4(AJOBLOCK-ORDWORKD)                    
         DC    AL4(COLIST-ORDWORKD),AL4(ACOLIST-ORDWORKD)                       
         DC    AL4(SAVE-ORDWORKD),AL4(ASAVE-ORDWORKD)                           
         DC    AL4(OFFBLK-ORDWORKD),AL4(AOFFBLK-ORDWORKD)                       
NEXTTAB  EQU   (*-EXTTAB)/L'EXTTAB                                              
*                                                                               
*---------------------------------------------------------                      
*        TABLE OF CORE-RESIDENT MODULES                                         
*---------------------------------------------------------                      
*                                                                               
CORETAB  DS    0X                                                               
         DC    AL1(QCENTER)                                                     
         DC    AL1(QCHOPPER)                                                    
         DC    AL1(QEDITOR)                                                     
         DC    AL1(QGETOPT)                                                     
         DC    AL1(QJOBBER)                                                     
         DC    AL1(QQSORT)                                                      
         DC    AL1(QSQUASH)                                                     
         DC    AL1(QOFFAL)                                                      
         DC    AL1(QPADDLE)                                                     
NCORES   EQU   *-CORETAB                                                        
*                                                                               
*---------------------------------------------------------                      
*        TABLE OF A&V-TYPES FOR RELOCATING INTO GLOBAL W/S.                     
*---------------------------------------------------------                      
*                                                                               
*              BYTE 0-3 = A/V-TYPE ADDRESS                                      
*                   1-N = HIGH ORDER BYTE VALUES DELIMITED BY X'FF'             
*                                                                               
ROUTTAB  DS    0X                                                               
         DC    VL3(ACJOBCOL),X'00FF'                                            
         DC    VL3(ACSRCHC),X'00FF'                                             
         DC    AL3(ACTNTAB),X'00FF'                                             
         DC    AL3(OPTNTAB),X'00FF'                                             
         DC    AL3(FVAL),X'00FF'                                                
         DC    AL3(GETNAME),X'00FF'                                             
         DC    AL3(NARRDIS),X'00FF'                                             
         DC    AL3(GETWC),X'00FF'                                               
*        DC    AL3(ACCIO),X'102527343640425060182D2F484A58FF'                   
         DC    AL3(ACCIO),X'101825272D2F3436384042484A50586068FF'               
         DC    X'FF'                                                            
         SPACE 1                                                                
*                                                                               
*---------------------------------------------------------                      
*        TABLE OF INPUT ACTIONS.                                                
*---------------------------------------------------------                      
*                                                                               
*              BYTE 0-7 = ACTION NAME                                           
*                   8-9 = SHORT ACTION NAME                                     
*                   10  = ACTION NUMBER                                         
*                   11  = INDICATORS - BIT0ON=DDS-ONLY ACTION                   
*                                      BIT1ON=ADDING ACTION                     
*                                      BIT2ON=PRINTING ACTION                   
*                                      BIT3ON=TWO-STAGE ACTION                  
*                                      BIT4ON=HAS PARAMETERS                    
*                   12  = OVERLAY PHASE NUMBER                                  
*                   13  = SPARE                                                 
*                                                                               
ACTNTAB  DS    0CL14                                                            
         DC    C'HELP    HE',AL1(HLP,0,4,0)                                     
         DC    C'ADD     AD',AL1(ADD,NEW,1,0)                                   
         DC    C'ADDPRINTAP',AL1(APR,NEW+PRINT+PARMS_OK,1,0)                    
         DC    C'CHANGE  CH',AL1(CHA,TWOSTAGE,1,0)                              
         DC    C'CLOSE   CL',AL1(CLO,TWOSTAGE,2,0)                              
         DC    C'DELETE  DE',AL1(DEL,TWOSTAGE,2,0)                              
         DC    C'DISPLAY DI',AL1(DISP,READOK,0,0)                               
         DC    C'EDIT    ED',AL1(EDIT,HASPARMS,1,0)                             
         DC    C'OPEN    OP',AL1(OPE,TWOSTAGE,2,0)                              
PRINTRN  DC    C'PRINT   PR',AL1(PRI,PRINT+PARMS_OK,3,0)                        
         DC    C'PNOCASH PN',AL1(PRI,PRINT,3,0)                                 
         DC    C'RESTORE RE',AL1(RES,TWOSTAGE,2,0)                              
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*---------------------------------------------------------                      
*        TABLE OF INPUT OPTIONS.                                                
*---------------------------------------------------------                      
*                                                                               
*              BYTE 0-7  = OPTION KEYWORD                                       
*                   8    = INDICATORS - BIT 0ON=DDS ONLY OPTION                 
*                                       BIT 1ON=GLOBAL FILTER/OPTION            
*                   9    = MINIMUM LENGTH OF DATA VALUE                         
*                   10   = MAXIMUM LENGTH OF DATA VALUE                         
*                   11-13= A(VALIDATION ROUTINE)                                
OPTNTAB  DS    0CL14                                                            
         DC    X'FF'                                                            
         EJECT                                                                  
STMPSTRQ EQU   X'04'               TEMPSTR PAGE NO. FOR SEARCH                  
SDSPNAMQ EQU   15                  DISP. TO NAME ON SEARCH FIELDS               
*---------------------------------------------------------                      
*        ORDER PROGRAM DSECT                                                    
*---------------------------------------------------------                      
*                                                                               
       ++INCLUDE ACORDDSECT                                                     
         EJECT                                                                  
* ACGENBOTH                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDACCFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDACCFACS                                                      
         PRINT ON                                                               
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
* FAXTRAINF                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAXTRAINF                                                      
         PRINT ON                                                               
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'049ACORD00   10/17/18'                                      
         END                                                                    
