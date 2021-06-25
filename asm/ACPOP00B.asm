*          DATA SET ACPOP00B   AT LEVEL 003 AS OF 05/18/06                      
*PHASE T60A00BA                                                                 
*INCLUDE ACJOBCOL                                                               
*INCLUDE SRCHCALL                                                               
*INCLUDE ACSRCHC                                                                
         TITLE 'ACPOP00 - PRODUCTION ORDERS PLUS - CONTROLLER'                  
ACPOP00  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (WORKX-POPWORKD),**POP**,RA,R5,RR=RF,CLEAR=YES                   
         LR    R9,RC                                                            
         USING POPWORKD,R9         R9=A(GLOBAL W/S)                             
         ST    RF,RELO                                                          
                                                                                
         L     RE,20(R1)                                                        
         MVC   FACFLAG,7(RE)       SAVE CONNECT FLAG                            
         MVC   FACUPD,10(RE)       AND UPDATIVE FACPAK ID                       
                                                                                
         L     R8,4(R1)                                                         
         USING TWAD,R8             R8=A(TWA)                                    
         EJECT                                                                  
***********************************************************************         
*        INITIALIZE WORKING STORAGE VALUES                            *         
***********************************************************************         
                                                                                
         ST    RB,ABASE1                                                        
         ST    RA,ABASE2                                                        
         ST    R5,ABASE3                                                        
         ST    RD,AWORK                                                         
         ST    R8,ATWA                                                          
         MVC   ATIA,12(R1)                                                      
         MVC   ACOMFACS,16(R1)                                                  
         MVC   COMPANY,0(R1)                                                    
         MVC   ATIOB,0(R1)                                                      
                                                                                
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
         CLI   PFKEY,12                                                         
         BNE   *+8                                                              
         MVI   MODE,INIT                                                        
         DROP  RE                                                               
                                                                                
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
                                                                                
         L     R2,=A(CORETAB)                                                   
         A     R2,RELO                                                          
         LA    R3,NCORES           GET ADCONS OF CORE-RESIDENT MODULES          
         LA    R4,COREFACS                                                      
         L     RF,VCALLOV                                                       
         LA    R1,DMCB                                                          
         MVC   DMCB+4(3),=X'D9000A'                                             
                                                                                
INIT02   MVC   DMCB+7(1),0(R2)     SET MODULE NUMBER                            
         GOTO1 (RF),(R1),0                                                      
         MVC   0(4,R4),0(R1)                                                    
         LA    R2,1(R2)            NEXT MODULE                                  
         LA    R4,4(R4)            NEXT ADDRESS POSITION                        
         BCT   R3,INIT02                                                        
                                                                                
*                                  INITIALIZE IO AREAS AND ADDRESSES            
         LA    R0,4                LOOP COUNTER                                 
         LA    R1,C'1'             IO AREA NUMBER                               
         LA    RE,IOAREAS                                                       
         LA    RF,AIOAREA1                                                      
                                                                                
INIT04   MVC   0(8,RE),=C'**IO0**'                                              
         STC   R1,4(RE)            SET IO NUMBER                                
         LA    RE,8(RE)            BUMP AHEAD PAST LABEL                        
         ST    RE,0(RF)            SET IO AREA ADDRESS                          
         LA    R1,1(R1)            INCREMENT IO AREA NUMBER                     
         LA    RE,LIOAREAS(RE)     POINT TO NEXT IO AREA                        
         LA    RF,4(RF)            NEXT ADCON                                   
         BCT   R0,INIT04                                                        
         EJECT                                                                  
*                                  BUILD INTERNAL/EXTERNAL DIRECTORY            
         LA    R1,AROUTINE                                                      
         L     RF,=A(ROUTTAB)                                                   
         A     RF,RELO                                                          
                                                                                
                                                                                
INIT06   ICM   RE,7,0(RF)                                                       
         LA    RE,0(RE)         RELOCATE A/V TYPE                               
         A     RE,RELO                                                          
         LA    RF,3(RF)                                                         
                                                                                
INIT08   ICM   RE,8,0(RF)                                                       
         ST    RE,0(R1)                                                         
         LA    R1,4(R1)                                                         
         LA    RF,1(RF)                                                         
         CLI   0(RF),X'FF'         END OF SUB-LIST                              
         BNE   INIT08                                                           
         LA    RF,1(RF)                                                         
         CLI   0(RF),X'FF'         END OF LIST                                  
         BNE   INIT06                                                           
                                                                                
         L     R1,=A(EXTTAB)                                                    
         A     R1,RELO                                                          
         LA    R0,NEXTTAB          SET ADCONS FOR EXTENDED STORAGE              
INIT10   LM    RE,RF,0(R1)                                                      
         LA    RE,POPWORKD(RE)     COMPUTE ACTUAL ADDRESSES                     
         LA    RF,POPWORKD(RF)                                                  
         ST    RE,0(RF)                                                         
         LA    R1,L'EXTTAB(R1)     NEXT TABLE ENTRY                             
         BCT   R0,INIT10                                                        
*                                  SET OTHER FIELDS                             
         LHI   RE,ACCORFST                                                      
         STH   RE,DATADISP         SET DISP TO FIRST ELEMENT                    
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
         MVC   MSG,SPACES                                                       
         MVC   XTRAMESS,SPACES                                                  
         MVI   DELETADD,C'N'                                                    
         MVI   EMULATE,C'Y'        YES                                          
                                                                                
         LA    R1,APOACTH          SET FADR TO FIRST INPUT FIELD                
         ST    R1,FADR                                                          
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         GOTO1 AREAD,AIOAREA1      GET COMPANY RECORD                           
         BNE   ERROR                                                            
         L     RE,AIOAREA                                                       
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
                                                                                
         USING CPYELD,RE                                                        
INIT12   CLI   CPYEL,0             LOCATE COMPANY ELEMENT                       
         BE    INIT14                                                           
         CLI   CPYEL,CPYELQ                                                     
         BE    *+14                                                             
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     INIT12                                                           
                                                                                
         MVC   PRODUL,CPYPROD                                                   
         MVC   SUPPUL,CPYSUPP                                                   
                                                                                
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVC   OFFACOMF,ACOMFACS                                                
         MVC   OFFAALPH,TWAAGY                                                  
         MVC   OFFACPY,COMPANY                                                  
         MVC   OFFACST1,CPYSTAT1                                                
         MVC   OFFACST2,CPYSTAT2                                                
         MVC   OFFACST3,CPYSTAT3                                                
         MVC   OFFACST4,CPYSTAT4                                                
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
                                                                                
         LA    RE,LWCTAB           CLEAR WORKCODE TABLE                         
         LHI   RF,20*L'LWCTAB                                                   
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         DROP  R1,RE                                                            
         EJECT                                                                  
***********************************************************************         
*        FIRST TIME INITIALIZATION CODE                               *         
***********************************************************************         
                                                                                
         CLI   MODE,INIT                                                        
         BNE   INITX                                                            
                                                                                
INIT14   MVC   KEY+1(2),PRODUL                                                  
         GOTO1 AREAD,AIOAREA1      GET PRODUCTION LEDGER RECORD                 
         BZ    INIT16                                                           
         MVC   XTRAMESS(6),=C'LEDGER'                                           
         MVC   XTRAMESS+7(2),PRODUL                                             
         B     ERROR                                                            
                                                                                
INIT16   LA    R1,PRODHEIR                                                      
         BAS   RE,GETHEIR                    GET HEIRARCHY LENGTHS              
         GOTO1 VDATCON,DMCB,(5,0),(0,WORK)   GET TODAY'S DATE                   
         GOTO1 VDATCON,DMCB,(0,WORK),(1,TODAYP)                                 
         MVC   THISMON(1),WORK+1             SAVE MOS                           
         MVC   THISMON+1(1),WORK+3                                              
         CLI   WORK+2,C'1'                                                      
         BNE   INIT18                                                           
         NI    THISMON+1,X'C3'     (10=A,11=B,12=C)                             
         ZIC   R1,THISMON+1                                                     
         LA    R1,1(R1)                                                         
         STC   R1,THISMON+1                                                     
                                                                                
INIT18   XC    WORK,WORK           SAVE AGENCY-LEVEL PROGRAM PROFILE            
         MVC   WORK(4),=C'AORD'                                                 
         NI    WORK,X'FF'-X'40'    MAKE SYSTEM LOWER CASE                       
         MVC   WORK+4(1),COMPANY                                                
         MVC   WORK+12(2),TWAAGY                                                
         GOTO1 VGETPROF,DMCB,WORK,PROGPROF,VDATAMGR                             
         MVI   MODE,FIRST          SET SAVE W/S INITIALIZED                     
                                                                                
INITX    B     VALACT                                                           
         EJECT                                                                  
***********************************************************************         
*        EXTRACT HEIRARCHY LENGTHS FROM A LEDGER RECORD               *         
*        LENGTHS ARE EXTRACTED FROM RECORD ADDRESSED BY AIOAREA       *         
*        INTO 3-BYTE FIELD ADDRESSED BY R1.                           *         
***********************************************************************         
                                                                                
GETHEIR  NTR1                                                                   
         L     RE,AIOAREA                                                       
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
                                                                                
GETH02   CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RE),ACLELQ                                                     
         BE    *+14                                                             
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     GETH02                                                           
                                                                                
         USING ACLELD,RE                                                        
         MVC   0(1,R1),ACLVLEN+(L'ACLVALS*0)                                    
         MVC   1(1,R1),ACLVLEN+(L'ACLVALS*1)                                    
         MVC   2(1,R1),ACLVLEN+(L'ACLVALS*2)                                    
         B     EXIT                                                             
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE ACTION                                              *         
***********************************************************************         
                                                                                
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
VACT02   CLI   0(R1),X'FF'                                                      
         BE    ERROR               IF INVALID GIVE ERROR MESSAGE                
         CLI   ACTSCAN,2           MATCH ON SHORT IF 2 CHARS                    
         BH    VACT06                                                           
         CLC   ACTDSHT,ACTSCAN+12                                               
         BE    VACT08                                                           
                                                                                
VACT04   LA    R1,ACTDLEN(R1)                                                   
         B     VACT02                                                           
                                                                                
VACT06   EX    RE,*+8              OTHERWISE FULL                               
         B     *+10                                                             
         CLC   ACTDNAME(0),ACTSCAN+12                                           
         BNE   VACT04                                                           
                                                                                
VACT08   MVC   ACTNVALS,0(R1)      MATCH FOUND                                  
         TM    ACTINDS,READOK      IS READ ONLY ACCESS ALLOWED?                 
         BO    VACT12              YES                                          
         TM    FACFLAG,XIROSYS+XIROMODE+XIWRONGF                                
         BZ    VACT12                                                           
                                                                                
         LHI   R2,360                                                           
         TM    FACFLAG,XIROMODE    CONNECTED IN READ ONLY MODE?                 
         BO    VACT10              YES                                          
                                                                                
         LHI   R2,357              MUST BE CONNECTED TO WRONG FACPAK            
         LA    RE,FACUPD                                                        
         ST    RE,DMCB+8                                                        
         MVI   DMCB+8,L'FACUPD                                                  
         TM    FACFLAG,XIWRONGF    CONNECTED TO WRONG FACPAK?                   
         BO    VACT10              YES                                          
                                                                                
         LHI   R2,358              CONNECTED TO READ ONLY SYSTEM                
         XC    DMCB+8(4),DMCB+8                                                 
                                                                                
VACT10   GOTOR TXTGET,DMCB,(0,(R2)),0,,0                                        
         B     EXIT                                                             
                                                                                
VACT12   MVI   FERN,INVACTN                                                     
         TM    ACTINDS,DDSONLY     CHECK FOR DDS ONLY                           
         BZ    *+12                                                             
         CLI   TWAOFFC,C'*'                                                     
         BNE   ERROR                                                            
         CLI   ACTDNUM,1           AMEND TYPE FUNCTION?                         
         BNH   VACT14              NO, DISPLAY                                  
         CLI   ACTDNUM,DI2         IS IT THE 2ND DISPLAY?                       
         BE    VACT14              YES, OK                                      
         CLI   PROGPROF+2,C'Y'     AUTH REQ'D?                                  
         BNE   VACT14              NO                                           
         TM    TWAAUTH,X'10'       ARE YOU AUTH'D?                              
         BO    VACT14              YES                                          
         MVI   FERN,SECLOCK        SECURITY LOCKOUT                             
         B     ERROR                                                            
                                                                                
VACT14   TM    ACTINDS,PARMS_OK    ARE PARMS NOT NEEDED, BUT OK                 
         BO    VACTX               YES                                          
         MVI   FNDX,2                                                           
         TM    ACTINDS,HASPARMS    ARE PARMS NEEDED                             
         BO    VACT16              YES                                          
         MVI   FERN,INVALID        PARMS NOT ALLOWED                            
         CLI   FLAG1,1                                                          
         BH    ERROR                                                            
         B     VACTX                                                            
                                                                                
VACT16   MVI   FERN,NOINPUT        CHECK FOR PARMS (VALIDATE IN OV'LAY)         
         CLI   FLAG1,1                                                          
         BNH   ERROR                                                            
                                                                                
VACTX    B     VALORD                                                           
         EJECT                                                                  
***********************************************************************         
*        VALIDATE ORDER NUMBER                                        *         
***********************************************************************         
                                                                                
VALORD   GOTO1 AFVAL,APONUMH                                                    
         BZ    ERROR               INPUT REQUIRED                               
         CLI   PFKEY,0                                                          
         BE    VORD08                                                           
         CLI   PFKEY,01                                                         
         BNE   VORD06                                                           
         XC    PFKEY,PFKEY                                                      
         CLI   ACTION,DISP                                                      
         BNE   VORD02                                                           
         MVC   ACTSAV,APOACT       SAVE ACTION FOR RETURN                       
         XC    APOACT,APOACT                                                    
         MVC   APOACT(4),=C'DIS2'                                               
         MVI   APOACTH+5,4                                                      
         B     VORD04                                                           
                                                                                
VORD02   CLI   ACTION,CHA                                                       
         BE    *+12                                                             
         CLI   ACTION,ADD                                                       
         BNE   VORD06                                                           
         MVC   ACTSAV,=C'CHA '     CAN'T GO BACK TO ADD                         
         XC    APOACT,APOACT                                                    
         MVC   APOACT(4),=C'CHA2'                                               
         MVI   APOACTH+5,4                                                      
                                                                                
VORD04   OI    APOACTH+6,X'81'     MODIFY                                       
         B     VALACT                                                           
                                                                                
VORD06   CLI   PFKEY,12                                                         
         BNE   VORD08                                                           
         XC    PFKEY,PFKEY                                                      
         XC    APOACT,APOACT                                                    
         MVC   APOACT(3),ACTSAV                                                 
         MVI   APOACTH+5,3                                                      
         B     VORD04                                                           
                                                                                
VORD08   TM    ACTINDS,NEW                                                      
         BZ    VORD10                                                           
         CLI   SCRNUM,X'00'        CHECK IF CORRECT SCREEN LOADED               
         BE    VORD10                                                           
         CLI   SCRNUM,X'FD'                                                     
         BE    VORD10                                                           
         MVC   DMCB+4(3),=X'D9060A'                                             
         MVI   DMCB+7,X'FD'                                                     
         GOTO1 VCALLOV,DMCB,(0,APOWRKTH)                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   SCRNUM,X'FD'                                                     
                                                                                
VORD10   MVI   FERN,INVNUM                                                      
         CLI   ACTION,PRI          OR (PR ONLY) LINEUP                          
         BNE   *+14                                                             
         CLC   FLD(6),=C'LINEUP'                                                
         BE    GOOVER                                                           
         MVI   FERN,INVALID                                                     
         ZIC   RF,FLDH+5                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   FLD(0),=CL6'AUTO'                                                
         BNE   VORD12                                                           
         TM    ACTINDS,NEW                                                      
         BZ    ERROR                                                            
         B     VORD26                                                           
                                                                                
VORD12   MVC   ORDNUMB(1),FLD      SAVE FIRST CHARACTER OF INPUT                
         MVC   WORK(5),=5X'F0'     TEST THE REST FOR NUMERIC                    
         BCTR  RF,0                AS FORMAT CAN BE 1 ALPHA PLUS                
         EX    RF,*+8              UP TO 5 NUMERIC                              
         B     *+10                                                             
         MVZ   WORK(0),FLD+1                                                    
         CLC   WORK(5),=5X'F0'                                                  
         BNE   ERROR                                                            
         CLI   FLDH+5,6            FOR SHORT INPUT WHICH STARTS WITH A          
         BE    VORD14              NUMBER,GO BACK TO THE OLD WAY                
         CLI   FLD,C'0'            IE PAD FROM LEFT WITH ZEROES                 
         BL    VORD14                                                           
         LA    RF,1(RF)                                                         
         MVI   ORDNUMB,C'0'                                                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD(0)                                                       
         B     VORD16                                                           
                                                                                
VORD14   EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FLD+1(0)                                                     
                                                                                
VORD16   CVB   RF,DUB                                                           
         STCM  RF,7,ORDNUMB+1                                                   
         CLC   ORDNUMB,=X'F0000000' MUST NOT BE ZEROES                          
         BE    ERROR                                                            
         LA    R7,KEY                                                           
         USING ORDRECD,R7                                                       
         XC    ORDKEY,ORDKEY       OTHERWISE READ FOR ORDER REC                 
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,COMPANY                                                  
         LA    RF,ORDNUMB+1                                                     
         LA    RE,ORDKORD+1                                                     
         EDIT  (3,0(RF)),(5,0(RE)),FILL=0                                       
         MVC   ORDKORD(1),ORDNUMB                                               
         OC    APONUM,SPACES                                                    
         CLC   APONUM,ORDKORD      ENSURE 6 DIGIT NUMBER DISPLAYED              
         BE    *+14                                                             
         MVC   APONUM,ORDKORD                                                   
         OI    APONUMH+6,X'80'                                                  
         DROP  R7                                                               
                                                                                
         LA    R1,AIOAREA1                                                      
         L     RF,AREADL                                                        
         CLI   ACTION,EDIT                                                      
         BE    VORD18                                                           
         TM    ACTINDS,NEW         (LOCK IN CASE ITS DELETED)                   
         BO    VORD18                                                           
         TM    ACTINDS,TWOSTAGE                                                 
         BZ    *+14                                                             
         CLC   ORDNUMB,LORDNUM                                                  
         BE    *+8                                                              
         L     RF,AREAD                                                         
                                                                                
VORD18   BASR  RE,RF                                                            
         L     RE,AIOAREA1         RE=A(RECORD)                                 
         TM    ACTINDS,NEW                                                      
         BO    VORDADD                                                          
                                                                                
         MVI   FERN,NOTFOUND                                                    
         TM    DMCB+8,X'10'                                                     
         BNZ   ERROR                                                            
                                                                                
         CLI   ACTION,RES                                                       
         BE    VORD20             ALLOW RESTORE X'80' DELETED RECS              
                                                                                
         MVI   FERN,DELETED                                                     
         TM    DMCB+8,2                                                         
         BNZ   ERROR                                                            
                                                                                
VORD20   BAS   RE,PRESTO           CHECK PRESTO ORDER/CERTAIN ACTION            
         BNE   ERROR               DO NOT ALLOW ACTION                          
                                                                                
         L     RE,AIOAREA1         RE=A(RECORD)                                 
                                                                                
         CLI   ACTION,DEL                                                       
         BE    VORDDEL                                                          
                                                                                
         CLI   ACTION,RES                                                       
         BE    VORDRES                                                          
                                                                                
         CLI   ACTION,CLO                                                       
         BE    VORDCLO                                                          
                                                                                
         CLI   ACTION,OPE                                                       
         BE    VORDOPE                                                          
                                                                                
         CLI   ACTION,DISP                                                      
         BE    VORDDIS                                                          
                                                                                
         CLI   ACTION,DI2                                                       
         BE    VORDDIS                                                          
                                                                                
         B     VORDDIS             ALL OTHER ACTIONS MUST MEET THE              
*                                  CRITERIA OF THE ORDER RECORD NOT             
*                                  BEING DELETED OR CLOSED.                     
*                                                                               
         USING ORDRECD,RE                                                       
VORDDEL  DS    0H                  DELETE AN ORDER RECORD                       
         CLI   ORDRSTAT,ORDSLDEL                                                
         BE    ERRMSG                                                           
         CLI   ORDRSTAT,ORDSDEL                                                 
         BE    ERRMSG                                                           
         B     VORD22                                                           
                                                                                
VORDRES  DS    0H                  RESTORE AN ORDER RECORD                      
         CLI   ORDRSTAT,ORDSLDEL                                                
         BE    VORD22                                                           
         CLI   ORDRSTAT,ORDSDEL                                                 
         BE    VORD22                                                           
         B     ERRMSG                                                           
                                                                                
VORDCLO  DS    0H                  CLOSE AN ORDER RECORD                        
         CLI   ORDRSTAT,X'00'                                                   
         BNE   ERRMSG                                                           
         B     VORD22                                                           
                                                                                
VORDOPE  DS    0H                  OPEN AN ORDER RECORD                         
         CLI   ORDRSTAT,ORDSFMCH                                                
         BNE   ERRMSG                                                           
         B     VORD22                                                           
                                                                                
VORDDIS  DS    0H                  DISPLAY AN ORDER RECORD                      
         CLI   ORDRSTAT,ORDSLDEL                                                
         BE    ERRMSG                                                           
         CLI   ORDRSTAT,ORDSFMCH                                                
         BE    ERRMSG                                                           
         CLI   ORDRSTAT,ORDSDEL                                                 
         BE    ERRMSG                                                           
         B     VORD22                                                           
                                                                                
VORDADD  DS    0H                                                               
         B     VORD22                                                           
                                                                                
VORD22   TM    ACTINDS,TWOSTAGE    IF SAME AS LAST AND TWO-STATE GO             
         BZ    VORD26              TO OVERLAY                                   
         CLC   ORDNUMB,LORDNUM     TEST IF ORDER NUMBER CHANGED                 
         BNE   VORD26              YES                                          
         CLC   ACTION,LACTION      HAS THE ACTION CHANGED?                      
         BE    VORD24              NO                                           
         MVC   BYTE,ACTION         YES, BUT IS THE SCREEN THE SAME?             
         NI    BYTE,X'80'                                                       
         MVC   BYTE2,LACTION                                                    
         NI    BYTE2,X'80'                                                      
         CLC   BYTE,BYTE2                                                       
         BNE   VORD26              NO                                           
                                                                                
VORD24   CLI   TYPE,EXPENSE        YES, TEST IF EXPENSE PO                      
         BE    GOOVER              YES-NO NEED FOR JOB LOOKUP                   
         BAS   RE,GETJOB           YES-GET JOB + DO LOOKUP FOR OVERLAY          
         B     GOOVER                                                           
                                                                                
VORD26   TM    ACTINDS,NEW                                                      
         BZ    DISORD                                                           
         XC    LORDNUM,LORDNUM                                                  
         OC    APODATS,APODATS                                                  
         BZ    *+14                                                             
         XC    APODATS,APODATS                                                  
         OI    APODATSH+6,X'80'                                                 
         MVI   TYPE,PRODN          DEFAULT TYPE ON ADD                          
         B     VALCLI                                                           
         EJECT                                                                  
***********************************************************************         
*        GET CORRECT STATUS MESSAGE                                   *         
***********************************************************************         
                                                                                
ERRMSG   MVI   FERN,ORDDEL         IS ORDER DELETED?                            
         CLI   ORDRSTAT,ORDSLDEL                                                
         BE    ERROR                                                            
                                                                                
         MVI   FERN,ORDCLO         IS ORDER RECORD CLOSED?                      
         CLI   ORDRSTAT,ORDSFMCH                                                
         BE    ERROR                                                            
                                                                                
         MVI   FERN,ORDOPN         IS ORDER RECORD OPEN?                        
         CLI   ORDRSTAT,X'00'                                                   
         BE    ERROR                                                            
                                                                                
         MVI   FERN,INVSTAT                                                     
         B     ERROR                                                            
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
*        CHECK FOR UPDATIVE ACTIONS BESIDES CLOSE OR OPEN             *         
*        ON A PRESTO ORDER.                                           *         
*        ON EXIT, CC=EQ FOR OK, CC=NEQ AND FERN, MSG SET              *         
***********************************************************************         
                                                                                
PRESTO   NTR1  ,                                                                
         L     RF,AIOAREA1                                                      
         AH    RF,DATADISP                                                      
         USING ORDELD,RF                                                        
                                                                                
PRESTO2  CLI   ORDEL,0                                                          
         BE    OKXIT                                                            
         CLI   ORDEL,ORDELQ                                                     
         BE    PRESTO4                                                          
         ZIC   R1,ORDLN                                                         
         AR    RF,R1                                                            
         B     PRESTO2                                                          
                                                                                
PRESTO4  CLI   ORDLN,ORDLN2Q       TEST ELEMENT IS LONG ENOUGH                  
         BL    OKXIT               NOT LONG ENOUGH FOR PRESTO FLAG              
         TM    ORDSTAT,ORDSPRES    TEST FOR PRESTO ORDER                        
         BZ    OKXIT               NO                                           
                                                                                
         MVI   PRESFLAG,C'Y'       SET FLAG FOR PRESTO ORDER                    
                                                                                
         CLI   ACTION,EDIT                                                      
         BE    PRESTO6                                                          
         CLI   ACTION,CHA                                                       
         BE    PRESTO6                                                          
         CLI   ACTION,CH2                                                       
         BE    PRESTO6                                                          
         CLI   ACTION,DEL                                                       
         BE    PRESTO6                                                          
         CLI   ACTION,RES                                                       
         BE    PRESTO6                                                          
         B     OKXIT                                                            
                                                                                
PRESTO6  MVI   FERN,PRESTORD                                                    
         B     ERRXIT                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
*        DISPLAY ORDER                                                *         
***********************************************************************         
                                                                                
DISORD   SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         LA    RE,TEMP                                                          
         LHI   RF,L'TEMP           CLEAR BUILD AREA                             
         MVCL  RE,R0                                                            
                                                                                
         LA    R3,TEMP             ADDRESS TABLE OF WORKCODES ETC.              
                                                                                
         L     R7,AIOAREA1                                                      
         AH    R7,DATADISP                                                      
                                                                                
DORD02   CLI   0(R7),0             FIND ELEMENTS                                
         BE    DORDX                                                            
         CLI   0(R7),ORDELQ                                                     
         BE    DORD06                                                           
         CLI   0(R7),OAMELQ                                                     
         BE    DORD20                                                           
                                                                                
DORD04   ZIC   R0,1(R7)                                                         
         AR    R7,R0                                                            
         B     DORD02                                                           
                                                                                
         USING ORDELD,R7                                                        
DORD06   MVI   TYPE,PRODN          ORDER ELEMENT                                
         CLC   PRODUL,ORDJOB+1                                                  
         BE    *+8                                                              
         MVI   TYPE,EXPENSE                                                     
         MVC   CRD,SPACES                                                       
                                                                                
         GOTO1 VDATCON,DMCB,(1,ORDDATE),(8,APODAT)                              
         OI    APODATH+6,X'80'                                                  
         MVC   LDATE,ORDDATE                                                    
         CLI   ORDAMNO,0                                                        
         BE    DORD08                                                           
         MVC   CRD(14),=C'LAST AMENDMENT'                                       
         LA    R2,CRD                                                           
         EDIT  ORDAMNO,(3,15(R2)),ALIGN=LEFT                                    
         AR    R2,R0                                                            
         MVI   15(R2),C','                                                      
         GOTO1 VDATCON,DMCB,(1,ORDAMDT),(8,16(R2))                              
                                                                                
DORD08   OC    APODATS,SPACES                                                   
         CLC   APODATS,CRD                                                      
         BE    DORD10                                                           
         MVC   APODATS,CRD                                                      
         OI    APODATSH+6,X'80'                                                 
                                                                                
DORD10   OC    APOAUT,SPACES       AUTHORISATION                                
         CLC   ORDAUTH,APOAUT                                                   
         BE    *+14                                                             
         MVC   APOAUT,ORDAUTH                                                   
         OI    APOAUTH+6,X'80'                                                  
                                                                                
         OC    ORDDDTE,ORDDDTE                                                  
         BZ    DORD12                                                           
         GOTO1 VDATCON,DMCB,(1,ORDDDTE),(5,APODDTE)                             
         OI    APODDTEH+6,X'80'                                                 
                                                                                
DORD12   CLI   ORDLN,ORDLN2Q                                                    
         BNE   DORD14                                                           
         OC    APOATTN,SPACES                                                   
         CLC   ORDATTN,APOATTN                                                  
         BE    *+14                                                             
         MVC   APOATTN,ORDATTN                                                  
         OI    APOATTNH+6,X'80'                                                 
         OC    APOTAX,SPACES                                                    
         CLC   ORDTAX,APOTAX                                                    
         BE    *+14                                                             
         MVC   APOTAX,ORDTAX                                                    
         OI    APOTAXH+6,X'80'                                                  
                                                                                
DORD14   SR    R1,R1               SET UP CLI/PRO/JOB CODES & INPUT LEN         
         CLI   TYPE,EXPENSE        EXCEPT IF IT IS AN EXPENSE ORDER             
         BE    DORD16                                                           
         GOTO1 AACSRCHC,DMCB,APOCLIH,ATWA,(PRODHEIR,0),                X        
               (X'C0',SDSPNAMQ),ORDJOB,0                                        
         GOTO1 (RF),(R1),APOPROH,,(PRODHEIR+1,PRODHEIR),,,                      
         GOTO1 (RF),(R1),APOJOBH,,(PRODHEIR+2,PRODHEIR+1),,,                    
         XC    APOEXP,APOEXP                                                    
         OI    APOEXPH+6,X'80'                                                  
         B     DORD18                                                           
                                                                                
DORD16   GOTO1 AACSRCHC,DMCB,APOEXPH,ATWA,0,(X'C0',SDSPNAMQ),ORDEXP,0           
         XC    APOCLI,APOCLI                                                    
         OI    APOCLIH+6,X'80'                                                  
         XC    APOPRO,APOPRO                                                    
         OI    APOPROH+6,X'80'                                                  
         XC    APOJOB,APOJOB                                                    
         OI    APOJOBH+6,X'80'                                                  
                                                                                
DORD18   GOTO1 AACSRCHC,DMCB,APOSUPH,ATWA,(C'*',SUPPUL),               X        
               (X'C0',SDSPNAMQ),ORDSUP,0                                        
         B     DORD04                                                           
         DROP  R7                                                               
                                                                                
         USING OAMELD,R7                                                        
DORD20   CLI   ACTION,CH2                                                       
         BE    DORD22                                                           
         CLI   ACTION,DI2                                                       
         BE    DORD22                                                           
         CLI   ACTION,EDIT                                                      
         BNE   DORD24                                                           
                                                                                
DORD22   MVC   DMCB+4(3),=X'D9060A'                                             
         MVI   DMCB+7,X'FE'                                                     
         GOTO1 VCALLOV,DMCB,(0,APOWRKTH)                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   SCRNUM,X'FE'                                                     
                                                                                
         MVI   FLAG,0              DISPLAY THE NARRATIVE                        
         GOTO1 ANARRDIS,AIOAREA1                                                
         MVI   FLAG,FOOTLINE       DISPLAY THE FOOTLINE                         
         GOTO1 ANARRDIS,AIOAREA1                                                
         B     DORDX                                                            
                                                                                
         USING WRKTABD,R3                                                       
DORD24   MVC   DMCB+4(3),=X'D9060A'                                             
         MVI   DMCB+7,X'FD'                                                     
         GOTO1 VCALLOV,DMCB,(0,APOWRKTH)                                        
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVI   SCRNUM,X'FD'                                                     
                                                                                
         MVC   WRKWORK,OAMWORK     PRODUCTION ORDER                             
         MVI   WRKSTAT,C'C'                                                     
         TM    OAMSTAT,OAMSNOCM                                                 
         BZ    *+8                 ASSUME COMMISSIONABLE                        
         MVI   WRKSTAT,C'N'                                                     
                                                                                
         CLI   TYPE,EXPENSE        ORDER AMOUNT ELEMENT                         
         BE    DORD26                                                           
                                                                                
         GOTO1 AGETWC,OAMWORK                                                   
         BNZ   ERROR                                                            
         MVC   WRKDES,WORK                                                      
                                                                                
DORD26   EDIT  OAMAMNT,WRKAMT,2,ALIGN=RIGHT,MINUS=YES                           
         AHI   R3,WRKTABL                                                       
                                                                                
         ZIC   R0,1(R7)            BUMP TO ELEMENT                              
         AR    R7,R0                                                            
         CLI   0(R7),OAMELQ                                                     
         BE    DORD24                                                           
                                                                                
         LA    R0,20               MAX NUMBER OF WORKCODES                      
         LA    R2,APOWRKH                                                       
         LA    R3,TEMP             FILL IN THE SCREEN NOW                       
                                                                                
DORD28   OC    8(L'WRKWORK,R2),SPACES                                           
         MVC   8(L'WRKWORK,R2),WRKWORK                                          
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
                                                                                
         OC    8(L'WRKAMT,R2),SPACES                                            
         MVC   8(L'WRKAMT,R2),WRKAMT                                            
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
                                                                                
         OC    8(L'WRKSTAT,R2),SPACES                                           
         MVC   8(L'WRKSTAT,R2),WRKSTAT                                          
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
                                                                                
         OC    8(L'WRKDES,R2),SPACES                                            
         MVC   8(L'WRKDES,R2),WRKDES                                            
         OI    6(R2),X'80'                                                      
         BAS   RE,BUMP                                                          
                                                                                
         AHI   R3,WRKTABL                                                       
         BCT   R0,DORD28                                                        
         B     DORD02                                                           
                                                                                
DORDX    B     VALCLI                                                           
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE CLIENT                                              *         
***********************************************************************         
                                                                                
VALCLI   CLI   TYPE,EXPENSE        SKIP CLIENT IF WE KNOW                       
         BE    VALSUP              THIS IS EXPENSE                              
         GOTO1 AACSRCHC,DMCB,('STMPSTRQ',APOCLIH),ATWA,PRODUL,ACOMFACS,X        
               (X'11',0)                                                        
         CLI   APOCLIH+5,0                                                      
         BNE   VCLI02                                                           
         MVI   TYPE,EXPENSE        IF NO CLIENT SET AS EXPENSE                  
         B     VALSUP              AND SKIP TO SUPPLIER                         
                                                                                
VCLI02   GOTO1 AFVAL,APOCLIH                                                    
         BZ    ERROR               CLIENT REQUIRED                              
         MVI   FERN,TOOLONG                                                     
         CLC   FLDH+5(1),PRODHEIR  CHECK L'CLIENT CODE                          
         BH    ERROR                                                            
         XC    APOEXP,APOEXP                                                    
         OI    APOEXPH+6,X'80'                                                  
                                                                                
         MVC   KEY,SPACES          BUILD CLIENT KEY                             
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),PRODUL                                                  
         MVC   KEY+3(L'LCLI),FLD                                                
         CLC   LCLI,FLD                                                         
         BE    VCLI04                                                           
                                                                                
         XC    LCLI(18),LCLI       CLEAR CLI/PRO/JOB DATA                       
         XC    CLIUNIT(6),CLIUNIT  CLEAR CLI,PROD AND JOB UNITS                 
                                                                                
         GOTO1 AREAD,AIOAREA2      READ CLIENT & CHECK OK                       
         BNE   ERROR                                                            
                                                                                
         GOTO1 AGETNAME,AIOAREA2                                                
         MVC   LCLINAME,WORK       SAVE CLIENT NAME                             
         MVC   LCLI,FLD            SAVE CLIENT CODE                             
                                                                                
         GOTO1 GETPROFL,AIOAREA2   GET CLIENT PROFILE ELEMENT                   
         MVC   CLIUNIT,WORK                                                     
         MVI   MODE,FIRST                                                       
                                                                                
VCLI04   GOTO1 AACSRCHC,DMCB,APOCLIH,ATWA,(PRODHEIR,0),                X        
               (X'C0',SDSPNAMQ),KEY,(L'LCLINAME,LCLINAME)                       
                                                                                
         BAS   RE,SETFAXPR         GET FAX PROFILE OPTIONS                      
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'AORD'                                                 
         NI    WORK,X'FF'-X'40'    LOWER CASE SYS ON-LINE                       
         MVC   WORK+4(1),COMPANY                                                
         MVC   WORK+5(2),=C'SJ'                                                 
         MVC   WORK+12(2),TWAAGY                                                
                                                                                
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         TM    OFFACST4,X'01'      TEST NEW OFFICES                             
         BO    VCLI06              YES                                          
                                                                                
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),CLIUNIT  OFFICE CODE                                  
         B     VCLI08                                                           
                                                                                
VCLI06   MVI   WORK+10,C'+'        NEW OFFICES                                  
         MVC   WORK+14(2),CLIUNIT                                               
                                                                                
VCLI08   ZIC   RF,PRODHEIR         L'CLIENT                                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK+7(0),FLD       CLIENT CODE                                  
                                                                                
         GOTO1 VGETPROF,DMCB,WORK,PROGPROF,VDATAMGR                             
         B     VALPRO                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*        VALIDATE PRODUCT                                             *         
***********************************************************************         
                                                                                
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
                                                                                
         LA    RE,KEY+3(RE)        BUILD PRODUCT KEY                            
         MVC   0(L'LPRO,RE),FLD                                                 
         CLC   LPRO,FLD                                                         
         BE    VPRO02                                                           
                                                                                
         XC    LPRO(12),LPRO       CLEAR PRO/JOB DATA                           
         XC    PROUNIT(4),PROUNIT  CLEAR PROD AND JOB UNITS                     
                                                                                
         GOTO1 AREAD,AIOAREA2      READ PRODUCT & CHECK OK                      
         BNE   ERROR                                                            
                                                                                
         GOTO1 AGETNAME,AIOAREA2                                                
         MVC   LPRONAME,WORK       SAVE PRODUCT NAME                            
         MVC   LPRO,FLD                                                         
                                                                                
         GOTO1 GETPROFL,AIOAREA2   GET PRODUCT PROFILE ELEMENT                  
         MVC   PROUNIT,WORK                                                     
         MVI   MODE,FIRST                                                       
                                                                                
VPRO02   GOTO1 AACSRCHC,DMCB,APOPROH,ATWA,(PRODHEIR+1,PRODHEIR),       X        
               (X'C0',SDSPNAMQ),KEY,(L'LPRONAME,LPRONAME)                       
         B     VALJOB                                                           
         EJECT                                                                  
***********************************************************************         
*        VALIDATE JOB                                                 *         
***********************************************************************         
                                                                                
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
                                                                                
         LA    RE,KEY+3(RE)                                                     
         MVC   0(L'LJOB,RE),FLD                                                 
                                                                                
         XC    LJOB,LJOB           CLEAR JOB DATA                               
         XC    JOBUNIT,JOBUNIT     CLEAR JOB UNIT                               
                                                                                
         GOTO1 AREAD,AIOAREA2      READ JOB RECORD                              
         BNE   ERROR                                                            
                                                                                
         GOTO1 AGETNAME,AIOAREA2                                                
         MVC   LJOBNAME,WORK                                                    
                                                                                
         GOTO1 AACSRCHC,DMCB,APOJOBH,ATWA,(PRODHEIR+2,PRODHEIR+1),     X        
               (X'C0',SDSPNAMQ),KEY,(L'LJOBNAME,LJOBNAME)                       
                                                                                
         MVC   LJOB,FLD                                                         
                                                                                
         L     RE,AIOAREA2                                                      
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
         MVI   FERN,CLOSED         CHECK FOR ACCOUNT NOT CLOSED                 
                                                                                
         USING RSTELD,RE                                                        
VJOB02   CLI   0(RE),0                                                          
         BE    ERROR                                                            
         CLI   0(RE),RSTELQ                                                     
         BE    *+14                                                             
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     VJOB02                                                           
                                                                                
         TM    RSTSTAT,RSTSACIC    CLOSED                                       
         BO    ERROR                                                            
         MVI   FERN,LOCKED         OR LOCKED                                    
         TM    RSTSTAT,RSTSACIL                                                 
         BO    ERROR                                                            
         GOTO1 GETPROFL,AIOAREA2   GET JOB PROFILE ELEMENT                      
                                                                                
         MVC   JOBUNIT,WORK                                                     
         MVI   MODE,FIRST                                                       
         MVC   JOBKEY,KEY                                                       
                                                                                
         L     RE,AIOAREA2                                                      
         CLC   KEY(15),0(RE)       JOB MAY NOT BE IN BUFFER                     
         BE    VJOB04                                                           
         MVI   FERN,NOTFOUND                                                    
         GOTO1 AREAD,AIOAREA2                                                   
         BNE   ERROR                                                            
                                                                                
VJOB04   BAS   RE,RDOPT            ALWAYS READ FOR THE OPTIONS                  
         CLI   PROGPROF+3,0                                                     
         BE    VJOB10              ANY ESTIMATE ELEMENTS                        
         BAS   RE,GETTAB           GET JOBBER TABLES                            
         GOTO1 =A(LOOKUP),RR=RELO                                               
                                                                                
         MVI   FERN,ESTERR                                                      
         L     R6,AJOBLOCK                                                      
         USING JBLOCKD,R6                                                       
         L     RE,JBACOLTB         RE=A(COLUMN TABLE)                           
         USING JBCOLD,RE                                                        
         LH    R2,JBNROWS          R2=LOOP COUNTER                              
                                                                                
VJOB06   CLI   JBCOLTYP,JBCOLTWC   TEST FOR WORKCODE ENTRY                      
         BNE   VJOB08              NO-SKIP IT                                   
                                                                                
         CP    JBCOLVAL,=P'0'      TEST FOR NON-ZERO ESTIMATE                   
         BNE   VJOB10              YES                                          
         CP    JBCOLVAL+L'JBCOLVAL(L'JBCOLVAL),=P'0'                            
         BNE   VJOB10                                                           
                                                                                
VJOB08   AH    RE,JBLCOL           NEXT TABLE ENTRY                             
         BCT   R2,VJOB06                                                        
         B     ERROR               ALL ZERO ENTRIES                             
                                                                                
VJOB10   B     VALOFFC                                                          
         DROP  R6,RE                                                            
         EJECT                                                                  
***********************************************************************         
*        VALIDATE OFFICE                                              *         
***********************************************************************         
                                                                                
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
***********************************************************************         
*        VALIDATE SUPPLIER                                            *         
***********************************************************************         
                                                                                
VALSUP   GOTO1 AACSRCHC,DMCB,('STMPSTRQ',APOSUPH),ATWA,(C'*',SUPPUL),  X        
               ACOMFACS,0                                                       
         GOTO1 AFVAL,APOSUPH                                                    
         BZ    ERROR               SUPPLIER REQUIRED                            
                                                                                
         XC    SUPADDEL,SUPADDEL   CLEAR SUPPLIER DATA                          
         XC    LSUPP,LSUPP                                                      
                                                                                
         XC    FAXNUM,FAXNUM       CLEAR FAX NUMBER                             
                                                                                
         MVC   KEY,SPACES          BUILD KEY                                    
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),FLD+1                                                  
         CLI   FLD,C'*'                                                         
         BE    *+16                                                             
         MVC   KEY+1(2),SUPPUL                                                  
         MVC   KEY+3(12),FLD                                                    
                                                                                
         GOTO1 AREAD,AIOAREA2      READ SUPPLIER                                
         BNE   ERROR                                                            
                                                                                
         GOTO1 AGETNAME,AIOAREA2                                                
         MVC   LSUPNAME,WORK       SAVE NAME                                    
         MVC   LSUPP,FLD           SAVE SUPPLIER CODE                           
                                                                                
         GOTO1 AACSRCHC,DMCB,APOSUPH,ATWA,(C'*',SUPPUL),               X        
               (X'C0',SDSPNAMQ),AIOAREA2,(L'LSUPNAME,LSUPNAME)                  
                                                                                
         L     R1,AIOAREA2                                                      
         AH    R1,DATADISP                                                      
         SR    RF,RF                                                            
         MVI   FERN,INVPOST        CHECK FOR BALANCE ELEMENT                    
                                                                                
VSUP02   CLI   0(R1),0                                                          
         BE    ERROR                                                            
         CLI   0(R1),ABLELQ                                                     
         BE    *+14                                                             
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     VSUP02                                                           
                                                                                
         L     R1,AIOAREA2         SAVE ELEMENT DATA                            
         AH    R1,DATADISP                                                      
         SR    RF,RF                                                            
                                                                                
VSUP04   CLI   0(R1),0                                                          
         BE    VSUP16                                                           
         CLI   0(R1),ADRELQ        ADDRESS ELEMENT?                             
         BE    VSUP08                                                           
         CLI   0(R1),OADELQ        OVERRIDE ADDRESS?                            
         BE    VSUP08                                                           
         CLI   0(R1),RSTELQ        STATUS ELEMENT?                              
         BE    VSUP10                                                           
         CLI   0(R1),FFTELQ        FAX NUMBER, EMAIL ADDRESS?                   
         BE    VSUP12                                                           
                                                                                
VSUP06   IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     VSUP04                                                           
                                                                                
         USING ADRELD,R1                                                        
VSUP08   MVC   SUPADDEL,ADREL                                                   
         B     VSUP06                                                           
         DROP  R1                                                               
                                                                                
         USING RSTELD,R1                                                        
VSUP10   MVC   LSUPPSTA,RSTSTAT                                                 
         B     VSUP06                                                           
         DROP  R1                                                               
                                                                                
         USING FFTELD,R1                                                        
VSUP12   CLI   FFTTYPE,FFTTPFAX    FAX NUMBER?                                  
         BNE   VSUP14              NO                                           
         IC    RF,FFTDLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FAXNUM(0),FFTDATA   SAVE FAX NUMBER                              
         B     VSUP06                                                           
                                                                                
VSUP14   CLI   FFTTYPE,FFTTEML       EMAIL ADDRESS?                             
         BNE   VSUP06                 NO                                        
         IC    RF,FFTDLEN                                                       
         STC   RF,EMAILADL           SAVE EMAIL LENGTH                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   EMAILAD(0),FFTDATA    SAVE EMAIL ADDRESS                         
         B     VSUP06                                                           
         DROP  R1                                                               
                                                                                
VSUP16   TM    ACTINDS,NEW           ARE WE ADDING AN ORDER?                    
         BZ    VSUPX                  NO                                        
         TM    LSUPPSTA,X'20'         YES, IS SUPPLIER LOCKED?                  
         BZ    VSUPX                   NO                                       
         MVI   FERN,LOCKED             YES, PRINT ERROR                         
         B     ERROR                                                            
                                                                                
VSUPX    CLI   TYPE,PRODN                                                       
         BE    GOINIT              THAT IS ALL IF THIS IS PRODUCTION            
         B     VALEXP                                                           
         EJECT                                                                  
***********************************************************************         
*        VALIDATE EXPENSE                                             *         
***********************************************************************         
                                                                                
VALEXP   CLI   TYPE,EXPENSE                                                     
         BNE   VEXP02                                                           
         OI    APOCLIH+6,X'80'                                                  
         XC    APOPRO,APOPRO                                                    
         OI    APOPROH+6,X'80'                                                  
         XC    APOJOB,APOJOB                                                    
         OI    APOJOBH+6,X'80'                                                  
         XC    APODES,APODES                                                    
         OI    APODESH+6,X'80'                                                  
         XC    APOWRK,APOWRK                                                    
         OI    APOWRKH+6,X'80'                                                  
                                                                                
VEXP02   GOTO1 AFVAL,APOEXPH                                                    
         BZ    ERROR               REQUIRED                                     
                                                                                
         MVI   FERN,INVALID        ONLY CERTAIN U/LS ALLOWED                    
         CLC   =C'SE',FLD                                                       
         BE    VEXP04                                                           
         CLC   =C'GP',FLD                                                       
         BE    VEXP04                                                           
         CLC   =C'SQ',FLD                                                       
         BE    VEXP04                                                           
         CLC   =C'SH',FLD                                                       
         BE    VEXP04                                                           
         CLC   =C'SA',FLD                                                       
         BE    VEXP04                                                           
         CLC   =C'SB',FLD                                                       
         BE    VEXP04                                                           
         B     ERROR                                                            
                                                                                
VEXP04   GOTO1 AACSRCHC,DMCB,('STMPSTRQ',APOEXPH),ATWA,0,ACOMFACS,0             
         GOTO1 AFVAL,APOEXPH                                                    
         BZ    ERROR                                                            
                                                                                
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(14),FLD                                                    
         CLC   LEXP,FLD                                                         
         BE    VEXPX                                                            
         XC    LEXP,LEXP                                                        
                                                                                
         GOTO1 AREAD,AIOAREA2      READ EXPENSE ACCOUNT                         
         BNE   ERROR                                                            
         MVI   FERN,INVPOST                                                     
         L     RF,AIOAREA2                                                      
         AH    RF,DATADISP                                                      
                                                                                
VEXP06   CLI   0(RF),0             SEE IF IT IS A BOTTOM  LEVEL ACCT            
         BE    ERROR                                                            
         CLI   0(RF),X'32'         BY FINDING A BALANCE ELEMENT                 
         BE    VEXP08                                                           
         ZIC   RE,1(RF)                                                         
         AR    RF,RE                                                            
         B     VEXP06                                                           
                                                                                
VEXP08   L     RE,AIOAREA2                                                      
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
         MVI   FERN,LOCKED         CHECK FOR ACCOUNT NOT LOCKED                 
                                                                                
VEXP10   CLI   0(RE),0                                                          
         BE    ERROR                                                            
         CLI   0(RE),RSTELQ                                                     
         BE    *+14                                                             
         IC    RF,1(RE)                                                         
         AR    RE,RF                                                            
         B     VEXP10                                                           
                                                                                
         USING RSTELD,RE                                                        
         TM    RSTSTAT,RSTSACIL                                                 
         BO    ERROR                                                            
         DROP  RE                                                               
                                                                                
         GOTO1 AGETNAME,AIOAREA2                                                
         MVC   LEXPNAME,WORK                                                    
         MVC   LEXP,FLD                                                         
                                                                                
VEXPX    MVC   EXPAKEY,KEY                                                      
         GOTO1 AACSRCHC,DMCB,APOEXPH,ATWA,0,(X'C0',SDSPNAMQ),EXPAKEY,  X        
               (L'LEXPNAME,LEXPNAME)                                            
                                                                                
         BAS   RE,SETFAXPR         GET LEDGER LEVEL(SE) PROFILE FOR FAX         
         B     GOINIT                                                           
         EJECT                                                                  
***********************************************************************         
*        TIDY UP BEFORE GOING TO APPLICATION                          *         
***********************************************************************         
                                                                                
GOINIT   LA    R0,APOACTH          SOME ACTIONS STOP HERE - SO SET MSG          
         LA    R1,ENTERNXT         AND FADR AND EXIT                            
         CLI   ACTION,DISP         DISPLAY                                      
         BE    GINI01                                                           
         CLI   ACTION,DI2                                                       
         BNE   GINI02                                                           
                                                                                
GINI01   CLI   PRESFLAG,C'Y'       TEST FOR PRESTO ORDER                        
         BNE   *+8                                                              
         LA    R1,PRESORD                                                       
         B     GINI04                                                           
                                                                                
GINI02   TM    ACTINDS,TWOSTAGE    2-STAGE ONES (OPEN,CLOSE,CHANGE)             
         BZ    GOOVER                                                           
         LA    R0,APOWRKH                                                       
         LA    R1,NOWCHANG                                                      
         CLI   ACTION,CHA                                                       
         BE    GINI04                                                           
         LA    R0,AP2FRSTH                                                      
         CLI   ACTION,CH2                                                       
         BE    GINI04                                                           
         OI    APOACTH+6,X'81'                                                  
         LA    R0,APOTABH                                                       
         LA    R1,HITOOPEN                                                      
         CLI   ACTION,OPE                                                       
         BE    GINI04                                                           
         LA    R1,HITOCLOS                                                      
         CLI   ACTION,CLO                                                       
         BE    GINI04                                                           
                                                                                
         LA    R0,APOACTH                                                       
         LA    R1,HITODEL                                                       
         CLI   ACTION,DEL                                                       
         BE    GINI04                                                           
                                                                                
         LA    R1,HITORES                                                       
         CLI   ACTION,RES                                                       
         BE    GINI04                                                           
         B     OKEND                                                            
                                                                                
GINI04   MVC   MSG,SPACES                                                       
         MVC   MSG(17),=C'Order Displayed -'                                    
         MVC   MSG+18(22),0(R1)                                                 
         ST    R0,FADR                                                          
         B     OKEND                                                            
                                                                                
ENTERNXT DC    CL22'Enter Next Action     '                                     
PRESORD  DC    CL22'This is a PRESTO order'                                     
HITOOPEN DC    CL22'Hit Enter to Open     '                                     
HITOCLOS DC    CL22'Hit Enter to Close    '                                     
HITODEL  DC    CL22'Hit Enter to Delete   '                                     
HITORES  DC    CL22'Hit Enter to Restore  '                                     
NOWCHANG DC    CL22'Now Enter Changes     '                                     
         EJECT                                                                  
***********************************************************************         
*        HANDLE INTERFACE TO APPLICATION PROGRAMS                     *         
***********************************************************************         
                                                                                
GOOVER   MVC   PHASE,ACTOVER       OVERLAY APPLICATION PHASE                    
         GOTO1 VCALLOV,DMCB,(PHASE,0),0,0                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   APHASE,0(R1)                                                     
                                                                                
         MVI   FERN,X'FF'          GO TO APPLICATION WITH ACTMODE               
         LA    R0,APOACTH                                                       
         ST    R0,FADR                                                          
         GOTO1 APHASE                                                           
         CLI   FERN,X'FE'          SPECIAL ERROR MESSAGE?                       
         BE    OMSG                YES                                          
         CLI   FERN,X'FF'          CHECK FOR ERRORS                             
         BNE   ERROR                                                            
         CLI   ACTION,APR                                                       
         BNE   OKEND                                                            
         L     RE,=A(PRINTRN)                                                   
         A     RE,RELO                                                          
         MVC   ACTNVALS,0(RE)                                                   
         B     GOOVER                                                           
         EJECT                                                                  
***********************************************************************         
*        FIND PROFILE ELEMENT ON CLIENT, PRODUCT OR JOB RECORD        *         
*        RECORD IS ADDRESSED BY R1                                    *         
***********************************************************************         
                                                                                
GETPROFL NTR1                                                                   
         L     R7,0(R1)                                                         
         AH    R7,DATADISP                                                      
         XC    WORK(2),WORK                                                     
                                                                                
GPRO02   CLI   0(R7),0                                                          
         BE    EXIT                                                             
         CLI   0(R7),PPRELQ                                                     
         BE    GPRO04                                                           
         ZIC   RF,1(R7)                                                         
         AR    R7,RF                                                            
         B     GPRO02                                                           
                                                                                
         USING PPRELD,R7                                                        
GPRO04   MVC   WORK(2),PPRGAOFF                                                 
         B     EXIT                                                             
         DROP  R7                                                               
         EJECT                                                                  
***********************************************************************         
*        SAVE THE PROFILE VALUES WHICH ARE USED TO CONTROL THE        *         
*        CREATION OF FAX OR EMAIL                                     *         
***********************************************************************         
                                                                                
SETFAXPR NTR1                                                                   
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'AORD'                                                 
         NI    WORK,X'FF'-X'40'    LOWER CASE SYS ON-LINE                       
         MVC   WORK+4(1),COMPANY                                                
         MVC   WORK+12(2),TWAAGY                                                
         MVC   WORK+5(2),=C'SE'                                                 
         CLI   TYPE,EXPENSE                                                     
         BE    SFAX04                                                           
                                                                                
         MVC   WORK+5(2),=C'SJ'                                                 
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         TM    OFFACST4,X'01'      TEST NEW OFFICES                             
         BO    SFAX02              YES                                          
                                                                                
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),CLIUNIT  OFFICE CODE                                  
         B     SFAX04                                                           
                                                                                
SFAX02   MVI   WORK+10,C'+'        NEW OFFICES                                  
         MVC   WORK+14(2),CLIUNIT                                               
                                                                                
SFAX04   GOTO1 VGETPROF,DMCB,WORK,PROFWORK,VDATAMGR                             
         MVC   PRAUTOFX,PROFWORK+10                                             
         MVC   PRFXNPRT,PROFWORK+11                                             
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        READ JOB AND GET JOB VALUES                                  *         
*        CALLED FROM VALORD                                           *         
***********************************************************************         
                                                                                
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
                                                                                
         GOTO1 AREAD,AIOAREA2                                                   
         BE    *+6                                                              
         DC    H'0'                DUMP IF CANNOT FIND IT                       
         BAS   RE,RDOPT            GET JOB OPTIONS                              
         BAS   RE,GETTAB                                                        
         GOTO1 =A(LOOKUP),RR=RELO                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        READ THE OPTIONS FOR THE JOB                                 *         
*        CALLED FROM VALJOB AND GETJOB                                *         
***********************************************************************         
                                                                                
RDOPT    NTR1  ,                                                                
         L     R6,AGOBLOCK                                                      
         USING GOBLOCKD,R6                                                      
         MVC   GOADM,VDATAMGR                                                   
         MVC   GOSELCUL(1),COMPANY                                              
         MVC   GOSELCUL+1(2),PRODUL PRODUCTION UNIT/LEDGER                      
                                                                                
         MVC   GOSELCLI,LCLI       SPACE PADDED CLIENT CODE                     
         MVC   GOSELPRO,LPRO                                                    
         MVC   GOSELJOB,LJOB                                                    
         MVI   GOWHICH,0           BOTH OLD AND NEW OPTIONS                     
         GOTO1 VGETOPT,DMCB,GOBLOCK                                             
                                                                                
RDOPTX   B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*        LOAD IN JOBBER TABLES AND TO SET THEIR ADDRESSES             *         
*        CALLED FROM VALJOB AND GETJOB                                *         
***********************************************************************         
                                                                                
GETTAB   ST    RE,FULL                                                          
         GOTO1 VCALLOV,DMCB,(X'50',0),0,0                                       
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,0(R1)            GET A(TABLES)                                
                                                                                
         LM    R0,R1,0(RF)         GET DISP TO/LENGTH OF COLUMN TABLE           
         AR    R0,RF                                                            
         STM   R0,R1,ACOLTAB                                                    
                                                                                
         LM    R0,R1,8(RF)                                                      
         AR    R0,RF                                                            
         STM   R0,R1,AOPVTAB                                                    
                                                                                
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        FORMAT OUTPUT MESSAGE/EXTRA MESSAGE INTO MSG & EXIT          *         
***********************************************************************         
                                                                                
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
                                                                                
OKEND    MVC   LORDNUM,ORDNUMB     SAVE LAST ORDER NUMBER                       
         MVC   LACTION,ACTION                                                   
                                                                                
OMSG     MVC   APOMSG,MSG          MOVE MESSAGE TO TWA & TRANSMIT               
         OI    APOMSGH+6,X'80'                                                  
         L     R1,FADR                                                          
         OI    6(R1),X'40'         SET CURSOR TO FIELD                          
         B     EXIT                                                             
                                                                                
OKXIT    SR    RB,RB                                                            
ERRXIT   LTR   RB,RB                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*        STANDARDIZED TEXT GET ROUTINE                                *         
***********************************************************************         
                                                                                
         USING GETTXTD,R2                                                       
TXTGET   NTR1                                                                   
         LA    R2,DMCB1                                                         
         LR    R3,R1                                                            
         XC    GTBLOCK,GTBLOCK     CLEAR WORK BLOCK                             
         MVC   GTMSGNO,2(R3)       MESSAGE NUMBER                               
         MVI   GTMTYP,GTMERR       ERROR TYPE (DEFAULT)                         
         CLI   0(R3),C' '          ANY MESSAGE TYPE ?                           
         BNH   *+8                 NO, SKIP                                     
         MVC   GTMTYP,0(R3)        GET MESSAGE TYPE                             
         MVI   GTMSYS,6            ACCOUNT SYSTEM                               
         CLI   8(R3),0             ANY EXTRA OUTPUT AREA ?                      
         BE    TXTG02              NO, SKIP                                     
         MVC   GTLTXT,8(R3)        LENGTH OF EXTRA OUTPUT                       
         MVC   GTATXT,9(R3)        A(EXTRA OUTPUT DATA)                         
                                                                                
TXTG02   CLI   12(R3),0            ANY OVERRIDE MESSAGE LANGUAGE ?              
         BE    TXTG04              NO, SKIP                                     
         MVC   GTMLANG,12(R3)      GET OVERRIDE MESSAGE LANGUAGE                
                                                                                
TXTG04   OC    13(3,R3),13(R3)     ANY SUBSTITUTION TABLE ?                     
         BZ    TXTG06              NO, SKIP                                     
         MVC   GTASUBST,13(R3)     YES, SUBSTITUTION TABLE                      
                                                                                
TXTG06   GOTO1 VGETTXT,GETTXTD     GET THE MESSAGE TEXT                         
         DROP  R1                                                               
                                                                                
         L     R1,FADR                                                          
         OI    6(R1),X'40'         SET CURSOR TO FIELD                          
                                                                                
TXTGEX   B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*        EXTRACT AND PRE-VALIDATE AN INPUT FIELD                      *         
*                                                                     *         
* ADDRESS OF FIELD HEADER IS PASSED IN R1. RETURN WITH:-              *         
*                                                                     *         
*              FADR     = A(INPUT FIELD HEADER)                       *         
*              FERN     = MISSING INPUT FIELD IF NO INPUT             *         
*              FNDX     = ZERO                                        *         
*              FLDH     = INPUT FIELD HEADER (FLDH(4) = BINARY VALUE  *         
*                                             FOR NUMERIC FIELD)      *         
*              FLD      = EXTRACTED & SPACE FILLED INPUT FIELD        *         
*                                                                     *         
* RETURN WITH CC=EQU IF NO INPUT IN FIELD                             *         
***********************************************************************         
                                                                                
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
***********************************************************************         
*        EXTRACT NAME FROM A RECORD INTO WORK                         *         
*        RECORD IS ADDRESSED BY R1                                    *         
***********************************************************************         
                                                                                
GETNAME  NTR1  BASE=ABASE1                                                      
         L     RA,ABASE2                                                        
         L     R5,ABASE3                                                        
         L     R1,0(R1)                                                         
         ST    R1,AIOAREA                                                       
         MVC   WORK,SPACES                                                      
         AH    R1,DATADISP                                                      
         SR    RF,RF                                                            
                                                                                
GNAM02   CLI   0(R1),0                                                          
         BE    GNAMX                                                            
         CLI   0(R1),X'20'                                                      
         BE    *+14                                                             
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     GNAM02                                                           
         IC    RF,1(R1)                                                         
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     GNAMX                                                            
         MVC   WORK(0),2(R1)                                                    
                                                                                
GNAMX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*        DISPLAY ORDER NARRATIVE                                      *         
*        RECORD IS ADDRESSED BY R1                                    *         
*        FLAG = NULL(NORMAL NARRATIVE) OR FOOTLINE                    *         
***********************************************************************         
                                                                                
NARRDIS  NTR1  BASE=ABASE1                                                      
         L     RA,ABASE2                                                        
         L     R5,ABASE3                                                        
         L     R7,0(R1)                                                         
         AH    R7,DATADISP                                                      
                                                                                
         LA    R3,AP2FRSTH         POINT TO WHERE TO START                      
         CLI   FLAG,FOOTLINE                                                    
         BNE   NDIS02                                                           
         LA    R3,AP2FOOTH                                                      
                                                                                
NDIS02   SR    R0,R0               R0 = COUNT OF UNSCAN ENTRIES PENDING         
                                                                                
NDIS04   CLI   0(R7),0             SEARCH FOR COMMENT ELEMENTS                  
         BE    NDIS14                                                           
         CLI   0(R7),X'3E'                                                      
         BE    NDIS08                                                           
                                                                                
NDIS06   ZIC   R1,1(R7)                                                         
         AR    R7,R1                                                            
         B     NDIS04                                                           
                                                                                
         USING SCMELD,R7                                                        
NDIS08   MVI   FLAG1,FOOTLINE      CHECK FOR FOOTLINE REQUIRED                  
         NC    FLAG1,SCMTYPE                                                    
         CLC   FLAG1,FLAG                                                       
         BNE   NDIS06                                                           
         TM    SCMTYPE,SCMTPRAD                                                 
         BO    NDIS10                                                           
         LTR   R0,R0               ANY N=123456 ENTRIES PENDING                 
         BZ    *+12                                                             
         BAS   RE,NARREQ           IF SO DISPLAY THEM AS A STRING               
         BNZ   EXIT                (NO MORE ROOM)                               
         ZIC   R4,SCMLN            THEN DISPLAY STANDARD COMMENT                
         SH    R4,=H'5'                                                         
         MVC   CRD,SPACES                                                       
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   CRD(0),SCMNARR                                                   
         BAS   RE,NARRTWA          MOVE TWA IF DIFFERENT & BUMP TWA             
         BNZ   EXIT                                                             
         B     NDIS06                                                           
                                                                                
NDIS10   LTR   R0,R0               NON-STANDARD COMMENT - ADD TO UNSCAN         
         BNZ   NDIS12              BLOCK                                        
                                                                                
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         ICM   R1,8,SPACES                                                      
         LA    RE,TEMP                                                          
         LHI   RF,L'TEMP           CLEAR BUILD AREA                             
         MVCL  RE,R0                                                            
                                                                                
NDIS12   LA    RF,20                                                            
         MR    RE,R0                                                            
         LA    RF,TEMP(RF)                                                      
         MVI   0(RF),C'N'                                                       
         AH    R0,=H'1'            BUMP ENTRY COUNT                             
         LA    RE,SCMNARR          MOVE IN COMMENT NUM LEFT-ALIGNED             
         LA    R1,5                                                             
         CLI   0(RE),C' '                                                       
         BH    *+12                                                             
         LA    RE,1(RE)                                                         
         BCT   R1,*-12                                                          
         EX    R1,*+8                                                           
         B     NDIS06                                                           
         MVC   10(0,RF),0(RE)                                                   
                                                                                
NDIS14   LTR   R0,R0               AT END                                       
         BZ    *+12                                                             
         BAS   RE,NARREQ           DISPLAY ANY N=123456 ENTRIES PENDING         
         BNZ   EXIT                                                             
         MVC   CRD,SPACES          AND CLEAR REMAINING LINES                    
         BAS   RE,NARRTWA                                                       
         BZ    *-4                                                              
         B     EXIT                                                             
         DROP  R7                                                               
                                                                                
***********************************************************************         
*        UNSCAN ENTRIES                                               *         
***********************************************************************         
                                                                                
NARREQ   NTR1                      ROUTINE TO UNSCAN (R0), ENTRIES FROM         
         L     RF,VUNSCAN          TEMP INTO TWA FIELD(S)-R3=A(1ST HDR)         
         GOTO1 ,DMCB,((R0),TEMP),CRDH                                           
         SR    R0,R0                                                            
                                                                                
NREQ02   MVC   CRDH(1),0(R3)                                                    
         MVC   CRD,SPACES                                                       
         L     RF,VUNSCAN                                                       
         BASR  RE,RF                                                            
         BAS   RE,NARRTWA                                                       
         BNZ   EXIT                                                             
         CLI   0(R1),0             ANY MORE TO DISPLAY                          
         BNE   NREQ02                                                           
         XIT1  REGS=(R0,R3)        PASS BACK A(NEXT HDR)                        
                                                                                
NARRTWA  ZIC   R4,0(R3)            ROUTINE TO MOVE CONTENTS OF CRD TO           
         SH    R4,=H'9'            FLD AT 8(R3) IF DIFFERENT, TO                
         EX    R4,NARROC           TRANSMIT,AND BUMP R3 TO NEXT UNPROT          
         EX    R4,NARRCLC          HDR                                          
         BE    *+12                CC = NEQ IF WE REACH END OF NARRATVE         
         EX    R4,NARRMVC               AREA                                    
         OI    6(R3),X'80'                                                      
         MVI   5(R3),X'3C'                                                      
                                                                                
NTWA02   IC    R4,0(R3)                                                         
         AR    R3,R4                                                            
         LA    RF,AP2FINLH                                                      
         CLI   FLAG,FOOTLINE                                                    
         BNE   *+8                                                              
         LA    RF,AP2FOOTH                                                      
                                                                                
         CR    R3,RF                                                            
         BNH   *+8                                                              
         LTR   RB,RB                                                            
         BR    RE                                                               
         TM    1(R3),X'20'                                                      
         BZR   RE                                                               
         B     NTWA02                                                           
                                                                                
NDISX    DS    0H                                                               
                                                                                
NARROC   OC    8(0,R3),SPACES                                                   
NARRCLC  CLC   8(0,R3),CRD                                                      
NARRMVC  MVC   8(0,R3),CRD                                                      
         EJECT                                                                  
***********************************************************************         
*       READ LWCTAB FOR WORKCODE DESCRIPTION AND RETURN IN WORK       *         
*       IF NOT FOUND, DO LOOKUP AND ADD TO TABLE                      *         
*       ADDRESS OF WORK CODE IS PASSED IN R1                          *         
*       RETURN WITH CC=NEQ & FERN SET IF WORKCODE NOT FOUND           *         
***********************************************************************         
                                                                                
GETWC    NTR1  BASE=ABASE1                                                      
         L     RA,ABASE2                                                        
         L     R5,ABASE3                                                        
         MVC   WORK(L'WRKDES),SPACES                                            
         LA    R2,LWCTAB           R2 = A(SAVED W/C TAB ENTRY)                  
         LA    R0,20                    CONTAINS CODE(CL2)/DESC(CL15)           
                                                                                
GETWC02  OC    0(2,R2),0(R2)                                                    
         BZ    GETWC04                                                          
         CLC   0(2,R2),0(R1)                                                    
         BE    GETWC12             WE HAVE W/C IN SAVE STORAGE                  
         LA    R2,L'LWCTAB(R2)                                                  
         BCT   R0,GETWC02                                                       
                                                                                
         USING WCORECD,R3                                                       
GETWC04  LA    R3,KEY                                                           
         MVC   WCOKEY,SPACES       BUILD KEY TO READ                            
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,COMPANY                                                  
         MVC   WCOKUNT(2),PRODUL                                                
         MVC   WCOKWRK,0(R1)                                                    
         GOTO1 AREAD,AIOAREA2                                                   
         BE    GETWC08                                                          
         TM    DMCB+8,X'10'                                                     
         BZ    ERRXIT                                                           
         DROP  R3                                                               
                                                                                
GETWC06  MVI   FERN,INVWC                                                       
         B     ERRXIT                                                           
                                                                                
GETWC08  L     R1,0(R1)            FIND ANALYSIS ELEMENT AND SAVE               
         AH    R1,DATADISP         CODE/DESC                                    
         SR    RF,RF                                                            
                                                                                
         USING WCOELD,R1                                                        
GETWC10  CLI   WCOEL,0                                                          
         BE    GETWC06                                                          
         CLI   WCOEL,WCOELQ                                                     
         BE    *+14                                                             
         IC    RF,1(R1)                                                         
         AR    R1,RF                                                            
         B     GETWC10                                                          
         MVC   0(L'LWCTAB,R2),WCOCODE                                           
         MVC   WORK(L'WCODESC),2(R2)     PASS DESC BACK IN WORK                 
         MVC   WORK+15(L'WCOSTAT),17(R2)    AS WELL AS STATUS                   
         SR    R0,R0               CC=EQU FOR OK                                
         B     EXIT                                                             
                                                                                
GETWC12  MVI   FERN,DUPWRK         DUPLICATE WORKCODE                           
         B     ERRXIT                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*        GET T0 THE NEXT LINE                                         *         
***********************************************************************         
                                                                                
NEXTWORK LHI   R1,4                NUMBER OF TIMES TO BUMP                      
                                                                                
BUMPNXT  ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         BCT   R1,BUMPNXT                                                       
         BR    RE                                                               
                                                                                
BUMP     ZIC   RF,0(R2)            BUMP TO NEXT HEADER                          
         AR    R2,RF                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
*        ACCOUNT FILE I/O                                             *         
*                                                                     *         
* I/O IS EXECUTED ON KEY INTO I/O AREA ADDRESSED BY R1. COMMAND IS    *         
* PASSED IN THE HIGH ORDER BYTE OF RF AS FOLLOWS:-                    *         
*                                                                     *         
*              BITS 0-3 = COMMAND NUMBER (1-6 SEE IOCMNDS)            *         
*                   4ON = READ/WRITE FROM DIRECTORY/USE BIGKEY        *         
*                   5ON = PASS BACK DELETED RECORDS                   *         
*                   6ON = READ KEY WITH LOCK                          *         
*                   7ON = SAVE KEY IN KEYSAVE BEFORE I/O              *         
*                                                                     *         
* RETURN WITH CC=NEQ ON I/O ERROR WITH FERN SET TO ERROR MESSAGE NUM. *         
***********************************************************************         
                                                                                
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
         BZ    AIO02                                                            
                                                                                
         LA    RE,KEY                                                           
         TM    DUB,X'08'           TEST DIRECTORY IN USE                        
         BZ    *+8                                                              
         LA    RE,BIGKEY                                                        
         MVC   KEYSAVE,0(RE)       SAVE KEY                                     
                                                                                
AIO02    CLC   DMUNLK,0(RF)        TEST COMMAND=DMUNLK                          
         BNE   AIO04                                                            
         CLI   EMULATE,C'Y'        TEST EMULATING ACCOUNT FILE                  
         BNE   AIO04                                                            
                                                                                
         GOTO1 VDATAMGR,DMCB,,ACCDIR,KEY,AIOAREA                                
         BE    *+6                 UNLOCK THE DIRECTORY/THEN FILE               
         DC    H'0'                                                             
         GOTO1 (RF),(R1),,ACCMST,KEY,AIOAREA                                    
         BE    AIO08                                                            
         DC    H'0'                                                             
                                                                                
AIO04    TM    DUB,X'08'           TEST DIRECTORY BASED COMMAND                 
         BO    AIO06               YES                                          
         GOTO1 VDATAMGR,DMCB,,IOFILE,KEY,AIOAREA                                
         B     AIO08                                                            
                                                                                
AIO06    GOTO1 VDATAMGR,DMCB,,ACCDIR,BIGKEY,BIGKEY,0                            
         MVC   DISKADD,BIGKEY+(ACCKDA-ACCRECD) GET DISK ADDRESS                 
                                                                                
AIO08    MVI   FERN,OK             SET FIELD ERROR NUMBER                       
         CLI   DMCB+8,0                                                         
         BE    AIOX                                                             
         MVI   FERN,NOTFOUND                                                    
         TM    DMCB+8,X'10'        TEST N/F                                     
         BO    AIOX                                                             
         MVI   FERN,DELETED                                                     
         TM    DMCB+8,X'02'        TEST DELETED                                 
         BO    AIOX                                                             
         MVI   FERN,IOERROR        EOF/ERR/DUP/LOCKS                            
                                                                                
AIOX     CLI   FERN,OK             EXIT WITH CC=EQ IF I/O OK                    
         B     EXIT                                                             
         DROP  R5,RA,RB                                                         
         EJECT                                                                  
***********************************************************************         
*        LIST OF I/O COMMANDS/FILES                                   *         
***********************************************************************         
                                                                                
IOCMNDS  DS    0CL8                                                             
         DC    C'DMADD   '                                                      
         DC    C'DMRDHI  '                                                      
         DC    C'DMREAD  '                                                      
         DC    C'DMRSEQ  '                                                      
         DC    C'DMWRT   '                                                      
DMUNLK   DC    C'DMUNLK  '                                                      
IOFILE   DC    C'ACCFIL  '                                                      
                                                                                
ACCDIR   DC    C'ACCDIR  '                                                      
ACCMST   DC    C'ACCMST  '                                                      
                                                                                
RELO     DS    F                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        LOOK UP THE JOB'S ORIGINAL AND CURRENT ESTIMATES             *         
*        CALLED FROM VALJOB AND GETJOB                                *         
***********************************************************************         
                                                                                
LOOKUP   NMOD1 0,**LOOK**                                                       
         GOTO1 VJOBCOL,DMCB,LOOKFLDH,ACOLIST,ACOMFACS                           
         CLI   4(R1),0             TEST FOR ERROR                               
         BNE   *+6                                                              
         DC    H'0'                                                             
                                                                                
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
         GOTO1 VJOBBER,DMCB,AJOBLOCK                                            
         CLI   JBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         XMOD1 1                                                                
         DROP  R6                                                               
                                                                                
LOOKFLDH DC    AL1(L'LOOKFLD+8),4X'00',AL1(L'LOOKFLD),2X'00'                    
LOOKFLD  DC    C'OE,CE'                                                         
         EJECT                                                                  
***********************************************************************         
*        TABLE OF ADCONS FOR EXTENDED WORKING STORAGE                 *         
*               BYTE 0-3 = DISPLACMENT TO STORAGE AREA                *         
*               BYTE 4-7 = DISPLACEMENT TO ADDRESSS OF STORAGE AREA   *         
***********************************************************************         
                                                                                
EXTTAB   DS    0D                                                               
         DC    AL4(GOBLOCKA-POPWORKD),AL4(AGOBLOCK-POPWORKD)                    
         DC    AL4(JOBLOCKA-POPWORKD),AL4(AJOBLOCK-POPWORKD)                    
         DC    AL4(COLIST-POPWORKD),AL4(ACOLIST-POPWORKD)                       
         DC    AL4(SAVE-POPWORKD),AL4(ASAVE-POPWORKD)                           
         DC    AL4(OFFBLK-POPWORKD),AL4(AOFFBLK-POPWORKD)                       
NEXTTAB  EQU   (*-EXTTAB)/L'EXTTAB                                              
                                                                                
***********************************************************************         
*        TABLE OF CORE-RESIDENT MODULES                               *         
***********************************************************************         
                                                                                
CORETAB  DS    0X                                                               
         DC    AL1(QCENTER)                                                     
         DC    AL1(QCHOPPER)                                                    
         DC    AL1(QEDITOR)                                                     
         DC    AL1(QGETOPT)                                                     
         DC    AL1(QJOBBER)                                                     
         DC    AL1(QQSORT)                                                      
         DC    AL1(QSQUASH)                                                     
         DC    AL1(QOFFAL)                                                      
NCORES   EQU   *-CORETAB                                                        
                                                                                
***********************************************************************         
*        TABLE OF A&V-TYPES FOR RELOCATING INTO GLOBAL W/S            *         
*              BYTE 0-3 = A/V-TYPE ADDRESS                            *         
*                   1-N = HIGH ORDER BYTE VALUES DELIMITED BY X'FF'   *         
***********************************************************************         
                                                                                
ROUTTAB  DS    0X                                                               
         DC    VL3(ACJOBCOL),X'00FF'                                            
         DC    VL3(ACSRCHC),X'00FF'                                             
         DC    AL3(ACTNTAB),X'00FF'                                             
         DC    AL3(OPTNTAB),X'00FF'                                             
         DC    AL3(FVAL),X'00FF'                                                
         DC    AL3(GETNAME),X'00FF'                                             
         DC    AL3(NARRDIS),X'00FF'                                             
         DC    AL3(GETWC),X'00FF'                                               
         DC    AL3(ACCIO),X'102527343640425060182D2F484A58FF'                   
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
*        TABLE OF INPUT ACTIONS                                       *         
*              BYTE 0-7 = ACTION NAME                                 *         
*                   8-9 = SHORT ACTION NAME                           *         
*                   10  = ACTION NUMBER                               *         
*                   11  = INDICATORS - BIT0ON=DDS-ONLY ACTION         *         
*                                      BIT1ON=ADDING ACTION           *         
*                                      BIT2ON=PRINTING ACTION         *         
*                                      BIT3ON=TWO-STAGE ACTION        *         
*                                      BIT4ON=HAS PARAMETERS          *         
*                   12  = OVERLAY PHASE NUMBER                        *         
*                   13  = SPARE                                       *         
***********************************************************************         
                                                                                
ACTNTAB  DS    0CL14                                                            
         DC    C'ADD     AD',AL1(ADD,NEW,1,0)                                   
         DC    C'ADDPRINTAP',AL1(APR,NEW+PRINT+PARMS_OK,1,0)                    
         DC    C'CHANGE  CH',AL1(CHA,TWOSTAGE,1,0)                              
         DC    C'CLOSE   CL',AL1(CLO,TWOSTAGE,2,0)                              
         DC    C'DELETE  DE',AL1(DEL,TWOSTAGE,2,0)                              
         DC    C'DISPLAY DI',AL1(DISP,READOK,0,0)                               
         DC    C'DIS2    D2',AL1(DI2,READOK,0,0)                                
         DC    C'EDIT    ED',AL1(EDIT,HASPARMS,1,0)                             
         DC    C'OPEN    OP',AL1(OPE,TWOSTAGE,2,0)                              
         DC    C'CHA2    C2',AL1(CH2,TWOSTAGE,1,0)                              
                                                                                
PRINTRN  DC    C'PRINT   PR',AL1(PRI,PRINT+PARMS_OK,3,0)                        
         DC    C'PNOCASH PN',AL1(PRI,PRINT,3,0)                                 
         DC    C'RESTORE RE',AL1(RES,TWOSTAGE,2,0)                              
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        TABLE OF INPUT OPTIONS                                       *         
***********************************************************************         
                                                                                
OPTNTAB  DS    0CL14                                                            
         DC    X'FF'                                                            
                                                                                
STMPSTRQ EQU   X'04'               TEMPSTR PAGE NO. FOR SEARCH                  
SDSPNAMQ EQU   15                  DISP. TO NAME ON SEARCH FIELDS               
         EJECT                                                                  
WRKTABD  DSECT                     DSECT FOR TEMP STORAGE                       
WRKWORK  DS    CL2                                                              
WRKAMT   DS    CL11                                                             
WRKSTAT  DS    C                                                                
WRKDES   DS    CL15                                                             
WRKTABL  EQU   *-WRKTABD                                                        
         EJECT                                                                  
       ++INCLUDE ACPOPDSECB                                                     
         EJECT                                                                  
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACPOP00B  05/18/06'                                      
         END                                                                    
