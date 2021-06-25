*          DATA SET TAGEN3A    AT LEVEL 018 AS OF 10/12/11                      
*PHASE T7023AC                                                                  
         TITLE 'T7023A - GUARANTEE CAST LIST'                                   
T7023A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7023A                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,PFTAB                                               
         SPACE 1                                                                
         MVC   SGCSHED(7),=C'Pid Num'                                           
         OI    SGCSHEDH+6,X'80'                                                 
         SPACE 1                                                                
GUA20    CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,LISTRECS                                                    
         BNE   GUA30                                                            
         LA    R2,LISTAR                                                        
         B     GUA40                                                            
         SPACE 1                                                                
GUA30    CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         BAS   RE,INIT             INSURE START AT TOP OF LIST                  
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,P+1                                                           
         SPACE 1                                                                
GUA40    BAS   RE,LREC             GO LIST THE RECORDS                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE KEY                                      
         SPACE 1                                                                
VKEY     NTR1                                                                   
         TM    SGCSSNH+4,X'20'     S/S NUMBER                                   
         BO    VK2                                                              
*                                                                               
         LA    R2,SGCSSNH                                                       
*                                                                               
         CLI   SGCSSNH+5,0                                                      
         BE    VK1A                                                             
         CLI   SGCSSNH+5,9         SSN ENTERED?                                 
         BE    VK1                                                              
         CLI   SGCSSNH+5,6         PID ENTERED?                                 
         BNE   FLDINV                                                           
         MVC   TGPID,SGCSSN                                                     
VK1A     OC    TGPID,TGPID                                                      
         BZ    FLDMISS                                                          
         GOTO1 SSNUNPK,DMCB,TGPID,TGSSN                                         
         BNE   VK1                                                              
         MVC   SGCSSN,TGSSN                                                     
         MVI   SGCSSNH+5,9                                                      
*                                                                               
VK1      GOTO1 RECVAL,DMCB,TLW4CDQ,(X'08',SGCSSNH),SGCSSNNH                     
         GOTO1 SSNPACK,DMCB,TGSSN,TGPID                                         
         MVC   SGCSSN,SPACES                                                    
         MVC   SGCSSN(L'TGPID),TGPID                                            
         MVI   SGCSSNH+5,L'TGPID                                                
         OI    SGCSSNH+6,X'80'                                                  
         SPACE 1                                                                
VK2      NI    SGCGUAH+4,X'DF'                                                  
         LA    R2,SGCGUAH          VALIDATE GUARANTEE CODE                      
         TM    4(R2),X'20'                                                      
         BO    VK8                                                              
         CLI   5(R2),0             TEST SOMETHING INPUT                         
         BNE   VK6                                                              
         OC    TGGUA,TGGUA         NO - SOMETHING IN GLOBAL                     
         BZ    VK4                                                              
         MVC   8(4,R2),TGGUA       YES - MOVE TO FIELD                          
         XC    8(4,R2),HEXFFS      AND UNCOMPLEMENT IT                          
         OI    6(R2),X'80'                                                      
         B     VK6                                                              
         SPACE 1                                                                
VK4      GOTO1 ANY                                                              
         SPACE 1                                                                
VK6      MVC   TGGUA,8(R2)         MOVE CODE TO GLOBAL                          
         XC    TGGUA,HEXFFS        AND COMPLEMENT                               
         GOTO1 RECVAL,DMCB,TLGUCDQ,(X'20',0)                                    
         BNE   THEEND                                                           
         BAS   RE,GCHKACC                                                       
         NI    SGCOPTSH+4,X'DF'                                                 
         SPACE 1                                                                
VK8      LA    R2,SGCOPTSH         OPTIONS                                      
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         OI    4(R2),X'20'         VALIDATED                                    
         SPACE 1                                                                
         BAS   RE,INIT             RE-INITIALIZE LIST                           
         SPACE 1                                                                
VKX      B     XIT                                                              
         EJECT                                                                  
*              INITIALIZATION ROUTINES                                          
         SPACE 1                                                                
INIT     NTR1                                                                   
         XC    KEY,KEY             INITIALIZE KEY                               
         LH    RF,=AL2(TIEND-TASYSIOD)                                          
         XCEFL TASYSIOD,(RF)                                                    
         MVC   TIUSERID,TWAORIG    SET UP SOME FIELDS FOR SYSIO                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVI   TIREAD,TLCAGCDQ     SET RECORD TYPE FOR READS                    
         MVC   TIFSSN,TGSSN                                                     
         MVC   TIFGUA,TGGUA                                                     
         XC    TIFGUA,HEXFFS       UNCOMPLEMENT                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS RECORD LISTING                                  
         SPACE 1                                                                
LREC     NTR1                                                                   
         TWAXC SGCSELH,SGCLSTH,PROT=Y  CLEAR SCREEN                             
         SPACE 1                                                                
         LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIKHOOK,SETLSTK                                                  
         SPACE 1                                                                
         MVI   NLISTS,9            IN ORDER TO GET CONTROL                      
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,8            BACK AFTER 1 FULL PAGE                       
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         CP    COUNTER,=P'0'       IF ANYTHING REPORTED                         
         BE    LRX                                                              
         EDIT  COUNTER,(5,P+1),COMMAS=YES,ALIGN=LEFT                            
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(12,R1),=C'CAST RECORDS'                                        
         GOTO1 SPOOL,DMCB,(R8)                                                  
         SPACE 1                                                                
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         SPACE 1                                                                
LRX      B     XIT                                                              
         EJECT                                                                  
*               PROCESS SYSIO RECORDS                                           
         SPACE 1                                                                
         USING LINED,R2            R2=A(OUTPUT AREA)                            
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC                                                   
         BNE   XIT                                                              
         CLI   MODE,LISTRECS       IF LISTING ON SCREEN                         
         BNE   *+12                                                             
         CLI   LISTNUM,8           AND ALREADY COMPLETED THIS PAGE              
         BE    ENDPAGE             GIVE MESSAGE - MORE TO COME                  
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         SPACE 1                                                                
         MVC   TGCOM,TICOM         SET INT. COMMERCIAL NO. IN GLOBAL            
         GOTO1 XNAME,DMCB,TLCOCCDQ,LINCID,TIKEY  GET COMMERCIAL ID              
         SPACE 1                                                                
*        BAS   RE,CCHKACC                                                       
*        BNE   XIT                                                              
         SPACE 1                                                                
         GOTO1 CHAROUT,DMCB,TANAELQ,0            GET TITLE (REC IN AIO)         
         MVC   LINTITL,TGNAME      COMMERCIAL TITLE                             
         SPACE 1                                                                
         L     R3,AIO              R3=A(COMMERCIAL RECORD)                      
         USING TLCOD,R3                                                         
         MVC   LINAGY,TLCOAGY      DISPLAY AGENCY                               
         MVC   TGAGY,TLCOAGY       SET GLOBAL AGENCY                            
         MVC   TGCLI,TLCOCLI                  CLIENT                            
         MVC   TGPRD,TLCOPRD                  PRODUCT                           
         SPACE 1                                                                
         MVC   LINCAT,TICAT        CATEGORY                                     
         MVC   LINONOF,TIONOF      ON/OFF CAMERA                                
         SPACE 1                                                                
         L     R3,TIAREC           GET CAST DETAILS EL.                         
         ST    R3,AIO                                                           
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R3                                                         
         GOTO1 SETOV2,DMCB,(R3),TIAREC,SPACES                                   
         MVC   LINDBL,TACADBL      DOUBLES                                      
         SPACE 1                                                                
         GOTO1 GETOV1,DMCB,SPACES,FULL  LOOK UP GLOBAL OVERSCALE RATE 1         
         CLI   0(R1),X'FF'                                                      
         BE    LRH8                SKIP IF IT'S AN AMOUNT                       
         L     R1,FULL             OVERSCALE RATE 1                             
         XR    R0,R0                                                            
         LA    RF,LINOV1                                                        
         TM    FULL,X'80'          PERCENT SCALE USED?                          
         BZ    LRH5                                                             
         MVI   LINOV1,C'%'                                                      
         N     R1,=X'7FFFFFFF'     STRIP OFF HOB                                
         LA    RF,1(RF)                                                         
LRH5     D     R0,=F'100'                                                       
         TM    FULL,X'80'                                                       
         BZ    LRH7                                                             
         EDIT  (R1),(2,(RF)),ZERO=BLANK                                         
         B     LRH8                                                             
LRH7     EDIT  (R1),(3,LINOV1),ZERO=BLANK                                       
*                                                                               
LRH8     ICM   R1,15,TACAOV2       OVERSCALE RATE 2                             
         XR    R0,R0                                                            
         D     R0,=F'100'                                                       
         EDIT  (R1),(3,LINOV2),ZERO=BLANK                                       
         MVC   AIO,AIO1            RESTORE DEFAULT I/O AREA                     
         SPACE 1                                                                
         MVC   LINUNI,TIUN         UNION                                        
         MVC   LINYR,TIYEAR        YEAR                                         
         MVC   LINAGNT,TIAGT       AGENT                                        
         SPACE 1                                                                
         CLI   MODE,PRINTREP       IF WE'RE PRINTING                            
         BNE   *+12                                                             
         LA    R2,132(R2)          BUMP TO NEXT PRINT LINE                      
         B     LRH10                                                            
         SPACE 1                                                                
         L     R2,ATHISLST         ELSE BUMP TO NEXT LINE                       
         BAS   RE,BUMP2                                                         
         LA    R2,8(R2)            SKIP PAST HEADER                             
         SPACE 1                                                                
LRH10    MVC   LINCLI,TGCLI        CLIENT                                       
         MVC   AIO,AIO2            USING ALT. I/O AREA                          
         GOTO1 XNAME,DMCB,TLCLCDQ,LINCLIN,TIKEY  GET CLIENT NAME                
         SPACE 1                                                                
         MVC   LINPRD,TGPRD        PRODUCT                                      
         OC    TGPRD,TGPRD         IF PRODUCT DEFINED                           
         BZ    LRH20                                                            
         GOTO1 XNAME,DMCB,TLPRCDQ,LINPRDN,TIKEY  GET PRODUCT NAME               
         B     LRH24                                                            
         SPACE 1                                                                
LRH20    MVC   AIO,AIO1            ELSE LOOK FOR PRODUCT NAME IN COMML          
         GOTO1 CHAROUT,DMCB,TAFNELQ,0,TAFNTPRD                                  
         MVC   LINPRDN,TGNAME                                                   
         SPACE 1                                                                
LRH24    CLI   MODE,PRINTREP                                                    
         BNE   LRH30                                                            
         GOTO1 SPOOL,DMCB,(R8)     PRINT REPORT                                 
         AP    COUNTER,=P'1'       INCREMENT COUNTER                            
         B     LRHX                                                             
         SPACE 1                                                                
LRH30    MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         GOTO1 LISTMON             CALL LISTMON                                 
         SH    R2,=H'8'            BUMP BACK TO FIELD HEADER                    
         BAS   RE,BUMP2            BUMP TO NEXT                                 
         ST    R2,ATHISLST         THIS IS A(NEXT)                              
         SPACE 1                                                                
LRHX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS IF STAFF HAS ACCESS TO THIS GUARANTEE         *         
*        ON ENTRY ... AIO=(GUARANTEE RECORD)                          *         
***********************************************************************         
                                                                                
GCHKACC  NTR1                                                                   
         USING TAGUD,R3                                                         
         L     R3,AIO            R3=A(GUARANTEE RECORD)                         
         MVI   ELCODE,TAGUELQ    GET GUARANTEE DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         OC    TAGUAGY(L'TAGUAGY+L'TAGUCLI),TAGUAGY                             
         JNZ   GCACC10                                                          
         TM    TGCTSTST,TGCTSCLI                                                
         JO    ERRNFND                                                          
*                                                                               
GCACC10  MVC   SAVEKEY,KEY       SAVE GUARANTEE KEY                             
         MVC   AIO,AIO2          AND PREPARE TO USE AIO2                        
         XR    R0,R0                                                            
*                                                                               
         OC    TAGUAGY,TAGUAGY   IF AGENCY DEFINED                              
         BZ    GCACC20           TEST ACCESS                                    
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'80',TAGUAGY)                              
         BE    GCACC20                                                          
         LHI   R0,1                                                             
*                                                                               
GCACC20  OC    TAGUCLI,TAGUCLI   IF CLIENT DEFINED                              
         BZ    GCACC30           TEST ACCESS                                    
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'80',TAGUCLI)                              
         BE    GCACC30                                                          
         LHI   R0,1                                                             
*                                                                               
GCACC30  LTR   R0,R0             IF USING NEW GUARANTEE SYSTEM                  
         BZ    GCACC60           AND AGY/CLI MATCH NOT YET FOUND                
*                                                                               
         USING TAVAD,R3                                                         
         L     R3,AIO1           R3=A(GUARANTEE RECORD)                         
         MVI   ELCODE,TAVAELQ    TRY TO GET NEW SYSTEM AGENCY/CLIENT            
         BAS   RE,GETEL          ELEMENT                                        
         B     *+8                                                              
GCACC40  BAS   RE,NEXTEL                                                        
         BNE   THEEND                                                           
*                                                                               
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'80',TAVAAGY)                              
         BNE   GCACC40                                                          
*                                                                               
         CLI   TAVALEN,TAVALNQ   IF NO CLIENT LIMITS ARE DEFINED                
         BE    GCACC60           ACCESS IS GRANTED                              
*                                                                               
         ZIC   R2,TAVALEN                                                       
         SHI   R2,TAVALNQ                                                       
         LA    R3,TAVACLI                                                       
GCACC50  GOTO1 RECVAL,DMCB,TLCLCDQ,(X'80',(R3))                                 
         BE    GCACC60                                                          
         LA    R3,L'TAVACLI(R3)                                                 
         SHI   R2,L'TAVACLI                                                     
         LTR   R2,R2                                                            
         JNZ   GCACC50                                                          
         B     GCACC40                                                          
         DROP  R3                                                               
*                                                                               
GCACC60  MVC   KEY,SAVEKEY         RESET KEY                                    
         MVC   AIO,AIO1            AND RESTORE I/O AREA                         
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*        ROUTINE CHECKS IF STAFF HAS ACCESS TO THIS COMMERCIAL        *         
*        ON ENTRY ... AIO=(COMMERCIAL RECORD)                         *         
***********************************************************************         
*&&DO                                                                           
CCHKACC  NTR1                                                                   
         USING TLCOD,R3                                                         
         L     R3,AIO              R3=A(COMMERCIAL RECORD)                      
*                                                                               
         LHI   R2,1                                                             
*                                                                               
         USING FAWSSVRD,R1                                                      
CCACC20  LA    R1,LIMBLK                                                        
         MVC   FAWSTOKN(3),=C'STF'                                              
         STC   R2,FAWSTOKN+3                                                    
         MVI   FAWSACTN,FAWSARST   RECALL STAFF2 INFORMATION VIA WWSVR          
         XC    FAWSLEN,FAWSLEN                                                  
         MVC   FAWSADR,TGAS2ACC                                                 
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0           IF NOT FOUND, STAFF HAS NO ACCESS            
         BNE   NO                                                               
         DROP  R1                                                               
*                                                                               
         AHI   R2,1                                                             
*                                                                               
         USING TAVAD,R1                                                         
         L     R1,TGAS2ACC                                                      
         OC    TAVAAGY,TAVAAGY     IF STAFF HAS NO AGENCY LIMITS,               
         BZ    YES                 STAFF HAS ACCESS TO ALL RECORDS              
*                                                                               
CCACC30  CLI   0(R1),0             RECALL NEXT RECORD FROM WSSVR                
         BE    CCACC20                                                          
*                                                                               
         CLC   TLCOAGY,TAVAAGY     IF AGENCY IS FOUND IN STAFF LIMITS           
         BNE   CCACC50                                                          
*                                                                               
         CLI   TAVALEN,TAVALNQ     IF NO CLIENT LIMITS ARE DEFINED              
         BE    YES                 ACCESS IS GRANTED                            
*                                                                               
         ZIC   RE,TAVALEN                                                       
         SHI   RE,TAVALNQ                                                       
         LA    RF,TAVACLI                                                       
CCACC40  CLC   TLCOCLI,0(RF)       IF CLIENT IS FOUND IN STAFF LIMITS           
         BE    YES                 ACCESS IS GRANTED                            
         LA    RF,L'TAVACLI(RF)                                                 
         SHI   RE,L'TAVACLI                                                     
         LTR   RE,RE                                                            
         BNZ   CCACC40                                                          
*                                                                               
CCACC50  ZIC   RE,TAVALEN          BUMP TO NEXT VALID AGENCY/CLIENT             
         AR    R1,RE               ELEMENT                                      
         B     CCACC30                                                          
         DROP  R1,R3                                                            
*&&                                                                             
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 2                                                                
ENDPAGE  MVC   MYMSGNO1,OKNO       MSG - HIT ENTER FOR NEXT                     
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SGCSELH                                                       
         B     THEEND                                                           
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
ERRNFND  MVI   ERROR,NOTFOUND      NOT FOUND                                    
         B     THEEND                                                           
         SPACE 1                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
BUMP2    ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
         SPACE 3                                                                
         GETEL R3,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTAB    DS    0C                                                               
         DC    AL1(PF10X-*,10,0,(PF10X-PF10)/KEYLNQ,0)                          
         DC    CL3'CA',CL8'CAST    ',CL8'LIST    '                              
PF10     DC    AL1(KEYTYCUR,L'LINAGY-1),AL2(LINAGY-LINED)                       
         DC    AL1(KEYTYCUR,L'LINCID-1),AL2(LINCID-LINED)                       
PF10X    EQU   *                                                                
         DC    AL1(PF11X-*,11,0,(PF11X-PF11)/KEYLNQ,0)                          
         DC    CL3'HI',CL8'HISTORY ',CL8'LIST    '                              
PF11     DC    AL1(KEYTYCUR,L'LINAGY-1),AL2(LINAGY-LINED)                       
         DC    AL1(KEYTYCUR,L'LINCID-1),AL2(LINCID-LINED)                       
PF11X    EQU   *                                                                
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3'  ',CL8'GRT     ',CL8'DISPLAY '                              
PF13     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYGLB,L'TGGUA-1),AL2(TGGUA-TGD)                           
PF13X    EQU   *                                                                
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3'  ',CL8'GRT     ',CL8'LIST    '                              
PF14     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
PF14X    EQU   *                                                                
         DC    AL1(PF15X-*,15,0,(PF15X-PF15)/KEYLNQ,0)                          
         DC    CL3'  ',CL8'GTRACK  ',CL8'REPORT  '                              
PF15     DC    AL1(KEYTYGLB,L'TGSSN-1),AL2(TGSSN-TGD)                           
         DC    AL1(KEYTYGLB,L'TGGUA-1),AL2(TGGUA-TGD)                           
PF15X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 1                                                                
HEXFFS   DC    4X'FF'                                                           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REPORT                                                     
         SSPEC H1,73,PAGE                                                       
         SSPEC H2,56,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H1,30,C'GUARANTEE CAST LIST'                                     
         SSPEC H2,30,C'-------------------'                                     
         SPACE 1                                                                
         SSPEC H4,2,C'AGENCY COMML-ID/CLI TITLE'                                
         SSPEC H5,2,C'------ ------------ -----'                                
         SSPEC H4,39,C'CAT/PRD CAM DBL OV1 OV2 UNI YR  AGNT'                    
         SSPEC H5,39,C'------- --- --- --- --- --- --- ----'                    
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER LIST LINE                                         
         SPACE 2                                                                
LINED    DSECT                                                                  
LINAGY   DS    CL6                 AGENCY                                       
         DS    CL1                                                              
LINCID   DS    CL12                COMMERCIAL ID                                
         DS    CL1                                                              
LINTITL  DS    CL16                COMMERCIAL TITLE                             
         DS    CL1                                                              
LINCAT   DS    CL3                 CATEGORY                                     
         DS    CL5                                                              
LINONOF  DS    CL3                 ON/OFF CAMERA                                
         DS    CL1                                                              
LINDBL   DS    CL3                 DOUBLES                                      
         DS    CL1                                                              
LINOV1   DS    CL3                 OVERSCALE RATE 1                             
         DS    CL1                                                              
LINOV2   DS    CL3                 OVERSCALE RATE 2                             
         DS    CL1                                                              
LINUNI   DS    CL3                 UNION                                        
         DS    CL1                                                              
LINYR    DS    CL3                 CONTRACT YEAR                                
         DS    CL1                                                              
LINAGNT  DS    CL4                 AGENT                                        
*                                                                               
         ORG   LINED               2ND LINE                                     
         DS    CL7                                                              
LINCLI   DS    CL6                 CLIENT                                       
         DS    CL7                                                              
LINCLIN  DS    CL16                CLIENT NAME                                  
         DS    CL1                                                              
LINPRD   DS    CL6                 PRODUCT                                      
         DS    CL2                                                              
LINPRDN  DS    CL16                PRODUCT NAME                                 
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR3AD                                                       
         SPACE 3                                                                
COUNTER  DS    PL4                 RECORD COUNTER                               
SAVEKEY  DS    CL(L'KEY)           SAVED KEY                                    
LIMBLK   DS    XL100               AGENCY/CLIENT LIMIT WORK BLOCK               
         EJECT                                                                  
* TASYSIOD      (MUST FOLLOW LAST SCREEN)                                       
* DDGENTWA      (MUST FOLLOW LAST SCREEN)                                       
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAWSSVRD                                                                      
         PRINT OFF                                                              
       ++INCLUDE TASYSIOD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAWSSVRD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018TAGEN3A   10/12/11'                                      
         END                                                                    
