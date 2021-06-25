*          DATA SET SPSFM5F    AT LEVEL 042 AS OF 12/05/13                      
*PHASE T2175FA                                                                  
*                                                                               
***********************************************************************         
* AKAT 042 04DEC13 ENHANCE COPY FEATURE                                         
* AKAT 041 22JAN13 FIX CM REQUEST BUG FOR NON-CABLE                             
* AKAT 040 31JAN11 FIX CM REQUEST BUG                                           
* AKAT 039 10JAN11 ADD A CM REQUEST WHEN CHANGING MKT FOR EXISTING BUYS         
*                  AND KEEP TRACK OF THE CHANGES WITH NEW X'04' ELEMS           
* AKAT 038 18NOV09 ADD FOUR NEW SUFFIX CODES NE,NF,KW AND KP                    
*                  AND PICK UP NEW VERSION OF SPCNCBLTAB                        
* AKAT 037 12NOV09 ALLOW ALL USERS TO CHANGE/DELETE MARKET                      
* AKAT 036 08AUG07 ADD 0D91 PASSIVE KEY ON ADDREC                               
* MCHO 035 09APR07 CHANGING KOOTENAYS TO KOOTENAY                               
* MCHO 034 09APR07 ADDED C'KO' KOOTENEYS                                        
* PWES 033 18AUG04 PRESERVE ANY PRORATION WHEN CHANGE A SUFFIX MARKET#          
* PWES 032 19JUL04 ACTION PFKEY AFTER ANY UPDATE / REMOVE PF11 FOR NEXT         
* PWES 031 21JUN04 COPY ACTION / CHANGE 'NO DEMO LINK' MSG ON LIST              
* PWES 030 19MAR04 ENHANCE DEMO ALPHAMKT UPDATE, INCL EST LEVEL RECS            
* PWES 029 14NOV03 CBLCONV REPLACES CBLDEF, NOW CBLDEF=CBLMKT SYNONYM           
* PWES 028 12NOV03 ACCOMMODATE ALPHAMKT FOR DEMO LOOKUP                         
* PWES 027 18MAR03 CBLMKT - FIX REDISP OF REC AFTER ADD + MKT DEL MSG           
* PWES 026 02JUL02 CBLMKT - LIST, SHOW IF CONVERTED (DDS ONLY)                  
* PWES 025 18JUN02 RETHINK UPDEST - IT ALWAYS IGNORED EST RECS ANYWAY!          
*                  REFINE MKTDEL CHK OF EST RECS, ADD ZZ AS N/B NOT 0%          
* PWES 024 22MAY02 NEW SUFFIX 'CE'                                              
* PWES 023 17MAY02 CBLDEF - CORRECT NDCONNWK (THO' NOT USED IN CONV!)           
*                  CBLMKT - NEW SUFFIX DEFS DEFAULT TO N/B (NOT 0%)             
* PWES 022 12APR02 CBLMKT - REFINE LOWER LEVEL DEFN CHECK (MKTDEL)              
* PWES 021 24MAR02 CBLDEF - ALLOW CONVERSION IF HAS LOCAL GST                   
* PWES 020 14MAR02 NEW SUFFIX 'SS' / CHKCONV SUFXTAB EOT TEST SOFTENED          
* PWES 019 04MAR02 DON'T PAGE ON CHANGE IF MARKETS ENTERED                      
* PWES 018 11FEB02 YO END OF SUFXTAB AGAIN + DONT DISP CBLMKT IN CBLDEF         
* PWES 017 05FEB02 FIX CHKCONV 'YO' NO LONGER END OF SUFXTAB                    
* PWES 016 05FEB02 2 NEW SUFFIXES KA / OK                                       
* PWES 015   JAN02 LEVEL 14 LOCAL PST REFINEMENT                                
* PWES 014 24JAN02 CBLDEF - ALLOW CONVERSION IF HAS LOCAL PST                   
* PWES 013 12DEC01 *DEF - ALLOW CBLPRO PFKEY SO CAN ALTER SEQ                   
* PWES 012 10DEC01 DON'T PAGE ON ADD IF MARKETS ENTERED                         
* PWES 011 05DEC01 DON'T ALLOW ADD OF CLIENT LEVEL *DEF RECORD                  
* PWES 9/10 30NOV01 APPEND 'ENG.' TO SOME SUFFIX NAMES                          
* PWES 008 26NOV01 ON ADD NEED CHECK FIELD FOR NULL FOR EOS                     
* PWES 007 21NOV01 'PF11 FOR NEXT' MESSAGE ON DISPLAY                           
* PWES 006 14NOV01 CBLDEF - CHECK FOR LOCAL PST/GST/TAX/SVC=CAN'T CONV          
* PWES 005 01NOV01 PRE-RELEASE REFINEMENTS + CBLDEF RECTYPE FOR CONVS           
* PWES 3-4 30OCT01 PRE-RELEASE REFINEMENTS + ONLY DISABLE 44 IF DDS             
* PWES 002 24OCT01 PRE-RELEASE REFINEMENTS + DISABLE TURNAROUND44               
* PWES 001 27SEP01 NEW - LIVE FOR DDS/TESTING                                   
***********************************************************************         
*                                                                     *         
*  TITLE: T2175F - CABLE MARKET DEFINITION MAINTENANCE                          
*                                                                     *         
*  CALLED FROM: SPOT CONTROLLER (T21700), WHICH CALLS                 *         
*               GEGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  INPUTS:  SCREENS SCSFM53  (T29753)  - MAINTENANCE                  *         
*           SCREENS SCSFM52  (T29752)  - LIST                         *         
*                                                                     *         
*  OUTPUTS: SKELETON NETWORK (X'0D11') RECORDS (CBLMKT RECORDS)       *         
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
         TITLE 'T2175F MARKET DEFINITION MAINTENANCE'                           
T2175F   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T2175F*,R7                                                    
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
*                                                                               
         BRAS  RE,SETUP                                                         
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
         CLI   MODE,PRINTREP       ADD  REQUEST FOR NSL REP (44)                
         BE    REQREC                                                           
         CLI   MODE,XRECADD        AFTER ADDREC                                 
         BE    REQREC                                                           
         CLI   MODE,XRECPUT        AFTER PUTREC                                 
         BE    REQREC                                                           
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
                                                                                
**********************************************************************          
*                  VALIDATE KEY ROUTINE                              *          
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
         BAS   RE,VNTWK            *** VALIDATE NETWORK ***                     
*                                                                               
         LA    R2,MNTCLTH          CLIENT                                       
         CLI   5(R2),0                                                          
         BE    *+8                 SKIP CLIENT                                  
         BAS   RE,VCLT             *** VALIDATE CLIENT ***                      
*                                                                               
         CLI   ACTNUM,ACTCOPY                                                   
         BE    VKCOPY                                                           
*                                                                               
         CLI   ACTNUM,ACTLIST      BOTTOM LINE FOR LIST                         
         BE    VKEX                                                             
         CLI   ACTNUM,ACTADD                                                    
         BE    *+12                                                             
         CLI   ACTNUM,ACTCHA                                                    
         BNE   VKDISP                                                           
*                                                                               
* ADD/CHANGE - IF MARKETS INPUT THIS TIME THEN --DO NOT PAGE--                  
* ELSE THEY WILL END UP BEING ASSIGNED TO THE NEXT PAGE'S SUFFIXES !            
         LA    R2,MNTSUFXH         START POSITION                               
         LA    R6,MNTNAMLH         LIMIT                                        
VKADD5   BAS   RE,NEXT             BUMP TO MARKET                               
         CLI   5(R2),0             CHECK FOR DEFINITION                         
         BE    *+12                                                             
         TM    4(R2),X'80'         INPUT THIS TIME                              
         BNZ   VKADD10             - YES LEAVE AS IS FOR VREC TO ADD            
         BAS   RE,NEXT             BUMP TO NAME                                 
         BAS   RE,NEXT             BUMP TO NEXT SUFFIX                          
         CR    R2,R6                                                            
         BL    VKADD5                                                           
         BAS   RE,VPAGE            *** VALIDATE PAGE ***                        
VKADD10  CLI   ACTNUM,ACTCHA                                                    
         BE    VKEX                                                             
         BAS   RE,SHOWMKTS                                                      
         B     VKEX                                                             
*                                                                               
VKDISP   BAS   RE,VPAGE            *** VALIDATE PAGE ***                        
         B     VKEX                                                             
*                                                                               
VKCOPY   LA    R2,CONRECH          *** VALIDATE COPY ***                        
         CLC   CONREC(4),=C'CBLC'  NOT FOR CBLCONV RECTYPE                      
         BE    VKERR                                                            
* COPY FROM KEY IS THAT FOR REC LAST DISPLAYED WHICH IS SET IN COPYKEY          
* BY DISPREC. ENSURE CONTENT OF COPYKEY WAS SET IMMEDIATLY PRIOR TO             
* COPY ACTION ELSE CONTENT COULD BE DEBRIS OR A HANGOVER FROM EARLIER           
         LA    R2,CONACTH          *** VALIDATE COPY ***                        
         CLC   TWALREC,RECNUM                                                   
         BNE   *+8                 BEEN ELSEWHERE, COPYKEY MAY BE DUFF          
         CLI   TWALACT,ACTDIS                                                   
         BE    *+14                ENSURE WAS DISP FOR EXTRA SAFETY             
         MVC   ERRNUM,=AL2(1247)                                                
         B     SPERREX                                                          
*                                                                               
         CLC   NDEFKEY,COPYKEY     ENSURE NOT EXACT SAME KEY                    
         BNE   *+14                                                             
         MVC   ERRNUM,=AL2(398)                                                 
         B     SPERREX                                                          
***      CLC   NDEFKEY(NDEFKCLT-NDEFRECD),COPYKEY   MUST BE SAME NWK            
***      BE    VKEX                                                             
***      MVC   ERRNUM,=AL2(836)                                                 
****     B     SPERREX                                                          
*                                                                               
VKEX     MVC   KEY,MYKEY                                                        
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
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
**********************************************************************          
*  VALIDATE NETWORK (CABLE NETWORK STATION)                          *          
**********************************************************************          
                                                                                
VNTWK    NTR1  ,                                                                
         LA    R4,MYKEY                                                         
         USING NDEFRECD,R4                                                      
*                                                                               
         LA    R2,MNTNTWKH         NETWORK                                      
         CLI   5(R2),4             CHECK FOR AGENCY LEVEL DEFAULT               
         BNE   *+14                                                             
         CLC   8(4,R2),=C'*DEF'    DEFAULT RECORD IS SPECIAL                    
         BE    VN20                LEAVE NDEFKNET NULL                          
*                                                                               
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+12                NOT REQUIRED IF LIST                         
         CLI   LSTCLT,C'>'                                                      
         BNE   VN10                                                             
         GOTO1 ANY                 REQUIRED                                     
*                                                                               
         MVI   USEIONUM,2          USE AIO2                                     
         GOTO1 VALISTA             VALIDATE STATION (SPSFM00 LEVEL)             
*                                  (VALID THUS MUST HAVE MASTER REC)            
         CLC   QMKT,=C'0000'                                                    
         BNE   ERRNETW             MUST BE A (CABLE) NETWORK STATION            
         MVI   USEIONUM,0          RESTORE AIO1                                 
         MVC   AIO,AIO1                                                         
*                                                                               
VN10     LLC   R5,5(R2)                                                         
         LTR   R5,R5                                                            
         BNP   VNEX                                                             
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   NDEFKNET(0),MNTNTWK MOVE NETWORK INTO KEY                        
         OC    NDEFKNET,SPACES     SPACEPAD                                     
VN20     MVC   SVNTWK,NDEFKNET                                                  
*                                                                               
VNEX     B     EXIT                                                             
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
         LLC   RF,5(R2)            DECRIMENT LENGTH                             
         BCTR  RF,0                                                             
         LTR   RF,RF                                                            
         BP    *+8                                                              
         B     INV                 ERROR                                        
         STC   RF,5(R2)                                                         
         OI    LISTFLAG,CLSTARTQ   START AT FILTER                              
         B     VCLT25                                                           
*                                                                               
VCLT10   CLC   =C'*DEF',MNTNTWK    DEFAULT RECORD CANNOT HAVE CLIENT            
         BE    VCERR                                                            
         MVC   KEY,NDEFKEY         CHECK FOR NETWORK KEY ON FILE                
         GOTO1 HIGH                READ THE KEY                                 
         CLC   KEY(13),KEYSAVE     SAME KEY (NETWORK LEVEL ON FILE)?            
         JE    VCLT20              YES - SO CONTINUE                            
*                                                                               
         LA    R2,MNTNTWKH         ELSE - GET A(NETWORK FIELD)                  
         B     NFND                ERROR                                        
*                                                                               
VCLT20   NI    LISTFLAG,X'FF'-CLSTARTQ                                          
         GOTO1 VALICLT                                                          
*                                                                               
VCLT25   TM    LISTFLAG,CLSTARTQ                                                
         BZ    VCLT30                                                           
         CLI   5(R2),2                                                          
         BL    VCERR                                                            
*                                                                               
         MVC   QCLT,SPACES                                                      
         MVI   QCLT+2,C'A'                                                      
         LLC   RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   QCLT(0),8(R2)                                                    
         GOTO1 CLPACK,DMCB,QCLT,BCLT                                            
         CLI   0(R1),0                                                          
         BNE   VCERR                                                            
         MVC   8(L'TEMPCLT,R2),TEMPCLT  RESTORE ORIGINAL VALUE                  
         LLC   RF,5(R2)            INCREMENT LENGTH                             
         LA    RF,1(RF)                                                         
         STC   RF,5(R2)                                                         
*                                                                               
VCLT30   MVC   NDEFKCLT,BCLT       MOVE CLIENT INTO THE KEY                     
         MVC   SVCLT,BCLT                                                       
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*  VALIDATE PAGE                                                     *          
**********************************************************************          
                                                                                
VPAGE    NTR1  ,                                                                
         LA    R2,MNTPAGEH         PAGE                                         
         CLI   5(R2),0                                                          
         BE    VPAGE15             DEFAULT TO 1                                 
*                                                                               
         TM    4(R2),X'08'         TEST VALID NUMERIC                           
         BZ    VKERR                                                            
         LLC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         CHI   RE,1                TEST IN RANGE 1-3                            
         BL    VKERR                                                            
         CHI   RE,3                                                             
         BH    VKERR                                                            
         STC   RE,THISPAGE         SET BINARY PAGE                              
*                                                                               
VPAGE10  CLI   PFKEY,5             HANDLE PF5 SCROLL                            
         BNE   VPAGEX                                                           
*                                                                               
         CLI   MNTLN24+12+L'PF05PAGE-1,C'2'   ON PAGE 1?                        
         BE    VPAGE20                        YES - FORCE TO PAGE 2             
         CLI   MNTLN24+12+L'PF05PAGE-1,C'3'   ON PAGE 2?                        
         BE    VPAGE30                        YES - FORCE TO PAGE 3             
*                                                                               
VPAGE15  MVI   THISPAGE,1                                                       
         MVI   8(R2),C'1'                                                       
         B     VPAGE40                                                          
*                                                                               
VPAGE20  MVI   THISPAGE,2                                                       
         MVI   8(R2),C'2'                                                       
         B     VPAGE40                                                          
*                                                                               
VPAGE30  MVI   THISPAGE,3                                                       
         MVI   8(R2),C'3'                                                       
                                                                                
VPAGE40  OI    6(R2),X'80'                                                      
*                                                                               
VPAGEX   B     EXIT                                                             
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
         MVC   MNTCLT(L'QCLT),QCLT        CLIENT UNPACKED FORMAT                
         OI    MNTCLTH+6,X'80'     CLIENT                                       
*                                                                               
DK10     BAS   RE,VPAGE                                                         
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
                                                                                
**********************************************************************          
*  DISPLAY RECORD                                                    *          
**********************************************************************          
                                                                                
DR       LA    R2,MNTSUFXH         FIRST FIELD TO CLEAR                         
         BAS   RE,CLEAR            CLEAR FIELDS                                 
         MVC   COPYKEY,MYKEY       COPY IS BASED ON LAST DISPD REC/KEY          
*                                                                               
* IF NOT A CBLMKT/PRO RECORD BUT ORIGINAL NETDEF WE ALLOW ADDING                
* OF CONVERSION ELEMENTS TO GET FROM OLD STATION ID TO NEW MKT/SFX              
* ALLOWED VIA CBLMKT AND TEMP RECTYPE SYNONYM OF CBLCONV                        
         L     R3,AIO                                                           
         MVI   ELCODE,NDEFNELQ                                                  
         BAS   RE,GETEL            NWK ELEMENT                                  
         BNE   *+12                ASSUME NOT CABLE                             
         CLI   2(R3),NDEFCABQ      CABLE RECORD ?                               
         BE    DRTEMP10                                                         
         L     R3,AIO                                                           
         USING NDEFRECD,R3                                                      
         LA    R2,MNTCLTH                                                       
         OC    NDEFKCLT,NDEFKCLT   ONLY ALLOW CONV AT NETWORK LEVEL             
         BNZ   INV                                                              
         DROP  R3                                                               
         LA    R2,MNTNTWKH                                                      
         BAS   RE,CHKNETOK         CHECK NETWORK OK TO CONVERT                  
         BNE   SPERREX                                                          
         BAS   RE,MATCONV          CHECK ALL STATIONS HAVE CONV DEF             
         BRAS  RE,SHOWCONV         DISPLAY CONVERSION MARKETS                   
         B     DREX                                                             
*                                                                               
DRTEMP10 MVC   MNTMSG,SPACES       CLEAR ANY PREV MSG                           
         OI    MNTMSGH+6,X'80'                                                  
*                                                                               
         CLC   CONREC(4),=C'CBLC'  DON'T DISP CBLMKT IF CBLCONV RECTYPE         
         BNE   *+12                                                             
         LA    R2,MNTNTWKH                                                      
         B     INV                                                              
*                                                                               
         L     R3,AIO                                                           
         BAS   RE,SHOWMKTS         DISPLAY MARKETS                              
*                                                                               
DREX     CLI   PFKEY,6                                                          
         BNE   *+8                                                              
         MVI   PFKEY,0                                                          
*                                                                               
         CLI   MODE,PRINTREP       <<< ??? >>>                                  
         BNE   DRXIT                                                            
         LA    R2,CONACTH                                                       
         B     REQOV               REPORT WILL BE GENERATED OV                  
*                                                                               
DRXIT   CLI    ADDEDCM,C'Y'        ADDED A CM REQUEST?                          
        BNE    EXIT                NO                                           
        MVI    ADDEDCM,C'N'        RESET                                        
        B      CMREQMSG            GIVE CM REQUEST MESSAGE                      
**********************************************************************          
*  VALIDATE RECORD                                                   *          
**********************************************************************          
                                                                                
VR       DS    0H                                                               
***      CLI   =AL1(SUFFNUMQ),CDEFMAXQ  TRAP IF TOO MANY SUFFIXES, IF           
***      BH    RECF                DEFINE ALL THEN WONT FIT CBLPRO SCRN         
*                                                                               
         CLI   ACTNUM,ACTCOPY                                                   
         BNE   *+12                                                             
         BRAS  RE,COPYREC                                                       
         B     EXIT                                                             
*                                                                               
* IF NOT A CBLMKT/PRO RECORD BUT ORIGINAL NETDEF WE ALLOW ADDING                
* OF CONVERSION ELEMENTS TO GET FROM OLD STATION ID TO NEW MKT/SFX              
* ALLOWED VIA CBLMKT AND TEMP RECTYPE SYNONYM OF CBLCONV                        
         CLI   ACTNUM,ACTADD                                                    
         BE    VR5                 CAN ONLY ADD NEW STYLE                       
         MVC   MYKEY,KEY                                                        
         MVC   AIO,AIO1                                                         
         L     R3,AIO                                                           
         MVC   0(13,R3),MYKEY                                                   
         MVI   ELCODE,NDEFNELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   VALCONV             ASSUME NOT CABLE                             
         CLI   2(R3),NDEFCABQ      CABLE RECORD ?                               
         BNE   VALCONV                                                          
*                                                                               
VR5      MVC   MYKEY,KEY                                                        
         MVC   AIO,AIO1                                                         
         L     R3,AIO                                                           
         MVC   0(13,R3),MYKEY                                                   
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR10                                                             
         LA    R2,MNTNTWKH                                                      
         BAS   RE,CHKNETOK         CHECK NETWORK OK TO BE SPECIALTY CBL         
         BNE   SPERREX                                                          
* ADD NEW NETWORK RECORD / CLIENT RECORD FOR NETWORK                            
*        CLI   MNTCLTH+5,0         CLIENT LEVEL?                                
*        BNE   ADDCLT              ADD CLIENT WITH NETWORK STATIONS             
*                                                                               
         BAS   RE,CHKMKTS          CHECK FOR MARKET DEFINITIONS                 
*                                                                               
         TM    EL01FLAG,EL01GOTQ                                                
         BNZ   *+12                                                             
         LA    R2,MNTMKTH                                                       
         B     MIS                 MUST HAVE AT LEAST 1 DEFINITION              
         BAS   RE,ADDZZ            CREATE 01 ZZZZ ELEMENT                       
         BAS   RE,ADD02            CREATE 02 ELEMENT                            
         B     VREX                ADD NEW NETWORK RECORD                       
*                                                                               
VR10     CLI   MNTMKTH+5,0         DELETE INDICATED IN 1ST MKT (DDS)            
         BE    VR20                                                             
         CLC   =C'$DEL',MNTMKT     DDS WANTS TO DELETE THE RECORD?              
         BNE   VR20                                                             
         BAS   RE,DELREC           CALL THE DELETE ROUTINE                      
         B     VREX10              EXIT THE PROGRAM                             
*                                                                               
VR20     BAS   RE,CHKMKTS          CHECK FOR NEW/CHANGED MARKETS                
*                                                                               
         TM    EL01FLAG,EL01GOTQ   ANY 01 ELS FOR SCREEN                        
         BNZ   VR30                - YES                                        
         L     R3,AIO              - NO, CHECK REC NOT MADE EMPTY               
         MVI   ELCODE,NDEFELQ                                                   
         BAS   RE,GETEL                                                         
         BE    VR30                                                             
         LA    R2,MNTMKTH                                                       
         B     MIS                 MUST HAVE AT LEAST 1 DEFINITION              
*                                                                               
VR30     CLI   PFKEY,6                                                          
         BNE   *+8                                                              
         BAS   RE,UPDALPH          UPDATE SOFT DEMO LINK ALPHAMKTS              
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   *+12                - NOPE                                       
         CLI   ACTNUM,ACTCHA       ALWAYS UPDATE EST RECS WHEN CHANGE           
         BE    *+12                ALLOWS US TO TIDY UP CURRENT MESS            
         TM    EL01FLAG,EL01CHAQ   ANY 01 EL CHANGES FOR SCREEN                 
         BZ    VREX                - NO                                         
*                                                                               
         CLI   MNTCLTH+5,0         CLIENT LEVEL?                                
         BE    *+8                                                              
         BAS   RE,UPDEST           UPDATE ESTIMATE RECORDS FOR THIS CLI         
*                                                                               
VREX     CLI   ACTNUM,ACTADD                                                    
         BE    VREX10                                                           
*                                                                               
* IF REC WAS CONVERTED FROM NETDEF THERE MAY NOT HAVE BEEN A ZZZZ               
         BAS   RE,ADDZZ            ENSURE CONVERTED NETDEFS GET ZZ ELEM         
*                                                                               
*                                  OTHER RECORD READS DONE                      
         MVC   KEY,MYKEY                                                        
         GOTO1 HIGH                RESTORE ORIGINAL SEQUENCE                    
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC              RESET CORRECT D/A FOR PUTREC                 
         MVC   AIO,AIO1                                                         
*                                                                               
VREX10   L     R3,AIO                                                           
         LA    R1,CDEFMAXQ+2       TRAP IF TOO MANY SUFFIXES                    
         MVI   ELCODE,NDEFELQ      COUNT ALL THE X'01' ELEMENTS                 
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
VREX20   BAS   RE,NEXTEL                                                        
         BNE   VREX30                                                           
         BCT   R1,VREX20                                                        
         B     RECF                WONT FIT CBLPRO SCREEN!                      
*                                                                               
VREX30   CLI   PFKEY,5             HANDLE PF5 AFTER DOING CHANGES               
         BNE   *+8                                                              
         BAS   RE,VPAGE                                                         
         B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
*  VALIDATE CONVERSIONS                                              *          
**********************************************************************          
                                                                                
VALCONV  EQU   *                                                                
         XC    CMREQ,CMREQ         CLEAR THE CM REQUEST AREA                    
         MVC   MYKEY,KEY                                                        
         MVC   AIO,AIO1                                                         
         L     R3,AIO                                                           
         MVC   0(13,R3),MYKEY                                                   
*                                                                               
         BAS   RE,CHKCONV          CHECK FOR CONVERSION STATIONS                
*                                                                               
*                                  OTHER RECORD READS DONE                      
VCEX     MVC   KEY,MYKEY                                                        
         GOTO1 HIGH                RESTORE ORIGINAL SEQUENCE                    
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC              RESET CORRECT D/A FOR PUTREC                 
         MVC   AIO,AIO1                                                         
*                                                                               
VCEX10   CLI   PFKEY,5             HANDLE PF5 AFTER DOING CHANGES               
         BNE   *+8                                                              
         BAS   RE,VPAGE                                                         
         B     EXIT                                                             
         EJECT                                                                  
                                                                                
**********************************************************************          
* ADD A CLIENT SPECIFIC RECORD WITH ALL NETWORK MARKETS              *          
* THIS IS EFFECTIVELY A 'COPY' OF NETWORK LEVEL RECORD               *          
**********************************************************************          
*&&DO                                                                           
ADDCLT   XC    KEY,KEY                                                          
         MVC   KEY(8),MYKEY        NETWORK                                      
         GOTO1 HIGH                FIND NETWORK LEVEL RECORD                    
*                                                                               
         CLC   KEY(8),KEYSAVE                                                   
         BE    ADDC10                                                           
         LA    R2,MNTNTWKH                                                      
         B     NFND                ERROR                                        
*                                                                               
ADDC10   GOTO1 GETREC              GET NETWORK LEVEL RECORD                     
*                                                                               
         USING NDEFRECD,R3                                                      
         MVC   NDEFKEY,MYKEY       SET NEW KEY                                  
         XC    NDEFLINK,NDEFLINK   CLEAR LINKAGE (?)                            
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
*&&                                                                             
**********************************************************************          
*  BUMP SCREEN FIELD POINTER                                         *          
**********************************************************************          
                                                                                
NEXT     NTR1  ,                                                                
         LLC   R4,0(R2)                                                         
         AR    R2,R4               BUMP TO NEXT FIELD                           
*                                                                               
NEXTEX   XIT1  REGS=(R2)           DON'T RESTORE OLD VALUE OF R2                
         EJECT                                                                  
**********************************************************************          
*  CLEAR SCREEN                                                      *          
*  ENTRY - R2=A(SCREEN FIELD TO BEGIN CLEARING FROM)                 *          
**********************************************************************          
CLEAR    NTR1  ,                                                                
*        LA    R6,MNTLAST          LAST FIELD                                   
         LA    R6,MNTNAML          LAST FIELD                                   
*                                                                               
CLEAR10  CR    R2,R6               ALL FIELDS HAVE BEEN CLEARED?                
         BNL   CLEAREX                                                          
         LLC   R5,0(R2)            TOTAL LENGTH                                 
         SHI   R5,8                MAX LENGTH OF DATA                           
         LTR   R5,R5               NO DATA?                                     
         BNP   CLEAR20                                                          
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)       CLEAR FIELD                                  
         OI    6(R2),X'80'                                                      
*                                                                               
CLEAR20  BAS   RE,NEXT             NEXT FIELD                                   
         B     CLEAR10                                                          
*                                                                               
CLEAREX  B     EXIT                                                             
         EJECT                                                                  
**********************************************************************          
* SHOW MARKETS FOR THIS PAGE                                         *          
* NOTE - CALLED FROM VALKEY FOR ACTION 'ADD'                         *          
**********************************************************************          
SHOWMKTS NTR1  ,                                                                
         LA    R2,MNTSUFXH                                                      
         LA    R6,PAGEMAXQ                                                      
         LAY   R5,SUFFTAB                                                       
         CLI   THISPAGE,2                                                       
         BNE   *+10                                                             
         LAY   R5,SUFFTAB2                                                      
         CLI   THISPAGE,3                                                       
         BNE   *+10                                                             
         LAY   R5,SUFFTAB3                                                      
         USING SUFFTABD,R5                                                      
*                                  --- SUFFIX ---                               
SM10     MVC   8(2,R2),SUFXCODE                                                 
         OI    6(R2),X'80'                                                      
*                                  --- MARKET ---                               
         BAS   RE,NEXT             GO TO NEXT FIELD                             
         XC    THISMKT,THISMKT                                                  
         CLI   ACTNUM,ACTADD       NO ELEMS EXIST ON ADD                        
         BNE   *+12                                                             
         CLI   MODE,XRECADD        - EXCEPT AFTER THE ADD IS DONE               
         BNE   SM30                                                             
         L     R3,AIO              FIND ANY ELEM FOR THIS SUFFIX                
         MVI   ELCODE,NDEFELQ                                                   
         BAS   RE,GETEL            FIRST ELEMENT                                
         B     *+8                                                              
SM20     BAS   RE,NEXTEL           NEXT ELEMENT                                 
         BNE   SM30                NO DEFINITION                                
         CLC   SUFXCODE,NDEFMSUF-NDEFEL01(R3)                                   
         BNE   SM20                                                             
*                                  SHOW MARKET CODE                             
         MVC   THISMKT,NDEFMNUM-NDEFEL01(R3)                                    
         LH    R1,THISMKT                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(4,R2),DUB                                                      
         OI    6(R2),X'80'                                                      
         MVI   5(R2),4             SET LENGTH FOR VALIMKT                       
         MVI   4(R2),X'08'         SET NUMERIC FOR VALIMKT                      
*                                  PASS CODE THRU VALIDATOR FOR NAME            
         MVI   USEIONUM,2          USE AIO2                                     
         OI    TRNSTAT,NOVALERR                                                 
         GOTO1 VALIMKT             VALIDATE MARKET (SPSFM00 LEVEL)              
         MVI   USEIONUM,0          RESTORE AIO1                                 
         MVC   AIO,AIO1                                                         
         MVC   THISAMKT,NDEFAMKT-NDEFEL01(R3) TAKE FROM REC NOT VALIMKT         
*                                  --- NAME ---                                 
SM30     BAS   RE,NEXT             BUMP TO NEXT FIELD                           
         MVC   8(L'MNTNAME,R2),SPACES                                           
         OC    THISMKT,THISMKT                                                  
         BZ    SM35                                                             
         MVC   8(L'MNTNAME,R2),MKTNM                                            
         MVC   8+(L'MNTNAME-5)(5,R2),SPACES                                     
         MVC   8+(L'MNTNAME-3)(3,R2),THISAMKT                                   
         B     *+10                                                             
SM35     MVC   8(L'MNTNAME,R2),SUFXNAME                                         
SM36     OI    6(R2),X'80'                                                      
*                                                                               
         BAS   RE,NEXT                                                          
         LA    R5,SUFFTBLQ(R5)     NEXT AVAILABLE MARKET                        
         CLI   0(R5),X'FF'                                                      
         BE    SM40                EOT                                          
         BCT   R6,SM10                                                          
*                                                                               
SM40     CLI   ACTNUM,ACTADD                                                    
         BNE   SMEX                                                             
         CLI   MODE,XRECADD                                                     
         BE    SMEX                                                             
         BAS   RE,CLEAR            CLEAR DOWN REST OF SCREEN                    
SMEX     B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
**********************************************************************          
* MATCH CONVERSIONS - CHECK FOR FULL SUITE OF CONVERSION ELEMENTS    *          
* NOTE - CALLED FOR NON CBLMKT/PRO RECORDS ONLY                      *          
**********************************************************************          
                                                                                
MATCONV  NTR1  ,                                                                
         MVC   CONVMSG,SPACES                                                   
         XC    UNMATCHD,UNMATCHD                                                
         LA    R5,CONVMSG+19       (ALLOW FOR MSG PREFIX)                       
         L     R3,AIO              SCAN DEF ELEMS                               
         MVI   ELCODE,NDEFELQ                                                   
         BAS   RE,GETEL            FIRST ELEMENT                                
         B     *+8                                                              
MC20     BAS   RE,NEXTEL           NEXT ELEMENT                                 
         BNE   MC50                END/NO DEFINITION                            
         CLC   NDEFSTA-NDEFEL01(L'NDEFSTA,R3),=C'ZZZZ'  IGNORE ZZZZ EL          
         BE    MC20                                                             
*                                                                               
         LR    R4,R3               FIND CONV ELEMS                              
         L     R3,AIO                                                           
         MVI   ELCODE,NDCONELQ                                                  
         BAS   RE,GETEL            FIRST ELEMENT                                
         B     *+8                                                              
MC30     BAS   RE,NEXTEL           NEXT ELEMENT                                 
         BNE   MC35                END/NO DEFINITION                            
         USING NDEFEL01,R4                                                      
         CLC   NDEFSTA,NDCONSTA-NDEFEL03(R3)                                    
         BNE   MC30                                                             
         B     MC40                NEXT DEF                                     
*                                  SHOW NEEDS CONV IN MSG                       
MC35     LLC   RF,UNMATCHD                                                      
         LA    RF,1(RF)                                                         
         STC   RF,UNMATCHD                                                      
MC35A    LA    RF,CONVMSG+(L'CONVMSG-5)                                         
         CR    R5,RF                                                            
         BNH   MC37                                                             
         LR    R5,RF                                                            
         MVC   0(3,R5),=C'...'                                                  
         B     MC50                                                             
MC37     MVC   0(4,R5),NDEFSTA                                                  
         LA    R5,5(R5)                                                         
MC40     MVI   ELCODE,NDEFELQ      NEXT DEF                                     
         LR    R3,R4                                                            
         B     MC20                                                             
         DROP  R4                                                               
*                                  SHOW MESSAGE                                 
MC50     CLI   UNMATCHD,0                                                       
         BE    *+10                                                             
         MVC   CONVMSG(19),=C'STATIONS TO DEFINE '                              
         MVC   MNTMSG,CONVMSG                                                   
         OI    MNTMSGH+6,X'80'                                                  
MCEX     B     EXIT                                                             
         EJECT                                                                  
                                                                                
**********************************************************************          
*  UPDATE ALL SOFT DEMO LINK ALPHA MARKETS - NOT JUST THOSE FOR      *          
*  MARKETS CURRENTLY DISPLAYED (I.E. CATER FOR PAGE 1&2 AT ONCE)     *          
**********************************************************************          
                                                                                
UPDALPH  NTR1  ,                                                                
         L     R3,AIO                                                           
         MVI   ELCODE,NDEFELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
UPDALP10 BAS   RE,NEXTEL                                                        
         BNE   UPDALPX                                                          
         BRAS  RE,SETAMKT                                                       
         B     UPDALP10                                                         
*                                                                               
UPDALPX  B     EXIT                                                             
         EJECT                                                                  
                                                                                
**********************************************************************          
*  UPDATE ALL ESTIMATE SPECIFIC CBLPRO RECORDS FOR THIS NWK/CLIENT   *          
*                                                                    *          
*  THE ESTIMATE SPECIFIC CBLPRO RECORDS ARE ADDED BY SPSFM60 AS A    *          
*  COPY OF THE CLIENT LEVEL CBLMKT/PRO RECORD - THEY MUST MAINTAIN   *          
*  THE SAME MARKET DEFINITIONS AS THE CLIENT LEVEL                   *          
**********************************************************************          
                                                                                
UPDEST   NTR1  ,                                                                
         MVC   KEY,MYKEY                                                        
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         B     UE99                                                             
*                                                                               
UE10     CLC   KEY(10),MYKEY       CHECK CORRECT NETWORK/CLIENT                 
         BNE   UESTEX                                                           
         CLI   KEY+10,0                                                         
         BE    UE99                NOT ESTIMATE SPECIFIC !                      
*                                                                               
         MVI   RDUPDATE,C'Y'       GET RECORD FOR UPDATE                        
         GOTO1 GETREC                                                           
*                                                                               
* UPDATE EST RECS BY COMPARING THEIR MKTS TO THOSE IN CLI REC                   
* MUCH EASIER THAN USING CHKMKTS WHICH WILL BREAK I/O READ SEQ                  
*        BAS   RE,CHKMKTS          APPLY NEW/CHANGED MARKETS                    
*                                                                               
* FIRST CHECK EST HAS EVERY CLI MARKET (I.E. CATER FOR ADDS)                    
*                                                                               
         L     R3,AIO1             FIND ALL MARKETS IN CLI REC                  
         MVI   ELCODE,NDEFELQ                                                   
         BAS   RE,GETEL            FIRST ELEMENT                                
         BE    *+6                 CLI MUST HAVE AT LEAST ONE 01 EL             
         DC    H'0'                                                             
UE20     LR    R5,R3               SAVE A(THIS CLI 01 ELEM)                     
         MVC   THISSUFX,NDEFMSUF-NDEFEL01(R5)                                   
         MVC   THISMKT,NDEFMNUM-NDEFEL01(R5)                                    
         MVC   THISAMKT,NDEFAMKT-NDEFEL01(R5)                                   
* THOUGHT OF INCLUDING THIS BUT DECIDED TO KEEP EST POSTIONS INLINE             
* WITH CLIENT ONES WHILE SORTING OUT CURRENT MESS                               
*!       CLC   THISSUFX,=C'ZZ'     IGNORE ZZ - IF EST MISSING WE ADD            
*!       BE    UE40                            LATER IN CORRECT POSN            
*                                                                               
         L     R3,AIO2             LOCATE MARKET IN EST REC                     
         MVI   ELCODE,NDEFELQ                                                   
         BAS   RE,GETEL            FIRST ELEMENT                                
         BE    *+6                 EST MUST HAVE AT LEAST ONE 01 EL             
         DC    H'0'                                                             
UE30     ST    R3,A01ELEM          SAVE A(THIS 01 ELEM)                         
         CLC   THISSUFX,NDEFMSUF-NDEFEL01(R3)                                   
         BE    UE35                FOUND ELEMENT FOR SUFFIX                     
         BAS   RE,NEXTEL           NEXT EST ELEMENT                             
         BE    UE30                                                             
*                                  NO EST ELEM FOR THIS CLI SUFFIX              
         L     R3,A01ELEM          POINT TO LAST EST 01 FOUND                   
* COULD CHECK IF ZZ AND ADD NEW ONE BEFORE IT ?                                 
         LLC   RF,1(R3)                                                         
         AR    R3,RF               BUMP PAST                                    
         ST    R3,A01ELEM          SET LOCATION FOR NEW ELEM                    
*                                                                               
         LA    RF,ELEM             BUILD A DEFINITION (01) ELEMENT              
         USING NDEFEL01,RF                                                      
         XC    ELEM,ELEM                                                        
         MVI   ELEM,NDEFELQ        X'01'                                        
         MVI   ELEM+1,ELEMLEN                                                   
         MVC   NDEFMSUF,THISSUFX                                                
         MVC   NDEFMNUM,THISMKT                                                 
         MVC   NDEFAMKT,THISAMKT                                                
         MVC   NDEFPCT,=F'-1'      PCT INITS TO N/B                             
         DROP  RF                                                               
*                                                                               
         L     R3,A01ELEM          ADD IN POSITION                              
         GOTO1 RECUP,DMCB,(0,AIO),ELEM,(R3)   ADD AN ELEMENT                    
         B     UE40                                                             
*                                  COMPARE CLI MKT WITH THAT IN EST             
UE35     CLC   THISMKT,NDEFMNUM-NDEFEL01(R3)      SHOULD BE SAME                
         BE    UE36                NO CHANGE                                    
*!*! DON'T FIX - WE NEED CHECK FOR BUYS ETC                                     
*!*!     MVC   NDEFMNUM-NDEFEL01(L'NDEFMNUM,R3),THISMKT  FIX IT                 
         B     UE40    *REMOVE THIS IF REINSTATE ABOVE SO DO UE36 MVC*          
UE36     MVC   NDEFAMKT-NDEFEL01(L'NDEFAMKT,R3),THISAMKT UPDATE DEMLINK         
UE40     LR    R3,R5                                                            
         BAS   RE,NEXTEL           NEXT CLIENT ELEMENT                          
         BE    UE20                                                             
*                                                                               
* NOW CHECK ALL EST MARKETS STILL IN CLIENT (I.E. DELETES)                      
*                                                                               
         L     R3,AIO2             FIND ALL MARKETS IN EST REC                  
         MVI   ELCODE,NDEFELQ                                                   
         BAS   RE,GETEL            FIRST ELEMENT                                
         BE    *+6                 EST MUST HAVE AT LEAST ONE 01 EL             
         DC    H'0'                                                             
UE50     LR    R5,R3               SAVE A(THIS EST 01 ELEM)                     
         MVC   THISSUFX,NDEFMSUF-NDEFEL01(R5)                                   
         MVC   THISMKT,NDEFMNUM-NDEFEL01(R5)                                    
*                                                                               
         L     R3,AIO1             LOCATE MARKET IN CLI REC                     
         MVI   ELCODE,NDEFELQ                                                   
         BAS   RE,GETEL            FIRST ELEMENT                                
         BE    UE60                CLI MUST HAVE AT LEAST ONE 01 EL             
         DC    H'0'                                                             
UE60     CLC   THISSUFX,NDEFMSUF-NDEFEL01(R3)                                   
         BE    UE65                FOUND CLI ELEMENT FOR EST SUFFIX             
         BAS   RE,NEXTEL           NEXT CLI ELEMENT                             
         BE    UE60                                                             
*                                  NO CLI ELEM FOR THIS EST SUFFIX              
         LR    R3,R5               READDRESS THE EST ELEM                       
         CLC   NDEFPCT-NDEFEL01(L'NDEFPCT,R3),=F'-1' DON'T REMOVE FROM          
         BE    *+14                                  EST IF HAS COST            
         OC    NDEFPCT-NDEFEL01(L'NDEFPCT,R3),NDEFPCT-NDEFEL01(R3)              
         BNZ   UE65                CLIENT REC NEEDS THIS ONE ADDED!             
         GOTO1 RECUP,DMCB,(0,AIO),(R3)     REMOVE ELEMENT FROM EST              
*                                                                               
UE65     LR    R3,R5                                                            
         BAS   RE,NEXTEL                                                        
         BE    UE50                                                             
*                                                                               
*>>>>>>>>                                                                       
* IF REC WAS CONVERTED FROM NETDEF THERE MAY NOT HAVE BEEN A ZZZZ               
         BAS   RE,ADDZZ            ENSURE CONVERTED NETDEFS GET ZZ ELEM         
*>>>>>>>>                                                                       
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
UE99     GOTO1 SEQ                                                              
         B     UE10                                                             
*                                                                               
UESTEX   MVC   AIO,AIO1                                                         
         MVC   KEY,MYKEY           RESTORE                                      
         B     EXIT                                                             
                                                                                
**********************************************************************          
*  CHECK MARKET DEFINTIONS                                           *          
*  SCAN DOWN SCREEN AND DELETE/ADD ELEMENTS FOR ALL AVAILABLE        *          
*  MARKETS SHOWN ON THIS SCREEN/PAGE                                 *          
*                                                                    *          
*  ENTRY - RECORD TO UPDATE ADDRESSED BY AIO                         *          
*  EXIT  - RECORD UPDATED WITH DEFINITION ADDITIONS                  *          
*        - EL01FLAG SET TO INDICATE 01 ELEM ACTIVITY FOR THIS PAGE   *          
**********************************************************************          
                                                                                
CHKMKTS  NTR1  ,                                                                
         XC    CMREQ,CMREQ         CLEAR THE CM REQUEST AREA                    
         MVI   EL01FLAG,0                                                       
         LA    R2,MNTSUFXH         START POSITION                               
         LA    R6,MNTNAMLH         LIMIT                                        
*                                                                               
CM10     CR    R2,R6                                                            
         BNL   CMKTEX              DONE SCREEN                                  
*                                                                               
         CLI   7(R2),0             CHECK FOR ZERO LENGTH SUFFIX                 
         BE    CMKTEX              DONE ALL AVAIL MKTS ON SCREEN                
         MVC   THISSUFX,8(R2)                                                   
         CLC   THISSUFX,SPACES                                                  
         BE    CMKTEX              DONE ALL AVAIL MKTS ON SCREEN                
         OC    THISSUFX,THISSUFX                                                
         BZ    CMKTEX              DITTO - 'CLEAR' LEAVES NULLS                 
*                                                                               
         BAS   RE,NEXT             BUMP TO MARKET                               
         ST    R2,AMKTFLD          SAVE A(MARKET FIELD) MAY NEED LATER          
         XC    THISMKT,THISMKT                                                  
         MVC   THISPCT,=F'-1'      DEFAULT PCT IS 'NB'                          
         CLI   5(R2),0             CHECK FOR DEFINITION                         
         BNE   CM15                - YES                                        
         BAS   RE,NEXT             - NO, BUMP TO NAME                           
         CLI   ACTNUM,ACTADD                                                    
         BE    CM50                MUST HAVE INPUT FOR ADD                      
         B     CM20                NO/CLEARED INPUT (IMPLIED DELETE)            
*                                                                               
CM15     EQU   *                                                                
*NO!     CLC   =C'DEL',8(R2)       (DELETE IS NOT ALLOWED)                      
*NO!     BE    CM20                                                             
         BAS   RE,VALMKT           VALIDATE MARKET                              
*                                                                               
         BAS   RE,NEXT             BUMP TO NAME                                 
         MVC   8(L'MNTNAME,R2),MKTNM  SHOW TRUE MARKET RECORD NAME              
         MVC   8+(L'MNTNAME-3)(3,R2),THISAMKT                                   
         OI    6(R2),X'80'                                                      
*                                                                               
CM20     L     R3,AIO              FIND ANY ELEM FOR THIS SUFFIX                
         LA    R1,NDEFEL-NDEFRECD(R3)                                           
         ST    R1,A01ELEM          SET AS START OF REC INCASE NO 01 ELS         
         MVI   ELCODE,NDEFELQ                                                   
         BAS   RE,GETEL            FIRST ELEMENT                                
         BNE   CM40                NO 01 ELEMS (ON ADD)                         
CM30     ST    R3,A01ELEM          SAVE A(THIS 01 ELEM)                         
         CLC   THISSUFX,NDEFMSUF-NDEFEL01(R3)                                   
         BE    CM35                FOUND ELEMENT FOR SUFFIX                     
         BAS   RE,NEXTEL           NEXT ELEMENT                                 
         BE    CM30                                                             
*                                  NO CURRENT ELEM FOR THIS SUFFIX              
         L     R3,A01ELEM          POINT TO LAST 01 FOUND                       
         LLC   RF,1(R3)                                                         
         AR    R3,RF               BUMP PAST                                    
         ST    R3,A01ELEM          SET LOCATION FOR NEW ELEM                    
         B     CM40                                                             
*                                  COMPARE SCREEN MKT WITH THAT IN ELEM         
CM35     CLC   THISMKT,NDEFMNUM-NDEFEL01(R3)                                    
         BE    CM45                NO CHANGE                                    
* USER CANNOT CHANGE/REMOVE DEFINITION INCASE BUYS EXIST                        
***      CLI   TWAOFFC,C'*'        DDS TERMINAL? (DDS CAN TIDY RECS)            
***      BE    CM36                                                             
***      L     R2,AMKTFLD          RESET FIELD ADDRESS FOR CURSOR               
***      OC    THISMKT,THISMKT     CANNOT DELETE/CHANGE                         
***      BZ    DELERR                                                           
***      CLC   THISMKT,SPACES                                                   
***      BE    DELERR                                                           
***      B     CHAERR                                                           
*                                                                               
* REMOVE EXISTING DEFINITION ELEMENT                                            
* CANNOT DELETE OR CHANGE DEFINITION IF DEFINED AT LOWER LEVEL                  
CM36     CLC   MNTNTWK,=C'*DEF'    DEFAULT RECORD IS SPECIAL                    
         BE    CM37                                                             
         BAS   RE,MKTDEL           CHECK IF MKT DEFINED IN LOWER REC            
         BE    CM37                                                             
         L     R2,AMKTFLD          RESET FIELD ADDRESS FOR CURSOR               
         B     LVLERR              CANNOT DELETE/CHANGE                         
*                                                                               
CM37     MVC   THISPCT,NDEFPCT-NDEFEL01(R3)  HOLD ONTO PRORATION                
         OC    THISMKT,THISMKT     IF NOT REDEFINING MARKET NUMBER              
         BNZ   CM38                                                             
         CLC   THISPCT,=F'-1'      SUFFIX MUST BE SET AS 'NB' OR 0%             
         BE    CM38                ELSE WILL END UP WITH TOTAL <> 100%          
         CLC   THISPCT,=F'0'                                                    
         BE    CM38                                                             
         L     R2,AMKTFLD          RESET FIELD ADDRESS FOR CURSOR               
         LH    R1,NDEFMNUM-NDEFEL01(R3)  RESHOW MARKET CODE                     
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(4,R2),DUB                                                      
         OI    6(R2),X'80'                                                      
         MVI   5(R2),4             SET LENGTH FOR VALIMKT                       
         MVI   4(R2),X'08'         SET NUMERIC FOR VALIMKT                      
         B     PCTERR              CANNOT DELETE/CHANGE                         
*                                                                               
CM38     BAS   RE,TST04            SEE IF WE ADDED CM REQ TODAY                 
         BRAS  RE,VOLDMKT          BUYS EXIST FOR THE OLD MKT?                  
         BNE   CM39                NO                                           
         OC    THISMKT,THISMKT     YES - REMOVING MARKET?                       
         BZ    MKT0ERR             YES - CANNOT REQUEST CM TO MKT 0!            
         BAS   RE,ADD04            ADD THE X'04' MKT CHG ELEMENT                
         BRAS  RE,ADDCM            ADD A CM REQUEST IN STORAGE                  
*                                                                               
CM39     GOTO1 RECUP,DMCB,(0,AIO),(R3)       REMOVE AN ELEMENT                  
         OI    EL01FLAG,EL01DELQ   INDICATE DELETED DEFINITION                  
*                                                                               
* ADD NEW DEFINITION ELEMENT                                                    
*                                                                               
CM40     OC    THISMKT,THISMKT     ANY DEFINITION THIS TIME                     
         BZ    CM50                                                             
*                                                                               
         LA    RF,ELEM             BUILD A DEFINITION (01) ELEMENT              
         USING NDEFEL01,RF                                                      
         XC    ELEM,ELEM                                                        
         MVI   ELEM,NDEFELQ        X'01'                                        
         MVI   ELEM+1,ELEMLEN                                                   
         MVC   NDEFMSUF,THISSUFX                                                
         MVC   NDEFMNUM,THISMKT                                                 
         MVC   NDEFAMKT,THISAMKT                                                
* ADD WITH PCT SET TO N/B - FORCING USER TO CHANGE, ELSE THEY WILL              
* JUST LEAVE AS 0 AND MAKE LOADS OF 'EMPTY' BUYS FILLING FILE AND               
* PREVENTING SOON REQUESTS.                                                     
* 18AUG04 - EXCEPT WHEN CHANGE A MARKET NUMBER WHEN USE ORIG PCT!               
*        MVC   NDEFPCT,=F'-1'                                                   
         MVC   NDEFPCT,THISPCT                                                  
         DROP  RF                                                               
*                                                                               
*        CLC   MNTNTWK,=C'*DEF'    DEFAULT RECORD IS SPECIAL                    
*        BNE   CM42                                                             
*        GOTO1 ADDELEM             ADD IN SEQUENCE                              
*        B     CM43                                                             
CM42     L     R3,A01ELEM          (RE)ADD IN POSITION                          
         GOTO1 RECUP,DMCB,(0,AIO),ELEM,(R3)   ADD AN ELEMENT                    
CM43     OI    EL01FLAG,EL01ADDQ   INDICATE ADDED NEW DEFINITION                
CM45     OI    EL01FLAG,EL01GOTQ   INDICATE GOT A DEFINITION                    
*                                                                               
CM50     BAS   RE,NEXT             NEXT STATION                                 
         B     CM10                                                             
*                                                                               
CMKTEX   B     EXIT                                                             
         EJECT                                                                  
                                                                                
**********************************************************************          
*  NOTE: ONLY GET CALLED IF ADD CBLMKT / CHANGE (CONV) NETDEF>CBLDEF *          
*                                                                    *          
*  A) CHECK NETWORK IS APPROPRIATE FOR CONVERSION                    *          
*  - CANNOT HAVE ANY SPILL (ASN/CIII ESPECIALLY)                     *          
*  - CANNOT HAVE ANY SHOW STATION EXCEPTIONS                         *          
*  - CANNOT HAVE ANY LOCAL PST/HST << DONE IN CHKCONV - EASIER >>    *          
*                                                                    *          
*  B) CHECK NETWORK IS APPROPRIATE TO ADD AS SPECIALTY CABLE         *          
*  - CANNOT HAVE ANY SPILL (SPILL RECORDS ARE ADDED BEFORE NETDEF ETC*          
*    RECS DEFINED SO CANNOT CROSS CHECK STATUS AT TIME OF SPILL ADD) *          
*                                                                    *          
*  EXIT  - NOT EQUAL WITH ERROR MESSAGE SET - IF ERROR               *          
**********************************************************************          
                                                                                
CHKNETOK NTR1  ,                                                                
         MVC   SVADDR,DMDSKADD                                                  
         MVC   MYKEY,KEY                                                        
         MVC   AIO,AIO2                                                         
*                                  CHECK FOR SPILL                              
         L     R3,AIO1                                                          
         MVI   ELCODE,NDEFELQ      CHECK EVERY STATION IN REC                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CNET10   BAS   RE,NEXTEL                                                        
         BNE   CNET20                                                           
         LA    RF,KEY                                                           
         USING SDEFRECD,RF                                                      
         XC    KEY,KEY                                                          
         MVC   SDEFKTYP,=X'0D13'                                                
         MVC   SDEFKAGY,AGENCY                                                  
         MVI   SDEFKRSV,C'0'       CIS                                          
         MVC   SDEFKSTA(4),NDEFSTA-NDEFEL01(R3)                                 
         DROP  RF                                                               
         MVC   SAVEKEY,KEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(9),SAVEKEY                                                   
         BE    CNETERX                                                          
         MVI   SAVEKEY+(SDEFKRSV-SDEFRECD),C'1'  BBM                            
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(9),SAVEKEY                                                   
         BE    CNETERX                                                          
         B     CNET10                                                           
*                                                                               
CNET20   LA    RF,KEY              CHECK NETWORK ITSELF                         
         USING SDEFRECD,RF                                                      
         XC    KEY,KEY                                                          
         MVC   SDEFKTYP,=X'0D13'                                                
         MVC   SDEFKAGY,AGENCY                                                  
         MVI   SDEFKRSV,C'0'       CIS                                          
         MVC   SDEFKSTA(4),SVNTWK                                               
         MVC   SAVEKEY,KEY                                                      
         DROP  RF                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(9),SAVEKEY                                                   
         BE    CNETERX                                                          
         MVI   SAVEKEY+(SDEFKRSV-SDEFRECD),C'1'  BBM                            
         MVC   KEY,SAVEKEY                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(9),SAVEKEY                                                   
         BE    CNETERX                                                          
         CLI   ACTNUM,ACTADD                                                    
         BE    CNETOKX             CBLMKT ADD - SPILL IS ONLY CHECK             
*                                                                               
         LA    RF,KEY              CHECK SHOW RECORDS                           
         USING NPGMRECD,RF                                                      
         XC    KEY,KEY                                                          
         MVC   NPGMKTYP,=X'0D12'   NETWORK RECORD ID                            
         MVC   NPGMKAGY,AGENCY     AGENCY ID                                    
         MVC   NPGMKNET,SVNTWK     NETWORK                                      
         MVC   SAVEKEY,KEY                                                      
         DROP  RF                                                               
         GOTO1 HIGH                                                             
         B     CNET35                                                           
CNET30   GOTO1 SEQ                                                              
CNET35   CLC   KEY(8),SAVEKEY                                                   
         BNE   CNETOKX                                                          
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BE    CNETERX             HAS STATION EXCEPTION                        
         B     CNET30                                                           
*                                                                               
CNETOKX  CR    RB,RB                                                            
         B     CNETX                                                            
CNETERX  MVC   ERRNUM,=AL2(841)    'CANNOT CONVERT...'                          
         CLI   ACTNUM,ACTADD                                                    
         BNE   *+10                                                             
         MVC   ERRNUM,=AL2(842)    'HAS SPILL DEFINED'                          
         LTR   RB,RB                                                            
CNETX    MVC   AIO,AIO1            RESTORE                                      
         MVC   KEY,MYKEY                                                        
         MVC   DMDSKADD,SVADDR                                                  
         B     EXIT                EXIT WITH CC                                 
         EJECT                                                                  
                                                                                
**********************************************************************          
*  CHECK MARKET CONVERSIONS (NEW/CHANGED)                            *          
*  SCAN DOWN SCREEN AND DELETE/ADD ELEMENTS FOR ALL AVAILABLE        *          
*  MARKETS SHOWN ON THIS SCREEN/PAGE                                 *          
*                                                                    *          
*  ENTRY - RECORD TO UPDATE ADDRESSED BY AIO                         *          
*  EXIT  - RECORD UPDATED WITH DEFINITION ADDITIONS/DELETIONS/CHANGES*          
*        - EL01FLAG SET TO INDICATE 01 ELEM ACTIVITY FOR THIS PAGE   *          
**********************************************************************          
                                                                                
CHKCONV  NTR1  ,                                                                
         MVI   EL01FLAG,0                                                       
         MVC   LOCALPST,XFF        HIGHVALS FOR 1ST PASS                        
         MVI   LOCALGST,X'FF'                                                   
         LA    R2,MNTSUFXH         START POSITION                               
         LA    R6,MNTNAMLH         LIMIT                                        
*                                                                               
CC10     CR    R2,R6                                                            
         BNL   CCONEX              DONE SCREEN                                  
*                                                                               
         CLI   7(R2),0             CHECK FOR ZERO LENGTH SUFFIX                 
         BE    CCONEX              DONE ALL AVAIL MKTS ON SCREEN                
         MVC   THISSUFX,8(R2)                                                   
*        CLC   THISSUFX,SPACES                                                  
*        BE    CCONEX              DONE ALL AVAIL MKTS ON SCREEN                
*                                                                               
         BAS   RE,NEXT             BUMP TO MARKET                               
         ST    R2,AMKTFLD          SAVE A(MARKET FIELD) MAY NEED LATER          
         XC    THISMKT,THISMKT                                                  
         CLI   5(R2),0             CHECK FOR DEFINITION                         
         BNE   CC15                - YES                                        
         BAS   RE,NEXT             - NO, BUMP TO NAME                           
         CLI   ACTNUM,ACTADD                                                    
         BE    CC50                MUST HAVE INPUT FOR ADD                      
         B     CC20                NO/CLEARED INPUT (IMPLIED DELETE)            
*                                                                               
CC15     EQU   *                                                                
*>>>>>>>>                          MAY BE DISPLAY OF PREF DEFINED CONV          
*>>>>>>>>                          WILL NOW SHOW AS MARKET CODE                 
         TM    4(R2),X'08'         TEST VALID NUMERIC                           
         BZ    CC16                                                             
         BAS   RE,NEXT             BUMP TO NAME                                 
*>>>                               NEED MASTER REC AGAIN TO CHECK PST           
         MVC   TEMPNET,=XL8'0C00000000040000'                                   
         LA    RF,8(R2)                                                         
         MVC   TEMPNET+8(4),L'MNTNAME-4(RF)                                     
         LR    R0,R2                                                            
         LA    R2,TEMPNET                                                       
         MVI   USEIONUM,2                                                       
         GOTO1 VALISTA             VALIDATE STATION (GET MKT CODE/NAME)         
         LR    R2,R0                                                            
         MVI   USEIONUM,0          RESTORE AIO1                                 
         MVC   AIO,AIO1                                                         
         L     RF,AIO2             CHECK FOR LOCAL PST                          
         USING STAREC,RF                                                        
         CLC   LOCALPST,XFF                                                     
         BNE   *+10                                                             
         MVC   LOCALPST,SPST       SET FOR FIRST PASS                           
         CLC   LOCALPST,SPST                                                    
         BNE   TAXERR              PST MUST BE SAME AT ALL LEVELS               
         CLI   LOCALGST,X'FF'                                                   
         BNE   *+10                                                             
         MVC   LOCALGST,SGSTCODE   SET FOR FIRST PASS                           
         CLC   LOCALGST,SGSTCODE                                                
         BNE   TAXERR              GST MUST BE SAME AT ALL LEVELS               
*>>>                                                                            
         B     CC50                IGNORE THIS ONE                              
*>>>>>>>>                                                                       
CC16     OC    8(L'MNTMKT,R2),SPACES                                            
         MVC   THISSTA,8(R2)                                                    
         XC    MISSFIX,MISSFIX                                                  
         L     R3,AIO              FIND ANY ELEM FOR THIS STATION               
         MVI   ELCODE,NDEFELQ      CHECK STATION CURRENTLY HAS 01               
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CC17     BAS   RE,NEXTEL                                                        
         BE    CC18                                                             
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   NOTEX               ERROR - MUST EXIST IN NETDEF REC             
         CLC   CONVMSG(19),=C'STATIONS TO DEFINE '                              
         BE    NOTEX               ERROR - MUST EXIST IN NETDEF REC             
         MVI   MISSFIX,C'Y'                                                     
         B     *+14                CAN ADD MISSING ONES AT END                  
CC18     CLC   THISSTA,NDEFSTA-NDEFEL01(R3)                                     
         BNE   CC17                                                             
*                                                                               
         MVI   USEIONUM,2                                                       
         GOTO1 VALISTA             VALIDATE STATION (GET MKT CODE/NAME)         
         MVC   AIO,AIO1                                                         
         MVC   THISMKT,BMKT                                                     
*>>>>>>>>>>>>>>>>>>>>>>>>>                                                      
         L     RF,AIO2             CHECK FOR LOCAL PST                          
         USING STAREC,RF                                                        
         CLC   LOCALPST,XFF                                                     
         BNE   *+10                                                             
         MVC   LOCALPST,SPST       SET FOR FIRST PASS                           
         CLC   LOCALPST,SPST                                                    
         BNE   TAXERR              PST MUST BE SAME AT ALL LEVELS               
         OC    SCANTAX(4),SCANTAX                                               
         BNZ   TAXERR              C58 TAX/SERVICE FEE SET                      
         OC    SNEWTAX,SNEWTAX                                                  
         BNZ   TAXERR              TAX SET                                      
*        CLI   SGSTCODE,0          GST                                          
*        BNE   TAXERR                                                           
         CLI   LOCALGST,X'FF'                                                   
         BNE   *+10                                                             
         MVC   LOCALGST,SGSTCODE   SET FOR FIRST PASS                           
         CLC   LOCALGST,SGSTCODE                                                
         BNE   TAXERR              GST MUST BE SAME AT ALL LEVELS               
         DROP  RF                                                               
*>>>>>>>>>>>>>>>>>>>>>>>>>                                                      
*                                                                               
         ST    R2,SAVER2                                                        
         BAS   RE,NEXT             BUMP TO NAME                                 
         MVC   8(L'MNTNAME-5,R2),MKTNM  SHOW TRUE MARKET RECORD NAME            
         MVC   8+L'MNTNAME-4(4,R2),THISSTA AND ORIG STN CODE                    
*        MVC   8+L'MNTNAME-4(4,R2),QMKT    AND CORRESPONDING MKT CODE           
         OI    6(R2),X'80'                                                      
*                                                                               
* ALLOW DEFINE MKTS AGAINST > 1 SUFFIX (02OCT01 - PRE RELEASE)                  
* BUT NOT DUPLICATE DEFINE A NETDEF STATION !                                   
         L     R3,AIO              ENSURE NOT ALREADY DEFINED MKT               
         MVI   ELCODE,NDCONELQ                                                  
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CC19     BAS   RE,NEXTEL                                                        
         BNE   CC20                                                             
*        CLC   THISMKT,NDCONNUM-NDEFEL03(R3)                                    
         CLC   THISSTA,NDCONSTA-NDEFEL03(R3)                                    
         BNE   CC19                                                             
         L     R2,SAVER2                                                        
         B     DUPEX                                                            
*                                                                               
CC20     L     R3,AIO              INIT ADD AFTER POINT FOR FIRST 03 EL         
         MVI   ELCODE,NDEFNELQ                                                  
         BAS   RE,GETEL            02 ELEMENT                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    R3,A03ELEM          ADD AFTER 02 ELEM IF NO 03 ELS               
*>>>>>>>>>                                                                      
         BRAS  RE,SETSFX           CONVERT SUFFIX TO BINARY EQUIV               
         LHI   RF,SUFXTABN                                                      
CC21     CLC   THISSUFX,0(R1)                                                   
         BE    CC22                                                             
         LA    R1,6(R1)                                                         
         BCT   RF,CC21                                                          
         DC    H'0'                SUFFIX NOT IN TABLE                          
CC22     MVC   THISNWK,2(R1)       GET 1 BYTE BINARY NWK VALUE (C0-FD)          
*>>>>>>>>>                                                                      
*                                                                               
         L     R3,AIO              FIND ANY ELEM FOR THIS SUFFIX                
         MVI   ELCODE,NDCONELQ                                                  
         BAS   RE,GETEL            FIRST ELEMENT                                
         BNE   CC40                NO 03 ELEMS (ON ADD)                         
CC30     ST    R3,A03ELEM          SAVE A(THIS 03 ELEM)                         
         CLC   THISSUFX,NDCONSUF-NDEFEL03(R3)                                   
         BE    CC35                FOUND ELEMENT FOR SUFFIX                     
         BAS   RE,NEXTEL           NEXT ELEMENT                                 
         BE    CC30                                                             
*                                  NO CURRENT ELEM FOR THIS SUFFIX              
         L     R3,A03ELEM          POINT TO LAST 03 FOUND                       
         LLC   RF,1(R3)                                                         
         AR    R3,RF               BUMP PAST                                    
         ST    R3,A03ELEM          SET LOCATION FOR NEW ELEM                    
         B     CC40                                                             
*                                                                               
* ALWAYS REBUILD CONV ELEM INCASE MKT CODE FOR STATION HAS BEEN CHANGED         
*                                                                               
CC35     EQU   *                                                                
*        OC    THISMKT,THISMKT     ANY DEFINITION THIS TIME                     
*        BNZ   CC37                - YES, OK                                    
*                                                                               
* REMOVE EXISTING DEFINITION ELEMENT                                            
*                                                                               
         CLI   MISSFIX,C'Y'        IF ADDING STATIONS NOW MISSING FROM          
         BE    CC40                NETDEF, LEAVE ANY OTHER CONV DEF             
*                                                                               
CC37     GOTO1 RECUP,DMCB,(0,AIO),(R3)     REMOVE AN ELEMENT                    
*                                                                               
* ADD NEW DEFINITION ELEMENT                                                    
*                                                                               
CC40     OC    THISMKT,THISMKT     ANY DEFINITION THIS TIME                     
         BZ    CC50                                                             
*                                                                               
         LA    RF,ELEM             BUILD A CONVERSION (03) ELEMENT              
         USING NDEFEL03,RF                                                      
         XC    ELEM,ELEM                                                        
         MVI   ELEM,NDCONELQ                                                    
         MVI   ELEM+1,NDCONLNQ                                                  
         MVC   NDCONSTA,THISSTA                                                 
         MVC   NDCONSUF,THISSUFX                                                
         MVC   NDCONNUM,THISMKT                                                 
         MVC   NDCONNWK,THISNWK                                                 
         DROP  RF                                                               
*                                                                               
         GOTO1 ADDELEM             ADD IN SEQUENCE                              
*                                                                               
         L     RF,SAVER2                                                        
         MVC   8(4,RF),QMKT        REPLACE STN WITH MKT CODE                    
         OI    6(RF),X'80'                                                      
*                                                                               
CC50     BAS   RE,NEXT             NEXT STATION                                 
         B     CC10                                                             
*                                                                               
CCONEX   EQU   *                   CHECK LOCAL PST SAME AS NETWORK              
         CLC   LOCALPST,XFF                                                     
         BE    CCONEXIT            NO DEFINITIONS THIS PAGE                     
         OC    LOCALPST,LOCALPST                                                
         BZ    CCONEXIT            NO LOCAL EXCEPTIONS                          
         LA    R2,MNTNTWKH         GET NETWORK AGAIN                            
         GOTO1 ANY                 - REQUIRED                                   
         MVI   USEIONUM,2          - USE AIO2                                   
         GOTO1 VALISTA             VALIDATE STATION                             
         MVI   USEIONUM,0          RESTORE AIO1                                 
         MVC   AIO,AIO1                                                         
         L     RF,AIO2                                                          
         USING STAREC,RF                                                        
         CLC   LOCALPST,SPST       CHECK NETWORK PST AGAINST LOCAL              
         BNE   TAXERR              PST MUST BE SAME AT ALL LEVELS               
*                                                                               
CCONEXIT B     EXIT                                                             
         EJECT                                                                  
                                                                                
**********************************************************************          
* CHECK MARKET DEFINED AT A LOWER (CLIENT/ESTIMATE) LEVEL            *          
* IF SO THEN CANNOT DELETE AT HIGHER (NETWORK/CLIENT) LEVEL          *          
* NOR CHANGE AT HIGHER LEVEL (MUST CHANGE LOWER LEVELS FIRST)        *          
* 18JUN02 - ALLOW DELETE/CHANGE ON CLI SPECIFIC IF IT'S EST RECS     *          
*           HAVE SUFFIX AS N/B OR 0% - MEANS NO PRO-RATION AFFECTED  *          
*                                                                    *          
* ENTRY  THISMKT CONTAINS INTENDED NEW MARKET VALUE FOR THISSUFX     *          
* EXIT - CC NOT EQUAL IF EXISTS AT LOWER LEVEL                       *          
*                                                                    *          
* NOTE - ESTIMATE SPECIFIC RECORDS ARE ACTUALLY CBLPRO RECORDS ADDED *          
*        BY SPSFM60                                                  *          
**********************************************************************          
                                                                                
MKTDEL   NTR1  ,                                                                
         MVC   SVADDR,DMDSKADD                                                  
         MVC   KEY,MYKEY                                                        
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         B     MD30                                                             
*                                                                               
MD10     OC    MYKEY+8(2),MYKEY+8  CLIENT LEVEL?                                
         BZ    *+18                NO                                           
         CLC   KEY(10),MYKEY       CHECK ALL ESTIMATE RECORDS                   
         BNE   MDOKX                                                            
         B     *+14                                                             
         CLC   KEY(8),MYKEY        CHECK ALL CLIENT, EST RECORDS                
         BNE   MDOKX                                                            
         GOTO1 GETREC                                                           
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,NDEFELQ      X'01'                                        
         BAS   RE,GETEL                                                         
*                                                                               
MD20     CLC   THISSUFX,NDEFMSUF-NDEFEL01(R3) HAS MARKET BEEN FOUND?            
         BNE   MD25                                                             
         OC    THISMKT,THISMKT     YES                                          
         BZ    *+14                - INTENDED DELETE                            
         CLC   THISMKT,NDEFMNUM-NDEFEL01(R3)                                    
         BE    MD25                - NO CHANGE                                  
         OC    MYKEY+8(2),MYKEY+8  CLIENT LEVEL? DEL/CHA OK IF EST NB/0         
         BZ    MDERX               NO - CANNOT DELETE/CHANGE                    
         CLI   KEY+10,X'00'        DOUBLE CHECK LOOKING AT EST REC              
         BE    MDERX                                                            
         CLC   NDEFPCT-NDEFEL01(L'NDEFPCT,R3),=F'-1'                            
         BE    MD25                                                             
         CLC   NDEFPCT-NDEFEL01(L'NDEFPCT,R3),=F'0'                             
         BNE   MDERX                                                            
*                                                                               
MD25     BAS   RE,NEXTEL                                                        
         BE    MD20                                                             
*                                                                               
MD30     GOTO1 SEQ                                                              
         B     MD10                                                             
*                                                                               
MDOKX    CR    RB,RB                                                            
         B     *+6                                                              
MDERX    LTR   RB,RB                                                            
         MVC   AIO,AIO1            RESTORE                                      
         MVC   KEY,MYKEY                                                        
         MVC   DMDSKADD,SVADDR                                                  
         B     EXIT                EXIT WITH CC                                 
         EJECT                                                                  
                                                                                
**********************************************************************          
*  VALIDATE MARKET                                                   *          
*  ENTRY - R2=A(SCREEN MKT CODE)                                     *          
*  EXIT  - THISMKT = MARKET CODE NUMBER IN                           *          
*          THISAMKT = ASSOCIATED ALPHAMKT (FOR DEMO LOOKUP)          *          
**********************************************************************          
                                                                                
VALMKT   NTR1  ,                                                                
         MVI   USEIONUM,2          USE AIO2                                     
         GOTO1 VALIMKT             VALIDATE MARKET (SPSFM00 LEVEL)              
         MVC   THISMKT,BMKT                                                     
         CLC   QMKT,=C'0000'                                                    
         BE    ERRNETW             CAN'T ADD NETWORK AS A MARKET                
         L     RE,AIO2                                                          
         MVC   THISAMKT,MKTALST-MKTREC(RE)                                      
                                                                                
* ENSURE NOT ALREADY DEFINED MKT CODE AGAINST ANOTHER SUFFIX                    
                                                                                
         L     R3,AIO                                                           
         USING NDEFEL01,R3                                                      
* ALLOWED DEFINE CONV MKTS AGAINST > 1 SUFFIX (02OCT01 - PRE RELEASE)           
*        MVI   ELCODE,NDEFELQ                                                   
*        BAS   RE,GETEL                                                         
*        B     *+8                                                              
*M05     BAS   RE,NEXTEL                                                        
*        BNE   VM10                                                             
*        CLC   THISMKT,NDEFMNUM                                                 
*        BNE   VM05                                                             
*        B     DUPEX               ERROR - DUPLICATE MARKET CODE USE            
                                                                                
* CHECK MARKET SUFFIX DEFINED AT NETWORK LEVEL                                  
                                                                                
VM10     MVC   KEY,MYKEY           ORIGINAL KEY                                 
         OC    KEY+8(2),KEY+8      CLIENT LEVEL?                                
         BZ    VM30                NO - NETWORK                                 
         XC    KEY+8(3),KEY+8      CLEAR CLIENT/EST                             
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                GET NETWORK LEVEL RECORD INTO IO2            
         GOTO1 GETREC                                                           
*                                  CANNOT DEFINE IF NOT AT NWK LEVEL            
         L     R3,AIO                                                           
         MVI   ELCODE,NDEFELQ      X'01'                                        
         BAS   RE,GETEL                                                         
         B     *+8                                                              
VM20     BAS   RE,NEXTEL                                                        
         BNE   NOTEX               ERROR - NOT AT NETWORK LEVEL                 
         CLC   THISSUFX,NDEFMSUF                                                
         BNE   VM20                                                             
*NO!     CLC   BMKT,NDEFMNUM                                                    
*NO!     BNE   NOTEX               ERROR - DIFFERENT DEFINITION                 
         DROP  R3                                                               
*                                                                               
VM30     MVI   USEIONUM,0          RESTORE AIO1                                 
         MVC   AIO,AIO1                                                         
*                                                                               
VMEX     B     EXIT                                                             
         EJECT                                                                  
                                                                                
**********************************************************************          
*  DELETE RECORD                                                     *          
*  NOTE - NO PASSIVE POINTER RECORD FOR CBLMKT/CBLPRO RECORDS        *          
**********************************************************************          
                                                                                
DELREC   NTR1  ,                                                                
         LA    R2,MNTMKTH                                                       
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   INV                                                              
*                                  ENSURE NO LOWER LEVEL RECORDS                
         MVC   SVADDR,DMDSKADD                                                  
         MVC   KEY,MYKEY                                                        
         MVC   AIO,AIO2                                                         
         GOTO1 HIGH                                                             
         GOTO1 SEQ                 READ RECORD AFTER ONE WANT TO DEL            
         OC    MYKEY+8(2),MYKEY+8  DELETING CLIENT LEVEL?                       
         BZ    *+14                NO - CHECK FOR CLIENT RECORDS                
         CLC   KEY(10),MYKEY       YES- CHECK FOR ESTIMATE RECORDS              
         B     *+10                IF NWK/CLI SAME MUST BE EST REC              
         CLC   KEY(8),MYKEY        IF NWK SAME MUST BE CLI REC                  
         MVC   AIO,AIO1            RESTORE                                      
         MVC   KEY,MYKEY                                                        
         MVC   DMDSKADD,SVADDR                                                  
         BE    LVLERR                                                           
*                                                                               
DELREC10 MVC   AIO,AIO1                                                         
         L     R3,AIO                                                           
         USING NDEFRECD,R3                                                      
         OI    NDEFCNTL,X'80'      MARK FILE RECORD FOR DELETION                
         OI    KEY+13,X'80'        MARK DIR ACTIVE KEY FOR DELETION             
         GOTO1 WRITE                                                            
         GOTO1 PUTREC                                                           
         MVI   IOOPT,C'Y'          GENCON DOESN'T NEED TO DO ANYTHING           
         DROP  R3                                                               
DELRECX  B     EXIT                                                             
                                                                                
**********************************************************************          
*  LIST RECORDS                                                      *          
**********************************************************************          
                                                                                
LR       LA    R4,KEY                                                           
         USING NDEFRECD,R4                                                      
*                                                                               
         OI    GENSTAT2,DISTHSPG                                                
*                                                                               
         OC    KEY,KEY             IS IT FIRST TIME THROUGH                     
         BNZ   LR10                - NO                                         
         MVC   NDEFKTYP,=AL2(NDEFRECQ)     X'0D11'                              
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
         CLC   KEY+2(2),AGENCY     BASED ON PROG WRITTEN BY                     
         BNE   LREND               SOMEONE NOT TOO GOOD                         
*                                                                               
         CLI   LSTCLTH+5,0         SHOW ALL CLIENTS                             
         BE    LR25                                                             
         TM    LISTFLAG,CLSTARTQ   START AT FILTER?                             
         BO    LR24                - YES                                        
         CLC   NDEFKCLT,SVCLT                                                   
         BNE   LR30                PURE CLIENT FILTER                           
         B     LR25                                                             
LR24     CLC   NDEFKCLT,SVCLT                                                   
         BL    LR30                                                             
*                                                                               
LR25     EQU   *                                                                
         XC    LISTAR,LISTAR                                                    
         CLI   NDEFKEST,0          DON'T SHOW IF ESTIMATE IN KEY                
         BNE   LR30                CBLPRO RECORD NOT CBLMKT                     
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         L     R3,AIO              DON'T SHOW IF NETDEF RECORD                  
         MVI   ELCODE,NDEFNELQ     (UNLESS HAS CONVERSION)                      
         BAS   RE,GETEL                                                         
         BNE   LR30                                                             
         CLI   NDEFNET-NDEFEL02(R3),NDEFCABQ                                    
         BE    LR26                                                             
         CLC   CONREC(4),=C'CBLM'  DON'T SHOW ANYWAY IF CBLMKT RECTYPE          
         BE    LR30                                                             
         CLC   CONREC(4),=C'CBLD'  (SYNONYM)                                    
         BE    LR30                                                             
         L     R3,AIO              FIND ANY ELEM FOR THIS SUFFIX                
         MVI   ELCODE,NDCONELQ                                                  
         BAS   RE,GETEL            FIRST ELEMENT                                
         BNE   LR30                                                             
         MVC   LISTAR+40(17),=C'NETDEF CONVERSION'                              
         B     LR27                                                             
*                                                                               
LR26     CLC   CONREC(4),=C'CBLC'  DON'T SHOW CBLMKT IF CBLCONV RECTYPE         
         BE    LR30                                                             
*                                                                               
         CLI   TWAOFFC,C'*'        SHOW IF CONVERTED (DDS)                      
         BNE   LR27                                                             
         L     R3,AIO              SEE IF ANY CONVERSION ELEM                   
         MVI   ELCODE,NDCONELQ                                                  
         BAS   RE,GETEL                                                         
         BNE   LR27                                                             
         MVC   LISTAR+35(9),=C'CONVERTED'                                       
*                                                                               
*R27     XC    LISTAR,LISTAR                                                    
LR27     MVC   LISTNET,NDEFKNET    NETWORK                                      
         OC    NDEFKNET,NDEFKNET                                                
         BNZ   *+10                                                             
         MVC   LISTNET(4),=C'*DEF' DEFAULT RECORD IS SPECIAL                    
*                                                                               
         GOTO1 CLUNPK,DMCB,NDEFKCLT,QCLT  UNPACK CLIENT FOR DISPLAY             
         MVC   LISTCLT,QCLT        CLIENT UNPACKED FORMAT                       
*                                                                               
         L     R3,AIO             INDICATE IF NOT SOFTDEMO COMPATIBLE           
         MVI   ELCODE,NDEFELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
LR28     BAS   RE,NEXTEL                                                        
         BNE   LR29                                                             
         CLI   NDEFAMKT-NDEFEL01(R3),X'00'                                      
         BE    LR28                                                             
         B     *+10                                                             
*R29     MVC   LISTAR+50(19),=X'C485949640D389959240D596A340E285A3A497'         
*                                  MC   'DEMO LINK NOT SETUP'                   
LR29     MVC   LISTAR+45(24),=X'D59640C485949640D3969692A49740D98598A48>        
               5A2A38584'          MC   'NO DEMO LOOKUP REQUESTED'              
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
*  ADD 01 ELEM FOR DUMMY STATION ZZZZ (PROPORTIONATE REDISTRIBUTION) *          
*  NOTE - RECORD CANNOT JUST HAVE ZZZZ ELEMENT                       *          
**********************************************************************          
                                                                                
ADDZZ    NTR1  ,                                                                
         L     R3,AIO                                                           
         MVI   ELCODE,NDEFELQ      SEE IF ELEM EXISTS                           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE AT LEAST ONE 01 ELEM                 
*DDZZ10  ST    R3,A01ELEM          SAVE A(THIS 01 ELEM)                         
ADDZZ10  CLC   NDEFSTA-NDEFEL01(L'NDEFSTA,R3),=C'ZZZZ'                          
         BE    ADDZZX              ALREADY EXISTS                               
         BAS   RE,NEXTEL                                                        
         BE    ADDZZ10                                                          
*                                  NO CURRENT ZZZZ ELEM                         
         LA    RF,ELEM             BUILD A DEFINITION (01) ELEMENT              
         USING NDEFEL01,RF                                                      
         XC    ELEM,ELEM                                                        
         MVI   ELEM,NDEFELQ                                                     
         MVI   ELEM+1,ELEMLEN                                                   
         MVC   NDEFSTA,=C'ZZZZ'    SUFFIX & MARKET CODE ARE ZD'S                
         MVC   NDEFPCT,=F'-1'      PCT INITS TO N/B                             
         DROP  RF                                                               
*                                                                               
         GOTO1 ADDELEM             ADD IN SEQUENCE                              
*                                                                               
ADDZZX   B     EXIT                                                             
         EJECT                                                                  
                                                                                
**********************************************************************          
*  ADD 02 ELEMENT                                                    *          
*  NETWORK SEQUENCE NUMBER SET TO INDICATE CABLE                     *          
*  NOTE - NO PASSIVE POINTER RECORDS FOR CBLMKT/CBLPRO RECORDS       *          
**********************************************************************          
                                                                                
ADD02    NTR1  ,                                                                
         L     R3,AIO                                                           
         MVI   ELCODE,NDEFNELQ     SEE IF 02 ELEM EXISTS                        
         BAS   RE,GETEL                                                         
         BE    ADD02X                                                           
*                                                                               
         XC    ELEM(10),ELEM                                                    
         MVC   ELEM(2),=X'0203'                                                 
         MVI   ELEM+2,NDEFCABQ     SET CABLE FLAG IN 02 ELEM                    
         MVC   AIO,AIO1                                                         
         GOTO1 ADDELEM             ADD IN SEQUENCE                              
*                                                                               
ADD02X   B     EXIT                                                             
         EJECT                                                                  
                                                                                
**********************************************************************          
*  TST X'04' MARKET CHANGE ELEMENT                                   *          
**********************************************************************          
                                                                                
TST04    NTR1                                                                   
*                                                                               
         LR    R2,R3               SAVE A(X'01' ELEMENT WERE DELETING)          
         USING NDEFEL01,R2         X'01' ELEMENT DSECT                          
         GOTO1 DATCON,DMCB,(5,0),(2,HALF)                                       
*                                                                               
         L     R3,AIO              A(CBLDEF RECORD)                             
         MVI   ELCODE,NDMKTELQ     SEE IF ANY 04 ELEMENTS                       
         BAS   RE,GETEL            ANY X'04' ELEMENT?                           
         B     *+8                 GO TEST ELEMENT                              
*                                                                               
TST04A   BAS   RE,NEXTEL           HAVE A X'04' ELEMENT?                        
         BNE   EXIT                NO - EXIT                                    
         USING NDEFEL04,R3         X'04' ELEMENT DSECT                          
         CLC   NDMKTDAT,HALF       THIS X'04' ELEM ADDED TODAY?                 
         BNE   TST04A              NO                                           
         CLC   NDMKTSUF,NDEFMSUF   THIS SUFFIX CHANGED TODAY?                   
         BNE   TST04A              NO - KEEP IT AROUND                          
         B     ERRCM               YES - ERROR CM REQ ALREADY ADDED             
         DROP  R2,R3               DROP USINGS                                  
**********************************************************************          
*  ADD X'04' MARKET CHANGE ELEMENT                                   *          
**********************************************************************          
                                                                                
ADD04    NTR1                                                                   
*                                                                               
         LR    R2,R3               SAVE A(X'01' ELEMENT WERE DELETING)          
         USING NDEFEL01,R2         X'01' ELEMENT DSECT                          
         GOTO1 DATCON,DMCB,(5,0),(2,HALF)                                       
         GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
*                                                                               
ADD04A   L     R3,AIO              A(CBLDEF RECORD)                             
         MVI   ELCODE,NDMKTELQ     SEE IF ANY 04 ELEMENTS                       
         BAS   RE,GETEL            ANY X'04' ELEMENT?                           
         B     *+8                 GO TEST ELEMENT                              
*                                                                               
ADD04B   BAS   RE,NEXTEL           HAVE A X'04' ELEMENT?                        
         BNE   ADD04C              NO - GO AND ADD THE X'04' ELEMENT            
         USING NDEFEL04,R3         X'04' ELEMENT DSECT                          
*                                                                               
         GOTO1 DATCON,DMCB,(2,NDMKTDAT),(0,WORK+6)                              
*                                                                               
         GOTO1 PERVERT,DMCB,WORK+6,WORK,0                                       
*                                                                               
         XR    RF,RF               CLEAR RF                                     
         ICM   RF,3,8(R1)          NUMBER OF DAYS BETWEEN DATES                 
         CHI   RF,30               MORE THEN 30 DAYS AGO?                       
         BNH   ADD04B              NO - KEEP IT AROUND                          
         GOTO1 RECUP,DMCB,(0,AIO),(R3)  REMOVE THE ELEMENT                      
         B     ADD04A              GO TEST THE REST OF THE X'04' ELEMS          
         DROP  R3                  DROP X'04' ELEMENT USING                     
*                                                                               
ADD04C   XC    ELEM,ELEM           CLEAR THE ELEMENT                            
         LA    R1,ELEM             R1 = ELEM                                    
         USING NDEFEL04,R1         X'04' ELEMENT DSECT                          
         MVI   NDMKTEL,NDMKTELQ    X'04'                                        
         MVI   NDMKTLEN,NDMKTLNQ   ELEM LENGTH                                  
         MVC   NDMKTSUF,NDEFMSUF   SUFFIX                                       
         MVC   NDMKTOLD,NDEFMNUM   BINARY MARKET NUMBER                         
         MVC   NDMKTDAT,HALF       TODAYS DATE                                  
         MVC   AIO,AIO1            NETDEF RECORD                                
         GOTO1 ADDELEM             ADD THE X'04' ELEMENT                        
         B     EXIT                EXIT                                         
         DROP  R1,R2               DROP USINGS                                  
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
         CLC   =C'$DEL',MNTMKT     DDS DELETED THE RECORD?                      
         BE    EXIT                                                             
*                                                                               
         CLI   MODE,XRECPUT        JUST DID A PUTREC?                           
         BNE   REQREC4             NO                                           
*                                                                               
         MVI   ADDEDCM,C'N'        DID NOT ADD CM REQ YET                       
         LA    R4,CMREQ            CM REQUESTS                                  
         USING CMREQD,R4           CM REQUEST DSECT                             
         LA    R2,CMNUMREQ         # OF CM REQ WE CAN ADD PER SCREEN            
*                                                                               
REQREC1  OC    0(CMLENQ,R4),0(R4)  HAVE A CM REQUEST?                           
         BNZ   REQREC3             NO - EMPTY SLOT                              
REQREC2  LA    R4,CMLENQ(R4)       BUMP TO NEXT CM REQ SLOT                     
         BCT   R2,REQREC1          CHECK NEXT SLOT                              
         XC    CMREQ,CMREQ         CLEAR THE CM REQUEST AREA                    
         B     REQREC4             NO MORE CM REQUESTS TO ADD                   
*                                                                               
REQREC3  MVI   ADDEDCM,C'Y'        ADDED A CM REQUEST                           
         L     R3,AIO2             BUILD REQUEST HERE                           
         XC    0(26,R3),0(R3)      CLEAR THE REQUEST HEADER                     
         LA    R3,26(R3)           BUMP PAST HEADER TO REQUEST AREA             
         MVC   0(80,R3),SPACES     SPACE PAD QRECORD (IN SPREPWORKD)            
         MVC   0(2,R3),=C'CM'      PROGRAM (QCODE)                              
         MVC   2(2,R3),AGENCY      AGENCY  (QAGY)                               
         MVI   4(R3),C'C'          MEDIA   (QMED)                               
         MVC   5(3,R3),CMCLT       CLIENT  (QCLT)                               
         MVC   14(4,R3),CMOLDMKT   OLD MARKET (QMKT)                            
         MVC   18(4,R3),CMSTA      STATION (QSTA)                               
         MVC   49(4,R3),CMNEWMKT   NEW MARKET (QBOOK1)                          
         MVC   62(2,R3),CMSUFFIX   NEW MARKET (QOPT2)                           
         MVC   68(5,R3),=C'CMREQ'  CMREQ IS REQUEST ID (QUESTOR)                
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',AIO2,AIO2                     
         TM    8(R1),X'50'         ANY ERRORS?                                  
         BZ    REQREC2             NO - GO PROCESS NEXT CM REQUEST              
         DC    H'0'                YES - DEATH                                  
         DROP  R4                  DROP CMREQD USING                            
*                                                                               
REQREC4  L     R3,AIO2             BUILD REQUEST HERE                           
         XC    0(110,R3),0(R3)     CLEAR THE REQUEST RECORD                     
         MVI   10(R3),44           REQUEST HEADER - BINARY PROGRAM              
         MVI   14(R3),106                                                       
         MVC   26(80,R3),SPACES    SPACE PAD QRECORD (IN SPREPWORKD)            
         MVC   26(2,R3),=C'44'     PROGRAM (QCODE)                              
         MVC   28(2,R3),AGENCY     AGENCY  (QAGY)                               
         MVC   30(1,R3),QMED       MEDIA   (QMED)                               
         MVC   31(3,R3),=C'ALL'    CLIENT  (QCLT)                               
         OC    BCLT,BCLT                                                        
         BZ    *+10                                                             
         MVC   31(3,R3),QCLT                                                    
         MVC   40(4,R3),MYKEY+4    NETWORK IN MKT (QMKT)                        
*                                                                               
REQREC5  MVI   87(R3),C'N'                                                      
         MVC   94(7,R3),=C'CONTROL'                                             
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   *+12                                                             
         CLI   MODE,PRINTREP       DISABLE TURNAROUND                           
         BNE   REQREC10            WHILE CONVERTING AGENCY RECORDS              
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',AIO2,AIO2                     
         TM    8(R1),X'50'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
REQREC10 MVC   KEY,MYKEY                                                        
         GOTO1 HIGH                RESTORE ORIGINAL REC FOR DISPLAY             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
         TM    15(R3),X'80'                                                     
         BO    REQREC20                                                         
         CLI   MODE,PRINTREP       PRINTREP?                                    
         BE    REQREC20            YES                                          
         BAS   RE,ADDPTR           NO - ADD PASSIVE POINTER                     
*                                                                               
* HANDLE ANY OUTSTANDING PFKEYS                                                 
REQREC20 CLI   MODE,XRECADD        AFTER ADDREC                                 
         BE    *+12                                                             
         CLI   MODE,XRECPUT        AFTER PUTREC                                 
         BNE   REQRECX                                                          
         BRAS  RE,DOPFK                                                         
REQRECX  B     DR                                                               
         EJECT                                                                  
**********************************************************************          
*  ADD PASSIVE POINTER IF ADDING NEW NETWORK                         *          
**********************************************************************          
*                                                                               
ADDPTR   NTR1                                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(8),MYKEY        GET NETWORK LEVEL DA                         
         GOTO1 HIGH                                                             
         MVC   DSKAD,KEY+14        SAVE DISK ADDRESS                            
*                                                                               
         XC    KEY,KEY                                                          
         L     R3,AIO1                                                          
OLD      USING NDEFRECD,R3                                                      
         LA    R4,KEY                                                           
NEW      USING NDEFRECD,R4                                                      
*                                                                               
         MVI   NEW.NWKPTYP,NWKPTYPQ                                             
         MVI   NEW.NWKPSUB,NWKPSUBQ                                             
         MVC   NEW.NWKPAGY,OLD.NDEFKAGY          AGENCY ID                      
         MVC   NEW.NWKPNET,OLD.NDEFKNET          NETWORK                        
         MVC   KEY+14(L'DSKAD),DSKAD             DISK ADDRESS                   
         DROP  OLD                                                              
*                                                                               
         MVI   ELCODE,X'02'        FIND 02 ELEMENT                              
         BAS   RE,GETEL                                                         
         USING NDEFEL02,R3                                                      
         MVC   NEW.NWKPSEQ,NDEFNET               NETWORK NUMBER                 
         DROP  R3,NEW                                                           
*                                                                               
         OI    DMINBTS,X'88'       READ FOR DELETE AND UPDATE                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     EXIST?                                       
         BNE   ADDPTR10                                                         
         TM    KEY+13,X'80'        MARKED DELETED                               
         BNO   ADDPTRX             NO, THEN ALREADY EXISTS                      
         NI    KEY+13,X'FF'-X'80'                                               
         GOTO1 WRITE               ELSE UNDELETE                                
         B     ADDPTRX                                                          
*                                                                               
ADDPTR10 MVC   KEY,KEYSAVE         RESTORE THE KEY                              
         GOTO1 ADD                 ADD KEY                                      
*                                                                               
ADDPTRX  NI    DMINBTS,X'FF'-X'88'                                              
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*********************************************************************           
*  ERROR EXITS                                                                  
*********************************************************************           
                                                                                
RECF     MVC   ERRNUM,=AL2(RECFULL)                                             
         B     SPERREX                                                          
NOTEX    MVC   ERRNUM,=AL2(NOTEXIST)                                            
         B     SPERREX                                                          
DUPEX    MVC   ERRNUM,=AL2(245)                                                 
         B     SPERREX                                                          
LVLERR   MVC   ERRNUM,=AL2(STDELERR)                                            
         B     SPERREX                                                          
DELERR   MVC   ERRNUM,=AL2(1187)   CONTACT DDS TO REMOVE MARKET                 
         B     SPERREX                                                          
CHAERR   MVC   ERRNUM,=AL2(960)    MUST RUN A 'CM' - CONTACT DDS                
         B     SPERREX                                                          
ERRNETW  MVC   ERRNUM,=AL2(NETSTA)                                              
         B     SPERREX                                                          
TAXERR   MVC   ERRNUM,=AL2(844)    <<< FOR CONV - TAX DEFINED >>>               
         B     SPERREX                                                          
PCTERR   MVC   ERRNUM,=AL2(1252)   SUFFIX MUST BE NB IN CBLPRO TO DEL           
         B     SPERREX                                                          
MKT0ERR  L     R2,AMKTFLD          RESET FIELD ADDRESS FOR CURSOR               
         MVC   ERRNUM,=AL2(1374)   BUYS EXIST - CANT CHANGE TO MKT 0            
         B     SPERREX                                                          
ERRCM    L     R2,AMKTFLD          RESET FIELD ADDRESS FOR CURSOR               
         MVC   ERRNUM,=AL2(1376)   CM REQUEST ADDED - CANNOT CHANGE             
         B     SPERREX                                                          
CMREQMSG LA    R2,MNTNTWKH         RESET FIELD ADDRESS FOR CURSOR               
         MVC   ERRNUM,=AL2(1377)   CM REQUEST ADDED                             
         B     SPERREX                                                          
REQOV    MVC   ERRNUM,=AL2(REPGEN)                                              
         B     SPINFEX                                                          
*                                                                               
VCERR    LA    R2,MNTCLTH                                                       
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
SPERREX  LA    RF,GETTXTCB                                                      
         MVI   GTMTYP,GTMERR                                                    
         B     *+12                                                             
SPINFEX  LA    RF,GETTXTCB                                                      
         MVI   GTMTYP,GTMINF                                                    
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMSYS,2                                                         
         OI    GENSTAT2,USGETTXT                                                
         DROP  RF                                                               
*                                                                               
ERRX     GOTO1 ERREX                                                            
                                                                                
* ERROR EQUATES                                                                 
                                                                                
RECFULL  EQU   391                                                              
NOTEXIST EQU   392                                                              
STDELERR EQU   408                                                              
REPGEN   EQU   411                                                              
NETSTA   EQU   392          <TEMP> CAN NOT ADD NETWORK AS STATION               
         EJECT                                                                  
                                                                                
**********************************************************************          
*  GETEL ROUTINE                                                     *          
**********************************************************************          
                                                                                
         GETEL R3,DATADISP,ELCODE                                               
                                                                                
**********************************************************************          
*  LOCAL TABLES                                                      *          
**********************************************************************          
                                                                                
PFTABLE  DS    0H                                                               
*        CBLPRO MAINT DISPLAY                                                   
         DC   AL1(PF02X-*,02,PFTCPROG,(PF02X-PF02)/KEYLNQ,0)                    
         DC   CL3' '                   NOT SELECTABLE                           
         DC   CL8'CBLP'                RECORD: CBLPRO                           
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
*        SHOW(DIS)/UPDATE(CHA) ALPHA MARKETS FOR SOFT DEMOS                     
PF06     DC   AL1(PF06X-*,06,0,0,PFTRETRN)                                      
         DC   3C' ',8C' ',8C' '                                                 
PF06X    EQU  *                                                                 
*                                                                               
*        NEXT (HANDLED ENTIRELY IN OVERLAY NOT PASSED IN PFTABLE)               
PF11     EQU  *                           HERE AS A REMINDER                    
*                                                                               
*        RETURN CALLER                                                          
         DC    AL1(PF12X-*,12,PFTRPROG,0,0)                                     
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
         DC    X'FF'                                                            
                                                                                
         EJECT                                                                  
XFF      DC    X'FFFFFFFFFFFFFFFFFFFF'                                          
**********************************************************************          
*  LITERAL POOL AND LOCAL EQUATES                                    *          
**********************************************************************          
                                                                                
ELEMLEN  EQU   16                  ELEMENT LENGTH                               
* TRAP INCASE ADD MORE SUFFIXES - IF USER WERE TO DEFINE ALL THEN               
* THEY MAY NOT FIT ON CBLPRO SCREEN (MINUS 1 TO ACCOUNT FOR ZZZZ)               
CDEFMAXQ EQU   64-1                MAX #ENTRIES FIT ON CBLPRO SCRN              
PAGEMAXQ EQU   32                  MAX #MARKETS PER PAGE                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
* TABLE OF VALID 2CHR MARKET SUFFIXES                                           
* ORDERED TO APPEAR IN ALPHA SEQUENCE DOWN EACH SCREEN COLUMN                   
                                                                                
SUFFTAB  DS    0CL(SUFFTBLQ)                                                    
         DC    CL(L'SUFXCODE)'BA'      EST 0                                    
         DC    CL(L'SUFXNAME)'BARRIE'                                           
         DC    CL(L'SUFXCODE)'KT'      EST 0                                    
         DC    CL(L'SUFXNAME)'KITCHENER'                                        
*                                                                               
         DC    CL(L'SUFXCODE)'BR'      CST -1                                   
         DC    CL(L'SUFXNAME)'BRANDON'                                          
         DC    CL(L'SUFXCODE)'KL'      EST 0                                    
         DC    CL(L'SUFXNAME)'KITCHENER/LONDON'                                 
*                                                                               
         DC    CL(L'SUFXCODE)'CA'      MST -2                                   
         DC    CL(L'SUFXNAME)'CALGARY'                                          
         DC    CL(L'SUFXCODE)'KO'      PST -3                                   
         DC    CL(L'SUFXNAME)'KOOTENAY'                                         
*                                                                               
         DC    CL(L'SUFXCODE)'CE'      MST -2                                   
         DC    CL(L'SUFXNAME)'CALGARY/LETHBRIDGE'                               
         DC    CL(L'SUFXCODE)'LE'      MST -2                                   
         DC    CL(L'SUFXNAME)'LETHBRIDGE'                                       
*                                                                               
         DC    CL(L'SUFXCODE)'CL'      EST 0                                    
         DC    CL(L'SUFXNAME)'CARLETON'                                         
         DC    CL(L'SUFXCODE)'LL'      MST -2                                   
         DC    CL(L'SUFXNAME)'LLOYDMINSTER'                                     
*                                                                               
         DC    CL(L'SUFXCODE)'CH'      AST +1                                   
         DC    CL(L'SUFXNAME)'CHARLOTTETOWN'                                    
         DC    CL(L'SUFXCODE)'LO'      EST 0                                    
         DC    CL(L'SUFXNAME)'LONDON'                                           
*                                                                               
         DC    CL(L'SUFXCODE)'CJ'      EST 0                                    
         DC    CL(L'SUFXNAME)'CHICOUTIMI'                                       
         DC    CL(L'SUFXCODE)'ME'      MST -2                                   
         DC    CL(L'SUFXNAME)'MEDICINE HAT'                                     
*                                                                               
         DC    CL(L'SUFXCODE)'CR'      NST +1.5                                 
         DC    CL(L'SUFXNAME)'CORNER BROOK'                                     
         DC    CL(L'SUFXCODE)'MO'      EST 0                                    
         DC    CL(L'SUFXNAME)'MONTREAL ENG.'                                    
*                                                                               
         DC    CL(L'SUFXCODE)'DA'      PST -3                                   
         DC    CL(L'SUFXNAME)'DAWSON CREEK'                                     
         DC    CL(L'SUFXCODE)'MF'      EST 0                                    
         DC    CL(L'SUFXNAME)'MONTREAL FR.'                                     
*                                                                               
         DC    CL(L'SUFXCODE)'ED'      MST -2                                   
         DC    CL(L'SUFXNAME)'EDMONTON'                                         
         DC    CL(L'SUFXCODE)'NB'      EST 0                                    
         DC    CL(L'SUFXNAME)'NORTHBAY'                                         
*                                                                               
         DC    CL(L'SUFXCODE)'HA'      AST +1                                   
         DC    CL(L'SUFXNAME)'HALIFAX'                                          
         DC    CL(L'SUFXCODE)'OK'      PST -3                                   
         DC    CL(L'SUFXNAME)'OKANAGAN'                                         
*                                                                               
         DC    CL(L'SUFXCODE)'HM'      EST 0                                    
         DC    CL(L'SUFXNAME)'HAMILTON'                                         
         DC    CL(L'SUFXCODE)'OT'      EST 0                                    
         DC    CL(L'SUFXNAME)'OTTAWA/HULL ENG.'                                 
*                                                                               
         DC    CL(L'SUFXCODE)'KA'      PST -3                                   
         DC    CL(L'SUFXNAME)'KAMLOOPS'                                         
         DC    CL(L'SUFXCODE)'OF'      EST 0                                    
         DC    CL(L'SUFXNAME)'OTTAWA/HULL FR.'                                  
*                                                                               
         DC    CL(L'SUFXCODE)'KE'      PST -3                                   
         DC    CL(L'SUFXNAME)'KELOWNA' WAS 'OKANAGAN/KAMLOOPS'                  
         DC    CL(L'SUFXCODE)'PM'      EST 0                                    
         DC    CL(L'SUFXNAME)'PEMBROKE'                                         
*                                                                               
         DC    CL(L'SUFXCODE)'KN'      EST 0                                    
         DC    CL(L'SUFXNAME)'KENORA'                                           
         DC    CL(L'SUFXCODE)'PE'      EST 0                                    
         DC    CL(L'SUFXNAME)'PETERBOROUGH'                                     
*                                                                               
         DC    CL(L'SUFXCODE)'KI'      EST 0                                    
         DC    CL(L'SUFXNAME)'KINGSTON'                                         
         DC    CL(L'SUFXCODE)'PA'      CST -1                                   
         DC    CL(L'SUFXNAME)'PRINCE ALBERT'                                    
*>>>> NEW PAGE 2                                                                
SUFFTAB2 DC    CL(L'SUFXCODE)'PG'      PST -3                                   
         DC    CL(L'SUFXNAME)'PRINCE GEORGE'                                    
         DC    CL(L'SUFXCODE)'SS'      EST 0                                    
         DC    CL(L'SUFXNAME)'SUDBURY/TIMM./N.BAY/SSM'                          
*                                                                               
         DC    CL(L'SUFXCODE)'PK'      PST -3                                   
         DC    CL(L'SUFXNAME)'PR. GEORGE/TERR./KITIMAT'                         
         DC    CL(L'SUFXCODE)'SW'      CST -1                                   
         DC    CL(L'SUFXNAME)'SWIFT CURRENT'                                    
*                                                                               
         DC    CL(L'SUFXCODE)'QU'      EST 0                                    
         DC    CL(L'SUFXNAME)'QUEBEC CITY'                                      
         DC    CL(L'SUFXCODE)'SY'      AST +1                                   
         DC    CL(L'SUFXNAME)'SYDNEY'                                           
*                                                                               
         DC    CL(L'SUFXCODE)'RD'      MST -2                                   
         DC    CL(L'SUFXNAME)'RED DEER'                                         
         DC    CL(L'SUFXCODE)'TK'      PST -3                                   
         DC    CL(L'SUFXNAME)'TERRACE/KITIMAT'                                  
*                                                                               
         DC    CL(L'SUFXCODE)'RE'      CST -1                                   
         DC    CL(L'SUFXNAME)'REGINA/MOOSE JAW'                                 
         DC    CL(L'SUFXCODE)'TB'      EST 0                                    
         DC    CL(L'SUFXNAME)'THUNDER BAY'                                      
*                                                                               
         DC    CL(L'SUFXCODE)'RI'      EST 0                                    
         DC    CL(L'SUFXNAME)'RIMOUSKI/MATANE/SEPT ILS'                         
         DC    CL(L'SUFXCODE)'TM'      EST 0                                    
         DC    CL(L'SUFXNAME)'TIMMINS'                                          
*                                                                               
         DC    CL(L'SUFXCODE)'RO'      EST 0                                    
         DC    CL(L'SUFXNAME)'RIVIERE-DU-LOUP'                                  
         DC    CL(L'SUFXCODE)'TO'      EST 0                                    
         DC    CL(L'SUFXNAME)'TORONTO'                                          
*                                                                               
         DC    CL(L'SUFXCODE)'RN'      EST 0                                    
         DC    CL(L'SUFXNAME)'ROUYN/NORANDA'                                    
         DC    CL(L'SUFXCODE)'TR'      EST 0                                    
         DC    CL(L'SUFXNAME)'TROIS RIVIERES'                                   
*                                                                               
         DC    CL(L'SUFXCODE)'SM'      AST +1                                   
         DC    CL(L'SUFXNAME)'SAINT JOHN/MONCTON ENG.'                          
         DC    CL(L'SUFXCODE)'VA'      PST -3                                   
         DC    CL(L'SUFXNAME)'VANCOUVER'                                        
*                                                                               
         DC    CL(L'SUFXCODE)'SF'      AST +1                                   
         DC    CL(L'SUFXNAME)'SAINT JOHN/MONCTON FR.'                           
         DC    CL(L'SUFXCODE)'VI'      PST -3                                   
         DC    CL(L'SUFXNAME)'VICTORIA'                                         
*                                                                               
         DC    CL(L'SUFXCODE)'SA'      CST -1                                   
         DC    CL(L'SUFXNAME)'SASKATOON'                                        
         DC    CL(L'SUFXCODE)'WH'      PST -3                                   
         DC    CL(L'SUFXNAME)'WHITEHORSE'                                       
*                                                                               
         DC    CL(L'SUFXCODE)'ST'      EST 0                                    
         DC    CL(L'SUFXNAME)'SAULT STE. MARIE'                                 
         DC    CL(L'SUFXCODE)'WI'      EST 0                                    
         DC    CL(L'SUFXNAME)'WINDSOR'                                          
*                                                                               
         DC    CL(L'SUFXCODE)'SH'      EST 0                                    
         DC    CL(L'SUFXNAME)'SHERBROOKE'                                       
         DC    CL(L'SUFXCODE)'WP'      CST -1                                   
         DC    CL(L'SUFXNAME)'WINNIPEG'                                         
*                                                                               
         DC    CL(L'SUFXCODE)'SJ'      NST +1.5                                 
         DC    CL(L'SUFXNAME)'ST. JOHN''S'                                      
         DC    CL(L'SUFXCODE)'YE'      MST -2                                   
         DC    CL(L'SUFXNAME)'YELLOW KNIFE'                                     
*                                                                               
         DC    CL(L'SUFXCODE)'SU'      EST 0                                    
         DC    CL(L'SUFXNAME)'SUDBURY'                                          
         DC    CL(L'SUFXCODE)'YO'      CST -1                                   
         DC    CL(L'SUFXNAME)'YORKTON'                                          
*                                                                               
         DC    CL(L'SUFXCODE)'SN'      EST 0                                    
         DC    CL(L'SUFXNAME)'SUDBURY/TIMMINS/NTH BAY'                          
         DC    CL(L'SUFXCODE)'NE'      EST 0                                    
         DC    CL(L'SUFXNAME)'NATIONAL ENGLISH'                                 
*>>>> NEW PAGE 3                                                                
SUFFTAB3 DC    CL(L'SUFXCODE)'NF'      EST 0                                    
         DC    CL(L'SUFXNAME)'NATIONAL FRENCH'                                  
         DC    CL(L'SUFXCODE)'KP'      EST 0                                    
         DC    CL(L'SUFXNAME)'PRINCE GEORGE/KAMLOOPS'                           
*                                                                               
         DC    CL(L'SUFXCODE)'KW'      EST 0                                    
         DC    CL(L'SUFXNAME)'KELOWNA'                                          
SUFFNUMQ EQU   (*-SUFFTAB)/SUFFTBLQ    #ENTRIES                                 
         DC    X'FF'               EOT                                          
         EJECT                                                                  
***********************************************************************         
*  SETUP                                                              *         
***********************************************************************         
                                                                                
SETUP    NTR1  BASE=*,LABEL=*                                                   
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
* SET LINE24 PFKEY HELP TEXT                                                    
         MVC   MNTLN24,SPACES      CLEAR PFK TEXT                               
         OI    MNTLN24H+FHOID,FHOITR                                            
*                                                                               
         CLI   MNTNTWKH+5,4        CHECK FOR AGENCY LEVEL DEFAULT               
         BNE   SETUP10                                                          
*        CLC   MNTNTWK,=C'*DEF'    DEFAULT RECORD IS SPECIAL                    
*        BNE   SETUP10                                                          
*        CLI   PFKEY,2             CANNOT SWAP TO CBLPRO                        
*        BNE   *+8                                                              
*        MVI   PFKEY,X'FF'                                                      
*        B     *+10                                                             
*                                                                               
SETUP10  MVC   MNTLN24+12(L'PF05PAGE),PF05PAGE                                  
         CLI   THISPAGE,2                                                       
         BNE   *+8                                                              
         MVI   MNTLN24+12+L'PF05PAGE-1,C'3'                                     
         CLI   THISPAGE,3                                                       
         BNE   *+8                                                              
         MVI   MNTLN24+12+L'PF05PAGE-1,C'1'                                     
*                                                                               
* HANDLE PFKEY SPECIFICS                                                        
         MVC   MNTLN24(L'PF02CPRO),PF02CPRO                                     
         MVC   PF02ACT,SPACES                  SWAP TO SAME ACTION              
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    *+12                                                             
         CLI   ACTNUM,ACTSEL       ACTION SEL?                                  
         BNE   *+10                                                             
         MVC   PF02ACT(4),=CL4'DISP'           SWAP TO DISP ACTION              
*                                                                               
SETUP06  CLI   ACTNUM,ACTCHA                                                    
         BE    SETUP6A                                                          
         CLI   ACTNUM,ACTSEL                                                    
         BNE   SETUP12                                                          
         CLI   THISLSEL,C'C'                                                    
         BE    SETUP6A                                                          
         CLI   THISLSEL,X'10'      (GENCON 'CHASELQ')                           
         BNE   SETUP12                                                          
SETUP6A  MVC   MNTLN24+23(L'PF06UPDT),PF06UPDT                                  
*                                                                               
SETUP12  CLI   CALLSP,0            ANYTHING TO RETURN TO?                       
         BE    *+10                NO                                           
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
         LLC   RE,PFTLEN           BUMP TO NEXT ENTRY IN TABLE                  
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
SETUP80  GOTO1 INITPFKY,DMCB,PFTABLE      MAINT PF TABLE                        
         B     SETUPX                                                           
*                                                                               
SETUPXPF MVI   PFKEY,0             CLEAR PFKEY EXIT                             
SETUPX   XIT1  ,                                                                
*                                                                               
* ACTION ANY PFKEYS                                                             
DOPFK    NTR1  BASE=*,LABEL=*                                                   
         OC    PFKEY,PFKEY                                                      
         BZ    DOPFKX                                                           
         GOTO1 INITPFKY,DMCB,PFTABLE      MAINT PF TABLE                        
DOPFKX   XIT1  ,                                                                
*                                                                               
* PFKEY TEXT MIXCASE CONSTANTS                                                  
PF02CPRO DC    X'D7C6F27EC38293D79996'    PF2=CBLPRO                            
PF05PAGE DC    X'D7C6F57ED7818785F2'      PF5=PAGE2                             
*                                         PF6=UPDATE DEMO LINK                  
PF06UPDT DC    X'D7C6F67EE4978481A38540C485949640D3899592'                      
PF12RET  DC    X'D7C6F1F27ED985A3A49995'  PF12=RETURN                           
         LTORG                                                                  
         EJECT                                                                  
                                                                                
**********************************************************************          
* ROUTINE TO SET AN ALPHA MARKET                                                
**********************************************************************          
SETAMKT  NTR1  BASE=*,LABEL=*                                                   
         LA    R2,TEMPFLD                                                       
         XC    TEMPFLD,TEMPFLD                                                  
         LH    R1,NDEFMNUM-NDEFEL01(R3)                                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(4,R2),DUB                                                      
         OI    6(R2),X'80'                                                      
         MVI   5(R2),4             SET LENGTH FOR VALIMKT                       
         MVI   4(R2),X'08'         SET NUMERIC FOR VALIMKT                      
*                                  USE VALIDATOR TO GET MARKET RECORD           
         MVI   USEIONUM,2          USE AIO2                                     
         OI    TRNSTAT,NOVALERR                                                 
         GOTO1 VALIMKT             VALIDATE MARKET (SPSFM00 LEVEL)              
         MVI   USEIONUM,0          RESTORE AIO1                                 
         MVC   AIO,AIO1                                                         
         L     RE,AIO2                                                          
         OI    EL01FLAG,EL01ALPQ   INDICATE UPDATED ALPHA MKT                   
         MVC   NDEFAMKT-NDEFEL01(L'NDEFAMKT,R3),MKTALST-MKTREC(RE)              
         XIT1  ,                                                                
         LTORG                                                                  
**********************************************************************          
*  COPY FUNCTION - COPIES RECORD WHOSE KEY IS IN COPYKEY             *          
*  USER MUST HAVE PREVIOUSLY DISPLAYED THE RECORD THEY WANT TO COPY  *          
**********************************************************************          
COPYREC  NTR1  BASE=*,LABEL=*                                                   
         MVC   MYKEY,KEY                                                        
         MVC   AIO,AIO1                                                         
         L     R3,AIO                                                           
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         OI    DMINBTS,X'08'       MAY HAVE BE DELETED                          
         GOTO1 HIGH                CHECK FOR COPY TO RECORD                     
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
*                                                                               
         CLC   NDEFKEY(NDEFKCLT-NDEFRECD),COPYKEY   SAME NETWORK?               
         BE    COPYR30             YES - COPY AS IS                             
*                                                                               
         LA    R3,24(R3)           POINT TO FIRST ELEMENT                       
*                                                                               
COPYR20  CLI   0(R3),0             END OF RECORD?                               
         BE    COPYR30             YES - GO ADD IT                              
         CLI   0(R3),NDEFELQ       X'01' ELEMENT?                               
         BNE   *+14                NO                                           
         USING NDEFEL01,R3         X'01' ELEMENT USING                          
         MVC   NDEFPCT,=F'-1'      PCT INITS TO N/B                             
         B     COPYR25             GET NEXT ELEMENT                             
         DROP  R3                  DROP USING                                   
*                                                                               
         CLI   0(R3),NDEFNELQ      X'02' ELEMENT?                               
         BE    COPYR25             YES - KEEP IT                                
         GOTO1 RECUP,DMCB,(0,AIO),(R3)   REMOVE THE ELEMENT                     
         B     COPYR20             CHECK NEXT ELEMENT                           
*                                                                               
COPYR25  LLC   R0,1(R3)            L'ELEMENT                                    
         AR    R3,R0               BUMP TO NEXT ELEMENT                         
         B     COPYR20             GO DELETE THE REST OF THE ELEMENTS           
*                                                                               
COPYR30  GOTO1 ADDREC                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XIT1  ,                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*  19AUG04 - CONVERSION CODE MOVED HERE FOR ADDRESSABILITY PROBLEMS             
*  ALTHOUGH NOT ALLOWING CONVERSIONS ANYMORE!                                   
**********************************************************************          
**********************************************************************          
* SHOW MARKET / OLD CALL LETTERS FOR THIS PAGE                       *          
* NOTE - CALLED ON CHANGE ACTION OF NON CBLMKT/PRO RECORDS           *          
**********************************************************************          
                                                                                
SHOWCONV NTR1  BASE=*,LABEL=*                                                   
         LA    R2,MNTSUFXH                                                      
         LA    R6,PAGEMAXQ                                                      
         LA    R5,SUFFTAB                                                       
         CLI   THISPAGE,2                                                       
         BNE   *+8                                                              
         LA    R5,SUFFTAB2                                                      
         CLI   THISPAGE,3                                                       
         BNE   *+10                                                             
         LAY   R5,SUFFTAB3                                                      
         USING SUFFTABD,R5                                                      
*                                  --- SUFFIX ---                               
SC10     MVC   8(2,R2),SUFXCODE                                                 
         OI    6(R2),X'80'                                                      
*                                  --- MARKET ---                               
         JAS   RE,NEXT             GO TO NEXT FIELD                             
         XC    THISMKT,THISMKT                                                  
         L     R3,AIO              FIND ANY ELEM FOR THIS SUFFIX                
         MVI   ELCODE,NDCONELQ                                                  
         JAS   RE,GETEL            FIRST ELEMENT                                
         B     *+8                                                              
SC20     JAS   RE,NEXTEL           NEXT ELEMENT                                 
         BNE   SC30                NO DEFINITION                                
         CLC   SUFXCODE,NDCONSUF-NDEFEL03(R3)                                   
         BNE   SC20                                                             
*                                  SHOW MARKET CODE                             
         MVC   THISMKT,NDCONNUM-NDEFEL03(R3)                                    
         LH    R1,THISMKT                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(4,R2),DUB                                                      
         OI    6(R2),X'80'                                                      
         MVI   5(R2),4             SET LENGTH FOR VALIMKT                       
         MVI   4(R2),X'08'         SET NUMERIC FOR VALIMKT                      
*                                  PASS CODE THRU VALIDATOR FOR NAME            
         MVI   USEIONUM,2          USE AIO2                                     
         OI    TRNSTAT,NOVALERR                                                 
         GOTO1 VALIMKT             VALIDATE MARKET (SPSFM00 LEVEL)              
         MVI   USEIONUM,0          RESTORE AIO1                                 
         MVC   AIO,AIO1                                                         
*                                  --- NAME ---                                 
SC30     JAS   RE,NEXT             BUMP TO NEXT FIELD                           
         MVC   8(L'MNTNAME,R2),SPACES                                           
         OC    THISMKT,THISMKT                                                  
         BZ    SC35                                                             
         MVC   8(L'MNTNAME-5,R2),MKTNM                                          
         MVC   8+L'MNTNAME-4(4,R2),NDCONSTA-NDEFEL03(R3) ORIG STN CODE          
         B     *+10                                                             
SC35     MVC   8(L'MNTNAME,R2),SUFXNAME                                         
         OI    6(R2),X'80'                                                      
*                                                                               
         JAS   RE,NEXT                                                          
         LA    R5,SUFFTBLQ(R5)     NEXT AVAILABLE MARKET                        
         CLI   0(R5),X'FF'                                                      
         BE    SC40                EOT                                          
         BCT   R6,SC10                                                          
*                                                                               
SC40     EQU   *                                                                
SCEX     XIT1  ,                                                                
         DROP  R5                                                               
*                                                                               
VOLDMKT  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   SVADDR,DMDSKADD     SAVE OFF THE DISK ADDRESS                    
         MVC   MYKEY,KEY           SAVE OFF THE KEY                             
         LA    R2,MYKEY            R2 = NETDEF KEY                              
         USING NDEFKEY,R2          NETDEF RECORD DSECT                          
         USING NDEFEL01,R3         NETDEF X'01' ELEMENT DSECT                   
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R4,KEY              R4 = KEY                                     
         USING BUYRECD,R4          BUY RECORD DSECT                             
         MVC   BUYKAM,BAGYMD       A/M                                          
         NI    BUYKAM,X'F0'        STRIP MEDIA BITS                             
         OI    BUYKAM,X'03'        LOOK FOR MEDIA N BUYS                        
         MVC   BUYKCLT,NDEFKCLT    CLIENT (MAY BE X'0000')                      
         MVI   BUYKPRD,X'FF'       POL PRODUCT                                  
         XC    WORK,WORK           CLEAR WORK                                   
         MVC   WORK(4),=C'0000'    ALREADY HAVE MARKET                          
         MVC   WORK+4(4),NDEFKNET  NETWORK                                      
         MVI   WORK+8,C'N'         MEDIA N                                      
         MVC   WORK+9(2),NDEFMSUF  SUFFIX                                       
         MVI   QMED,C'N'           SWITCH TO MEDIA N                            
         GOTO1 MSPACK,DMCB,WORK,WORK+4,WORK+11                                  
         MVI   QMED,C'T'           SWITCH BACK TO MEDIA T                       
         MVC   WORK+11(2),NDEFMNUM OLD MARKET                                   
         MVC   BUYKMSTA,WORK+11    MKT/NETWORK/SUFFIX                           
*                                                                               
VOMK15   GOTO1 HIGH                READ HIGH                                    
*                                                                               
         CLC   BUYKAM,KEYSAVE      HAVE THE SAME A/M?                           
         BNE   VOMNO               NO - SET CC NEQ (BUY DOES NOT EXIST)         
         OC    NDEFKCLT,NDEFKCLT   HAVE A CLT SPECIFIC CBLDEF REC?              
         BZ    *+14                NO - CHECK ALL CLIENTS                       
         CLC   BUYKCLT,NDEFKCLT    HAVE THE SAME CLIENT?                        
         BNE   VOMNO               NO - SET CC NEQ (BUY DOES NOT EXIST)         
         CLI   BUYKPRD,X'FF'       POL PRODUCT?                                 
         BE    *+12                YES                                          
         MVI   BUYKPRD,X'FF'       FORCE POL PRODUCT                            
         B     VOMK15              AND READ HIGH                                
         CLC   BUYKMSTA,WORK+11    SAME MKT/STA?                                
         BE    VOMYES              YES - BUY EXISTS!                            
*                                                                               
         OC    NDEFKCLT,NDEFKCLT   HAVE A CLT SPECIFIC CBLDEF REC?              
         BNZ   VOMNO               YES - ALL CHECKED BUY DOES NOT EXIST         
         CLC   BUYKMSTA,WORK+11    BUY KEY MKT/STA < OLD MKT/STA                
         BL    *+14                YES                                          
         MVC   BUYKPRD(10),XFF     BUMP TO NEXT CLIENT                          
         B     VOMK15              AND GO READHIGH                              
         MVC   BUYKMSTA,WORK+11    BUMP KEY TO THE OLD MKT/STA                  
         XC    BUYKEST(4),BUYKEST  CLEAR THE ESTIMATE/BUYLINE                   
         B     VOMK15              AND GO READHIGH                              
*                                                                               
VOMNO    LTR   RB,RB               SET CC NEQ (BUY DOES NOT EXIST)              
         B     VOMKX               RESTORE KEY AND DISK ADDRESS                 
*                                                                               
VOMYES   CR    RB,RB               SET CC EQU (BUY DOES EXIST)                  
*                                                                               
VOMKX    MVC   KEY,MYKEY           RESTORE THE KEY                              
         MVC   DMDSKADD,SVADDR     RESTORE THE DISK ADDRESS                     
         J     EXIT                EXIT WITH CC                                 
         DROP  R2,R3,R4            DROP USINGS                                  
         LTORG                                                                  
*                                                                               
ADDCM    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,CMREQ            CM REQUESTS                                  
         USING CMREQD,R1           CM REQUEST DSECT                             
         LA    R2,CMNUMREQ         # OF CM REQ WE CAN ADD PER SCREEN            
*                                                                               
ADDCM10  OC    0(CMLENQ,R1),0(R1)  HAVE A CM REQUEST?                           
         BZ    ADDCM20             NO - EMPTY SLOT                              
         LA    R1,CMLENQ(R1)        BUMP TO NEXT CM REQ SLOT                    
         BCT   R2,ADDCM10          CHECK NEXT SLOT                              
         DC    H'0'                CM REQ AREA NEEDS EXPANSION                  
*                                                                               
ADDCM20  LA    R2,KEY              R2 = CBLDEF KEY                              
         USING NDEFRECD,R2         CBLDEF RECORD DSECT                          
         USING NDEFEL01,R3         NETDEF X'01' ELEMENT DSECT                   
         MVC   CMCLT,=C'ALL'       DEFAULT TO ALL CLIENTS                       
         OC    NDEFKCLT,NDEFKCLT   HAVE A CLIENT SPECIFIC CBLDEF REC?           
         BZ    *+10                NO                                           
         MVC   CMCLT,QCLT          YES - MOVE IN EBCEDIC CLIENT                 
         SR    R0,R0               CLEAR R0                                     
         ICM   R0,3,NDEFMNUM       OLD MARKET NUMBER                            
         CVD   R0,DUB              CONVERT TO DECIMAL                           
         OI    DUB+7,X'0F'         VALID EBCIDIC                                
         UNPK  CMOLDMKT,DUB        EBCEDIC MARKET                               
         MVC   CMSTA,NDEFKNET      STATION                                      
         ICM   R0,3,THISMKT        NEW MARKET NUMBER                            
         CVD   R0,DUB              CONVERT TO DECIMAL                           
         OI    DUB+7,X'0F'         VALID EBCIDIC                                
         UNPK  CMNEWMKT,DUB        EBCEDIC MARKET                               
         MVC   CMSUFFIX,NDEFMSUF   SUFFIX                                       
         J     EXIT                EXIT                                         
         DROP  R1,R2,R3            DROP USINGS                                  
* INCLUDE FOR CONVERSION                                                        
         DS    0D                                                               
       ++INCLUDE SPCNCBLTAB                                                     
**********************************************************************          
*  LOCAL DSECTS                                                      *          
**********************************************************************          
                                                                                
SUFFTABD DSECT                                                                  
SUFXCODE DS    CL2                                                              
SUFXNAME DS    CL24                                                             
SUFFTBLQ EQU   *-SUFFTABD                                                       
                                                                                
CMREQD   DSECT                                                                  
CMCLT    DS    CL3                 CLIENT                                       
CMOLDMKT DS    CL4                 OLD MARKET                                   
CMSTA    DS    CL4                 STATION                                      
CMNEWMKT DS    CL4                 NEW MARKET                                   
CMSUFFIX DS    CL2                 SUFFIX                                       
CMLENQ   EQU   *-CMREQD            CM DSECT LENGTH                              
                                                                                
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD               SPOOL DSECT                             
         EJECT                                                                  
       ++INCLUDE SPGENNDEF              NETWORK DEFINITION DSECT                
       ++INCLUDE SPGENMKT               MARKET DSECT                            
       ++INCLUDE SPGENSDEF        >>>>  SPILL DEF - FOR CONVERSION              
       ++INCLUDE SPGENNPGM        >>>>  SHOW DEF - FOR CONVERSION               
       ++INCLUDE SPGENSTA         >>>>  STATION MASTER - FOR CONVERSION         
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY               BUY RECORD DSECT                        
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
       ++INCLUDE SCSFM52D                                                       
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE SCSFM53D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
**********************************************************************          
*  LOCAL WORKING STORAGE                                                        
**********************************************************************          
         ORG   SYSSPARE                                                         
ERRNUM   DS    XL2                                                              
TEMPFLD  DS    CL12                WORK AREA                                    
TEMPNET  DS    CL12                WORK AREA                                    
MYKEY    DS    CL48                                                             
SAVEKEY  DS    XL48                                                             
SVNTWK   DS    CL4                                                              
SVADDR   DS    A                                                                
DSKAD    DS    XL4                                                              
SVCLT    DS    CL2                                                              
TEMPCLT  DS    CL4                                                              
LISTFLAG DS    X                                                                
CLSTARTQ EQU   X'80'               START AT CLIENT FILTER                       
EL01FLAG DS    X                   01 ELEMENT FLAG                              
EL01GOTQ EQU   X'80'               - AT LEAST 1 01 ELEM EXISTS                  
EL01DELQ EQU   X'40'               - AT LEAST 1 01 ELEM WAS DELETED             
EL01ADDQ EQU   X'20'               - AT LEAST 1 01 ELEM WAS ADDED               
EL01ALPQ EQU   X'10'               - AT LEAST 1 01 ELEM AMKT UPDATED            
EL01CHAQ EQU   EL01DELQ+EL01ADDQ+EL01ALPQ     - 01 ELEM CHANGES MADE            
THISSUFX DS    CL2                                                              
THISMKT  DS    XL2                                                              
THISAMKT DS    CL3                                                              
THISPAGE DS    X                                                                
AMKTFLD  DS    A                                                                
A01ELEM  DS    A                   A(01 ELEMENT IN RECORD)                      
CONVMSG  DS    CL80                <<< TEMP FOR CONV CODE >>>                   
A03ELEM  DS    A                   <<< TEMP FOR CONV CODE >>>                   
SAVER2   DS    A                   <<< TEMP FOR CONV CODE >>>                   
THISSTA  DS    CL4                 <<< TEMP FOR CONV CODE >>>                   
THISNWK  DS    X                   <<< TEMP FOR CONV CODE >>>                   
UNMATCHD DS    X                   <<< TEMP FOR CONV CODE >>>                   
MISSFIX  DS    X                   <<< TEMP FOR CONV CODE >>>                   
LOCALPST DS    XL10                                                             
LOCALGST DS    X                                                                
COPYKEY  DS    XL(L'NDEFKEY)                                                    
SELKEY   DS    XL(L'NDEFKEY)                                                    
THISPCT  DS    XL(L'NDEFPCT)                                                    
ADDEDCM  DS    CL1                 CM REQUEST FLAG                              
CMREQ    DS    XL(CMNUMREQ*CMLENQ) CM REQUEST AREA                              
CMNUMREQ EQU   10                  UP TO 10 CM REQUESTS                         
                                                                                
**********************************************************************          
*  REDEFINITION OF LIST LINE AT LISTAR                                          
**********************************************************************          
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LISTNET  DS    CL4                                                              
         DS    CL7                                                              
LISTCLT  DS    CL3                                                              
         DS    CL7                                                              
LISTEST  DS    CL3                                                              
         DS    CL2                                                              
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042SPSFM5F   12/05/13'                                      
         END                                                                    
