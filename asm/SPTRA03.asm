*          DATA SET SPTRA03    AT LEVEL 187 AS OF 01/14/20                      
*PHASE T21603C                                                                  
*INCLUDE DPTRD                                                                  
*                                                                               
*==================================================================             
*                                                                               
* AIO USAGE - AIO1 - VALI RTNS IN CONTROLLER (SPTRA00-T21600)                   
*             AIO1 - IN AIO FROM CONTROLLER AND HYPER CONTROLLER                
*                    (DDGENCON-T00A30)                                          
*             AIO2 - USED IN VCML TO READ COMMERCIAL RECS                       
*                    ALSO IN CHKDEL TO CHECK FOR DELETED CMML RECS              
*             AIO3 - REC SAVED FROM AIO1 IN VR (VALREC) AND RESTORED            
*                    TO AIO1 AT END OF VR RTN, ALSO USED FOR COMPARING          
*                    FOR CHANGES IN PUT FOR TURN AROUND REPORT                  
* REGISTER USAGE -                                                              
*        R0 - WORK REG                                                          
*        R1 - WORK REG                                                          
*        R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR                 
*        R3 - WORK REG                                                          
*        R4 - WORK REG & KEY DSECT POINTER                                      
*        R5 - WORK REG - IN LIST RTN POINTER TO LIST/PRINT LINE                 
*        R6 - USED FOR GETEL ELEMENT DSECT POINTER AND ALSO TO ELEM             
*              FOR DSECT IN VALREC                                              
*        R7 - SECOND BASE REG                                                   
*        R8 - POINTER TO SPOOLD                                                 
*        R9 - POINTER TO SYSD                                                   
*        RA - POINTER TO ATWA                                                   
*        RB - FIRST BASE                                                        
*        RC - POINTER TO GEND                                                   
*        RD - SAVE AREA POINTER                                                 
*        RE - GOTO1 REG                                                         
*        RF - GOTO1 REG                                                         
*                                                                               
*        PROGRAM LABELS MEANING:                                                
*        V PREFIX = VALIDATE                                                    
*        VALI ROUTINES ARE IN BASE (SPTR00-T21600)                              
*        F PREFIX = FIND                                                        
*        P PREFIX = PRINT/FORMAT FOR PRINT, DISPLAY, OR LIST                    
*                                                                               
*=====================================================================          
*                                                                               
*        ANY CHANGE TO CML'S OR ROTATIONS IN AN EXISTING PATTERN                
*        WITH THE DATED USED FIELD NON-ZERO WILL FORCE A NEW PATTERN            
*        RECORD TO BE ADDED TO THE FILE, WITH SUBLINE OF REF/SUB                
*        CHANGED.                                                               
*                                                                               
*=====================================================================          
         TITLE 'T21603 PATTERN REC DISPLAY, CHANGE, ADD, DELETE'                
                                                                                
*=====================================================================          
* LEV 127 SMUR JUN27/02 CLIENT STRING SECURITY                                  
* LEV 128 SMUR JAN27/04 SUPPORT 2 CHAR MARKET GROUP                             
* LEV 129 BGRI JUL28/04 SOX                                                     
* LEV 130 SMUR APR26/05 MARK STEXT ELEM INACTIVE WHEN DELETING PAT REC          
* LEV 131 SMUR AUG17/05 BUG FIX 1 PROD COVERED BY P/B CMLS                      
*         MHER FEB/07   SEPARATE SCREEN FOR COMMENTS                            
*              MAR/07   PFKEYS FOR PREVIOUS/NEXT VERSION                        
* LEV 139 SMUR NOV28/08 FIX DISAPPEARING ROTATION,P/B CMLS W/SAME PRDS          
*                       ALLOW ROTATION OF A=100%                                
* LEV 142 SMUR MAR06/08 DO NOT UNPROTECT PROTECTED FIELDS                       
* LEV 143 SMUR APR25/08 NEW BPAT RECORD - 50 COMMERCIALS PER RECORD             
*                       NO T/A REQUESTS FOR BPAT                                
* LEV 144 MHER JUL15/08 SUPPORT FOR ADID IN COMMERCIAL FIELD AND                
*                       COMMERCIALS ON NEW C1 SCREEN                            
* LEV 149 SMUR DEC10/08 BPAT FOR AGENCY ALPHA FR                                
* LEV 161 SMUR OCT13/09 ISSUE NO PREVIOUS PAT TO BE COPIED ERROR MSG            
* LEV 163 MHER OCT26/09 FIX BUG THAT DISPLAYS PREVIOUS VERSION OF A             
*                       RECORD ON LIST/SELECT WHEN A NEW SUBLINE GETS           
*                       ADDED. ADD A TRACE ROUTINE TRHIGH                       
*                       DO THIS ON MODE SETFILE-SET KEY AND GLOBDA              
* LEV 171 MNAS JUL07/10 OPEN BPAT TO AGENCY ALPHA H7                            
* LEV 172 SMUR AUG25/10 FIX ADID ON *DEL* AND *TIM* MESSAGE                     
* LEV 174 MNAS FEB16/11 PREVENT CHA/DEL OF DAYPART ONCE ADDED                   
*              CAN ONLY ADD A DAYPART CODE ON ACTION ADD                        
* LEV 177 MNAS JUL20/11 ADD ADD ADDITIONAL VALIDATION OF COMMLS FROM            
*                       BASE SCREEN (SOURCE RECORD INSTEAD OF SCREEN)           
* LEV 180 SMUR NOV07/11 OPEN BPAT TO AGENCY ALPHA OO                            
* LEV 181 MNAS JUN11/12 ALTERNATE TO DUMPING WHEN CLIENT IN KEY DOES            
*                       NOT EQUAL CLIENT ON SCREEN                              
* LEV 182 MNAS OCT23/12 MORE BANDS                                              
* LEV 183 SMUR APR28/14 ALLOW TIME ON PATTERNS WHEN TRFKING BY FLIGHT           
* LEV 184 SMUR APR28/15 TURN OFF PATTERN CHANGED IN OPTICA FLAG                 
* LEV 185 SMUR NOV03/15 FIX ABSURD PERCENTAGE DISPLAY. MISSING X'36'            
* SPEC-41651  SMUR 12/12/19 BUG FIX OVERLAPPING ERROR MESSAGE (20.1)            
*=====================================================================          
                                                                                
T21603   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21603**,R7,RR=R2                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA             BASE SCREEN FOR SYSTEM + THIS PROG           
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,SPTR03RR                                                      
         MVI   NLISTS,14           ONLY 14 LINES ON LIST SCREEN                 
         XC    SVREGS,SVREGS       CLEAR MULTI-LINE                             
*                                                                               
         OI    CONSERVH+1,X'01'    FORCE MODIFIED FOR PFKEYS                    
         OI    CONSERVH+6,X'80'                                                 
*                                                                               
         CLI   TRLSTPGM,X'03'                                                   
         BNE   *+12                                                             
         TM    CONRECH+4,X'20'     TEST REC FIELD HAS CHANGED                   
         BO    TRPAT00             NO - SO BEEN HERE BEFORE                     
*                                                                               
         MVI   TRLSTPGM,X'03'                                                   
         MVI   MYSCREEN,0                                                       
         XC    LATEST,LATEST       ELSE INITIALIZE THESE GUYS                   
         XC    MYSUBLIN,MYSUBLIN                                                
         XC    FILTERS,FILTERS                                                  
         XC    SVPRVDA,SVPRVDA                                                  
         XC    PRVPATDA,PRVPATDA                                                
         OI    CONRECH+4,X'20'     AND SET FLAG TO DETECT CHANGE                
*                                                                               
TRPAT00  CLI   MYSCREEN,0                                                       
         BNE   *+8                                                              
         MVI   MYSCREEN,X'F3'      INITIALIZE MYSCREEN                          
*                                                                               
         MVC   TRACMODE+4(1),MODE                                               
         MVC   TRACLACT+4(1),TWALACT                                            
         GOTO1 DATAMGR,DMCB,=C'DMTRACE',=C'DATA',TRACEIT                        
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000AFE'    GET ADDRESS OF TRPACK                  
         GOTO1 CALLOV,DMCB                                                      
         MVC   VTRPACK,0(R1)                                                    
*                                                                               
         CLI   MODE,SETFILE                                                     
         BNE   *+12                                                             
         BAS   RE,SETKEY                                                        
         B     EXIT                                                             
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BRAS  RE,DK                                                            
         B     EXIT                                                             
*                                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,XRECADD        AFTER ADD RECORD                             
         BE    AAR                                                              
         CLI   MODE,RECPUT         BEFORE REWRITING, CK IF REQ NEEDED           
         BE    PUT                                                              
         CLI   MODE,RECDEL         BEFORE DELETE RECORD (INVALID PAT)           
         BE    DELREC                                                           
         CLI   MODE,XRECPUT                                                     
         BE    EXIT                                                             
         CLI   MODE,PROCPFK                                                     
         BNE   EXIT                                                             
         LR    RE,RA                GET TWA ADDRESS                             
         AHI   RE,THISLSEL-T216FFD  POINT TO FIELD                              
         CLI   0(RE),C'C'           TEST THIS IS CHANGE ACTION                  
         BNE   DR                                                               
         BRAS  RE,SETKEY                                                        
         B     VR                                                               
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
TRACEIT  DC    AL1(TRACEITX-*)                                                  
TRACMODE DC    C'MODE= '                                                        
TRACLACT DC    C'LACT= '                                                        
TRACEITX EQU   *                                                                
         EJECT                                                                  
*=====================================================================          
* WHEN IN SELECT MODE AND CHANGING RECORDS, NEED TO CHECK TO SEE                
* IF A NEW SUBLINE HAS BEEN ADDED BY A PREVIOUS CHANGE.                         
* IF IT HAS, RETURN MOST RECENT KEY IN KEY AND SET GLOBDA FOR GENCON            
*=====================================================================          
                                                                                
SETKEY   NTR1                                                                   
         CLI   TWALACT,ACTSEL      TEST DOING SELECT                            
         BNE   EXIT                                                             
*                                                                               
         MVC   AIO,AIO1            SET IOAREA ADDRESS                           
                                                                                
* KEY MAY NOT HAVE BEEN MOVED YET, SO GET KEY VIA GETREC                        
                                                                                
         OC    KEY+14(4),KEY+14    IF NO DISK ADDRESS                           
         BZ    SETKEY2             ASSUME KEY IS THE RECORD KEY                 
*                                                                               
         GOTO1 GETREC                                                           
         L     RE,AIO1                                                          
         MVC   KEY(13),0(RE)         MOVE KEY                                   
*                                                                               
SETKEY2  NC    KEY+10(3),=X'FFFC00'  DROP SUBLINE                               
         LA    R0,10                                                            
         BRAS  RE,TRHIGH                                                        
         GOTO1 HIGH                READ LATEST VERSION                          
*                                                                               
         MVC   TWAKEYSV,KEY        SAVE NEW KEY JUST FOUND!                     
         MVI   MODE,UPDATEDA       TELL GENCON IT'S A NEW RECORD                
*                                                                               
         L     RE,AIO1                                                          
         CLC   KEY(13),KEYSAVE     IS IT THE RECORD WE'VE GOT                   
         BE    EXIT                                                             
*                                                                               
         GOTO1 GETREC              ELSE GET IT NOW                              
         B     EXIT                                                             
         EJECT                                                                  
*===========================================                                    
*     VALIDATE KEY ROUTINE                                                      
*===========================================                                    
                                                                                
VK       CLI   ACTNUM,ACTDEL       DELETE IS INVALID ACTION                     
         BE    DELREC                                                           
         CLI   ACTNUM,ACTADD       ALWAYS VALIDATE KEY ON ADD STUPIDO!          
         BE    GOVKEY                                                           
*                                                                               
         TM    TRAMEDH+4,X'20'     TEST ALL FIELDS PREV VALIDATED               
         BZ    GOVKEY                                                           
         TM    TRACLTH+4,X'20'                                                  
         BZ    GOVKEY                                                           
         TM    TRAPRLNH+4,X'20'                                                 
         BZ    GOVKEY                                                           
         TM    TRAPTLNH+4,X'20'                                                 
         BZ    GOVKEY                                                           
         TM    TRACODEH+4,X'20'    CODE                                         
         BZ    GOVKEY                                                           
         TM    TRAREFH+4,X'20'     REFERENCE NUMBER                             
         BZ    GOVKEY                                                           
         B     *+8                                                              
*                                                                               
GOVKEY   BRAS  RE,VKEY                                                          
         B     EXIT                                                             
         EJECT                                                                  
*==============================================================                 
* VALIDATE RECORD ROUTINE - IF REC CHANGED AND NEWSUBSW SET,                    
*                             MUST ADD PTTN WITH NEW SUBLINE                    
*==============================================================                 
                                                                                
VR       OC    MYSUBLIN,MYSUBLIN   TEST VERSIONING ACTIVE                       
         BNZ   DR                  IF IT IS, CAN'T CHANGE RECORD                
*                                                                               
         CLI   MYSCREEN,X'C1'      TEST PTTN CMML                               
         BE    VR02                                                             
*                                                                               
         CLI   MYSCREEN,X'5C'      TEST BPAT                                    
         BE    VR02                                                             
*                                                                               
         CLI   MYSCREEN,X'5D'      TEST COMMENTS                                
         BE    VR02                                                             
*                                                                               
         XC    TRAPTYP,TRAPTYP     CLEAR ADD/CHANGE DATA                        
         OI    TRAPTYPH+6,X'80'                                                 
                                                                                
VR02     LA    R3,2000             LENGTH FOR MOVE                              
         LR    RF,R3               FOR BOTH FIELDS                              
         L     RE,AIO3             TO                                           
         LR    R4,RE                                                            
         L     R2,AIO1             FROM                                         
         MVC   20(2,R2),AGENCY                                                  
         ST    RE,AIO                                                           
         MVCL  RE,R2                                                            
                                                                                
         NI    FLAGFTR,X'FF'-DELPATSW   INIT DELETE PATTERN                     
         NI    FLAGFTR,X'FF'-RESPATSW   INIT RESTORE PATTERN                    
         MVC   SVKEY,KEY           SAVE KEY                                     
                                                                                
         TM    SOXSW,SOXOKFLG      DDS & FACTEST?                               
         BO    VR10                                                             
*                                                                               
         TM    SOXSW,SOXERFLG      SOX ERROR?                                   
         BZ    VR10                                                             
*                                                                               
         GOTO1 VSOXERR                                                          
*                                                                               
* SEE IF LOCKET HAS THIS LOCKED OUT *                                           
*                                                                               
VR10     DS    0H                                                               
         MVC   DUB,=X'E3010A2200000000'                                         
         GOTO1 VALILOC,DMCB,0                                                   
*                                                                               
         CLI   MYSCREEN,X'5D'      TEST COMMENT SCREEN ACTIVE                   
         BE    VR50                                                             
         CLI   MYSCREEN,X'5C'      TEST BPAT CMML SCREEN                        
         BE    VR60                                                             
         CLI   MYSCREEN,X'C1'      TEST PTTN CMML SCREEN                        
         BE    VR60                                                             
*                                                                               
         USING PATKEY,R4                                                        
         CLC   PATKCLT,BCLT                                                     
         BE    VR12                                                             
         NI    TRACLTH+4,X'FF'-X'20'   TURN OFF VALIDATED PREVIOUSLY            
         OI    TRACLTH+4,X'80'         TURN O FIELD INPUT THIS TIME             
         B     EXIT                                                             
*                                                                               
VR12     MVC   BCLT(6),PATKCLT     CLT/PRD/SLN/PRD2/SLN2                        
         ICM   R0,7,PATKREF                                                     
         STCM  R0,7,BREFSUB                                                     
         SRDL  R0,10                                                            
         X     R0,=XL4'00003FFF'                                                
         STH   R0,BREF                                                          
         SRL   R1,22                                                            
         X     R1,=XL4'000003FF'                                                
         STH   R1,BSUB                                                          
         MVI   NEWSUBSW,0                                                       
*                                                                               
         LA    R2,TRADESCH         DESCRIPTION                                  
         GOTO1 ANY                                                              
         XC    ELEM,ELEM                                                        
         MVI   ELCODE,X'10'                                                     
         GOTO1 REMELEM                                                          
         LA    R6,ELEM                                                          
         USING PATDTAEL,R6                                                      
         MVI   PATDTAEL,X'10'      ELEMENT IDENTIFIER                           
         MVI   ELEM+1,PATDTAX-PATDTAEL ELEMENT LENGTH                           
         MVC   SVOLDDPT,PATDPT-PATDTAEL(R6)                                     
*                                                                               
         CLI   ACTNUM,ACTADD       IS THIS AN ADD                               
         BE    VR14                                                             
         EJECT                                                                  
* IF DELETE JUST DELETE, BYPASS ALL OTHER CKS *                                 
                                                                                
         LAY   RE,DELETE                                                        
         CLC   0(L'DELETE,RE),WORK  SOFT DELETE THIS PATTERN                    
         BNE   VR14                 NO                                          
         OI    FLAGFTR,DELPATSW     DELETE PATTERN REQUESTED                    
         OI    PATSTAT,X'80'        SET ON SOFT DELETE BIT                      
         GOTO1 ADDELEM                                                          
         B     VR80                                                             
                                                                                
* IF COPY CODE = ESTIMATE, CAN'T CHANGE NON-ESTIMATE PATTERNS *                 
                                                                                
VR14     CLI   SVPROF11,C'E'       COPY CODE = ESTIMATE                         
         BNE   *+12                 NO                                          
         CLI   PATKCODE,0          THIS BY ESTIMATE                             
         BE    NOESTER              NO                                          
                                                                                
         CLI   CODESW,C'Y'         THIS A COPY CODE=EST                         
         BNE   *+8                                                              
         OI    PATSTAT,X'10'                                                    
         DROP  R4                                                               
                                                                                
* SEE IF RESTORING A DELETED PATTERN *                                          
                                                                                
         CLC   =C'RESTORE',WORK    IS THIS A RESTORE SOFT DEL                   
         BNE   VR16                NO                                           
         OI    FLAGFTR,RESPATSW    RESTORE PATTERN REQUESTED                    
*                                                                               
         TM    PATSTAT,X'80'       WAS PAT SOFT DELETED                         
         BZ    RSTDELER            NO                                           
         NI    PATSTAT,X'FF'-X'80' SET OFF SOFT DELETE                          
         B     VR18                                                             
VR16     MVC   PATDESC,WORK                                                     
                                                                                
* IF MAINTAINING AN AUTO P/B PATTERN, MAKE IT MANUAL *                          
                                                                                
VR18     TM    PATSTAT,X'40'       AUTO P/B PATTERN                             
         BZ    VR20                                                             
         NI    PATSTAT,X'FF'-X'40'                                              
         CLC   PATDESC,=CL16'AUTO P/B PATTERN'                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PATDESC,=CL16'AUTO P/B NOW MAN'                                  
*                                                                               
         OI    PATSTAT,X'20'       SET ON WAS AUTO P/B PATTERN                  
*                                                                               
VR20     LA    R2,TRAINVPH         INVERTED PROD/CMML FIELD                     
         CLI   8(R2),0             ANY INPUT?                                   
         BNE   *+12                TO NEXT COMPARE                              
         MVI   8(R2),C'N'                                                       
         B     VR33                                                             
         CLI   8(R2),C'N'                                                       
         BE    VR33                GO TO NEXT VALIDATION                        
         CLI   8(R2),C'Y'                                                       
         BNE   INVALERR                                                         
         CLI   BPRD2,0             WAS THERE PARTNER PROD                       
         BE    INVPTNER                                                         
*                                                                               
VR30     OI    PATSTAT,X'04'       INDICATE YES FOR INVERTED                    
         B     *+8                                                              
VR33     NI    PATSTAT,X'FB'       TURN OFF INVERTED ONLY                       
         OI    TRAINVPH+6,X'80'    TRANSMIT                                     
*                                                                               
VR35     LA    R2,TRAPERH          PERIOD                                       
         BRAS  RE,VPER                                                          
*                                                                               
         CLI   CONREC,C'B'         IS THIS BPAT                                 
         BNE   *+8                                                              
         OI    PATSTAT1,PATSBPAT   THIS IS BPAT RECORD                          
*                                                                               
         NI    PATSTAT1,X'FF'-PATOPCHG  TURN OFF PAT CHANGED IN OPTICA          
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         MVI   ELCODE,X'20'        MARKET/STATION LIST                          
         GOTO1 REMELEM                                                          
*                                                                               
         L     RF,AIO2                                                          
         AHI   RF,2000                                                          
         MVC   0(256,RF),ELEM      SAVE ELEM IN IO2+2000                        
*                                                                               
         LA    R2,TRAMS1H          MARKET/STATION                               
         BRAS  RE,VMS                                                           
         GOTO1 ADDELEM                                                          
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VR36                                                             
         SR    RE,RE                                                            
         IC    RE,ELEM+1                                                        
         L     RF,AIO2                                                          
         AHI   RF,2000                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(9,RF),ELEM                                                     
         BE    *+8                                                              
         MVI   NEWSUBSW,1          IF MKTLIST CHANGES, FORCE NEW SUBL           
*                                                                               
VR36     DS    0H                                                               
         CLI   CONREC,C'B'         IS THIS BPAT                                 
         BNE   VR36A                                                            
                                                                                
         LA    R2,TRADPTH                                                       
         CLI   5(R2),0                                                          
         JNE   INVALERR                                                         
         LA    R2,TRAINVPH                                                      
         CLI   8(R2),C'N'                                                       
         BE    *+12                                                             
         CLI   5(R2),0                                                          
         JNE   INVALERR                                                         
                                                                                
VR36A    XC    DUB,DUB                                                          
         LA    R2,TRASTIMH         PATTERN START TIME                           
         CLI   5(R2),0             TEST ENTERED                                 
         BNE   VR36B               YES                                          
         CLI   TRAETIMH+5,0        ELSE MUST BE NO END TIME                     
         JNE   MISSERR                                                          
         B     VR36X                                                            
*                                                                               
VR36B    BAS   RE,GOTIMVAL                                                      
         MVC   DUB(2),FULL                                                      
*                                                                               
         CLI   TRADPTH+5,0                                                      
         BNE   VR37ERR             ERROR, NO DAYPART AND TIME                   
*                                                                               
*NOP     CLI   SVPROF11,C'E'       MUST BE COPYCODE=EST                         
*****    BNE   VR37ERR                                                          
*                                                                               
         LA    R2,TRAETIMH         PATTERN END TIME                             
         CLC   ENDPAT,=X'FFFFFF'   TEST UFN PATTERN                             
         BNE   VR36C                                                            
         CLI   5(R2),0                                                          
         JNE   INVALERR                                                         
         B     VR36X                                                            
*                                                                               
VR36C    CLI   5(R2),0                                                          
         JE    MISSERR                                                          
         BAS   RE,GOTIMVAL                                                      
         MVC   DUB+2(2),FULL                                                    
         CLC   DUB+2(2),=H'2400'   MUST END BEFORE MIDNIGHT                     
         JNL   INVALERR                                                         
*                                                                               
VR36X    LA    R2,TRADLYH                                                       
         CLI   5(R2),0                                                          
         BE    VR37                                                             
         CLI   8(R2),C'N'                                                       
         BE    VR37                                                             
         CLI   8(R2),C'Y'                                                       
         JNE   INVALERR                                                         
         CLI   TRASTIMH+5,0        TEST TIME WAS INPUT                          
         JE    DLYTMER1            YES- DAILY TIMES REQUIRE TIME                
*                                                                               
VR37     LA    R2,TRADPTH                                                       
         CLI   5(R2),0             IF BOTH SCREEN AND RECORD                    
         BNE   VR37C               HAVE NO DAYPART SKIP VALIDATION              
         CLI   SVOLDDPT,0          ELSE CANNOT ADD OR DELETE DAYPART            
         BE    VR40                ONCE THE PATTERN HAS BEEN ADDED              
         B     VR37D                                                            
*                                                                               
VR37C    CLI   TRASTIMH+5,0        PATTERN START TIME                           
         BNE   VR37ERR             ERROR, NO DAYPART AND TIME                   
*                                                                               
         MVC   DUB+4(1),8(R2)      SAVE DAYPART                                 
*                                                                               
         CLI   ACTNUM,ACTADD       IS THIS AN ADD                               
         BE    VR37F                                                            
VR37D    MVC   GERROR,=Y(NOCHGDPT)                                              
*        CLI   SVOLDDPT,0          TEST NO OLD DPT                              
*        BE    VR37F                                                            
         CLC   SVOLDDPT,DUB+4      ELSE THEY CAN'T CHANGE IT                    
         BNE   VR37ERRX                                                         
*                                                                               
VR37F    CLI   SVPROF11,C'E'       TEST COPYCODE=EST                            
         BE    VR37X                                                            
*                                                                               
VR37ERR  MVC   GERROR,=Y(MUSTBEST)  ONLY ENTER DPT IF COPYCODE=EST              
*                                                                               
VR37ERRX GOTO1 VTRAERR                                                          
*                                                                               
VR37X    LA    R1,DMCB             GET DAYPART MENU                             
         MVC   0(2,R1),AGENCY                                                   
         MVC   2(1,R1),TRAMED                                                   
         MVC   3(1,R1),SVMENU                                                   
         GOTO1 =V(DPTRD),(R1),,AIO2,DATAMGR,RR=SPTR03RR                         
*                                                                               
         CLI   8(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         TM    8(R1),X'08'                                                      
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R0,35               MAX DPTS IN MENU                             
         L     R1,AIO2                                                          
*                                                                               
VR39     CLI   0(R1),0             TEST FOR EOT                                 
         BE    VR39ERR                                                          
         CLC   DUB+4(1),0(R1)                                                   
         BE    VR40                                                             
         LA    R1,5(R1)                                                         
         B     VR39                                                             
*                                                                               
VR39ERR  MVC   GERROR,=Y(BADDPT)                                                
         GOTO1 VTRAERR                                                          
*                                                                               
VR40     MVI   ELCODE,X'10'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   TRADLY,C'Y'         TEST DAILY TIMES                             
         BE    VR41                                                             
         CLC   PATSTART,PATEND     PATTERN FOR ONE DAY                          
         BNE   VR42                                                             
*                                                                               
VR41     LH    R0,DUB              YES - MUST START BEFORE IT ENDS              
         B     *+16      <-----    NOP FOR CALENDAR DAYS                        
         CHI   R0,599                                                           
         BH    *+8                                                              
         AHI   R0,2400             FOR CALENDAR DAYS                            
*                                                                               
         CHI   R0,2400             FOR CALENDAR DAYS, CHECK FOR 12M             
         BL    *+6                                                              
         XR    R0,R0               MAKE IT A LOW TIME                           
*                                                                               
         LH    R1,DUB+2                                                         
         B     *+16      <------   NOP FOR CALENDAR DAYS                        
         CHI   R1,599                                                           
         BH    *+8                                                              
         AHI   R1,2400                                                          
*                                                                               
         CR    R0,R1               TEST START BEFORE END                        
         BNH   VR42                                                             
         LA    R2,TRASTIMH                                                      
         MVC   GERROR,=Y(STENDTIM)                                              
         GOTO1 VTRAERR                                                          
*                                                                               
VR42     CLC   PATSTIM(4),DUB                                                   
         BNE   VR44                                                             
         CLC   PATDPT,DUB+4                                                     
         BE    VR46                                                             
VR44     MVI   NEWSUBSW,1          FORCE NEW SUBL IF NEW TIM OR DPT             
*                                                                               
VR46     MVC   PATSTIM(4),DUB                                                   
         MVC   PATDPT,DUB+4                                                     
*                                                                               
         NI    PATSTAT1,X'FF'-PATSDLY                                           
         CLI   TRADLY,C'Y'                                                      
         BNE   *+8                                                              
         OI    PATSTAT1,PATSDLY    SET DAILY FLAG                               
*                                                                               
         OC    PATSTIM(4),PATSTIM  TEST TIME ENTERED                            
         BZ    VR50                NO - GOOD                                    
         CLI   PATDPT,0            TEST DPT ENTERED                             
         BE    VR50                                                             
         MVC   GERROR,=Y(TIMORDPT)  DON'T ENTER BOTH                            
         GOTO1 VTRAERR                                                          
*                                                                               
GOTIMVAL NTR1                                                                   
         LLC   R0,5(R2)                                                         
         GOTO1 TIMVAL,DMCB,((R0),8(R2)),FULL                                    
         CLI   0(R1),X'FF'                                                      
         JE    INVALERR                                                         
         J     EQXIT                                                            
*                                                                               
VR50     CLI   ACTNUM,ACTADD                                                    
         BNE   VR54                                                             
         CLI   SVT3PROF,C'Y'       TEST TO COPY COMMENTS ON ADD                 
         BNE   VR54                                                             
         OC    SVPRVDA,SVPRVDA     TEST OK TO COPY PRV CLT                      
         BZ    VR54                                                             
*                                                                               
* GET THE PREVIOUS PATTERN RECORD                                               
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+14(4),SVPRVDA                                                
         L     R0,AIO              SAVE PREVIOUS                                
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         ST    R0,AIO              RESTORE PREVIOUS                             
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'40'        FIND COMMENTS                                
         BRAS  RE,GETEL                                                         
         BNE   VR54                                                             
*                                                                               
VR52     MVC   ELEM,0(R6)          MOVE TO ELEM                                 
         GOTO1 ADDELEM             AND COPY TO NEW RECORD                       
         BRAS  RE,NEXTEL                                                        
         BE    VR52                                                             
*                                                                               
VR54     CLI   MYSCREEN,X'5D'      TEST COMMENT SCREEN                          
         BNE   VR60                                                             
         BAS   RE,VCMT                                                          
         B     VR82                                                             
*                                                                               
VR60     CLI   MYSCREEN,X'5C'      TEST BPAT CMML SCREEN                        
         BNE   VR62                                                             
                                                                                
         CLC   =C'** COPY',PCOCMLA  TEST TO DISPLAY PREVIOUS CMMLS              
         BNE   VR61                                                             
         XC    KEY,KEY                                                          
         MVC   KEY+14(4),PRVPATDA                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC              GET THE PREVIOUS PATTERN                     
         MVI   DELFLAG,C'N'         DO NOT SHOW DELETED CMMLS                   
         BRAS  RE,BCMMLS            DISPLAY THE CMMLS                           
         MVC   KEY+14(4),GLOBDA    REREAD CURRENT RECORD                        
         GOTO1 GETREC                                                           
                                                                                
VR61     BRAS  RE,VBPAT                                                         
         B     VR82                                                             
*                                                                               
VR62     CLI   MYSCREEN,X'C1'      TEST PTTN/CMML SCREEN                        
         BNE   VR66                                                             
         CLC   =C'** COPY',TCMCMLA  TEST TO DISPLAY PREVIOUS CMMLS              
         BNE   VR64                                                             
                                                                                
         OC    PRVPATDA,PRVPATDA   ANY DISK ADDRESS FROM PREV PAT               
         BZ    NOCPYERR            NO, ERROR                                    
                                                                                
         XC    KEY,KEY                                                          
         MVC   KEY+14(4),PRVPATDA                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC              GET THE PREVIOUS PATTERN                     
         MVI   DELFLAG,C'N'         DO NOT SHOW DELETED CMMLS                   
         BRAS  RE,DCMMLS            DISPLAY THE CMMLS                           
*                                                                               
         MVC   KEY+14(4),GLOBDA    REREAD CURRENT RECORD                        
         GOTO1 GETREC                                                           
*                                   AND THEN VALIDATE CMMLS                     
VR64     MVI   DELFLAG,C'N'        DO NOT DISPLAY DELETED CMMLS                 
         BRAS  RE,VCMMLS                                                        
         MVC   PRVPATDA,GLOBDA     SAVE LAST PATTERN WITH CMMLS!                
         MVC   PRVPATMD,BAGYMD                                                  
         MVC   PRVPATCL,BCLT                                                    
         B     VR82                                                             
                                                                                
* CHECK FOR ANY OTHER PATTERNS FOR THIS CLT/PRD WITH SAME                       
* MARKET/STATION LIST AND OVERLAPPING DATES                                     
                                                                                
VR66     BAS   RE,CHKOV                                                         
                                                                                
*===================================================================            
* VALIDATE SPECIAL TEXT MUST BE DONE AFTER ANY POSSIBLE ERRORS                  
*===================================================================            
                                                                                
VR80     LA    R2,TRATXTH          TEXT - KEY TO SPECIAL TEXT RECORD            
         BAS   RE,VSTX             GO VALIDATE SPECIAL TEXT (IF ANY)            
         MVC   KEY,TWAKEYSV        RESTORE PATTERN KEY!                         
*                                                                               
VR82     TM    FLAGFTR,DELPATSW    DELETE PATTERN                               
         BO    VR94                                                             
*                                                                               
         CLI   SVPROF+9,C'R'       AUTO TURNAROUND REMOTE                       
         BE    VR84                YES                                          
         CLI   SVPROF+9,C'D'       AUTO TURNAROUND                              
         BE    VR84                NO                                           
         MVC   CHREASON,=C'NA'                                                  
         B     *+10                                                             
VR84     MVC   CHREASON,=C'TA'     SET UP AS MAINT ADD                          
         CLI   ACTNUM,ACTADD       IS THIS AN ADD                               
         BE    VR90                YES                                          
         MVI   CHREASON+1,C'C'                                                  
         CLI   NEWSUBSW,0          IS A NEW REC CALLED FOR                      
         BE    VR90                NO                                           
         L     R6,AIO3                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
         CLI   CONREC,C'B'         IS THIS BPAT                                 
         BNE   VR86                                                             
         OI    PATSTAT1,PATSADID   SET CMMLS ARE ADIDS                          
         CLI   ADIDFLAG,C'Y'                                                    
         BE    *+8                                                              
         NI    PATSTAT1,X'FF'-PATSADID  UNSET FLAG                              
*                                                                               
VR86     DS    0H                                                               
         OC    PATUSED,PATUSED     HAS THIS REC BEEN USED                       
         BZ    VR90                NO, NO NEW SUBLINE NEEDED                    
         XC    PATUSED,PATUSED     CLEAR USED DATE FLD                          
*                                                                               
         NI    PATSTAT,X'FF'-X'08'-X'02'-X'01' SET OFF COMML TEXT,              
*                                         PATTERN SPECIAL TEXT CHANGE,          
*                                         USED IN JWT BOOKLET FLAGS             
         LH    R1,BSUB             GET SUBLINE                                  
         LA    R1,1(,R1)           ADD 1                                        
         STH   R1,BSUB             AND SAVE                                     
         X     R1,=XL4'000003FF'   GET 1'S COMPLEMENT                           
         LH    R0,BREF             GET REF                                      
         X     R0,=XL4'00003FFF'   COMPLEMENTED                                 
         SLL   R1,22                                                            
         SLDL  R0,10               COMBINE WITH REF                             
         STCM  R0,7,BREFSUB                                                     
         L     R4,AIO3                                                          
         USING PATKEY,R4                                                        
         STCM  R0,7,PATKREF                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(13),PATKEY                                                   
         ST    R4,AIO                                                           
         GOTO1 ADDREC              NOW ADD REC WITH NEW SUBLINE                 
         BRAS  RE,GENR             NOW ADD REQ FOR T/A                          
         MVI   IOOPT,C'Y'          TELL CONTROLLER I/O IS DONE                  
         B     VR94                                                             
*                                                                               
VR90     MVC   KEY(L'SVKEY),SVKEY                                               
*                                                                               
         CLI   ACTNUM,ACTADD       IF THIS IS AN ADD, BYPASS                    
         BE    VR94                                                             
*                                                                               
         L     R6,AIO1             READ ORIGINAL RECORD                         
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         L     RE,AIO3                                                          
         CLC   13(2,RE),13(R6)     SAME LENGTH                                  
         BNE   VR92                 NO, MUST BE A CHANGE                        
         LR    R0,R6                                                            
         SR    R1,R1                                                            
         ICM   R1,3,13(RE)                                                      
         SH    R1,=H'20'           DON'T COMPARE ACTIVITY ELEM                  
         LR    RF,R1                                                            
         CLCL  RE,R0                                                            
         BE    VR94                                                             
*                                                                               
VR92     MVI   ELCODE,X'10'                                                     
         L     R6,AIO3                                                          
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
         NI    PATSTAT,X'FF'-X'02'  SET OFF PATTERN BOOKLET SW                  
*                                                                               
* MOVE RECORD BACK TO AIO1 *                                                    
*                                                                               
VR94     LA    R3,2000             LENGTH FOR MOVE                              
         LR    RF,R3               FOR BOTH FIELDS                              
         L     RE,AIO1             TO                                           
         ST    RE,AIO              RESTORE AIO ADDRESS                          
         L     R2,AIO3             FROM                                         
         MVCL  RE,R2                                                            
*                                                                               
         TM    FLAGFTR,DELPATSW    DELETE PATTERN REQUESTED                     
         BO    VR104                                                            
*                                                                               
* IF NO COMMERCIALS IN RECORD, FLAG RECORD INCOMPLETE                           
*                                                                               
         MVI   ELCODE,X'30'        PTTN CMML ELEMS                              
         CLI   CONREC,C'B'         TEST BPAT                                    
         BNE   VR96                NO                                           
         MVI   ELCODE,X'31'                                                     
*                                                                               
         L     R6,AIO                                                           
         CLI   ACTNUM,ACTADD       IF THIS IS NOT AN ADD                        
         BNE   *+12                THEN BPAT FLAG IS ALREADY ON                 
         OI    15(R6),X'01'        TURN ON BPAT RECORD                          
         OI    KEY+13,X'01'                                                     
*                                                                               
VR96     L     R6,AIO                                                           
         BRAS  RE,GETEL            COMMERCIAL LIST ELEMENT                      
         BE    VR100                                                            
*                                                                               
         MVI   MYPFKEY,3           FORCE THEM TO ADD CMMLS!                     
*                                                                               
* NO CML FOUND, RECORD IS INCOMPLETE                                            
*                                                                               
         TM    KEY+13,X'02'        IS INCOMPLETE FLAG ON                        
         BO    VR104               YES, BYPASS WRT                              
         L     R6,AIO                                                           
         OI    15(R6),X'02'        TURN ON INCOMPLETE RECORD                    
         OI    KEY+13,X'02'                                                     
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
         OI    PATSTAT1,PATSINC    INCOMPLETE STATUS IN ELEM                    
         B     VR102                                                            
*                                                                               
* CML FOUND, RECORD IS COMPLETE                                                 
*                                                                               
VR100    TM    KEY+13,X'02'        IS INCOMPLETE FLAG STILL ON                  
         BZ    VR104               NO, BYPASS WRT                               
         L     R6,AIO                                                           
         NI    15(R6),X'FF'-X'02'  TURN OFF INCOMPLETE RECORD                   
         NI    KEY+13,X'FF'-X'02'  AND IN KEY                                   
*                                                                               
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
         NI    PATSTAT1,X'FF'-PATSINC  RESET INCOMPLETE STAT IN ELEM            
         MVI   COMPFLAG,C'Y'           AND CHANGE LOCAL FLAG                    
*                                                                               
VR102    CLI   ACTNUM,ACTADD                                                    
         BE    VR104                                                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TRFDIR',KEY,KEY                        
*                                                                               
VR104    CLI   MODE,PROCPFK        FOR THIS MODE, NEED TO DO PUTREC             
         BNE   VR110                                                            
         CLI   NEWSUBSW,0          UNLESS ALREADY DONE                          
         BNE   VR110                                                            
         GOTO1 PUTREC              SO PUT RECORD NOW                            
         MVI   IOOPT,C'Y'          TELL CONTROLLER I/O IS DONE                  
*                                                                               
VR110    MVI   NEWSUBSW,0          SET OFF NEW SUBLINE NEEDED SW                
         MVC   SVKEY,KEY           THIS SEEMS TO FIX AAR DEATH!!!               
         B     DR                  NOW DISPLAY VALIDATED RECORD                 
         DROP  R4,R6                                                            
         EJECT                                                                  
*=============================================================                  
* DISPLAY RECORD                                                                
*                                                                               
* CURRENT SCREEN   PFKEY    NEW SCREEN                                          
*                                                                               
* PATTERN (F3)       2       5D COMMENTS                                        
*                    3       C1 COMMERCIALS                                     
*                  5/6/7     NEXT/PREV/CURRENT                                  
*                                                                               
* PTTN/COMMENT (5D)  2       F3 PATTERN                                         
*                    3       C1 COMMERCIALS                                     
*                                                                               
* PTTN/CMMLS (C1)    2       5D COMMENTS                                        
*                    3       F3 PATTERN                                         
*------------------------------------------------                               
* BPAT  (F3)         3       5C COMMERCIALS                                     
*                                                                               
* BPAT/COMMENT (5C)  2       F3 BPAT                                            
*=============================================================                  
                                                                                
* ENTRIES ARE RECORD (B/P), FROM SCREEN, PFKEY, TO SCREEN                       
*                                                                               
PFKEYTAB DC    X'D7F3025D'         PATT TO PATT/COMM                            
         DC    X'D7F303C1'         PATT TO PATT/CMML                            
         DC    X'D75D02F3'         PATT/COMM TO PATT                            
         DC    X'D75D03C1'         PATT/COMM TO PATT/CMML                       
         DC    X'D7C1025D'         PATT/CMML TO PATT/COMM                       
         DC    X'D7C103F3'         PATT/CMML TO PATT                            
         DC    X'C2F3035C'         BPAT TO BPAT/CMML                            
         DC    X'C25C03F3'         BPAT/CMML TO BPAT                            
         DC    X'C2F3025D'         BPAT TO BPAT/COMM                            
         DC    X'C25C025D'         BPAT/CMML TO BPAT/COMM                       
         DC    X'C25D035C'         BPAT/COMM TO BPAT/CMML                       
         DC    X'C25D02F3'         BPAT/CMML TO BPAT                            
PFKEYTBX DC    X'FF'                                                            
                                                                                
DR       OI    CONSERVH+1,X'01'    FORCE MODIFIED FOR PFKEYS                    
         OI    CONSERVH+6,X'80'                                                 
*                                                                               
         MVI   GENPFOPT,C'N'       RESET                                        
         CLI   THISLSEL,CHASELQ                                                 
         BNE   *+8                                                              
         MVI   GENPFOPT,C'Y'                                                    
*                                                                               
         CLI   PFKEY,0                                                          
         BE    DR20                                                             
         CLI   ACTNUM,ACTDIS                                                    
         BE    DR2                                                              
         CLI   ACTNUM,ACTSEL                                                    
         BE    DR2                                                              
         OC    MYSUBLIN,MYSUBLIN   IS CURRENT VERSION ACTIVE                    
         BZ    DR10                YES                                          
         XC    MYSUBLIN,MYSUBLIN   SET TO GET CURRENT VERSION                   
         B     DR4                                                              
*                                                                               
DR2      CLI   PFKEY,6             TEST PFKEY FOR PREV VERSION                  
         BE    DR4                                                              
         CLI   PFKEY,7             OR CURR VERSION                              
         BE    DR4                                                              
         CLI   PFKEY,5             OR FOR NEXT VERSION                          
         BNE   DR10                                                             
*                                                                               
DR4      CLI   CONREC,C'B'         TEST BPAT                                    
         BE    DR6                 YES                                          
         BRAS  RE,GETVRSN          FOR PTTN, GET THIS VRSN                      
         B     DR20                                                             
*                                                                               
DR6      MVI   PFKEY,0             NOT VALID PFKEY FOR BPAT                     
         B     DR14                GO RELOAD PRESENT SCREEN                     
*                                                                               
DR10     LA    R4,PFKEYTAB         TABLE OF PFKEYS                              
*                                                                               
DR12     CLC   CONREC(1),0(R4)     MATCH RECORD TYPE (P OR B)                   
         BNE   DR14                                                             
         CLC   MYSCREEN,1(R4)      MATCH CURRENT SCREEN                         
         BNE   DR14                                                             
         CLC   PFKEY,2(R4)         MATCH PFKEY                                  
         BE    DR16                                                             
*                                                                               
DR14     LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   DR12                                                             
         MVI   PFKEY,0             PFKEY IS INVALID - IGNORE IT                 
         B     DR20                                                             
*                                                                               
DR16     MVC   MYSCREEN,3(R4)      SET NEW SCREEN                               
*                                                                               
         BRAS  RE,GETSCR                                                        
         MVI   GENPFOPT,C'Y'       SET TO STAY ON THIS SELECTION                
*                                                                               
         OC    MYSUBLIN,MYSUBLIN   TEST CURRENT VERSION                         
         BZ    DR20                                                             
         IC    R0,PFKEY            SET PFKEY=0 TO FIX SCRN                      
         MVI   PFKEY,0                                                          
         BRAS  RE,GETVRSN          JUST PROTECTS/UNPROTECTS                     
         STC   R0,PFKEY            AND RESTORE PFKEY VALUE                      
*                                                                               
DR20     LA    R2,CONHEADH                                                      
         L     R6,AIO                                                           
*MH                                                                             
* ALWAYS READ RECORD IN CASE SUBLINE HAS CHANGED                                
         CLI   MODE,PROCPFK        NEED TO GET RECORD                           
         BE    *+12                                                             
         CLI   MODE,DISPREC        ONLY DO THIS FOR REAL DR CALL                
         BNE   DR21                                                             
*MH                                                                             
         MVC   KEY,TWAKEYSV                                                     
         NC    KEY+10(3),=X'FFFC00'     DROP SUBLINE                            
         OC    MYSUBLIN,MYSUBLIN   TEST VERSION DISPLAY ACTIVE                  
         BZ    DR20A                                                            
         SR    R1,R1                                                            
         ICM   R1,7,KEY+10                                                      
         N     R1,=X'00FFFC00'     DROP SUBLINE                                 
         LH    R0,MYSUBLIN                                                      
         X     R0,=X'000003FF'     COMPLEMENT                                   
         OR    R1,R0                                                            
         STCM  R1,7,KEY+10                                                      
*                                                                               
DR20A    LA    R0,1                                                             
         BRAS  RE,TRHIGH                                                        
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
DR21     L     R6,AIO                                                           
         MVI   ELCODE,X'10'        FIND STATUS ELEM                             
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   ADIDFLAG,C'Y'                                                    
         USING PATDTAEL,R6                                                      
         TM    PATSTAT1,PATSADID   TEST CMMLS ARE ADIDS                         
         BO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
*                                                                               
         MVI   COMPFLAG,C'N'                                                    
         TM    PATSTAT1,PATSINC    TEST INCOMPLETE FLAG                         
         BO    DR21X               YES - INCOMPLETE                             
         MVI   COMPFLAG,C'Y'                                                    
         CLI   ACTNUM,ACTADD       IS THIS AN ADD                               
         BE    DR21X                                                            
         CLC   =X'0A22',KEY        IS IT A CMML KEY                             
         BNE   DR21X                                                            
         MVC   PRVPATDA,KEY+14     SAVE LAST PATTERN WITH CMMLS!                
         MVC   PRVPATMD,BAGYMD                                                  
         MVC   PRVPATCL,BCLT                                                    
         DROP  R6                                                               
*                                                                               
DR21X    CLI   MYSCREEN,X'5D'      TEST PTTN/COMMENT                            
         BE    DR60                                                             
         CLI   MYSCREEN,X'C1'      TEST PTTN/CMML                               
         BE    DR80                                                             
         CLI   MYSCREEN,X'5C'      TEST BPAT/CMML SCREEN                        
         BE    DR80                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE PATTERN DATA ELEMENT               
*                                                                               
         USING PATDTAEL,R6                                                      
         XC    WORK,WORK                                                        
         CLI   PATDTALN,38         IS THIS OLD REC                              
         BE    DR22                YES, NO SOFT DELETE                          
         TM    PATSTAT,X'80'       SOFT DELETE                                  
         BZ    DR22                                                             
         LAY   RE,DELMSG                                                        
         MVC   WORK(L'DELMSG),0(RE)                                             
*                                                                               
DR22     CLC   TRADEL,WORK                                                      
         BE    *+14                                                             
         MVC   TRADEL,WORK                                                      
         OI    TRADELH+6,X'80'                                                  
*                                                                               
         XC    TRADESC,TRADESC                                                  
         MVC   TRADESC(L'PATDESC),PATDESC                                       
         OI    TRADESCH+6,X'80'                                                 
*                                                                               
         MVI   TRAINVP,C'N'        DEFAULT                                      
         TM    PATSTAT,X'04'       IS INVERTED BIT ON?                          
         BZ    *+8                                                              
         MVI   TRAINVP,C'Y'                                                     
         OI    TRAINVPH+6,X'80'    TRANSMIT                                     
*                                                                               
         LA    R2,PATSTART                                                      
         BRAS  RE,PPER                                                          
         CLC   TRAPER,WORK                                                      
         BE    *+14                                                             
         MVC   TRAPER,WORK                                                      
         OI    TRAPERH+6,X'80'                                                  
         MVC   BYTE,PATSTAT        SAVE PATSTAT FOR A MOMENT                    
*                                                                               
         LA    R2,TRASTIMH                                                      
         LA    R4,PATSTIM                                                       
         BAS   RE,GOUNTIME                                                      
*                                                                               
         LA    R2,TRAETIMH                                                      
         LA    R4,PATETIM                                                       
         BAS   RE,GOUNTIME                                                      
*                                                                               
         LA    R2,TRADLYH                                                       
         MVI   8(R2),C'N'                                                       
         TM    PATSTAT1,PATSDLY                                                 
         BZ    *+8                                                              
         MVI   8(R2),C'Y'                                                       
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,TRADPTH                                                       
         MVC   8(1,R2),PATDPT                                                   
         OI    6(R2),X'80'                                                      
         B     DR23                                                             
*                                                                               
GOUNTIME NTR1                                                                   
         XC    8(5,R2),8(R2)                                                    
         OI    6(R2),X'80'                                                      
*                                                                               
         OC    0(2,R4),0(R4)       TEST NO TIME PRESENT                         
         JZ    EQXIT                                                            
*                                                                               
         XC    FULL,FULL                                                        
         MVC   FULL(2),0(R4)                                                    
         XC    WORK,WORK                                                        
         GOTO1 UNTIME,DMCB,FULL,WORK                                            
         MVC   8(5,R2),WORK                                                     
         J     EQXIT                                                            
*                                                                               
DR23     XC    TRAPTYP,TRAPTYP                                                  
         OI    TRAPTYPH+6,X'80'                                                 
*                                                                               
         LA    R4,TRAPTYP                                                       
*                                                                               
         L     R6,AIO              SEE IF ANY COMMENTS IN RECORD                
         MVI   ELCODE,X'40'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DR23A                                                            
         MVC   0(17,R4),=C'COMMENTS PRESENT!'                                   
         XC    1(15,R4),SPACES     MAKE LOWERCASE                               
         LA    R4,18(R4)                                                        
*                                                                               
DR23A    TM    BYTE,X'10'          BYTE HAS PATSTAT FROM ABOVE                  
         BZ    DR23B                                                            
         MVC   0(10,R4),=CL20'COPYCD=EST'                                       
         XC    1(5,R4),SPACES     MAKE LOWERCASE                                
         XC    8(2,R4),SPACES                                                   
         LA    R4,11(R4)                                                        
*                                                                               
DR23B    L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         MVI   BYTE,C' '           CLEAR                                        
         BRAS  RE,GETEL                                                         
         JNE   DR23C                                                            
         USING PATDTAEL,R6                                                      
         OC    PATUSED,PATUSED                                                  
         BZ    *+8                                                              
         MVI   BYTE,C'='           THIS FLAG SAYS PTTN USED                     
         DROP  R6                                                               
*                                                                               
DR23C    L     R6,AIO                                                           
         MVI   ELCODE,X'F1'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DR24                                                             
         USING ACTVD,R6                                                         
*                                                                               
         LA    R0,ACTVADDT                                                      
         OC    SVADDWHN,SVADDWHN   USE VERSION 1 ADD DATE                       
         BZ    *+8                 IF IT'S AROUND                               
         LA    R0,SVADDWHN                                                      
         MVC   0(5,R4),=C'ADDED'                                                
         XC    1(4,R4),SPACES     MAKE LOWERCASE                                
         MVC   5(1,R4),BYTE                                                     
         LA    R4,6(R4)                                                         
         GOTO1 DATCON,DMCB,(3,(R0)),(8,(R4))                                    
         LA    R4,9(R4)                                                         
*                                                                               
         LA    R0,ACTVCHDT                                                      
         OC    ACTVCHDT,ACTVCHDT    IF CHANGE DATE PRESENT                      
         BNZ   DR23D                USE IT                                      
         CLC   LATEST,=H'1'         ELSE CHECK THIS IS VRSN 1                   
         BNH   DR23E                WHICH MAY NOT HAVE A CHANGE DATE            
         LA    R0,ACTVADDT          SO ADD DATE IS CHANGE DATE                  
*                                                                               
DR23D    MVC   0(7,R4),=C'CHANGED'                                              
         XC    1(6,R4),SPACES      MAKE LOWERCASE                               
         MVC   7(1,R4),BYTE                                                     
         LA    R4,8(R4)                                                         
         GOTO1 DATCON,DMCB,(3,(R0)),(8,(R4))                                    
         LA    R4,9(R4)                                                         
*                                                                               
DR23E    CLI   1(R6),20            TEST ID IN ELEMENT                           
         BNH   DR24                                                             
         MVC   0(2,R4),=C'BY'                                                   
         XI    1(R4),X'40'                                                      
         MVC   3(8,R4),ACTVSCID                                                 
*                                                                               
DR24     XC    WORK(L'TRATXT),WORK                                              
         L     R6,AIO                                                           
         MVI   ELCODE,X'50'        SPECIAL TEXT POINTER                         
         BRAS  RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   WORK(6),3(R6)                                                    
         CLC   TRATXT,WORK                                                      
         BE    *+14                                                             
         MVC   TRATXT,WORK                                                      
         OI    TRATXTH+6,X'80'                                                  
*                                                                               
DR26     L     R6,AIO                                                           
         MVI   ELCODE,X'20'        MARKET/STATION LIST                          
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BRAS  RE,PMKST            PRINT MARKET/STATION LIST                    
*                                                                               
         CLI   MYSCREEN,X'5D'      TEST PATT/COMM SCREEN                        
         BE    DR80                                                             
         CLI   MYSCREEN,X'C1'      TEST PATT/CMML SCREEN                        
         BE    DR80                                                             
                                                                                
*===================================================================            
* FOR PATTERN RECORD, SHOW FIRST FOUR CMMLS AND ++++MORE IF NEEDED              
*    AND BPAT RECORD                                                            
*===================================================================            
                                                                                
         L     R6,AIO              COMMERCIAL LIST ELEMENT                      
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BE    DR30                                                             
* FOR BPAT, LOOK FOR 31 ELEM                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'31'                                                     
         BRAS  RE,GETEL                                                         
         BE    DR31                                                             
*                                                                               
         CLI   COMPFLAG,C'Y'       IS RECORD COMPLETE                           
         BNE   *+6                 NO - THAT'S OK                               
         DC    H'0'                ELSE BIG TROUBLE                             
*                                                                               
         LA    R2,TRACMLAH           SET TO CLEAR SCREEN                        
         LA    R3,4                                                             
         B     DR40                                                             
*                                                                               
         USING PATCMLEL,R6                                                      
DR30     CLI   MODE,DISPREC                                                     
         BNE   DR31                                                             
*                                                                               
DR31     LA    R2,TRACMLAH         1ST SCREEN FIELD                             
         LLC   R0,PATCMLLN         GET ELEM LEN                                 
         SRL   R0,4                DIV BY 16=NO OF CMML PRS (DROPS ODD)         
*                                                                               
         XC    TRACMOR,TRACMOR                                                  
         OI    TRACMORH+6,X'80'                                                 
         CHI   R0,4                                                             
         BNH   *+14                                                             
         LHI   R0,4                SET TO DISPLAY ONLY FIRST FOUR               
         MVC   TRACMOR,=CL10'++ MORE ++'                                        
*                                                                               
         LA    R5,PATCML           FIRST CMML                                   
         CLI   ELCODE,X'31'        TEST BPAT                                    
         BNE   *+8                                                              
         LA    R5,1(R5)                                                         
         LA    R3,4                MAX CMML PAIRS ON PTTN SCREEN                
         SR    R3,R0               # OF FLDS TO BLANK                           
*                                                                               
DR32     XC    8(25,R2),8(R2)                                                   
*                                                                               
         MVI   8(R2),C'*'                                                       
         CLC   =X'5C00',0(R5)                                                   
         BE    DR38                                                             
         MVC   8(8,R2),0(R5)                                                    
*                                                                               
         CLC   =C'HIATUS',0(R5)                                                 
         BE    DR38                                                             
*                                                                               
         OC    8(8,R5),8(R5)                                                    
         BZ    *+14                                                             
         MVI   16(R2),C'-'                                                      
         MVC   17(8,R2),8(R5)                                                   
*                                                                               
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   DR38                                                             
         XC    8(25,R2),8(R2)                                                   
         GOTO1 VTRPACK,DMCB,(C'U',0(R5)),8(R2)                                  
         OC    8(8,R5),8(R5)                                                    
         BZ    DR38                                                             
*                                                                               
         LA    R4,20(R2)                                                        
DR34     CLI   0(R4),C' '                                                       
         BH    DR36                                                             
         BCT   R4,DR34                                                          
*                                                                               
DR36     MVI   1(R4),C'-'                                                       
         GOTO1 VTRPACK,DMCB,(C'U',8(R5)),2(R4)                                  
*                                                                               
DR38     OI    6(R2),X'80'                                                      
         LA    R5,16(R5)                                                        
         AHI   R2,TRACMLBH-TRACMLAH                                             
         BCT   R0,DR32                                                          
*                                                                               
         LTR   R3,R3               ANY FLDS TO BLANK                            
         BZ    DR60                NO                                           
*                                                                               
DR40     XC    WORK,WORK                                                        
         CLC   8(L'TRACMLA,R2),WORK                                             
         BE    *+14                                                             
         MVC   8(L'TRACMLA,R2),WORK                                             
         OI    6(R2),X'80'                                                      
         LLC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         LLC   RE,0(R2)                                                         
         AR    R2,RE                                                            
         BCT   R3,DR40                                                          
         EJECT                                                                  
DR60     CLI   MYSCREEN,X'5D'      TEST PTTN/COMM SCREEN ACTIVE                 
         BNE   DR90                                                             
         BRAS  RE,BLDINFO                                                       
         XC    PCMINFO,PCMINFO                                                  
         MVC   PCMINFO(64),WORK                                                 
         OI    PCMINFOH+6,X'80'                                                 
*                                                                               
         LA    R0,4                MAX COMMENTS LINES                           
         LA    R2,PCMCMT1H                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'40'        COMMENT ELEMENT(S)                           
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
DR70     BRAS  RE,NEXTEL                                                        
         BNE   DR76                                                             
         USING PATCMTEL,R6                                                      
         LLC   R1,PATCMTLN         GET COMMENT ELEM LEN                         
         AHI   R1,-4               GET ACTUAL COMMENT LEN-1                     
         LLC   RF,0(R2)            FLD LEN                                      
         AHI   RF,-9                                                            
         EX    RF,DRXC             CLEAR FIELD BEFORE MOVE                      
         CR    RF,R1               INSURE                                       
         BNL   DR74                NO FLDH                                      
         LR    R1,RF               CLOBBER                                      
*                                                                               
DR74     EX    R1,DRMVC                                                         
         OI    6(R2),X'80'                                                      
         LLC   RF,0(R2)            GET FLD LENGTH                               
         AR    R2,RF                                                            
         BCT   R0,DR70                                                          
         BRAS  RE,NEXTEL           SEE IF 5TH COMMENT                           
         BNE   DR90                NO, OK                                       
         DC    H'0'                                                             
*                                                                               
DR76     LTR   R0,R0                                                            
         BZ    DR90                                                             
*                                                                               
DR78     LLC   RF,0(R2)                                                         
         AHI   RF,-9               GET FLD LEN -1                               
         EX    RF,DROC                                                          
         BZ    *+12                                                             
         EX    RF,DRXC                                                          
         OI    6(R2),X'80'                                                      
         LA    R2,9(RF,R2)                                                      
         BCT   R0,DR78                                                          
         B     DR90                                                             
*                                                                               
DR80     MVI   GMSGTYPE,0                                                       
         CLI   MYSCREEN,X'C1'      TEST PTTN/CMML SCREEN                        
         BNE   DR82                                                             
         BRAS  RE,BLDINFO                                                       
         XC    TCMINFO,TCMINFO                                                  
         MVC   TCMINFO(64),WORK                                                 
         OI    TCMINFOH+6,X'80'                                                 
*                                                                               
DR81X    CLI   COMPFLAG,C'Y'                                                    
         BNE   DR82                                                             
         MVI   DELFLAG,C'Y'        SHOW DELETED CMMLS                           
         BRAS  RE,DCMMLS                                                        
         B     DR90                                                             
*                                                                               
DR82     CLI   MYSCREEN,X'5C'      TEST BPAT/CMML SCREEN                        
         BNE   DR84                                                             
         BRAS  RE,DBPAT            DISPLAY BPAT CMMLS                           
         B     DR90                                                             
*                                                                               
DR84     CLC   CONACT(3),=C'ADD'                                                
         BE    DR90                                                             
         L     R6,AIO              PERCENTAGE ELEMENT                           
         MVI   ELCODE,X'31'                                                     
         BRAS  RE,GETEL                                                         
         BE    DR90                                                             
         B     ADCMLERR                                                         
*                                                                               
DR90     LA    R2,TRAXXXXH                                                      
         SR    R0,R0                                                            
*                                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO FIRST DATA FIELD                    
         TM    1(R2),X'20'         TEST PROTECTED                               
         BO    *-10                                                             
         OI    6(R2),X'40'         POSITION CURSOR                              
*                                                                               
         BRAS  RE,SETPFTXT                                                      
*                                                                               
DR94     OC    MYSUBLIN,MYSUBLIN   TEST VERSIONING ACTIVE                       
         BZ    DR96                                                             
         BRAS  RE,SETFLDS          SET PROT/UNP AS REQUIRED                     
*                                                                               
         CLI   MYSCREEN,X'F3'                                                   
         BNE   EXIT                                                             
         L     R4,AIO                                                           
         CLC   0(2,R4),=X'0A22'    MAKE SURE IT'S A PATTERN REC                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TEMPKEY(L'TEMPKEY+L'TEMPKSV),KEY                                 
         MVC   TEMPAIO,AIO                                                      
         BRAS  RE,VCMMLSF3         RE-VALIDATE CMMLS ON ACTION CHA              
         MVC   KEY(L'TEMPKEY+L'TEMPKSV),TEMPKEY                                 
         MVC   AIO,TEMPAIO                                                      
         B     EXIT                                                             
*                                                                               
DR96     XC    SVPRVDA,SVPRVDA                                                  
         CLI   SVT3PROF,C'Y'       TEST TO COPY COMMENTS THIS CLT               
         BNE   DR98                                                             
         CLI   ACTNUM,ACTADD                                                    
         BE    DR98                MUST SAVE AFTER ADDREC                       
         MVC   KEY(13),TWAKEYSV    KEY OF CURRENT RECORD                        
         CLC   =X'0A22',KEY        MAKE SURE IT'S A PATTERN REC                 
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R0,2                                                             
         BRAS  RE,TRHIGH                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVPRVDA,KEY+14      THEN SAVE THIS DISK ADDR                     
*                                                                               
DR98     L     R1,AIO                                                           
         MVC   KEY(13),0(R1)                                                    
         CLC   =X'0A22',KEY        MAKE SURE IT'S A PATTERN REC                 
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   MYSCREEN,X'F3'                                                   
         BNE   DR99                                                             
         L     R4,AIO                                                           
         CLC   0(2,R4),=X'0A22'    MAKE SURE IT'S A PATTERN REC                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TEMPKEY(L'TEMPKEY+L'TEMPKSV),KEY                                 
         MVC   TEMPAIO,AIO                                                      
         BRAS  RE,VCMMLSF3         RE-VALIDATE CMMLS ON ACTION CHA              
         MVC   KEY(L'TEMPKEY+L'TEMPKSV),TEMPKEY                                 
         MVC   AIO,TEMPAIO                                                      
*                                                                               
DR99     CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   EXIT                NO, JUST DISPLAY REC                         
         CLI   ACTNUM,ACTADD                                                    
         BE    EXIT                                                             
         CLI   IOOPT,C'Y'          WAS I/O DONE                                 
         BE    EXIT                YES                                          
         LA    R0,3                                                             
         BRAS  RE,TRHIGH                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         B     EXIT                                                             
*                                                                               
DRMVC    MVC   8(0,R2),PATCMT-PATCMTEL(R6)                                      
DROC     OC    8(0,R2),8(R2)                                                    
DRXC     XC    8(0,R2),8(R2)                                                    
DRMVCA   MVC   WORK(0),PATPTN-PATPTNEL(R6)                                      
         DROP  R6                                                               
*                                                                               
ADCMLERR MVC   GERROR,=Y(ACMLMSG)  PRESS PF3 TO ADD CMLS                        
         CLI   MYSCREEN,X'C1'      TEST HAVE CMML SCREEN                        
         BNE   *+10                                                             
         MVC   GERROR,=Y(NTRCMMLS)                                              
         MVI   GMSGTYPE,C'I'                                                    
         LA    R2,CONACTH          ACTION                                       
         B     ERREXIT                                                          
         EJECT                                                                  
*================================================================               
* GENERATE AUTO-TURNAROUND REQUEST IF PROFILE SET                               
*================================================================               
                                                                                
AAR      CLI   ACTNUM,ACTADD       IS THIS AN ADD                               
         BNE   AAR10                                                            
*                                                                               
         CLI   MYPFKEY,3           TEST USER WANTS TO ADD CMMLS                 
         BNE   AAR2                                                             
*                                                                               
         MVI   MYSCREEN,X'5C'      SET FOR BPAT/CMMLS                           
         CLI   CONREC,C'B'                                                      
         BNE   AAR0                                                             
         BRAS  RE,GETSCR                                                        
         LA    R2,PCOCMLAH                                                      
         B     AAR1                                                             
                                                                                
AAR0     DS    0H                                                               
         MVI   MYSCREEN,X'C1'      SET FOR PTTN/CMMLS                           
         BRAS  RE,GETSCR                                                        
         LA    R2,TCMCMLAH                                                      
AAR1     OC    PRVPATDA,PRVPATDA   IF NO PREVIOUS, DON'T DO THIS                
         BZ    AAR6                                                             
         CLC   PRVPATMD,BAGYMD                                                  
         BNE   AAR6                                                             
         CLC   PRVPATCL,BCLT                                                    
         BNE   AAR6                                                             
         MVC   8(7,R2),=C'** COPY'                                              
         OI    6(R2),X'80'                                                      
         B     AAR6                                                             
*                                                                               
AAR2     CLI   MYPFKEY,2           TEST USER WANTS TO ADD COMMENTS              
         BNE   AAR10                                                            
*                                                                               
         XC    SVPRVDA,SVPRVDA                                                  
         CLI   SVT3PROF,C'Y'       TEST TO COPY COMMENTS THIS CLT               
         BNE   *+10                                                             
         MVC   SVPRVDA,TWAKEYSV    THEN DISK ADDRESS IS HERE!                   
         MVI   MYSCREEN,X'5D'                                                   
*                                                                               
AAR4     BRAS  RE,GETSCR                                                        
*                                                                               
AAR6     MVC   CONACT(3),=C'CHA'                                                
         OI    CONACTH+6,X'80'     XMT                                          
         MVI   TWALACT,ACTCHA      LIE!                                         
         MVI   ACTNUM,ACTCHA                                                    
         MVI   ACTEQU,ACTCHA                                                    
*                                                                               
         MVC   KEY(13),SVKEY       RESTORE KEY                                  
         LA    R0,4                                                             
         BRAS  RE,TRHIGH                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TWAKEYSV,KEY        AND SAVE IT WHERE GENCON WANTS IT            
*                                                                               
AAR10    BRAS  RE,GENR             GO GENERATE T/A REPORT REQ                   
         CLI   MYSCREEN,X'5C'                                                   
         BE    DR                                                               
         CLI   MYSCREEN,X'5D'                                                   
         BE    DR                                                               
         CLI   MYSCREEN,X'C1'                                                   
         BE    DR                                                               
         B     EXIT                                                             
         EJECT                                                                  
*==================================================================             
* TEST FOR GENUINE RECORD CHANGE - IF SO                                        
* GENERATE AUTO-TURNAROUND REQUEST IF PROFILE SET                               
*==================================================================             
                                                                                
PUT      MVC   AIO,AIO3            USE AIO3 FOR ORIGINAL REC                    
         L     R2,AIO1             GET KEY FROM AIO1                            
         MVC   KEY,0(R2)                                                        
         LA    R0,5                                                             
         BRAS  RE,TRHIGH                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     MUST HAVE FOUND REC                          
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R4,AIO3                                                          
         CLC   0(24,R2),0(R4)      COMPARE START OF 2 RECS FOR CHANGE           
         BNE   PUT20               IF DIFFERENT, GEN REQ                        
         LA    R2,24(,R2)                                                       
         LA    R4,24(,R4)                                                       
         SR    R1,R1                                                            
         ICM   R1,3,13(R2)         GET LEN                                      
         AHI   R1,-46              DON'T COMPARE ACT ELEM                       
*                                                                               
PUT10    CHI   R1,256   '          SEE IF REC LENGTH MORE THAN 256              
         BNH   PUT14                                                            
         CLC   0(256,R2),0(R4)                                                  
         BNE   PUT20                                                            
         LA    R2,256(,R2)                                                      
         LA    R4,256(,R4)                                                      
         AHI   R1,-256                                                          
         B     PUT10                                                            
*                                                                               
PUT14    EX    R1,PUTCLC                                                        
         BNE   PUT20               GEN REQ                                      
         MVI   IOOPT,C'Y'          ELSE, DO NOT UPDATE REC                      
         B     PUT24                                                            
*                                                                               
PUT20    BRAS  RE,GENR             GO GENERATE T/A REPORT REQ                   
*                                                                               
PUT24    MVC   AIO,AIO1                                                         
         B     EXIT                                                             
PUTCLC   CLC   0(0,R2),0(R4)                                                    
*                                                                               
* DELETE RECORD INVALID FOR PATTERNS                                            
*                                                                               
DELREC   MVI   ERROR,INVACT        DELETE IS INVALID                            
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         B     TRAPERR                                                          
         EJECT                                                                  
         PRINT GEN                                                              
         GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
         EJECT                                                                  
* VALIDATE STEXT FOR REQUEST                                                    
*                                                                               
VSTX     NTR1                                                                   
*                                                                               
         XC    FLD,FLD                                                          
*                                                                               
         MVC   AIO,AIO3                                                         
         XC    ELEM,ELEM                                                        
         MVI   ELCODE,X'50'                                                     
         GOTO1 REMELEM                                                          
*                                                                               
         LA    R4,KEY                                                           
         XC    KEY,KEY                                                          
         USING DTXKEY,R4                                                        
         MVC   DTXKID,=X'0A2D'                                                  
         MVC   DTXKAM(3),BAGYMD     A-M/CLT                                     
         MVI   DTXKDESC,C'-'                                                    
         MVI   DTXKTYP,C'L'                                                     
*                                                                               
         CLI   5(R2),0                                                          
         BE    VSTX40                                                           
         GOTO1 ANY                                                              
         MVC   DTXKDESC+1(6),WORK                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   NOTEXTER                                                         
*                                                                               
         CLC   WORK(6),PATTXTKY+1-PATTXTEL+ELEM                                 
         BNE   VSTX00                                                           
         GOTO1 ADDELEM                                                          
*                                                                               
         TM    FLAGFTR,DELPATSW    DELETE PATTERN                               
         BO    VSTX06                                                           
*                                                                               
         TM    FLAGFTR,RESPATSW    RESTORE PATTERN                              
         BO    VSTX06                                                           
*                                                                               
         B     VSTXX                                                            
*                                                                               
VSTX00   MVC   FLD(7),PATTXTKY-PATTXTEL+ELEM                                    
*                                                                               
         L     R6,AIO3                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    PATSTAT-PATDTAEL(R6),X'01'     SET ON PATTN TEXT CHANGE          
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING PATTXTEL,R6                                                      
         MVI   PATTXTEL,X'50'                                                   
         MVI   PATTXTLN,9                                                       
         MVI   PATTXTKY,C'-'                                                    
         MVC   PATTXTKY+1(6),WORK                                               
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
VSTX06   L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'70'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VSTX15                                                           
         USING DTXPATEL,R6                                                      
VSTX10   CLC   DTXPATKY(4),BPRD                                                 
         BNE   VSTX14                                                           
         CLC   DTXPATES,CODE                                                    
         BNE   VSTX14                                                           
         CLC   BREF,DTXPATRF                                                    
         BE    VSTX16                                                           
VSTX14   BRAS  RE,NEXTEL                                                        
         BE    VSTX10                                                           
*                                                                               
VSTX15   TM    FLAGFTR,DELPATSW    DELETE PATTERN (INSURANCE)                   
         BO    VSTX30              DO NOT CREATE NEW ELEM                       
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING DTXPATEL,R6                                                      
         MVI   DTXPATEL,X'70'                                                   
         MVI   DTXPATLN,DTXPATX-DTXPATEL                                        
         MVC   DTXPATP1(4),BPRD                                                 
         MVC   DTXPATES,CODE                                                    
         MVC   DTXPATRF,BREF                                                    
         MVI   DTXPATST,1          MAKE ACTIVE                                  
         GOTO1 ADDELEM                                                          
         B     VSTX18                                                           
*                                                                               
VSTX16   DS    0H                                                               
         TM    FLAGFTR,DELPATSW    DELETE PATTERN                               
         BZ    *+12                                                             
         MVI   DTXPATST,0          MAKE INACTIVE                                
         B     VSTX18                                                           
*                                                                               
         MVI   DTXPATST,1          MAKE ACTIVE                                  
*                                                                               
VSTX18   GOTO1 PUTREC                                                           
*                                                                               
         OC    FLD(7),FLD          WAS THERE AN OLD SPEC TEXT                   
         BZ    VSTX30               NO                                          
*                                                                               
         MVC   DTXKDESC,FLD                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'70'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DTXPATEL,R6                                                      
VSTX20   CLC   DTXPATKY(4),BPRD                                                 
         BNE   VSTX24                                                           
         CLC   DTXPATES,CODE                                                    
         BNE   VSTX24                                                           
         CLC   BREF,DTXPATRF                                                    
         BE    VSTX26                                                           
VSTX24   BRAS  RE,NEXTEL                                                        
         BE    VSTX20                                                           
         DC    H'0'                                                             
VSTX26   MVI   DTXPATST,0          MAKE INACTIVE                                
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
VSTX30   MVC   AIO,AIO3                                                         
*                                                                               
VSTXX    B     EXIT                                                             
*                                                                               
* NO DATA ENTERED IN TEXT, SEE IF THERE WAS A SPEC TEXT BEFORE *                
*                                                                               
VSTX40   L     R6,AIO3                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    PATSTAT-PATDTAEL(R6),X'FE'     SET OFF PATTN TEXT CHGE           
*                                                                               
         OC    ELEM,ELEM           WAS THERE AN OLD SPEC TEXT                   
         BZ    VSTXX                NO, NO UPDATE NEEDED                        
*                                                                               
         MVC   DTXKDESC+1(6),PATTXTKY+1-PATTXTEL+ELEM                           
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'70'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING DTXPATEL,R6                                                      
VSTX50   CLC   DTXPATKY(4),BPRD                                                 
         BNE   VSTX54                                                           
         CLC   DTXPATES,CODE                                                    
         BNE   VSTX54                                                           
         CLC   BREF,DTXPATRF                                                    
         BE    VSTX56                                                           
VSTX54   BRAS  RE,NEXTEL                                                        
         BE    VSTX50                                                           
         DC    H'0'                                                             
VSTX56   MVI   DTXPATST,0          MAKE INACTIVE                                
*                                                                               
         GOTO1 PUTREC                                                           
*                                                                               
         B     VSTX30                                                           
         DROP  R4,R6                                                            
         EJECT                                                                  
* VALIDATE PERIOD                                                               
*                                                                               
         USING PATDTAEL,R6                                                      
VPER     NTR1                                                                   
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERR             NO                                           
*                                                                               
         CLI   CODESW,C'Y'         THIS A COPY CODE = EST PATTERN               
         BNE   VPER10                                                           
         CLI   5(R2),2             ENTRY LEN 2                                  
         BNE   VPER10                                                           
*                                                                               
VPER10   LA    R3,8(,R2)           START DATE                                   
         GOTO1 DATVAL,DMCB,(R3),DATE                                            
         L     RE,DMCB             GET LENGTH OF FIELD                          
         LTR   RE,RE                                                            
         BZ    DATERR                                                           
         LA    R3,1(RE,R3)         POINT TO END DATE                            
         GOTO1 DATCON,(R1),(0,DATE),(3,STRTPAT)                                 
         CLC   0(3,R3),=CL3'UFN'                                                
         BNE   VPER20                                                           
*                                                                               
         CLI   SVT1PR15,C'Y'       T1 PROFILE 15 PROHIBITS UFN END DATE         
         BE    UFNDATER                                                         
*                                                                               
         MVC   ENDPAT,=XL3'FFFFFF'                                              
         B     VPER30                                                           
*                                                                               
VPER20   GOTO1 DATVAL,(R1),(R3),DATE                                            
         OC    DMCB(4),DMCB                                                     
         BZ    DATERR                                                           
         GOTO1 DATCON,(R1),(0,DATE),(3,ENDPAT)                                  
         CLC   STRTPAT,ENDPAT                                                   
         BH    DATERR                                                           
*                                                                               
VPER30   CLI   SVPROF11,C'E'       COPY CODE = EST                              
         BNE   VPER40                                                           
         CLI   CODE,0                                                           
         BE    VPER40                                                           
         CLC   ESTEND,STRTPAT                                                   
         BL    ESTDTER                                                          
         CLC   ESTSTR,ENDPAT                                                    
         BH    ESTDTER                                                          
         CLC   TRAEST,SPACES                                                    
         BE    VPER40                                                           
         OC    TRAEST,TRAEST                                                    
         BZ    VPER40                                                           
         XC    TRAEST,TRAEST                                                    
         OI    TRAESTH+6,X'80'                                                  
*                                                                               
VPER40   CLC   PATDTS,PATSTART    WERE DATES CHANGED                            
         BE    VPERX                                                            
*                                                                               
         MVI   NEWSUBSW,1                                                       
         MVC   PATSTART(6),PATDTS  SAVE DATES FOR CK TO CMML                    
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VPERX                                                            
                                                                                
*=========================================================                      
* CHECK THAT ALL CMMLS FALL WITHIN NEW DATES                                    
*=========================================================                      
                                                                                
         MVI   VCMLFLAG,C'P'       SET FLAG FOR VALIDATING PERIOD               
* 10 ELEM IS IN ELEM - IT HAS BEEN REMOVED FROM RECORD                          
         MVI   ADIDFLAG,C'Y'                                                    
         USING PATDTAEL,R6                                                      
         TM    PATSTAT1-PATDTAEL+ELEM,PATSADID   TEST CMMLS ARE ADIDS           
         BO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
                                                                                
         L     R6,AIO              COMMERCIAL LIST ELEMENT                      
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BE    VPER41                                                           
* FOR BPAT, LOOK FOR 31 ELEM                                                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'31'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VPERX                                                            
*                                                                               
         USING PATCMLEL,R6                                                      
VPER41   LLC   R0,1(R6)                                                         
         SRL   R0,4                SET FOR BCT/DIVIDE BY 16                     
*                                                                               
         LA    R5,PATCML           FIRST CMML                                   
         CLI   ELCODE,X'31'        TEST BPAT                                    
         BNE   *+8                                                              
         LA    R5,1(R5)                                                         
*                                                                               
VPER42   DS    0H                                                               
         MVC   WORK(8),0(R5)                                                    
         CLC   =X'5C00',WORK                                                    
         BE    VPER44                                                           
         BRAS  RE,VCML                                                          
         BNZ   VPERERR                                                          
*                                                                               
VPER44   OC    8(8,R5),8(R5)                                                    
         BZ    VPER46                                                           
         MVC   WORK(8),8(R5)                                                    
         CLC   =X'5C00',WORK                                                    
         BE    VPER46                                                           
         BRAS  RE,VCML                                                          
         BNZ   VPERERR                                                          
*                                                                               
VPER46   LA    R5,16(R5)                                                        
         BCT   R0,VPER42                                                        
*                                                                               
VPERX    XIT1                                                                   
*                                                                               
VPERERR  LHI   R0,BADCMLDT                                                      
         STH   R0,GERROR                                                        
         XC    ELEM,ELEM                                                        
         MVI   ELEM,13                                                          
         MVC   ELEM+1(8),WORK                                                   
*                                                                               
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   VPERERR2                                                         
         GOTO1 VTRPACK,DMCB,(C'U',WORK),ELEM+1                                  
*                                                                               
VPERERR2 LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST                                                     
         GOTO1 VTRAERR                                                          
         DROP  R6                                                               
         EJECT                                                                  
*===========================================================                    
* VALIDATE COMMENTS                                                             
*===========================================================                    
                                                                                
VCMT     NTR1                                                                   
         LA    R3,1                COMMENT NUMBER                               
         LA    R5,4                MAX POSSIBLE COMMENT LINES                   
         MVI   ELCODE,X'40'                                                     
         GOTO1 REMELEM             WILL REMOVE ALL X'40' ELEMENTS               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING PATCMTEL,R6                                                      
         MVI   PATCMTEL,X'40'                                                   
         LA    R2,PCMCMT1H                                                      
VCMT10   CLI   5(R2),0                                                          
         BE    VCMT20                                                           
         CLI   5(R2),53            MAX COMMENT LENGTH IS 53                     
         BH    COMLNER                                                          
         GOTO1 ANY                                                              
         LLC   R1,5(R2)            LENGTH                                       
         LA    R0,3(,R1)                                                        
         STC   R0,PATCMTLN                                                      
         STC   R3,PATCMTNO         COMMENT NUMBER                               
         BCTR  R1,0                                                             
         EX    R1,VCMTMVC                                                       
         GOTO1 ADDELEM                                                          
         LA    R3,1(,R3)           INCREMENT COMMENT NUMBER                     
VCMT20   LLC   R0,0(R2)            GET FIELD LENGTH                             
         AR    R2,R0               POINT TO NEXT FIELD                          
         BCT   R5,VCMT10                                                        
         B     EXIT                                                             
VCMTMVC  MVC   PATCMT(0),WORK                                                   
         EJECT                                                                  
*=================================================================              
* CHECK FOR ANY OTHER PATTERNS FOR THIS CLIENT PROD WITH SAME                   
* MARKET/STATION LIST AND OVERLAPPING DATES                                     
* AND START TIME, END TIME, AND DAYPART                                         
*=================================================================              
                                                                                
         EJECT                                                                  
* PRINT PRODUCT CODE AND SPOT LEN - R3 MUST POINT TO BINARY PROD                
*                                                                               
PPRD     NTR1                                                                   
         LA    R5,FLD              ADDRESS OF OUTPUT AREA                       
         XC    FLD,FLD                                                          
         CLI   0(R3),0             ANY PRODUCT CODE                             
         JE    EXIT                NO,DONE                                      
*                                                                               
         L     R1,ASVCLIST         ADDRESS OF SAVED CLIST (VALICLT)             
*                                                                               
PPRD10   CLI   0(R1),C' '                                                       
         JNL   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R3),3(R1)                                                    
         JE    PPRD12                                                           
         LA    R1,4(R1)                                                         
         J     PPRD10                                                           
*                                                                               
PPRD12   MVC   0(3,R5),0(R1)                                                    
         CLI   1(R3),0             ANY SPOT LEN                                 
         JE    PPRD16              NO                                           
         LA    R5,2(R5)                                                         
         CLI   0(R5),C' '                                                       
         JNH   PPRD14                                                           
         LA    R5,1(R5)                                                         
*                                                                               
PPRD14   MVI   0(R5),C'-'                                                       
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+6(2)                                                  
*                                                                               
         MVC   1(2,R5),DUB+1       MOVE 2 CHARS                                 
         CHI   R0,100                                                           
         JL    *+10                                                             
         MVC   1(3,R5),DUB                                                      
PPRD16   J     EXIT                                                             
         EJECT                                                                  
NOESTER  LHI   R0,ESTNF                                                         
         STH   R0,GERROR                                                        
         LA    R2,TRACODEH                                                      
         J     ERREXIT                                                          
ESTDTER  MVC   GERROR,=Y(DTNINEST) PATTERN DATES NOT IN ESTIMATE                
         J     ERREXIT                                                          
NOCPYERR MVC   GERROR,=Y(NOCPYAVL) NO PREVIOUS PATTERN TO BE COPIED             
         J     ERREXIT                                                          
CMLENER  MVC   GERROR,=Y(BDCMLLN4) ENTER XXXXXXXX OR XXXXXXXX-XXXXXXXX          
         J     ERREXIT                                                          
EQCMLERR MVC   GERROR,=Y(PBCMLSAM) PIGGY BACK CMMLS SAME                        
         J     ERREXIT                                                          
COMLNER  MVC   GERROR,=Y(COMLENER) COMMENT LENGTH MORE THAN 52                  
         B     ERREXIT                                                          
UFNDATER MVC   GERROR,=Y(SPUFNINV) UFN END DATE INVALID T1 PROFILE 15           
         B     ERREXIT                                                          
NOTEXTER MVC   GERROR,=Y(NOSTXFND) NO SPECIAL TEXT FOUND                        
         B     ERREXIT                                                          
*                                                                               
INVPTNER MVC   GERROR,=Y(IPREQPB)     INVERT = Y REQUIRES P/B PROD              
         LA    R2,TRAPTLNH                                                      
         B     ERREXIT                                                          
ERREXIT  GOTO1 VTRAERR                                                          
*                                                                               
CMLLENER MVI   ERROR,INVCMMLN      CML ID MUST BE 8 CHAR LEN, WASN'T            
         J     TRAPERR                                                          
*                                                                               
RSTDELER MVI   ERROR,INVALID       NO RESTORE UNLESS SOFT DELETED               
         J     TRAPERR                                                          
*                                                                               
CMLTYPER MVI   ERROR,UNMCMLTP      PIGGY-BACK CML TYPES UNEQUAL                 
         J     TRAPERR                                                          
*                                                                               
EQCMLER  MVI   ERROR,DUPLCMML      2 PIGGY-BACK COMMERCIALS EQUAL               
         J     TRAPERR                                                          
*                                                                               
MATPRDER MVI   ERROR,CMLPRDER      PAT PROD(S) NOT IN CMML                      
         J     TRAPERR                                                          
*                                                                               
INVDELER MVI   ERROR,INVCMLDL      CAN'T DELETE AN EMPTY FLD                    
         J     TRAPERR                                                          
*                                                                               
DATERR   MVI   ERROR,INVDATE                                                    
         J     TRAPERR                                                          
*                                                                               
CMLMISER LA    R2,TCMCMLAH         FIRST COMMERCIAL FIELD                       
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         J     TRAPERR                                                          
*                                                                               
CMLPRD   MVI   ERROR,CMLPRDER      PAT PROD(S) NOT IN CMML                      
         J     TRAPERR                                                          
*                                                                               
INVALERR MVI   ERROR,INVALID                                                    
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  R7                                                               
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* CHECK FOR ANY OTHER PATTERNS FOR THIS CLIENT PROD WITH SAME                   
* MARKET/STATION LIST AND OVERLAPPING DATES                                     
* AND START TIME, END TIME, AND DAYPART                                         
* IF PATSTAT1, PATSDLY, MOVE A * TO DAYPART                                     
* AND TESTS ARE DIFFERENT                                                       
*=================================================================              
                                                                                
CHKOV    NTR1  BASE=*,LABEL=*                                                   
         XC    MYPTNSTM(5),MYPTNSTM                                             
         L     R6,AIO3                                                          
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PATDTAEL,R6                                                      
         CLI   PATDTALN,38         IS THIS AN OLD PAT LEN (NO STAT)             
         BE    CHKOV04             YES, NOT DELETED                             
         TM    PATSTAT,X'80'       IS THIS PATTERN DELETED                      
         JO    EXIT                YES, NO DATE OVERLAP CK NEEDED               
*                                                                               
CHKOV04  MVC   MYPTNSTM(5),PATSTIM   SAVE ST/END TIMES + DPT                    
         TM    PATSTAT1,PATSDLY      TEST DAILY TIMES IN NEW REC                
         BZ    *+8                                                              
         MVI   MYPTNDPT,C'*'       SET SPECIAL DPT AS DAILY TIME FLAG           
*                                                                               
         LA    R2,PATSTART         R2 = THIS REC DATES                          
         MVI   ELCODE,X'20'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R3,R6               R3 = THIS REC MARKET/STAT LIST               
         MVC   KEY(10),SVKEY                                                    
         XC    KEY+10(3),KEY+10                                                 
         MVC   AIO,AIO1                                                         
*                                                                               
CHKOV10  DS    0H                                                               
         LA    R0,6                                                             
         BRAS  RE,TRHIGH                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE     ANY SIMILAR                                  
         JNE   EXIT                NO                                           
*                                                                               
         L     R1,AIO3                                                          
         CLC   KEY(13),0(R1)       SAME KEY AS THIS REC                         
         BE    CHKOV60             YES, BYPASS THIS                             
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PATDTAEL,R6                                                      
         CLI   PATDTALN,38         IS THIS AN OLD PAT LEN (NO STAT)             
         BE    CHKOV12             YES, NOT DELETED                             
         TM    PATSTAT,X'80'       IS THIS PATTERN DELETED                      
         BO    CHKOV60             YES, NO DATE OVERLAP CK NEEDED               
*                                                                               
CHKOV12  ST    R6,NEW10EL          SAVE 10 ELEM ADDR                            
*                                                                               
CHKOV20  LA    R5,PATSTART         SAVE IF ERROR                                
         CLC   =C'ES',0(R2)        THIS 'ES' DATES FROM ESTIMATE                
         BE    CHKOV24                                                          
         CLC   0(3,R5),3(R2)       THIS START TO END                            
         BH    CHKOV60             CAN'T OVERLAP                                
         CLC   3(3,R5),0(R2)       THIS END TO START                            
         BL    CHKOV60             CAN'T OVERLAP                                
         MVI   SAMEDTS,C'N'                                                     
         CLC   0(6,R5),0(R2)       TEST SAME DATES                              
         BNE   *+8                                                              
         MVI   SAMEDTS,C'Y'        SET FLAG FOR SAME DATES                      
*                                                                               
CHKOV24  CLI   PATDPT,0            TEST FILE HAS DPT                            
         BE    CHKOV23             NO                                           
         OC    MYPTNSTM,MYPTNSTM   TEST NEW HAS TIME                            
         BZ    CHKOV26                                                          
         B     CHKOV23X                                                         
*                                                                               
CHKOV23  OC    PATSTIM,PATSTIM     TEST FILE HAS TIME                           
         BZ    CHKOV26                                                          
         CLI   MYPTNDPT,C'*'                                                    
         BE    CHKOV26                                                          
         CLI   MYPTNDPT,0          TEST NEW HAS DPT                             
         BE    CHKOV26                                                          
*                                                                               
*HKOV23X SR    R5,R5                                                            
CHKOV23X B     DTMIXERR                                                         
*                                                                               
CHKOV26  MVI   ELCODE,X'20'                                                     
         BRAS  RE,NEXTEL                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   2(1,R6),2(R3)       IF LIST TYPES DIFF, CAN'T OVERLAP            
         BNE   CHKOV60                                                          
         LLC   R0,1(R6)            GET ELEM LEN                                 
         AHI   R0,-3               GET LIST LENL                                
         LA    R1,3(R6)            POINT TO START OF LIST-OLD PATTN             
*                                                                               
CHKOV28  LLC   RE,1(R3)            GET ELEM LEN                                 
         AHI   RE,-3               GET LIST LEN                                 
         LA    RF,3(R3)            POINT TO START OF LIST-NEW PATTN             
*                                                                               
CHKOV30  CLI   2(R6),C'C'          IF LIST TYPE = COMBINED CK FURTHER           
         BNE   CHKOV32              NO, GO ON                                   
*                                                                               
         CLI   0(RF),0             THIS A MARKET                                
         BNE   CHKOV60              NO, AFFILIATE, DONE                         
         CLI   0(R1),0             THIS A MARKET                                
         BNE   CHKOV60              NO, AFFILIATE, DONE                         
*                                                                               
CHKOV32  CLC   0(5,R1),0(RF)       CK ENTRY FOR EQ                              
         BNE   CHKOV50              NO PROBLEM                                  
*                                                                               
         CLI   2(R6),C'C'          IF LIST TYPE = COMBINED CK FURTHER           
         BNE   CHKOV70                                                          
*                                                                               
CHKOV33  STM   RE,R1,FLD                                                        
*                                                                               
* NOW JUMP TO AFFILIATES, AND SEE IF EQUAL THERE *                              
*                                                                               
CHKOV34  CLI   0(R1),0             THIS AN AFFILIATE                            
         BNE   CHKOV36              YES                                         
         LA    R1,5(R1)            BUMP THRU OLD PATTN                          
         AHI   R0,-5                                                            
         BP    CHKOV34                                                          
         DC    H'0'                MUST BE AFFILIATES                           
*                                                                               
CHKOV36  CLI   0(RF),0             THIS AN AFFILIATE                            
         BNE   CHKOV40              YES                                         
         LA    RF,5(,RF)           BUMP THRU OLD PATTN                          
         AHI   RE,-5                                                            
         BP    CHKOV36                                                          
         DC    H'0'                MUST BE AFFILIATES                           
*                                                                               
CHKOV40  STM   RE,RF,FLD+16                                                     
         B     CHKOV46                                                          
*                                                                               
CHKOV44  LM    RE,RF,FLD+16                                                     
*                                                                               
CHKOV46  CLC   0(5,R1),0(RF)       CK ENTRY FOR EQ AFFILIATE                    
         BE    CHKOV70             OVERLAP ERROR                                
         LA    RF,5(RF)            GO THRU LIST                                 
         AHI   RE,-5               UNTIL                                        
         BP    CHKOV46             END                                          
         LA    R1,5(R1)            BUMP THRU OLD PATTN                          
         AHI   R0,-5                                                            
         BP    CHKOV44                                                          
*                                                                               
         LM    RE,R1,FLD           RESTORE REGS                                 
*                                                                               
CHKOV50  LA    RF,5(,RF)           GO THRU LIST                                 
         AHI   RE,-5               UNTIL                                        
         BP    CHKOV30             END                                          
         LA    R1,5(R1)            BUMP THRU OLD PATTN                          
         AHI   R0,-5                                                            
         BP    CHKOV28                                                          
*                                                                               
CHKOV60  LA    R4,KEY                                                           
         USING PATKEY,R4                                                        
         SR    R1,R1                                                            
         ICM   R1,7,PATKREF                                                     
         SRL   R1,10               RIGHT JUSTIFY, DROPPING SUBLINE              
         X     R1,=XL4'00003FFF'   NOW MAKE POSITIVE                            
         AHI   R1,-1               BUILD NEXT KEY                               
         JZ    EXIT                IF ZERO, DONE                                
         X     R1,=XL4'00003FFF'             RESET REF TO 1'S COMPL             
         SLL   R1,10                         AND SUBLINE ZERO                   
         STCM  R1,7,PATKREF                                                     
         B     CHKOV10                                                          
                                                                                
*==============================================================                 
* PATTERNS OVERLAP - BUT CHECK FOR START/END TIMES OR DPT                       
* DATES MAY OVERLAP IF DAYPARTS ARE DIFFERENT                                   
*==============================================================                 
                                                                                
CHKOV70  L     R6,NEW10EL                                                       
         USING PATDTAEL,R6                                                      
*                                                                               
         CLI   MYPTNDPT,C'*'       TEST NEW REC HAS DAILY TIMES                 
         BE    CHKOV72             YES                                          
         TM    PATSTAT1,PATSDLY    NO - TEST OLD REC HAS DAILY TIMES            
         BO    DLYTMER2            YES - ERROR                                  
         B     CHKOV80                                                          
*                                                                               
CHKOV72  TM    PATSTAT1,PATSDLY    TEST FILE HAS DAILY TIMES                    
         BZ    DLYTMER3                                                         
         B     CHKOV76                                                          
*                                                                               
CHKOV74  CLI   MYPTNDPT,C'*'       TEST NEW HAS DAILY TIMES                     
         BE    DLYTMER3                                                         
*                                                                               
CHKOV76  MVI   SAMEDTS,C'Y'        SET FLAG TO FORCE ERR IF OVLP                
         B     CHKOV84                                                          
*                                                                               
CHKOV80  CLI   PATDPT,0            TEST FILE HAS DPT                            
         BE    CHKOV82             NO                                           
         CLI   MYPTNDPT,0          YES - TEST NEW HAS DPT                       
         BE    CHKOV60             NO - OK                                      
         CLC   PATDPT,MYPTNDPT     SEE IF THEY MATCH                            
         BE    DTOVERR                                                          
         CLI   SAMEDTS,C'Y'        TEST PATTERNS HAVE SAME DATES                
         BNE   DTSAMERR            TOO BAD - THEY SHOULD                        
         B     CHKOV60                                                          
*                                                                               
CHKOV82  CLI   MYPTNDPT,0          NO DPT IN NEW, ANY IN OLD                    
         BNE   CHKOV60             YES - THAT'S OK                              
*                                                                               
         OC    PATSTIM(4),PATSTIM     TEST TIME IN OLD                          
         BZ    DTOVERR                NO                                        
         OC    MYPTNSTM(4),MYPTNSTM   TIME IN OLD, TEST TIME IN NEW             
         BZ    DTOVERR                NO - THAT'S AN ERROR                      
*                                                                               
CHKOV84  MVC   MYADJSTM(4),MYPTNSTM   MOVE MY START/END TIMES                   
         MVC   MYADJSDT(6),0(R2)      MOVE MY START/END DATES                   
*                                                                               
         MVC   PTADJSTM(4),PATSTIM    MOVE FILE START/END TIMES                 
         MVC   PTADJSDT(6),0(R5)      MOVE FILE START/END DATES                 
*                                                                               
         LA    R0,4                                                             
         LA    R1,MYADJSTM                                                      
         LA    RF,MYADJSDT                                                      
*                                                                               
         BAS   RE,ADJTIM              ADJUST TO 24 HR CLOCK                     
         LA    R1,2(R1)                                                         
         LA    RF,3(RF)                                                         
         BCT   R0,*-12                                                          
*                                                                               
CHKOV86  CLI   MYPTNDPT,C'*'       TEST DAILY TIMES                             
         BNE   CHKOV87                                                          
         CLC   MYADJETM,PTADJSTM      THEN IT'S OK TO END BEFORE                
         BL    CHKOV60                                                          
         CLC   MYADJSTM,PTADJETM      OR TO START AFTER                         
         BH    CHKOV60                                                          
         B     DTOVERR                                                          
*                                                                               
CHKOV87  CLC   MYADJSDT,PTADJEDT      MY START DATE = FILE END DATE             
         BNE   CHKOV88                NO                                        
         CLC   MYADJSTM,PTADJETM      THEN I SHOULD START AFTER                 
         BH    CHKOV60                                                          
         CLC   PATSTART,PATEND        TEST PATTERN ALL ON 1 DAY                 
         BNE   DTOVERR                                                          
         CLC   MYADJETM,PTADJSTM      THEN IT'S OK TO END BEFORE                
         BL    CHKOV60                                                          
         B     DTOVERR                                                          
*                                                                               
CHKOV88  CLC   MYADJEDT,PTADJSDT      MY END DATE = FILE START DATE             
         BNE   CHKOV90                                                          
         CLC   MYADJETM,PTADJSTM      THEN I SHOULD END BEFORE                  
         BL    CHKOV60                                                          
         CLC   PATSTART,PATEND        TEST PATTERN ALL ON 1 DAY                 
         BNE   DTOVERR                                                          
         CLC   MYADJSTM,PTADJETM      THEN IT'S OK TO START AFTER               
         BL    CHKOV60                                                          
         B     DTOVERR                                                          
*                                                                               
CHKOV90  CLI   SAMEDTS,C'Y'                                                     
         BE    DTOVERR                                                          
         B     CHKOV60                                                          
*                                                                               
ADJTIM   NTR1                                                                   
         LH    R0,0(R1)                                                         
         CHI   R0,2400                                                          
         BL    *+6                                                              
         XR    R0,R0               MIDNIGHT BECOMES 0                           
         STH   R0,0(R1)                                                         
         B     ADJTIMX             NOP FOR CALENDAR DAYS                        
*                                                                               
         LH    R0,0(R1)  <<NOP>>                                                
         CHI   R0,600                                                           
         BNL   ADJTIMX                                                          
         AHI   R0,2400                                                          
         STH   R0,0(R1)                                                         
* NOW NEED TO BACK UP DATE TO PREVIOUS DAY                                      
         LR    R2,RF                  SAVE ADDRESS OF DATE FIELD                
         GOTO1 DATCON,DMCB,(3,(R2)),WORK                                        
         LHI   R0,-1                                                            
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R0)                                      
         GOTO1 DATCON,DMCB,WORK+6,(3,(R2))                                      
*                                                                               
ADJTIMX  XIT1                                                                   
*                                                                               
DLYTMER1 LHI   R0,DLYTIME1          IF DAILY=Y, MUST ENTER TIMES                
         STH   R0,GERROR                                                        
         J     DTOVERRB                                                         
*                                                                               
DLYTMER2 LHI   R0,DLYTIME2          FILE HAS DAILY, NEW DOESN'T                 
         STH   R0,GERROR                                                        
         J     DTOVERRB                                                         
*                                                                               
DLYTMER3 LHI   R0,DLYTIME3          NEW HAS DAILY, FILE DOESN'T                 
         STH   R0,GERROR                                                        
         J     DTOVERRB                                                         
*                                                                               
DTOVERR  MVC   GERROR,=Y(DATOVLAP)  PAT REF &1 DATES &2-&3 OVERLAP              
         J     *+10                                                             
*                                                                               
DTSAMERR MVC   GERROR,=Y(DTNOTSAM)  NOT SAME DATES                              
         J     *+10                                                             
*                                                                               
DTMIXERR MVC   GERROR,=Y(DPTTMERR)  CAN'T MIX DPTS/TIMES                        
         J     *+10                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         STCM  R3,7,GASUBST                                                     
         L     R4,AIO                                                           
         USING PATKEY,R4                                                        
         SR    R1,R1                                                            
         ICM   R1,7,PATKREF                                                     
         SRL   R1,10                                                            
         X     R1,=XL4'00003FFF'                                                
         MVI   0(R3),4             L'SUBST TEXT + 1                             
         EDIT  (R1),(3,1(R3))                                                   
         LA    R3,4(R3)                                                         
         MVI   0(R3),09            L'SUBST TEXT + 1                             
         LTR   R5,R5               TEST NO DATES                                
         BNZ   *+12                                                             
         MVI   0(R3),0             CLEAR                                        
         B     DTOVERRB                                                         
         GOTO1 DATCON,DMCB,(3,(R5)),(5,1(R3)) START DATE                        
         LA    R3,9(R3)                                                         
         CLI   3(R5),X'FF'         IS THIS A UFN DATE                           
         BNE   DTOVERRA                                                         
         MVI   0(R3),04            L'SUBST TEXT + 1                             
         MVC   1(3,R3),=C'UFN'                                                  
         B     DTOVERRB                                                         
*                                                                               
DTOVERRA MVI   0(R3),09                                                         
         GOTO1 (RF),(R1),(3,3(R5)),(5,1(R3)) END DATE                           
*                                                                               
DTOVERRB LA    R2,TRAPERH          POINT TO ERROR FIELD                         
         GOTO1 VTRAERR                                                          
         DROP  R4                                                               
         LTORG                                                                  
*===========================================                                    
*     VALIDATE KEY ROUTINE                                                      
*===========================================                                    
VKEY     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
VK2      LA    R2,TRAMEDH          MEDIA                                        
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
         NI    TRACLTH+4,X'FF'-X'20'                                            
*                                                                               
VK10     LA    R2,TRACLTH          CLIENT                                       
         XC    BCLT(6),BCLT        CLEAR BCLT, BPRD, BSLN, BPRD2, BSLN2         
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK12                                                             
*                                                                               
         TM    WHEN,X'38'          SOON/OV                                      
         BZ    VK12                 NO                                          
*                                                                               
* MUST ENTER CLIENT IF CLIENT STRING SECURITY                                   
*                                                                               
         MVI   ERROR,SECLOCK       PRESET SECURITY LOCKOUT                      
*                                                                               
         L     RF,ASECBLK                                                       
         USING SECD,RF                                                          
*                                                                               
         OC    SECCLAL,SECCLAL     CLIENT STRING?                               
         JNZ   TRAPERRV                                                         
*                                                                               
         MVI   ERROR,0                                                          
         B     VK20                                                             
*                                                                               
VK12     GOTO1 VALICLT                                                          
         OI    4(R2),X'20'                                                      
         BRAS  RE,GETPRF           GET TRAFFIC PROFILES                         
*                                                                               
VK20     LA    R2,TRAPRLNH         PRODUCT                                      
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERRX            NO, MUST BE                                  
         NI    SECFLAG,X'FF'-PRDTHTR INIT THEATRICAL PRODUCT                    
         GOTO1 VALIPRD                                                          
         CLC   =C'SJ',AGENCY       SJR                                          
         BE    VK20C                                                            
         CLC   =C'H7',AGENCY       AGY H7                                       
         BE    VK20C                                                            
         CLC   =C'OO',AGENCY       AGY OO                                       
         BE    VK20C                                                            
         CLC   =C'FR',AGENCY       AGY FR                                       
         BE    VK20C                                                            
         CLC   =C'M2',AGENCY       IF AGENCY MEDIACOM                           
         BNE   VK22                                                             
*                                                                               
VK20C    TM    SECFLAG,PRDTHTR     THEATRICAL PRODUCT                           
         BZ    VK21                                                             
         CLI   CONREC,C'B'         MUST USE BPAT                                
         BNE   BPATERR                                                          
         B     VK22                                                             
*                                                                               
VK21     CLC   =C'PAT',CONREC      MUST USE PAT FOR NON THTR PROD               
         BNE   UPATERR                                                          
*                                                                               
VK22     CLC   =C'POL',WORK                                                     
         BE    PRDINV                                                           
         CLC   =C'AAA',WORK                                                     
         BE    PRDINV                                                           
*                                                                               
         MVC   QPRD,WORK                                                        
         CLI   WORK+4,0            VALID SPOT LEN                               
         BNE   VK24                YES                                          
         MVI   WORK+4,30           DEFAULT SPOT LENGTH IS 30 SEC                
         LA    R0,L'TRAPRLN                                                     
         LA    R1,TRAPRLN                                                       
         CLI   0(R1),C' '                                                       
         BNH   *+16                                                             
         LA    R1,1(,R1)                                                        
         BCT   R0,*-12                                                          
         B     INVALPRD            IF NO ROOM IN FIELD, INVALID                 
*                                                                               
         MVC   0(3,R1),=C'-30'                                                  
         OI    TRAPRLNH+6,X'80'    FORCE TRANSMIT                               
*                                                                               
VK24     MVC   BPRD(2),WORK+3      GET BIN PROD AND SPOT LENGTH                 
         CLI   SVPROF13,C'Y'       THIS CLT USE PROD EQUIV                      
         BNE   VK30                 NO                                          
         BRAS  RE,CEP              GO CK EQUIV PRODS                            
         EJECT                                                                  
VK30     OI    TRAPRLNH+4,X'20'    SET PRODUCT VALIDATED                        
         LA    R2,TRAPTLNH         PRODUCT PARTNER                              
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VK40                NO                                           
*                                                                               
VK32     GOTO1 VALIPRD                                                          
         CLC   =C'SJ',AGENCY       SJR                                          
         BE    VK32B                                                            
         CLC   =C'H7',AGENCY       AGY H7                                       
         BE    VK32B                                                            
         CLC   =C'OO',AGENCY       AGY OO                                       
         BE    VK32B                                                            
         CLC   =C'FR',AGENCY       AGY FR                                       
         BE    VK32B                                                            
         CLC   =C'M2',AGENCY       IF AGENCY MEDIACOM                           
         BNE   VK32C                                                            
*                                                                               
VK32B    TM    SECFLAG,PRDTHTR     THEATRICAL PRODUCT                           
         BO    NOPBERR             NO P/B ALLOWED                               
*                                                                               
VK32C    CLC   =C'POL',WORK                                                     
         BE    PRDINV                                                           
         CLC   =C'AAA',WORK                                                     
         BE    PRDINV                                                           
*                                                                               
         CLI   WORK+4,0            VALID SPOT LEN                               
         BNE   VK34                YES                                          
         MVI   WORK+4,30           DEFAULT SPOT LENGTH IS 30 SEC                
         LA    R0,L'TRAPTLN                                                     
         LA    R1,TRAPTLN                                                       
         CLI   0(R1),C' '                                                       
         BNH   *+16                                                             
         LA    R1,1(,R1)                                                        
         BCT   R0,*-12                                                          
         B     INVALPRD            IF NO ROOM IN FIELD, INVALID                 
*                                                                               
         MVC   0(3,R1),=C'-30'                                                  
         OI    TRAPTLNH+6,X'80'    FORCE TRANSMIT                               
*                                                                               
VK34     CLC   QPRD,WORK           COMPARE 2 PRODUCTS                           
         BH    PRDSQERR            MUST BE LOWER FIRST                          
         BE    EQPRDERR            CAN NOT BE 2 EQUAL                           
         MVC   QPRD2,WORK                                                       
         MVC   BPRD2(2),WORK+3     GET BIN PROD AND SPOT LENGTH                 
*                                                                               
         CLI   SVPROF13,C'Y'       THIS CLT USE PROD EQUIV                      
         BNE   VK40                 NO                                          
         BRAS  RE,CEP              GO CK EQUIV PRODS                            
*                                                                               
VK40     OI    TRAPTLNH+4,X'20'    SET PARTNER VALIDATED                        
*                                                                               
         LA    R2,TRACODEH         CODE                                         
         BRAS  RE,VCC                                                           
         OI    4(R2),X'20'         SET VALIDATED                                
*                                                                               
         LA    R2,TRAREFH          REFERENCE NUMBER                             
         BRAS  RE,VREF                                                          
         OI    4(R2),X'20'         SET REF VALIDATED                            
                                                                                
* NO FILTERS IN MAINT MODE, ONLY IN LIST MODE (SPTRA23-T21623)                  
                                                                                
* NOW BUILD KEY                                                                 
                                                                                
VK70     XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PATKEY,R4                                                        
         MVC   PATKID,=XL2'0A22'                                                
         MVC   PATKAM(7),BAGYMD                                                 
         MVC   PATKCODE,CODE                                                    
         MVC   PATKREF,BREFSUB                                                  
*                                                                               
         CLC   TWAKEYSV(13),0(R4)                                               
         BE    VKX                                                              
         CLI   ACTNUM,ACTADD                                                    
         BE    VK70A                                                            
*                                                                               
         LA    R0,13                                                            
         BRAS  RE,TRHIGH                                                        
         GOTO1 HIGH                ATTEMPT TO GIVE GENCON KEY AND D/A           
         CLC   KEY(13),KEYSAVE     BUT ONLY IF WE FOUND THE RECORD              
         BE    *+10                                                             
         XC    KEY+14(4),KEY+14    ELSE CLEAR DISK ADDRESS                      
*                                                                               
VK70A    MVC   MYPFKEY,PFKEY       SAVE REAL PFKEY                              
         XC    MYSUBLIN,MYSUBLIN   CLEAR VERSION                                
         XC    SVADDWHN,SVADDWHN   CLEAR RECORD ADDED DATE                      
*                                                                               
         CLI   CONREC,C'B'     <--WILL THIS WORK OR RESTORE PFK IN VR           
         BE    VK70C                                                            
*                                                                               
         MVI   PFKEY,0             NO REAL PFKEY ON ADD                         
         BRAS  RE,GETVRSN          MAKE SURE VSRN SCREEN NOT ACTV               
                                                                                
* NEED TO READ VERSION ONE TO GET ORIGINAL ADD DATE                             
                                                                                
VK70C    MVC   WORK,KEY            SAVE CURRENT KEY                             
         CLI   ACTNUM,ACTADD                                                    
         BE    VK72                                                             
*                                                                               
         ICM   R0,7,PATKREF                                                     
         N     R0,=X'FFFFFC00'                                                  
         O     R0,=X'000003FE'     SET FOR HIGHEST SUBLINE                      
         STCM  R0,7,PATKREF                                                     
         LA    R0,7                                                             
         BRAS  RE,TRHIGH                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'F1'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VK72                                                             
         LA    RE,ACTVADDT-ACTVD(R6)                                            
         MVC   SVADDWHN,0(RE)      SAVE ORIGINAL ADD DATE                       
*                                                                               
VK72     MVC   KEY,WORK            RESTORE KEY                                  
*                                                                               
         CLI   MYSCREEN,X'5C'      TEST BPAT CMML                               
         BE    VK74                                                             
         CLI   MYSCREEN,X'C1'      TEST PTTN CMML SCREEN                        
         BE    VK74                                                             
         CLI   MYSCREEN,X'5D'      TEST PTTN CMMT  SCREEN                       
         BNE   VKX                 NO                                           
VK74     MVI   MYSCREEN,X'F3'      RESTORE PATTERN SCREEN                       
         BRAS  RE,GETSCR                                                        
*                                                                               
VKX      ICM   R0,7,PATKREF        GET REF/SUB                                  
         N     R0,=X'000003FF'     DROP REF                                     
         X     R0,=X'000003FF'     UNCOMPLEMENT                                 
         STH   R0,LATEST           SAVE MOST RECENT SUBLINE                     
*                                                                               
         LA    R4,PCOPFK                                                        
         CLI   MYSCREEN,X'5C'                                                   
         BNE   *+14                                                             
         XC    15(63,R4),15(R4)     CLEAR ALL BUT PF2                           
         B     VKX1                                                             
*                                                                               
         LA    R4,TCMPFK                                                        
         CLI   MYSCREEN,X'C1'                                                   
         BNE   *+14                                                             
         XC    21(57,R4),21(R4)    CLEAR ALL BUT PF2, PF3                       
         B     VKX1                                                             
*                                                                               
         LA    R4,PCMPFK                                                        
         CLI   MYSCREEN,X'5D'                                                   
         BE    *+8                                                              
         LA    R4,TRAPFK                                                        
         XC    15(63,R4),15(R4)     CLEAR ALL BUT PF2                           
*                                                                               
VKX1     CLC   LATEST,=H'1'        TEST ONLY ONE VERSION                        
         BE    VKX2                                                             
         CLI   ACTNUM,ACTDIS                                                    
         BE    *+12                                                             
         CLI   ACTNUM,ACTSEL                                                    
         BNE   VKX2                                                             
         BRAS  RE,SETPFTXT                                                      
*                                                                               
VKX2     EQU   *                                                                
VKXIT    XIT1                                                                   
         EJECT                                                                  
*                                                                               
NOPBERR  LHI   R0,NOPBTHTR         NO P/B FOR THEATRICAL PRODUCTS               
         LA    R2,TRAPTLNH                                                      
         J     VTRERR                                                           
*                                                                               
UPATERR  LHI   R0,UPATTERR         USE PAT FOR NON THEATRICAL PRODUCTS          
         LA    R2,CONRECH                                                       
         J     VTRERR                                                           
*                                                                               
BPATERR  LHI   R0,BPATTHTR         USE BPAT FOR THEATRICAL PRODUCTS             
         LA    R2,CONRECH                                                       
*                                                                               
VTRERR   STH   R0,GERROR                                                        
         GOTO1 VTRAERR                                                          
*                                                                               
PRDSQERR MVI   ERROR,INVPRDSQ      PRODS OUT OF SEQ                             
         B     PRDERR                                                           
*                                                                               
EQPRDERR MVI   ERROR,INVEQPRD      PROD/PARTNER PROD EQ                         
*                                                                               
PRDERR   LA    R2,TRAPRLNH         POINT TO PROD-SPOT LEN                       
         B     TRAPERRX                                                         
*                                                                               
INVALPRD MVI   ERROR,INVPROD       BAD ENTRY IN PRODUCT OR PARTNER              
         B     TRAPERRX                                                         
*                                                                               
PRDINV   MVI   ERROR,INVPRDCD      POL & AAA INVALID PRODUCT CODES              
         B     TRAPERRX                                                         
*                                                                               
MISSERRX MVI   ERROR,MISSING                                                    
TRAPERRX GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* GENERATE AUTO-TURNAROUND REQUEST IF PROFILE SET                               
*===========================================================                    
                                                                                
GENR     NTR1  BASE=*,LABEL=*                                                   
         CLI   CONREC,C'B'         BPAT RECORD                                  
         JE    EXIT                NO T/A                                       
*                                                                               
         CLI   SVPROF+9,C'R'       AUTO TURNAROUND REMOTE                       
         BE    GENR10              YES                                          
         CLI   SVPROF+9,C'D'       AUTO TURNAROUND                              
         JNE   EXIT                NO                                           
*                                                                               
GENR10   XC    REQHDR(26),REQHDR                                                
         MVC   REQUEST(80),SPACES                                               
         MVC   REQUEST(2),=C'TZ'                                                
         MVC   REQUEST+2(2),AGENCY                                              
         MVC   REQUEST+4(23),=CL23'*.PAT.LIST..DDS,T/A....'                     
         MVC   REQUEST+27(1),QMED                                               
         MVI   REQUEST+28,C'.'                                                  
         MVC   REQUEST+29(3),QCLT                                               
         MVI   REQUEST+32,C'.'                                                  
         MVC   REQUEST+33(3),QPRD                                               
         LA    R1,REQUEST+36                                                    
         CLI   BPRD2,0                                                          
         BE    GENR20                                                           
*                                                                               
         MVI   0(R1),C'.'                                                       
         MVC   1(3,R1),QPRD2                                                    
         LA    R1,4(,R1)                                                        
*                                                                               
GENR20   MVC   0(2,R1),=C'.*'                                                   
         CLI   SVPROF+9,C'D'                                                    
         BE    *+10                                                             
         MVC   REQHDR+11(2),T216FFD+17                                          
         XC    FLD,FLD                                                          
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',FLD,REQHDR                    
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*-----------------------------------------------------                          
* DISPLAY COMMERCIALS AND PERCENTAGES FOR BPAT RECORD                           
* BUILD ONE CONTINUOUS CMML ELEM IN OLDELEM                                     
*-----------------------------------------------------                          
                                                                                
DBPAT    NTR1  BASE=*,LABEL=*,WORK=(R5,WORKX-WORKD)                             
         USING WORKD,R5            LOCAL STORAGE                                
*                                                                               
         LA    R0,WORKD            CLEAR WORKD                                  
         LHI   R1,WORKX-WORKD                                                   
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R6,AIO              COMMERCIAL LIST ELEMENT                      
         MVI   ELCODE,X'31'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DBPATX                                                           
*                                                                               
         LA    R4,OLDELEM                                                       
DBPAT2   LLC   RE,1(R6)            ELEM LEN                                     
         SHI   RE,4                MINUS ELCODE/LEN/NUMBER/EX MOVE              
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),3(R6)       SAVE OLD ELEM INFO                           
*                                                                               
         AR    R4,RE                                                            
         LA    R4,1(R4)                                                         
         BRAS  RE,NEXTEL                                                        
         BE    DBPAT2                                                           
*                                                                               
         L     R6,AIO              PERCENTAGE ELEMENT                           
         MVI   ELCODE,X'34'                                                     
         BRAS  RE,GETEL                                                         
         JNE   EXIT                                                             
*                                                                               
         LA    R3,2(R6)            R3 POINTS TO PCT                             
         LA    R6,OLDELEM          R6 POINTS TO CMML                            
         LA    R2,PCOCMLAH         1ST SCREEN FIELD                             
         MVI   PCTCT,1             CMML ID IN PCT ELEM                          
*                                                                               
DBPAT10  CLI   0(R6),0             TEST NO MORE CMMLS                           
         BE    DBPAT20                                                          
*                                                                               
         CLC   =X'5C00',0(R6)                                                   
         BE    DBPAT11                                                          
*                                                                               
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   DBPAT11                                                          
         GOTO1 VTRPACK,DMCB,(C'U',0(R6)),8(R2)                                  
         B     *+10                                                             
DBPAT11  DS    0H                                                               
         MVC   8(8,R2),0(R6)       MOVE CMML                                    
         OI    6(R2),X'80'                                                      
*                                                                               
         LLC   RE,0(R2)                                                         
         AR    R2,RE               PT TO % FIELD                                
         OI    6(R2),X'80'                                                      
*                                                                               
         CLI   0(R6),C'*'          TEST DELETED CMML                            
         BE    DBPAT12                                                          
*                                                                               
         LLC   R0,2(R3)                                                         
         EDIT  (R0),(3,8(R2)),ALIGN=LEFT                                        
*                                                                               
DBPAT12  LA    R3,3(R3)            BUMP IN PERCENTAGE ELEM                      
*                                                                               
         LA    R6,16(R6)           NEXT CMML                                    
*                                                                               
         LLC   RE,0(R2)                                                         
         AR    R2,RE               PROTECTED FIELD                              
         LLC   RE,0(R2)                                                         
         AR    R2,RE               CML                                          
*                                                                               
         LA    R0,PCOPC24H                                                      
         CR    R2,R0                                                            
         BL    DBPAT10                                                          
*                                                                               
DBPAT20  LA    R0,PCOPC24H                                                      
         CR    R2,R0                                                            
         BNL   DBPATX              NO                                           
*                                                                               
         XC    8(8,R2),8(R2)       CLEAR CMML FIELD                             
         OI    6(R2),X'80'         XMT                                          
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         XC    8(2,R2),8(R2)       CLEAR PCT FIELD                              
         OI    6(R2),X'80'                                                      
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         LLC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     DBPAT20                                                          
*                                                                               
DBPATX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*============================================================                   
* CHECK FOR DELETED COMMERCIALS                                                 
*============================================================                   
                                                                                
CHKDEL   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING PATCMLEL,R6                                                      
         LLC   R3,PATCMLLN         GET ELEM LEN                                 
         SRL   R3,3                DIV BY 8=NO OF CMML PRS (DROPS ODD)          
         LA    R4,PATCML           1ST CMML                                     
         DROP  R6                                                               
         LA    R2,TCMDCML                                                       
         XC    TCMDCML,TCMDCML                                                  
         LA    R2,4(R2)            START AT +4                                  
         OI    TCMDCMLH+6,X'80'                                                 
*                                                                               
CHKDEL10 OC    0(8,R4),0(R4)       IS THIS A CML                                
         BZ    CHKDEL30                                                         
         CLC   =C'HIATUS',0(R4)    HIATUS                                       
         BE    CHKDEL30                                                         
         LAY   RE,DELCML                                                        
         CLC   0(L'DELCML,RE),0(R4)  DELETED CML                                
         BE    CHKDEL30                                                         
         MVC   WORK(8),0(R4)                                                    
*                                                                               
         BRAS  RE,FNDCML            GO FIND COMMERCIAL                          
         BNE   CHKDEL30                                                         
*                                                                               
CHKDEL12 MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
         TM    CMLSTAT,X'80'       CMML DELETED                                 
         BZ    CHKDEL20            NO                                           
         LA    R0,TCMDCML+63                                                    
         LA    RF,16(R2)                                                        
         CR    RF,R0               ROOM FOR ONE MORE CML?                       
         BNL   CHKDEL22                                                         
*                                                                               
         MVC   0(4,R2),=C'DEL*'                                                 
         MVC   4(8,R2),0(R4)                                                    
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   CHKDEL18                                                         
         GOTO1 VTRPACK,DMCB,(C'U',0(R4)),4(R2)                                  
*                                                                               
         LA    RF,16(R2)                                                        
CHKDEL14 CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,CHKDEL14                                                      
         LA    R2,2(RF)                                                         
         B     CHKDEL30                                                         
*                                                                               
CHKDEL18 LA    R2,13(R2)                                                        
         B     CHKDEL30                                                         
*                                                                               
CHKDEL20 CLI   HASTIME,C'Y'        TEST PATTERN HAS TIME                        
         BE    *+12                                                             
         CLI   HASDPT,C'Y'         OR PATTERN HAS DPT                           
         BNE   CHKDEL30                                                         
*                                                                               
         MVI   ELCODE,X'B0'        GET CMML MATCHING ELEMENT                    
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   CHKDEL30                                                         
*                                                                               
         USING CMLMATEL,R6                                                      
         OC    CMLMSTIM,CMLMSTIM   TEST CMML HAS TIME                           
         BZ    CHKDEL30                                                         
         LA    R0,TCMDCML+63                                                    
         LA    RF,16(R2)                                                        
         CR    RF,R0               ROOM FOR ONE MORE CML?                       
         BL    CHKDEL24                                                         
*                                                                               
CHKDEL22 MVC   0(8,R2),=C'MORE....'                                             
         B     CHKDELX                                                          
*                                                                               
CHKDEL24 MVC   0(4,R2),=C'TIM*'                                                 
         MVC   4(8,R2),0(R4)                                                    
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   CHKDEL30                                                         
         GOTO1 VTRPACK,DMCB,(C'U',0(R4)),4(R2)                                  
*                                                                               
         LA    RF,16(R2)                                                        
CHKDEL26 CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,CHKDEL26                                                      
         LA    R2,2(RF)                                                         
*                                                                               
CHKDEL30 LA    R4,8(R4)                                                         
         BCT   R3,CHKDEL10                                                      
         DROP  R6                                                               
*                                                                               
CHKDELX  MVC   AIO,AIO1                                                         
         OC    TCMDCML,TCMDCML     TEST ANY ERRORS                              
         BZ    *+10                                                             
         MVC   TCMDCML(3),=C'==>'                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* DISPLAY CMMLS/PCTS/ROTATIONS FOR X'5C' BPAT/CMML SCREEN                       
* FOR COPY FUNCTION                                                             
*=================================================================              
                                                                                
BCMMLS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   HASTIME,C'N'        SET PATTERN HAS TIME FLAG                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
         OC    PATSTIM(4),PATSTIM                                               
         BZ    *+8                                                              
         MVI   HASTIME,C'Y'                                                     
*                                                                               
         TM    PATSTAT1,PATSADID                                                
         BZ    *+8                                                              
         MVI   ADIDFLAG,C'Y'                                                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'34'        LOOK FOR NORMAL ROT ELEM                     
         BRAS  RE,GETEL                                                         
         BE    BCM2                                                             
*                                                                               
         SR    R3,R3               INDICATE NO ROT ELEM                         
         B     BCM4                                                             
*                                                                               
BCM2     LA    R3,PATPCTLT-PATPCTEL(R6)  POINT TO FIRST PCT LETTER              
*                                                                               
BCM4     L     R6,AIO              COMMERCIAL LIST ELEMENT                      
         MVI   ELCODE,X'31'                                                     
         BRAS  RE,GETEL                                                         
         BE    BCM5                                                             
         CLI   COMPFLAG,C'Y'                                                    
         BNE   BCMX                                                             
         DC    H'0'                                                             
*                                                                               
BCM5     CLI   DELFLAG,C'N'        TEST DO NOT DISPLAY DELETES                  
         BE    *+8                                                              
         BRAS  RE,CHKDEL           DISPLAY ANY DELETED CMLS                     
*                                                                               
         USING PATBCMEL,R6                                                      
         LA    R2,PCOCMLAH         1ST SCREEN FIELD                             
         LLC   R4,PATBCMLN                                                      
         SRL   R4,4                GIVES NUMBER OF CMMLS                        
         LA    R5,PATBCML          FIRST CMML                                   
*                                                                               
BCM6     XC    8(L'PCOCMLA,R2),8(R2)      CLEAR FIELD                           
         OI    6(R2),X'80'                AND XMT                               
         LLC   RE,0(R2)                                                         
         AR    RE,R2                                                            
         XC    8(L'PCOPCTA,RE),8(RE)    CLEAR PCT FIELD                         
         OI    6(RE),X'80'              AND XMT                                 
*                                                                               
BCM8     CLC   =X'5C00',0(R5)        TEST DELETED                               
         BNE   BCM10                                                            
         CLI   DELFLAG,C'N'          TEST DO NOT DISPLAY DELETES                
         BE    BCM24                                                            
         MVI   8(R2),C'*'                                                       
         MVI   5(R2),1                                                          
         AHI   R2,PCOCMLBH-PCOCMLAH  POINT TO NEXT CMML                         
         B     BCM24                                                            
*                                                                               
BCM10    MVC   8(8,R2),0(R5)       MOVE FIRST CMML                              
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   BCM12                                                            
         GOTO1 VTRPACK,DMCB,(C'U',0(R5)),8(R2)                                  
                                                                                
BCM12    LLC   RE,0(R2)                                                         
         AR    RE,R2                                                            
         BCTR  RE,0                POINT TO LAST CHAR IN FIELD                  
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         SR    RE,R2                                                            
         AHI   RE,-7                                                            
         STC   RE,5(R2)                                                         
*                                                                               
BCM20    LLC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO PCT FIELD                           
         OI    6(R2),X'80'                                                      
*                                                                               
         LTR   R3,R3               TEST HAVE PCTS                               
         BZ    BCM22                                                            
BCM20F   SR    R0,R0                                                            
         ICM   R0,3,1(R3)          GET PCT                                      
                                                                                
         CLI   DELFLAG,C'N'                                                     
         BNE   BCM21                                                            
         LTR   R0,R0                                                            
         BNZ   BCM21                                                            
         LA    R3,3(R3)                                                         
         B     BCM20F                                                           
                                                                                
BCM21    EDIT  (R0),(3,8(R2)),0,ALIGN=LEFT                                      
*                                                                               
         MVI   5(R2),3                                                          
         CLI   10(R2),C' '                                                      
         BH    BCM22                                                            
         MVI   5(R2),2                                                          
         CLI   9(R2),C' '                                                       
         BH    BCM22                                                            
         MVI   5(R2),1                                                          
*                                                                               
BCM22    LLC   R0,0(R2)            POINT TO NEXT LETTER                         
         AR    R2,R0                                                            
         LLC   R0,0(R2)            POINT TO NEXT CMML FIELD                     
         AR    R2,R0                                                            
*                                                                               
         LTR   R3,R3               TEST HAVE PCT ELEMENT                        
         BZ    *+8                                                              
         LA    R3,3(R3)                                                         
*                                                                               
BCM24    LA    R5,16(R5)                                                        
         BCT   R4,BCM6                                                          
                                                                                
         MVI   ELCODE,X'31'                                                     
         BRAS  RE,NEXTEL                                                        
         BNE   BCM26                                                            
*                                                                               
         USING PATBCMEL,R6                                                      
         LLC   R4,PATBCMLN                                                      
         SRL   R4,4                GIVES NUMBER OF CMMLS                        
         LA    R5,PATBCML          FIRST CMML                                   
         B     BCM6                                                             
                                                                                
* CLEAR REMAINING CMMLS/PCTS                                                    
                                                                                
BCM26    XC    8(L'PCOCMLA,R2),8(R2)      CLEAR FIELD                           
         OI    6(R2),X'80'                AND XMT                               
         LLC   RE,0(R2)                                                         
         AR    RE,R2                                                            
         XC    8(L'PCOPCTA,RE),8(RE)    CLEAR PCT FIELD                         
         OI    6(RE),X'80'              AND XMT                                 
*                                                                               
         AHI   R2,PCOCMLBH-PCOCMLAH                                             
         LA    R0,PCOPC24H         LAST PCT FIELD                               
         CR    R2,R0               TEST DONE ALL                                
         BL    BCM26                                                            
BCMX     J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* DISPLAY CMMLS/PCTS/ROTATIONS FOR X'C1' PATT/CMML SCREEN                       
*=================================================================              
                                                                                
DCMMLS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   HASTIME,C'N'        SET PATTERN HAS TIME FLAG                    
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
         OC    PATSTIM(4),PATSTIM                                               
         BZ    *+8                                                              
         MVI   HASTIME,C'Y'                                                     
*                                                                               
         MVI   ADIDFLAG,C'Y'                                                    
         TM    PATSTAT1,PATSADID   TEST CMMLS ARE ADIDS                         
         BO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'36'        LOOK FOR ABSURD ROT ELEM                     
         BRAS  RE,GETEL                                                         
         BE    DCM2                                                             
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'34'        LOOK FOR NORMAL ROT ELEM                     
         BRAS  RE,GETEL                                                         
         BE    DCM2                                                             
*                                                                               
         SR    R3,R3               INDICATE NO ROT ELEM                         
         B     DCM4                                                             
*                                                                               
DCM2     LA    R3,PATPCTLT-PATPCTEL(R6)  POINT TO FIRST PCT LETTER              
*                                                                               
DCM4     L     R6,AIO              COMMERCIAL LIST ELEMENT                      
         MVI   ELCODE,X'30'                                                     
         BRAS  RE,GETEL                                                         
         BE    DCM5                                                             
         CLI   COMPFLAG,C'Y'                                                    
         BNE   DCMX                                                             
         DC    H'0'                                                             
*                                                                               
DCM5     CLI   DELFLAG,C'N'        TEST DO NOT DISPLAY DELETES                  
         BE    *+8                                                              
         BRAS  RE,CHKDEL           DISPLAY ANY DELETED CMLS                     
*                                                                               
         USING PATCMLEL,R6                                                      
         LA    R2,TCMCMLAH         1ST SCREEN FIELD                             
         LLC   R4,PATCMLLN                                                      
         SRL   R4,4                GIVES NUMBER OF CMMLS                        
         LA    R5,PATCML           FIRST CMML                                   
*                                                                               
DCM6     XC    8(L'TCMCMLA,R2),8(R2)      CLEAR FIELD                           
         OI    6(R2),X'80'                AND XMT                               
         LLC   RE,0(R2)                                                         
         AR    RE,R2                                                            
         XC    8(L'TCMPCTA,RE),8(RE)    CLEAR PCT FIELD                         
         OI    6(RE),X'80'              AND XMT                                 
*                                                                               
DCM8     CLC   =X'5C00',0(R5)        TEST DELETED                               
         BNE   DCM10                                                            
         CLI   DELFLAG,C'N'          TEST DO NOT DISPLAY DELETES                
         BE    DCM24                                                            
         MVI   8(R2),C'*'                                                       
         MVI   5(R2),1                                                          
         AHI   R2,TCMCMLBH-TCMCMLAH  POINT TO NEXT CMML                         
         B     DCM24                                                            
*                                                                               
DCM10    MVC   8(8,R2),0(R5)       MOVE FIRST CMML                              
         OC    8(8,R5),8(R5)       TEST FOR SECOND CMML                         
         BZ    *+14                NO                                           
         MVI   16(R2),C'-'                                                      
         MVC   17(8,R2),8(R5)                                                   
*                                                                               
         CLC   8(6,R2),=C'HIATUS'                                               
         BE    DCM12                                                            
*                                                                               
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   DCM12                                                            
         XC    8(25,R2),8(R2)                                                   
         GOTO1 VTRPACK,DMCB,(C'U',0(R5)),8(R2)                                  
         OC    8(8,R5),8(R5)                                                    
         BZ    DCM12                                                            
*                                                                               
         LA    RE,20(R2)                                                        
DCM10A   CLI   0(RE),C' '                                                       
         BH    DCM10B                                                           
         BCT   RE,DCM10A                                                        
*                                                                               
DCM10B   MVI   1(RE),C'-'                                                       
         LA    RE,2(RE)                                                         
         ST    RE,DMCB+4                                                        
         GOTO1 VTRPACK,DMCB,(C'U',8(R5))                                        
*                                                                               
DCM12    LLC   RE,0(R2)                                                         
         AR    RE,R2                                                            
         BCTR  RE,0                POINT TO LAST CHAR IN FIELD                  
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         SR    RE,R2                                                            
         AHI   RE,-7                                                            
         STC   RE,5(R2)                                                         
*                                                                               
DCM20    LLC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO PCT FIELD                           
         OI    6(R2),X'80'                                                      
*                                                                               
         LTR   R3,R3               TEST HAVE PCTS                               
         BZ    DCM22                                                            
         ICM   R0,3,1(R3)          GET PCT                                      
         EDIT  (R0),(3,8(R2)),0,ALIGN=LEFT                                      
*                                                                               
         MVI   5(R2),3                                                          
         CLI   10(R2),C' '                                                      
         BH    DCM22                                                            
         MVI   5(R2),2                                                          
         CLI   9(R2),C' '                                                       
         BH    DCM22                                                            
         MVI   5(R2),1                                                          
*                                                                               
DCM22    LLC   R0,0(R2)            POINT TO NEXT LETTER                         
         AR    R2,R0                                                            
         LLC   R0,0(R2)            POINT TO NEXT CMML FIELD                     
         AR    R2,R0                                                            
*                                                                               
         LTR   R3,R3               TEST HAVE PCT ELEMENT                        
         BZ    *+8                                                              
         LA    R3,3(R3)                                                         
*                                                                               
DCM24    LA    R5,16(R5)                                                        
         BCT   R4,DCM6                                                          
*                                                                               
         LA    R0,TCMPCTOH         TEST PAST LAST PCT FIELD                     
         CR    R2,R0                                                            
         BNL   DCM30                                                            
                                                                                
* CLEAR REMAINING CMMLS/PCTS                                                    
                                                                                
DCM26    XC    8(L'TCMCMLA,R2),8(R2)      CLEAR FIELD                           
         OI    6(R2),X'80'                AND XMT                               
         LLC   RE,0(R2)                                                         
         AR    RE,R2                                                            
         XC    8(L'TCMPCTA,RE),8(RE)    CLEAR PCT FIELD                         
         OI    6(RE),X'80'              AND XMT                                 
*                                                                               
         AHI   R2,TCMCMLBH-TCMCMLAH                                             
         LA    R0,TCMPCTOH         LAST PCT FIELD                               
         CR    R2,R0               TEST DONE ALL                                
         BL    DCM26                                                            
         EJECT                                                                  
*=======================================================                        
* DISPLAY LETTER ROTATION ELEMENT                                               
*=======================================================                        
                                                                                
DCM30    XC    TCMROT,TCMROT                                                    
         OI    TCMROTH+6,X'80'                                                  
         XC    TCMDROT,TCMDROT                                                  
         OI    TCMDROTH+6,X'80'                                                 
*                                                                               
         LA    R2,TCMDROTH         IF HAVE PCTS, SHOW DERIVED ROT               
         MVI   ELCODE,X'34'        CHECK FOR PCT ELEM                           
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    *+8                                                              
         LA    R2,TCMROTH          IF NO PCT, SHOW ROT                          
*                                                                               
         MVI   ELCODE,X'32'        FIND ROTATION ELEMENT                        
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    DCM34                                                            
         CLC   TCMCMLA(6),=CL6'HIATUS'                                          
         BE    DCMX                                                             
         DC    H'0'                                                             
*                                                                               
         USING PATPTNEL,R6                                                      
DCM34    LLC   R1,PATPTNLN                                                      
         AHI   R1,-3                                                            
         EX    R1,*+8              MOVE IN ROTATION                             
         B     *+10                                                             
         MVC   8(0,(R2)),PATPTN-PATPTNEL(R6)                                    
*                                                                               
DCMX     J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*===============================================================                
* VALIDATE CMMLS AND PCTS FOR NORMAL PATTERN WHEN ON F3 SCREEN                  
*===============================================================                
                                                                                
VCMMLSF3 NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'        FIND STATUS ELEM                             
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
*                                                                               
         MVI   ADIDFLAG,C'Y'                                                    
         TM    PATSTAT1,PATSADID   TEST CMMLS ARE ADIDS                         
         BO    *+8                                                              
         MVI   ADIDFLAG,C'N'                                                    
*                                                                               
         MVC   STRTPAT,PATSTART    PASS THESE TO VCMML                          
         MVC   ENDPAT,PATEND                                                    
*                                                                               
         MVI   HASTIME,C'N'        SET PATTERN HAS TIME FLAG                    
         OC    PATSTIM(4),PATSTIM                                               
         BZ    *+8                                                              
         MVI   HASTIME,C'Y'                                                     
*                                                                               
         MVI   HASDPT,C'N'         SET PATTERN HAS DAYPART FLAG                 
         CLI   PATDPT,0                                                         
         BE    *+8                                                              
         MVI   HASDPT,C'Y'                                                      
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'30'        PTTN CMML ELEMS                              
         CLI   CONREC,C'B'         TEST BPAT                                    
         BNE   *+8                 NO                                           
         MVI   ELCODE,X'31'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL            COMMERCIAL LIST ELEMENT                      
         BNE   VCMLFX                                                           
*                                                                               
         USING PATCMLEL,R6                                                      
         CLC   PATCML(6),=C'HIATUS'                                             
         BE    VCMLFX                                                           
*                                                                               
         SR    R4,R4                                                            
         IC    R4,PATCMLLN                                                      
         BCTR  R4,0                                                             
         BCTR  R4,0                                                             
         LA    R5,PATCML                                                        
*                                                                               
         CLC   CONREC(4),=C'BPAT'                                               
         BNE   VCMLF30                                                          
         LA    R5,1(R5)                                                         
         BCTR  R4,0                                                             
*                                                                               
VCMLF30  DS    0H                                                               
         XC    SVTYPE1(7),SVTYPE1                                               
         XC    SVTYPE2(7),SVTYPE2                                               
                                                                                
         CLC   0(2,R5),=X'5C00'         DELETED                                 
         BE    VCMLF55                                                          
                                                                                
         XC    WORK(20),WORK                                                    
         MVC   WORK(8),0(R5)                                                    
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   VCMLF40                                                          
         MVC   WORK+12(8),0(R5)                                                 
         GOTO1 VTRPACK,DMCB,(C'U',WORK+12),WORK                                 
VCMLF40  MVI   VCMLFLAG,0                                                       
         LA    R2,TRACMLAH                                                      
         LA    R1,SVTYPE1                                                       
         BRAS  RE,VCML                                                          
                                                                                
         OC    8(8,R5),8(R5)                                                    
         BZ    VCMLF50                                                          
         XC    WORK(20),WORK                                                    
         MVC   WORK(8),8(R5)                                                    
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   VCMLF45                                                          
         MVC   WORK+12(8),8(R5)                                                 
         GOTO1 VTRPACK,DMCB,(C'U',WORK+12),WORK                                 
VCMLF45  MVI   VCMLFLAG,0                                                       
         LA    R2,TRACMLAH                                                      
         LA    R1,SVTYPE2                                                       
         BRAS  RE,VCML                                                          
                                                                                
VCMLF50  BRAS  RE,CHKCMLS          CHECK PRDS/SLNS                              
                                                                                
VCMLF55  SH    R4,=H'16'                                                        
         LTR   R4,R4                                                            
         BNP   VCMLF60                                                          
         LA    R5,16(R5)                                                        
         OC    0(16,R5),0(R5)                                                   
         BZ    VCMLF55                                                          
         B     VCMLF30                                                          
*                                                                               
VCMLF60  DS    0H                                                               
*                                                                               
VCMLFX   J     EXIT                                                             
         EJECT                                                                  
*                                                                               
*===============================================================                
* VALIDATE CMMLS AND PCTS FOR NORMAL PATTERN                                    
*===============================================================                
                                                                                
VCMMLS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   VCMLFLAG,0                                                       
         MVI   HASTIME,C'N'        SET PATTERN HAS TIME FLAG                    
         MVI   HASDPT,C'N'                                                      
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
         OC    PATSTIM(4),PATSTIM                                               
         BZ    *+8                                                              
         MVI   HASTIME,C'Y'                                                     
         CLI   PATDPT,0                                                         
         BE    *+8                                                              
         MVI   HASDPT,C'Y'                                                      
                                                                                
* NEED TO SEE IF ALL CMMLS ARE VALID ADIDS                                      
                                                                                
         LA    R2,TCMCMLAH                                                      
         CLI   5(R2),0                                                          
         JE    MISSERR                                                          
*                                                                               
VCMML2   CLI   5(R2),0                                                          
         BE    VCMML6                                                           
*                                                                               
VCMML4   BRAS  RE,MYSCAN           SCANNER CAN'T DO 12 CHAR FIELDS!             
*                                                                               
         CLC   =C'HIATUS',WORK+44                                               
         BE    VCMML6                                                           
         CLC   =C'DELETE',WORK+44                                               
         BE    VCMML6                                                           
         CLI   WORK+44,C'*'                                                     
         BE    VCMML6                                                           
         OC    WORK+44(12),SPACES                                               
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'P',WORK+44),WORK+12                              
         BNE   VCMML8                                                           
*                                                                               
         CLI   WORK+33,0           TEST P/B INPUT                               
         BE    VCMML6              NO                                           
         GOTO1 VTRPACK,DMCB,(C'P',WORK+56),WORK+12                              
         BNE   VCMML8                                                           
*                                                                               
VCMML6   AHI   R2,TCMCMLBH-TCMCMLAH                                             
         LA    R0,TCMCMLOH         LAST CMML FIELD                              
         CR    R2,R0                                                            
         BL    VCMML2                                                           
         MVI   ADIDFLAG,C'Y'                                                    
         B     VCMML10                                                          
*                                                                               
VCMML8   MVI   ADIDFLAG,C'N'       DO NOT USE ADIDS THIS REC                    
*                                                                               
VCMML10  MVC   AIO,AIO3                                                         
*                                                                               
         XC    PCTTBL,PCTTBL       CLEAR PCT BUILD AREA                         
         LA    RE,PCTTBL                                                        
         ST    RE,PCTNXT           SET A(NEXT ENTRY)                            
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
         MVC   STRTPAT,PATSTART    PASS THESE TO VCMML                          
         MVC   ENDPAT,PATEND                                                    
         DROP  R6                                                               
*                                                                               
         LA    R2,TCMCMLAH         FIRST INPUT CMML                             
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING PATCMLEL,R6                                                      
*                                                                               
         CLC   TCMCMLA(6),=CL6'HIATUS'                                          
         BNE   VCMML12                                                          
         MVC   PATCML(6),=CL6'HIATUS'                                           
         MVI   CMMLCT,1            SET ENTRY TO ONE                             
         LA    R3,PATCML+16        POINT BEYOND FIRST ENTRY                     
         B     VCMML70                                                          
*                                                                               
VCMML12  MVI   CMMLCT,0            CLEAR COUNTERS                               
         MVI   DELCT,0                                                          
         MVI   PCTTOT,0                                                         
         MVI   PRDMATSW,0                                                       
*                                                                               
         LA    R3,PATCML                                                        
*                                                                               
VCMML14  LR    RE,R2                                                            
         BCTR  RE,0                BACK UP TO CMML LETTER                       
         IC    R0,0(RE)                                                         
         L     RE,PCTNXT                                                        
         STC   R0,0(RE)            SAVE LETTER                                  
*                                                                               
VCMML20  CLI   5(R2),0             TEST NO INPUT                                
         BNE   VCMML22                                                          
*                                                                               
         LR    RE,R2               SHOULD BE NO MORE CMMLS                      
VCMML21  AHI   RE,TCMCMLBH-TCMCMLAH                                             
         LA    R0,TCMCMLOH                                                      
         CR    RE,R0                                                            
         BH    VCMML66                                                          
         CLI   5(RE),0                                                          
         JNE   MISSERR             THERE SHOULD BE INPUT AT 0(R2)               
         B     VCMML21                                                          
*                                                                               
VCMML22  BRAS  RE,MYSCAN                                                        
*                                                                               
         XC    SVTYPE1(7),SVTYPE1  TYPE(4)/SLN(1)/SOLO(1)/FLAG(1)               
         XC    SVTYPE2(7),SVTYPE2                                               
*                                                                               
         CLI   WORK+33,0           IF NO CMML2                                  
         BNE   VCMML26                                                          
         CLI   WORK+32,6           IF INPUT IS 6 CHARS                          
         BNE   *+14                                                             
         CLC   WORK+44(6),DELETE   THEY CAN INPUT 'DELETE'                      
         BE    VCMML24                                                          
*                                                                               
         CLI   WORK+32,1           OR IF INPUT IS ONE CHAR                      
         BNE   VCMML26                                                          
         CLI   WORK+44,C'*'        IT SHOULD BE A *                             
         BNE   VCMML26                                                          
*                                                                               
VCMML24  MVC   0(16,R3),DELCML                                                  
         LLC   R1,DELCT                                                         
         LA    R1,1(R1)            BUMP DELETED COUNT                           
         STC   R1,DELCT                                                         
         AHI   R2,TCMCMLBH-TCMCMLAH  POINT TO NEXT CMML                         
         LA    R3,16(R3)                                                        
         B     VCMML64                                                          
*                                                                               
VCMML26  MVC   WORK(12),WORK+44    MOVE CMML1                                   
         CLI   WORK+32,8           MUST HAVE AT LEAST 8 CHARS                   
         JL    VCMLERR                                                          
         CLI   ADIDFLAG,C'Y'                                                    
         BE    *+12                                                             
         CLI   WORK+32,8                                                        
         JNE   VCMLERR                                                          
*                                                                               
         LA    R1,SVTYPE1                                                       
         BRAS  RE,VCML                                                          
         MVC   0(8,R3),WORK+12     MOVE COMPRESSED CODE TO ELEM                 
*                                                                               
         CLI   WORK+33,0           TEST CMML2 INPUT                             
         BE    VCMML30             NO                                           
*                                                                               
VCMML28  CLI   WORK+33,8                                                        
         JL    VCMLERR                                                          
         CLI   ADIDFLAG,C'Y'                                                    
         BE    *+12                                                             
         CLI   WORK+33,8                                                        
         JL    VCMLERR                                                          
*                                                                               
         MVC   WORK(12),WORK+56     VALIDATE CMML2                              
         LA    R1,SVTYPE2                                                       
         BRAS  RE,VCML                                                          
         MVC   8(8,R3),WORK+12                                                  
*                                                                               
         CLC   SVTYPE1,SVTYPE2     ARE BOTH CMML TYPES THE SAME                 
         JNE   CMLTYPER            NO ERROR                                     
*                                                                               
         CLC   0(8,R3),8(R3)       BOTH CMLS SHOULD NOT BE EQUAL                
         JE    EQCMLERR                                                         
*                                                                               
VCMML30  BRAS  RE,CHKCMLS          CHECK PRDS/SLNS                              
                                                                                
*================================================================               
* CHECK FOR DUPLICATE INPUT                                                     
*================================================================               
                                                                                
VCMML40  LA    R1,PATCML           FIRST COMMERCIAL                             
*                                                                               
VCMML42  CR    R1,R3               TEST THIS IS US                              
         BE    VCMML50             YES - DONE                                   
         CLC   0(16,R1),0(R3)      TEST EQUAL                                   
         JE    EQCMLER                                                          
         LA    R1,16(R1)           POINT TO NEXT CMML(S)                        
         B     VCMML42                                                          
*                                                                               
VCMML50  LLC   R1,CMMLCT                                                        
         LA    R1,1(R1)                                                         
         STC   R1,CMMLCT                                                        
*                                                                               
VCMML52  LLC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO PCT FIELD                           
         CLI   5(R2),0             TEST INPUT                                   
         BE    VCMML54             NO                                           
*                                                                               
         GOTO1 VALINUM                                                          
         LLC   R0,PCTTOT                                                        
         LLC   RE,ACTUAL                                                        
         AR    R0,RE                                                            
         STC   R0,PCTTOT                                                        
*                                                                               
         L     RE,PCTNXT                                                        
         MVC   2(1,RE),ACTUAL      SAVE PCT                                     
         LA    RE,3(RE)                                                         
         ST    RE,PCTNXT                                                        
         B     VCMML60                                                          
*                                                                               
VCMML54  CLI   PCTTOT,0            TEST PCTS PREVIOUSLY INPUT                   
         JNE   MISSERR                                                          
*                                                                               
VCMML60  LA    R3,16(R3)                                                        
*                                                                               
VCMML62  LLC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO NEXT LETTER                         
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO NEXT CMML                           
*                                                                               
VCMML64  LA    R0,TCMPCTOH         LAST PCT FIELD                               
         CR    R2,R0                                                            
         BL    VCMML14                                                          
*                                                                               
VCMML66  CLI   CMMLCT,0           ANY ENTRIES FOUND                             
         JE    CMLMISER            NO                                           
*                                                                               
VCMML70  MVI   ELEM,X'30'          SET ELEMENT CODE                             
         LA    R0,ELEM                                                          
         SR    R3,R0               GIVE ELEMENT LENGTH                          
         STC   R3,ELEM+1                                                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'        GET OLD CMML ELEM                            
         BRAS  RE,GETEL                                                         
         BNE   VCMML72                                                          
         SR    RE,RE                                                            
         IC    RE,1(R6)            GET OLD ELEM LENGTH                          
         BCTR  RE,0                                                             
         EX    RE,VCMMLCLC                                                      
         BE    VCMML74                                                          
         MVI   NEWSUBSW,1          SET TO ADD NEW REF/SUBL ON CHG               
         GOTO1 VRECUP,DMCB,AIO,(R6)   DELETE OLD ELEMENT                        
*                                                                               
VCMML72  GOTO1 ADDELEM                                                          
*                                                                               
         L     R6,AIO              SET FLAG THAT CMMLS ARE ADIDS                
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
*                                                                               
         OI    PATSTAT1,PATSADID   SET CMMLS ARE ADIDS                          
         CLI   ADIDFLAG,C'Y'                                                    
         BE    *+8                                                              
         NI    PATSTAT1,X'FF'-PATSADID  UNSET FLAG                              
         B     VCMML74                                                          
*                                                                               
VCMMLCLC CLC   0(0,R6),ELEM                                                     
*                                                                               
VCMML74  MVC   AIO,AIO3                                                         
         MVI   ELCODE,X'32'                                                     
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'34'                                                     
         GOTO1 REMELEM                                                          
         MVI   ELCODE,X'36'                                                     
         GOTO1 REMELEM                                                          
         CLC   TCMCMLA(6),=CL6'HIATUS' NO ROTATION FOR HIATUS                   
         BE    VCMMLX                                                           
*                                                                               
         CLI   PCTTOT,0            TEST ANY PCTS INPUT                          
         BNE   VCMML76             YES                                          
         CLI   DELCT,0             TEST ANY DELETED CMMLS                       
         BNE   VCMML80             YES                                          
         CLI   CMMLCT,1            TEST EXACTLY 1 CMML                          
         BNE   VCMML80             NO                                           
         MVI   PCTTOT,100                                                       
         MVC   ELEM(5),=X'3405C10064'   SET A=100                               
         XC    TCMROT,TCMROT            AND IGNORE ROTATION INPUT               
         MVI   TCMROTH+5,0                                                      
         OI    TCMROTH+6,X'80'                                                  
         B     VCMML76X                                                         
*                                                                               
VCMML76  CLI   PCTTOT,99                                                        
         BE    *+12                                                             
         CLI   PCTTOT,100                                                       
         JNE   ROTPCERR            ERROR MUST BE 99/100 PCT                     
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'36'                                                       
         L     RE,PCTNXT           GET A(NEXT ENTRY)                            
         MVI   0(RE),0             MARK END OF LIST                             
         LA    R0,PCTTBL                                                        
         SR    RE,R0               GIVES LENGTH OF ENTRIES                      
         AHI   RE,2                                                             
         STC   RE,ELEM+1           SET ELEM LENGTH                              
         MVC   ELEM+2(45),PCTTBL   MOVE TABLE TO ELEM                           
         GOTO1 ADDELEM                                                          
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'34'                                                       
         L     RE,PCTNXT           GET A(NEXT ENTRY)                            
         MVI   0(RE),0             MARK END OF LIST                             
         LA    R0,PCTTBL                                                        
         SR    RE,R0               GIVES LENGTH OF ENTRIES                      
         AHI   RE,2                                                             
         STC   RE,ELEM+1           SET ELEM LENGTH                              
         MVC   ELEM+2(45),PCTTBL   MOVE TABLE TO ELEM                           
*                                                                               
VCMML76X GOTO1 ADDELEM                                                          
*                                                                               
VCMML80  LA    R2,TCMROTH          ROTATION PATTERN ENTRIES                     
         BRAS  RE,VROT                                                          
                                                                                
*===============================================================                
* TURN OFF INCOMPLETE RECORD FLAG IN 10 ELEM                                    
*===============================================================                
                                                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
         NI    PATSTAT1,X'FF'-PATSINC TURN OFF INCOMPLETE REC FLAG              
*                                                                               
VCMMLX   J     EXIT                                                             
         EJECT                                                                  
DELCML   DS    0XL16                                                            
         DC    C'*'                                                             
         DC    XL15'00'                                                         
DELMSG   DS    0CL9                                                             
         DC    C'*'                                                             
DELETE   DC    CL6'DELETE'                                                      
         DC    C'D*'                                                            
ATBL     DC    CL15'ABCDEFGHIJKLMNO'  15 LETTERS FOR 15 COMMERCIALS             
         EJECT                                                                  
*==============================================================                 
* SPLIT THE COMMERCIAL FIELD AT 8(R2)                                           
*==============================================================                 
                                                                                
MYSCAN   NTR1                                                                   
         XC    WORK+32(12),WORK+32                                              
         MVC   WORK+44(24),SPACES                                               
         MVC   WORK+32(1),5(R2)    SET FIELD 1 LENGTH                           
*                                                                               
         CLI   5(R2),12                                                         
         BH    MYSCAN2                                                          
*                                                                               
         LLC   RE,5(R2)            GET INPUT FIELD LENGTH                       
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         J     EQXIT                                                            
MYMVCLHS MVC   WORK+44(0),8(R2) *EXECUTED*                                      
*                                                                               
MYSCAN2  LA    R1,8(R2)                                                         
         LLC   R0,5(R2)                                                         
*                                                                               
MYSCAN4  CLI   0(R1),C'-'                                                       
         BE    MYSCAN10                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,MYSCAN4                                                       
         J     INVALERR            TOO MANY CHARS                               
*                                                                               
MYSCAN10 LA    R4,1(R1)            SAVE POINTER TO RHS DATA                     
         LA    RE,8(R2)                                                         
         SR    R1,RE               GIVES NUM CHARS UP TO -                      
         STC   R1,WORK+32          SET AS LENGTH OF LHS                         
         BCTR  R1,0                                                             
         EX    R1,MYMVCLHS         MOVE LHS DATA                                
*                                                                               
         AHI   R0,-1               ADJUST REMAINING COUNT                       
         JNP   INVALERR            SHOULD BE POSITIVE                           
         STC   R0,WORK+33          SET AS LEN OF RHS                            
         LR    RE,R0                                                            
         BCTR  RE,0                                                             
         EX    RE,MYMVCRHS         MOVE RHS DATA                                
         J     EQXIT                                                            
MYMVCRHS MVC   WORK+56(0),0(R4) *EXECUTED*                                      
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* VALIDATE COMMERCIALS FOR BPAT RECORD                                          
*================================================================               
                                                                                
VBPAT    NTR1  BASE=*,LABEL=*,WORK=(R5,WORKX-WORKD)                             
         USING WORKD,R5            LOCAL STORAGE                                
         MVI   VCMLFLAG,0                                                       
*                                                                               
         LA    R0,WORKD            CLEAR WORKD                                  
         LHI   R1,WORKX-WORKD                                                   
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
                                                                                
* NEED TO SEE IF ALL CMMLS ARE VALID ADIDS                                      
                                                                                
         LA    R2,PCOCMLAH                                                      
*                                                                               
VBPAT2   CLI   5(R2),0                                                          
         BE    VBPAT6                                                           
*                                                                               
         MVC   WORK,SPACES                                                      
         LLC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)                                                    
         CLC   =C'DELETE',WORK                                                  
         BE    VBPAT6                                                           
         CLI   WORK,C'*'                                                        
         BE    VBPAT6                                                           
*                                                                               
         GOTO1 VTRPACK,DMCB,(C'P',WORK),WORK+12                                 
         BNE   VBPAT8                                                           
*                                                                               
VBPAT6   AHI   R2,PCOCMLBH-PCOCMLAH                                             
         LA    R0,PCOCM24H         LAST CMML FIELD                              
         BL    VBPAT2                                                           
         MVI   ADIDFLAG,C'Y'                                                    
         B     *+8                                                              
VBPAT8   MVI   ADIDFLAG,C'N'       DO NOT USE ADIDS THIS REC                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
         MVC   STRTPAT,PATSTART    PASS THESE TO VCML                           
         MVC   ENDPAT,PATEND                                                    
*                                                                               
         OI    PATSTAT1,PATSADID   SET CMMLS ARE ADIDS                          
         CLI   ADIDFLAG,C'Y'                                                    
         BE    *+8                                                              
         NI    PATSTAT1,X'FF'-PATSADID  UNSET FLAG                              
*                                                                               
         DROP  R6                                                               
*                                                                               
         MVC   AIO,AIO3                                                         
         L     R6,AIO                                                           
         MVI   ELCODE,X'31'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VBPAT20             MUST BE INCOMPLETE REC                       
                                                                                
* SAVE OLD X'31' ELEMS IN OLDELEM                                               
                                                                                
         LA    R4,OLDELEM                                                       
VBPAT10  LLC   RE,1(R6)            ELEM LEN                                     
         SHI   RE,4                MINUS ELCODE/LEN/NUMBER/EX MOVE              
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),3(R6)       SAVE OLD ELEM INFO                           
*                                                                               
         AR    R4,RE                                                            
         LA    R4,1(R4)                                                         
         BRAS  RE,NEXTEL                                                        
         BE    VBPAT10                                                          
*                                                                               
VBPAT20  MVI   ELCODE,X'31'        COMMERCIALS                                  
         GOTO1 REMELEM             WILL REMOVE ALL X'31' ELEMENTS               
*                                                                               
         LA    R2,PCOCMLAH                                                      
*                                                                               
         CLC   PCOCMLA(6),=CL6'HIATUS'                                          
         BE    HIATERR             HIATUS NOT ALLOWED FOR THTR                  
*                                                                               
         MVI   CMMLCT,0            SET ENTRIES TO ZERO                          
         MVI   DELCT,0                                                          
         MVI   PCTTOT,0                                                         
         MVI   PCTCT,0             INITIALIZE PCT ID                            
*                                                                               
         LA    R3,NEWELEM          START OF NEW ELEM                            
         LA    R1,PCTELEM                                                       
         ST    R1,PCTNXT           SAVE ADDRESS OF PERCENT SAVE AREA            
*                                                                               
VBPAT30  CLI   5(R2),0             TEST NO INPUT                                
         BNE   VBPAT34                                                          
*                                                                               
         LR    RE,R2               SHOULD BE NO MORE CMMLS                      
VBPAT32  AHI   RE,PCOCMLBH-PCOCMLAH                                             
         LA    R0,PCOPC24H                                                      
         CR    RE,R0                                                            
         BNL   VBPAT90                                                          
         CLI   5(RE),0                                                          
         JNE   MISSERR             THERE SHOULD BE INPUT AT 0(R2)               
         B     VBPAT32                                                          
*                                                                               
VBPAT34  CLI   5(R2),1             IF INPUT IS ONE CHAR                         
         BNE   *+12                                                             
         CLI   8(R2),C'*'          IT SHOULD BE A *                             
         BE    VBPAT36                                                          
*                                                                               
         CLI   5(R2),6             IF INPUT IS 6 CHARS                          
         BNE   VBPAT40                                                          
         LAY   RE,DELETE                                                        
         CLC   8(6,R2),0(RE)       THEY CAN INPUT 'DELETE'                      
         BNE   VBPAT40                                                          
*                                                                               
VBPAT36  LAY   RE,DELCML                                                        
         MVC   0(16,R3),0(RE)                                                   
         LLC   R1,DELCT                                                         
         LA    R1,1(R1)            BUMP DELETED COUNT                           
         STC   R1,DELCT                                                         
         XC    ACTUAL,ACTUAL                                                    
         IC    R1,0(R2)                                                         
         AR    R2,R1               POINT TO PCT FIELD                           
         B     VBPAT70             OLD VRSN USED SLOT FOR DEL CMML              
*                                                                               
VBPAT40  DS    0H                                                               
         CLI   5(R2),8                                                          
         BL    CMLENERR            MUST BE 8 CHAR                               
*                                                                               
         LLC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         MVC   WORK,SPACES                                                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)                                                    
*                                                                               
         LA    R1,SVTYPE1                                                       
         MVI   PRDMATSW,0                                                       
         BRAS  RE,VCML                                                          
*                                                                               
         MVC   0(8,R3),WORK+12     MOVE NEW CMML TO ELEM                        
                                                                                
*===============================================================                
* CHECK FOR DUPLICATE COMMERCIAL                                                
*===============================================================                
                                                                                
         LA    R1,NEWELEM          FIRST COMMERCIAL IN NEW ELEM                 
*                                                                               
VBPAT50  CR    R1,R3               AT THIS COMMERCIAL (PAIR)                    
         BE    VBPAT60             YES, NO EQUALS                               
         BL    *+6                 THEN MUST BE LOW                             
         DC    H'0'                                                             
         CLC   0(16,R1),0(R3)      CK IF EQUAL                                  
         BNE   VBPAT52                                                          
         CLI   0(R1),C'*'          IF DELETED COMML, OKAY                       
         BNE   EQCMLER1             NO                                          
*                                                                               
VBPAT52  LA    R1,16(R1)           POINT TO NEXT COMMERCIAL (PAIR)              
         B     VBPAT50                                                          
*                                                                               
VBPAT60  LLC   RE,0(R2)            GET FIELD LENGTH                             
         AR    R2,RE               POINT TO % FIELD                             
         CLI   5(R2),0             TEST PCT INPUT                               
         JE    MISSERR             NO                                           
*                                                                               
         GOTO1 VALINUM                                                          
*                                                                               
         LLC   R0,PCTTOT                                                        
         LLC   RE,ACTUAL                                                        
         AR    R0,RE                                                            
         STC   R0,PCTTOT                                                        
*                                                                               
VBPAT70  LLC   R1,CMMLCT                                                        
         LA    R1,1(,R1)                                                        
         STC   R1,CMMLCT                                                        
*                                                                               
         L     RE,PCTNXT                                                        
*                                                                               
         LLC   RE,PCTCT            GET SLOT NUMBER THIS CMML                    
         MHI   RE,3                                                             
         LAY   RF,SPTRRTAB         ROTATION TABLE                               
         AR    RF,RE                                                            
*                                                                               
         L     RE,PCTNXT                                                        
         MVC   0(1,RE),2(RF)       MOVE CMML IDENTIFIER                         
         MVC   2(1,RE),ACTUAL      MOVE PCT                                     
         LA    RE,3(RE)                                                         
         ST    RE,PCTNXT                                                        
*                                                                               
         LA    R3,16(R3)           NEXT SLOT IN ELEM                            
                                                                                
         LLC   RE,0(R2)            GET FIELD LENGTH                             
         AR    R2,RE               POINT TO NEXT FIELD (PROT)                   
         LLC   RE,0(R2)            GET FIELD LENGTH                             
         AR    R2,RE               POINT TO CML FIELD                           
         LLC   RE,PCTCT                                                         
         LA    RE,1(RE)                                                         
         STC   RE,PCTCT            BUMP PCT SLOT COUNTER                        
*                                                                               
VBPAT80  LA    R0,PCOPC24H                                                      
         CR    R2,R0                                                            
         BL    VBPAT30                                                          
*                                                                               
VBPAT90  CLI   CMMLCT,0            ANY ENTRIES FOUND                            
         BE    CMLMISR1            NO                                           
*                                                                               
         CLI   PCTTOT,100                                                       
         BE    *+12                                                             
         CLI   PCTTOT,99                                                        
         BNE   ROTPCERR            ERROR MUST BE 99/100 PCT                     
                                                                                
* SEE IF ANY COMMERCIALS HAVE CHANGED                                           
                                                                                
         LA    R0,OLDELEM                                                       
         LHI   R1,OLDELEMX-OLDELEM                                              
         LA    RE,NEWELEM                                                       
         LR    RF,R1                                                            
         CLCL  R0,RE                                                            
         BE    *+8                                                              
         MVI   NEWSUBSW,1                                                       
                                                                                
* SEE IF ANY PERCENTS HAVE CHANGED                                              
                                                                                
         L     R6,AIO              PERCENTAGE ELEMENT                           
         MVI   ELCODE,X'34'                                                     
         BRAS  RE,GETEL                                                         
         JNE   VBPAT92                                                          
         LLC   RE,1(R6)                                                         
         AHI   RE,-3               ADJUST FOR ELCODE/LEN/SET FOR EX             
         EX    RE,*+8              COMPARE NEW DATA IN PCTELEM                  
         B     *+10                                                             
         CLC   PCTELEM(0),2(R6)                                                 
         BE    *+8                                                              
         MVI   NEWSUBSW,1                                                       
                                                                                
* ADD MULTIPLE 31 ELEMS IF CMMLCT > 12 COMMERCIALS                              
                                                                                
VBPAT92  LA    R3,NEWELEM          START OF NEW ELEM                            
         LA    R2,1                ELEMENT NUMBER                               
*                                                                               
VBPAT100 XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING PATBCMEL,R6                                                      
         MVI   PATBCMEL,X'31'                                                   
*                                                                               
         MVC   BYTE,CMMLCT         CML COUNT                                    
         CLI   CMMLCT,MAXCELEM     12 CMLS PER ELEM                             
         BH    *+12                                                             
         MVI   CMMLCT,0            DONE ADDING CML ELEM                         
         B     VBPAT102                                                         
*                                                                               
         LLC   RE,CMMLCT           NO. OF CMLS TO ADD                           
         SHI   RE,MAXCELEM         MINUS MAX THAT FITS IN ONE ELEM              
         STC   RE,CMMLCT                                                        
         MVI   BYTE,MAXCELEM                                                    
*                                                                               
VBPAT102 LLC   R1,BYTE             NUMBER OF CML TO ADD                         
         SLL   R1,4                MULT BY ENTRY LEN (16)                       
         LA    R1,3(R1)            ADD 3 FOR ELEM/LEN/ELEM NUMBER               
         STC   R1,PATBCMLN         ELEM LEN                                     
         STC   R2,PATBCMNO         ELEMENT NUMBER                               
*                                                                               
         SHI   R1,4                ADJ FOR EX MOVE                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PATBCML(0),0(R3)    MOVE CMLS TO ELEM                            
*                                                                               
         AHI   R1,1                ADJ FOR REAL LEN TO                          
         AR    R3,R1               BUMP TO NEXT SET OF CMLS TO ADD              
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         LA    R2,1(R2)            INCREMENT ELEM NUMBER                        
*                                                                               
         CLI   CMMLCT,0            DONE ADDING CML ELEM                         
         BNE   VBPAT100                                                         
*                                                                               
* TURN OFF INCOMPLETE RECORD FLAG IN 10 ELEM                                    
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
         NI    PATSTAT1,X'FF'-PATSINC TURN OFF INCOMPLETE REC FLAG              
                                                                                
* ADD X'34' COMMERCIAL ROTATION ELEMENT                                         
                                                                                
         MVI   ELCODE,X'34'                                                     
         GOTO1 REMELEM             DELETE OLD X'34' ELEM                        
*                                                                               
         XC    ELEM,ELEM                                                        
         MVC   ELEM+2(150),PCTELEM                                              
         MVI   ELEM,X'34'                                                       
         L     RE,PCTNXT                                                        
         LA    R0,PCTELEM                                                       
         SR    RE,R0                                                            
         AHI   RE,2                                                             
         STC   RE,ELEM+1                                                        
         GOTO1 ADDELEM                                                          
         XIT1                                                                   
*                                                                               
CMLMISR1 LA    R2,TCMCMLAH         FIRST COMMERCIAL FIELD                       
         MVI   ERROR,MISSING                                                    
         J     ERRX1                                                            
*                                                                               
ROTERR1  MVI   ERROR,INVCMLRT      MISSING/EXTRA ROTATION CHAR                  
         J     ERRX1                                                            
*                                                                               
NUMERR1  MVI   ERROR,NOTNUM                                                     
         J     ERRX1                                                            
*                                                                               
INVDELR1 MVI   ERROR,INVCMLDL      CAN'T DELETE AN EMPTY FLD                    
         J     ERRX1                                                            
*                                                                               
EQCMLER1 MVI   ERROR,DUPLCMML      2 PIGGY-BACK COMMERCIALS EQUAL               
ERRX1    GOTO1 ERREX                                                            
*                                                                               
HIATERR  LHI   R0,HIATTHTR        NO HIATUS FOR THEATRICAL                      
         J     ERREXIT1                                                         
*                                                                               
ROTPCERR LHI   R0,ROTPCT          PCTS MUST ADD UP TO 99 OR 100                 
         J     ERREXIT1                                                         
*                                                                               
CMLENERR LHI   R0,CMLLENMS        COMMERCIAL MUST BE 8 CHARS                    
*                                                                               
ERREXIT1 STH   R0,GERROR                                                        
         GOTO1 VTRAERR                                                          
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPTRRTAB                                                       
         EJECT                                                                  
* VALIDATE REFERENCE NUMBER                                                     
         EJECT                                                                  
*============================================================                   
* BUILD INFO LINE TO DISPLAY PATTERN NAME/PERIOD/TIMES/DPT                      
*============================================================                   
                                                                                
BLDINFO  NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING PATDTAEL,R6                                                      
         XC    WORK,WORK                                                        
         MVC   WORK(16),PATDESC                                                 
*                                                                               
         LA    R4,WORK+18                                                       
         GOTO1 DATCON,DMCB,(3,PATSTART),(8,0(R4))                               
         MVC   8(4,R4),=C'-UFN'                                                 
         CLC   PATEND,=X'FFFFFF'                                                
         BE    BLDINF2                                                          
         GOTO1 (RF),(R1),(3,PATEND),(8,9(R4))                                   
*                                                                               
BLDINF2  LA    R4,WORK+37                                                       
         OC    PATSTIM(4),PATSTIM                                               
         BZ    BLDINF4                                                          
         GOTO1 UNTIME,DMCB,PATSTIM,0(R4)                                        
         B     BLDINFX                                                          
*                                                                               
BLDINF4  CLI   PATDPT,0                                                         
         BE    BLDINFX                                                          
         MVC   0(4,R4),=C'DPT='                                                 
         MVC   4(1,R4),PATDPT                                                   
*                                                                               
BLDINFX  J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
VREF     NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PATKEY,R4                                                        
         MVC   PATKID,=XL2'0A22'                                                
         MVC   PATKAM(7),BAGYMD    BCLT,BPRD,BSLN,BPRD2,BSLN2                   
         MVC   PATKCODE,CODE                                                    
         CLI   ACTNUM,ACTADD       IS THIS ADD                                  
         BE    VREF60                                                           
         XC    BREFSUB,BREFSUB                                                  
         CLI   5(R2),0             ANY ENTRY                                    
         BE    MISSERRV                                                         
VREF10   TM    4(R2),X'08'         WAS IT NUMERIC                               
         BZ    NUMERR                                                           
         GOTO1 ANY                                                              
         LLC   R1,5(R2)            GET DATA LENGTH                              
         BCTR  R1,0                                                             
         EX    R1,VKPACK                                                        
         CVB   R0,DUB                                                           
         CH    R0,=H'16383'                                                     
         BH    BIGERR                                                           
         STH   R0,BREF                                                          
         X     R0,=XL4'00003FFF'                                                
         SLL   R0,10                                                            
         STCM  R0,7,BREFSUB                                                     
         STCM  R0,7,PATKREF        COMBINED REF NUMBER/GENERATED SUBLIN         
         OI    DMINBTS,X'08'       READ DELETED RECS                            
         LA    R0,8                                                             
         BRAS  RE,TRHIGH                                                        
         GOTO1 HIGH                                                             
         NI    DMOUTBTS,X'FD'      TURN OFF DELETED REC BIT                     
         NI    DMINBTS,X'F7'       RESET READ DELETED RECS                      
         CLC   KEY(10),KEYSAVE                                                  
         BNE   NOTFNDER                                                         
         SR    R0,R0                                                            
         ICM   R0,7,PATKREF                                                     
         STCM  R0,7,BREFSUB                                                     
         SRDL  R0,10                                                            
         X     R0,=XL4'00003FFF'                                                
         CH    R0,BREF                                                          
         BNE   NOTFNDER                                                         
         SRL   R1,22                                                            
         X     R1,=XL4'000003FF'                                                
         STH   R1,BSUB                                                          
VREFX    XIT1                                                                   
         EJECT                                                                  
* FIND REFERENCE NUMBER FOR ADDED RECORD                                        
*                                                                               
VREF60   OI    DMINBTS,X'08'       READ DELETED RECS                            
         LA    R0,9                                                             
         BRAS  RE,TRHIGH                                                        
         GOTO1 HIGH                                                             
         NI    DMOUTBTS,X'FD'      TURN OFF DELETED REC BIT                     
         NI    DMINBTS,X'F7'       RESET READ DELETED RECS                      
         CLC   KEY(10),KEYSAVE                                                  
         BE    VREF62                                                           
         MVC   BREF(2),=XL2'0001'                                               
         MVC   BSUB(2),=XL2'0001'                                               
         MVC   BREFSUB,=XL3'FFFBFE'                                             
         B     VREF64                                                           
VREF62   SR    R1,R1                                                            
         ICM   R1,7,KEY+10         GET THIS REF/SUB                             
         SRL   R1,10               NOW HAVE REF ONLY                            
         X     R1,=XL4'00003FFF'                                                
         LA    R1,1(,R1)           ADD 1                                        
         STH   R1,BREF                                                          
         X     R1,=XL4'00003FFF'                                                
         LR    R0,R1                                                            
         SR    R1,R1                                                            
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         SLL   R1,22                                                            
         SLDL  R0,10                                                            
         STCM  R0,7,BREFSUB                                                     
VREF64   LH    R1,BREF                                                          
         EDIT  (R1),(5,TRAREF),ALIGN=LEFT                                       
         OI    TRAREFH+6,X'80'                                                  
         B     VREFX                                                            
VKPACK   PACK  DUB,WORK(0)                                                      
BIGERR   MVI   ERROR,INVREFSZ      REF NUMBER TOO LARGE                         
         J     TRAPERRV                                                         
NOTFNDER MVI   ERROR,NOTFOUND                                                   
         J     TRAPERRV                                                         
MISSERRV MVI   ERROR,MISSING                                                    
         J     TRAPERRV                                                         
NUMERR   MVI   ERROR,NOTNUM                                                     
*                                                                               
TRAPERRV GOTO1 ERREX                                                            
         DROP  R4                                                               
         EJECT                                                                  
*=============================================================                  
* VALIDATE MARKET/STATION LIST                                                  
* TYPE OF LIST CAN BE A = ALL MARKETS                                           
*                     T = STATION TYPE                                          
*                     M = MARKET LIST                                           
*                     S = STATION LIST                                          
*                     C = COMBINED MARKET/AFFILIATE                             
*                     G = MARKET GROUP                                          
*=============================================================                  
                                                                                
VMS      NTR1  BASE=*,LABEL=*                                                   
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING PATLSTEL,R6                                                      
         MVI   PATLSTEL,X'20'      ELEMENT IDENTIFIER                           
*                                                                               
* FIRST CHECK TYPE FIELD, AND IF DATA, ONLY CHECK THAT TYPE                     
* OTHERWISE CHECK BY ENTRY                                                      
*                                                                               
         BAS   RE,VTYP             GO VALIDATE TYPE LIST IF ANY                 
         BE    VMSX                 ALL DONE IN VTYP                            
*                                                                               
         GOTO1 ANY                                                              
         CLC   =CL3'ALL',WORK       ALL MARKETS                                 
         BNE   VMS10                                                            
         MVI   PATLSTTY,C'M'                                                    
         XC    PATLST(5),PATLST                                                 
         B     VMS14                                                            
*                                                                               
VMS10    CLC   =CL5'TYPE=',WORK    STATION TYPE                                 
         BNE   VMS20                                                            
*                                                                               
         CLI   TRAMS1H+5,6                                                      
         BNE   TYPERR                                                           
         CLI   WORK+5,C' '                                                      
         BNH   TYPERR                                                           
         MVI   PATLSTTY,C'T'                                                    
         MVC   PATLST(1),WORK+5                                                 
         XC    PATLST+1(4),PATLST+1                                             
VMS14    MVI   PATLSTLN,8                                                       
         B     VMSX                GO ADD ELEMENT                               
*                                                                               
VMS20    CLC   =CL4'AFF=',WORK     STATION AFFILIATE                            
         BNE   VMS30                                                            
         LLC   R5,5(R2)            GET INPUT LENGTH                             
         SH    R5,=H'4'                                                         
         LA    R4,WORK+4                                                        
         BAS   RE,VAFF                                                          
         B     VMSX                                                             
*                                                                               
VMS30    BAS   RE,SCAN                                                          
*                                                                               
         TM    2(R5),X'80'         IF NOT NUMERIC                               
         BZ    VMS50               ASSUME STATION                               
         MVI   PATLSTTY,C'M'                                                    
         B     VMS54                                                            
VMS50    MVI   PATLSTTY,C'S'                                                    
*                                                                               
VMS54    BAS   RE,VCOM             COM VALIDATE FOR MKT, STA, COMBINED          
*                                                                               
VMSX     XIT1                                                                   
*                                                                               
* VALIDATE TYPE OF PATTERN, IF ENTERED - THEN USE IT TO VALIDATE LIST *         
*   SEE TYPTAB FOR ALL POSSIBLE TYPES                                           
*                                                                               
VTYP     NTR1                                                                   
         LA    R2,TRALTYH                                                       
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VTYP30               NO                                          
         GOTO1 ANY                                                              
*                                                                               
         LA    RE,TYPTAB                                                        
         LA    RF,TYPTABNU                                                      
VTYP10   CLC   WORK(1),0(RE)                                                    
         BE    VTYP20                                                           
         LA    RE,TYPTABLN(,RE)                                                 
         BCT   RF,VTYP10                                                        
         B     BADTYPER                                                         
VTYP20   SR    RF,RF                                                            
         ICM   RF,3,1(RE)                                                       
         AR    RF,RB                                                            
*                                                                               
         LA    R2,TRAMS1H                                                       
         LA    R4,TRAMS1                                                        
         LLC   R5,TRAMS1H+5        GET INPUT LENGTH                             
*                                                                               
         BASR  RE,RF                                                            
         CR    RE,RE                                                            
         B     VMSX                                                             
VTYP30   CR    RB,RC                                                            
         B     VMSX                                                             
*                                                                               
* STATION TYPE - 1 CHARACTER, ONLY 1 ENTRY ALLOWED *                            
*                                                                               
VTY      NTR1                                                                   
         MVI   PATLSTTY,C'T'                                                    
         LA    R2,TRAMS1H                                                       
         MVC   PATLST(1),TRAMS1                                                 
         XC    PATLST+1(4),PATLST+1                                             
         MVI   PATLSTLN,8                                                       
         B     VMSX                                                             
         EJECT                                                                  
* LIST OF STATION AFFILIATES *                                                  
*                                                                               
VAFF     NTR1                                                                   
         MVI   PATLSTTY,C'A'                                                    
         MVI   PATLSTLN,8                                                       
*                                                                               
         LA    R2,TRAMS1H                                                       
         LA    R3,PATLST                                                        
         SR    RF,RF                                                            
*                                                                               
VAFF20   LA    R0,STAFFCT                                                       
         LA    R1,STAFFTAB                                                      
*                                                                               
         CLI   QMED,C'R'           IF RADIO                                     
         BNE   VAFF24                                                           
         CLC   AGENCY,=C'TH'       AND ZENITH                                   
         BE    VAFF22                                                           
         CLC   AGENCY,=C'BS'       OR  BACKER                                   
         BNE   VAFF24                                                           
VAFF22   LA    R0,STAFBSCT                                                      
         LA    R1,STAFBSTB                                                      
*                                                                               
VAFF24   CLC   0(3,R4),0(R1)                                                    
         BE    VAFF26                                                           
         LA    R1,3(,R1)                                                        
         BCT   R0,VAFF24                                                        
         B     AFFILER                                                          
VAFF26   MVC   0(3,R3),0(R4)                                                    
         XC    3(2,R3),3(R3)                                                    
         LA    R3,5(,R3)                                                        
         SH    R5,=H'3'                                                         
         BZ    VAFF30              GO CHECK SORT                                
*                                                                               
         CLI   3(R4),C','          IS THERE ANOTHER AFFILIATE                   
         BNE   AFFERR                                                           
         BCTR  R5,0                SUBTRACT 1 FOR COMMA                         
         LA    R4,4(,R4)                                                        
         LLC   R1,PATLSTLN                                                      
         LA    R1,5(,R1)                                                        
         STC   R1,PATLSTLN                                                      
         LA    RF,1(,RF)                                                        
         LR    R0,RF                                                            
         LA    R1,PATLST                                                        
VAFF28   CLC   0(3,R1),0(R4)                                                    
         BE    AFFDUPER                                                         
         LA    R1,5(,R1)                                                        
         BCT   R0,VAFF28                                                        
         B     VAFF20                                                           
*                                                                               
VAFF30   LTR   RF,RF               MORE THAN 1 ENTRY                            
         BZ    VMSX                 NO                                          
         LA    R3,1(,RF)                                                        
         GOTO1 XSORT,DMCB,PATLST,(R3),5,5,0                                     
         B     VMSX                GO ADD ELEM                                  
         EJECT                                                                  
* VALIDATE MARKET GROUP LIST                                                    
*                                                                               
VMGR     NTR1                                                                   
*                                                                               
         MVI   PATLSTTY,C'G'                                                    
         LA    R1,3                MAX POSSIBLE MARKET/STATION LINES            
         LA    R3,PATLST           SLOT FOR 1ST MKT GRP IN ELEM                 
VMGR10   ST    R1,MKTSTACT                                                      
         MVI   MSCT,1              SET LINE ENTRY CTR TO 1                      
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VMGR50              NO                                           
*                                                                               
         BAS   RE,SCAN                                                          
*                                                                               
VMGR20   CLI   0(R5),2                                                          
         BL    MGRENTER                                                         
         CLI   0(R5),6                                                          
         BH    MGRENTER                                                         
         CLI   12(R5),C'A'                                                      
         BL    MGRENTER                                                         
         CLI   12(R5),C'Z'                                                      
         BH    MGRENTER                                                         
*                                                                               
         MVC   MKTGID,12(R5)       MARKET GROUP ID                              
*                                                                               
         LA    R1,12+1(,R5)                                                     
         LA    RE,DUB                                                           
         MVC   DUB(4),=C'0000'                                                  
*                                                                               
         LLC   RF,0(R5)            LENGTH OF INPUT                              
         BCTR  RF,0                                                             
*                                                                               
         CLI   0(R1),C'A'          SEE IF 2 CHAR MARKET GROUP                   
         BL    VMGR30                                                           
         CLI   0(R1),C'Z'                                                       
         BH    VMGR30                                                           
         LA    R1,1(R1)                                                         
*                                                                               
* CONVERT 2 CHAR MARKET GROUP TO 1 CHAR                                         
*                                                                               
         L     RE,=A(SPMGRTAB)                                                  
         A     RE,SPTR03RR                                                      
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
*                                                                               
VMGR22   CLC   12(2,R5),0(RE)      IS THIS IT                                   
         BE    VMGR24                                                           
         LA    RE,3(RE)                                                         
         BCT   RF,VMGR22                                                        
*                                                                               
         B     MGRNFND                                                          
*                                                                               
VMGR24   MVC   MKTGID,2(RE)        MOVE HEX VALUE FROM TABLE                    
*                                                                               
         LA    RE,DUB                                                           
         LLC   RF,0(R5)            LENGTH OF INPUT                              
         SHI   RF,2                                                             
*                                                                               
VMGR30   CLI   0(R1),C'0'                                                       
         BL    MGRENTER                                                         
         CLI   0(R1),C'9'                                                       
         BH    MGRENTER                                                         
         MVC   0(1,RE),0(R1)       MOVE DIGIT TO DUB                            
         LA    R1,1(,R1)           BUMP IN INPUT FIELD                          
         LA    RE,1(,RE)           AND IN DUB                                   
         BCT   RF,VMGR30                                                        
*                                                                               
         PACK  WORK(3),DUB(5)                                                   
         MVC   0(1,R3),MKTGID      MARKET GROUP ID                              
         MVC   1(2,R3),WORK                                                     
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(3),BAGYMD    & BCLT                                        
         MVC   KEY+8(3),0(R3)                                                   
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'SPTDIR' SWTICH TO SPOT MEDIA                       
         GOTO1 HIGH                                                             
         OC    KEY+5(3),KEY+5      FIND PRODUCT GROUP                           
         BZ    VMGR36                                                           
         CLC   KEY(5),KEYSAVE                                                   
         BE    VMGR34                                                           
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLIENT                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE                                                   
         BNE   NOMGRPER                                                         
*                                                                               
VMGR34   CLC   KEY+8(3),0(R3)                                                   
         BE    VMGR36                                                           
         MVC   KEY+8(3),0(R3)                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
VMGR36   CLC   KEY(11),KEYSAVE     FIND A MARKET GROUP                          
         BE    VMGR38                                                           
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      TRY ALL CLIENT                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
VMGR38   CLC   KEY(11),KEYSAVE     FIND A MARKET GROUP                          
         BNE   NOMGRPER                                                         
*                                                                               
         LLC   R1,MSCT             GET MKT/STA CTR                              
         LA    R1,1(,R1)           UPDATE ENTRIES FOR  THIS LINE                
         STC   R1,MSCT             COUNTER                                      
*                                                                               
         LA    R5,32(,R5)                                                       
         LA    R3,5(,R3)                                                        
         BCT   R4,VMGR20                                                        
*                                                                               
VMGR50   LLC   R0,0(R2)            GET FIELD LENGTH                             
         AR    R2,R0               SKIP TO NEXT FIELD                           
*                                                                               
         LLC   R0,0(R2)            GET FIELD LENGTH                             
         AR    R2,R0               SKIP OVER TITLE                              
*                                                                               
         L     R1,MKTSTACT                                                      
         BCT   R1,VMGR10                                                        
*                                                                               
         LA    R0,PATLST           SLOT FOR 1ST MKT/STA IN ELEM                 
         CR    R0,R3                                                            
         BNL   MISSERRT                                                         
         SR    R3,R6                                                            
         STC   R3,PATLSTLN                                                      
         LR    R1,R3               MOVE ELEM LENGTH                             
         BCTR  R1,0                GET                                          
         BCTR  R1,0                    ENTRIES                                  
         BCTR  R1,0                            LEN ONLY                         
         SR    R0,R0               FOR DIVIDE                                   
         D     R0,=F'5'            DIVIDE BY ENTRY LEN                          
         LTR   R0,R0                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         CH    R1,=H'1'            IF ONLY 1 ENTRY                              
         BE    VMGR60              BYPASS SORT                                  
         LR    R3,R1               SAVE NUMBER OF ENTRIES IN LIST               
         GOTO1 XSORT,DMCB,PATLST,(R3),5,5,0                                     
*                                                                               
VMGR60   XC    FILENAME,FILENAME                                                
         B     VMSX                GO ADD ELEM                                  
*                                                                               
MGRNFND  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(26),=C'* ERROR * MKT GROUP SCHEME'                       
         MVC   CONHEAD+28(2),12(R5)                                             
         MVC   CONHEAD+31(14),=C'DOES NOT EXIST'                                
         GOTO1 ERREX2                                                           
*                                                                               
         EJECT                                                                  
* VALIDATE MARKET LIST                                                          
*                                                                               
VMKT     NTR1                                                                   
         MVI   PATLSTTY,C'M'                                                    
*                                                                               
         BAS   RE,VCOM             COM VALIDATE FOR MKT, STA, COMBINED          
*                                                                               
         B     VMSX                GO ADD ELEM                                  
*                                                                               
* VALIDATE STATION LIST                                                         
*                                                                               
VSTA     NTR1                                                                   
         MVI   PATLSTTY,C'S'                                                    
*                                                                               
         BAS   RE,VCOM             COM VALIDATE FOR MKT, STA, COMBINED          
*                                                                               
         B     VMSX                GO ADD ELEM                                  
*                                                                               
* VALIDATE COMBINED MARKET/AFFILIATE LIST                                       
*                                                                               
VCMB     NTR1                                                                   
         MVI   PATLSTTY,C'C'                                                    
*                                                                               
         BAS   RE,VCOM             COM VALIDATE FOR MKT, STA, COMBINED          
*                                                                               
         B     VMSX                GO ADD ELEM                                  
         EJECT                                                                  
* USED FOR MARKET, STATION, AND COMBINED MARKET-AFFILIATE LISTS *               
VCOM     NTR1                                                                   
*                                                                               
         MVC   FLDH,0(R2)                                                       
         MVI   HOLDSIGN,0                                                       
*                                                                               
         CLI   PATLSTTY,C'M'       MARKET LIST PATTERN                          
         BNE   VCOM10               NO                                          
         CLI   5(R2),3             ANY ENTRY                                    
         BNE   VCOM10               NO                                          
         CLC   =C'ALL',8(R2)       THIS ALL MARKETS                             
         BNE   VCOM10               NO                                          
         MVI   PATLSTLN,8                                                       
         XC    PATLST(5),PATLST                                                 
         B     VMSX                GO ADD ELEM                                  
*                                                                               
VCOM10   LA    R1,3                MAX POSSIBLE MARKET/STATION LINES            
         LA    R3,PATLST           SLOT FOR 1ST MKT/STA IN ELEM                 
VCOM14   ST    R1,MKTSTACT                                                      
         MVI   MSCT,1              SET LINE ENTRY CTR TO 1                      
         CLI   5(R2),0             ANY ENTRY                                    
         BE    VCOM50              NO                                           
*                                                                               
         BAS   RE,SCAN                                                          
*                                                                               
VCOM16   LA    R0,ELEM+253         50*5+3                                       
         CR    R3,R0               MAX (50) ENTRIES IN ELEMENT                  
         BNL   MAXLSTER                                                         
*                                                                               
         MVC   FLDH+5(1),0(R5)     DATA LENGTH                                  
         PACK  FLDH+4(1),2(1,R5)   INVERT BITS                                  
         MVC   FLD(10),12(R5)      MOVE IN STAION/MARKET                        
         SR    R2,RA               MAKE R2 RELATIVE ADDR                        
         ST    R2,SVR2             SAVE FOR ERROR RETURN                        
         LA    R2,FLDH                                                          
         MVI   ERROR,0                                                          
         MVI   ERROPT,C'Y'         SET ERROPT FOR RETURN HERE                   
*                                                                               
         CLI   PATLSTTY,C'S'       STATION                                      
         BE    VCOM30                                                           
         CLI   PATLSTTY,C'M'       MARKET                                       
         BE    VCOM32                                                           
         CLI   PATLSTTY,C'C'       COMBINED MARKET/AFFIALIATE                   
         BE    VCOM20                                                           
         DC    H'0'                                                             
*                                                                               
VCOM20   TM    2(R5),X'80'         IF NUMERIC                                   
         BO    VCOM32               ASSUME MARKET                               
         LA    R0,STAFFCT                                                       
         LA    R1,STAFFTAB                                                      
*                                                                               
         CLI   QMED,C'R'           IF RADIO                                     
         BNE   VCOM24                                                           
         CLC   AGENCY,=C'TH'       AND ZENITH                                   
         BE    VCOM22                                                           
         CLC   AGENCY,=C'BS'       AND BACKER                                   
         BNE   VCOM24                                                           
VCOM22   LA    R0,STAFBSCT                                                      
         LA    R1,STAFBSTB                                                      
*                                                                               
VCOM24   CLC   12(3,R5),0(R1)                                                   
         BE    VCOM26                                                           
         LA    R1,3(,R1)                                                        
         BCT   R0,VCOM24                                                        
         MVI   ERROPT,0            RESET ERROPT                                 
         L     R2,SVR2             RESTORE                                      
         AR    R2,RA               MAKE R2 REAL                                 
         B     AFFILER                                                          
*                                                                               
VCOM26   MVC   0(3,R3),12(R5)                                                   
         XC    3(2,R3),3(R3)                                                    
         OI    HOLDSIGN,X'F0'                                                   
         B     VCOM34                                                           
*                                                                               
VCOM30   GOTO1 VALISTA                                                          
*                                                                               
         MVC   0(5,R3),QSTA                                                     
*                                                                               
         OC    STANET,STANET                                                    
         BZ    VCOM34                                                           
         XC    0(2,R3),0(R3)                                                    
         MVC   2(3,R3),BSTA                                                     
         B     VCOM34                                                           
VCOMVN   MVN   DUB(0),12(R5)                                                    
VCOMCLC  CLC   DUB(0),12(R5)                                                    
*                                                                               
VCOM32   CLI   0(R5),4                                                          
         BH    BADMS                                                            
         CLI   0(R5),0             NO INPUT                                     
         BE    BADMS                                                            
         MVC   DUB(4),=C'0000'                                                  
         IC    RF,0(R5)                                                         
         BCTR  RF,0                                                             
         EX    RF,VCOMVN                                                        
         EX    RF,VCOMCLC                                                       
         BNE   BADMS                                                            
         GOTO1 VALIMKT                                                          
         XC    0(3,R3),0(R3)                                                    
         MVC   3(2,R3),BMKT                                                     
         OI    HOLDSIGN,X'0F'                                                   
*                                                                               
VCOM34   LLC   R1,MSCT             GET MKT/STA CTR                              
         MVI   ERROPT,0            RESET ERROPT                                 
         CLI   ERROR,0             IF NON-ZERO, ERROR                           
         BNE   BADMS               BAD MARKET/STATION                           
         LA    R1,1(,R1)           UPDATE ENTRIES FOR  THIS LINE                
         STC   R1,MSCT             COUNTER                                      
*                                                                               
         LA    R1,PATLST           START OF LIST                                
VCOM36   CR    R1,R3               AT END OF LIST?                              
         BNL   VCOM40              YES                                          
         CLC   0(5,R1),0(R3)       EQUAL ENTRY?                                 
         BE    DUPLMSER            YES, ERROR                                   
         LA    R1,5(,R1)           NEXT SLOT                                    
         B     VCOM36              SEE IF END OF LIST                           
*                                                                               
VCOM40   L     R2,SVR2                                                          
         AR    R2,RA               CHANGE FROM REL TO ACTUAL                    
         LA    R3,5(,R3)                                                        
         LA    R5,32(,R5)                                                       
         BCT   R4,VCOM16                                                        
*                                                                               
VCOM50   LLC   R0,0(R2)            GET FIELD LENGTH                             
         AR    R2,R0               SKIP TO NEXT FIELD                           
*                                                                               
         LLC   R0,0(R2)            GET FIELD LENGTH                             
         AR    R2,R0               SKIP OVER TITLE                              
*                                                                               
         L     R1,MKTSTACT                                                      
         BCT   R1,VCOM14                                                        
*                                                                               
         LA    R0,PATLST           SLOT FOR 1ST MKT/STA IN ELEM                 
         CR    R0,R3                                                            
         BNL   MISSERRT                                                         
         SR    R3,R6                                                            
         STC   R3,PATLSTLN                                                      
         LR    R1,R3               MOVE ELEM LENGTH                             
         BCTR  R1,0                GET                                          
         BCTR  R1,0                    ENTRIES                                  
         BCTR  R1,0                            LEN ONLY                         
         SR    R0,R0               FOR DIVIDE                                   
         D     R0,=F'5'            DIVIDE BY ENTRY LEN                          
         LTR   R0,R0                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         CH    R1,=H'1'            IF ONLY 1 ENTRY                              
         BE    VCOM60              BYPASS SORT                                  
         LR    R3,R1               SAVE NUMBER OF ENTRIES IN LIST               
         GOTO1 XSORT,DMCB,PATLST,(R3),5,5,0                                     
VCOM60   MVC   AIO,AIO3                                                         
         CLI   PATLSTTY,C'C'       COMBINED MARKET/AFFIALIATE                   
         BNE   VMSX                 NO, GO ADD ELEM                             
         CLI   HOLDSIGN,X'FF'      GET BOTH MARKET AND AFFIL                    
         BE    VMSX                 YES, GO ADD ELEM                            
         B     AFFMKTER                                                         
*                                                                               
* SCANNER RTN - USED FOR MKT LIST, STA LIST, COMBINED MKT/AFFIL                 
SCAN     NTR1                                                                   
         L     R5,AIO2                                                          
         GOTO1 SCANNER,DMCB,(R2),(25,(R5)),0                                    
         LLC   R4,DMCB+4           COUNT OF BLOCKS FOUND                        
         XIT1  REGS=(R4,R5)                                                     
         DROP  R6                                                               
*                                                                               
DUPLMSER LHI   R0,DUPMSER       ENTRIES &1 & &2 IN MKT/STA LIST ARE             
         STH   R0,GERROR                                                        
*                                   SAME (&3)                                   
*                                                                               
         LA    R2,TRAMS1H          FIRST MARKET/STATION LIST FLD                
         LA    R0,WORK                                                          
         STCM  R0,7,GASUBST                                                     
         XC    WORK,WORK                                                        
         MVI   WORK,03                                                          
         LA    R0,PATLST-PATLSTEL(,R6) GET START OF LIST                        
         SR    R1,R0                                                            
         SR    R0,R0                                                            
         D     R0,=F'5'            GET ENTRY NUMBER                             
         LA    R1,1(,R1)           POINT TO TROUBLE FLD                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+1(2),DUB+6(2)                                               
         MVI   WORK+3,03                                                        
         LA    R0,PATLST-PATLSTEL(,R6) GET START OF LIST                        
         LR    R1,R3                                                            
         SR    R1,R0                                                            
         SR    R0,R0                                                            
         D     R0,=F'5'            GET ENTRY NUMBER                             
         LA    R1,1(,R1)           POINT TO TROUBLE FLD                         
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+4(2),DUB+6(2)                                               
         MVI   WORK+6,5                                                         
         MVC   WORK+7(4),0(R3)                                                  
         CLI   0(R3),C' '          SEE IF ALPHA                                 
         BH    VMSERX2                                                          
         SR    R1,R1                                                            
         ICM   R1,3,3(R3)          GET MARKET                                   
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+7(4),DUB                                                    
         B     VMSERX2                                                          
*                                                                               
BADMS    L     R2,SVR2                                                          
         AR    R2,RA                                                            
         MVI   ERROPT,0            RESET ERROPT                                 
         MVC   GERROR,=Y(INVMSENT) ENTRY XX IS AN INVALID MARKET/STA-           
*                                   TION ENTRY                                  
         XC    ELEM,ELEM                                                        
         LA    R0,ELEM                                                          
         STCM  R0,7,GASUBST                                                     
         LLC   R1,MSCT             COUNTER                                      
         CVD   R1,DUB              SAVE ITEM COUNT                              
         OI    DUB+7,X'0F'                                                      
         MVI   ELEM,03                                                          
         UNPK  ELEM+1(2),DUB+6(2)                                               
         B     VMSERX2                                                          
MAXLSTER MVC   GERROR,=Y(MAXLSTEX)                                              
         B     VMSERX2                                                          
AFFMKTER MVC   GERROR,=Y(MISMKTMS)                                              
         LA    R2,TRAMS1H                                                       
         TM    HOLDSIGN,X'F0'      WAS AFFIL FOUND                              
         BO    VMSERX2                                                          
         MVC   GERROR,=Y(MISAFFMS)                                              
         TM    HOLDSIGN,X'0F'      WAS MKT FOUND                                
         BO    VMSERX2                                                          
         DC    H'0'                                                             
BADTYPER MVC   GERROR,=Y(BADTYPMS)                                              
         B     VMSERX2                                                          
*                                                                               
AFFDUPER MVC   GERROR,=Y(AFFDUPMS) AFFILIATE IS DUPLICATED                      
         B     VMSERX2                                                          
*                                                                               
AFFERR   MVC   GERROR,=Y(AFFENTER) ENTER 1 AFFILIATE OR AFF, AFF                
         LA    R2,TRAMS1H                                                       
         B     VMSERX2                                                          
*                                                                               
MGRENTER MVC   GERROR,=Y(BDMGRENT)                                              
         B     MGRERR                                                           
*                                                                               
NOMGRPER MVC   GERROR,=Y(NOMGRENT) ENTRY &1 NO MARKET GROUP FOUND               
*                                                                               
MGRERR   LLC   R3,MSCT             SAVE ITEM COUNT                              
         XC    ELEM,ELEM                                                        
         CVD   R3,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   ELEM,03                                                          
         UNPK  ELEM+1(2),DUB+6(2)                                               
         LA    R3,ELEM                                                          
         STCM  R3,7,GASUBST                                                     
         B     VMSERX2                                                          
*                                                                               
TYPERR   MVC   GERROR,=Y(STATYPER)                                              
         LA    R2,TRAMS1H                                                       
*                                                                               
VMSERX2  MVI   ERROPT,0            RESET ERROPT                                 
         GOTO1 VTRAERR                                                          
*                                                                               
MISSERRT MVI   ERROR,MISSING                                                    
         LA    R2,TRAMS1H                                                       
         B     VMSERX                                                           
*                                                                               
AFFILER  MVI   ERROR,INVAFFIL      AFFIL MUST BE ABC, CBS, NBC                  
VMSERX   GOTO1 ERREX                                                            
*                                                                               
TYPTAB   DC    C'A',AL2(VAFF-VMS)        STATION AFFILIATE                      
         DC    C'T',AL2(VTY-VMS)         STATION TYPE                           
         DC    C'M',AL2(VMKT-VMS)        MARKET LIST                            
         DC    C'S',AL2(VSTA-VMS)        STATION LIST                           
         DC    C'C',AL2(VCMB-VMS)        COMBINED MARKET/AFFILIATE LIST         
         DC    C'G',AL2(VMGR-VMS)        MARKET GROUP SCHEME                    
TYPTABNU EQU   (*-TYPTAB)/3                                                     
TYPTABLN EQU   3                                                                
STAFFTAB DC    CL3'ABC',CL3'CBS',CL3'NBC',C'FOX',C'UNI',C'TEL',C'IND'           
STAFFCT  EQU   (*-STAFFTAB)/3                                                   
STAFBSTB DC    CL3'AOR'                                                         
         DC    CL3'BAN'                                                         
         DC    CL3'BTF'                                                         
         DC    CL3'CHR'                                                         
         DC    CL3'CIH'                                                         
         DC    CL3'CON'                                                         
         DC    CL3'CRN'                                                         
         DC    CL3'CSH'                                                         
         DC    CL3'CTI'                                                         
         DC    CL3'CTR'                                                         
         DC    CL3'DLT'                                                         
         DC    CL3'ESY'                                                         
         DC    CL3'FRT'                                                         
         DC    CL3'GBD'                                                         
         DC    CL3'HPC'                                                         
         DC    CL3'JFO'                                                         
         DC    CL3'KAD'                                                         
         DC    CL3'LCK'                                                         
         DC    CL3'MES'                                                         
         DC    CL3'MOR'                                                         
         DC    CL3'NST'                                                         
         DC    CL3'NWS'                                                         
         DC    CL3'OLD'                                                         
         DC    CL3'PRT'                                                         
         DC    CL3'RCK'                                                         
         DC    CL3'SSC'                                                         
         DC    CL3'TEC'                                                         
         DC    CL3'TET'                                                         
         DC    CL3'TOP'                                                         
         DC    CL3'TRA'                                                         
         DC    CL3'TRC'                                                         
         DC    CL3'TRN'                                                         
         DC    CL3'TRO'                                                         
         DC    CL3'TRU'                                                         
         DC    CL3'TRY'                                                         
         DC    CL3'TSH'                                                         
         DC    CL3'URB'                                                         
         DC    CL3'WTR'                                                         
         DC    CL3'YGA'                                                         
         DC    CL3'ZZJ'                                                         
STAFBSCT EQU   (*-STAFBSTB)/3                                                   
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*===============================================================                
* VALIDATE COMMERCIAL ROTATION                                                  
*===============================================================                
                                                                                
VROT     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
* BUILD A TABLE IN VALTBL OF VALID COMMERCIAL LETTERS                           
* X'00'= INVALID X'01'=OK C'D'=DELETED                                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'30'        GET CMML ELEM                                
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LLC   R0,1(R6)                                                         
         SRL   R0,4                GIVES NUMBER OF CMMLS PRESENT                
         LA    R6,2(R6)                                                         
*                                                                               
         XC    VALTBL,VALTBL                                                    
         LA    R1,VALTBL                                                        
*                                                                               
VROT2    MVI   0(R1),1             ASSUME VALID                                 
         CLC   =X'5C00',0(R6)      TEST DELETED CMML                            
         BNE   *+8                                                              
         MVI   0(R1),C'D'                                                       
         LA    R6,16(R6)           NEXT CMML                                    
         LA    R1,1(R1)                                                         
         BCT   R0,VROT2                                                         
                                                                                
VROT10   MVC   GERROR,=Y(ROTORPCT) EITHER ROTATION OR PCTS                      
         LA    R2,TCMROTH                                                       
         CLI   5(R2),0             TEST ANY ROTATION INPUT                      
         BE    VROT11              NO                                           
         CLI   PCTTOT,0            HAVE ROT - TEST ANY PCTS INPUT               
         BE    VROT12              NO - GO VALIDATE ROTATION                    
         GOTO1 VTRAERR             ELSE SHOULD NOT INPUT BOTH                   
*                                                                               
VROT11   CLI   PCTTOT,0            NO ROT INPUT, SHOULD HAVE PCTS               
         BNE   VROT30              GO BUILD ROTATION ELEMENT                    
         GOTO1 VTRAERR                                                          
*                                                                               
VROT12   LLC   R0,5(R2)                                                         
         LA    R1,8(R2)                                                         
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM+2                                                        
*                                                                               
VROT14   LLC   RE,0(R1)            GET INPUT CHAR                               
         AHI   RE,-192             A=192 =1                                     
         CHI   RE,9                                                             
         BNH   *+8                                                              
         AHI   RE,-7               J=209 = 10                                   
         LA    RF,VALTBL-1(RE)                                                  
*                                                                               
         CLI   0(RF),0             IF 0, INVALID CHAR                           
         BE    VROTERR                                                          
         CLI   0(RF),C'D'          TEST DELETED                                 
         BE    VROT16                                                           
*                                                                               
         LLC   RE,0(RF)            GET VALID CMML FLAG                          
         LA    RE,1(RE)            BUMP USAGE COUNT                             
         STC   RE,0(RF)                                                         
         MVC   0(1,R4),0(R1)       MOVE LETTER TO ELEM                          
         LA    R4,1(R4)            BUMP OUTPUT POSN                             
*                                                                               
VROT16   LA    R1,1(R1)                                                         
         BCT   R0,VROT14                                                        
*                                                                               
* MAKE SURE ALL CMMLS HAVE BEEN INPUT                                           
*                                                                               
         LA    R1,VALTBL                                                        
         LA    R0,15                                                            
VROT20   CLI   0(R1),1             TABLE SHOULD NOT BE 1 ANYMORE                
         JE    ROTERR                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,VROT20                                                        
*                                                                               
         LA    R6,ELEM             INSERT ROTATION ELEMENT IN RECORD            
         USING PATPTNEL,R6                                                      
*                                                                               
         MVI   PATPTNEL,X'32'                                                   
         LA    R0,ELEM                                                          
         SR    R4,R0                                                            
         STC   R4,PATPTNLN                                                      
*                                                                               
         GOTO1 ADDELEM                                                          
*                                                                               
         XC    TCMROT,TCMROT     MOVE DATA TO SCREEN                            
         MVC   TCMROT,ELEM+2                                                    
         OI    TCMROTH+6,X'80'                                                  
*                                                                               
         XC    TCMDROT,TCMDROT     DO NOT SHOW DERIVED ROTATION                 
         OI    TCMDROT+6,X'80'                                                  
*                                                                               
VROTX    XIT1                                                                   
*                                                                               
VROTERR  XC    DUB,DUB                                                          
         LA    RE,DUB                                                           
         STCM  RE,7,GASUBST        A(SUBST TEXT)                                
         MVI   DUB,2                                                            
         MVC   DUB+1(1),0(R1)                                                   
         MVC   GERROR,=Y(BADLTTR)                                               
         GOTO1 VTRAERR                                                          
         DROP  R6                                                               
         EJECT                                                                  
*================================================================               
* BUILD ROTATION ELEMENT FROM INPUT PERCENTAGES                                 
*================================================================               
                                                                                
VROT30   L     R6,AIO                                                           
         MVI   ELCODE,X'34'        GET PCT ELEMENT PREVIOUSLY ADDED             
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
* FIND MIN/MAX PCTS                                                             
         LA    R0,255                                                           
         ST    R0,MINPCT                                                        
         XC    MAXPCT,MAXPCT                                                    
*                                                                               
         XC    PCTTBL,PCTTBL                                                    
         LLC   RE,1(R6)                                                         
         AHI   RE,-3                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PCTTBL(0),2(R6)     MOVE PCTS TO TABLE AREA                      
*                                                                               
         LLC   R0,1(R6)                                                         
         AHI   R0,-2                                                            
         SRDL  R0,32                                                            
         D     R0,=F'3'                                                         
         LTR   R0,R1                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STC   R0,PCTCT            SAVE NUMBER OF PERCENTS                      
         LA    R1,PCTTBL           POINT TO FIRST PCT                           
*                                                                               
VROT32   SR    RE,RE                                                            
         ICM   RE,3,1(R1)                                                       
         C     RE,MINPCT                                                        
         BNL   *+8                                                              
         ST    RE,MINPCT                                                        
         C     RE,MAXPCT                                                        
         BNH   *+8                                                              
         ST    RE,MAXPCT                                                        
         LA    R1,3(R1)                                                         
         BCT   R0,VROT32                                                        
*                                                                               
         BAS   RE,DIV              GET DIVISOR                                  
         BE    VROT40                                                           
*                                                                               
         CLI   SVT2PR03,C'Y'                                                    
         BNE   CALROTER                                                         
*                                                                               
         BAS   RE,ABSEL                                                         
                                                                                
         MVI   ELCODE,X'34'                                                     
         GOTO1 REMELEM             DELETE OLD X'34' ELEM                        
*                                                                               
         LA    R6,ELEM                                                          
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'34'                                                       
         L     RE,PCTNXT           GET A(NEXT ENTRY)                            
         MVI   0(RE),0             MARK END OF LIST                             
         LA    R0,PCTTBL                                                        
         SR    RE,R0               GIVES LENGTH OF ENTRIES                      
         AHI   RE,2                                                             
         STC   RE,ELEM+1           SET ELEM LENGTH                              
         MVC   ELEM+2(45),PCTTBL   MOVE TABLE TO ELEM                           
         MVC   AIO,AIO3                                                         
         GOTO1 ADDELEM                                                          
*                                                                               
* NOW BUILD TABLE OF LETTERS AND NUMBERS TO BUILD ROTATION *                    
*                                                                               
VROT40   XC    WORK,WORK                                                        
         LLC   R0,PCTCT                                                         
         LA    R3,PCTTBL                                                        
         LA    R4,WORK+2                                                        
         L     R6,COMDIV                                                        
         SR    R1,R1                                                            
         SR    RF,RF                                                            
*                                                                               
VROT60   MVC   0(1,R4),0(R3)       SAVE LETTER                                  
         ICM   RF,3,1(R3)                                                       
         SR    RE,RE                                                            
         DR    RE,R6                                                            
         LTR   RE,RE               IF A REMAINDER, NG                           
         BZ    VROT66                                                           
         DC    H'0'                                                             
*                                                                               
VROT66   STCM  RF,3,1(R4)                                                       
         AR    R1,RF                                                            
         LA    R3,3(,R3)                                                        
         LA    R4,3(,R4)                                                        
         BCT   R0,VROT60                                                        
*                                                                               
         STH   R1,WORK             ROTATION TABLE SIZE                          
         CHI   R1,60               MAX ROT LEN                                  
         BNL   CALROTER                                                         
*                                                                               
VROT68   LR    R3,R1                                                            
         XC    ELEM,ELEM                                                        
         MVI   ELEM,X'32'                                                       
         LA    R0,2(,R3)                                                        
         STC   R0,ELEM+1                                                        
         LA    R6,ELEM+2                                                        
*                                                                               
VROT70   LLC   R0,PCTCT            GET NUMBER OF ENTRIES                        
         LA    R1,WORK+2                                                        
*                                                                               
VROT74   SR    RE,RE                                                            
         ICM   RE,3,1(R1)                                                       
         BZ    VROT76                                                           
         MVC   0(1,R6),0(R1)                                                    
         BCTR  RE,0                                                             
         STCM  RE,3,1(R1)                                                       
         LA    R6,1(R6)                                                         
*                                                                               
VROT76   LA    R1,3(R1)                                                         
         BCT   R0,VROT74                                                        
         BCT   R3,VROT70                                                        
*                                                                               
         GOTO1 ADDELEM                                                          
         J     EQXIT                                                            
         EJECT                                                                  
*=============================================================                  
* DEVELOP DIVISOR TO BUILD ROTATION TABLE                                       
*=============================================================                  
                                                                                
DIV      NTR1                                                                   
         SR    RE,RE               SEE IF MAX DIVIDES BY MIN                    
         L     RF,MAXPCT                                                        
         D     RE,MINPCT                                                        
         MVC   COMDIV,MINPCT                                                    
         LTR   RE,RE               IF A REMAINDER, NG                           
         BNZ   DIV10                                                            
         BAS   RE,CKD              CK ALL ENTRIES                               
         JE    EQXIT                                                            
*                                                                               
* SEE IF MAX DIVIDES BY MIN/2 *                                                 
*                                                                               
DIV10    TM    COMDIV+3,1          IF ODD NUMBER, BYPASS                        
         BO    DIV14                                                            
         SR    RE,RE                                                            
         L     RF,MAXPCT                                                        
         L     R0,COMDIV                                                        
         SRL   R0,1                DIVIDE BY 2                                  
         ST    R0,COMDIV                                                        
         DR    RE,R0                                                            
         LTR   RE,RE               IF A REMAINDER, NG                           
         BNZ   DIV14                                                            
         BAS   RE,CKD              CK ALL ENTRIES                               
         JE    EQXIT                                                            
*                                                                               
* DIVIDE BY SOME PRIME NOS & SEE IF RESULT IS COMMON DIVISOR *                  
*                                                                               
DIV14    LA    R2,PRIMETBL         TRY LIMITED PRIME NUMBERS                    
*                                                                               
DIV20    LH    R0,0(R2)                                                         
         ST    R0,COMDIV                                                        
         BAS   RE,CKD              CK ALL ENTRIES                               
         BE    DIV30                                                            
         LA    R2,2(R2)                                                         
         CLI   0(R2),255                                                        
         BNE   DIV20                                                            
         J     NEQXIT                                                           
*                                                                               
* NOW DOUBLE DIVISOR AND TRY AGAIN *                                            
*                                                                               
DIV30    L     R0,COMDIV                                                        
         AR    R0,R0                                                            
         ST    R0,COMDIV                                                        
         BAS   RE,CKD              CK ALL ENTRIES                               
         BE    DIV30                                                            
         L     R0,COMDIV                                                        
         SRL   R0,1                                                             
         ST    R0,COMDIV                                                        
         J     EQXIT                                                            
         EJECT                                                                  
*===================================================================            
* BUILD AND ADD PERCENTAGE ROTATION ELEMENT *                                   
* DEVELOP DIVISOR TO BUILD REAL ROTATION TABLE FROM ABSURD PERCENT              
*                                                                               
* METHOD - DIVIDE EACH ENTRY BY 2, 3, 5, 11, ADDING REMAINDERS                  
* DIVIDE TOTAL REMAINDER BY PRIME, SAVE Q & R, USE SMALLEST #                   
*===================================================================            
                                                                                
ABSEL    NTR1                                                                   
*                                                                               
         LA    R0,PRCTABLN                                                      
         LA    R1,PRCTAB                                                        
         LA    R2,RELTAB                                                        
*                                                                               
ABSEL10  LLC   R3,PCTCT            GET NUMBER OF PCTS                           
         SR    R4,R4                                                            
         LA    R5,PCTTBL           AND POINT TO FIRST                           
*                                                                               
ABSEL20  SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,1(R5)                                                       
         D     RE,0(R1)                                                         
         AR    R4,RE               ADD REMAINDER                                
         LA    R5,3(R5)                                                         
         BCT   R3,ABSEL20                                                       
*                                                                               
         SR    RE,RE                                                            
         LR    RF,R4               GET TOTAL REMAINDERS                         
         D     RE,0(R1)                                                         
         STC   RE,1(R2)            REMAINDER                                    
         STC   RF,0(R2)            QUOTIENT                                     
         LA    R1,4(R1)                                                         
         LA    R2,2(R2)                                                         
         BCT   R0,ABSEL10                                                       
*                                                                               
         LA    R0,PRCTABLN                                                      
         LA    R1,PRCTAB                                                        
         LA    R2,RELTAB                                                        
         LA    R3,2047                                                          
         SR    R4,R4                                                            
*                                                                               
ABSEL30  CLM   R3,3,0(R2)                                                       
         BNH   ABSEL34                                                          
         LR    R4,R1               SAVE ADDRESS OF LOWEST Q/R                   
         ICM   R3,3,0(R2)                                                       
*                                                                               
ABSEL34  LA    R1,4(R1)                                                         
         LA    R2,2(R2)                                                         
         BCT   R0,ABSEL30                                                       
*                                                                               
         LTR   R4,R4                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   COMDIV,0(R4)                                                     
*                                                                               
         LLC   R0,PCTCT                                                         
         LA    R2,PCTTBL                                                        
         LA    R3,REMTBL                                                        
*                                                                               
ABSEL40  SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,3,1(R2)                                                       
         D     RE,0(R4)                                                         
         STCM  RF,3,1(R2)          STORE QUOTIENT                               
         ST    RE,0(R3)            & REMAINDER                                  
         LA    R2,3(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,ABSEL40                                                       
*                                                                               
* NOW MULTIPLY EACH BY PRIME #                                                  
*                                                                               
         LLC   R0,PCTCT                                                         
         LA    R2,PCTTBL                                                        
ABSEL50  SR    RE,RE                                                            
         ICM   RE,3,1(R2)                                                       
         MH    RE,2(R4)                                                         
         STCM  RE,3,1(R2)          STORE RESULT                                 
         LA    R2,3(R2)                                                         
         BCT   R0,ABSEL50                                                       
*                                                                               
ABSEL60  LLC   R0,PCTCT            SEE IF SUM 99 OR 100                         
         LA    R2,PCTTBL                                                        
         SR    RF,RF                                                            
*                                                                               
ABSEL64  SR    RE,RE                                                            
         ICM   RE,3,1(R2)                                                       
         AR    RF,RE                                                            
         LA    R2,3(,R2)                                                        
         BCT   R0,ABSEL64                                                       
*                                                                               
         CHI   RF,100                                                           
         JE    EQXIT                                                            
         CHI   RF,99                                                            
         JE    EQXIT                                                            
*                                                                               
         LLC   R0,PCTCT                                                         
         LA    R2,PCTTBL                                                        
         LA    R3,REMTBL                                                        
         SR    R1,R1                                                            
*                                                                               
ABSEL70  C     R1,0(R3)                                                         
         BNL   ABSEL74                                                          
         LR    RE,R2                                                            
         LR    RF,R3                                                            
         L     R1,0(R3)                                                         
*                                                                               
ABSEL74  LA    R2,3(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,ABSEL70                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,1(RE)                                                       
         A     R0,0(R4)                                                         
         STCM  R0,3,1(RE)                                                       
         XC    0(4,RF),0(RF)                                                    
         B     ABSEL60                                                          
*                                                                               
PRCTAB   DC    F'2',F'3',F'5',F'11'                                             
PRCTABLN EQU   (*-PRCTAB)/4                                                     
         EJECT                                                                  
*=========================================================                      
* CHECK COMMON DIVISOR AGAINST ALL ENTRIES                                      
*=========================================================                      
                                                                                
CKD      LLC   RF,PCTCT                                                         
         LA    R3,PCTTBL           GET A(FIRST PCT IN ELEM)                     
         SR    R0,R0                                                            
         SR    R1,R1                                                            
*                                                                               
CKD10    ICM   R1,3,1(R3)                                                       
         D     R0,COMDIV                                                        
         LTR   R0,R0                                                            
         BNZR  RE                                                               
         LA    R3,3(R3)                                                         
         BCT   RF,CKD10                                                         
         CR    R0,R0                                                            
         BR    RE                                                               
*                                                                               
VROTMVC  MVC   ELEM+2(0),PCTTBL                                                 
         USING PATPTNEL,R6                                                      
VROTMVCA MVC   PATPTN(0),WORK                                                   
         DROP  R6                                                               
*                                                                               
CALROTER MVC   GERROR,=Y(LIST2LNG) LIST WOULD BE TOO LONG TO CREATE             
         J     ERREXITD                                                         
*                                                                               
MIXPCTER MVC   GERROR,=Y(NOMIXPCT) MUST HAVE ALL PERCENTS OR NONE               
         J     ERREXITD                                                         
*                                                                               
ROTPCTER MVC   GERROR,=Y(ROTPCT)  ALL PERCENTS MUST ADD UP TO 99 OR 100         
         J     ERREXITD                                                         
*                                                                               
PCTFMTER MVC   GERROR,=Y(INVPCT)  PERCENTS MUST BE 1 OR 2 DIGITS                
         J     ERREXITD                                                         
*                                                                               
ZEROPER  MVC   GERROR,=Y(ZEROPCT) PERCENTS MUST BE NON-ZERO                     
         J     ERREXITD                                                         
*                                                                               
NOPCTER  MVC   GERROR,=Y(NOPCT1CM)  NO PERCENTAGE ALLOWED FOR 1 COMML           
*                                                                               
ERREXITD GOTO1 VTRAERR                                                          
*                                                                               
ROTERR   MVI   ERROR,INVCMLRT      MISSING/EXTRA ROTATION CHAR                  
         LA    R2,TCMROTH                                                       
         GOTO1 ERREX                                                            
*                                                                               
PRIMETBL DC    H'5',H'3',H'2',H'7',H'11',H'13',H'17',H'19',H'23',H'29'          
         DC    H'31',H'37'                                                      
         DC    X'FF'                                                            
         DROP  RB                                                               
         EJECT                                                                  
*================================================================               
* VALIDATE COMMERCIAL                                                           
* ON ENTRY WORK(12) CONTAINS INPUT CMML                                         
*          R1 POINTS TO SVTYPE(4),SVSLN                                         
* ON EXIT  WORK+12(8) CONTAINS 8 BYTE VERSION                                   
*          AND ADIDFLAG IS 'Y' IF AN ADID WAS INPUT                             
*================================================================               
                                                                                
VCML     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LR    R5,R1               SAVE POINTER                                 
*                                                                               
         MVC   AIO,AIO2            USE IO2 FOR CMMLS                            
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
*                                                                               
         MVC   KEY+5(8),WORK       USE EBCDIC CMML                              
         MVC   WORK+12(8),WORK     SET AS TRUNPK OUTPUT                         
*                                                                               
         CLI   ADIDFLAG,C'Y'       TEST CAN USE ADIDS                           
         BNE   VCML4                                                            
*                                                                               
VCML2    MVC   KEY(2),=X'0AC1'                                                  
         CLI   VCMLFLAG,C'P'       TEST VALIDATING PERIOD                       
         BE    VCML4               YES-THEN WORK IS PACKED ALREADY              
         GOTO1 VTRPACK,DMCB,(C'P',WORK),WORK+12                                 
         JNE   VCMLERR                                                          
         MVC   KEY+5(8),WORK+12                                                 
*                                                                               
VCML4    MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   VCMLERR                                                          
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING CMLDTAEL,R6                                                      
         TM    CMLSTAT,X'80'       CMML DELETED                                 
         JNZ   CMLDELER                                                         
*                                                                               
         CLC   CMLRLSE,STRTPAT     SEE IF THIS CMML WILL START IN TIME          
         JH    CMLDTERA            NO, CMLRLSE AFTER PAT START                  
*                                                                               
         CLC   CMLRCL,STRTPAT      IS CML RECALL BEFORE PAT START               
         JL    CMLDTERC             YES, ERROR                                  
*                                                                               
         CLC   CMLRCL,ENDPAT       SEE IF THIS CMML WILL LAST THRU PAT          
         BNL   VCML10              YES, OK                                      
*                                                                               
         CLC   ENDPAT,=XL3'FFFFFF' IS PAT UFN                                   
         JNE   CMLDTERB                                                         
*                                                                               
VCML10   CLI   VCMLFLAG,C'P'       TEST VALIDATING FOR PERIOD CHANGE            
         JE    VCMLX               YES - EXIT NOW                               
*                                                                               
         MVC   SVCMLRCL,CMLRCL     SAVE COMMERCIAL RECALL DATE                  
*                                                                               
         MVC   0(4,R5),CMLTYPE     SAVE COMMERCIAL TYPE (SETS SVTYPE)           
         MVC   4(1,R5),CMLSLN      SAVE SPOT LEN                                
         MVC   5(1,R5),CMLSOLO                                                  
*                                                                               
         LA    R4,BPRD                                                          
         BAS   RE,CHKPRD                                                        
         BNE   *+8                                                              
         OI    6(R5),X'01'       SET PRD1 IN CMML                               
*                                                                               
         LA    R4,BPRD2                                                         
         CLI   0(R4),0                                                          
         BE    VCML12                                                           
         BAS   RE,CHKPRD                                                        
         BNE   *+8                                                              
         OI    6(R5),X'02'       SET PRD2 IN CMML                               
*                                                                               
VCML12   TM    6(R5),X'03'       TEST EITHER PRD IN THIS CMML                   
         JZ    MATPRDEV                                                         
*                                                                               
         CLI   HASDPT,C'Y'         TEST PATTERN HAS DPT                         
         BE    *+12                YES                                          
         CLI   HASTIME,C'Y'        OR PATTERN HAS TIME                          
         BNE   VCML14              NO                                           
*                                                                               
         MVI   ELCODE,X'B0'                                                     
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   VCML14                                                           
         USING CMLMATEL,R6                                                      
         OC    CMLMSTIM,CMLMSTIM   TEST TIME IN CMML                            
         BZ    VCML14                                                           
         MVC   GERROR,=Y(CMLHASTM)                                              
         GOTO1 VTRAERR                                                          
         EJECT                                                                  
*============================================================                   
* FURTHER VALIDATE CML (APPROVALS AND DATES IF ANY)                             
*============================================================                   
                                                                                
VCML14   MVI   ELCODE,X'90'        BORADCAST BUSINESS ELEMENT                   
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BE    VCML20                                                           
         CLI   SVT2PR9,C'Y'        IS THIS BRAND AGENCY                         
         BNE   VCMLX                                                            
         J     NOAIRERR            YES, NOT APPROVED TO AIR                     
*                                                                               
         USING CMLBBEL,R6                                                       
*                                                                               
VCML20   DS    0H                                                               
         CLI   CMLBBBAG,C'N'       IS BRAND AGY=N                               
         BE    VCMLX                                                            
         CLI   CMLBBBAG,C'Y'                                                    
         BE    *+12                                                             
         CLI   SVT2PR9,C'Y'        IS THIS BRAND AGENCY                         
         BNE   VCMLX                                                            
*                                                                               
         OC    CMLBBMXD,CMLBBMXD   ANY MAX USE DATE                             
         BZ    VCML30                                                           
         CLC   SVCMLRCL,CMLBBMXD   COMPARE RECALL DATE TO MAX USE DTE           
         BNH   VCML30                                                           
         MVC   SVCMLRCL,CMLBBMXD   SAVE EARLIER DATE                            
*                                                                               
         CLC   SVCMLRCL,STRTPAT    IS CML RECALL BEFORE PAT START               
         BL    CMLDTERC             YES, ERROR                                  
         CLC   SVCMLRCL,ENDPAT     SEE IF THIS CMML WILL LAST THRU PAT          
         BNL   VCML30               YES, OK                                     
         CLC   ENDPAT,=XL3'FFFFFF' IS PAT UFN                                   
         BNE   CMLDTERB                                                         
*                                                                               
VCML30   DS    0H                                                               
         CLI   CMLBBBAG,0          LEO CML?                                     
         BNE   *+12                                                             
         CLI   SVT1PROF+12,C'Y'    IS THIS BRAND AGENCY                         
         BE    *+12                                                             
         CLI   CMLBBBAG,C'Y'       LEO B. CML                                   
         BNE   VCMLX                NO, DONE                                    
*                                                                               
         OC    CMLBBCAD,CMLBBCAD   ANY CLIENT APPROVAL DATE?                    
         JZ    CADTERR              NO, ERROR                                   
*                                                                               
         CLI   CMLBBAPR,C'Y'       BROADCAST BUSINESS APPROVAL?                 
         BE    VCML32              YES                                          
*                                                                               
         CLI   CMLATAIR,C'Y'       APPROVED TO AIR                              
         JNE   NOAIRERR            NOT APPROVED TO AIR                          
         BE    VCMLX                YES, DONE                                   
*                                                                               
VCML32   OC    CMLBBREF,CMLBBREF   ANY CML REFERENCE?                           
         JZ    NOAIRERR             NO, ERROR                                   
*                                                                               
* CHECK IF CML FROM REFERENCE FIELD IS APPROVED TO AIR                          
*                                                                               
         MVC   WORK(8),CMLBBREF                                                 
         BRAS  RE,FNDCML            GO FIND COMMERCIAL                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   WORK(8),SVCML       RESTORE ORIGINAL CML                         
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'90'        BROADCAST BUSINESS ELEM                      
         BRAS  RE,GETEL                                                         
         JNE   NOAIRERR                                                         
*                                                                               
         USING CMLBBEL,R6                                                       
*                                                                               
         CLI   CMLATAIR,C'Y'                                                    
         JNE   NOAIRERR                                                         
*                                                                               
VCMLX    MVC   AIO,AIO3            RESTORE AIO                                  
         MVI   ERROR,0             CLEAR ERROR CODE                             
         J     EQXIT                                                            
         DROP  R6                                                               
*                                                                               
         USING CMLPRDEL,R6                                                      
CHKPRD   NTR1                                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'        GET PRDLIST ELEMENT                          
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   =XL3'2003FF',CMLPRDEL   IS THIS CMML PRD=ALL                     
         JE    EQXIT                   YES, COVERS ALL PRODUCTS                 
*                                                                               
         LLC   R0,CMLPRDLN                                                      
         AHI   R0,-2                                                            
         LA    R1,CMLPRDS          START OF PROD LIST                           
*                                                                               
CHKPRD2  CLC   0(1,R4),0(R1)       MATCH PRD                                    
         JE    EQXIT                YES                                         
         LA    R1,1(,R1)                                                        
         BCT   R0,CHKPRD2                                                       
         J     NEQXIT                                                           
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*==========================================================                     
* CHECK THAT COMMERCIALS AND PATTERN PRDS/SLNS AGREE                            
*==========================================================                     
                                                                                
CHKCMLS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   BPRD2,0             TEST P/B PATTERN                             
         BNE   CHKCML20            YES                                          
                                                                                
*================================================                               
* PATTERN IS FOR ONE PRODUCT ONLY                                               
*================================================                               
                                                                                
         CLI   SVSOLO1,C'P'        TEST FOR P/B ONLY                            
         JE    PGSOERR             YES - THEN ERROR                             
         CLI   SVSOLO2,C'P'                                                     
         JE    PGSOERR                                                          
*                                                                               
         CLI   SVSLN2,0            TEST 2 CMMLS INPUT                           
         BNE   CHKCML10            YES                                          
                                                                                
* ONE CMML INPUT - NON P/B                                                      
                                                                                
         TM    SVFLAG1,X'01'       PRD IN CMML                                  
         JZ    MATPRDEV            NO - ERROR                                   
         CLC   BSLN,SVSLN1         RIGHT SLN                                    
         JNE   INVSLNER            NO                                           
         J     EXIT                                                             
                                                                                
* TWO CMMLS INPUT - NON P/B                                                     
                                                                                
CHKCML10 CLI   SVSOLO1,C'S'        TEST SOLO ONLY                               
         JE    PGSOERR             YES - THEN ERROR                             
         CLI   SVSOLO2,C'S'        TEST SOLO ONLY                               
         JE    PGSOERR                                                          
*                                                                               
         CLI   SVFLAG1,1           TEST PRD IN BOTH CMMLS                       
         JNE   MATPRDEV                                                         
         CLI   SVFLAG2,1                                                        
         JNE   MATPRDEV                                                         
*                                                                               
         LLC   R0,SVSLN1                                                        
         LLC   R1,SVSLN2                                                        
         AR    R0,R1               GET TOTAL SLN                                
         CLM   R0,1,BSLN           RIGHT SLN                                    
         JNE   INVSLNER            NO                                           
         J     EXIT                                                             
                                                                                
*============================================================                   
* PATTERN IS FOR PIGGYBACK                                                      
*============================================================                   
                                                                                
CHKCML20 CLI   SVSOLO1,C'S'        TEST FOR SOLO ONLY                           
         JE    PGSOERR             YES - THEN ERROR                             
         CLI   SVSOLO2,C'S'                                                     
         JE    PGSOERR                                                          
*                                                                               
         CLI   SVSLN2,0            TEST TWO CMMLS                               
         BNE   CHKCML30                                                         
                                                                                
* ONE CMML INPUT - P/B                                                          
                                                                                
         TM    SVFLAG1,X'02'+X'01' TEST BOTH PRDS IN CMML                       
         JNO   MATPRDEV            NO - ERROR                                   
         LLC   R0,SVSLN1                                                        
         SRL   R0,1                BOTH PRDS MUST BE HALF CMML SLN              
         CLM   R0,1,BSLN                                                        
         JNE   INVSLNER                                                         
         CLM   R0,1,BSLN2                                                       
         JNE   INVSLNER                                                         
         J     EXIT                                                             
                                                                                
* TWO CMMLS INPUT- P/B                                                          
                                                                                
CHKCML30 TM    SVFLAG1,X'01'       TEST PRD1 IN CMML1                           
         BO    CHKCML32            YES                                          
         TM    SVFLAG2,X'01'       TEST PRD1 IN CMML2                           
         JZ    MATPRDEV                                                         
*                                                                               
CHKCML32 TM    SVFLAG1,X'02'       TEST PRD2 IN CMML1                           
         BO    CHKCML40                                                         
         TM    SVFLAG2,X'02'       TEST PRD2 IN CMML2                           
         JZ    MATPRDEV                                                         
                                                                                
* FULL+0=PRD1/CMML1, FULL+1=PRD2/CMML2                                          
* FULL+2=PRD1/CMML2, FULL+3=PRD2/CMML1                                          
                                                                                
CHKCML40 XC    FULL,FULL                                                        
         TM    SVFLAG1,X'01'       TEST PRD1 IN CMML 1                          
         BZ    CHKCML42                                                         
         MVI   FULL,X'10'          SET FLAG                                     
         CLC   SVSLN1,BSLN         MATCH PRD1 SLN                               
         BNE   *+8                                                              
         MVI   FULL,X'11'          SET PRD1/SLN1 MATCH CMML1                    
*                                                                               
CHKCML42 TM    SVFLAG1,X'02'       TEST PRD2 IN CMML1                           
         BZ    CHKCML44                                                         
         MVI   FULL+3,X'10'                                                     
         CLC   SVSLN2,BSLN2        MATCH PRD2 SLN                               
         BNE   *+8                                                              
         MVI   FULL+3,X'11'                                                     
*                                                                               
CHKCML44 TM    SVFLAG2,X'01'       TEST PRD1 IN CMML2                           
         BZ    CHKCML46                                                         
         MVI   FULL+2,X'10'                                                     
         CLC   SVSLN1,BSLN         MATCH PRD1 SLN                               
         BNE   *+8                                                              
         MVI   FULL+2,X'11'                                                     
*                                                                               
CHKCML46 TM    SVFLAG2,X'02'       TEST PRD2 IN CMML2                           
         BZ    CHKCML50                                                         
         MVI   FULL+1,X'10'                                                     
         CLC   SVSLN2,BSLN2        MATCH PRD2 SLN                               
         BNE   *+8                                                              
         MVI   FULL+1,X'11'                                                     
*                                                                               
CHKCML50 CLI   FULL,X'11'          TEST PRD1/SLN1 MATCH CMML1                   
         BNE   CHKCML52            NO                                           
         CLI   FULL+1,X'11'        TEST PRD2/SLN2 MATCH CMML2                   
         BE    CHKCMLX             YES - ALL IS GOOD                            
*                                                                               
CHKCML52 CLI   FULL+2,X'11'        TEST PRD1/SLN1 MATCH CMML2                   
         JNE   INVSLNER            NO - ERROR                                   
         CLI   FULL+3,X'11'        TEST PRD2/SLN2 MATCH CMML1                   
         JNE   INVSLNER            NO  - ERROR                                  
*                                                                               
CHKCMLX  MVC   AIO,AIO3                                                         
         J     EXIT                                                             
*                                                                               
VCMLERR  MVI   ERROR,INVCOMM       NO SUCH CMML FOR CLT                         
         J     TRAPERRC                                                         
*                                                                               
MATPRDEV MVI   ERROR,CMLPRDER      PAT PROD(S) NOT IN CMML                      
         J     TRAPERRC                                                         
*                                                                               
CMLDTERA MVI   ERROR,BDCMLDTS      CML RELEASE DTE AFTER PAT START              
         J     *+8                                                              
*                                                                               
CMLDTERB MVI   ERROR,BDCMLDTE      CML RECALL DTE BEFORE PAT END                
         CLI   VCMLFLAG,C'P'       TEST VALIDATING FOR PERIOD                   
         JNE   TRAPERRC                                                         
         J     NEQXIT              EXIT WITH CC NEQ FOR ERROR!                  
*                                                                               
CMLDTERC MVI   ERROR,BDCMLRCD      CML RECALL DTE BEFORE PAT START              
         J     TRAPERRC                                                         
*                                                                               
INVSLNER MVI   ERROR,PATCMLER      PAT/CML SPOT LENS DIFFERENT                  
*                                                                               
TRAPERRC GOTO1 ERREX                                                            
*                                                                               
CMLDELER LHI   R0,CMLISDEL       COMMERCIAL IS DELETED                          
         STH   R0,GERROR                                                        
         J     ERREXITC                                                         
*                                                                               
CADTERR  LHI   R0,NOCLADTE       NO CLIENT APPROVAL DATE                        
         J     *+8                                                              
*                                                                               
NOAIRERR LHI   R0,NAPRTAIR       NOT APPROVED TO AIR                            
         STH   R0,GERROR                                                        
*                                                                               
         XC    ELEM,ELEM                                                        
         MVI   ELEM,9                                                           
         MVC   ELEM+1(8),SVCML                                                  
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST                                                     
         J     ERREXITC                                                         
*                                                                               
PGSOERR  LHI   R0,INVPGSO        CMML PIG/SOLO CODE MUST MATCH USAGE            
         STH   R0,GERROR                                                        
*                                                                               
ERREXITC GOTO1 VTRAERR                                                          
         EJECT                                                                  
*=====================================================                          
* FIND CML - BUILD KEY AND READ KEY                                             
*=====================================================                          
                                                                                
FNDCML   NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2            USE I/O 2                                    
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CMLKEY,R4                                                        
         MVC   CMLKID,=XL2'0A21'                                                
         MVC   CMLKAM(3),BAGYMD AND BCLT                                        
         MVC   CMLKCML,WORK                                                     
*                                                                               
         CLI   ADIDFLAG,C'Y'                                                    
         BNE   *+10                                                             
         MVC   CMLKID,=X'0AC1'                                                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*=================================================                              
* CHECK EQUIVALENT PRODS IF PARTNER                                             
*=================================================                              
                                                                                
CEP      NTR1  BASE=*,LABEL=*                                                   
         LA    R0,TRAPRLNH         PRODUCT                                      
         LA    R5,QPRD                                                          
         LA    R6,EQVPRD                                                        
         CR    R0,R2                                                            
         BE    CEP10                                                            
*                                                                               
         LA    R0,TRAPTLNH         PARTNER PRODUCT                              
         LA    R5,QPRD2                                                         
         LA    R6,EQVPRD2                                                       
         CR    R0,R2                                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         SR    R0,R0                                                            
*                                                                               
CEP10    XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING PEQKEY,R4                                                        
         MVC   PEQKID,=X'0A37'                                                  
         MVC   PEQKAM(3),BAGYMD                                                 
         MVC   PEQKPRD,0(R5)                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE      THIS A BASE PRODUCT                          
         BNE   MISEQVER             NO, ERROR                                   
         MVC   0(3,R6),PEQKPRD                                                  
*                                                                               
CEP20    LTR   R0,R0               ON PARTNER PROD                              
         BNZ   CEPX                 NO                                          
*                                                                               
         CLC   EQVPRD,EQVPRD2                                                   
         BNE   CEPX                                                             
         DC    H'0'                                                             
*                                                                               
CEPX     XIT1                                                                   
                                                                                
MISEQVER MVC   GERROR,=Y(MISEQVPR) MUST SET UP PRD AS BASE                      
         GOTO1 VTRAERR                                                          
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
* FORMAT PERCENT ELEMENT *                                                      
*                                                                               
PCT      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING PATPCTEL,R6                                                      
         SR    R0,R0                                                            
         LLC   R1,PATPCTLN                                                      
         D     R0,=F'3'                                                         
         CHI   R0,2                                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R5,R1                                                            
         SR    R1,R1                                                            
         LA    R2,ELEM                                                          
         XC    ELEM,ELEM                                                        
         LA    R3,PATPCTLT                                                      
         B     PCT20                                                            
PCT10    MVI   0(R2),C','                                                       
         LA    R1,1(,R1)                                                        
         LA    R2,1(,R2)                                                        
PCT20    MVC   0(1,R2),0(R3)                                                    
         MVI   1(R2),C'='                                                       
*                                                                               
         CLI   2(R3),100           100% ?                                       
         BNE   *+14                                                             
         MVC   2(3,R2),=C'100'                                                  
         B     PCTX                                                             
*                                                                               
*        EDIT  (B2,1(R3)),(2,2(R2)),ZERO=NOBLANK                                
         SR    R0,R0                                                            
         ICM   R0,3,1(R3)                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  HALF,DUB                                                         
         CLI   HALF,C'0'                                                        
         BE    PCT30                                                            
         MVC   2(1,R2),HALF                                                     
         LA    R2,1(,R2)                                                        
PCT30    MVC   2(1,R2),HALF+1                                                   
         LA    R1,4(,R1)                                                        
         LA    R2,3(,R2)                                                        
         LA    R3,3(,R3)                                                        
         BCT   R5,PCT10                                                         
PCTX     XIT1                                                                   
         DROP  R6,RB                                                            
         EJECT                                                                  
* VALIDATE CANADIAN ELEMENTS OF COMMERCIAL RECORDS *                            
*                                                                               
VCAN     NTR1  BASE=*,LABEL=*                                                   
         L     R6,AIO                                                           
         MVI   ELCODE,X'50'                                                     
         BRAS  RE,GETEL            REGISTRATION ELEMENT                         
         BNE   VCAN20                                                           
         LLC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         LA    R5,2(,R6)                                                        
         LA    R3,CRTCMS                                                        
*                                                                               
VCAN10   OC    12(3,R5),12(R5)     ANY DATE                                     
         BZ    VCAN14              NO                                           
         CLC   ENDPAT,12(R5)       EXPIRATION DATE                              
         BH    CANREGER                                                         
VCAN14   LA    R3,7(,R3)           NEXT REG ERR MS                              
         LA    R5,15(,R5)                                                       
         SH    R1,=H'15'                                                        
         BP    VCAN10                                                           
*                                                                               
VCAN20   L     R6,AIO                                                           
         MVI   ELCODE,X'52'                                                     
         BRAS  RE,GETEL                                                         
         BNE   NOTALCYC                                                         
         LLC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         BCTR  R1,0                                                             
         LA    R5,2(,R6)                                                        
*                                                                               
VCAN30   CLC   STRTPAT,0(R5)       START PAT TO START TAL CYCLE                 
         BL    VCAN34                                                           
         CLC   STRTPAT,3(R5)       START PAT TO END TAL CYCLE                   
         BH    VCAN34                                                           
         CLC   ENDPAT,0(R5)        END PAT TO START TAL CYCLE                   
         BL    VCAN34                                                           
         CLC   ENDPAT,3(R5)        END PAT TO END TAL CYCLE                     
         BNH   VCAN40              FITS IN THIS CYCLE                           
VCAN34   LA    R5,6(,R5)                                                        
         SH    R1,=H'6'                                                         
         BP    VCAN30                                                           
*                                                                               
         B     TALCYCER            NO TALENT CYCLE FOR THIS PAT                 
*                                                                               
VCAN40   XIT1                                                                   
*                                                                               
CANREGER MVC   GERROR,=Y(CANREGIS)  XXXXXXX DATE EXPIRES BEFORE                 
*                                   PATTERN                                     
         XC    ELEM,ELEM                                                        
         MVI   ELEM,08                                                          
         MVC   ELEM+1(7),0(R3)                                                  
         LA    R1,ELEM                                                          
         STCM  R1,7,GASUBST                                                     
         B     VCANEREX                                                         
*                                                                               
NOTALCYC MVC   GERROR,=Y(MSTALCYC) NO TALENT CYCLE(S) FOR COMMERCIAL            
         B     VCANEREX                                                         
*                                                                               
TALCYCER MVC   GERROR,=Y(CANTALCY) NO TALENT CYCLE IN COMML FOR PATTERN         
*                                                                               
VCANEREX GOTO1 VTRAERR                                                          
*                                                                               
CRTCMS   DC    C'CRTC NO'                                                       
         DC    C'REG NO '                                                       
         DC    C'TC NO  '                                                       
         DC    C'CBC NO '                                                       
         DC    C'ASC NO '                                                       
         DROP  RB                                                               
         EJECT                                                                  
*==================================================================             
* PRINT MARKET/STATION LIST R3=ADDR 1ST FLD                                     
*                           R4=LEN OF THIS FLD                                  
*                           R5=LEN TO NEXT FLD                                  
*==================================================================             
                                                                                
PMKST    NTR1  BASE=*,LABEL=*                                                   
         XC    TRAMS1,TRAMS1                                                    
         XC    TRAMS2,TRAMS2                                                    
         XC    TRAMS3,TRAMS3                                                    
*                                                                               
         LA    R3,TRAMS1           START OF 1ST FIELD                           
         LA    R4,L'TRAMS1         LENGTH OF ALL FLDS                           
         LA    R5,L'TRAMS1+8       LENGTH TO NEXT FLD                           
         LA    RE,0(R3,R4)                                                      
         LLC   RF,0(RE)            ALSO BYPASS TITLE FIELD                      
         AR    R5,RF                                                            
         USING PATLSTEL,R6                                                      
         CLI   PATLSTTY,C'T'       STATION TYPE                                 
         BNE   PMKST10                                                          
         MVC   0(5,R3),=CL5'TYPE='                                              
         MVC   5(1,R3),PATLST  MOVE IN STATION TYPE                             
         B     PMKST16                                                          
*                                                                               
PMKST10  CLI   PATLSTTY,C'A'       AFFILIATE                                    
         BNE   PMKST20                                                          
*                                                                               
         LLC   R0,PATLSTLN                                                      
         SH    R0,=H'8'                                                         
         LA    R1,PATLST                                                        
         LA    RF,4(,R3)                                                        
         MVC   0(4,R3),=CL4'AFF='                                               
*                                                                               
PMKST14  MVC   0(3,RF),0(R1)   MOVE IN AFFILIATE                                
*                                                                               
         LTR   R0,R0               AT END OF ELEM                               
         BZ    PMKST16                                                          
         MVI   3(RF),C','                                                       
         LA    R1,5(,R1)                                                        
         LA    RF,4(,RF)                                                        
         SH    R0,=H'5'                                                         
         B     PMKST14                                                          
*                                                                               
PMKST16  XC    TRALTY,TRALTY                                                    
         OI    TRALTYH+6,X'80'                                                  
         B     PMKST60                                                          
*                                                                               
PMKST20  MVC   TRALTY(1),PATLSTTY                                               
         OI    TRALTYH+6,X'80'                                                  
         CLI   PATLSTTY,C'M'                                                    
         BNE   PMKST24                                                          
         OC    PATLST(5),PATLST                                                 
         BNZ   PMKST30                                                          
         MVC   0(3,R3),=CL3'ALL'                                                
         B     PMKST60                                                          
*                                                                               
PMKST24  CLI   PATLSTTY,C'S'                                                    
         BE    PMKST30                                                          
         CLI   PATLSTTY,C'C'                                                    
         BE    PMKST30                                                          
         CLI   PATLSTTY,C'G'                                                    
         BE    PMKST30                                                          
         DC    H'0'                MUST BE A, T, M, S, C, OR G                  
*                                                                               
PMKST30  LR    R2,R3               START OF FIRST PRINT FLD                     
         LA    R0,0(R5,R2)         END OF THIS FLD +1                           
         BCTR  R0,0                SUBTRACT 2 FOR END OF THIS FLD               
         BCTR  R0,0                                                             
         LLC   RF,PATLSTLN                                                      
         SR    RE,RE                                                            
         D     RE,=F'5'            DIVIDE BY ENTRY SIZE (ODD 3 DROPPED)         
         LA    RE,PATLST                                                        
         MVC   MSCT,PATLSTTY                                                    
         LA    R6,3                MAX NUMBER OF FLS                            
         DROP  R6                                                               
PMKST40  CLI   MSCT,C'G'           MARKET GROUP                                 
         BE    PMKST47              YES                                         
         CLI   MSCT,C'C'           COMBINED MARKET/AFFILIATE                    
         BE    *+12                 YES                                         
         CLI   MSCT,C'M'           MARKET                                       
         BNE   PMKST46             NO, STATION                                  
*                                                                               
         CLI   0(RE),0             THIS A MARKET OR AFFIL                       
         BH    PMKST44              AFFIL                                       
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,3(RE)                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(5),DUB+5(3)                                                 
         CLI   WORK+1,C'0'         1ST A ZERO                                   
         BNE   PMKST42                                                          
         MVC   0(3,R2),WORK+2      ONLY 3 DIGITS OF MARKET                      
         LA    R2,3(,R2)                                                        
         B     PMKST48                                                          
*                                                                               
PMKST42  MVC   0(4,R2),WORK+1      ALL 4 OF MARKET                              
         LA    R2,4(,R2)                                                        
         B     PMKST48                                                          
*                                                                               
PMKST44  MVC   0(3,R2),0(RE)       MOVE IN AFFILIATE                            
         LA    R2,3(,R2)                                                        
         B     PMKST48                                                          
*                                                                               
PMKST46  OC    0(2,RE),0(RE)       IS THIS A CABLE HEAD STATION                 
         BNZ   PMKST46C                                                         
*                                                                               
         STM   RE,R1,SVRER1                                                     
         LR    R0,RE                                                            
         GOTO1 MSUNPK,DMCB,(X'80',(R0)),WORK,WORK+4                             
         LM    RE,R1,SVRER1                                                     
*                                                                               
         MVC   0(8,R2),WORK+4                                                   
         MVI   4(R2),C'/'                                                       
         LA    R2,7(R2)                                                         
         CLI   0(R2),C' '                                                       
         BNH   PMKST48                                                          
         LA    R2,1(R2)                                                         
         B     PMKST48                                                          
*                                                                               
PMKST46C MVC   0(4,R2),0(RE)       MOVE IN 1ST 4 LETTERS                        
         LA    R2,3(,R2)                                                        
         CLI   0(R2),C' '          SEE IF ONLY THREE LETTERS                    
         BNH   *+8                 YES                                          
         LA    R2,1(,R2)           BUMP OVER 4TH LETTER                         
         CLI   4(RE),C'T'          IF TV                                        
         BE    PMKST48             ONLY PRINT STATION LETTERS                   
         MVI   0(R2),C'-'                                                       
         MVC   1(1,R2),4(RE)                                                    
         CLI   4(RE),C'L'          IF LOW POWER TV                              
         BE    PMKST46E             PRINT 'L'                                   
*                                                                               
         CLI   4(RE),C'N'          IF NETWORK                                   
         BE    PMKST46E            ONLY PRINT C'N'                              
*                                                                               
*MNMB                                                                           
         CLI   4(RE),C'D'                                                       
         BNE   *+12                                                             
         MVI   2(R2),C'V'                                                       
         B     *+8                                                              
*MNMB                                                                           
         MVI   2(R2),C'M'                                                       
         LA    R2,3(R2)                                                         
         B     PMKST48                                                          
*                                                                               
PMKST46E LA    R2,2(R2)                                                         
         B     PMKST48                                                          
*                                                                               
PMKST47  STM   RF,R1,SVRFR1        SAVE R1 AND RF                               
*                                                                               
* LOOK UP 1 CHAR MARKET GROUP IN THE CONVERT TABLE                              
*                                                                               
         L     R1,=A(SPMGRTAB)                                                  
         A     R1,SPTR03RR                                                      
         LHI   RF,(SPMGRTBX-SPMGRTAB)/3                                         
*                                                                               
PMKST47C CLC   0(1,RE),2(R1)      IS THIS IT                                    
         BE    PMKST47F                                                         
         LA    R1,3(R1)                                                         
         BCT   RF,PMKST47C                                                      
         DC    H'0'                SHOULD NOT BE (NOT IN TABLE)???              
*                                                                               
PMKST47F MVC   0(2,R2),0(R1)       MOVE IN LETTER OF MARKET GROUP               
*                                                                               
         LM    RF,R1,SVRFR1        RESTORE REGS                                 
*                                                                               
         UNPK  DUB(5),1(3,RE)                                                   
         CLI   1(R2),X'40'         SEE IF SECOND CHAR IS BLANK                  
         BNH   PMKST47H                                                         
*                                                                               
         MVC   2(4,R2),DUB         MOVE 4 DIGITS AFTER 2 CHAR MKT GRP           
         LA    R2,6(,R2)                                                        
         B     PMKST48                                                          
*                                                                               
PMKST47H MVC   1(4,R2),DUB         THEN 4 DIGITS                                
         LA    R2,5(,R2)                                                        
*                                                                               
PMKST48  LA    RE,5(RE)            POINT TO NEXT IN PATLST                      
         CR    R2,R0                                                            
         BNH   *+6                                                              
         DC    H'0'                                                             
         LA    R1,0(R3,R4)                                                      
         SR    R1,R2                                                            
         CHI   R1,8                SEE IF OUT OF SPACE                          
         BNH   PMKST50                                                          
         MVI   0(R2),C','                                                       
         LA    R2,1(R2)                                                         
         B     PMKST54                                                          
*                                                                               
PMKST50  LA    R2,0(R3,R5)         POINT TO NEXT FLD                            
         LR    R3,R2                                                            
         LA    R0,0(R2,R4)                                                      
         BCT   R6,PMKST54          ONLY 3 FLDS                                  
         CHI   RF,1                                                             
         BNE   MSLSTER                                                          
*                                                                               
PMKST54  BCT   RF,PMKST40                                                       
         BCTR  R2,0                                                             
         CLI   0(R2),C','                                                       
         BNE   PMKST60                                                          
         MVI   0(R2),C' '                                                       
*                                                                               
PMKST60  OI    TRAMS1H+6,X'80'                                                  
         OI    TRAMS2H+6,X'80'                                                  
         OI    TRAMS3H+6,X'80'                                                  
         XIT1                                                                   
*                                                                               
MSLSTER  MVC   GERROR,=Y(MSMAXLST) TOO MANY ENTRIES TO FIT IN LIST,             
*                                   TAKE ONE OUT                                
         LA    R2,TRAMS1H                                                       
         GOTO1 VTRAERR                                                          
         DROP  RB                                                               
         EJECT                                                                  
* PRINT PERIOD INTO WORK                                                        
*                                                                               
PPER     NTR1  BASE=*,LABEL=*                                                   
         XC    WORK,WORK                                                        
         CLC   =XL6'C5E200000000',0(R2)                                         
         BE    PPER20                                                           
         GOTO1 DATCON,DMCB,(3,(R2)),(5,WORK)                                    
         MVI   WORK+8,C'-'                                                      
         CLC   =XL3'FFFFFF',3(R2)                                               
         BNE   PPER10                                                           
         MVC   WORK+9(3),=CL3'UFN'                                              
         B     PPERX                                                            
PPER10   GOTO1 (RF),(R1),(3,3(R2)),(5,WORK+9)                                   
         B     PPERX                                                            
*                                                                               
PPER20   XC    WORK,WORK                                                        
         MVC   WORK+L'TRAPER(9),=C'EST DATES'                                   
         GOTO1 DATCON,DMCB,(3,ESTSTR),(5,WORK+10+L'TRAPER)                      
         MVI   WORK+18+L'TRAPER,C'-'                                            
         GOTO1 (RF),(R1),(3,ESTEND),(5,WORK+19+L'TRAPER)                        
         CLC   TRAEST,WORK+L'TRAPER                                             
         BE    PPER24                                                           
         MVC   TRAEST,WORK+L'TRAPER                                             
         OI    TRAESTH+6,X'80'                                                  
PPER24   MVC   WORK(2),=C'ES'                                                   
*                                                                               
PPERX    XIT1                                                                   
         DROP  RB                                                               
         EJECT                                                                  
* VALIDATE COPY CODE (MAY BE ESTIMATE IF T1 PROFILE 12 ON) *                    
*                                                                               
VCC      NTR1  BASE=*,LABEL=*                                                   
         MVI   CODE,0                                                           
         MVI   CODESW,C'N'                                                      
         MVI   QBEST,0                                                          
         MVI   QBESTEND,0                                                       
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VCC10                                                            
*                                                                               
         CLI   SVPROF11,C'P'       COPY CODE = PROGRAM                          
         BE    MISSERRA                                                         
*                                                                               
         CLI   SVPROF11,C'E'       COPY CODE = ESTIMATE                         
         BE    MISSERRA                                                         
         B     VCCX                                                             
*                                                                               
VCC10    CLI   SVPROF11,C'E'       COPY CODE = ESTIMATE                         
         BNE   VCC30                                                            
*                                                                               
         GOTO1 VALINUM                                                          
*                                                                               
         MVC   CODE,ACTUAL                                                      
         MVI   CODESW,C'Y'                                                      
*                                                                               
         MVC   FILENAME,=CL8'SPTDIR' SWTICH TO SPOT MEDIA                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ESTHDRD,R4                                                       
*                                                                               
         MVC   EKEYAM(3),BAGYMD                                                 
         MVC   EKEYPRD,QPRD                                                     
         MVC   EKEYEST,CODE                                                     
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ESTCDER                                                          
*                                                                               
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         MVC   FILENAME,=CL8'SPTFIL' SWTICH TO SPOT MEDIA                       
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         CLI   ECOPY,0             MUST NOT BE COPY CODE                        
         BNE   ESTCCER                                                          
         GOTO1 DATCON,DMCB,(0,ESTART),(3,ESTSTR)                                
         GOTO1 (RF),(R1),(0,EEND),(3,ESTEND)                                    
*                                                                               
         XC    FILENAME,FILENAME   SWTICH TO SPOT TRAFFIC                       
*                                                                               
         MVC   QBEST,CODE          SAVE EST FOR VALILOC                         
         MVC   SVMENU,EDAYMENU     SAVE DAYPART MENU                            
*                                                                               
         B     VCCX                                                             
         DROP  R4                                                               
*                                                                               
VCC30    CLI   5(R2),1                                                          
         BH    VCCLENER                                                         
         GOTO1 ANY                                                              
*                                                                               
         MVC   CODE,WORK                                                        
         CLI   SVPROF11,C'P'       COPY CODE = PROGRAM                          
         BNE   VCCX                                                             
         LA    R0,15                                                            
         LA    R1,SVPROGCD+1                                                    
VCC40    CLI   SVPROGCD,C'I'       INCLUDE                                      
         BE    VCC44                                                            
         CLI   SVPROGCD,C'E'       EXCLUDE                                      
         BE    VCC46                                                            
         DC    H'0'                                                             
*                                                                               
VCC44    CLC   CODE,0(R1)          THIS A VALID CODE                            
         BE    VCCX                                                             
         LA    R1,1(,R1)                                                        
         BCT   R0,VCC44                                                         
         B     PRGCDERR                                                         
*                                                                               
VCC46    CLC   CODE,0(R1)          THIS A VALID CODE                            
         BE    PRGCDERR                                                         
         LA    R1,1(,R1)                                                        
         BCT   R0,VCC44                                                         
*                                                                               
VCCX     XIT1                                                                   
*                                                                               
MISSERRA MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
*                                                                               
PRGCDERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(26),=C'* ERROR * INCLUDED CODES ='                       
         CLI   SVPROGCD,C'E'       EXLUDE                                       
         BNE   *+10                                                             
         MVC   CONHEAD+10(2),=C'EX'                                             
         MVC   CONHEAD+28(15),SVPROGCD+1                                        
         MVI CONHEAD+44,C'*'                                                    
*                                                                               
         GOTO1 ERREX2                                                           
VCCLENER MVC   GERROR,=Y(VCCLEN)   COPY CODE MORE THAN 1 CHARACTER              
*                                   LONG                                        
         B     ERREXITV                                                         
ESTCCER  MVC   GERROR,=Y(ESTCCDE)  ESTIMATE HAS COPY CODE                       
         B     ERREXITV                                                         
ESTCDER  MVC   GERROR,=Y(ESTNF)    ESTIMATE NOT FOUND                           
ERREXITV GOTO1 VTRAERR                                                          
         LTORG                                                                  
         EJECT                                                                  
* DISPLAY KEY                                                                   
DK       NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,GETPRF           GET PROFS IN CASE CALL FROM LIST             
         CLI   MYSCREEN,X'C1'      TEST PTTN/CMML SCREEN                        
         BE    DK1                                                              
         CLI   MYSCREEN,X'5D'      TEST PTTN/COMMENT SCREEN                     
         BE    DK1                                                              
         CLI   MYSCREEN,X'5C'      TEST BPAT/CMML SCREEN                        
         BNE   DK2                                                              
*                                                                               
DK1      L     R4,AIO                                                           
         CLC   TWAKEYSV(13),0(R4)   TEST SAME RECORD                            
         BE    DK2                                                              
         MVI   MYSCREEN,X'F3'       RESTORE PATTERN SCREEN                      
         BRAS  RE,GETSCR                                                        
*                                                                               
         USING PATKEY,R4                                                        
DK2      L     R4,AIO                                                           
         ICM   R0,7,PATKREF        GET REF/SUB                                  
         N     R0,=X'000003FF'     DROP REF                                     
         X     R0,=X'000003FF'     UNCOMPLEMENT                                 
         STH   R0,LATEST           SAVE MOST RECENT SUBLINE                     
*                                                                               
         CLC   TWAKEYSV(13),0(R4)  TEST SAME RECORD                             
         BE    DK4                                                              
         XC    MYSUBLIN,MYSUBLIN                                                
*                                                                               
DK4      LA    R4,PCMPFK                                                        
         CLI   MYSCREEN,X'5D'                                                   
         BE    *+8                                                              
         LA    R4,TRAPFK                                                        
         XC    15(63,R4),15(R4)    CLEAR ALL BUT PF2                            
         AHI   R4,-8               BACK UP TO FLDHDR                            
         OI    6(R4),X'80'                                                      
         LA    R4,8(R4)                                                         
*                                                                               
         CLC   LATEST,=H'1'        TEST ONLY ONE VERSION                        
         BE    DK6                                                              
         CLI   ACTNUM,ACTDIS                                                    
         BE    *+12                                                             
         CLI   ACTNUM,ACTSEL                                                    
         BNE   DK6                                                              
         BRAS  RE,SETPFTXT                                                      
*                                                                               
DK6      L     R4,AIO                                                           
         USING PATKEY,R4                                                        
         MVC   BAGYMD,PATKAM                                                    
         MVC   BCLT,PATKCLT                                                     
         MVC   BPRD(4),PATKPRD     BSLN BPRD2 BSLN2                             
         MVC   CODE,PATKCODE                                                    
         MVI   CODESW,C'N'                                                      
         XC    WORK(L'TRAMED),WORK                                              
         MVC   WORK(L'QMED),QMED                                                
         CLC   TRAMED,WORK                                                      
         BE    *+14                                                             
         MVC   TRAMED,WORK                                                      
         OI    TRAMEDH+6,X'80'                                                  
         XC    WORK(L'TRACLT),WORK                                              
         MVC   WORK(L'QCLT),QCLT                                                
         CLC   TRACLT,WORK                                                      
         BE    DK10                                                             
         MVC   TRACLT,WORK                                                      
         OI    TRACLTH+6,X'80'                                                  
*                                                                               
DK10     LA    R3,PATKPRD          PROD AND SPOT LEN                            
         BRAS  RE,PPRD             OUTPUT WILL BE IN FLD                        
         CLC   TRAPRLN,FLD                                                      
         BE    *+14                                                             
         MVC   TRAPRLN,FLD                                                      
         OI    TRAPRLNH+6,X'80'                                                 
         MVC   QPRD,FLD                                                         
         CLI   QPRD+2,C'-'                                                      
         BNE   *+8                                                              
         MVI   QPRD+2,C' '                                                      
         LA    R3,PATKPRD2         PROD PARTNER AND SPOT LEN                    
         BRAS  RE,PPRD             OUTPUT WILL BE IN FLD                        
         CLC   TRAPTLN,FLD                                                      
         BE    *+14                                                             
         MVC   TRAPTLN,FLD                                                      
         OI    TRAPTLNH+6,X'80'                                                 
         MVC   QPRD2,FLD                                                        
         CLI   QPRD2+2,C'-'                                                     
         BNE   *+8                                                              
         MVI   QPRD2+2,C' '                                                     
*                                                                               
         XC    WORK(L'TRACODE),WORK                                             
         MVC   WORK(L'PATKCODE),PATKCODE                                        
         CLI   PATKCODE,0                                                       
         BE    DK16                                                             
         LR    R6,R4                                                            
         MVI   ELCODE,X'10'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PATDTAEL,R6                                                      
         TM    PATSTAT,X'10'       THIS A COPY CODE=EST                         
         BZ    DK16                                                             
         DROP  R6                                                               
*                                                                               
         EDIT  (B1,PATKCODE),(3,DMCB),ALIGN=LEFT                                
         LR    R1,R0                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),DMCB                                                     
         CLC   TRACODE,WORK                                                     
         BE    *+14                                                             
         MVC   TRACODE,WORK                                                     
         OI    TRACODEH+6,X'80'                                                 
         MVI   CODESW,C'Y'                                                      
*                                                                               
         MVC   SVKEY,KEY                                                        
         MVC   FILENAME,=CL8'SPTDIR' SWTICH TO SPOT MEDIA                       
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING ESTHDRD,R4                                                       
         MVC   EKEYAM(3),BAGYMD                                                 
         MVC   EKEYPRD,QPRD                                                     
         MVC   EKEYEST,CODE                                                     
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         JNE   NOESTER                                                          
         L     R4,AIO2                                                          
         ST    R4,AIO                                                           
         MVI   RDUPDATE,C'N'         NOT READ FOR UPDATE                        
         MVC   FILENAME,=CL8'SPTFIL' SWTICH TO SPOT MEDIA                       
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 DATCON,DMCB,(0,ESTART),(3,ESTSTR)                                
         GOTO1 (RF),(R1),(0,EEND),(3,ESTEND)                                    
         MVC   SVMENU,EDAYMENU     SAVE DAYPART MENU                            
*                                                                               
         XC    FILENAME,FILENAME   SWTICH TO SPOT TRAFFIC                       
*                                                                               
         MVC   KEY(L'SVKEY),SVKEY                                               
         LA    R0,11                                                            
         BRAS  RE,TRHIGH                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'       NOT READ FOR UPDATE                          
         GOTO1 GETREC                                                           
         B     DK18                                                             
*                                                                               
DK16     CLC   TRACODE,WORK                                                     
         BE    *+14                                                             
         MVC   TRACODE,WORK                                                     
         OI    TRACODEH+6,X'80'                                                 
*                                                                               
DK18     L     R4,AIO1                                                          
         ST    R4,AIO                                                           
         USING PATKEY,R4                                                        
         ICM   R0,7,PATKREF                                                     
         STCM  R0,7,BREFSUB                                                     
         SRDL  R0,10                                                            
         X     R0,=XL4'00003FFF'                                                
         STH   R0,BREF                                                          
         SRL   R1,22                                                            
         X     R1,=XL4'000003FF'                                                
         STH   R1,BSUB                                                          
         EDIT  (R0),(5,REF),ALIGN=LEFT                                          
         CLC   TRAREF,REF                                                       
         BE    *+14                                                             
         MVC   TRAREF,REF                                                       
         OI    TRAREFH+6,X'80'                                                  
         EJECT                                                                  
* PRINT OUT ANY FILTERS                                                         
*                                                                               
         OC    FILTERS,FILTERS     ANY FILTERS                                  
         BZ    DKX                 NO                                           
         LA    R3,FLD              OUTPUT AREA                                  
         LR    R4,R3               COMPARAND                                    
         XC    FLD,FLD                                                          
         CLI   DATEFTR1,0          DATE FILTER                                  
         BE    DK20                                                             
         MVI   0(R3),C'D'                                                       
         CLI   DATESFTR,0          ANY GREATER/LESS THAN                        
         BE    *+14                NO                                           
         MVC   1(1,R3),DATESFTR                                                 
         LA    R3,1(,R3)                                                        
         MVI   1(R3),C'='                                                       
         GOTO1 DATCON,DMCB,(3,DATEFTR1),(5,2(R3))                               
         LA    R3,10(,R3)                                                       
         CLI   DATEFTR2,0          SECOND DATE FILTER                           
         BE    DK20                                                             
         MVI   0(R3),C'-'                                                       
         GOTO1 (RF),(R1),(3,DATEFTR2),(5,1(R3))                                 
         LA    R3,9(,R3)                                                        
*                                                                               
DK20     TM    FLAGFTR,X'80'       DELETED RECS FILTER                          
         BZ    DK30                                                             
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVC   0(3,R3),=C'DEL'                                                  
         LA    R3,3(,R3)                                                        
*                                                                               
DK30     CLI   CODEFTR,0           COPY CODE FILTER                             
         BE    DK32                                                             
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVC   0(2,R3),=CL2'CD'                                                 
         MVI   2(R3),C'='                                                       
         CLI   CODEFTRS,C'Y'                                                    
         BNE   DK31                                                             
         EDIT  (B1,CODEFTR),(3,3(R3)),ALIGN=LEFT                                
         LA    R3,4(,R3)                                                        
         CLI   0(R3),C' '                                                       
         BNH   DK32                                                             
         LA    R3,1(,R3)                                                        
         B     *-12                                                             
*                                                                               
DK31     MVC   3(1,R3),CODEFTR                                                  
         LA    R3,4(,R3)                                                        
*                                                                               
DK32     OC    QMKTFTR,QMKTFTR     MARKET FILTER                                
         BZ    DK34                                                             
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVI   0(R3),C'M'                                                       
         MVI   1(R3),C'='                                                       
         MVC   2(4,R3),QMKTFTR                                                  
         CLI   QMKTFTR+3,C' '                                                   
         BH    *+6                                                              
         BCTR  R3,0                                                             
         LA    R3,6(,R3)                                                        
*                                                                               
DK34     OC    AFFFTR,AFFFTR       AFFILIATE FILTER                             
         BZ    DK36                                                             
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVI   0(R3),C'A'                                                       
         MVI   1(R3),C'='                                                       
         MVC   2(3,R3),AFFFTR                                                   
         LA    R3,5(,R3)                                                        
*                                                                               
DK36     OC    STAFTR,STAFTR       STATION FILTER                               
         BZ    DK38                                                             
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVI   0(R3),C'S'                                                       
         MVI   1(R3),C'='                                                       
         MVC   2(4,R3),STAFTR                                                   
         LA    R3,6(,R3)                                                        
         CLI   STAFTR+4,C'T'                                                    
         BE    DK38                                                             
         MVI   0(R3),C'-'                                                       
         MVC   1(1,R3),STAFTR+4                                                 
         LA    R3,2(,R3)                                                        
*                                                                               
DK38     OC    CMLFTR,CMLFTR       ANY CML FILTERS                              
         BZ    DK40                                                             
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVC   0(2,R3),=CL3'CML'                                                
         MVI   2(R3),C'='                                                       
         MVC   3(8,R3),CMLFTR                                                   
         LA    R3,11(,R3)                                                       
*                                                                               
DK40     OC    PERFTR,PERFTR       PERIOD FILTER                                
         BZ    DK42                                                             
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVI   0(R3),C'P'                                                       
         MVI   1(R3),C'='                                                       
         LA    R2,PERFTR                                                        
         BRAS  RE,PPER                                                          
         MVC   2(17,R3),WORK                                                    
         LA    R3,19(,R3)                                                       
*                                                                               
DK42     TM    FLAGFTR,X'40'       WAS SORT ENTERED                             
         BZ    DK46                                                             
         CR    R3,R4                                                            
         BE    *+12                                                             
         MVI   0(R3),C','                                                       
         LA    R3,1(,R3)                                                        
         MVC   0(8,R3),=C'SORT=REF'                                             
         TM    FLAGFTR,X'20'                                                    
         BZ    DK44                                                             
         MVC   5(4,R3),=C'DATE'                                                 
         LA    R3,1(,R3)                                                        
DK44     LA    R3,8(,R3)                                                        
*                                                                               
DK46     CLC   TRAFLTR,FLD                                                      
         BE    *+14                                                             
         MVC   TRAFLTR,FLD                                                      
         OI    TRAFLTRH+6,X'80'                                                 
*                                                                               
DKX      J     EXIT                                                             
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* GET PROFILE REC(S)                                                            
*================================================================               
                                                                                
GETPRF   NTR1  BASE=*,LABEL=*                                                   
         MVC   SVCLT,BCLT                                                       
*                                                                               
         XC    WORK,WORK           READ T0 PROFILE                              
         MVC   WORK(4),=C'S0T0'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),TRAMED                                                 
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCLTOFF                                              
         MVC   WORK+16(16),WORK    SAVE PROFILE KEY                             
         GOTO1 GETPROF,DMCB,WORK,SVPROF,DATAMGR                                 
*                                                                               
         MVC   WORK(16),WORK+16    RESTORE PROFILE KEY                          
         MVI   WORK+3,C'1'         T1 PROFILE                                   
*                                                                               
         GOTO1 (RF),(R1),,SVT1PROF                                              
*                                                                               
         MVC   WORK(16),WORK+16    RESTORE PROFILE KEY                          
         MVI   WORK+3,C'2'         T2 PROFILE                                   
*                                                                               
         GOTO1 (RF),(R1),,ELEM                                                  
         MVC   SVT2PR03,ELEM+2                                                  
         MVC   SVT2PR9,ELEM+8      BRAND AGENCY                                 
*                                                                               
         MVC   WORK(16),WORK+16    RESTORE PROFILE KEY                          
         MVI   WORK+3,C'3'         T3 PROFILE                                   
         GOTO1 (RF),(R1),,SVT3PROF                                              
*                                                                               
         XC    SVPROGCD,SVPROGCD                                                
*                                                                               
         CLI   SVPROF11,C'P'       RUN BY PROGRAM CODES                         
         JNE   EXIT                                                             
*                                                                               
         MVI   WORK+2,C'B'         BP PROFILE WITH LEGAL PROG CDES              
         MVI   WORK+3,C'P'                                                      
*                                                                               
         GOTO1 (RF),(R1),,SVPROGCD                                              
         MVC   SVT2PR03,ELEM+2                                                  
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
GETSCR   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   BLOCK(TRAXXXXH-TRAKEYH),TRAKEYH   SAVE KEY FIELDS                
*                                                                               
         XC    DMCB(24),DMCB                                                    
         MVC   DMCB+4(3),=X'D90216'                                             
         MVC   DMCB+7(1),MYSCREEN                                               
*                                                                               
         GOTO1 CALLOV,DMCB,TRAKEYH                                              
         CLI   4(R1),X'FF'                                                      
         JNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TRAKEYH(TRAXXXXH-TRAKEYH),BLOCK    RESTORE KEY FIELDS            
*                                                                               
         LA    R1,TRAXXXXH         FIND EOS                                     
         SR    R0,R0                                                            
*                                                                               
GETSCR2  TM    1(R1),X'20'         TEST PROTECTED                               
         BZ    GETSCR4                                                          
         IC    R0,0(R1)                                                         
         AR    R1,R0                                                            
         B     GETSCR2                                                          
*                                                                               
GETSCR4  ST    R1,AFRSTREC         SET A(FIRST UNP FIELD) IN REC                
*                                                                               
GETSCR10 IC    R0,0(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0                                                          
         BNE   GETSCR10                                                         
         MVC   0(3,R1),=X'000101'  ERASE BEFORE/AFTER                           
*                                                                               
         OI    CONSERVH+1,X'01'    FORCE MODIFIED FOR PFKEYS                    
         OI    CONSERVH+6,X'80'                                                 
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*==================================================================             
* READ PREVIOUS/NEXT VERSION OF THIS PATTERN                                    
*==================================================================             
                                                                                
GETVRSN  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   PFKEY,0             TEST REQUEST TO CLEAR VRSN SCRN              
         BE    GETVRS10                                                         
*                                                                               
         CLI   ACTNUM,ACTSEL                                                    
         BNE   *+8                                                              
         MVI   GENPFOPT,C'Y'       IF LIST SELECT, STAY ON THIS REC             
*                                                                               
         LA    R1,CONHEADH                                                      
         SR    R0,R0                                                            
*                                                                               
GETVRS2  IC    R0,0(R1)                                                         
         AR    R1,R0                                                            
         CLI   0(R1),0             FIND E-O-S                                   
         BNE   GETVRS2                                                          
*                                  NOW SAVE OLD SCREEN IN TIA                   
         L     R0,ATWA                                                          
         SR    R1,R0               GIVES LEN TO EOS                             
*                                  NOW SAVE OLD SCREEN IN TIA                   
         L     RE,ATIA                                                          
         LA    RF,1(R1)                                                         
         MVCL  RE,R0                                                            
*                                                                               
         OC    MYSUBLIN,MYSUBLIN                                                
         BNZ   *+10                                                             
         MVC   MYSUBLIN,LATEST                                                  
*                                                                               
         CLI   PFKEY,7             TEST 'LATEST' REQUEST                        
         BNE   GETVRS4                                                          
GETVRSL  SR    R0,R0                                                            
         ICM   R0,3,LATEST                                                      
         B     GETVRS8                                                          
*                                                                               
GETVRS4  CLI   PFKEY,6             TEST GO BACK 1                               
         BNE   GETVRS6                                                          
         LH    R0,MYSUBLIN                                                      
         AHI   R0,-1                                                            
         BZ    GETVRSE1                                                         
         B     GETVRS8                                                          
*                                                                               
GETVRS6  CLI   PFKEY,5             TEST GO FORWARD 1                            
         BNE   GETVRSL             IF PFKEY INVALID, GET LATEST                 
         CLC   MYSUBLIN,LATEST                                                  
         BE    GETVRSE2            NO LATER VERSIONS                            
         LH    R0,MYSUBLIN                                                      
         AHI   R0,1                                                             
*                                                                               
GETVRS8 MVC    KEY,TWAKEYSV                                                     
         ICM   R1,7,KEY+PATKREF-PATKEY       GET REF/SUBLINE                    
         N     R1,=X'00FFFC00'               DROP SUBLINE                       
         X     R0,=X'000003FF'               COMP NEW SUBLINE                   
         OR    R0,R1                                                            
         STCM  R0,7,KEY+PATKREF-PATKEY       SET NEW REF/SUBLINE                
*                                                                               
         LA    R0,12                                                            
         BRAS  RE,TRHIGH                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   GETVRSE2                                                         
*                                                                               
         ICM   R0,7,KEY+10         GET ACTUAL REF/SUB                           
         N     R0,=X'000003FF'     DROP REF                                     
         X     R0,=X'000003FF'     UNCOMPLEMENT                                 
         STH   R0,MYSUBLIN         AND SAVE REFNUM                              
*                                                                               
         CLC   MYSUBLIN,LATEST     TEST BACK TO CURRENT                         
         BNE   *+10                                                             
         XC    MYSUBLIN,MYSUBLIN   CLEAR VERSIONING FLAG                        
*                                                                               
         L     R6,AIO1                                                          
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
                                                                                
*===================================================================            
* NOW FIX THE SCREEN TO PROTECT ALL FIELDS IF NOT CURRENT VERSION               
*===================================================================            
                                                                                
GETVRS10 LA    R4,TRAF3TAB                                                      
         CLI   MYSCREEN,X'F3'                                                   
         BE    GETVRS12                                                         
*                                                                               
         LA    R4,TRA5DTAB                                                      
         CLI   MYSCREEN,X'5D'                                                   
         BE    GETVRS12                                                         
*                                                                               
         LA    R4,TRAC1TAB                                                      
         CLI   MYSCREEN,X'C1'                                                   
         BE    GETVRS12                                                         
         J     EXIT                                                             
*                                                                               
GETVRS12 LA    RE,T216FFD          START OF TWA                                 
         AH    RE,0(R4)            POINT TO THE FLDHDR                          
*                                                                               
         NI    1(RE),X'FF'-X'2C'   SET TO UNP/NORMAL                            
         OC    MYSUBLIN,MYSUBLIN   TEST CURRENT VERSION                         
         BZ    GETVRS16                                                         
         OI    1(RE),X'20'         SET PROTECTED                                
*                                                                               
GETVRS16 LA    R4,2(R4)                                                         
         CLC   0(2,R4),=X'FFFF'                                                 
         BNE   GETVRS12                                                         
*                                                                               
GETVRS20 NI    CONACTH+1,X'FF'-X'20'  UNPROTECT ACTION FIELD                    
         OC    MYSUBLIN,MYSUBLIN      TEST CURRENT VERSION                      
         BZ    *+12                                                             
         OI    CONACTH+1,X'20'        PROTECT ACTION FIELD                      
         OI    CONRECH+4,X'20'        SET FLAG SO KNOW IF REC CHANGES           
*                                                                               
         SR    R0,R0                                                            
GETVRS22 IC    R0,0(RE)                                                         
         AR    RE,R0                                                            
         CLI   0(RE),0             FIND EOS                                     
         BNE   GETVRS22                                                         
         MVC   0(3,RE),=X'000101'  FORCE XMT ALL                                
*                                                                               
         BRAS  RE,SETPFTXT                                                      
         J     EXIT                                                             
*                                                                               
GETVRSE1 MVC   GERROR,=Y(NOOLDER)                                               
         B     GETVRSEX                                                         
*                                                                               
GETVRSE2 MVC   GERROR,=Y(NONEWER)                                               
*                                                                               
GETVRSEX LA    R2,TRACODEH                                                      
*                                                                               
         CLC   MYSUBLIN,LATEST     TEST BACK TO CURRENT                         
         BNE   *+10                                                             
         XC    MYSUBLIN,MYSUBLIN   CLEAR VERSIONING FLAG                        
         J     ERREXIT                                                          
         EJECT                                                                  
*==============================================================                 
* SET PFKEY LINE AS REQUIRED                                                    
*==============================================================                 
                                                                                
SETPFTXT NTR1  BASE=*,LABEL=*                                                   
         NC    PFKCMT+3(7),=7X'BF' MAKE LOWERCASE AS NEEDED                     
         NC    PFKCML+3(4),=7X'BF'                                              
         NC    PF2PTN+3(6),=7X'BF'                                              
         NC    PF3PTN+3(6),=7X'BF'                                              
         NC    PFKVRSN+3(9),=9X'BF'                                             
         NC    PFKVRSN+16(9),=9X'BF'                                            
         NC    PFKVRSN+29(6),=9X'BF'                                            
*                                                                               
         CLI   MYSCREEN,X'F3'      TEST PATTERN BASE                            
         BNE   SETPF1                                                           
         XC    TRAPFK,TRAPFK                                                    
         LA    R2,TRAPFKH                                                       
         LA    R4,8(R2)                                                         
         MVC   0(2,R4),=C'PF'                                                   
         LA    R4,2(R4)                                                         
                                                                                
         MVC   0(L'PFKCMT,R4),PFKCMT    2=COMMENTS                              
         LA    R4,L'PFKCMT+1(R4)                                                
                                                                                
         MVC   0(L'PFKCML,R4),PFKCML    3=CMMLS                                 
         LA    R4,L'PFKCML+1(R4)                                                
         B     SETPF10                                                          
SETPF1   CLI   MYSCREEN,X'5C'      TEST PATTERN CMML                            
         BNE   SETPF2                                                           
         XC    PCOPFK,PCOPFK                                                    
         LA    R2,PCOPFKH                                                       
         LA    R4,8(R2)                                                         
         MVC   0(2,R4),=C'PF'                                                   
         LA    R4,2(R4)                                                         
         MVC   0(L'PFKCMT,R4),PFKCMT    2=COMMENTS                              
         LA    R4,L'PFKCMT+1(R4)                                                
         MVC   0(L'PF3PTN,R4),PF3PTN    3=PATTERN                               
         LA    R4,L'PF3PTN+1(R4)                                                
         B     SETPF10                                                          
                                                                                
SETPF2   CLI   MYSCREEN,X'C1'      TEST PATTERN CMML                            
         BNE   SETPF4                                                           
         XC    TCMPFK,TCMPFK                                                    
         LA    R2,TCMPFKH                                                       
         LA    R4,8(R2)                                                         
         MVC   0(2,R4),=C'PF'                                                   
         LA    R4,2(R4)                                                         
         MVC   0(L'PFKCMT,R4),PFKCMT    2=COMMENTS                              
         LA    R4,L'PFKCMT+1(R4)                                                
         MVC   0(L'PF3PTN,R4),PF3PTN    3=PATTERN                               
         LA    R4,L'PF3PTN+1(R4)                                                
         B     SETPF10                                                          
*                                                                               
SETPF4   CLI   MYSCREEN,X'5D'      TEST PATTERN COMMENT                         
         BNE   SETPF6                                                           
         XC    PCMPFK,PCMPFK                                                    
         LA    R2,PCMPFKH                                                       
         LA    R4,8(R2)                                                         
         MVC   0(2,R4),=C'PF'                                                   
         LA    R4,2(R4)                                                         
         MVC   0(L'PF2PTN,R4),PF2PTN    2=PATTERN                               
         LA    R4,L'PF2PTN+1(R4)                                                
         MVC   0(L'PFKCML,R4),PFKCML    3=CMMLS                                 
         LA    R4,L'PFKCML+1(R4)                                                
         B     SETPF10                                                          
*                                                                               
SETPF6   CLI   MYSCREEN,X'5C'      TEST BPAT CMML SCREEN                        
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    PCOPFK,PCOPFK                                                    
         LA    R2,PCOPFKH                                                       
         LA    R4,8(R2)                                                         
         MVC   0(2,R4),=C'PF'                                                   
         LA    R4,2(R4)                                                         
         MVC   0(L'PF2PTN,R4),PF2PTN    2=PATTERN                               
         LA    R4,L'PF2PTN+1(R4)                                                
         OI    6(R2),X'80'          SET TO XMT                                  
         B     SETPFX               NO PREVIOUS FOR BPAT                        
*                                                                               
SETPF10  OI    6(R2),X'80'         SET FIELD TO XMT                             
*                                                                               
         CLC   LATEST,=H'1'        TEST ONLY ONE VERSION                        
         BE    SETPFX                                                           
*                                                                               
         CLI   ACTNUM,ACTDIS                                                    
         BE    *+12                                                             
         CLI   ACTNUM,ACTSEL                                                    
         BNE   SETPFX                                                           
         MVC   0(L'PFKVRSN,R4),PFKVRSN                                          
*                                                                               
SETPFX   J     EQXIT                                                            
*                                                                               
PFKCMT   DC    C'2=COMMENTS'                                                    
PFKCML   DC    C'3=CMMLS'                                                       
PF2PTN   DC    C'2=PATTERN'                                                     
PF3PTN   DC    C'3=PATTERN'                                                     
PFKVRSN  DC    C'5=NEWER VRSN 6=OLDER VRSN 7=CURRENT'                           
         LTORG                                                                  
         EJECT                                                                  
*================================================================               
* WHEN VERSIONING ACTIVE, SET CHANGED FIELDS TO HIGH INTENSITY                  
*================================================================               
                                                                                
SETFLDS  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R4,TRAF3TAB                                                      
         CLI   MYSCREEN,X'F3'                                                   
         BE    SETFLD2                                                          
*                                                                               
         LA    R4,TRAC1TAB                                                      
         CLI   MYSCREEN,X'C1'      PATT/CMML                                    
         BE    SETFLD2                                                          
*                                                                               
         LA    R4,TRA5DTAB                                                      
         CLI   MYSCREEN,X'5D'      PATT/COMM                                    
         BE    SETFLD2                                                          
         J     EXIT                                                             
*                                                                               
SETFLD2  LA    RE,T216FFD          START OF TWA                                 
         AH    RE,0(R4)            POINT TO THE FLDHDR                          
*                                                                               
         L     RF,ATIA             POINT TO SAVED TWA                           
         AH    RF,0(R4)            POINT TO FLDHDR                              
*                                                                               
         SR    R1,R1                                                            
         IC    R1,0(RE)            GET FIELD LEN                                
         AHI   R1,-9               DECREMENT FOR FLDHDR + EX                    
         TM    1(RE),X'02'         TEST FOR EXTENDED FLDHDR                     
         BZ    *+8                                                              
         AHI   R1,-8                                                            
*                                                                               
         EX    R1,OC8RE                                                         
         EX    R1,OC8RF                                                         
*                                                                               
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,RE),8(RF)       TEST FIELD CHANGED                           
         BE    SETFLD10                                                         
*                                                                               
         OI    1(RE),X'08'         SET HIGH INTENSITY                           
         B     SETFLD10                                                         
*                                                                               
OC8RE    OC    8(0,RE),SPACES                                                   
OC8RF    OC    8(0,RF),SPACES                                                   
*                                                                               
SETFLD10 LA    R4,2(R4)                                                         
         CLC   0(2,R4),=X'FFFF'                                                 
         BNE   SETFLD2                                                          
*                                                                               
         LA    R4,CONHEAD                                                       
         MVC   0(17,R4),=C'CURRENT VERSION -'                                   
         LA    R4,17(R4)                                                        
         LH    R0,LATEST                                                        
         SH    R0,MYSUBLIN                                                      
         EDIT  (R0),(3,(R4)),0,ALIGN=LEFT                                       
         AR    R4,R0                                                            
         MVC   0(4,R4),=C' OF '                                                 
         LA    R4,4(R4)                                                         
         LH    R0,LATEST                                                        
         BCTR  R0,R0                                                            
         EDIT  (R0),(3,(R4)),0,ALIGN=LEFT                                       
         AR    R4,R0                                                            
         MVC   0(10,R4),=C' DISPLAYED'                                          
         OI    GENSTAT2,USMYOK     TELL GENCON I DID MESSAGE                    
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
*                                                                               
         DS    0H                                                               
TRAF3TAB DC    AL2(TRATXTH-T216FFD)                                             
         DC    AL2(TRADESCH-T216FFD)                                            
         DC    AL2(TRAINVPH-T216FFD)                                            
         DC    AL2(TRAPERH-T216FFD)                                             
         DC    AL2(TRALTYH-T216FFD)                                             
         DC    AL2(TRAMS1H-T216FFD)                                             
         DC    AL2(TRAMS2H-T216FFD)                                             
         DC    AL2(TRAMS3H-T216FFD)                                             
         DC    AL2(TRASTIMH-T216FFD)                                            
         DC    AL2(TRAETIMH-T216FFD)                                            
         DC    AL2(TRADLYH-T216FFD)                                             
         DC    AL2(TRADPTH-T216FFD)                                             
         DC    X'FFFF'                                                          
*                                                                               
TRA5DTAB DC    AL2(PCMCMT1H-T216FFD)                                            
         DC    AL2(PCMCMT2H-T216FFD)                                            
         DC    AL2(PCMCMT3H-T216FFD)                                            
         DC    AL2(PCMCMT4H-T216FFD)                                            
         DC    X'FFFF'                                                          
*                                                                               
TRAC1TAB DC    AL2(TCMCMLAH-T216FFD)                                            
         DC    AL2(TCMPCTAH-T216FFD)                                            
         DC    AL2(TCMCMLBH-T216FFD)                                            
         DC    AL2(TCMPCTBH-T216FFD)                                            
         DC    AL2(TCMCMLCH-T216FFD)                                            
         DC    AL2(TCMPCTCH-T216FFD)                                            
         DC    AL2(TCMCMLDH-T216FFD)                                            
         DC    AL2(TCMPCTDH-T216FFD)                                            
         DC    AL2(TCMCMLEH-T216FFD)                                            
         DC    AL2(TCMPCTEH-T216FFD)                                            
         DC    AL2(TCMCMLFH-T216FFD)                                            
         DC    AL2(TCMPCTFH-T216FFD)                                            
         DC    AL2(TCMCMLGH-T216FFD)                                            
         DC    AL2(TCMPCTGH-T216FFD)                                            
         DC    AL2(TCMCMLHH-T216FFD)                                            
         DC    AL2(TCMPCTHH-T216FFD)                                            
         DC    AL2(TCMCMLIH-T216FFD)                                            
         DC    AL2(TCMPCTIH-T216FFD)                                            
         DC    AL2(TCMCMLJH-T216FFD)                                            
         DC    AL2(TCMPCTJH-T216FFD)                                            
         DC    AL2(TCMCMLKH-T216FFD)                                            
         DC    AL2(TCMPCTKH-T216FFD)                                            
         DC    AL2(TCMCMLLH-T216FFD)                                            
         DC    AL2(TCMPCTLH-T216FFD)                                            
         DC    AL2(TCMCMLMH-T216FFD)                                            
         DC    AL2(TCMPCTMH-T216FFD)                                            
         DC    AL2(TCMCMLNH-T216FFD)                                            
         DC    AL2(TCMPCTNH-T216FFD)                                            
         DC    AL2(TCMCMLOH-T216FFD)                                            
         DC    AL2(TCMPCTOH-T216FFD)                                            
         DC    AL2(TCMROTH-T216FFD)                                             
         DC    X'FFFF'                                                          
         EJECT                                                                  
* RDHI TRACE ROUTINE                                                            
TRHIGH   NTR1  BASE=*,LABEL=*                                                   
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  TRACEHI+6(2),DUB                                                 
         GOTO1 DATAMGR,DMCB,=C'DMTRACE',=C'DATA',TRACEHI                        
         XIT1                                                                   
TRACEHI  DC    X'08',C'RDHI=00'                                                 
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPMGRTAB                                                       
         EJECT                                                                  
WORKD    DSECT                                                                  
OLDELEM  DS    CL800               OLD CML ELEM (50CMLS 16 BYTES EACH)          
OLDELEMX EQU   *                                                                
NEWELEM  DS    CL800               NEW CML ELEM                                 
NEWELEMX EQU   *                                                                
PCTELEM  DS    CL150               PERCENT ELEM  3X50 ENTRIES                   
PCTELEMX EQU   *                                                                
WORKX    EQU   *                                                                
         EJECT                                                                  
       ++INCLUDE SPTRPAT                                                        
       ++INCLUDE DDACTIVD                                                       
         EJECT                                                                  
* INCLUDED DSECTS                                                               
* INCLUDE DDSPOOLD                                                              
* INCLUDE DDSPLWORKD                                                            
         PRINT OFF                                                              
       ++INCLUDE SPTRCMML                                                       
         EJECT                                                                  
       ++INCLUDE SPTRPRDEQV                                                     
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
       ++INCLUDE SPTRDTXT                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPTRAFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE SPTRAF3D                                                       
         ORG   TRAKEYH                                                          
       ++INCLUDE SPTRA5DD                                                       
         ORG   TRAKEYH                                                          
       ++INCLUDE SPTRA5CD                                                       
         ORG   TRAKEYH                                                          
       ++INCLUDE SPTRAC1D                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
* INCLUDED DSECTS                                                               
* INCLUDE SPTRAWORKD                                                            
         PRINT OFF                                                              
       ++INCLUDE SPTRAWORKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
SYSD     DSECT                                                                  
         ORG   SVSPAREX                                                         
MAXCMLS  EQU   15                                                               
MAXCELEM EQU   12                  MAX CMLS PER ELEMENT                         
MAXCMLB  EQU   50                  50 CMLS FOR BPAT REC                         
SPTR03RR DS    A                                                                
       ++INCLUDE SPTRA03WRK                                                     
VTRPACK  DS    A                                                                
MAXPCT   DS    F                                                                
MINPCT   DS    F                                                                
COMDIV   DS    F                                                                
PCTNXT   DS    A                   NEXT PERCENT SAVE AREA                       
SVCMLDEL DS    XL1                                                              
SVOLDDPT DS    C                                                                
MYPFKEY  DS    XL1                                                              
CMMLCT   DS    XL1                                                              
DELCT    DS    XL1                                                              
PCTCT    DS    XL1                 PERCENT COUNTER                              
PCTTOT   DS    XL1                 PERCENTAGE TOTAL                             
VCMLFLAG DS    XL1                                                              
*                                                                               
SVPROGCD DS    CL16                VALID PROG CODES IF SVPROF11 = P             
*                                                                               
TEMPKEY  DS    CL48                                                             
TEMPKSV  DS    CL48                                                             
TEMPAIO  DS    CL48                                                             
*                                                                               
         DS    0D                                                               
PCTTBL   DS    XL48                CMML LETTER(1)/CMML PCT(2)                   
REMTBL   DS    XL60                                                             
RELTAB   DS    XL8       4 ENTRIES OF 1 BYTE QUOTIENT, 1 BYTE REMAINDER         
*                                                                               
         ORG   PCTTBL                                                           
NEW10EL  DS    A                                                                
MYPTNSTM DS    XL2                                                              
MYPTNETM DS    XL2                                                              
MYPTNDPT DS    C                                                                
SAMEDTS  DS    C                                                                
MYADJSTM DS    XL2                                                              
MYADJETM DS    XL2                                                              
PTADJSTM DS    XL2                                                              
PTADJETM DS    XL2                                                              
MYADJSDT DS    XL3                                                              
MYADJEDT DS    XL3                                                              
PTADJSDT DS    XL3                                                              
PTADJEDT DS    XL3                                                              
         ORG                                                                    
                                                                                
* NOTE - THAT AUTO REQ ARE BUILT IN BLOCK                                       
* REQHDR IS 26 BYTES LONG, REQUEST 80                                           
*                                                                               
REQHDR   EQU   BLOCK                                                            
REQUEST  EQU   BLOCK+26                                                         
*                                                                               
EQVPRD   DS    CL3                                                              
EQVPRD2  DS    CL3                                                              
SVT2PR03 DS    CL1                                                              
SVT2PR9  DS    CL1                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'187SPTRA03   01/14/20'                                      
         END                                                                    
