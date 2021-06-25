*          DATA SET CTSFM13    AT LEVEL 134 AS OF 08/10/20                      
*PHASE TA0A13C                                                                  
*INCLUDE DLFLD                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE: TA0A13 - DARE STATION ASSIGNMENT MAINT/LIST                 *         
*                                                                     *         
*  COMMENTS: MAINTAINS DARE STATION RECORDS                           *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (TA0A00), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS: SCREENS CTSFMB8 (TA0AB8) -- MAINTENANCE                    *         
*                  CTSFMC8 (TA0AC8) -- UPDATE                         *         
*                  CTSFMB9 (TA0AB9) -- LIST                           *         
*                  CTSFMD8 (TA0AD8) -- REPORT                         *         
*                                                                     *         
*  OUTPUTS: UPDATED DARE STATION RECORDS                              *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - GETEL REGISTER                                        *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - WORK                                                  *         
*          R7 - 2ND BASE                                              *         
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
         EJECT                                                                  
         TITLE 'TA0A13 DARE STATION ASSIGNMENT RECORDS'                         
TA0A13   CSECT                                                                  
*                                                                               
MAXREP2  EQU   6                                                                
*                                                                               
         PRINT NOGEN                                                            
         NMOD1 0,*TA0A13*,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         XC    DMCB,DMCB           CLEAR 1ST PARAMETER OF DMCB                  
         MVC   DMCB+4(4),=X'D9000A03' GET ADDR OF DAYVAL VIA CALLOV             
         GOTO1 CALLOV,DMCB         RETURN WITH ADDR IN 1ST PARAM.               
         CLI   DMCB+4,X'FF'        COULDN'T GET ADDRESS?                        
         JE    *+2                 YES, DIE THEN                                
         MVC   VDAYVAL,DMCB        SAVE THAT ADDRESS FOR LATER                  
         OI    GENSTAT5,NODLST     NO DELETING FROM THE LIST SCREEN             
*                                                                               
         CLC   =C'IR',14(RA)       INTEREP                                      
         BE    MAIN00                                                           
         CLC   =C'K3',14(RA)       KATZ RADIO GROUP                             
         BE    MAIN00                                                           
         CLC   =C'MS',14(RA)       TEST MASTER REP                              
         BE    MAIN00                                                           
         CLC   =C'SJ',14(RA)       SJR                                          
         BE    MAIN00                                                           
         LA    R2,CONRECH                                                       
         MVC   GERROR,=AL2(806)    USER MUST ENTER AT MASTER LEVEL              
REPERROR LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVI   GTMSYS,X'08'        USE GENERAL SYSTEM                           
         DROP  RF                                                               
         B     SFMERROR                                                         
*                                                                               
MAIN00   GOTO1 DATCON,DMCB,(5,0),(0,TODAYDTE)                                   
*                                                                               
         CLI   MODE,RECDEL         DELETE THE RECORD?                           
         BNE   MAIN10                                                           
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BNE   MAIN05              ONLY WE CAN DELETE AND                       
         CLC   SFMCREP,=C'DDS'     MUST HAVE DDS AS REP TO DELETE               
         BE    MAIN10                                                           
*                                                                               
MAIN05   LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVI   GTMSYS,X'FF'        USE GENERAL SYSTEM                           
         DROP  RF                                                               
         MVC   GERROR,=AL2(216)    RECORD/ACTION COMBO INVALID                  
         LA    R2,CONACTH                                                       
         B     SFMERROR                                                         
*                                                                               
MAIN10   CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VK                                                            
         B     EXIT                                                             
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   MAIN20                                                           
         CLI   ACTNUM,ACTUPDTE        FOR DSTA/UPDATE?                          
         BE    MAIN15                                                           
         BAS   RE,VR               VALIDATE RECORD                              
*                                                                               
         CLI   ACTNUM,ACTADD       ADDING FROM DDS TERMINAL?                    
         BNE   *+12                                                             
         CLI   1(RA),C'*'                                                       
         BE    EXIT                DON'T REDISPLAY (RADIO PROTECTS)             
*                                                                               
         BRAS  RE,DR               AND THEN REDISPLAY IT                        
         B     EXIT                                                             
*                                                                               
ACTUPDTE EQU   16                  SPCL ACTION FOR CHANGING RADIO REPS          
*                                                                               
MAIN15   TM    CONACTH+4,X'20'     ACTION FIELD PREVIOUSLY VALIDATED?           
         BZ    *+12                NO, DISPLAY NO MATTER WHAT                   
         TM    MYFLAG,KEYCHNGD                                                  
         BZ    *+12                                                             
         BRAS  RE,DRUPDTE                                                       
         B     EXIT                                                             
         BRAS  RE,VRUPDTE                                                       
         B     EXIT                                                             
*                                                                               
MAIN20   CLI   MODE,XRECPUT        AFTER I PUT TO THE RECORD!                   
         BE    VREC0800                                                         
         CLI   MODE,XRECADD        AFTER I ADD                                  
         BE    VREC0900                                                         
*                                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   *+12                                                             
         BRAS  RE,DR                                                            
         B     EXIT                                                             
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+12                                                             
         BAS   RE,DK                                                            
         B     EXIT                                                             
*                                                                               
         CLI   MODE,LISTRECS       ONLINE LIST RECORDS                          
         BNE   *+12                                                             
         BAS   RE,LISTRECD                                                      
         B     EXIT                                                             
*                                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   YES                                                              
*                                                                               
*   TEST                                                                        
         GOTO1 SPOOL,DMCB,(R8)     DISPLAY RECORD                               
*   TEST END                                                                    
*                                                                               
         BRAS  RE,PR                                                            
         BNZ   VKEY0080            ERROR RETURN: DISPLAY MESSAGE                
         B     EXIT                                                             
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
                                                                                
**********************************************************************          
*   VALIDATE KEY                                                                
**********************************************************************          
VK       NTR1                                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING STAKEYD,R4                                                       
*                                                                               
         CLI   ACTNUM,ACTLIST      DO TESTS IF LIST                             
         BE    VKEY0020                                                         
         CLI   ACTNUM,ACTREP       SKIP TESTS IF REPORT                         
         BNE   VKEY0040                                                         
*                                                                               
VKEY0020 OC    SFLMEDA,SFLMEDA     WE NEED MEDIA                                
         BNZ   VKEY0260                                                         
         B     VKEY0080                                                         
*                                                                               
VKEY0040 MVI   MYFLAG,0                                                         
         LA    R2,SFMMEDAH                                                      
*                                                                               
         TM    4(R2),X'20'         PREV VALIDATED?                              
         BNZ   *+8                                                              
         OI    MYFLAG,KEYCHNGD                                                  
*                                                                               
         CLI   5(R2),1             IS THE MEDIA 1 CHAR?                         
         BNE   VKEY0080                                                         
         CLI   SFMMEDAH+8,C'T'     IS THE MEDIA T?                              
         BNE   VKEY0060                                                         
         CLI   1(RA),C'*'          ONLY DDS CAN LOOK AT MEDIA T                 
         BNE   VKEY0080                                                         
         CLI   ACTNUM,ACTUPDTE                                                  
         BE    VKEY0080            UPDATE ONLY VALID FOR MEDIA R                
         B     VKEY0100                                                         
VKEY0060 CLI   SFMMEDAH+8,C'R'        OR  MEDIA R?                              
         BE    VKEY0100                                                         
*                                                                               
VKEY0080 MVC   GERROR,=AL2(300)    INVALID MEDIA                                
         LA    R2,SFMMEDAH                                                      
         B     SFMERROR                                                         
*                                                                               
VKEY0100 OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         CLI   ACTNUM,ACTUPDTE     UPDATE SCREEN DOES NOT HAVE THE              
         BE    VKEY0110               "CALL" FIELD DEFINED IN CTSFMC8           
         XC    SFMCALL,SFMCALL                                                  
         OI    SFMCALLH+6,X'80'                                                 
*                                                                               
VKEY0110 LA    R2,SFMSTATH                                                      
         TM    4(R2),X'20'         PREV VALIDATED?                              
         BNZ   *+8                                                              
         OI    MYFLAG,KEYCHNGD                                                  
*&&DO                                                                           
         TM    SFMEDATH+4,X'20'    DATE CHANGED?                                
         BZ    *+14                YES                                          
         XC    SFMEDAT,SFMEDAT     NO, STATION CHANGED, RESET DATE              
         MVI   SFMEDATH+5,0                                                     
*&&                                                                             
         GOTOR VALISTA,DMCB,SFMSTATH,STAKSTIN                                   
         BNE   SFMERROR                                                         
*                                                                               
VKEY0260 MVI   STAKSYS,STAKSYSQ    BUILD KEY...                                 
         MVI   STAKTYP,STAKTYPQ                                                 
*                                                                               
         MVC   STAKMEDA,SFMMEDA    ...FOR MAINTENANCE                           
         MVC   TEMPEXT,STAKSTIN+4  CALL LETTER EXTENSION                        
*                                                                               
VKEY0280 OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         CLI   ACTNUM,ACTREP       REPORT REQUEST?                              
         BE    VKEY0290                                                         
         CLI   ACTNUM,ACTLIST       OR LIST REQUEST?                            
         BE    VKEY0290            YES - SKIP DATE CHECK                        
*                                                                               
*   DISPLAY OF A RECORD, AND THE USE OF THE EFFECTIVE DATE.                     
*        GENCON IS VALIDATING THE EXISTENCE OF THE KEY ENTERED.                 
*        IF NO EFFECTIVE DATE IS ENTERED, GENCON WILL ALWAYS RETURN             
*        A 'NO FOUND' CONDITION, WHICH IS NOT CORRECT.  THE PROPER              
*        RETURN IS THE **CURRENT** RECORD FOR THAT STATION.  THIS IS            
*        THE **FIRST** KEY ON FILE FOR THAT STATION.                            
*        THEREFORE, SOME INTERVENTION IS REQUIRED.  IF NO EFF DATE              
*        IS ENTERED ON THE SCREEN, A READ HIGH ON THE GENDIR FOR THE            
*        KEY CONSTRUCTED, UP TO BUT NOT INCLUDING THE EFF DATE, WILL            
*        BE DONE.  IF THE KEY IS FOR THE SAME STATION, THIS KEY,                
*        NOW CONTAINING THE EFF DATE, WILL BE PASSED TO THE GENCON              
*        LOOKUP, WHICH WILL NOW SUCCEED.  IF THE KEY IS NOT FOR THE             
*        SAME STATION, THE ORIGINAL KEY WILL BE RESTORED, AND THE               
*        GENCON LOOKUP WILL FAIL.                                               
*        WHEN AN EFFECTIVE DATE IS ENTERED ON THE SCREEN, IT **MUST**           
*        BE THE EXACT DATE ASSOCIATED WITH THE RECORD, OR THE LOOKUP            
*        WILL FAIL.  WE COULD PLAY GAMES, AND RETURN THE RECORD                 
*        WHICH WAS EFFECTIVE FOR THE DATE ENTERED, BUT IT WOULD BE              
*        MORE REALISTIC TO EXPECT THE USER TO SELECT FROM THE LIST              
*        SCREEN, RATHER THAN BLINDLY INSERTING DATES.  BILL 2/04                
*                                                                               
         LA    R2,SFMEDATH         A(EFFECTIVE DATE)                            
         CLI   5(R2),0             ANY INPUT?                                   
         BNZ   VKEY0320            YES - USE IT                                 
         CLI   ACTNUM,ACTADD       ADD ACTION?                                  
         BNE   VKEY0290            NO  - GET LATEST (1ST) KEY ON FILE           
*                                  YES - USE TODAY AS EFFECTIVE DATE            
         GOTO1 DATCON,DMCB,(5,WORK),(3,WORK+6)                                  
         ZICM  RE,WORK+6,3                                                      
         B     VKEY0350            HANDLE THE KEY                               
VKEY0290 EQU   *                   GET LATEST (1ST) KEY ON FILE                 
         MVC   KEYSAVE,KEY                                                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'GENDIR',KEYSAVE,KEY               
         CLC   KEY(24),KEYSAVE                                                  
         BE    VKEY0300            STATION NOT FOUND                            
         MVC   KEY(27),KEYSAVE     RESTORE ORIGINAL KEY - IT'S                  
*                                     NOT SAME STATION                          
         B     VKEY0360            LET LOOKUP PROCEED: IT WILL FAIL             
VKEY0300 EQU   *                                                                
**       OI    MYFLAG,KEYCHNGD                                                  
         B     VKEY0360                                                         
VKEY0320 EQU   *                                                                
         GOTO1 PERVAL,DMCB,(SFMEDATH+5,SFMEDAT),(X'60',PERVALST)                
         LA    RF,PERVALST                                                      
         USING PERVALD,RF                                                       
         TM    DMCB+4,X'03'                                                     
         JNZ   INVLDATE               DATE NOT ACCEPTABLE                       
*                                                                               
         ZICM  RE,PVALBSTA,3       LOAD TO REG TO INVERT                        
VKEY0350 EQU   *                                                                
         LNR   RE,RE                                                            
         STCM  RE,7,STAKEFDA       INSERT INVERTED INTO KEY                     
         DROP  RF                                                               
*                                                                               
VKEY0360 EQU   *                                                                
         CLI   ACTNUM,ACTLIST                                                   
         BE    VKEY0520                                                         
         CLI   ACTNUM,ACTREP                                                    
         BNE   EXIT                                                             
*                                                                               
         CLI   SFRPENDH+5,0                                                     
         BNE   VKEY0380                                                         
         MVI   SFRPEND,C'N'                                                     
         OI    SFRPEND+6,X'80'                                                  
*                                                                               
VKEY0380 CLI   SFRPEND,C'N'                                                     
         BE    VKEY0400                                                         
         CLI   SFRPEND,C'Y'                                                     
         BNE   INVLFLD                                                          
         B     EXIT                                                             
*                                                                               
INVLDTRG MVC   GERROR,=AL2(330)    INVALID DATE OR DATE RANGE                   
         LA    R2,SFRDATEH                                                      
         B     SFMERROR                                                         
*                                                                               
VKEY0400 XC    LUPDATE,LUPDATE     ACTIVITY DATE FILTER FOR REPORT ONLY         
         CLI   SFRDATEH+5,0                                                     
         BE    VKEY0440            NO                                           
*                                                                               
         ZIC   R2,SFRDATEH+5        INPUT LENGTH                                
         GOTO1 PERVAL,DMCB,((R2),SFRDATE),(X'20',PERVALST)                      
         TM    4(R1),X'01'+X'02'   VALID DATE?                                  
         JNZ   INVLDTRG                                                         
*                                                                               
VKEY0440 MVC   LUPDATE(6),PERVALST+28                                           
         CLI   SFRSORTH+5,0        ANY SORT?                                    
         BE    VKEY0520            NO                                           
*                                                                               
         LLC   R5,SFRSORTH+5                                                    
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   SFRSORT(0),=C'REP'  SORT ON CURRENT REP?                         
         BNE   VKEY0460                                                         
         OI    MYFLAG,SORTREP      YES-SORT ON REP                              
         MVC   SAVESORT,=C'REP   '                                              
         B     VKEY0520                                                         
VKEY0460 EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   SFRSORT(0),=C'STATUS'   SORT ON STATUS?                          
         BNE   VKEY0480                                                         
         OI    MYFLAG,SORTSTAT     YES-SORT ON STATUS                           
         MVC   SAVESORT,=C'STATUS'                                              
         B     VKEY0520                                                         
*                                                                               
VKEY0480 LA    R2,SFRSORTH                                                      
INVLFLD  MVC   GERROR,=AL2(INVALID)                                             
         J     SFMERROR                                                         
*                                                                               
VKEY0520 MVC   STAKMEDA,SFLMEDA    ..OR FOR LIST                                
         XC    STAKSTIN,STAKSTIN                                                
         MVC   STAKSTIN,SFLSTAT    CHECK FILTERS                                
*                                                                               
         LA    R2,SFRFILTH                                                      
         CLI   ACTNUM,ACTREP                                                    
         BE    VKEY0525                                                         
         LA    R2,SFLFILTH                                                      
*                                  Reestablish list columns in case             
         MVC   SFLCOL2+22(12),=CL12'Mkt  User ID'                               
         OI    SFLCOL2H+6,X'80'                                                 
*                                                                               
VKEY0525 TM    4(R2),X'20'                                                      
         BNZ   EXIT                                                             
         MVI   FILTFLAG,0          TURN OFF MKT &USER ID FLAGS                  
         MVI   FLTBAND,0                                                        
         XC    FLTCREP,FLTCREP                                                  
         XC    FLTPREP,FLTPREP                                                  
         XC    FLTCRP2,FLTCRP2                                                  
         XC    FLTPRP2,FLTPRP2                                                  
         XC    FLTNWSTA,FLTNWSTA                                                
         XC    BLOCK1,BLOCK1                                                    
         XC    PREVSTA,PREVSTA                                                  
*                                                                               
         CLI   5(R2),0                                                          
         BE    EXIT                                                             
VKEY0540 GOTO1 SCANNER,DMCB,(R2),BLOCK1                                         
         LA    R5,BLOCK1                                                        
         ZIC   R6,DMCB+4                                                        
*                                                                               
VKEY0541 CLC   =C'REP2',12(R5)                                                  
         JNE   VKEY0543                                                         
         MVC   FLTCRP2,22(R5)                                                   
         J     VKEY0546                                                         
*                                                                               
VKEY0543 CLC   =C'FLTPREP2',12(R5)                                              
         JNE   VKEY0550                                                         
         MVC   FLTPRP2,22(R5)                                                   
*                                                                               
VKEY0546 CLI   ACTNUM,ACTREP                                                    
         JE    VKEY0550                                                         
         MVC   SFLCOL2+22(12),=CL12'Rp2     ' List columns change               
         J     VKEY0580                                                         
*                                                                               
VKEY0550 CLC   =C'REP',12(R5)                                                   
         JNE   VKEY0552                                                         
         CLI   0(R5),3             Avoid confusion between rep and rep2         
         JNE   VKEY0552                                                         
         MVC   FLTCREP,22(R5)                                                   
         J     VKEY0580                                                         
*                                                                               
VKEY0552 CLC   =C'FLTPREP',12(R5)                                               
         JNE   VKEY0558                                                         
         CLI   0(R5),7             Avoid confusion between rep and rep2         
         JNE   VKEY0558                                                         
         MVC   FLTPREP,22(R5)                                                   
         J     VKEY0580                                                         
*                                                                               
VKEY0558 CLC   =C'SHOWSPCL',12(R5)                                              
         JNE   *+12                                                             
         OI    FILTFLAG,SHOWSPCL   TURN ON "SPCL DARE LC" ID REQUEST            
         B     VKEY0580                                                         
*                                                                               
         CLC   =C'SHOWRECV',12(R5)                                              
         JNE   *+12                                                             
         OI    FILTFLAG,SHOWRECV   TURN ON HOME MARKET ID REQUEST               
         J     VKEY0580                                                         
*                                                                               
         CLC   =C'SHOWDIRC',12(R5)                                              
         JNE   *+12                                                             
         OI    FILTFLAG,SHOWDIRC   TURN ON DIRECT TO STATION MKT REQ            
         J     VKEY0580                                                         
*                                                                               
         CLC   =C'DOWNLOAD2',12(R5)                                             
         JNE   *+12                                                             
         MVI   DOWNLOAD,C'2'       TURN ON DOWNLOAD FLAG                        
         J     VKEY0580                                                         
*                                                                               
         CLC   =C'DOWNLOAD',12(R5)                                              
         JNE   *+12                                                             
         MVI   DOWNLOAD,C'Y'       TURN ON DOWNLOAD FLAG                        
         J     VKEY0580                                                         
*                                                                               
         CLC   =C'BAND',12(R5)                                                  
         JNE   *+14                                                             
         MVC   FLTBAND,22(R5)      SAVE OFF THE FILTER                          
         J     VKEY0580                                                         
*                                                                               
         CLC   =C'RECENT',12(R5)   ONLY USE MOST RECENT CALL LETTER?            
         JNE   *+12                                                             
         OI    FILTFLAG,SHOWRCNT   YES, IGNORE OLDER STATIONS                   
         J     VKEY0580                                                         
*                                                                               
         CLC   =C'NEWSTA',12(R5)                                                
         JNE   VKEY0560                                                         
         MVC   FLTNWSTL,1(R5)                                                   
         MVC   FLTNWSTA,22(R5)                                                  
         J     VKEY0580                                                         
*                                                                               
VKEY0560 MVC   GERROR,=AL2(210)    INVALID FILTER EXPRESSION                    
         J     SFMERROR                                                         
*                                                                               
VKEY0580 LA    R5,32(R5)                                                        
         BCT   R6,VKEY0541         GET OUT WHEN NO MORE FILTERS                 
*                                                                               
         OC    FLTCREP,FLTCREP     CHECK FOR CURRENT REP FIRST                  
         BZ    VKEY0670                                                         
         MVC   SAVEKEY3,KEY        SAVE OFF WHAT WE WERE BUILDING               
         LA    R2,FLTCREP                                                       
*                                                                               
VKEY0600 XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTEPRECD,R4                                                      
         MVI   CTEPKSTY,CTEPKSTQ   X'0039' - TYPE AND SUBTYPE                   
         MVC   CTEPKREP(L'FLTCREP),0(R2)                                        
         GOTO1 HIGH                READ FOR THE REQUESTED REP                   
         CLC   KEY(32),KEYSAVE                                                  
         BNE   VKEY0630            NO  - ACCEPTABLE                             
*                                                                               
         CLC   0(3,R2),FLTPREP     CURRENT REP SAME AS PREV REP FILTER?         
         BE    VKEY0650            YES, WE'RE DONE                              
         OC    FLTPREP,FLTPREP     DO WE EVEN HAVE A PREV REP FILTER?           
         BZ    VKEY0650            NO, WE'RE DONE THEN                          
         LA    R2,FLTPREP          YES, CHECK FOR A VALID REP HERE              
         B     VKEY0600                                                         
*                                                                               
VKEY0630 LA    R2,SFLFILTH                                                      
         CLI   ACTNUM,ACTREP                                                    
         BNE   *+8                                                              
         LA    R2,SFRFILTH                                                      
         MVC   GERROR,=AL2(BADREP)   BAD REP FILTER                             
         B     SFMERROR                                                         
*                                                                               
VKEY0650 XC    KEY,KEY             RESTORE                                      
         MVC   KEY(L'SAVEKEY3),SAVEKEY3                                         
VKEY0670 B     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*   VALIDATE RECORD                                                             
*                                                                               
VR       NTR1                                                                   
*                                                                               
         MVC   SAVEKEY2,KEY        SAVE CURRENT KEY FOR RESETTING LATER         
         MVI   MISCFLG1,0                                                       
         XC    OLDREP,OLDREP                                                    
         XC    OLDPREV,OLDPREV                                                  
         L     R3,AIO                                                           
         USING STAREPD,R3                                                       
         MVI   ELCODE,STAREPCQ                                                  
         BAS   RE,GETEL                                                         
         BNE   VREC0010            MUST BE A REP ELEMENT                        
         USING STAREPD,R3          REP ELEMENT DSECT                            
         MVC   OLDREP,STAREPCR     SAVE OLD REP VALUES                          
         MVC   OLDPREV,STAREPPR                                                 
*                                                                               
         XC    OLDSTAT,OLDSTAT     CLEAR OLD 'NEW STATION' STORAGE              
         L     R3,AIO                                                           
         USING STAKEYD,R3                                                       
         MVI   ELCODE,STASTACQ     RETRIEVE OLD 'NEW STATION' ELEMENT           
         BAS   RE,GETEL                                                         
         BNE   VREC0010            NOT FOUND                                    
         MVC   OLDSTAT,STASTA-STASTAC(R3)                                       
*                                  SAVE 'NEW STATION' CALLS                     
         MVC   OLDEFDAT,STASTADT-STASTAC(R3)                                    
*                                  SAVE 'NEW STATION' EFF DATE                  
VREC0010 EQU   *                                                                
         L     R3,AIO                                                           
         USING STAKEYD,R3                                                       
         MVI   ELCODE,STAREPCQ     REMOVE REP ELEMENT TO REBUILD IT             
         GOTO1 REMELEM                                                          
         MVI   ELCODE,STARP2CQ     REMOVE 2ND REP ELEM TO REBUILD IT            
         GOTO1 REMELEM                                                          
         MVI   ELCODE,STASTACQ     REMOVE NEW STATION ELEMENT                   
         GOTO1 REMELEM                                                          
         MVI   ELCODE,STAHOMCQ     REMOVE MARKET ELEMENT CODE                   
         GOTO1 REMELEM                                                          
         MVI   ELCODE,STAH2CQ      HIDDEN DARE ELEMENT CODE                     
         GOTO1 REMELEM                                                          
         MVI   ELCODE,STABLELQ     BILL TO ELEMENT CODE                         
         GOTO1 REMELEM                                                          
*                                                                               
VREC0020 XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING STAREPD,R3          REP ELEMENT DSECT                            
         MVI   STAREPC,STAREPCQ                                                 
         MVI   STAREPLN,STAREPLQ                                                
         EJECT                                                                  
*===============================================================                
* VALIDATE THE REP                                                              
*===============================================================                
VREC0040 LA    R2,SFMCREPH                                                      
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1RCCHG   NO                                           
*                                                                               
* AN 'X' IN DATE MEANS CLEAR PREVIOUS REP FIELD                                 
*                                                                               
         CLI   SFMDATE,C'X'        TEST DATE INPUT IS 'X'                       
         BNE   VREC0060            NO                                           
         MVC   GERROR,=AL2(NOPRVREP)                                            
         OC    OLDPREV,OLDPREV     TEST ANY PREVIOUS REP                        
         BZ    SFMERROR            NO - ERROR                                   
         MVC   STAREPCR,OLDPREV    MOVE PREVIOUS TO CURRENT                     
         OI    MISCFLG1,MF1RCCHG                                                
         B     VREC0200                                                         
*                                                                               
VREC0060 OC    SFMCREP,SPACES      MUST INIT TO SPACES                          
*                                                                               
         CLC   OLDREP,SFMCREP      TEST REP HAS CHANGED                         
         BNE   VREC0080            YES                                          
         MVC   STAREPCR,OLDREP     RESTORE OLD REP VALUES                       
         MVC   STAREPPR,OLDPREV                                                 
*                                                                               
         MVI   STATUS2,0                                                        
         CLC   SFMCREP,=C'EOC'     EOC - END OF CONTRACT (TV)?                  
         BE    *+10                                                             
         CLC   SFMCREP,=C'ENC'     ENC - END OF CONTRACT (RADIO)?               
         BNE   VREC0100                                                         
         OI    STATUS2,STT2EOC     YES, SET STATUS                              
         B     VREC0100                                                         
*                                                                               
VREC0080 LA    R1,SFMCREP                                                       
         BRAS  RE,REPSTAT                                                       
         MVC   STAREPCR,SFMCREP                                                 
         MVC   STAREPPR,OLDREP                                                  
*                                                                               
VREC0100 DS    0H                                                               
         OI    SFMCREPH+4,X'20'    VALIDATE THE REP                             
         EJECT                                                                  
*===============================================================                
* VALIDATE THE THE EFFECTIVE DATE                                               
*===============================================================                
VREC0120 LA    R2,SFMDATEH                                                      
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1RCCHG   NO                                           
*                                                                               
         CLI   SFMDATEH+5,0        TEST DATE INPUT                              
         BNE   VREC0160            YES                                          
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    VREC0180            YES, DATE NOT MANDATORY                      
         CLC   OLDREP,SFMCREP      DID THE REP CHANGE?                          
         BE    VREC0180            NO, DATE NOT NEEDED                          
INVLDATE MVC   GERROR,=AL2(BADDATE)  INVALID EFFECTIVE DATE                     
         B     SFMERROR                                                         
*                                                                               
VREC0160 GOTO1 PERVAL,DMCB,(SFMDATEH+5,SFMDATE),(X'60',PERVALST)                
         LA    R6,PERVALST                                                      
         USING PERVALD,R6                                                       
         TM    DMCB+4,X'03'                                                     
         JNZ   INVLDATE            INVALID DATE                                 
*                                                                               
         GOTO1 DATCON,DMCB,(0,PVALESTA),(15,STAREPED)                           
         DROP  R6                                                               
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VREC0180                                                         
         GOTO1 DATCON,DMCB,(5,0),(15,FULL)                                      
         CLC   STAREPED,FULL                                                    
         JNH   INVLDATE            INVALID DATE                                 
*                                                                               
VREC0180 OI    SFMDATEH+4,X'20'    VALIDATE THE EFF DATE                        
         EJECT                                                                  
*===============================================================                
* NEED TO SET DARE/NOT DARE CODES FOR CURRENT/PREVIOUS REPS                     
*===============================================================                
VREC0200 DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VREC0210                                                         
         BAS   RE,RESDREC          RESTORE DSTA RECORD READ SEQ                 
*===============================================================                
* SET ELECTRONIC INVOICE FLAG                                                   
*===============================================================                
VREC0210 LA    R2,SFMEINVH                                                      
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1RCCHG   NO                                           
         CLI   5(R2),0                                                          
         BE    VREC0215                                                         
         CLI   8(R2),C'N'                                                       
         BE    VREC0215                                                         
         MVC   GERROR,=AL2(238)    YES OR NO                                    
         CLI   8(R2),C'Y'                                                       
         BNE   SFMERROR                                                         
         OI    STAREPFG,STAREPFG_EI                                             
*                                                                               
VREC0215 DS    0H                                                               
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         JNE   *+2                                                              
         OI    SFMEINVH+4,X'20'    VALIDATE THE ELECTRONIC INVOICE FLAG         
         EJECT                                                                  
*===============================================================                
* VALIDATE THE SECOND REP                                                       
*===============================================================                
VREC0230 LA    R2,SFMCRP2H                                                      
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1RCCHG   NO                                           
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING STAREPD,R3          2ND REP ELEMENT DSECT                        
         MVI   STAREPC,STARP2CQ    X'11'                                        
         MVI   STAREPLN,STARP2LQ                                                
*                                                                               
         LA    R2,SFMCRP2H         POINT TO THE 2ND REP                         
         CLI   5(R2),0             DO WE HAVE INPUT HERE?                       
         BE    VREC0239            NONE                                         
*                                                                               
         BRAS  RE,DO2REP           PROCESS 2ND REPS                             
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VREC0235                                                         
         BAS   RE,RESDREC          RESTORE DSTA RECORD READ SEQ                 
*                                                                               
VREC0235 GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         JNE   *+2                                                              
VREC0239 OI    SFMCRP2H+4,X'20'    VALIDATE THE 2ND REP FIELD                   
         DROP  R3                                                               
*=============================================================                  
* EDIT NEW STATION CALL LETTERS AND EFFECTIVE DATE                              
*=============================================================                  
VREC0240 LA    R2,SFMNSTAH                                                      
         TM    4(R2),X'20'         STATION PREVIOUSLY VALIDATED?                
         BNZ   *+8                 YES                                          
         OI    MISCFLG1,MF1RCCHG   NO  - SET RECORD CHANGED                     
*                                                                               
         CLI   5(R2),0             ANY NEW CALL LETTERS?                        
         BE    VREC0280            NONE                                         
*                                                                               
         LA    R2,SFMNSDTH         CALL LETTERS: ANY DATE?                      
         CLI   5(R2),0                                                          
         JNE   VREC0300            YES                                          
         J     MISSFLD                                                          
*                                                                               
VREC0280 EQU   *                                                                
         LA    R2,SFMNSDTH         NO CALL LETTERS: ANY DATE?                   
         CLI   5(R2),0                                                          
         BE    VREC0480            NO DATE, WE'RE OKAY                          
         MVC   GERROR,=AL2(DATEXTRA)  YES, DATE NOT ALLOWED                     
         B     SFMERROR                                                         
*                                                                               
VREC0300 EQU   *                                                                
         MVC   GERROR,=AL2(INVSTA)                                              
         LA    R2,SFMNSTAH                                                      
*                                                                               
* BUILD NEW STATION ELEMENT                                                     
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING STASTAD,R3                                                       
         MVI   STASTAC,STASTACQ    X'20' - NEW STATION ELEMENT                  
         MVI   STASTALN,STASTALQ                                                
*                                                                               
         CLI   SFMNSTAH+12,0           DID USER ENTER A BAND?                   
         BNE   *+10                                                             
         MVC   SFMNSTAH+12(1),TEMPEXT  CALL LETTER EXTENSION                    
                                                                                
         GOTOR VALISTA,DMCB,SFMNSTAH,STASTA                                     
         BNE   SFMERROR                                                         
*                                                                               
* MAKE SURE NEW CALL LETTERS ARE NOT THE SAME AS STATION CALL LETTERS           
         CLC   STASTA(4),SFMSTAT     SAME CALL LETTERS W/O THE POWER?           
         BNE   VREC0400                                                         
         CLI   SFMMEDA,C'R'            RADIO?                                   
         BNE   VREC0360                                                         
         CLC   STASTA+4(1),SFMSTAT+5   YES, SAME BAND?                          
         BE    SFMERROR                                                         
         B     VREC0400                                                         
*                                      NO, TELEVISION                           
VREC0360 CLI   SFMSTAT+5,C'L'              LOW POWER?                           
         BNE   VREC0380                                                         
         CLI   STASTA+4,C'L'                                                    
         BE    SFMERROR                ERROR IF BOTH STA ARE THE SAME           
         B     VREC0400                                                         
*                                                                               
VREC0380 CLI   STASTA+4,C'T'       NO, TELEVISION                               
         BE    SFMERROR                ERROR IF BOTH STA ARE THE SAME           
*                                                                               
VREC0400 LA    R2,SFMNSDTH         VALIDATE DATE (REQUIRED)                     
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1RCCHG   NO                                           
*                                                                               
         CLI   5(R2),0                                                          
         BE    VREC0420                                                         
         GOTO1 PERVAL,DMCB,(SFMNSDTH+5,SFMNSDT),(X'60',PERVALST)                
         LA    R6,PERVALST                                                      
         USING PERVALD,R6                                                       
         TM    DMCB+4,X'03'                                                     
         JNZ   INVLDATE                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(0,PVALESTA),(15,STASTADT)                           
         GOTO1 DATCON,DMCB,(0,PVALESTA),(3,WORK+64)                             
         ZICM  RE,WORK+64,3        INVERT THIS DATE                             
         LNR   RE,RE                                                            
         STCM  RE,7,WORK+64                                                     
         DROP  R6                                                               
*                                  SAVE FOR NEW STATION VALIDATION              
*                                                                               
VREC0420 DS    0H                  ADD ELEMENT TO RECORD                        
         GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         JNE   *+2                                                              
*                                                                               
*   CHECK FOR EXISTENCE OF NEW STATION ON FILE                                  
*                                                                               
         TM    SFMNSTAH+4,X'20'    NEW CALLS AND STA DATE PREVIOUSLY            
         BZ    VREC0450               VALIDATED?                                
         TM    SFMNSDTH+4,X'20'                                                 
         BNZ   VREC0500            YES                                          
*                                                                               
VREC0450 MVC   KEY,SAVEKEY2        RESET ORIGINAL KEY                           
*                                     READ SEQUENCE                             
         MVC   KEY+STAKSTIN-STAKEY(5),STASTA                                    
*                                  INSERT NEW STATION INTO KEY                  
         MVC   KEY+STAKEFDA-STAKEY(3),WORK+64                                   
*                                  INSERT NEW EFF DATE INTO KEY                 
         MVC   SAVEKEY,KEY         SAVE KEY FOR COMPARISON                      
         MVC   SAVEKEY3,KEY        SAVE KEY FOR NEW STN ADD LATER               
         GOTO1 HIGH                READ NEW STATION KEY                         
         CLC   KEY(32),SAVEKEY     KEY ALREADY ON FILE?                         
         BNE   VREC0460            NO  - ACCEPTABLE                             
         MVC   GERROR,=AL2(STAXISTS) STATION/DATE ON FILE:  ERROR               
         B     SFMERROR                                                         
*                                                                               
VREC0460 MVC   KEY(32),SAVEKEY2    RESET ORIGINAL KEY                           
         GOTO1 HIGH                REREAD KEY                                   
         CLC   KEY(32),SAVEKEY2    KEY REREAD SUCCESSFULLY?                     
         JNE   *+2                 ORIGINAL KEY MUST BE FOUND                   
*                                                                               
         MVC   AIO,AIO3            READ INTO AIO3 TO RESET                      
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         OI    MISCFLG1,MF1NWSTA   SET 'NEW STATION NEEDED'                     
*                                                                               
         MVC   AIO,AIO1            RESET A(ORIGINAL RECORD AREA)                
*                                                                               
VREC0480 DS    0H                                                               
         OI    SFMNSTAH+4,X'20'    TURN ON PREVIOUSLY VALID FLAG                
         OI    SFMNSDTH+4,X'20'    TURN ON PREVIOUSLY VALID FLAG                
*=================================================================              
* EDIT HOME MARKET ELEMENT                                                      
*=================================================================              
VREC0500 DS    0H                                                               
         LA    R2,SFMCITYH                                                      
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1RCCHG   NO                                           
*                                                                               
         CLI   SFMCITYH+5,0        ANY HOME MARKET CITY?                        
         BNE   VREC0510                                                         
         CLI   SFMRIDH+5,0         NONE                                         
         BE    VREC0580            OKAY IF NO RECEIVING ID TOO                  
MISSFLD  MVC   GERROR,=AL2(MISSING)                                             
         B     SFMERROR                                                         
*                                                                               
VREC0510 CLI   SFMCITYH+5,2        2 CHARACTERS LONG?                           
         JNE   INVLFLD             NO, INVALID                                  
*                                                                               
         CLC   =C'**',SFMCITY      IN EFFECT FOR ALL CITIES?                    
         BNE   VREC0520                                                         
         CLC   SFMCREP,=C'NOR'     THEN CURRENT REP HAS TO BE NOR               
         BNE   INVLFLD             SO WE CAN IMPLEMENT DIRECT TO STA            
         B     VREC0530                                                         
*                                                                               
VREC0520 TM    SFMCITYH+4,X'04'    ALPHABETIC?                                  
         BO    VREC0530                                                         
         MVC   GERROR,=AL2(NOTALPHA)                                            
         B     SFMERROR                                                         
*                                                                               
VREC0530 XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING STAHOMD,R3          HOME MARKET ELEMENT DSECT                    
         MVI   STAHOMC,STAHOMCQ                                                 
         MVI   STAHOMLN,STAHOMLQ                                                
         MVC   STAHOMCT,SFMCITY                                                 
*                                                                               
         MVC   SAVEKEY,KEY                                                      
***************                                                                 
* RECEIVING ID                                                                  
***************                                                                 
         LA    R2,SFMRIDH                                                       
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1RCCHG   NO                                           
*                                                                               
         CLI   SFMRIDH+5,0         NO RECEIVING ID?                             
         BNE   VREC0540                                                         
         CLC   =C'**',SFMCITY      IN EFFECT FOR ALL CITIES?                    
         BNE   VREC0570            FAX DIRECTLY TO STATION                      
         B     MISSFLD             WE NEED A RECEIVING ID FOR **                
*                                                                               
VREC0540 XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIREC,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,SFMRID                                                    
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,MYIO                  
         LA    R4,MYIO                                                          
         OC    SFMRID,=C'          '                                            
         CLC   SFMRID,CTIKID                                                    
         JNE   INVLFLD                                                          
         MVC   STAHOMIC,CTIKID                                                  
*                                                                               
         LA    R4,CTIDATA                                                       
VREC0550 CLI   0(R4),X'00'         END OF RECORD?                               
         BNE   *+6                                                              
         DC    H'00'                                                            
         CLI   0(R4),X'02'                                                      
         BE    VREC0560                                                         
         ZIC   R5,1(R4)            LENGTH                                       
         AR    R4,R5                                                            
         B     VREC0550                                                         
*                                                                               
VREC0560 MVC   STAHOMIB,2(R4)      HEXADECIMAL ID                               
*                                                                               
VREC0570 DS    0H                                                               
         GOTO1 ADDELEM             ADD ELEMENT TO RECORD                        
         CLI   DMCB+12,0                                                        
         JNE   *+2                                                              
         MVC   KEY,SAVEKEY                                                      
*                                                                               
VREC0580 DS    0H                                                               
         OI    SFMCITYH+4,X'20'    VALIDATE THE HOME MARKET                     
         OI    SFMRIDH+4,X'20'     VALIDATE THE RECEIVING ID                    
*=================================================================              
* EDIT SPECIAL HIDDEN ELEMENT                                                   
*=================================================================              
         LA    R2,SFMDLIDH         SPECIAL DARE LOCAL ID                        
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1RCCHG   NO                                           
*                                                                               
         CLI   SFMDLIDH+5,0        NONE                                         
         BE    VREC0590            OKAY IF NO RECEIVING ID TOO                  
*                                                                               
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIREC,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKID,SFMDLID                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,MYIO                  
         LA    R4,MYIO                                                          
         OC    SFMDLID,=C'          '                                           
         CLC   SFMDLID,CTIKID                                                   
         JNE   INVLFLD                                                          
         MVC   STAHOMIC,CTIKID                                                  
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING STAH2D,R3           HOME MARKET ELEMENT DSECT                    
         MVI   STAH2C,STAH2CQ                                                   
         MVI   STAH2LN,STAH2LQ                                                  
         MVC   STAH2IC,CTIKID                                                   
         DROP  R3,R4                                                            
*                                                                               
         GOTO1 ADDELEM             ADD ELEMENT TO RECORD                        
         CLI   DMCB+12,0                                                        
         JNE   *+2                                                              
         MVC   KEY,SAVEKEY                                                      
*                                                                               
VREC0590 OI    SFMDLIDH+4,X'20'                                                 
*=================================================================              
* EDIT BILL TO FIELD                                                            
*=================================================================              
         LA    R2,SFMBLTOH         BILL TO FIELD                                
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED?                        
         JNZ   *+8                 YES                                          
         OI    MISCFLG1,MF1RCCHG   NO                                           
*                                                                               
         L     R4,AIO                                                           
         USING STAKEYD,R4                                                       
         CLI   STAKSTIN+4,C'D'     CHECK IF DIGITAL BAND                        
         JE    VREC0600                                                         
         CLI   STAKSTIN+4,C'S'                                                  
         JE    VREC0600                                                         
         CLI   STAKSTIN+4,C'C'                                                  
         JE    VREC0600                                                         
* NOT A DIGITAL BAND                                                            
         CLI   5(R2),0             DO WE HAVE A BILL TO?                        
         JNE   INVLFLD             OKAY IF NO RECEIVING ID TOO                  
         J     VREC0690                                                         
*                                                                               
VREC0600 CLI   5(R2),0             WE NEED A BILL TO                            
         JE    MISSFLD                                                          
*                                                                               
         LAY   RF,VRECBL2R         LIST OF RADIO DIGITAL VENDORS                
         CLI   SFMMEDA,C'R'        DIGITAL RADIO?                               
         BE    VREC0610                                                         
         LAY   RF,VRECBL2T         LIST OF TV DIGITAL VENDORS                   
         CLI   SFMMEDA,C'T'        DIGITAL TELEVISION?                          
         BNE   INVLFLD             NEITHER, DON'T KNOW WHAT IT IS               
*                                                                               
VREC0610 CLI   0(RF),0             EOL?                                         
         JE    INVLFLD                                                          
         LLC   R1,0(RF)                                                         
         CLC   5(1,R2),0(RF)       LENGTH MATCH?                                
         JE    VREC0620                                                         
VREC0615 LA    RF,1(R1,RF)                                                      
         J     VREC0610                                                         
*                                                                               
VREC0620 BCTR  R1,0                SET UP FOR EX                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),1(RF)                                                    
         JE    VREC0630                                                         
         LA    R1,1(R1)                                                         
         J     VREC0615                                                         
*                                                                               
VREC0630 XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING STABLELD,R6                                                      
         MVI   STABLEL,STABLELQ    X'40' - BILL TO ELEMENT CODE                 
         MVI   STABLLEN,STABLLNQ                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   STABLWHO(0),8(R2)                                                
         OC    STABLWHO,SPACES                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 ADDELEM             ADD ELEMENT TO RECORD                        
         CLI   DMCB+12,0           ANY ERRORS?                                  
         JNE   *+2                 DIE THEN                                     
*                                                                               
VREC0690 OI    SFMBLTOH+4,X'20'    NOW VALIDATED                                
***********************************************************************         
VREC0700 TM    MISCFLG1,MF1RCCHG                                                
         BNZ   VREC750                                                          
         LA    R2,SFMCREPH                                                      
         MVI   GERROR,0                                                         
         MVI   GERROR1,4           ENTER CHANGES                                
         MVI   GMSGTYPE,C'I'       INFO MESSAGES                                
         B     SFMERROR                                                         
*                                                                               
VREC750 BRAS   RE,UPDATTIM                                                      
*                                                                               
         B     EXIT                                                             
***********************************************************************         
* SECTION IS FOR XRECPUT                                                        
***********************************************************************         
VREC0800 EQU   *                                                                
         TM    MISCFLG1,MF1NWSTA   NEED TO ADD NEW STATION RECORD?              
         BNO   VREC0900            NO                                           
*                                                                               
*   THE ORIGINAL DSTA RECORD HAS BEEN UPDATED AND REWRITTEN.  IT IS             
*        STILL IN THE IOAREA.  THAT RECORD WILL BE USED TO ADD A                
*        NEW STATION RECORD TO THE FILE.                                        
*        1.    THE NEW STATION KEY (IN SAVEKEY3) WILL BE INSERTED               
*        2.    THE NEW STATION ELT (FOR THE ORIGINAL RECORD) WILL               
*              BE DROPPED, AS IT DOESN'T BELONG IN THE NEW RECORD               
*        3.    THE RECORD WILL BE 'ADDED,' WHICH WILL ADD THE KEY               
*                                                                               
         L     R3,AIO              SET A(NEW STATION RECORD)                    
         MVC   0(32,R3),SAVEKEY3   INSERT NEW STATION KEY                       
         MVI   ELCODE,STASTACQ     X'20' - DELETE NEW STATION ELEM              
*                                                                               
         GOTO1 REMELEM                                                          
*                                                                               
         XC    KEY,KEY             CLEAR KEY                                    
         MVC   KEY(32),SAVEKEY3                                                 
         OI    DMINBTS,X'88'       READ FOR UPDATE + DELETED                    
         GOTO1 HIGH                READ FOR EXISTENCE                           
         CLC   KEY(32),SAVEKEY3    KEY ON FILE?                                 
         BNE   VREC0840            NO  - DO AN ADDREC                           
         OI    DMINBTS,X'88'       YES - RETRIEVE DELETED RECORD                
         MVC   FULL,AIO            SAVE CURRENT A(IO AREA)                      
         MVC   AIO,AIO3            SET A(IO AREA3)                              
         GOTO1 GETREC              READ DELETED REC FOR UPDATE                  
         MVC   AIO,FULL            IO AREA WITH NEW RECORD                      
         GOTO1 PUTREC              REWRITE 'NEW' VERSION                        
         MVI   KEY+32,0            CLEAR STATUS BYTE OF KEY                     
         GOTO1 WRITE               REWRITE KEY AS UNDELETED                     
         B     VREC0860                                                         
*                                                                               
VREC0840 EQU   *                                                                
         GOTO1 ADDREC              ADD NEW DSTA RECORD                          
*                                                                               
*   NOW MUST SEE IF THE 'NEW STATION' REPLACES A PREVIOUS 'NEW                  
*        STATION'.  IF THIS IS THE CASE, IT MEANS THE USER HAS                  
*        MADE A CHANGE TO WHAT WAS PREVIOUSLY ENTERED, AND THE                  
*        LATEST MUST SUPERCEDE THE EARLIER, WHICH MUST BE REMOVED.              
*                                                                               
VREC0860 EQU   *                                                                
         OC    OLDSTAT,OLDSTAT     ANY OLD 'NEW STATION'?                       
         BZ    VREC0900            NO  - NOTHING TO DO                          
         XC    KEY,KEY             CLEAR THE KEY AREA                           
         L     R3,AIO              SET A(RECORD JUST WRITTEN)                   
         MVC   KEY(32),0(R3)       RESET KEY                                    
         MVC   STAKSTIN-STAKEY+KEY(5),OLDSTAT                                   
*                                  INSERT OLD STATION INTO KEY                  
         GOTO1 DATCON,DMCB,(6,OLDEFDAT),(3,WORK)                                
         ZICM  RF,WORK,3                                                        
         LNR   RF,RF                                                            
         STCM  RF,7,STAKEFDA-STAKEY+KEY                                         
*                                  INSERT OLD EFF DATE INTO KEY                 
         MVC   SAVEKEY,KEY         SAVE KEY FOR COMPARISON                      
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                READ NEW STATION KEY                         
         CLC   KEY(32),SAVEKEY     KEY ON FILE?                                 
         BNE   VREC0900            NO  - OR IT HAS BEEN DELETED                 
         MVI   KEY+32,X'FF'        YES - MARK FOR DELETION                      
         GOTO1 WRITE               REWRITE THE KEY                              
***********************************************************************         
* SECTION IS FOR XRECADD                                                        
***********************************************************************         
VREC0900 EQU   *                                                                
         MVC   BLOCK+1(58),=C'FOR IMMEDIATE UPDATE TO DARE, PLEASE CALLX        
                COMPUTER ROOM!!!'                                               
         MVI   BLOCK+59,0          TERMINATING ZERO                             
         LHI   RF,60                                                            
         STC   RF,BLOCK                                                         
*                                                                               
         MVI   GERROR,0                                                         
         MVI   GERROR1,65          &1                                           
         MVI   GMSGTYPE,C'I'       INFO MESSAGES                                
         MVI   GETMSYS,10                                                       
         OI    GENSTAT2,USGETTXT                                                
         XC    GETTXTCB,GETTXTCB                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         LA    RE,BLOCK                                                         
         STCM  RE,7,GTASUBST                                                    
         B     SFMERROR                                                         
         DROP  RF                                                               
         EJECT                                                                  
*================================================================               
* RESTORE THE CURRENT DSTA RECORD INTO AIO1                                     
*                                                                               
* ON ENTRY : SAVEKEY2 HAS THE CURRENT KEY                                       
*                                                                               
* ON EXIT  : DSTA READ SEQUENCE IS RESTORED                                     
*          : AIO IS POINTING AT AIO1                                            
*                                                                               
* NOTE :  AIO3 GETS CLOBBERED                                                   
*================================================================               
RESDREC  NTR1                                                                   
         MVC   KEY(32),SAVEKEY2    RESET ORIGINAL KEY                           
         GOTO1 HIGH                REREAD KEY                                   
         CLC   KEY(32),SAVEKEY2    KEY REREAD SUCCESSFULLY?                     
         JNE   *+2                                                              
         MVC   AIO,AIO3            READ INTO AIO3 TO RESET                      
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1            RESET A(ORIGINAL RECORD AREA)                
         B     EXIT                                                             
*================================================================               
* VALIDATE REP CODE AND LOOK UP STATUS FOR DARE OR NOTDARE                      
*                                                                               
* ON ENTRY:    (R1)                A(REP CODE TO LOOKUP)                        
* ON EXIT:     STATUS                                                           
*        :     STATUS2                                                          
*                                                                               
* NOTE:  AIO2  GETS CLOBBERED                                                   
*================================================================               
REPSTAT  NTR1                                                                   
         MVC   AIO,AIO2                                                         
         MVI   STATUS,0                                                         
         MVI   STATUS2,0                                                        
         LR    R0,R1               SAVE ADDRESS OF REP CODE                     
         OC    0(3,R1),0(R1)                                                    
         BZ    GETSTX                                                           
*                                                                               
         XC    KEY,KEY             CHECK EDIPARTNER RECORD                      
         LA    R2,KEY                                                           
         USING CTEPRECD,R2                                                      
         MVI   CTEPKSTY,CTEPKSTQ   X'0039' - TYPE AND SUBTYPE                   
         MVC   CTEPKREP,0(R1)                                                   
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BNE   GETSTERR            BAD REP                                      
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         LA    R6,CTEPDAT-CTEPRECD(R6)                                          
GETST10  CLI   0(R6),0                                                          
         JE    *+2                 DIE IF NO PRIMARY INFO ELEM                  
         CLI   0(R6),CTEPRELQ      X'02' - PRIMARY INFO ELEMENT                 
         BE    GETST20                                                          
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     GETST10                                                          
*                                                                               
         USING CTEPRD,R6                                                        
GETST20  CLC   SFMMEDA,CTEPRMED    MUST MATCH MEDIA                             
         JNE   GETSTERR                                                         
         TM    CTEPFLG1,CTE1NDAR   NOT A DARE REP?                              
         JZ    *+8                                                              
         OI    STATUS,STT1NDAR     TURN ON 'NOT A DARE REP' FLAG                
*                                                                               
         CLC   CTEPKREP,=C'EOC'    IF REP EOC?                                  
         BE    GETST30              YES                                         
         CLC   CTEPKREP,=C'ENC'    IF REP ENC?                                  
         BE    GETST30              YES                                         
         TM    CTEPFLG2,CTE2EOC    O/W TEST END OF CONTRACT FLAG?               
         JZ    *+8                                                              
GETST30  OI    STATUS2,STT2EOC     TURN ON 'END OF CONTRACT' FLAG               
*                                                                               
GETSTX   MVC   AIO,AIO1                                                         
         J     EXIT                                                             
*                                                                               
GETSTERR OI    STATUS2,STT2NVR     BAD REP                                      
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    GETSTX                                                           
         LA    R2,SFMCREPH                                                      
         MVC   GERROR,=AL2(BADREP) NO, BAD REP FOR THIS MEDIA                   
         J     SFMERROR                                                         
         DROP  R6,R2                                                            
         EJECT                                                                  
*                                                                               
*   LIST RECORDS                                                                
*                                                                               
LISTRECD NTR1                                                                   
         LA    R4,KEY                                                           
         USING STAKEYD,R4                                                       
*                                                                               
         OC    KEY(STAKSTIN-STAKEY),KEY                                         
         BNZ   LREC0080                                                         
*                                                                               
         MVI   STAKSYS,STAKSYSQ    BUILD SEARCH KEY USE INPUT                   
         MVI   STAKTYP,STAKTYPQ                                                 
*                                                                               
         CLI   SFLMEDAH+5,1        IS THE MEDIA 1 CHAR?                         
         BNE   VKEY0080            NO, INVALID MEDIA                            
*                                                                               
         CLI   SFLMEDA,C'R'        RADIO?                                       
         BE    LREC0040            YES                                          
*                                                                               
LREC0020 CLI   SFLMEDA,C'T'        IS THE MEDIA T?                              
         BNE   VKEY0080            NO  - INVALID MEDIA                          
LREC0040 EQU   *                                                                
*                                                                               
         MVC   STAKMEDA,SFLMEDA    BUILD SEARCH KEY                             
*                                                                               
         OC    SFLSTAT,SFLSTAT                                                  
         BZ    *+10                                                             
         MVC   STAKSTIN,SFLSTAT                                                 
*                                                                               
LREC0060 MVC   SAVEKEY,KEY                                                      
*                                                                               
LREC0080 GOTO1 HIGH                                                             
*                                                                               
LREC0100 EQU   *                                                                
         CLC   KEY(STAKSTIN-STAKEY),SAVEKEY                                     
*                                  COMPARE THRU MEDIA                           
         BNE   LREC0900            NOT THE SAME                                 
*                                                                               
         TM    FILTFLAG,SHOWRCNT   X'08' - SHOW MOST RECENT STATION             
         JZ    LREC0120            NO, WE WANT ALL STATIONS LISTED              
*                                  SAW THIS CALL LETTER ALREADY?                
         CLC   KEY+STAKSTIN-STAKEY(L'STAKSTIN),PREVSTA                          
         JE    LREC0800            YES                                          
         MVC   PREVSTA,KEY+STAKSTIN-STAKEY                                      
*                                                                               
LREC0120 XC    LISTAR,LISTAR                                                    
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
*                                                                               
         LA    R3,STAFSTEL                                                      
         USING STAREPD,R3                                                       
*                                                                               
         CLI   FLTBAND,C' '                                                     
         BNH   LREC0125                                                         
         CLC   FLTBAND,STAKSTIN+4                                               
         BNE   LREC0800                                                         
*                                                                               
LREC0125 MVI   ELCODE,STAREPCQ                                                  
         BAS   RE,FIRSTEL          INFO ELEMENT?                                
         JNE   *+2                                                              
*                                                                               
         OC    FLTCREP,FLTCREP     CHECK TO MATCH FILTERS                       
         BZ    *+14                                                             
         CLC   FLTCREP,STAREPCR                                                 
         BNE   LREC0800                                                         
*                                                                               
         OC    FLTPREP,FLTPREP                                                  
         BZ    *+14                                                             
         CLC   FLTPREP,STAREPPR                                                 
         BNE   LREC0800                                                         
*                                                                               
         MVC   LSTMEDIA,STAKMEDA                                                
         MVC   LSTSTATN,STAKSTIN                                                
         MVC   LSTSFDT,=C'**BASE**'                                             
*                                  SET DEFAULT FIELD VALUE                      
         CLC   =X'FFFFFF',STAKEFDA BASE DATE IN KEY (CONVERTED)?                
         BE    LREC0140            YES                                          
         OC    STAKEFDA,STAKEFDA   BASE DATE IN KEY (BIN ZERO)?                 
         BZ    LREC0140            YES                                          
         ZICM  RF,STAKEFDA,3       INVERT KEY DATE                              
         LNR   RF,RF                                                            
         STCM  RF,7,WORK+60                                                     
         GOTO1 DATCON,DMCB,(3,WORK+60),(5,LSTSFDT)                              
*                                  NO  - INSERT KEY EFFECTIVE DATE              
LREC0140 EQU   *                                                                
         MVC   LSTREP,STAREPCR                                                  
         MVC   LSTPREP,STAREPPR                                                 
         OC    STAREPED,STAREPED                                                
         BZ    LREC0150                                                         
         GOTO1 DATCON,DMCB,(6,STAREPED),(5,LSTEFDT)                             
*                                                                               
LREC0150 DS    0H                  ANY FILTER FOR 2ND REP?                      
         OC    FLTCRP2(L'FLTCRP2+L'FLTPRP2),FLTCRP2                             
         BZ    LREC0170            NO, NEITHER CURRENT NOR PREVIOUS             
*                                                                               
         LA    R3,STAFSTEL         LOOK FOR 2ND REP ELEM                        
         MVI   ELCODE,STARP2CQ                                                  
         BAS   RE,FIRSTEL                                                       
         BE    LREC0152            FOUND IT                                     
         OC    FLTCRP2,FLTCRP2     ELSE TEST IF 2ND REP FILTER                  
         BNZ   LREC0800            YES - SKIP RECORD                            
         B     LREC0160            ELSE DISPLAY 2ND REP                         
*                                                                               
LREC0152 CLC   FLTCRP2,STAREPCR    FOR COMPATIBILITY                            
         BE    LREC0160            IF MATCH - DISPLAY                           
*                                                                               
         USING STAREPD,R3                                                       
         CLC   =C'***',FLTCRP2     REP2=*** MEANS SHOW ANY WITH REP2            
         BE    LREC0160                                                         
*                                                                               
LREC0154 CLI   STAREPLN,STARP2LQ   ELEM HAVE SECONDARY REPS                     
         JNE   LREC0800            NO - THEN SKIP RECORD                        
*                                                                               
         LA    RE,STAREPS          MATCH FILTER TO REP IN LIST                  
LREC0156 CLI   0(RE),C' '                                                       
         JNH   LREC0800            NO MATCH IN LIST                             
         CLC   FLTCRP2,0(RE)                                                    
         JE    LREC0160                                                         
         LA    RE,3(RE)                                                         
         J     LREC0156                                                         
*                                                                               
LREC0160 MVC   LSTRP2(3),STAREPCR  COMPATIBILTY DISPLAY                         
*                                                                               
         CLI   STAREPLN,STARP2LQ   TEST ELEM HAS SECONDARY REPS                 
         JNE   LREC0170                                                         
*                                                                               
         LA    RE,STAREPS          ELSE DISPLAY THE LIST                        
         LA    RF,LSTRP2                                                        
         CLI   STAREPS,C' '                                                     
         JNH   LREC0170                                                         
         LA    R0,4                MAX 4 REPS TO DISPLAY                        
*                                  IF MORE THAN 4, SHOW ... ON 4TH              
LREC0164 CLI   0(RE),C' '                                                       
         JNH   LREC0166                                                         
         MVC   0(3,RF),0(RE)                                                    
         MVI   3(RF),C','                                                       
         LA    RF,4(RF)                                                         
         LA    RE,3(RE)                                                         
         JCT   R0,LREC0164                                                      
*                                                                               
LREC0166 BCTR  RF,0                                                             
         MVI   0(RF),C' '                                                       
*                                                                               
         CHI   R0,0                WE DISPLAYED 4 SECONDARY REPS?               
         JNE   LREC0170            NO                                           
         CLI   0(RE),C' '          YES, HAVE MORE THAN 4 2NDARY REPS?           
         JE    LREC0170            NO                                           
         SHI   RF,3                                                             
         MVC   0(3,RF),=C'...'                                                  
*                                                                               
LREC0170 LA    R3,STAFSTEL                                                      
         MVI   ELCODE,STASTACQ                                                  
         BAS   RE,FIRSTEL                                                       
         BNE   LREC0180                                                         
*                                                                               
         USING STASTAD,R3          REP ELEMENT DSECT                            
         MVC   LSTNSTA,STASTA                                                   
         GOTO1 DATCON,DMCB,(6,STASTADT),(5,LSTNSDT)                             
*                                                                               
LREC0180 OC    FLTNWSTA,FLTNWSTA   FILTER FOR NEW STATION?                      
         BZ    LREC0185                                                         
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FLTNWSTL                                                    
         BZ    LREC0181                                                         
         BCTR  RF,0                                                             
         CLC   FLTNWSTA(0),LSTNSTA                                              
         EX    RF,*-6                                                           
         BNE   LREC0800                                                         
LREC0181 OC    LSTNSTA,LSTNSTA                                                  
         BZ    LREC0800                                                         
*                                                                               
LREC0185 OC    FLTCRP2,FLTCRP2      2ND REP FILTER?                             
         BNZ   LREC0190             YES, SKIP THIS                              
*                                                                               
         LA    R3,STAFSTEL                                                      
         MVI   ELCODE,STAHOMCQ                                                  
         BAS   RE,FIRSTEL                                                       
         BNE   LREC0190                                                         
*                                                                               
         USING STAHOMD,R3          MARKET ELEMENT DSECT                         
         MVC   LSTMKT,STAHOMCT                                                  
         MVC   LSTRID,STAHOMIC                                                  
*                                                                               
LREC0190 DS    0H                                                               
         LA    R3,STAFSTEL                                                      
         MVI   ELCODE,STAH2CQ                                                   
         BAS   RE,FIRSTEL                                                       
         BNE   LRFLT10                                                          
*                                                                               
         CLI   LSTRID,C' '         IS USER ID ALREADY THERE ?                   
         BNH   LREC0195            NO - OKAY                                    
         MVC   LSTRID(6),=C'******'   HAS BOTH MARKET AND HIDDEN ID             
         B     LRFLT10                                                          
*                                                                               
LREC0195 DS    0H                                                               
         USING STAH2D,R3           SPCL DARE LC (HIDDEN ID) DSECT               
         MVC   LSTRID,STAH2IC                                                   
*                                                                               
LRFLT10  DS    0H                  CHECK FOR "FILTERING"                        
         CLI   FILTFLAG,0          ANYTHING TO FILTER ?                         
         JE    LREC0700            NO - DISPLAY RECORD                          
*                                                                               
         CLI   FILTFLAG,SHOWRCNT   JUST FILTERING "RECENT"?                     
         JE    LREC0700            YES, DISPLAY RECORD                          
*                                                                               
         CLI   LSTRID,C' '         ANYTHING IN ID ?                             
         BNH   LREC0750            NO - SKIP RECORD                             
*                                                                               
         CLI   FILTFLAG,1          ONLY SELECT "DIRECT TO STATION" ?            
         BH    LRFLT20             NO                                           
         CLC   LSTMKT(2),=C'**'    DIRECT TO STATION ?                          
         BNE   LREC0750            NO - SKIP RECORD                             
         B     LREC0700            DISPLAY IT                                   
*                                                                               
LRFLT20  DS    0H                                                               
         TM    FILTFLAG,SHOWSPCL   SHOW "SPCL DARE LC" ?                        
         BNO   LRFLT60             NO                                           
         LA    R3,STAFSTEL         LOOK FOR SPCL DARE LC ELEM                   
         MVI   ELCODE,STAH2CQ                                                   
         BAS   RE,FIRSTEL                                                       
         BNE   LRFLT60             NOT FOUND                                    
         USING STAH2D,R3           SPCL DARE LC (HIDDEN ID) DSECT               
         CLI   STAH2IC,C' '                                                     
         BNH   LRFLT60             NO ID TO DISPLAY                             
         B     LRFLT100            CHECK FOR "MARKET" FILTER                    
*                                                                               
LRFLT60  DS    0H                                                               
         TM    FILTFLAG,SHOWRECV   SHOW "HOME MARKET" ?                         
         BNO   LREC0750            NO - SKIP RECORD                             
         LA    R3,STAFSTEL         LOOK FOR HOME MARKET ELEMENT                 
         MVI   ELCODE,STAHOMCQ                                                  
         BAS   RE,FIRSTEL                                                       
         BNE   LREC0750            NOT FOUND - SKIP RECORD                      
         USING STAHOMD,R3          MARKET ELEMENT DSECT                         
         CLI   STAHOMIC,C' '                                                    
         BNH   LREC0750            NO ID TO DISPLAY - SKIP RECORD               
         B     LRFLT100            CHECK FOR "MARKET" FILTER                    
*                                                                               
LRFLT100 DS    0H                                                               
         CLC   LSTMKT(2),=C'**'    DIRECT TO STATION ?                          
         BNE   LREC0700            NO - DISPLAY RECORD                          
         TM    FILTFLAG,SHOWDIRC   SHOW "DIRECT TO STATION" ?                   
         BNO   LREC0750            NO - SKIP RECORD                             
*                                                                               
LREC0700 GOTO1 LISTMON             DISPLAY RECORD                               
*                                                                               
LREC0750 DS    0H                                                               
         XC    LISTAR,LISTAR                                                    
*                                                                               
LREC0800 GOTO1 SEQ                 GET NEXT RECORD AND CHECK FOR MATCH          
         LA    R4,KEY                                                           
         B     LREC0100                                                         
*                                                                               
LREC0900 B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY / IN SELECT                                                       
***********************************************************************         
DK       NTR1                                                                   
         L     R4,AIO                                                           
         USING STAKEYD,R4                                                       
         OI    SFMMEDAH+6,X'80'                                                 
         OI    SFMSTATH+6,X'80'                                                 
         MVC   SFMMEDA,STAKMEDA    PUT KEY TO SCREEN                            
*                                                                               
         MVC   SFMSTAT(4),STAKSTIN                                              
         CLI   SFMSTAT+3,C' '                                                   
         BNE   DKEY0020                                                         
         MVI   SFMSTAT+3,C'-'                                                   
         MVC   SFMSTAT+4(1),STAKSTIN+4                                          
         B     DKEY0040                                                         
*                                                                               
DKEY0020 MVI   SFMSTAT+4,C'-'                                                   
         MVC   SFMSTAT+5(1),STAKSTIN+4                                          
*                                                                               
DKEY0040 EQU   *                                                                
         MVC   SFMEDAT,=C'**BASE**'                                             
*                                  SET DEFAULT FIELD VALUE                      
         CLC   =X'FFFFFF',STAKEFDA BASE DATE IN KEY (CONVERTED)?                
         BE    DKEY0060            YES                                          
         OC    STAKEFDA,STAKEFDA   BASE DATE IN KEY (BIN ZERO)?                 
         BE    DKEY0060            YES                                          
         ZICM  RF,STAKEFDA,3       INVERT KEY DATE                              
         LNR   RF,RF                                                            
         STCM  RF,7,WORK+60                                                     
         GOTO1 DATCON,DMCB,(3,WORK+60),(5,SFMEDAT)                              
***>>>   GOTO1 DATCON,DMCB,(6,STAKEFDA),(5,SFMEDAT)                             
*                                  NO  - INSERT KEY EFFECTIVE DATE              
DKEY0060 EQU   *                                                                
         MVC   TEMPEXT,STAKSTIN+4  CALL LETTER EXTENSION                        
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
*===================================================================            
* PRINT REPORTS                                                                 
*===================================================================            
                                                                                
PR       NTR1                                                                   
         CLI   DOWNLOAD,C'2'                                                    
         JE    *+12                                                             
         CLI   DOWNLOAD,C'Y'                                                    
         BNE   PR00                                                             
         L     RE,=V(DLFLD)                                                     
         A     RE,RELO                                                          
         ST    RE,VDLFLD                                                        
*                                                                               
         XC    DLCB,DLCB                                                        
*                                                                               
PR00     MVI   FORCEHED,C'Y'                                                    
         MVI   FORCEHED,C'Y'                                                    
         LA    R1,HEDSPECS         HEADLINE SPECS                               
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK           HEAD HOOK                                    
         ST    R1,HEADHOOK                                                      
*                                                                               
         CLI   SFRPEND,C'Y'                                                     
         BNE   PR1                                                              
         BRAS  RE,SPR                                                           
         B     PREXIT0             EXIT ZERO                                    
*                                                                               
PR1      CLI   SFRSORTH+5,0        ANY SORT?                                    
         BE    PR2                 NO                                           
         CLI   WHEN,X'40'          SORT ON NOW?                                 
         BNE   PRSORT              NO                                           
         LA    R2,SFRSORTH                                                      
         J     INVLFLD                                                          
*                                                                               
PRSORT   OI    MYFLAG,CALLSORT     CALLED SORTER                                
         GOTO1 SORTER,DMCB,SCARDR,RECCARDR                                      
*                                                                               
PR2      LA    R4,KEY                                                           
         USING STAKEYD,R4                                                       
*                                                                               
         MVI   STAKSYS,STAKSYSQ    BUILD SEARCH KEY USE INPUT                   
         MVI   STAKTYP,STAKTYPQ                                                 
*                                                                               
         CLI   SFRMEDAH+5,1        IS THE MEDIA 1 CHAR?                         
         BNE   PREXIT1             SET EXIT CC NOT ZERO                         
         XC    MEDNAME,MEDNAME                                                  
         CLI   SFRMEDA,C'T'        IS THE MEDIA T?                              
         BNE   *+14                                                             
         MVC   MEDNAME,=C'TELEVISION'                                           
         B     PR5                                                              
         CLI   SFRMEDA,C'R'           OR  MEDIA R?                              
         BNE   PREXIT1             SET EXIT CC NOT ZERO                         
         MVC   MEDNAME(5),=C'RADIO'                                             
PR5      EQU   *                                                                
         MVC   STAKMEDA,SFRMEDA    BUILD SEARCH KEY                             
         OC    SFRSTAT,SFRSTAT                                                  
         BZ    *+10                                                             
         MVC   STAKSTIN,SFRSTAT                                                 
PR7      MVC   SAVEKEY,KEY                                                      
*                                                                               
PR10     GOTO1 HIGH                                                             
*                                                                               
PR20     EQU   *                                                                
PR21     CLC   KEY(STAKSTIN-STAKEY),SAVEKEY                                     
         BE    PR22                                                             
PR21A    CLI   SFRSORTH+5,0        ANY SORT?                                    
         BNE   PR31                YES                                          
         B     PR900                                                            
*                                                                               
PR22     XC    P,P                                                              
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
*                                                                               
         OC    LUPDATE,LUPDATE     DATE FILTER PRESENT?                         
         BZ    PR30                NO                                           
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   PR800               NONE PRESENT SKIP IT                         
*                                                                               
         USING ACTVD,R3                                                         
         CLC   ACTVCHDT,LUPDATE    UPDATED BEFORE FILTER START DATE?            
         BL    PR800               YES - SKIP IT                                
         OC    LUPDATE+3(3),LUPDATE+3    FILTER ON END DATE                     
         BZ    PR30                                                             
         CLC   ACTVCHDT,LUPDATE+3  UPDATED AFTER FILTER END DATE?               
         BH    PR800               YES - SKIP IT                                
         DROP  R3                                                               
*                                                                               
PR30     L     R4,AIO                                                           
         LA    R3,STAFSTEL         FIRST ELEMENT                                
         USING STAREPD,R3          REP ELEMENT                                  
*                                                                               
         CLI   FLTBAND,C' '                                                     
         BNH   PR30A                                                            
         CLC   FLTBAND,STAKSTIN+4                                               
         BNE   PR800                                                            
*                                                                               
PR30A    MVI   ELCODE,STAREPCQ     REP ELEMENT                                  
         BAS   RE,FIRSTEL          MUST BE AN INFO ELEMENT                      
         JNE   *+2                                                              
*                                                                               
         OC    FLTCREP,FLTCREP     CHECK TO MATCH FILTERS                       
         BZ    *+14                                                             
         CLC   FLTCREP,STAREPCR                                                 
         BNE   PR800                                                            
*                                                                               
         OC    FLTPREP,FLTPREP                                                  
         BZ    *+14                                                             
         CLC   FLTPREP,STAREPPR                                                 
         BNE   PR800                                                            
*                                                                               
         LA    R1,STAREPCR         POINT TO CURRENT REP                         
         BRAS  RE,REPSTAT                                                       
*                                                                               
         CLI   SFRSORTH+5,0        ANY SORT?                                    
         BE    PR32                NO                                           
*                                                                               
         L     R4,AIO                                                           
         MVC   RECLEN,STARECLN     RECORD LENGTH                                
         USING SORTKEYD,R4                                                      
         MVC   SORTLEN,RECLEN      RECORD LENGTH                                
*                                                                               
         MVC   SORTDARE,STATUS     DARE OR NOT DARE STATUS                      
         MVC   SORTCREP,STAREPCR   CURRENT REP                                  
         DROP  R4                                                               
         USING STAKEYD,R4                                                       
         GOTO1 SORTER,DMCB,=C'PUT',AIO                                          
         B     PR800                                                            
                                                                                
PR31     GOTO1 SORTER,DMCB,=C'GET'                                              
         OC    DMCB+4(4),DMCB+4    ANY MORE SORTED RECORDS?                     
         BZ    PR900               NO                                           
         L     R3,DMCB+4                                                        
         L     R4,DMCB+4                                                        
         USING STAKEYD,R4                                                       
         LA    R3,STAFSTEL         FIRST ELEMENT                                
         USING STAREPD,R3          REP ELEMENT                                  
*                                                                               
         LA    R3,STAFSTEL         FIRST ELEMENT                                
         MVI   ELCODE,STAREPCQ     REP ELEMENT                                  
         BAS   RE,FIRSTEL          MUST BE AN INFO ELEMENT                      
         JNE   *+2                                                              
*                                                                               
         CLI   SFRSORT,C'S'        SORT ON STATUS?                              
         BNE   PR31A                                                            
         CLC   PREVSTAT,STATUS     PREVIOUS STATUS = CURRENT STATUS?            
         BE    PR32                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   PREVSTAT,STATUS                                                  
         B     PR32                                                             
*                                                                               
PR31A    CLC   PREVREP,STAREPCR    PREVIOUS REP = CURRENT REP?                  
         BE    PR32                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   PREVREP,STAREPCR    PREVIOUS REP                                 
PR32     EQU   *                                                                
*                                                                               
         MVC   RPTSTATN,STAKSTIN                                                
         CLC   STAKEFDA,SPACES     ANY DATE IN KEY                              
         BNH   PR330020            NO  -                                        
         ZICM  RF,STAKEFDA,3       YES - INVERT IT FOR DISPLAY                  
         LNR   RF,RF                                                            
         STCM  RF,7,WORK                                                        
         MVI   BYTE,5              MMMDDYY                                      
         CLI   DOWNLOAD,C'2'                                                    
         BNE   *+8                                                              
         MVI   BYTE,20             YYYYMMDD                                     
         GOTO1 DATCON,DMCB,(3,WORK),(BYTE,RPTSTEFD)                             
         B     PR34                                                             
*                                                                               
PR330020 EQU   *                                                                
         MVC   RPTSTEFD,=C'*NO DATE'                                            
PR34     MVC   RPTREP,STAREPCR                                                  
         MVC   RPTPREP,STAREPPR                                                 
*                                                                               
         MVC   RPTSTAT,=C'* DARE *'                                             
         TM    STATUS,STT1NDAR     TEST NOT DARE REP                            
         BZ    *+10                                                             
         MVC   RPTSTAT,=C'NOT DARE'                                             
*                                                                               
         TM    STATUS2,STT2EOC     TEST END OF CONTRACT                         
         BZ    *+10                                                             
         MVC   RPTSTAT,=C'* EOC  *'                                             
*                                                                               
         TM    STATUS2,STT2NVR     TEST BAD REP                                 
         BZ    *+10                                                             
         MVC   RPTSTAT,=C'BAD REP '                                             
*                                                                               
         OC    STAREPED,STAREPED                                                
         BZ    PR35                                                             
         MVI   BYTE,5                                                           
         CLI   DOWNLOAD,C'2'                                                    
         BNE   *+8                                                              
         MVI   BYTE,20                                                          
         GOTO1 DATCON,DMCB,(6,STAREPED),(BYTE,RPTEFDT)                          
*                                                                               
PR35     LA    R3,STAFSTEL         FIRST ELEMENT                                
         MVI   ELCODE,STASTACQ                                                  
         BAS   RE,FIRSTEL                                                       
         BNE   PR40                                                             
*                                                                               
         USING STASTAD,R3          REP ELEMENT DSECT                            
         MVC   RPTNSTA,STASTA                                                   
         MVI   BYTE,5              MMMDDYY                                      
         CLI   DOWNLOAD,C'2'                                                    
         BNE   *+8                                                              
         MVI   BYTE,20             YYYYMMDD                                     
         GOTO1 DATCON,DMCB,(6,STASTADT),(BYTE,RPTNSDT)                          
*                                                                               
PR40     OC    FLTNWSTA,FLTNWSTA   FILTER FOR NEW STATION?                      
         BZ    PR41                                                             
         SR    RF,RF                                                            
         ICM   RF,1,FLTNWSTL                                                    
         BZ    PR40A                                                            
         BCTR  RF,0                                                             
         CLC   FLTNWSTA(0),RPTNSTA                                              
         EX    RF,*-6                                                           
         BNE   PR800                                                            
PR40A    OC    RPTNSTA,RPTNSTA                                                  
         BZ    PR800                                                            
         B     PR41                                                             
*                                                                               
PR41     LA    R3,STAFSTEL                                                      
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         BAS   RE,FIRSTEL                                                       
         BNE   PR45                                                             
*                                                                               
         USING ACTVD,R3                                                         
         LA    R2,ACTVADDT                                                      
         OC    ACTVCHDT,ACTVCHDT   IF WE HAVE A CHANGE DATE, THEN SHOW          
         BZ    *+8                   IT, OTHERWISE SHOW CREATION DATE           
         LA    R2,ACTVCHDT                                                      
         GOTO1 DATCON,DMCB,(3,0(R2)),(5,RPTCHGDT)                               
         DROP  R3                                                               
*                                                                               
PR45     LA    R3,STAFSTEL         FIRST ELEMENT                                
         MVI   ELCODE,STAHOMCQ                                                  
         BAS   RE,FIRSTEL                                                       
         BNE   PR50                                                             
*                                                                               
         USING STAHOMD,R3          HOME MARKET ELEMENT DSECT                    
         MVC   RPTMKT,STAHOMCT                                                  
         MVC   RPTRID,STAHOMIC                                                  
*                                                                               
PR50     DS    0H                                                               
         LA    R3,STAFSTEL                                                      
         MVI   ELCODE,STAH2CQ                                                   
         BAS   RE,FIRSTEL                                                       
         BNE   PR60                                                             
*                                                                               
         CLI   RPTRID,C' '         IS USER ID ALREADY THERE ?                   
         BNH   PR55                NO - OKAY                                    
         MVC   RPTRID(6),=C'******'   HAS BOTH MARKET AND HIDDEN ID             
         B     PR60                                                             
*                                                                               
PR55     DS    0H                                                               
         USING STAH2D,R3           SPCL DARE LC (HIDDEN ID) DSECT               
         MVC   RPTRID,STAH2IC                                                   
*                                                                               
PR60     LA    R3,STAFSTEL                                                      
         MVI   ELCODE,STABLELQ     X'40' - WHO TO BILL TO  ELEM                 
         BAS   RE,FIRSTEL                                                       
         BNE   PR65                                                             
*                                                                               
         USING STABLELD,R3                                                      
         MVC   RPTBILTO,STABLWHO   SHOW IT IF WE HAVE IT                        
*                                                                               
PR65     LA    R3,STAFSTEL                                                      
         MVI   ELCODE,STARP2CQ     X'11' - 2ND REP ELEMENT                      
         BAS   RE,FIRSTEL                                                       
         BNE   PR95                                                             
*                                                                               
         USING STAREPD,R3                                                       
*                                                                               
         MVC   RPTRP2(3),STAREPCR     MOVE FOR COMPATIBILILTY                   
         CLI   STAREPLN,STARP2LQ      ELEM HAVE SECONDARIES                     
         JL    PR95                                                             
*                                                                               
         LA    R0,6                MAX that can print                           
         LA    RE,STAREPS                                                       
         LA    RF,RPTRP2                                                        
*                                                                               
PR90     CLI   0(RE),C' '                                                       
         JNH   PR92                                                             
         MVC   0(3,RF),0(RE)                                                    
         MVI   3(RF),C','                                                       
*                                                                               
         CLI   DOWNLOAD,C'2'                                                    
         JE    *+12                                                             
         CLI   DOWNLOAD,C'Y'                                                    
         JNE   *+8                                                              
         MVI   3(RF),C'|'          USE VERTICAL BAR                             
*                                                                               
         LA    RE,3(RE)                                                         
         LA    RF,4(RF)                                                         
         JCT   R0,PR90                                                          
*                                                                               
PR92     BCTR  RF,0                                                             
         MVI   0(RF),C' '                                                       
*                                                                               
PR95     DS    0H                                                               
*                                                                               
PRFLT10  DS    0H                  CHECK FOR "FILTERING"                        
         CLI   FILTFLAG,0          ANYTHING TO FILTER ?                         
         BNH   PR600               NO - DISPLAY RECORD                          
*                                                                               
         CLI   RPTRID,C' '         ANYTHING IN ID ?                             
         BNH   PR700               NO - SKIP RECORD                             
*                                                                               
         CLI   FILTFLAG,1          ONLY SELECT "DIRECT TO STATION" ?            
         BH    PRFLT20             NO                                           
         CLC   RPTMKT(2),=C'**'    DIRECT TO STATION ?                          
         BNE   PR700               NO - SKIP RECORD                             
         B     PR600               DISPLAY IT                                   
*                                                                               
PRFLT20  DS    0H                                                               
         TM    FILTFLAG,SHOWSPCL   SHOW "SPCL DARE LC" ?                        
         BNO   PRFLT60             NO                                           
         LA    R3,STAFSTEL         LOOK FOR SPCL DARE LC ELEM                   
         MVI   ELCODE,STAH2CQ                                                   
         BAS   RE,FIRSTEL                                                       
         BNE   PRFLT60             NOT FOUND                                    
         USING STAH2D,R3           SPCL DARE LC (HIDDEN ID) DSECT               
         CLI   STAH2IC,C' '                                                     
         BNH   PRFLT60             NO ID TO DISPLAY                             
         B     PRFLT100            CHECK FOR DIRECT TO STATION FILTER           
*                                                                               
PRFLT60  DS    0H                                                               
         TM    FILTFLAG,SHOWRECV   SHOW "HOME MARKET" ?                         
         BNO   PR700               NO - SKIP RECORD                             
         LA    R3,STAFSTEL         LOOK FOR HOME MARKET ELEMENT                 
         MVI   ELCODE,STAHOMCQ                                                  
         BAS   RE,FIRSTEL                                                       
         BNE   PR700               NOT FOUND - SKIP RECORD                      
         USING STAHOMD,R3          MARKET ELEMENT DSECT                         
         CLI   STAHOMIC,C' '                                                    
         BNH   PR700               NO ID TO DISPLAY - SKIP RECORD               
******   B     PRFLT100            CHECK FOR DIRECT TO STATION FILTER           
*                                                                               
PRFLT100 DS    0H                                                               
         CLC   RPTMKT(2),=C'**'    DIRECT TO STATION ?                          
         BNE   PR600               NO - DISPLAY RECORD                          
         TM    FILTFLAG,SHOWDIRC   SHOW "DIRECT TO STATION" ?                   
         BNO   PR700               NO - SKIP RECORD                             
*                                                                               
PR600    DS    0H                                                               
         CLI   DOWNLOAD,C'2'                                                    
         BE    *+12                                                             
         CLI   DOWNLOAD,C'Y'                                                    
         BNE   PR650                                                            
         MVC   MYIO(L'P),P         TRANSFORM TO DOWNLOADABLE FORMAT             
         LAY   R4,RPRECTAB         REPORT LAYOUT                                
         LA    R3,MYIO                                                          
         BRAS  RE,OUTPUT                                                        
         XC    P,P                                                              
         B     PR800                                                            
*                                                                               
PR650    MVI   LINE,1              NO MORE HEADLINES                            
         GOTO1 SPOOL,DMCB,(R8)     DISPLAY RECORD                               
*                                                                               
PR700    DS    0H                                                               
         XC    P,P                                                              
         CLI   SFRSORTH+5,0        ANY SORT?                                    
         BNE   PR31                YES                                          
*                                                                               
PR800    DS    0H                                                               
         L     R4,AIO                                                           
         CLC   KEY(L'STAKEY),0(R4) READ SEQUENCE CHANGED?                       
         BE    PR810               NO                                           
         MVC   KEY(L'STAKEY),0(R4) YES, RESTORE READ SEQUENCE                   
         GOTO1 HIGH                                                             
*                                                                               
PR810    GOTO1 SEQ                 GET NEXT RECORD AND CHECK FOR MATCH          
         LA    R4,KEY                                                           
         B     PR20                                                             
*                                                                               
PR900    CLI   DOWNLOAD,C'2'                                                    
         BE    *+12                                                             
         CLI   DOWNLOAD,C'Y'                                                    
         BNE   PR950                                                            
         SR    R4,R4               CLEAR TABLE ADDRESS                          
         BRAS  RE,OUTPUT                                                        
         B     PREXIT0                                                          
*                                                                               
PR950    TM    MYFLAG,CALLSORT     CALLED SORTER?                               
         BZ    PREXIT0                                                          
         GOTO1 SORTER,DMCB,=C'END'                                              
*                                                                               
PREXIT0  EQU   *                                                                
         SR    R0,R0               SET CC ZERO, AND EXIT                        
         B     PREXIT99                                                         
PREXIT1  EQU   *                                                                
         LTR   RB,RB               SET CC NOT ZERO, AND EXIT                    
PREXIT99 EQU   *                                                                
         J     EXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* HEAD HOOK                                                                     
**********************************************************************          
HDHOOK   NTR1                                                                   
*                                                                               
         MVC   H1+55(22),=C'DSTATION RECORD REPORT'                             
*                                                                               
         CLI   SFRFILTH+5,0        ANY FILTERS?                                 
         JE    HD10                NO                                           
         MVC   H5(8),=C'FILTERS:'                                               
         MVC   H5+10(L'SFRFILT),SFRFILT FILTERS                                 
*                                                                               
HD10     CLI   SFRSORTH+5,0        ANY SORTS?                                   
         JE    HDHOOKX             NO                                           
         MVC   H6(8),=C'SORT BY:'                                               
         MVC   H6+10(L'SAVESORT),SAVESORT                                       
*                                                                               
HDHOOKX  J     EXIT                                                             
*                                                                               
HEDSPECS SSPEC H1,1,AGYNAME                                                     
         SSPEC H2,1,AGYADD                                                      
         SSPEC H1,95,REPORT                                                     
         SSPEC H1,114,REQUESTOR                                                 
         SSPEC H2,95,RUN                                                        
         SSPEC H2,121,PAGE                                                      
         SSPEC H4,1,C'MEDIA:'                                                   
         SSPEC H8,2,C'STA'                                                      
         SSPEC H8,8,C'EFF DATE'                                                 
         SSPEC H8,17,C'REP'                                                     
         SSPEC H8,21,C'STATUS'                                                  
         SSPEC H8,30,C'EFF DATE'                                                
         SSPEC H8,39,C'PRV'                                                     
         SSPEC H8,43,C'NWSTA'                                                   
         SSPEC H8,49,C'EFF DATE'                                                
         SSPEC H8,58,C'MKT'                                                     
         SSPEC H8,62,C'USER ID'                                                 
         SSPEC H8,73,C'CHNGD ON'                                                
         SSPEC H8,82,C'BILL TO'                                                 
         SSPEC H8,108,C'RP2'                                                    
         DC    X'00'                                                            
*                                                                               
ERR      OI    CONHEADH+6,X'80'                                                 
         GOTOR ERREX2                                                           
*                                                                               
SFMERROR GOTOR SFMERR                                                           
*                                                                               
RELO     DS    A                                                                
SAVESORT DS    CL6                 SAVE SORT FIELD FROM SCREEN                  
PREVREP  DS    CL3                 PREVIOUS REP                                 
PREVSTAT DS    XL1                 PREVIOUS STATUS                              
SCARDR   DC    CL80'SORT FIELDS=(5,4,A),FORMAT=BI,WORK=1'                       
RECCARDR DC    CL80'RECORD TYPE=V,LENGTH=2000'                                  
*                                                                               
         GETEL  R3,DATADISP,ELCODE                                              
*&&DO                                                                           
       ++INCLUDE DDDARETAB                                                      
*&&                                                                             
         LTORG                                                                  
         EJECT                                                                  
VRECBL2R DS    0C                  RADIO VENDORS TO BILL TO                     
         DC    AL1(07),C'PANDORA'    NAMES CANT BE LONGER THAN CL25             
         DC    AL1(05),C'APPLE'                                                 
         DC    AL1(06),C'TRITON'                                                
         DC    AL1(07),C'SPOTIFY'                                               
         DC    AL1(12),C'IHEART MEDIA'                                          
         DC    AL1(03),C'DAX'                                                   
         DC    X'00'                                                            
*                                                                               
VRECBL2T DS    0C                  TV VENDORS TO BILL TO                        
         DC    AL1(04),C'HULU'     NAMES SHOULD NOT BE LONGER THAN CL25         
         DC    AL1(03),C'AOL'                                                   
         DC    AL1(11),C'COLLAB INC.'                                           
         DC    AL1(07),C'COXREPS'                                               
         DC    AL1(09),C'VIDEOLOGY'                                             
         DC    AL1(07),C'AERSERV'                                               
         DC    AL1(03),C'NCM'                                                   
         DC    AL1(07),C'PREMION'                                               
         DC    AL1(04),C'YUME'                                                  
         DC    AL1(04),C'VEVO'                                                  
         DC    AL1(07),C'JAMLOOP'                                               
         DC    AL1(06),C'ADMORE'                                                
         DC    AL1(15),C'CBS INTERACTIVE'                                       
         DC    AL1(18),C'SCREENVISION'                                          
         DC    AL1(05),C'SPOTX'                                                 
         DC    AL1(06),C'SPOTON'                                                
         DC    AL1(07),C'SYNCBAK'                                               
         DC    AL1(12),C'SINCLAIR OTT'                                          
         DC    AL1(06),C'TREMOR'                                                
         DC    AL1(06),C'NEWSON'                                                
         DC    AL1(07),C'NEXSTAR'                                               
         DC    AL1(12),C'KATZ NEXSTAR'                                          
         DC    AL1(12),C'KATZ SCRIPPS'                                          
         DC    AL1(13),C'KATZ SINCLAIR'                                         
         DC    AL1(04),C'KATZ'                                                  
         DC    X'00'                                                            
                                                                                
***********************************************************************         
*   DISPLAY RECORD                                                              
***********************************************************************         
DR       NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,SETUP                                                         
         L     R4,AIO              CHECK KEY SAME AS RECORD                     
         USING STAKEYD,R4                                                       
         CLC   KEY(STAKLENQ),STAKEY                                             
         JNE   *+2                                                              
*                                                                               
         MVC   SFMEDAT,=C'**BASE**'                                             
*                                  SET DEFAULT FIELD VALUE                      
         CLC   =X'FFFFFF',STAKEFDA BASE DATE IN KEY (CONVERTED)?                
         BE    DREC0020            YES                                          
         OC    STAKEFDA,STAKEFDA   BASE DATE IN KEY (BIN ZERO)?                 
         BZ    DREC0020            YES                                          
         ZICM  RF,STAKEFDA,3       INVERT KEY DATE                              
         LNR   RF,RF                                                            
         STCM  RF,7,WORK+60                                                     
         GOTO1 DATCON,DMCB,(3,WORK+60),(5,SFMEDAT)                              
*                                  NO  - INSERT KEY EFFECTIVE DATE              
         OI    SFMEDATH+4,X'20'                                                 
         OI    SFMEDATH+6,X'80'                                                 
DREC0020 EQU   *                                                                
         L     R3,AIO                                                           
         USING STAREPD,R3                                                       
         MVI   ELCODE,STAREPCQ                                                  
         BAS   RE,GETEL                                                         
         JNE   *+2                 MUST BE A REP ELEMENT                        
*                                                                               
         MVC   SFMCREP,STAREPCR    DISPLAY REP ELEMENT                          
         OI    SFMCREPH+6,X'80'                                                 
*                                                                               
         MVC   SFMPREP,STAREPPR    PUT RECORD TO SCREEN                         
         OI    SFMPREPH+6,X'80'                                                 
         OC    STAREPED,STAREPED                                                
         BZ    DREC0030                                                         
         GOTO1 DATCON,DMCB,(6,STAREPED),(5,SFMDATE)                             
         MVI   SFMDATEH+5,8                                                     
         OI    SFMDATEH+6,X'80'                                                 
*                                                                               
DREC0030 DS    0H                  REMOVED DARE/NOT DARE STATUS -- WH           
         MVI   SFMEINV,C'N'                                                     
         TM    STAREPFG,STAREPFG_EI  DISPLAY IF ELECTRONIC INVOICE REP          
         BZ    *+8                                                              
         MVI   SFMEINV,C'Y'                                                     
*                                                                               
         XC    SFMCRP2,SFMCRP2                                                  
         OI    SFMCRP2H+6,X'80'                                                 
*                                                                               
         L     R3,AIO                                                           
         USING STAREPD,R3                                                       
         MVI   ELCODE,STARP2CQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DREC0040            NO SECONDARY REP ELEM                        
*                                                                               
         MVC   SFMCRP2(3),STAREPCR    MOVE FOR COMPATIBILILTY                   
         CLI   STAREPLN,STARP2LQ      ELEM HAVE SECONDARIES                     
         JL    DREC0040                                                         
*                                                                               
         LA    R0,8                MAX                                          
         LA    RE,STAREPS                                                       
         LA    RF,SFMCRP2                                                       
DREC0032 CLI   0(RE),C' '                                                       
         JNH   DREC0034                                                         
         MVC   0(3,RF),0(RE)                                                    
         MVI   3(RF),C','                                                       
         LA    RE,3(RE)                                                         
         LA    RF,4(RF)                                                         
         JCT   R0,DREC0032                                                      
*                                                                               
DREC0034 BCTR  RF,0                                                             
         MVI   0(RF),C' '                                                       
*                                                                               
DREC0040 DS    0H                                                               
*==========================================================                     
* DISPLAY  BILL TO  IF ELEMENT PRESENT                                          
*==========================================================                     
         L     R3,AIO                                                           
         USING STAREPD,R3                                                       
         MVI   ELCODE,STABLELQ       X'40' - BILL TO ELEMENT CODE               
         BAS   RE,GETEL                                                         
         BNE   DREC0050                                                         
         USING STABLELD,R3                                                      
*                                                                               
         MVC   SFMBLTO,STABLWHO                                                 
*==========================================================                     
* DISPLAY NEW CALL LETTER DATA IF ELEMENT PRESENT                               
*==========================================================                     
DREC0050 L     R3,AIO                                                           
         USING STAREPD,R3                                                       
         MVI   ELCODE,STASTACQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DREC0060                                                         
         USING STASTAD,R3                                                       
*                                                                               
         MVC   SFMNSTA,STASTA                                                   
         OI    SFMNSTAH+6,X'80'                                                 
*                                                                               
         OC    STASTADT,STASTADT                                                
         BZ    DREC0080                                                         
         GOTO1 DATCON,DMCB,(6,STASTADT),(5,SFMNSDT)                             
         OI    SFMNSDTH+6,X'80'                                                 
         EJECT                                                                  
*==========================================================                     
* DISPLAY HOME MARKET CITY AND RECEIVING ID                                     
*==========================================================                     
DREC0060 L     R3,AIO                                                           
         USING STAHOMD,R3                                                       
         MVI   ELCODE,STAHOMCQ                                                  
         BAS   RE,GETEL                                                         
         BNE   DREC0080                                                         
*                                                                               
         MVC   SFMCITY,STAHOMCT                                                 
         OI    SFMCITYH+6,X'80'                                                 
         MVC   SFMRID,STAHOMIC                                                  
         OI    SFMRIDH+6,X'80'                                                  
*                                                                               
DREC0080 XC    SFMDLID,SFMDLID                                                  
         L     R3,AIO                                                           
         USING STAH2D,R3                                                        
         MVI   ELCODE,STAH2CQ                                                   
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   SFMDLID,STAH2IC                                                  
         OI    SFMDLIDH+6,X'80'                                                 
*                                                                               
DREC0090 DS    0H                                                               
         L     R3,AIO              POINT TO DSTA RECORD                         
         MVI   ELCODE,X'D1'        DATE/TIME ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   DREC0100                                                         
*                                                                               
         USING DATTIMD,R3                                                       
         GOTO1 DATCON,DMCB,(8,DATTMGDT),(11,LUPDATE)                            
*                                                                               
         GOTO1 HEXOUT,DMCB,DATTMGTM,WORK,L'DATTMGTM                             
         MVC   LUPDATE+9(2),WORK                                                
         MVI   LUPDATE+11,C':'                                                  
         MVC   LUPDATE+12(2),WORK+2                                             
         B     DREC0120                                                         
         DROP  R3                                                               
*                                                                               
DREC0100 L     R3,AIO              POINT TO DSTA RECORD                         
         MVI   ELCODE,X'F1'        ACTIVITY ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   DREC0140            NONE PRESENT                                 
*                                                                               
         USING ACTVD,R3                                                         
         GOTO1 DATCON,DMCB,(3,ACTVCHDT),(11,LUPDATE)                            
DREC0120 XC    DMCB,DMCB                                                        
         GOTO1 GETTXT,DMCB,28,0,(C'I',0),(14,LUPDATE)                           
         OI    GENSTAT2,USMYOK                                                  
         DROP  R3                                                               
*                                                                               
DREC0140 J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
*                                                                               
* INTERFACE TO DLFLD FOR DOWNLOADS                                              
* THIS CODE IS OFFLINE ONLY -- SOON AND OV !!!!                                 
*                                                                               
* R4 POINTS TO DATA TABLE  (ZERO TO CLOSE)                                      
* R3 POINTS TO DATA AREA - MODIFY ADDRESSES BY THIS VALUE                       
*                                                                               
* **WARNING**                                                                   
* DLFLD IS NOT BASELESS AND RELIES ON RB.  DO NOT CHANGE RB                     
*=================================================================              
OUTPUT   NTR1  BASE=*,LABEL=*                                                   
         MVI   LINE,0                                                           
         MVI   FORCEHED,C'N'                                                    
         MVC   P1,SPACES                                                        
         MVC   P2,SPACES                                                        
*                                                                               
D        USING DLCBD,DLCB                                                       
*                                                                               
         LTR   R4,R4               TEST CLOSE                                   
         BNZ   OUTPUT0             NO                                           
         CLI   DOWNINIT,C'Y'       TEST INITIALIZED                             
         BNE   OUTPUTX                                                          
         MVI   D.DLCBACT,C'R'      SET E-O-R                                    
         GOTO1 VDLFLD,DLCB                                                      
         MVI   DOWNINIT,C'N'       RESET INITIALIZED FLAG                       
         B     OUTPUTX                                                          
*                                                                               
OUTPUT0  CLI   DOWNINIT,C'Y'                                                    
         BE    OUTPUT1                                                          
         MVI   DOWNINIT,C'Y'                                                    
*                                                                               
         XC    DLCB,DLCB                                                        
         MVI   D.DLCBACT,C'I'      START AND INITIALIZE REPORT                  
         LARL  RE,DOWNPRT                                                       
         ST    RE,D.DLCBAPR        PRINT ROUTINE ADDRESS                        
         LA    R0,P1                                                            
         ST    R0,D.DLCBAPL        PRINT LINE ADDRESS                           
         OI    D.DLCBFLG1,DLCBFXTN                                              
         MVC   D.DLCXTND(7),MAXLINE                                             
         CLI   DOWNLOAD,C'2'                                                    
         BNE   *+10                                                             
         MVC   D.DLCXTND(7),MAXLIN2                                             
                                                                                
         GOTO1 VDLFLD,DLCB                                                      
                                                                                
         XC    DMCB(8),DMCB        GET EDITOR ADDRES                            
         MVC   DMCB+4(3),=X'D9000A71'   EDITOR                                  
         GOTOR CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         MVC   D.DLCBAED,4(R1)     DLFLD REQUIRES A(EDITOR)                     
*                                                                               
OUTPUT1  MVC   D.DLCXTND(7),MAXLINE                                             
         CLI   DOWNLOAD,C'2'                                                    
         BNE   *+10                                                             
         MVC   D.DLCXTND(7),MAXLIN2                                             
OUTPUT2  CLI   DOWNFRST,0                                                       
         BNE   OUTPUT10                                                         
*                                                                               
         MVI   DOWNFRST,C'N'                                                    
         MVI   FORCEHED,C'Y'       SKIP TO NEW PAGE AND PRINT NOTHING!          
         LA    R0,14                                                            
         LA    R1,HEAD1                                                         
         MVC   0(132,R1),SPACES                                                 
         LA    R1,132(R1)                                                       
         BCT   R0,*-10                                                          
*                                                                               
         USING DWNTABD,R4                                                       
OUTPUT10 L     RE,DVALDISP         GET DATA ADDR                                
         AR    RE,R3               POINT TO DATA                                
         LLC   RF,DVALLEN          GET DATA LEN                                 
         BCTR  RF,0                                                             
         MVC   D.DLCBFLD(0),0(RE)                                               
         EX    RF,*-6                                                           
*                                                                               
         TM    DVALIND,DVALISO     convert to ISO for download2                 
         BZ    OUTPUT20                                                         
         CLI   DOWNLOAD,C'2'                                                    
         BNE   OUTPUT20                                                         
         CLC   D.DLCBFLD(8),SPACES                                              
         BNH   OUTPUT20            convert YYYYMMDD to YYYY-MM-DD               
         MVC   WORK(4),D.DLCBFLD+4         MMDD                                 
         MVI   D.DLCBFLD+4,C'-'                                                 
         MVC   D.DLCBFLD+5(2),WORK         MM                                   
         MVI   D.DLCBFLD+7,C'-'                                                 
         MVC   D.DLCBFLD+8(2),WORK+2       DD                                   
         AHI   RF,2                                                             
*                                                                               
OUTPUT20 CLI   DVALTYPE,C'T'       TEST TEXT                                    
         JNE   *+2                 ONLY SUPPORT TEXT FOR NOW                    
         OC    D.DLCBFLD(0),SPACES                                              
         EX    RF,*-6                                                           
*                                                                               
OUTPUT30 MVC   D.DLCBTYP(1),DVALTYPE SET DATA TYPE                              
         MVI   D.DLCBACT,DLCBPUT                                                
*                                                                               
         GOTO1 VDLFLD,DLCB                                                      
*                                                                               
         AHI   R4,DWNTABLQ           Bump to next entry                         
         CLI   0(R4),X'FF'                                                      
         BE    OUTPUT32                                                         
         CLI   DWNTABLQ(R4),X'FF'    NEXT ENTRY IS EOT?                         
         BNE   OUTPUT2                                                          
         MVI   D.DLCXDELC,C' '       TERMINATOR (NO DELIMITER AFTER)            
         B     OUTPUT2                                                          
*                                                                               
OUTPUT32 MVI   D.DLCBACT,DLCBEOL                                                
         GOTO1 VDLFLD,DLCB                                                      
*                                                                               
         MVI   D.DLCBACT,DLCBEOL                                                
         GOTO1 VDLFLD,DLCB                                                      
*                                                                               
OUTPUTX  XIT1                                                                   
*                                                                               
MAXLINE  DC    H'132'              MAX LINE WIDTH                               
DELIM    DC    C','                FIELD DELIMITER CHR                          
EOTCHR   DC    C'"'                END OF TEXT FIELD DELIMITER                  
EOTALT   DC    C''''               END OF TEXT CHR ALTERNATE                    
EOLCHR   DC    C' '                END OF LINE CHAR - SEMICOLON                 
EORCHR   DC    C':'                END OF REPORT CONTROL CHR                    
MAXLINLN EQU   *-MAXLINE                                                        
*                                                                               
MAXLIN2  DC    H'132'              MAX LINE WIDTH                               
         DC    C' '                FIELD DELIMITER CHR                          
         DC    C'"'                END OF TEXT FIELD DELIMITER                  
         DC    C''''               END OF TEXT CHR ALTERNATE                    
         DC    C';'                END OF LINE CHAR - SEMICOLON                 
         DC    C':'                END OF REPORT CONTROL CHR                    
MAXLINL2 EQU   *-MAXLIN2                                                        
                                                                                
*---------------------------------------------------------------------          
* DOWNLOAD PRINT HOOK                                                           
*---------------------------------------------------------------------          
DOWNPRT  NTR1                                                                   
         MVI   LINE,0              FORCE NO PAGE BREAK                          
         MVI   FORCEHED,C'N'       AND NO HEADLINES !                           
         MVI   FORCEMID,C'N'       OR MIDLINES !                                
         GOTOR SPOOL,DMCB,(R8)                                                  
         MVI   FORCEHED,C'N'       AND NO HEADLINES !                           
         MVI   FORCEMID,C'N'       OR MIDLINES !                                
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* TABLE OF DATA DISPLACEMENTS AND LENGTHS FOR DOWNLOAD REPORT                   
*=================================================================              
                                                                                
RPRECTAB DS    0D                                                               
         DC    A(RPTSTATN-RPTCOL1),AL1(L'RPTSTATN),C'T',XL2'00'                 
         DC    A(RPTSTEFD-RPTCOL1),AL1(L'RPTSTEFD),C'T',XL2'1000'               
         DC    A(RPTREP-RPTCOL1),AL1(L'RPTREP),C'T',XL2'00'                     
         DC    A(RPTSTAT-RPTCOL1),AL1(L'RPTSTAT),C'T',XL2'00'                   
         DC    A(RPTEFDT-RPTCOL1),AL1(L'RPTEFDT),C'T',XL2'1000'                 
         DC    A(RPTPREP-RPTCOL1),AL1(L'RPTPREP),C'T',XL2'00'                   
         DC    A(RPTNSTA-RPTCOL1),AL1(L'RPTNSTA),C'T',XL2'00'                   
         DC    A(RPTNSDT-RPTCOL1),AL1(L'RPTNSDT),C'T',XL2'1000'                 
         DC    A(RPTMKT-RPTCOL1),AL1(L'RPTMKT),C'T',XL2'00'                     
         DC    A(RPTRID-RPTCOL1),AL1(L'RPTRID),C'T',XL2'00'                     
         DC    A(RPTBILTO-RPTCOL1),AL1(L'RPTBILTO),C'T',XL2'00'                 
         DC    A(RPTRP2-RPTCOL1),AL1(L'RPTRP2),C'T',XL2'00'                     
         DC    X'FF'                                                            
                                                                                
*================================================================               
* VALIDATE SECONDARY REP CODES                                                  
* NOTE:  AIO2  GETS CLOBBERED                                                   
*================================================================               
                                                                                
MAXRP2ER EQU   450                 MAX 2ND REPS                                 
NOT2ND   EQU   451                 CURRENT REP CAN'T BE SECONDARY               
DUPREP2  EQU   452                 DUPLICATE REP IN SECONDARY LIST              
EOCRP2ER EQU   453                 EOC REPS CANNOT BE SECONDARY                 
EOCNO2RP EQU   454                 EOC PRIMARY REP, NO SECONDARY REPS           
                                                                                
DO2REP   NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2                                                         
         LA    R2,SFMCRP2H                                                      
*                                                                               
         TM    STATUS2,STT2EOC     PRIMARY REP IS END OF CONTRACT?              
         BZ    DO2RP010            NO                                           
         CLI   5(R2),0             USER INPUT 2ND REPS?                         
         JNE   DO2RPER5            YES, 2ND REPS NOT VALID                      
*                                                                               
DO2RP010 LA    R0,BLOCK1                                                        
         LHI   R1,BLOCK1X-BLOCK1                                                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),BLOCK1                                         
*                                                                               
         LA    R0,6                                                             
         LA    R1,BLOCK1           MAKE SURE NO MORE THAN 6 REPS                
*                                                                               
DO2RP020 CLI   0(R1),0                                                          
         JE    DO2RP030                                                         
         LA    R1,32(R1)                                                        
         JCT   R0,DO2RP020                                                      
         CLI   0(R1),0                                                          
         JNE   DO2RPER3            ERROR, MAX OF 6 2ND REPS                     
*                                                                               
DO2RP030 LA    R4,BLOCK1           POINT TO FIRST SECONDARY REP                 
         LA    R5,STAREPS-STAREPD+ELEM                                          
*                                                                               
DO2RP040 CLC   =C'EOC',12(R4)      IF REP EOC?                                  
         BE    DO2RPER4            YES, EOC NOT VALID 2ND REP                   
         CLC   =C'ENC',12(R4)      IF REP ENC?                                  
         BE    DO2RPER4            YES, ENC NOT VALID 2ND REP                   
*                                                                               
DO2RP050 XC    KEY,KEY             CHECK EDIPARTNER RECORD                      
         LA    R6,KEY                                                           
         USING CTEPRECD,R6                                                      
         MVI   CTEPKSTY,CTEPKSTQ   X'0039' - TYPE AND SUBTYPE                   
         MVC   CTEPKREP(L'FLTCREP),12(R4)                                       
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(32),KEYSAVE                                                  
         BNE   DO2RPERR            BAD REP                                      
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         LA    R6,CTEPDAT                                                       
DO2RP060 CLI   0(R6),0                                                          
         JE    *+2                 DIE IF NO PRIMARY INFO ELEM                  
         CLI   0(R6),CTEPRELQ      X'02' - PRIMARY INFO ELEMENT                 
         BE    DO2RP070                                                         
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DO2RP060                                                         
*                                                                               
         USING CTEPRD,R6                                                        
DO2RP070 CLC   SFMMEDA,CTEPRMED    MUST MATCH MEDIA                             
         JNE   DO2RPERR            BAD REP                                      
                                                                                
* CHECK TO SEE IF ALREADY IN LIST OR MATCHES CURRENT REP                        
                                                                                
         CLC   SFMCREP,12(R4)      MATCH CURRENT REP?                           
         BE    DO2RPER1            YES, DUPLICATE PRIMARY REP ERROR             
*                                                                               
DO2RP080 LA    R1,STAREPS-STAREPD+ELEM                                          
*                                                                               
DO2RP090 CLC   12(3,R4),0(R1)      MATCH THIS ENTRY                             
         BE    DO2RPER2            YES, DUPLICATE REP ERROR                     
         LA    R1,3(R1)                                                         
         CLI   0(R1),C' '                                                       
         JH    DO2RP090                                                         
*                                                                               
         MVC   0(3,R5),12(R4)      MOVE REP TO ELEMENT                          
         LA    R5,3(R5)            NEXT ELEMENT POSN                            
*                                                                               
         LA    R4,32(R4)           NEXT SCANNER ENTRY                           
         CLI   0(R4),0                                                          
         JNE   DO2RP040                                                         
*                                                                               
         LA    RE,STAREPCR-STAREPD+ELEM                                         
         LA    RF,STAREPS-STAREPD+ELEM                                          
         MVC   0(3,RE),0(RF)       MOVE FIRST SECONDARY REP HERE TOO            
         MVC   AIO,AIO1                                                         
         J     EXIT                                                             
*                                                                               
DO2RPERR MVC   GERROR,=Y(BADREP)      BAD REP FOR THIS MEDIA                    
*                                                                               
DO2RPERX LA    R2,SFMCRP2H                                                      
         LA    RE,12(R4)                                                        
         STCM  RE,7,GATXT                                                       
         MVI   GLTXT,3                SET TEXT LENGTH                           
         J     SFMERROR                                                         
*                                                                               
DO2RPER1 MVC   GERROR,=Y(NOT2ND)   ALREADY PRIMARY REP                          
         J     SFMERROR                                                         
*                                                                               
DO2RPER2 MVC   GERROR,=Y(DUPREP2)  DUPLICATE 2ND REP                            
         J     DO2RPERX                                                         
*                                                                               
DO2RPER3 MVC   GERROR,=Y(MAXRP2ER) MAX 6 2ND REPS                               
         J     SFMERROR                                                         
*                                                                               
DO2RPER4 MVC   GERROR,=Y(EOCRP2ER) EOC REPS CANNOT BE 2ND REP                   
         J     SFMERROR                                                         
*                                                                               
DO2RPER5 MVC   GERROR,=Y(EOCNO2RP) EOC PRIMARY REP, NO 2ND REPS                 
         J     SFMERROR                                                         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS SECTION OF CODE MOVED TO GAIN SOME ADDRESSABILITY SPACE.                 
***********************************************************************         
SETUP    NTR1  LABEL=*,BASE=*                                                   
*                                                                               
         L     R4,AIO              CHECK KEY SAME AS RECORD                     
         USING STAKEYD,R4                                                       
*                                                                               
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BNE   SETUP10                                                          
         GOTO1 HEXOUT,DMCB,KEY+STAKDA-STAKEY,SFMDSKA+4,4                        
         OI    SFMDSKAH+6,X'80'                                                 
*                                                                               
SETUP10  XC    SFMCREP,SFMCREP                                                  
         NI    SFMCREPH+1,X'FF'-X'20'                                           
         OI    SFMCREPH+6,X'80'                                                 
         OI    SFMCREPH+4,X'20'                                                 
         XC    SFMPREP,SFMPREP                                                  
         OI    SFMPREPH+6,X'80'                                                 
         OI    SFMPREPH+4,X'20'                                                 
         XC    SFMDATE,SFMDATE                                                  
         MVI   SFMDATEH+5,0                                                     
         NI    SFMDATEH+1,X'FF'-X'20'                                           
         OI    SFMDATEH+6,X'80'                                                 
         OI    SFMDATEH+4,X'20'                                                 
*                                  REMOVED DARE/NOT DARE STATUS -- WH           
         XC    SFMNSTA,SFMNSTA                                                  
         NI    SFMNSTAH+1,X'FF'-X'20'                                           
         OI    SFMNSTAH+6,X'80'                                                 
         OI    SFMNSTAH+4,X'20'                                                 
         XC    SFMNSDT,SFMNSDT                                                  
         NI    SFMNSDTH+1,X'FF'-X'20'                                           
         OI    SFMNSDTH+6,X'80'                                                 
         OI    SFMNSDTH+4,X'20'                                                 
         XC    SFMCITY,SFMCITY                                                  
         MVI   SFMCITYH+5,0                                                     
         OI    SFMCITYH+6,X'80'                                                 
         OI    SFMCITYH+4,X'20'                                                 
         XC    SFMRID,SFMRID                                                    
         MVI   SFMRIDH+5,0                                                      
         OI    SFMRIDH+6,X'80'                                                  
         OI    SFMRIDH+4,X'20'                                                  
         XC    SFMEINV,SFMEINV                                                  
         OI    SFMEINVH+6,X'80'                                                 
         OI    SFMEINVH+4,X'20'                                                 
         XC    SFMBLTO,SFMBLTO                                                  
         NI    SFMBLTOH+1,X'FF'-X'20'                                           
         OI    SFMBLTOH+6,X'80'                                                 
         OI    SFMBLTOH+4,X'20'                                                 
         J     EXIT                                                             
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET THE CALL LETTERS GIVEN A UID IN KEY                                       
*                                                                               
* ON ENTRY:    KEY                 DSTA KEY FOR STATION USING UID               
*              PARAM 1             DATA FIELD FOR CALL LETTERS                  
***********************************************************************         
GETCALLS NTR1  BASE=*,LABEL=*                                                   
         L     R2,DMCB                                                          
         MVC   KEYSAVE,KEY         KEYSAVE WILL HAVE THE UID                    
         XC    KEY,KEY                                                          
         LA    R4,KEY              GET CALL LETTERS FROM M STREET REC           
         USING CT99KEY,R4                                                       
         MVI   CT99KTYP,CT99KTYQ   X'99' - CTGENRAD UID RECORD                  
         MVC   CT99KUID,STAKPUID-STAKEY+KEYSAVE                                 
         DROP  R4                                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',(R4),AIO3                 
*                                                                               
         L     R6,AIO3                                                          
         CLC   KEY(L'CT99KEY),0(R6)    FIND MY UID RECORD?                      
         BE    GTCALL10                                                         
         MVC   0(6,R2),STAKPUID-STAKEY+KEYSAVE   NO, JUST SHOW UID              
         B     GTCALL40                                                         
*                                                                               
         USING CT99KEY,R6                                                       
GTCALL10 LA    R6,CT99DATA         X'01' ELEM HAS THE CALL LETTERS              
         USING CRCLD,R6                                                         
         MVC   0(4,R2),CRCLCLL                                                  
         CLI   3(R2),C' '                                                       
         BNE   GTCALL20                                                         
         MVI   3(R2),C'-'                                                       
         MVC   4(1,R2),CRCLBND                                                  
         B     GTCALL30                                                         
*                                                                               
GTCALL20 MVI   4(R2),C'-'                                                       
         MVC   5(1,R2),CRCLBND                                                  
*                                                                               
GTCALL30 MVC   TEMPEXT,CRCLBND     CALL LETTER EXTENSION                        
         DROP  R6                                                               
*                                                                               
GTCALL40 MVC   KEY,KEYSAVE                                                      
         J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
* SPECIAL PENDING REPORT                                                        
***********************************************************************         
SPR      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,HEDSPEC2         HEADLINE SPECS                               
         ST    R1,SPECS                                                         
         LA    R1,HDHOOK2          HEAD HOOK                                    
         ST    R1,HEADHOOK                                                      
*                                                                               
         CLI   SFRMEDAH+5,1        IS THE MEDIA 1 CHAR?                         
         JNE   VKEY0080                                                         
         CLI   SFRMEDA,C'R'                                                     
         JNE   VKEY0080                                                         
         XC    MEDNAME,MEDNAME                                                  
         MVC   MEDNAME(5),=C'RADIO'                                             
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(19,PWOSDATE)                                  
*                                                                               
         LA    R4,KEY                                                           
         USING STAKEYD,R4                                                       
*                                                                               
         MVI   STAKSYS,STAKSYSQ    BUILD SEARCH KEY USE INPUT                   
         MVI   STAKTYP,STAKTYPQ                                                 
         MVI   STAKPMED,C'R'                                                    
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         GOTO1 HIGH                                                             
*                                                                               
SPR20    CLC   KEY(STAKPUID-STAKEY),SAVEKEY                                     
         JNE   EXIT                DONE, EXIT PROGRAM                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         MVC   SRPUNIQ,STAKPUID                                                 
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,STAREPCQ                                                  
         BAS   RE,GETEL                                                         
         USING STAREPD,R3                                                       
         MVC   OLDREP,STAREPCR                                                  
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,STARCELQ                                                  
         BAS   RE,GETEL                                                         
         BE    SPR40                                                            
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,STARHELQ                                                  
         BAS   RE,GETEL                                                         
         BE    SPR70                                                            
*                                                                               
SPRSEQ   GOTO1 SEQ                                                              
         XC    P,P                                                              
         B     SPR20                                                            
*                                                                               
SPR40    LA    R2,WORK             R2 = A(WORK)                                 
         XC    WORK,WORK                                                        
*                                                                               
         CLI   0(R3),STARCELQ      DO I HAVE A PENDING CHANGE?                  
         BNE   SPR70               NO, REP CHANGE ELEMENT                       
         USING STARCELD,R3                                                      
*                                                                               
* HAVE A PENDING ELEMENT, DISPLAY IT?                                           
*                                                                               
         CLC   =C'NOR',OLDREP      IS CURRENT REP NOR?                          
         BE    SPR45               YES, SKIP CHECK                              
         MVC   WORK+8(L'OLDREP),OLDREP                                          
         BAS   RE,CHKMASTR         CURRENT REP A SUBSIDIARY OF MASTER?          
         BE    SPR50               YES, DISPLAY IT                              
*                                                                               
SPR45    OC    STARCGRP,STARCGRP   DO WE HAVE A GAIN REP?                       
         BZ    SPRSEQ              NO, SKIP IT                                  
         CLC   =C'NOR',STARCGRP    NO REP?                                      
         BE    SPRSEQ              YES, SKIP IT                                 
         MVC   WORK+8(L'STARCGRP),STARCGRP                                      
         BAS   RE,CHKMASTR         YES, IS G-REP A SUBSIDARY OF MASTER?         
         BNE   SPRSEQ              NO, SKIP IT                                  
*                                                                               
* DISPLAY THE PENDING ELEMENT!                                                  
*                                                                               
SPR50    MVC   SRPSTAT,=C' PENDING '                                            
         MVC   SRPGREP,STARCGRP    DISPLAY THE GAIN REP                         
         MVC   SRPLREP,OLDREP      DISPLAY THE LOSE REP                         
*                                                                               
         OC    STARCGED,STARCGED                                                
         BZ    SPR60                                                            
         GOTO1 DATCON,DMCB,(8,STARCGED),(11,SRPGEFF)                            
         MVI   SRPGBT,C'B'                                                      
         TM    STARCGST,STARCTRL                                                
         BZ    *+8                                                              
         MVI   SRPGBT,C'T'                                                      
*                                                                               
SPR60    OC    STARCLED,STARCLED                                                
         BZ    SPR90                                                            
         GOTO1 DATCON,DMCB,(8,STARCLED),(11,SRPLEFF)                            
         MVI   SRPLBT,C'B'                                                      
         TM    STARCLST,STARCTRL                                                
         BZ    *+8                                                              
         MVI   SRPLBT,C'T'                                                      
         B     SPR90                                                            
*                                                                               
* HAVE A REP CHANGE HISTORY ELEMENT, DISPLAY IT?                                
*                                                                               
         USING STARHELD,R3                                                      
SPR70    MVC   WORK+8(L'STARHGRP),STARHGRP                                      
         BAS   RE,CHKMASTR         IS G-REP A SUBSIDIARY OF MASTER?             
         BE    SPR75               YES, CHECK THE DATE                          
         MVC   WORK+8(L'STARHLRP),STARHLRP                                      
         BAS   RE,CHKMASTR         IS L-REP A SUBSIDIARY OF MASTER?             
         BNE   SPRSEQ              NO , SKIP IT                                 
*                                                                               
SPR75    MVC   WORK(L'STARHEDT),STARHEDT                                        
         XC    WORK(L'STARHEDT),=X'FFFFFF'                                      
         CLC   PWOSDATE,WORK       PAST EFFECTIVE DATE?                         
         BNL   SPRSEQ              YES, SKIP IT                                 
*                                                                               
* DISPLAY THE REP CHANGE ELEMENT                                                
*                                                                               
         MVC   SRPSTAT,=C' ACCEPTED'                                            
         MVC   SRPGREP,STARHGRP    DISPLAY THE GAIN REP                         
         MVC   SRPLREP,STARHLRP    DISPLAY THE LOSE REP                         
         GOTO1 DATCON,DMCB,(8,WORK),(11,SRPLEFF)                                
         MVC   SRPGEFF,SRPLEFF                                                  
         MVI   SRPLBT,C'B'                                                      
         TM    STARHSTA,STARHTRL                                                
         BZ    *+8                                                              
         MVI   SRPLBT,C'T'                                                      
         MVC   SRPGBT,SRPLBT                                                    
*                                                                               
SPR90    DS    0H                                                               
         LA    R1,SRPSTATN                                                      
         ST    R1,DMCB                                                          
         BRAS  RE,GETCALLS                                                      
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     DISPLAY RECORD                               
*                                                                               
         B     SPRSEQ                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE THE NEW TIME STAMP ELEMENT                                             
***********************************************************************         
UPDATTIM NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R2,ELEM                                                          
         USING DATTIMD,R2                                                       
         MVI   DATTIM,DATTIMLQ     X'D1'                                        
         MVI   DATTIMLN,DATTMLNQ                                                
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(19,DATTMGDT)                                  
         THMS  DDSTIME=YES                                                      
         STCM  R0,15,PACKOF4B                                                   
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
*                                                                               
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    UPDT10                                                           
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 DATCON,DMCB,(5,0),(0,DUB)   THE TIME                             
         GOTO1 ADDAY,DMCB,DUB,DUB,F'1'                                          
         GOTO1 DATCON,DMCB,(0,DUB),(19,DATTMGDT)                                
*                                                                               
UPDT10   ICM   R1,15,PACKOF4B                                                   
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STCM  R1,3,DATTMGTM                                                    
         MVC   DATTMCDT,DATTMGDT                                                
         MVC   DATTMCTM,DATTMGTM                                                
*                                                                               
         L     R3,AIO              WE DON'T KNOW CREATION DATE SO WE'LL         
         MVI   ELCODE,X'F1'           USE THE X'F1' ELEM TO GET IT              
         BAS   RE,GETEL                                                         
         BNE   UPDT15                                                           
         USING ACTVD,R3                                                         
         GOTO1 DATCON,DMCB,(3,ACTVADDT),(19,DATTMCDT)                           
         DROP  R3                                                               
*                                                                               
UPDT15   L     R3,AIO                                                           
         MVI   ELCODE,DATTIMLQ     X'D1'                                        
         BAS   RE,GETEL            DO WE HAVE ONE?                              
         BNE   UPDT20              NO, ADD IT                                   
OLD      USING DATTIMD,R3                                                       
         MVC   DATTMCDT,OLD.DATTMCDT                                            
         MVC   DATTMCTM,OLD.DATTMCTM                                            
         MVC   WORK(DATTMLNQ),ELEM      SAVE ELEM, REMELEM CLOBBERS IT          
         GOTO1 REMELEM                                                          
         MVC   ELEM(DATTMLNQ),WORK      RESTORE ELEM                            
UPDT20   GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         JNE   *+2                                                              
         DROP  OLD,R2                                                           
         J     EXIT                                                             
         LTORG                                                                  
***********************************************************************         
* CHECK IF CURRENT REP IS A SUBSIDIARY OF THE MASTER                            
***********************************************************************         
CHKMASTR NTR1                                                                   
         CLC   =C'MS',14(RA)       TEST MASTER REP                              
         BE    CMYES                                                            
         CLC   =C'SJ',14(RA)       SJR                                          
         BE    CMYES                                                            
*                                                                               
CMNO     J     NO                                                               
CMYES    J     YES                                                              
         LTORG                                                                  
*                                                                               
HDHOOK2  MVC   H1+55(23),=C'DSTATION PENDING REPORT'                            
         MVC   H4+8(L'MEDNAME),MEDNAME MEDIA                                    
         J     EXIT                                                             
*                                                                               
HEDSPEC2 SSPEC H1,1,AGYNAME                                                     
         SSPEC H2,1,AGYADD                                                      
         SSPEC H1,95,REPORT                                                     
         SSPEC H1,114,REQUESTOR                                                 
         SSPEC H2,95,RUN                                                        
         SSPEC H2,121,PAGE                                                      
         SSPEC H4,1,C'MEDIA:'                                                   
         SSPEC H8,2,C'STATION'                                                  
         SSPEC H8,12,C'UNIQID'                                                  
         SSPEC H8,22,C'STATUS'                                                  
         SSPEC H8,32,C'TO REP'                                                  
         SSPEC H8,41,C'EFF DATE'                                                
         SSPEC H8,51,C'B/T'                                                     
         SSPEC H8,57,C'FROM REP'                                                
         SSPEC H8,68,C'EFF DATE'                                                
         SSPEC H8,78,C'B/T'                                                     
         DC    X'00'                                                            
*                                                                               
***********************************************************************         
* DISPLAY FOR THE REP UPDATES                                                   
***********************************************************************         
DRUPDTE  NTR1  BASE=*,LABEL=*                                                   
         OI    CONACTH+4,X'20'     VALIDATE THE ACTION FIELD                    
         OI    CONACTH+6,X'80'                                                  
*                                                                               
         SR    RE,RE               MODIFIED TWAXC TO TURN ON VALIDATED          
         LA    R1,RCMGREPH                                                      
         LA    RF,RCMLBTOH                                                      
DRUP01   IC    RE,0(R1)                                                         
         TM    1(R1),X'20'         PROTECTED?                                   
         BO    DRUP09                                                           
         SH    RE,=H'9'                                                         
         TM    1(R1),X'02'                                                      
         BZ    *+8                                                              
         SH    RE,=H'8'                                                         
*                                                                               
         LTR   RE,RE                                                            
         BM    DRUP10                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         XC    8(0,R1),8(R1)                                                    
         OI    6(R1),X'80'                                                      
         OI    4(R1),X'20'         VALIDATED                                    
         IC    RE,0(R1)                                                         
DRUP09   BXLE  R1,RE,DRUP01                                                     
*                                                                               
DRUP10   XC    RCMLREP,RCMLREP     NEED TO CLEAR THIS PROTECTED FLD             
         OI    RCMLREPH+6,X'80'                                                 
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'STAKEY),KEYSAVE                                            
         BE    DRUP15                                                           
         LA    R2,SFMMEDAH                                                      
         NI    4(R2),X'FF'-X'20'   INVALIDATE THE MEDIA                         
         MVC   GERROR,=AL2(53)                                                  
         B     SFMERROR            RECORD DOES NOT EXIST                        
*                                                                               
DRUP15   MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,STAREPCQ     X'10' - INFO ELEM                            
         BRAS  RE,GETEL                                                         
         BNE   DRUP20                                                           
         USING STAREPD,R3                                                       
         MVC   RCMLREP,STAREPCR                                                 
         OC    STAREPED,STAREPED                                                
         BZ    DRUP20                                                           
         GOTO1 DATCON,DMCB,(5,0),(15,WORK)                                      
         CLC   STAREPED,WORK                                                    
         BNH   DRUP20              EFF DATE NOT HIGHER THAN TODAY               
***************                                                                 
* PENDING CHANGE NOT EFFECTIVE YET                                              
***************                                                                 
         MVC   RCMLREP,STAREPPR                                                 
         DROP  R3                                                               
*                                                                               
DRUP20   L     R3,AIO                                                           
         MVI   ELCODE,STARCELQ     X'50' - PENDING REP CHANGE ELEM              
         BRAS  RE,GETEL                                                         
         BNE   DRUP50                                                           
         USING STARCELD,R3                                                      
         OC    STARCGRP,STARCGRP   WE HAVE A GAIN REP?                          
         BZ    DRUP30                                                           
         MVC   RCMGREP,STARCGRP                                                 
         GOTO1 DATCON,DMCB,(8,STARCGED),(11,RCMGDTE)                            
         TM    STARCGST,STARCTRL   X'80' - GAIN REP WANTS A TRAILOUT?           
         BZ    *+8                                                              
         MVI   RCMGBTO,C'T'        YES                                          
         TM    STARCGST,STARCBUY   X'40' - GAIN REP WANTS A BUYOUT?             
         BZ    *+8                                                              
         MVI   RCMGBTO,C'B'        BUYOUT IS DEFAULT, BUT ...                   
*                                                                               
DRUP30   OC    STARCLED,STARCLED   ANY LOSE EFFECTIVE DATE?                     
         BZ    DRUP40              NONE                                         
         GOTO1 DATCON,DMCB,(8,STARCLED),(11,RCMLDTE)                            
*                                                                               
DRUP40   TM    STARCLST,STARCTRL   X'80' - LOSE REP WANTS A TRAILOUT?           
         BZ    *+8                                                              
         MVI   RCMLBTO,C'T'        YES                                          
         TM    STARCLST,STARCBUY   X'40' - LOSE REP WANTS A BUYOUT?             
         BZ    *+8                                                              
         MVI   RCMLBTO,C'B'        BUYOUT IS DEFAULT, BUT ...                   
** WE WANT THE REP TO ACTUALLY TYPE SOMETHING HERE SO THE PROGRAM               
** DOESN'T MAKE ASSUMPTIONS                                                     
         DROP  R3                                                               
*                                                                               
DRUP50   TWAXC RCML1GRH,PROT=Y     CLEARING ANY OLD HISTORY OUT THERE           
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,STARHELQ     X'60' - REP CHANGE HISTORY ELEM              
         BRAS  RE,GETEL                                                         
         BNE   DRUPX               MUST BE A REP ELEMENT                        
         LA    R2,RCML1GRH                                                      
         USING HISTLIND,R2                                                      
*                                                                               
         USING STARHELD,R3                                                      
DRUP60   MVC   HISTGREP,STARHGRP                                                
         MVC   WORK(L'STARHEDT),STARHEDT                                        
*********************************************                                   
* CODE SAVED FOR ANOTHER LOW PRIORITY PROJECT                                   
*********************************************                                   
***      CLI   WORK,X'E0'          WAS IT FF'D?                                 
***      BL    *+10                                                             
*********************************************                                   
* CODE SAVED FOR ANOTHER LOW PRIORITY PROJECT                                   
*********************************************                                   
         XC    WORK(L'STARHEDT),=X'FFFFFF'                                      
         GOTO1 DATCON,DMCB,(8,WORK),(11,HISTEDTE)                               
         MVC   HISTLREP,STARHLRP                                                
*                                                                               
         MVI   HISTBTOT,C'B'       IN HISTORY, BUYOUT IS THE DEFAULT            
         TM    STARHSTA,STARHTRL   X'80' - REP CHANGE WAS A TRAILOUT            
         BZ    *+8                                                              
         MVI   HISTBTOT,C'T'       YES                                          
         DROP  R3                                                               
*                                                                               
         LA    R2,HISTNXTL         WE SHOWED MORE THAN 5 CHANGES?               
         LA    R0,RCML5GRH                                                      
         CR    R2,R0                                                            
         BH    DRUPX               YES, THAT'S ALL WE'LL SHOW FOR NOW           
         DROP  R2                                                               
*                                                                               
DRUP70   BRAS  RE,NEXTEL                                                        
         BE    DRUP60                                                           
*                                                                               
DRUPX    J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE FOR THE REP UPDATES                                                  
***********************************************************************         
VRUPDTE  NTR1  BASE=*,LABEL=*                                                   
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         XC    ELEM,ELEM                                                        
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,STAREPCQ     X'10' REP ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   ELEM+128(STAREPLQ),0(R3)  KEEP IN 2ND HALF OF ELEM               
VRUP00D  USING STAREPD,ELEM+128                                                 
         OC    VRUP00D.STAREPED,VRUP00D.STAREPED  ANY EFF DATE?                 
         BNZ   VRUP03                             YES                           
         MVI   ELEM+128,0                                                       
         B     VRUP06                             NONE, WE CAN CHANGE           
*                                                                               
VRUP03   GOTO1 DATCON,DMCB,(5,0),(15,FULL)                                      
         CLC   VRUP00D.STAREPED,FULL     REP CHANGE EFF DATE > TODAY?           
         BH    VRUP06                    OLD REP STILL ACTIVE                   
         XC    ELEM,ELEM                                                        
*                                                                               
VRUP06   L     R3,AIO                                                           
         MVI   ELCODE,STARCELQ     X'50' ANY PENDING REP CHANGE?                
         BAS   RE,GETEL                                                         
         BE    VRUP08                                                           
         LA    R3,ELEM             ADD ELEM IF R3 --> ELEM                      
         USING STARCELD,R3                                                      
         MVI   STARCEL,STARCELQ                                                 
         MVI   STARCLEN,STARCLNQ                                                
*                                                                               
VRUP08   TM    RCMGREPH+4,X'20'    ANY CHANGES WE'RE CONCERNED WITH?            
         BZ    VRUP10                                                           
         TM    RCMGDTEH+4,X'20'                                                 
         BZ    VRUP10                                                           
         TM    RCMGBTOH+4,X'20'                                                 
         BZ    VRUP10                                                           
*                                  NO GAIN FIELDS CHANGED                       
         TM    RCMLDTEH+4,X'20'                                                 
         BZ    VRUP50                                                           
         TM    RCMLBTOH+4,X'20'                                                 
         BZ    VRUP50                                                           
         B     VRUPX               NONE, NO FIELDS CHANGED                      
*********                                                                       
* GAIN REP SECTION                                                              
*********                                                                       
VRUP10   LA    R2,RCMGREPH         GAIN REP CHANGED?                            
         OC    8(3,R2),SPACES                                                   
         LA    R1,8(R2)                                                         
         BRAS  RE,REPSTAT                                                       
*                                                                               
         L     R6,AIO2                                                          
         USING CTEPRECD,R6                                                      
         LA    R6,CTEPDAT                                                       
VRUP12   CLI   0(R6),0                                                          
         JE    *+2                                                              
         CLI   0(R6),CTEPRELQ      X'02' - PRIMARY INFO ELEMENT                 
         BE    VRUP15                                                           
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     VRUP12                                                           
*                                                                               
         USING CTEPRD,R6                                                        
VRUP15   CLI   SFMMEDA,C'R'        IS THIS A RADIO REP?                         
         JNE   INVLFLD                                                          
         DROP  R6                                                               
*                                                                               
         CLC   RCMGREP,RCMLREP     GAIN AND LOSE REP CAN'T BE THE SAME          
         JE    INVLFLD                                                          
         MVC   STARCGRP,8(R2)                                                   
*                                                                               
         XC    GREPALPH(L'GREPALPH+L'GMRPALPH),GREPALPH                         
         CLC   =C'NOR',STARCGRP    NO REP?                                      
         BE    VRUP35              YES, NO POWER CODE NOR MASTER                
         BRAS  RE,GETRPALF                                                      
         JNE   REPERROR                                                         
         MVC   GREPALPH(L'GREPALPH+L'GMRPALPH),REPALPHA                         
         CLC   GMRPALPH,14(RA)     GAIN'S MASTER = USER'S POWERCODE?            
         JE    VRUP35                                                           
         CLC   =C'SJ',14(RA)       ARE WE SJR?                                  
         BE    VRUP35              THEN WE'RE OKAY                              
MASTRERR MVC   GERROR,=AL2(83)     ONLY REP'S MASTER CAN CHANGE LINE            
         J     SFMERROR                                                         
*                                                                               
VRUP35   CLI   VRUP00D.STAREPC,STAREPCQ  X'10' EFFECTIVE DATE > TODAY?          
         BNE   VRUP38                    NO, OKAY TO CHANGE                     
         CLC   VRUP00D.STAREPCR,RCMGREP  MATCHES GAIN IN  X'10'?                
         BE    VRUP38                                                           
VRUPEFFC MVC   GERROR,=AL2(66)     NO, STILL WAITING FOR CURRENT EFFEC          
         LA    R2,RCMGREPH             DATE TO GO                               
         B     SFMERROR                                                         
*                                                                               
VRUP38   LA    R2,RCMGDTEH                                                      
         CLI   5(R2),0                                                          
         JE    MISSFLD                                                          
*                                                                               
         CLC   =C'REM',8(R2)             REP WANTS TO REMOVE?                   
         BNE   VRUP40                                                           
         CLI   VRUP00D.STAREPC,STAREPCQ  X'10' EFFECTIVE DATE > TODAY?          
         JNE   INVLFLD                   NO, CAN'T USE REMOVE                   
         GOTO1 RVRSECHG,DMCB,(R2)                                               
         JNE   INVLFLD                                                          
         B     VRUPPUT                                                          
*                                                                               
AFTRTDAY MVC   GERROR,=AL2(67)     DATE MUST BE AFTER TODAY                     
         J     SFMERROR                                                         
*                                                                               
VRUP40   CLI   VRUP00D.STAREPC,STAREPCQ  X'10' EFFECTIVE DATE > TODAY?          
         BE    VRUPEFFC                  YES, STILL WAITING FOR EFFEC           
         BAS   RE,VALIADTE                                                      
*                                                                               
         LA    R6,PERVALST                                                      
         USING PERVALD,R6                                                       
         CLC   PVALESTA,TODAYDTE   COMPARE INPUTTED DATE WITH TODAY             
         JNH   AFTRTDAY                                                         
         MVC   8(8,R2),PVALCPER    RECOPY NEW VALID DATE                        
         MVI   5(R2),8                                                          
         GOTO1 DATCON,DMCB,(0,PVALESTA),(19,STARCGED)                           
         DROP  R6                                                               
*                                                                               
         LA    R2,RCMGBTOH                                                      
         CLI   5(R2),0                                                          
         JE    MISSFLD                                                          
         NI    STARCGST,X'FF'-(STARCBUY+STARCTRL)                               
         CLI   8(R2),C'B'                                                       
         BNE   *+12                                                             
         OI    STARCGST,STARCBUY                                                
         B     VRUP47                                                           
         CLI   8(R2),C'T'                                                       
         BNE   VRUP47                                                           
***************                                                                 
* TRAILOUT SECTION                                                              
***************                                                                 
         CLC   RCMGREP,=C'NOR'                                                  
         BE    VRUP40ER                                                         
         CLC   RCMLREP,=C'NOR'                                                  
         BE    VRUP40ER                                                         
         LA    R2,RCMLREPH         LOSE REP                                     
         OC    RCMLREP,SPACES                                                   
         XC    LREPALPH(L'LREPALPH+L'LMRPALPH),LREPALPH                         
         BRAS  RE,GETRPALF                                                      
         BNE   VRUP40ER                                                         
         MVC   LREPALPH(L'LREPALPH+L'LMRPALPH),REPALPHA                         
*                                                                               
         CLC   GMRPALPH,LMRPALPH            SAME AS GAIN REP'S MASTER           
         BE    VRUP45                                                           
VRUP40ER LA    R2,RCMGBTOH                                                      
         MVC   GERROR,=AL2(82)     MASTER REPS ARE NOT THE SAME                 
         J     SFMERROR                                                         
*                                                                               
VRUP45   OI    STARCGST,STARCTRL                                                
*                                                                               
VRUP47   TM    RCMLDTEH+4,X'20'    LOSE EFFECTIVE PREV VALIDATED?               
         BZ    VRUP50                                                           
         TM    RCMLBTOH+4,X'20'         BUY/TRAIL PREV VALIDATED?               
         BZ    VRUP50              NONE, LOSE FIELDS NOT CHANGED                
         CLI   RCMLDTEH+5,0        ANYTHING IN THESE FIELDS                     
         BNE   VRUP50                                                           
         CLI   RCMLBTOH+5,0                                                     
         BNE   VRUP50                                                           
*                                                                               
         LA    R0,ELEM             NOTHING                                      
         CR    R3,R0                                                            
         BNE   VRUP49              NEED TO ADD THE ELEM TO DSTA RECORD          
         GOTO1 ADDELEM                                                          
*                                                                               
VRUP49   B     VRUP75              PUT THE RECORD BACK OUT                      
*********                                                                       
* LOSE REP SECTION                                                              
*********                                                                       
VRUP50   MVC   STARCLRP,RCMLREP                                                 
*                                                                               
         XC    LREPALPH(L'LREPALPH+L'LMRPALPH),LREPALPH                         
         CLC   =C'NOR',STARCLRP                                                 
         BE    VRUP55                                                           
         LA    R2,RCMLREPH                                                      
         OC    RCMLREP,SPACES                                                   
         BRAS  RE,GETRPALF                                                      
         JNE   REPERROR                                                         
         MVC   LREPALPH(L'LREPALPH+L'LMRPALPH),REPALPHA                         
         CLC   LMRPALPH,14(RA)     GAIN'S MASTER = USER'S POWERCODE?            
         BE    VRUP55                                                           
         CLC   =C'SJ',14(RA)          ARE WE SJR?                               
         BNE   MASTRERR            NO, ONLY MASTER CAN CHANGE THIS LINE         
*                                                                               
VRUP55   LA    R2,RCMLDTEH         NO CHANGE TO LOSE EFFECTIVE                  
         CLI   5(R2),0                                                          
         JE    MISSFLD                                                          
*                                                                               
         CLC   =C'REM',8(R2)                                                    
         BNE   VRUP59                                                           
         CLI   VRUP00D.STAREPC,STAREPCQ  X'10' EFFECTIVE DATE > TODAY?          
         JNE   INVLFLD                   NO, CAN'T USE REMOVE                   
         GOTO1 RVRSECHG,DMCB,(R2)                                               
         JNE   INVLFLD                                                          
         B     VRUPPUT                                                          
*                                                                               
VRUP59   CLI   VRUP00D.STAREPC,STAREPCQ  X'10' EFFECTIVE DATE > TODAY?          
         BE    VRUPEFFC                  YES, STILL WAITING FOR EFFEC           
         BAS   RE,VALIADTE                                                      
*                                                                               
         LA    R6,PERVALST                                                      
         USING PERVALD,R6                                                       
         CLC   PVALESTA,TODAYDTE   COMPARE INPUTTED DATE WITH TODAY             
         JNH   AFTRTDAY            TELL USER MUST BE GREATER                    
         MVC   8(8,R2),PVALCPER    RECOPY NEW VALID DATE                        
         MVI   5(R2),8                                                          
         GOTO1 DATCON,DMCB,(0,PVALESTA),(19,STARCLED)                           
         DROP  R6                                                               
*                                                                               
VRUP60   LA    R2,RCMLBTOH                                                      
         CLI   5(R2),0                                                          
         JE    MISSFLD                                                          
         NI    STARCLST,X'FF'-(STARCBUY+STARCTRL)                               
         CLI   8(R2),C'B'                                                       
         BNE   *+12                                                             
         OI    STARCLST,STARCBUY                                                
         B     VRUP70                                                           
         CLI   8(R2),C'T'                                                       
         BNE   *+8                                                              
         OI    STARCLST,STARCTRL                                                
*                                                                               
VRUP70   LA    R0,ELEM                                                          
         CR    R3,R0                                                            
         BNE   VRUP75              NEED TO ADD THE ELEM TO DSTA RECORD          
         GOTO1 ADDELEM                                                          
*                                                                               
VRUP75   L     R3,AIO                                                           
         MVI   ELCODE,STARCELQ     X'50' ANY PENDING REP CHANGE?                
         BRAS  RE,GETEL            DEFINITELY HAVE ONE BY NOW                   
*                                                                               
         USING STARCELD,R3                                                      
         CLC   STARCGED,STARCLED   EFFECTIVE DATE AND BUY/TRAIL OUT             
         BNE   VRUPPUT                HAS TO MATCH BEFORE WE CREATE             
         CLC   STARCGST,STARCLST      A NEW STAREPCQ  (X'10')                   
         BNE   VRUPPUT                                                          
***********************************                                             
* GAIN AND LOSE MATCHES                                                         
***********************************                                             
         CLI   VRUP00D.STAREPC,STAREPCQ   X'10' EFFECTIVE > TODAY?              
         BNE   VRUP90              NO                                           
         CLC   VRUP00D.STAREPCR,RCMGREP  MATCHES GAIN IN  X'10'?                
         BNE   VRUPEFFC                                                         
         CLC   VRUP00D.STAREPPR,RCMLREP  MATCHES LOSE IN  X'10'?                
         BNE   VRUPEFFC                                                         
***************                                                                 
* JUST CHANGING THE EFFECTIVE DATE IN X'10' AND X'60'                           
***************                                                                 
         MVC   FULL(3),STARCGED    SAVE EFFECTIVE DATE (PWOS JULIAN)            
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,STAREPCQ     X'10' - INFO ELEMENT                         
         BRAS  RE,GETEL                                                         
         USING STAREPD,R3                                                       
         GOTO1 DATCON,DMCB,(8,FULL),(15,STAREPED)                               
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,STARHELQ     X'60' - REP CHANGE HISTORY ELEM              
         BRAS  RE,GETEL                                                         
         BNE   VRUP95              NO HISTORY TO CHANGE                         
         USING STARHELD,R3                                                      
         XC    FULL(3),=X'FFFFFF'                                               
         MVC   STARHEDT,FULL                                                    
         B     VRUP95              DELETE PENDING AND PUT RECORD OUT            
***************                                                                 
* NORMAL PENDING CHANGE ELEMENT BECOMING EFFECTIVE REP                          
***************                                                                 
         USING STARCELD,R3                                                      
VRUP90   XC    ELEM,ELEM           SETUP REP CHANGE HISTORY ELEM                
         LA    R6,ELEM                                                          
         USING STARHELD,R6                                                      
         MVI   STARHEL,STARHELQ                                                 
         MVI   STARHLEN,STARHLNQ                                                
         MVC   STARHGRP,STARCGRP                                                
         MVC   STARHEDT,STARCGED                                                
         XC    STARHEDT,=X'FFFFFF' SO WE HAVE SORTED HISTORY ELEMS              
         MVC   STARHLRP,STARCLRP                                                
         TM    STARCGST,STARCBUY   BUY IS DEFAULT IN HISTORY                    
         BNZ   VRUP92                                                           
         TM    STARCGST,STARCTRL   DO WE HAVE A TRAILOUT?                       
         BZ    VRUP92                                                           
         OI    STARHSTA,STARHTRL   YES                                          
*                                                                               
VRUP92   GOTO1 ADDELEM             MOST RECENT FIRST                            
*  UPDATE CURRENT REP AND PREV REP IN THE INFO ELEMENT                          
         L     R3,AIO                                                           
         MVI   ELCODE,STAREPCQ     X'10' - INFO ELEMENT                         
         BRAS  RE,GETEL                                                         
         USING STAREPD,R3                                                       
         MVC   STAREPPR,STAREPCR                                                
         MVC   STAREPCR,STARHGRP   CURRENT REP IS NOW GAIN REP                  
         MVC   WORK(3),STARHEDT                                                 
         XC    WORK(3),=X'FFFFFF'                                               
         GOTO1 DATCON,DMCB,(8,WORK),(15,STAREPED)                               
*  IS CURRENT REP A DARE REP?                                                   
         MVC   BYTE,STAREPST       COPY THE STATUS                              
         LA    R1,STAREPCR                                                      
         BRAS  RE,REPSTAT                                                       
         MVC   STAREPST,STATUS     SET CURRENT REP DARE STATUS                  
*                                                                               
         TM    BYTE,STAREPST_ND    WAS CURRENT REP NOT A DARE REP?              
         BZ    *+8                                                              
         OI    STAREPST,STAREPST_PND   YES, PREV REP IS NOT A DARE REP          
         DROP  R3,R6                                                            
*                                                                               
VRUP95   L     R3,AIO              NOW WE CAN DELETE THE                        
         MVI   ELCODE,STARCELQ     X'50' - PENDING REP CHANGE ELEM              
         GOTO1 REMELEM                                                          
*                                                                               
VRUPPUT  L     R3,AIO                                                           
         MVC   KEY(L'STAKEY),0(R3)                                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 READ                                                             
         MVC   AIO,AIO3            READ INTO AIO3 FOR THE UPDATIVE              
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         GOTO1 PUTREC              PUT THE RECORD BACK OUT                      
*                                                                               
         BRAS  RE,DRUPDTE          RE-DISPLAY                                   
*                                                                               
VRUP99   OI    RCMGREPH+4,X'20'                                                 
         OI    RCMGDTEH+4,X'20'                                                 
         OI    RCMGBTOH+4,X'20'                                                 
         OI    RCMLDTEH+4,X'20'                                                 
         OI    RCMLBTOH+4,X'20'                                                 
*                                                                               
VRUPX    OI    RCMGREPH+6,X'80'                                                 
         OI    RCMGDTEH+6,X'80'                                                 
         OI    RCMGBTOH+6,X'80'                                                 
         OI    RCMLDTEH+6,X'80'                                                 
         OI    RCMLBTOH+6,X'80'                                                 
         J     EXIT                                                             
         DROP  VRUP00D                                                          
         EJECT                                                                  
***********************************************************************         
* VALIDATE A DATE                                                               
*                                                                               
* ON ENTRY:    (R2)                A(FIELD HEADER)                              
*                                                                               
* ON EXIT:     PERVALST            PERVAL OUTPUT AREA                           
***********************************************************************         
VALIADTE NTR1                                                                   
         TM    4(R2),X'04'         VALID ALPHA?                                 
         BZ    VDTE10              NO                                           
         CLI   5(R2),3             GREATER THAN 3 LETTERS?                      
         BH    VDTE30              THEN PERVAL IT                               
         LA    R3,8(R2)                                                         
         ICM   R3,8,5(R2)                                                       
         GOTO1 VDAYVAL,DMCB,(R3),DUB,DUB+1                                      
         CLI   DUB,0               VALID ALPHA DAY?                             
         BE    VDTE30              MAYBE PERVAL CAN HANDLE IT                   
         MVC   DATEBIN(1),DUB+1    GET NUMBER EQUIVALENT OF DAY                 
         NI    DATEBIN,X'0F'       REMOVE THE DUPLICATE                         
         GOTO1 GETDAY,DMCB,TODAYDTE,DUB                                         
         MVC   DATEBIN+1(1),DMCB   GET NUMBER EQUIV. OF DAY FOR TODAY           
         ZIC   R1,DATEBIN                                                       
         CLC   DATEBIN(1),DATEBIN+1                                             
         BH    *+8                                                              
         LA    R1,7(R1)            ADD A WEEK AHEAD                             
         MVI   DATEBIN,0                                                        
         SH    R1,DATEBIN          DIFFERENCE IN # OF DAYS                      
         MVC   8(4,R2),=C'T(1)'                                                 
         STC   R1,10(R2)                                                        
         OI    10(R2),X'F0'        MAKE EBCDIC NUMERIC                          
         MVI   5(R2),4                                                          
         B     VDTE30              LET PERVAL HANDLE IT FROM HERE               
*                                                                               
VDTE10   TM    4(R2),X'08'         VALID NUMERIC?                               
         BZ    VDTE30              NO                                           
         CLI   5(R2),2             1 OR 2 DIGIT NUMERIC?                        
         BH    VDTE30              NO, GO THROUGH PERVAL                        
         ZIC   R1,5(R2)            GET LENGTH OF INPUT                          
         BCTR  R1,0                -1 FOR PACK                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)         CONVERT TO PACKED                            
         CVB   R0,DUB              CONVERT TO BINARY                            
         GOTO1 DATCON,DMCB,(5,0),(3,DUB)  GET TODAY'S DATE 3 BYTE BIN           
         MVC   DUB+3(3),DUB        MAKE A COPY                                  
         STC   R0,DUB+2            STORE THE DAY AWAY                           
         CLM   R0,1,DUB+5          DAY GREATER?                                 
         BH    VDTE20              YES, SAME MONTH                              
         IC    R0,DUB+1            GET MONTH(TODAY)                             
         AH    R0,=H'1'            NEXT MONTH                                   
         STC   R0,DUB+1            STORE IT                                     
         CLI   DUB+1,12            MONTH(TODAY) DECEMBER?                       
         BNH   VDTE20              NO                                           
         IC    R0,DUB              GET YEAR(TODAY)                              
         AH    R0,=H'1'            NEXT YEAR                                    
         STC   R0,DUB              STORE IT                                     
         MVI   DUB+1,1             MONTH IS JANUARY                             
VDTE20   DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,DUB),(11,8(R2))  CONVERT THE DATE                 
         MVI   5(R2),8             NEW LENGTH IS 8                              
VDTE30   LA    R3,8(R2)            PREPARE FIRST PARAMETER FOR PERVAL           
         ICM   R3,8,5(R2)          LENGTH OF INPUT                              
         O     R3,=X'40000000'     MM/DD  NOT  MM/YY                            
         LA    R4,PERVALST         ADDR OF OUTPUT AREA                          
*        ICM   R4,8,LANGNUM        ONLY IF WE WANT DATE IN LANG FORMAT          
         O     R4,=X'40000000'     SINGLE DATE ONLY IS VALID                    
         GOTO1 PERVAL,DMCB,(R3),(R4)                                            
         TM    DMCB+4,X'03'        DID WE GET VALID INPUT?                      
         JNZ   INVLFLD             NO                                           
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* REVERSES A REP CHANGE THAT HASN'T REACHED THE EFFECTIVE DATE                  
*                                                                               
* ON ENTRY:    (R2)                A(FLDHDR OF GAIN/LOSE REP EFF DATE)          
*              AIO                 A(DSTA RECORD)                               
***********************************************************************         
RVRSECHG NTR1                                                                   
         L     R3,AIO                                                           
         MVI   ELCODE,STAREPCQ     X'10' REP ELEMENT                            
         BRAS  RE,GETEL                                                         
         LR    R4,R3               WE'LL USE R4 TO POINT TO REP ELEM            
         USING STAREPD,R4                                                       
*                                                                               
         MVI   ELCODE,STARHELQ     X'60' REP CHANGE HISTORY ELEMENT             
RVRS10   BAS   RE,NEXTEL                                                        
         BNE   RVRSNO              NONE, CAN'T BE DONE                          
***********************************************************************         
* FIRST X'60' HISTORY IS GETTING CONVERTED TO A X'50' PENDING                   
***********************************************************************         
         USING STARHELD,R3                                                      
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING STARCELD,R6                                                      
         MVI   STARCEL,STARCELQ                                                 
         MVI   STARCLEN,STARCLNQ                                                
*                                                                               
         LA    R0,RCMGDTEH         GAIN REP REMOVING?                           
         CR    R2,R0                                                            
         BNE   RVRS20                                                           
*                                                                               
         MVC   STARCLRP,STARHLRP   NOW COPY OVER LOSE REP DATA                  
         MVC   STARCLED,STARHEDT                                                
         XC    STARCLED,=X'FFFFFF'                                              
         TM    STARHSTA,STARHTRL                                                
         BNZ   *+12                                                             
         OI    STARCLST,STARCBUY                                                
         B     RVRS30                                                           
         OI    STARCLST,STARCTRL                                                
         B     RVRS30                                                           
*                                                                               
RVRS20   MVC   STARCGRP,STARHGRP   NOW COPY OVER GAIN REP DATA                  
         MVC   STARCGED,STARHEDT                                                
         XC    STARCGED,=X'FFFFFF'                                              
         TM    STARHSTA,STARHTRL                                                
         BNZ   *+12                                                             
         OI    STARCGST,STARCBUY                                                
         B     RVRS30                                                           
         OI    STARCGST,STARCTRL                                                
         DROP  R6                                                               
***************                                                                 
* WE ALSO HAVE TO MODIFY THE X'10' WITH WHAT WE HAVE IN CASE NO PRIORS          
***************                                                                 
RVRS30   MVC   STAREPCR,STAREPPR   CURR REP IS NOW PREV REP IN X'10'            
         XC    STAREPPR,STAREPPR   FILL IN WHAT WE CAN LATER                    
         XC    STAREPED,STAREPED                                                
         LA    R1,STAREPCR         POINT TO CURRENT REP                         
         BRAS  RE,REPSTAT                                                       
         MVC   STAREPST,STATUS     SET CURRENT REP DARE STATUS                  
*                                                                               
         XC    WORK,WORK                 SAVE HIST ELEM BEFORE ADDELEM          
         MVC   WORK(STARHLNQ-2),STARHEDT  BUMPS HISTORY ELEM FURTHER            
*                                                                               
         GOTO1 ADDELEM             ADD THE PENDING CHANGE                       
*                                                                               
         LA    R1,WORK             ONLY DELETE THIS HISTORY ELEM                
         ST    R1,DMCB+8                                                        
         MVI   DMCB+8,STARHLNQ-2                                                
         GOTO1 HELLO,DMCB,(C'D',=C'GENFIL'),(X'60',AIO),,0 DELETE HIST          
***************                                                                 
* AFTER THE DELETE R3 SHOULD ACTUALLY BE THE NEXT X'60'                         
***************                                                                 
         CLI   0(R3),0             HIT END OF RECORD AFTER DELETE?              
         BE    RVRSYES             YES, WE'RE DONE HERE                         
         CLI   0(R3),X'60'                                                      
         BE    RVRS50                                                           
RVRS40   BRAS  RE,NEXTEL           NEXT X'60' CAN BE PRIOR HISTORY              
         BNE   RVRSYES             NONE                                         
*                                                                               
RVRS50   MVC   STAREPPR,STARHLRP   FILL IN WHAT WE CAN LATER                    
         MVC   WORK(3),STARHEDT                                                 
         XC    WORK(3),=X'FFFFFF'                                               
         GOTO1 DATCON,DMCB,(8,WORK),(15,STAREPED)                               
         LA    R1,STAREPPR         POINT TO PREVIOUS REP                        
         BRAS  RE,REPSTAT                                                       
         TM    STATUS,X'80'                                                     
         BZ    *+8                                                              
         OI    STAREPST,X'40'                                                   
*                                                                               
RVRSYES  J     YES                                                              
*                                                                               
RVRSNO   J     NO                                                               
         DROP  R3,R4                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET'S THE REP'S ALPHA CODE FROM THE 3 CHAR REP USER ENTERED                   
*                                                                               
* ON ENTRY:    (R2)                REP FIELD HEADER                             
*                                                                               
* ON EXIT:     REPALPHA            REP'S AGENCY ALPHA                           
*              MRPALPHA            MASTER REP'S AGENCY ALPHA                    
***********************************************************************         
GETRPALF NTR1  BASE=*,LABEL=*                                                   
         LA    R1,8(R2)            POINT TO TEXT OF 3 CHAR REP CODE             
         BRAS  RE,REPSTAT                                                       
*                                                                               
         L     R6,AIO2             R6 = A(REP PARTNER RECORD)                   
         USING CTEPRECD,R6                                                      
         LA    R6,CTEPDAT                                                       
GRALF10  CLI   0(R6),0                                                          
         JNE   *+2                                                              
         CLI   0(R6),CTEPRELQ      X'02' - PRIMARY INFO ELEMENT                 
         BE    GRALF20                                                          
         LLC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     GRALF10                                                          
         DROP  R6                                                               
*                                                                               
GRALF20  LR    R5,R6                                                            
         USING CTEPRD,R5                                                        
         MVC   AIO,AIO3                                                         
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTIKEY,R4                                                        
         MVI   CTIKTYP,CTIKTYPQ    C'I' - ID RECORD TO GET POWER CODE           
         MVC   CTIKID,CTEPPREF     COPY REP PREFIX                              
         TM    CTEPFLG1,CTE1NOOF   REP SIGNON DOESN'T APPEND OFFICE?            
         BNZ   GRALF30             LEAVE IT LIKE IT IS IN VREPIDS               
         LLC   RF,CTEPLPRF         L(REP USERID PREFIX)                         
         LA    R1,CTIKID(RF)                                                    
         MVC   0(2,R1),=C'NY'      ASSUME ALL REPS HAVE 'NY' OFFICE             
*                                                                               
GRALF30  GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',(R4),AIO3                 
*                                                                               
         L     R6,AIO3                                                          
         LLC   R2,CTEPLPRF                                                      
         AHI   R2,CTIKID-CTIKEY                                                 
         TM    CTEPFLG1,CTE1NOOF   REP SIGNON DOESN'T APPEND OFFICE?            
         BNZ   *+8                 LEAVE IT LIKE IT IS IN VREPIDS               
         AHI   R2,2                                                             
         DROP  R5                                                               
*                                                                               
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R4),0(R6)                                                    
         BNE   GRALFNO                                                          
*                                                                               
         L     R6,AIO3             LOOK FOR THE POWER CODE OF GAIN REP          
         XR    R0,R0                                                            
         AHI   R6,CTIDATA-CTIKEY                                                
GRALF40  CLI   0(R6),0                                                          
         BE    GRALFNO                                                          
         CLI   0(R6),X'06'                                                      
         BE    GRALF50                                                          
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     GRALF40                                                          
*                                                                               
         USING CTAGYD,R6                                                        
GRALF50  MVC   REPALPHA,CTAGYID    SAVE THE REP AGENCY ALPHA CODE               
*                                                                               
         MVC   MRPALPHA,REPALPHA   ASSUME REP HAS NO MASTER YET                 
         XC    KEY,KEY             WITH THE REP ALPHA CODE, WE CAN SEE          
         LA    R6,KEY                 IF IT IS A SUBSIDIARY OF A MASTER         
         USING GSPLKEY,R6                                                       
         MVI   GSPLKTYP,GSPLRECQ   X'71'                                        
         MVC   GSPLKREP,REPALPHA                                                
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   0(GSPLMREP-GSPLKEY,R6),KEYSAVE    REP HAS A MASTER REP?          
         BNE   *+10                              NO, IT IS A MASTER             
         MVC   MRPALPHA,GSPLMREP                 SAVE REP'S MASTER              
GRALFYES MVC   AIO,AIO1                                                         
         J     YES                                                              
*                                                                               
GRALFNO  MVC   AIO,AIO1                                                         
         MVC   GERROR,=AL2(102)     REP SYSTEM: INVALID REP                     
         J     NO                                                               
         DROP  R6                                                               
*                                                                               
         LTORG                                                                  
*************************************************                               
* VALIDATE THE STATION                                                          
*  ON ENTRY: PARM1   A(FIELD HEADER)                                            
*            PARM2   A(OUTPUT FIELD) OPTIONAL                                   
*                                                                               
*   ON EXIT: CC=EQ   PARM2  5-CHARACTER RETURNED                                
*            CC=NEQ  GERROR SET                                                 
*************************************************                               
VALISTA  NTR1  BASE=*,LABEL=*                                                   
         L     R2,DMCB             INPUT FIELD                                  
         L     R4,DMCB+4           OUTPUT FIELD                                 
*                                                                               
*                                                                               
         IF (CLI,8(R2),GE,C'0')   DO WE HAVE CABLE?                             
          DO FROM=(R1,4)         MAKE SURE FIRST 4 ARE DIGITS                   
           LA    RE,7(R1,R2)                                                    
           IF (CLI,0(RE),L,C'0'),OR,(CLI,0(RE),H,C'9')                          
            B     VSTANO        CAN'T BE CABLE                                  
           ENDIF                                                                
          ENDDO                                                                 
          IF (CLC,8(4,R2),EQ,=C'0000') CAN'T HAVE 0000                          
           B     VSTANO                                                         
          ENDIF                                                                 
          SELECT CLI,5(R2),EQ     LETS VALIDATE BAND, IF ANY                    
           WHEN (4)              4 MEANS NO BAND, ITS VALID                     
           WHEN (5)              IF WE HAVE 5 CHAR INPUT                        
            IF (CLI,12(R2),NE,C'T')      IS SUFFIX T?                           
             B     VSTANO                NO, IT'S INVALID                       
            ENDIF                                                               
           WHEN (6)              IF WE HAVE 6 CHAR INPUT                        
            IF (CLC,12(2,R2),NE,=C'-T')  IS SUFFIX -T?                          
             B     VSTANO                NO, IT'S INVALID                       
            ENDIF                                                               
           OTHRWISE              FOR ALL OTHER INPUT                            
            B     VSTANO          THEY ARE INVALID                              
          ENDSEL ,                                                              
         ENDIF                                                                  
*                                                                               
         LA    R3,BLOCK                                                         
         USING STABLKD,R3                                                       
         XC    0(STBLNQ,R3),0(R3)  CLEAR INTERFACE BLOCK                        
         MVC   STBMED,SFMMEDA      SET MEDIA                                    
         ST    R2,STBADDR          SET A(STATION FIELD)                         
         MVI   STBCTRY,C'U'        ALWAYS ASSUME US                             
         MVC   STBACOM,ACOMFACS                                                 
                                                                                
         XC    DMCB,DMCB           CLEAR 1ST PARAMETER OF DMCB                  
         MVC   DMCB+4(4),=X'D9000A68' GET ADDR OF STAVAL VIA CALLOV             
         GOTOR CALLOV,DMCB         RETURN WITH ADDR IN 1ST PARAM.               
         CLI   DMCB+4,X'FF'        COULDN'T GET ADDRESS?                        
         JE    *+2                 YES, DIE THEN                                
         L     RF,DMCB             RF = A(DDSTAVAL)                             
                                                                                
         GOTOR (RF),DMCB,(R3)      CALL STAVAL TO VALIDATE                      
         CLI   STBERR,0             STAVAL RETURN ERROR?                        
         BNE   VSTANO                YES                                        
                                                                                
         IF (CLI,STBSTA,L,C'0')    IS IT BROADCAST?                             
          LA    R1,STBSTA+3         YES, INDEX BKWD FROM 4TH CHAR               
          DO FROM=(R0,4)            AND REPEAT 4 TIMES                          
           IF (CHI,R0,EQ,4),AND,(CLI,0(R1),EQ,C' '),ORIF,              +        
               (CLI,0(R1),GE,C'A'),AND,(CLI,0(R1),LE,C'I'),ORIF,       +        
               (CLI,0(R1),GE,C'J'),AND,(CLI,0(R1),LE,C'R'),ORIF,       +        
               (CLI,0(R1),GE,C'S'),AND,(CLI,0(R1),LE,C'Z')                      
*                                  IF SPACE AND 4 CHAR     ORIF                 
*                                  LETTER GE A AND LE I    ORIF,                
*                                  LETTER GE J AND LE R    ORIF,                
*                                  LETTER GE S AND LE Z                         
            BCTR  R1,0             VALID ALPHA, GO BACK 1                       
           ELSE                                                                 
            B     VSTANO           THEN NOT A VALID ALPHA                       
           ENDIF                                                                
          ENDDO                                                                 
         ENDIF                                                                  
                                                                                
VSTAYES  LTR   R4,R4                                                            
         JZ    YES                                                              
         MVC   0(5,R4),STBSTA                                                   
         CLI   STBSTA,C'0'         IS IT CABLE?                                 
         JL    YES                  NO, JUST EXIT                               
         MVI   4(R4),C'T'           YES, DON'T FORGET THE BAND                  
         J     YES                                                              
*                                                                               
VSTANO   MVC   GERROR,=AL2(INVSTA) INVALID STATION CALL LETTERS                 
         J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
         EJECT                                                                  
       ++INCLUDE CTGENSTAD                                                      
         EJECT                                                                  
       ++INCLUDE CTGENRAD                                                       
         EJECT                                                                  
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE GEGENEDI                                                       
       ++INCLUDE GEGENSPSAL                                                     
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTGENDEPT                                                      
       ++INCLUDE DDACTIVD                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE SPSTABLK                                                       
         EJECT                                                                  
       ++INCLUDE CTSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMB8D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMC8D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMB9D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMD8D                                                       
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
*                                                                               
         ORG   SYSSPARE                                                         
VDAYVAL  DS    V                                                                
VDLFLD   DS    V                                                                
**VREPIDS  DS    V                                                              
LREPIDS  EQU   25    <-- SEE DDDARETAB                                          
DATEBIN  DS    H                   DATE IN COMPRESSED BINARY FORMAT             
RECLEN   DS    CL2                 RECORD LENGTH                                
TEMPKEY  DS    CL32                                                             
TEMPEXT  DS    CL1                 EXTENSION OF CALL LETTERS                    
FLTCREP  DS    CL3                 FILTER CURRENT  REP                          
FLTPREP  DS    CL3                        PREVIOUS REP                          
FLTCRP2  DS    CL3                        CURRENT  REP2                         
FLTPRP2  DS    CL3                        PREVIOUS REP2                         
FLTNWSTA DS    CL6                                                              
FLTNWSTL DS    X                   LENGTH OF NEW STATION FILTER                 
FLTBAND  DS    C                   FILTER BAND                                  
MEDNAME  DS    CL10                MEDIA TYPE                                   
MYFLAG   DS    XL1                 MY FLAGS                                     
SORTREP  EQU   X'01'                - SORT ON CURRENT REP                       
SORTSTAT EQU   X'02'                - SORT ON STATUS                            
NOCITY   EQU   X'04'                - NO HOME MARKET CITY                       
CALLSORT EQU   X'08'                - CALLED SORTER                             
KEYCHNGD EQU   X'80'                - KEY FIELD WAS CHANGED                     
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAGS 1                        
MF1RCCHG EQU   X'80'                - RECORD CHANGED, UPDATE IT                 
MF1NWSTA EQU   X'40'                - NEW STATION NEEDED, ADD IT                
*                                                                               
PREVSTA  DS    CL(L'STAKSTIN)      PREV CALL LETTER USED FOR "RECENT"           
PACKOF4B DS    PL4                 PACKED OF 4 BYTES                            
THISDATE DS    CL6                 YYMMDD                                       
RADIOUID DS    CL6                 RADIO UNIQUE ID (FROM M STREET)              
CONVDATE DS    CL4                 0CYYDDDF                                     
SAVEKEY  DS    CL32                                                             
SAVEKEY2 DS    CL32                                                             
SAVEKEY3 DS    CL32                                                             
BLOCK1   DS    10CL32              SCANNER OUTPUT                               
BLOCK1X  EQU   *                                                                
PERVALST DS    XL56                PERVAL OUTPUT                                
TODAYDTE DS    XL6                                                              
PWOSDATE DS    CL3                 TODAYS DATE PWOS                             
REPALPHA DS    CL2                 REP'S ALPHA CODE                             
MRPALPHA DS    CL2                 MASTER REP'S ALPHA CODE                      
GREPALPH DS    CL2                 GAIN REP'S ALPHA CODE                        
GMRPALPH DS    CL2                 GAIN MASTER REP'S ALPHA CODE                 
LREPALPH DS    CL2                 LOSE REP'S ALPHA CODE                        
LMRPALPH DS    CL2                 LOSE MASTER REP'S ALPHA CODE                 
OLDREP   DS    CL3                                                              
OLDPREV  DS    CL3                                                              
STATUS   DS    XL1                                                              
STT1NDAR EQU   X'80'               NOT A DARE REP                               
STATUS2  DS    XL1                                                              
STT2EOC  EQU   X'80'               END OF CONTRACT (EOC OR ENC)                 
STT2NVR  EQU   X'40'               NOT A VALID REP                              
*                                                                               
LUPDATE  DS    CL14                BUFFER FOR DATE/TIME OF LAST UPDATE          
OLDSTAT  DS    CL5                 OLD 'NEW STATION CALLS'                      
OLDEFDAT DS    CL4                 OLD 'NEW STATION EFFECTIVE DATE'             
*                                                                               
FILTFLAG DS    XL1                 FLAGS FOR MKT AND USER ID "FILTERS"          
SHOWDIRC EQU   X'01'                - SHOW DIRECT TO STATION RECS               
SHOWRECV EQU   X'02'                - SHOW HOME MARKET RECS                     
SHOWSPCL EQU   X'04'                - SHOW SPCL DARE LC RECS                    
SHOWRCNT EQU   X'08'                - SHOW ONLY MOST RECENT STATION             
*                                                                               
DOWNLOAD DS    C                                                                
DOWNINIT DS    C                                                                
DOWNFRST DS    C                                                                
*                                                                               
DLCB     DS    XL256                                                            
*                                                                               
MYIO     DS    1000X                                                            
*                                                                               
         EJECT                                                                  
* ONLINE LIST (1 LINE)                                                          
*SEL  MEDIA STATN EFF DATE REP REP DATE PREV NEWSTA STA DATE  MKT  USER         
*       T   YYVAT DEC05/06 VDA                                                  
*12345678901234567890123456789012345678901234567890123456789012345678           
SPOOLD   DSECT                                                                  
         ORG   P                                                                
RPTCOL1  DS    0C                                                               
         DS    CL1                                                              
RPTSTATN DS    CL5                 STATION                                      
         DS    CL1                                                              
RPTSTEFD DS    CL8                 STATION EFFECTIVE DATE                       
         DS    CL1                                                              
RPTREP   DS    CL3                 CURRENT REP                                  
         DS    CL1                                                              
RPTSTAT  DS    CL8                 STATUS                                       
         DS    CL1                                                              
RPTEFDT  DS    CL8                 EFFECTIVE DATE FOR CURRENT STATION           
         DS    CL1                                                              
RPTPREP  DS    CL3                 PREVIOUS REP                                 
         DS    CL1                                                              
RPTNSTA  DS    CL5                 NEW STATION                                  
         DS    CL1                                                              
RPTNSDT  DS    CL8                 EFFECTIVE DATE FOR NEW STATION               
         DS    CL1                                                              
RPTMKT   DS    CL2                 HOME MARKET CITY                             
         DS    CL2                                                              
RPTRID   DS    CL10                RECEIVING ID                                 
         DS    CL1                                                              
RPTCHGDT DS    CL8                 CHANGE DATE                                  
         DS    CL1                                                              
RPTBILTO DS    CL25                WHO TO BILL TO                               
****                                                                            
         DS    CL1                                                              
RPTRP2   DS    CL16                SECONDARY REPS                               
         ORG   P                                                                
         DS    CL2                                                              
SRPSTATN DS    CL5                                                              
         DS    CL4                                                              
SRPUNIQ  DS    CL6                                                              
         DS    CL3                                                              
SRPSTAT  DS    CL9                                                              
         DS    CL2                                                              
SRPLREP  DS    CL3                                                              
         DS    CL6                                                              
SRPLEFF  DS    CL8                                                              
         DS    CL2                                                              
SRPLBT   DS    CL1                                                              
         DS    CL5                                                              
SRPGREP  DS    CL3                                                              
         DS    CL8                                                              
SRPGEFF  DS    CL8                                                              
         DS    CL2                                                              
SRPGBT   DS    CL1                                                              
*                                                                               
GEND   DSECT                                                                    
         ORG   LISTAR                                                           
         DS    CL2                                                              
LSTMEDIA DS    CL1                 MEDIA                                        
         DS    CL3                                                              
LSTSTATN DS    CL5                 STATION                                      
         DS    CL1                                                              
LSTSFDT  DS    CL8                 STATION EFFECTIVE DATE                       
         DS    CL1                                                              
LSTREP   DS    CL3                 CURRENT REP                                  
         DS    CL1                                                              
LSTEFDT  DS    CL8                 EFFECTIVE DATE FOR CURRENT REP               
         DS    CL1                                                              
LSTPREP  DS    CL3                 PREVIOUS REP                                 
         DS    CL2                                                              
LSTNSTA  DS    CL5                 NEW STATION                                  
         DS    CL2                                                              
LSTNSDT  DS    CL8                 EFFECTIVE DATE FOR NEW STATION               
         DS    CL2                                                              
LSTMKT   DS    CL2                 HOME MARKET CITY                             
         DS    CL3                                                              
LSTRID   DS    CL10                RECEIVING ID                                 
*                                                                               
         ORG   LSTMKT                                                           
LSTRP2   DS    CL3                 2ND REP                                      
         DS    CL2                                                              
LSTPRP2  DS    CL3                 PREV 2ND REP                                 
*                                                                               
SORTKEYD DSECT                                                                  
SORTLEN  DS    CL2                 RECORD LENGTH                                
SORTSPAR DC    CL2'0000'                                                        
SORTDARE DS    CL1                 STATUS                                       
SORTCREP DS    CL3                 CURRENT REP                                  
*                                                                               
HISTLIND DSECT                                                                  
         DS    CL8                                                              
HISTGREP DS    CL3                 GAIN REP                                     
         DS    CL8                                                              
HISTEDTE DS    CL8                 EFF DATE                                     
         DS    CL8                                                              
HISTLREP DS    CL3                 GAIN REP                                     
         DS    CL8                                                              
HISTBTOT DS    CL1                 BUY/TRAIL OUT                                
*                                                                               
HISTNXTL DS    0X                                                               
*                                                                               
DWNTABD  DSECT                                                                  
DVALDISP DS    A                                                                
DVALLEN  DS    XL1                                                              
DVALTYPE DS    C                                                                
DVALIND  DS    C                                                                
DVALISO  EQU   X'10'                                                            
         DS    X                                                                
DWNTABLQ EQU   *-DWNTABD                                                        
       ++INCLUDE DDDLCB                                                         
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'134CTSFM13   08/10/20'                                      
         END                                                                    
