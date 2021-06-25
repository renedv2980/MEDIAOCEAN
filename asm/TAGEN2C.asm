*          DATA SET TAGEN2C    AT LEVEL 099 AS OF 01/16/14                      
*PHASE T7022CC,*                                                                
         TITLE 'T7022C - ESTIMATE RECORD MAINTENANCE'                           
T7022C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T7022C,R8,R7,R6,RR=R2                                          
         L     RC,0(R1)            RC=CONTROLLER STORAGE AREA                   
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         LA    R5,TWAHOLE          R5=LOCAL WORKING STORAGE                     
         USING ESTD,R5                                                          
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 2                                                                
         TM    TRNSTAT,RETURNED    IF JUST POPPED                               
         BZ    XC5                                                              
         MVC   CONHEAD,SCONHEAD    RESET MESSAGE                                
         MVI   ERROR,X'FE'         SET DUMMY ERROR                              
         GOTO1 EXIT                                                             
         SPACE                                                                  
XC5      GOTO1 INITIAL,DMCB,0      SYSTEM INITIALIZATION                        
         ST    RC,SVRC                                                          
         ST    R2,RELO                                                          
         MVI   NEWEST,C'N'         RESET ESTIMATE INDICATOR                     
         TM    TRNSTAT2,CTFLSTER   IF FULL SESSION STEREO                       
         BZ    *+8                                                              
         MVI   NEWEST,C'Y'         SET NEW ESTIMATE DISPLAY                     
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY FIELDS                           
         BNE   XC15                                                             
         BAS   RE,DKEY                                                          
         SPACE 1                                                                
XC10     MVC   KEY,ESTKEY          ** RECDEL AND RECREST ENTER HERE **          
         OI    DMINBTS,X'08'                                                    
         GOTO1 HIGH                INSURE WE HAVE RECORD FOR CONTROLLER         
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'F7'                                                    
         B     XIT                                                              
         SPACE 2                                                                
XC15     CLI   MODE,VALKEY         VALIDATE KEY FIELDS                          
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 2                                                                
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   XC30                                                             
         NI    STATUS2,ALL-BUILDING                                             
         CLI   ACTNUM,ACTSEL       IF ACTION IS SELECT                          
         BNE   XC20                                                             
         CLI   THISLSEL,C'D'       SKIP IF IT'S DELETE                          
         BE    XC20                                                             
         OI    GENSTAT2,RETEQSEL   SET TO RETURN THIS SELECTION                 
         SPACE 1                                                                
         CLI   THISLSEL,C'C'       IF IT'S CHANGE                               
         BNE   XC20                                                             
         ZIC   R1,SELLISTN         FIND SELECT CODE IN LIST DIRECTORY           
         MH    R1,=H'6'                                                         
         LA    R1,LISTDIR(R1)                                                   
         MVI   0(R1),C'C'          AND RESET CONTROLLER'S SELECT CODE           
         B     XC40                GO TO VALREC ROUTINE                         
         SPACE 1                                                                
XC20     B     PROCESS             PROCESS DISPLAY                              
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES, CONTINUED                              
         SPACE 2                                                                
XC30     CLI   MODE,VALREC         ** ENTER HERE FOR ACTION ADD/CHANGE          
         BNE   XC50                                                             
         TM    STATUS2,KEYVALTD    HAVE WE VALIDATED KEY                        
         BO    *+8                                                              
         BAS   RE,VKEY             NO - CONTROLLER DIDN'T GIVE VALKEY           
         SPACE 1                                                                
*                                  ** ENTER HERE FOR CHANGES FROM LIST          
XC40     OI    STATUS2,BUILDING    SET BUILD STATUS                             
         TM    DISPMODE,HYPO       IF WE'RE IN HYPO MODE                        
         BZ    *+8                                                              
         OI    HYPPFPGH+4,X'20'    INSURE HYPO PAGE SET VALIDATED               
         BAS   RE,CHKACTV          CHK ACTIVITY EL NEEDS TO BE REPLACED         
         B     PROCESS             PROCESS ADD/CHANGE                           
         SPACE 2                                                                
XC50     CLI   MODE,RECPUT         IMMEDIATELY PRECEEDING CHANGE                
         BNE   XC60                                                             
         MVI   IOOPT,C'Y'          SET SO CONTROLLER DOESN'T DO WRITE           
         B     XIT                                                              
         SPACE 2                                                                
XC60     CLI   MODE,XRECPUT        POST-CHANGE                                  
         BNE   XC70                                                             
         MVI   IOOPT,C'N'          RESET I/O SWITCH                             
         B     XIT                                                              
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES, CONTINUED                              
         SPACE 2                                                                
XC70     CLI   MODE,RECDEL         DELETE                                       
         BE    *+12                                                             
         CLI   MODE,RECREST        OR RESTORE                                   
         BNE   XC80                                                             
         SPACE 1                                                                
         CLI   ACTNUM,ACTSEL       IF ACTION IS SELECT                          
         BNE   *+16                                                             
         CLI   THISLSEL,DELSELQ    AND GENCON'S INTERNAL DELETE SET             
         BNE   *+8                                                              
         BAS   RE,DKEY             PROCESS DISPKEY - DIDN'T GET MODE            
         SPACE 1                                                                
         GOTO1 AINITSTR            INITIALIZE STORAGE                           
         BAS   RE,DISPLAY          DISPLAY TOP HALF OF SCREEN FOR USER          
         NI    STATUS2,ALL-KEYVALTD SET KEY NOT VALIDATED FOR NEXT TIME         
         B     XC10                INSURE WE HAVE RECORD FOR CONTROLLER         
         SPACE 2                                                                
XC80     CLI   MODE,XRECDEL        POST-DELETE                                  
         BNE   XC90                                                             
         OI    DMINBTS,X'08'       SET TO READ DELETED                          
         GOTO1 ADELETE             DELETE ANY REMAINING RECORDS                 
         B     XIT                                                              
         SPACE 2                                                                
XC90     CLI   MODE,XRECREST       POST-RESTORE                                 
         BNE   XIT                                                              
         L     R3,AIO              R3=A(RECORD JUST RESTORED)                   
         USING TLESD,R3                                                         
XC94     LR    R4,R3                                                            
         MVI   ELCODE,TAACELQ      LOOK FOR ACTIVITY EL.                        
         BAS   RE,GETEL                                                         
         BE    XIT                 FOUND IT, SO NOTHING MORE TO RESTORE         
         GOTO1 HIGH                RE-READ DIRECTORY REC. JUST RESTORED         
         MVI   RDUPDATE,C'Y'                                                    
         OI    DMINBTS,X'08'       SET READ FOR DELETED                         
         GOTO1 SEQ                 CHECK FOR ANOTHER RECORD                     
         NI    DMINBTS,X'F7'                                                    
         CLC   KEY(TLESSEQ-TLESD),KEYSAVE  TEST STILL SAME ESTIMATE             
         BNE   XIT                                                              
         OI    DMINBTS,X'08'       SET READ FOR DELETED                         
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC              GET THE RECORD                               
         NI    TLESSTAT,X'7F'      UNMARK IT                                    
         GOTO1 PUTREC              AND WRITE IT BACK                            
         NI    KEY+TLDRSTAT-TLDRD,X'7F'  UNMARK DIRECTORY RECORD                
         GOTO1 WRITE                     AND WRITE IT BACK                      
         B     XC94                LOOK FOR MORE                                
         EJECT                                                                  
*              ROUTINE SETS FLAG IF ACTIVITY ELEMENT                            
*              NEEDS TO BE REPLACED                                             
         SPACE                                                                  
CHKACTV  NTR1                                                                   
         NI    STATUS3,ALL-ADDACTV DEFAULT NOT TO REPLACE ACTIVITY EL           
         CLI   NEWEST,C'Y'         IF NOT NEW ESTIMATE DISPLAY                  
         BE    CHKACT3                                                          
*                                                                               
         LA    R2,999              CHECK TILL END OF SCREEN                     
         TM    DISPMODE,LISTHYP    IF LISTING SELECTABLE HYPO COMMLS            
         BZ    *+8                                                              
         LA    R2,ESTOVLYH         CHECK TOP OF SCREEN ONLY                     
         GOTO1 FLDVAL,DMCB,(X'40',ESTNAMEH),(X'80',(R2)) CK ANY FLD CHG         
         BE    *+8                                                              
         OI    STATUS3,ADDACTV     SET TO REPLACE ACTIVITY ELEMENT              
*                                  IF CHANGE BOTTOMS OF SCREENS                 
         GOTO1 FLDVAL,DMCB,(X'40',ESTOVLYH),(X'80',999)                         
         BE    *+8                                                              
         MVI   PFAID,PFSTAY        SIMULATE USER PRESSED 'STAY ON SCRN'         
         B     CHKACTX                                                          
*                                                                               
CHKACT3  LA    R2,999              CHECK TILL END OF SCREEN                     
         CLI   DISPMODE,LISTSCOM   IF LISTING SELECTABLE COMMERCIALS            
         BE    CHKACT7                                                          
         TM    DISPMD2,LISTSPRF    OR IF LISTING SELECTABLE PERFS               
         BO    CHKACT7                                                          
         TM    DISPMODE,LISTHYP    OR LISTING SELECTABLE HYPO COMMLS            
         BO    CHKACT7                                                          
         TM    DISPMODE,HYPO                                                    
         BZ    CHKACT8                                                          
         TM    DISPMD2,SELCTHPF                                                 
         BO    CHKACT8                                                          
*                                  OR HYPO SCRN, SELECT HYPO COMML/PERF         
CHKACT7  LA    R2,ESTOVLYH         CHECK TOP OF SCREEN ONLY                     
*                                                                               
CHKACT8  GOTO1 FLDVAL,DMCB,(X'40',ESTNAMEH),(X'80',(R2)) IF ANY FLD CHG         
         BNE   CHKACT20                                                         
         TM    DISPMODE,HYPO       IF HYPO SCREEN LOADED                        
         BZ    CHKACTX                                                          
         TM    DISPMD2,SELCTHPF                                                 
         BO    CHKACTX                                                          
*                                  CHECK MIDDLE SECTION (COMML INFO)            
         GOTO1 FLDVAL,DMCB,(X'40',HYPCOMH),(X'80',HYPOPT4H)                     
         BNE   CHKACT20                                                         
*                                                                               
         LA    R3,HYPDATAH         CHECK HYPO PERFORMER LIST                    
         USING LINHED,R3                                                        
CHKACT10 GOTO1 FLDVAL,DMCB,(X'40',LINHNUMH),(X'80',LINHCRPH)                    
         BNE   CHKACT20                                                         
         LA    R3,LINHNEXT                                                      
         LA    R1,HYPLSTDH                                                      
         CR    R3,R1                                                            
         BH    CHKACTX                                                          
         B     CHKACT10                                                         
*                                                                               
CHKACT20 OI    STATUS3,ADDACTV     SET TO REPLACE ACTIVITY ELEMENT              
CHKACTX  B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY KEY                                           
         SPACE 1                                                                
DKEY     NTR1                                                                   
         BRAS  RE,INIT             GENERAL INITIALIZING                         
         SPACE 1                                                                
         L     R3,AIO                                                           
         USING TLESD,R3                                                         
         MVC   ESTEST,TLESEST      ESTIMATE NUMBER TO SCREEN                    
         OI    ESTESTH+6,X'80'                                                  
         SPACE 1                                                                
         CLC   TLESEST,ESTKEY+TLESEST-TLESD IF ESTIMATE NUMBER CHANGED          
         BE    *+12                                                             
         OI    STATUS,RESET+DISPLYIT  SET TO RESTART                            
         NI    ESTCLIH+4,X'DF'        AND RE-VALIDATE CLIENT/PRODUCT            
         SPACE 1                                                                
         MVC   ESTKEY,TLESKEY                                                   
         GOTO1 AGETEREC,DMCB,(RC)  ESTIMATE RECORD(S) TO TIA                    
         SPACE 1                                                                
         TM    STATUS,RESET        IF KEY CHANGED                               
         BZ    DK8                                                              
         XC    TGCLI,TGCLI                                                      
         XC    TGPRD,TGPRD                                                      
         BAS   RE,GETCLIPR         GET FIRST CLIENT/PRODUCT                     
         SPACE 1                                                                
DK8      GOTO1 AVALFILT            VALIDATE DISPLAY FILTERS                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY                                          
VKEY     NTR1                                                                   
         BRAS  RE,INIT                                                          
         CLI   PFAID,PFREPORT      USER WANTS REPORT SCREEN                     
         BE    REPOUT                                                           
         SPACE 1                                                                
         TM    ESTAGYH+4,X'20'     IF AGENCY CHANGED                            
         BO    *+8                                                              
         NI    ESTESTH+4,X'DF'     SET ESTIMATE CHANGED                         
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',ESTAGYH)  VALIDATE AGENCY             
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
         SPACE 1                                                                
         LA    R2,ESTESTH          R2=A(ESTIMATE CODE)                          
         TM    4(R2),X'20'            IF ESTIMATE CHANGED                       
         BO    VK4                                                              
         OI    STATUS,RESET+DISPLYIT  SET TO RESTART                            
         CLI   ACTNUM,ACTADD          ONLY IF ADDING NEW RECORD                 
         BNE   *+8                                                              
         NI    ESTCLIH+4,X'DF'        SET TO VALIDATE CLIENT FIELD              
         SPACE 1                                                                
VK4      GOTO1 RECVAL,DMCB,TLESCDQ,(X'40',(R2))  BUILD EST. RECORD KEY          
         MVC   ESTKEY,KEY          INSURE WE HAVE CORRECT KEY                   
         SPACE 1                                                                
         GOTO1 AGETEREC,DMCB,(RC)  GET EXISTING RECORD IF AROUND                
         BE    VK6                                                              
         CLI   ACTNUM,ACTADD       IT ISN'T - IF THIS ISN'T ACTION ADD          
         BE    VK9                                                              
         CLI   ACTNUM,ACTREST      OR RESTORE                                   
         BE    VK9                                                              
         B     RECNTFND            THEN FLAG NO SUCH RECORD                     
         SPACE 1                                                                
VK6      CLI   ACTNUM,ACTADD       IT IS - IF THIS IS ACTION ADD                
         BNE   VK9                                                              
         TM    ESTESTH+4,X'20'     THEN IF THIS IS FIRST TIME FOR KEY           
         BZ    RECONFIL            FLAG ERROR                                   
         SPACE 1                                                                
VK9      OI    ESTESTH+4,X'20'     SET ESTIMATE NUMBER PREV. VALIDATED          
         TM    STATUS,RESET        IF KEY CHANGED                               
         BZ    VK11                                                             
         XC    TGCLI,TGCLI                                                      
         XC    TGPRD,TGPRD                                                      
         BAS   RE,GETCLIPR         GET FIRST CLIENT/PRODUCT                     
         SPACE 1                                                                
VK11     GOTO1 AVALFILT            VALIDATE DISPLAY FILTERS                     
         CLI   ACTNUM,ACTDEL       IF WE'RE DELETING                            
         BE    *+12                                                             
         CLI   ACTNUM,ACTREST      OR IF WE'RE RESTORING                        
         BNE   *+8                                                              
         NI    WHENOK,X'FE'        INSURE CONTROLLER HANDLES MAINT.             
         SPACE 1                                                                
         MVC   KEY,ESTKEY          RESTORE KEY FOR CONTROLLER                   
         OI    STATUS2,KEYVALTD    SET KEY HAS BEEN VALIDATED                   
         B     XIT                                                              
         EJECT                                                                  
         SPACE 1                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         EJECT                                                                  
*              ROUTINE TO DISPLAY ESTIMATE RECORDS                              
         SPACE 1                                                                
DISPLAY  NTR1                                                                   
         XC    ESTVER,ESTVER                                                    
         OI    ESTVERH+6,X'80'                                                  
         TM    STATUS2,BUILDING    IF BUILDING RECORD                           
         BZ    DISP2                                                            
         XC    ESTLCHG,ESTLCHG     CLEAR LAST CHANGED INFO                      
         OI    ESTLCHGH+6,X'80'    AND TRANSMIT                                 
         SPACE 1                                                                
DISP2    TM    STATUS2,HAVEREC     DON'T BOTHER IF NOT ON FILE YET              
         BZ    XIT                                                              
         MVC   AIO,ATIA            SET AIO=A(ESTIMATE RECORD)                   
         SPACE 1                                                                
         BAS   RE,DISPVER                                                       
         SPACE 1                                                                
         GOTO1 ACTVOUT,DMCB,ESTLCHGH  DISPLAY LAST CHANGED INFO                 
         SPACE 1                                                                
         TM    STATUS,DISPLYIT     ONLY IF CERTAIN FIELDS CHANGED               
         BZ    DISP4                                                            
         GOTO1 CHAROUT,DMCB,TANUELQ,ESTREVH,TANUTREV  DISPLAY REVISION          
         MVI   ESTREVH+5,0                                                      
         OC    ESTREV,ESTREV                                                    
         BZ    *+8                                                              
         MVI   ESTREVH+5,3         SIMULATE L'INPUT FOR INITREC                 
         SPACE 1                                                                
         GOTO1 (RF),(R1),TANAELQ,ESTNAMEH               ESTIMATE NAME           
         GOTO1 (RF),(R1),TACMELQ,(1,ESTNARRH),TACMTYPG  NARRATIVE               
         SPACE 1                                                                
**NO-OP* XC    SVNARR,SVNARR       CLEAR SAVED NARRATIVE                        
*        NI    ESTNARRH+1,X'DE'    UNPROTECT SCREEN FIELD                       
*        XC    ESTNARR,ESTNARR     PRE-CLEAR NARRATIVE FIELD                    
*        GOTO1 (RF),(R1),TACMELQ,(1,ESTNARRH),TACMTYPG  NARRATIVE               
*        MVI   ELCODE,TACMELQ                                                   
*        GOTO1 GETL,DMCB,(1,=AL1(TACMTYPG))                                     
*        BNE   DISP4                                                            
*        L     R4,TGELEM           SAVE NARRATIVE                               
*        ZIC   R1,1(R4)                                                         
*        BCTR  R1,0                                                             
*        EX    R1,*+8                                                           
*        B     *+10                                                             
**NO-OP* MVC   SVNARR(0),0(R4)                                                  
         SPACE 1                                                                
DISP4    MVC   AIO,AIO1            RESTORE DEFAULT I/O AREA                     
         SPACE 1                                                                
         TM    STATUS,PRDCHGED     ONLY IF PRODUCT CHANGED                      
         BZ    XIT                                                              
         XC    ESTOPTS,ESTOPTS     DISPLAY ESTIMATE OPTIONS                     
         OI    ESTOPTSH+6,X'80'                                                 
         MVI   ESTOPTSH+5,0                                                     
         OI    ESTOPTSH+4,X'20'    SET PREVIOUSLY VALIDATED                     
         XC    ESTOPT2,ESTOPT2     SAME FOR SECOND LINE                         
         OI    ESTOPT2H+6,X'80'                                                 
         MVI   ESTOPT2H+5,0                                                     
         OI    ESTOPT2H+4,X'20'                                                 
         SPACE 1                                                                
         XC    ELDATA,ELDATA                                                    
         MVC   ELDATA(6),TGPRD     SET ELEMENT DATA                             
         MVI   ELTYPE,TAESTPRD     AND ELEMENT TYPE FOR PRODUCT                 
         SPACE 1                                                                
         OC    TGPRD,TGPRD         IF NO PRODUCT DEFINED                        
         BNZ   *+14                                                             
         MVC   ELDATA(6),TGCLI     OPTIONS ARE ON CLIENT ELEMENT                
         MVI   ELTYPE,TAESTCLI     SET ELEMENT TYPE FOR CLIENT                  
         SPACE 1                                                                
         BAS   RE,LOOKUP           LOOK UP ENTRY                                
         BNE   XIT                                                              
         L     R4,FULL             R4=A(EL.) FROM LOOKUP                        
         SPACE 1                                                                
         LA    RF,ESTOPT2H-ESTOPTSH  DISPLAY OPTIONS                            
         GOTO1 AOPUNSCN,DMCB,ESTOPTSH,((RF),ESTOPT2H)                           
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINES TO HANDLE PROCESSING FOR ADD/CHANGE/DISPLAY             
         SPACE 1                                                                
PROCESS  DS    0H                                                               
         NI    STATUS2,ALL-KEYVALTD SET KEY NOT VALIDATED FOR NEXT TIME         
         SPACE 1                                                                
PROC2    GOTO1 AVALCP              DISPLAY (NEXT) CLIENT/PRODUCT                
         SPACE 1                                                                
         GOTO1 AINITSTR            INITIALIZE STORAGE IF NECESSARY              
         SPACE 1                                                                
         BAS   RE,DISPLAY          DISPLAY ESTIMATE RECORD HDR DETAILS          
         SPACE 1                                                                
         GOTO1 FLDVAL,DMCB,(X'20',ESTNAMEH),(X'80',999) SET FLDS VALID          
         SPACE 1                                                                
         CLI   ACTNUM,ACTDEL       IF ACTION IS DELETE                          
         BE    XIT                 THEN GO BACK TO CONTROLLER                   
         CLI   ACTNUM,ACTSEL       OR IF ACTION IS SELECT                       
         BNE   *+12                                                             
         CLI   THISLSEL,C'D'       AND SELECTING FOR DELETE                     
         BE    XIT                 THEN GO BACK TO CONTROLLER                   
         SPACE 1                                                                
         BAS   RE,CONTROL          PAGE THROUGH DETAIL DISPLAY                  
         BNE   NOMORE2             GIVE USER FINAL MESSAGE                      
         SPACE 1                                                                
         OC    NEXTCLI,NEXTCLI     IF WE HAVE A NEW CLIENT                      
         BZ    *+8                                                              
         MVI   ESTCLIH+5,0         SET TO RE-VALIDATE CLIENT                    
         SPACE 1                                                                
         MVI   ESTPRDH+5,0         SET WE HAVE NEW PRODUCT                      
         B     PROC2               THERE'S ANOTHER CLI/PRD TO DISPLAY           
         EJECT                                                                  
*              ROUTINE CONTROLS RECORD DISPLAY/CHANGE                           
CONTROL  NTR1                                                                   
         BAS   RE,INITREC          INITIALIZE NEW RECORD                        
         BNE   CNTL9D                                                           
         SPACE 1                                                                
         TM    STATUS,RESET        WE HAVE TO START OVER                        
         BZ    CNTL1                                                            
         TM    DISPMODE,HYPO+MISCADD  IF WE WERE IN HYPO OR MISC SCREEN         
         BZ    *+8                                                              
         BAS   RE,RESTSCRN         RESTORE PREVIOUS SCREEN                      
         B     CNTL10              AND BEGIN COMMERCIAL LIST                    
         SPACE 1                                                                
CNTL1    TM    STATUS,FINISHED     IF WE ALREADY TOLD USER WE'RE DONE           
         BO    CNTL7               THEN SKIP TO CHECK PFKEYS                    
         SPACE 1                                                                
         TM    DISPMODE,MISCADD    IF ON MISC. ADDITIONS SCREEN                 
         BZ    CNTL2                                                            
         GOTO1 AVALMISC            VALIDATE INPUT - ADD ELEMENTS                
         BAS   RE,CKMSCPFS         CHECK FOR PFKEY PRESENCE                     
         BM    CNTL9               RETURN TO COMM'L LIST                        
         BE    MISCOUT             OR DISPLAY MESSAGE                           
         B     CNTL4X              ELSE RETURN/CONT WITH COMML LIST             
         SPACE 1                                                                
CNTL2    TM    DISPMD2,SELCTHPF    IF IN HYPO PERFORMER SELECT MODE             
         BZ    CNTL2H                                                           
         BAS   RE,VALSHPRF         VALIDATE INPUT - ADD ELEMENTS                
         BAS   RE,CKSHPPFS         CHECK FOR PFKEY PRESENCE/CK FOR NEXT         
         B     HYPOOUT                                                          
         SPACE 1                                                                
CNTL2H   TM    DISPMODE,HYPO       IF IN HYPO MODE                              
         BZ    CNTL2S                                                           
         BAS   RE,VALHYPO          VALIDATE INPUT - ADD ELEMENTS                
         BAS   RE,CKHYPPFS         CHECK FOR PFKEY PRESENCE/CK FOR NEXT         
         BE    HYPOOUT             YES  - DISPLAY MESSAGE                       
         B     CNTL4A              ELSE - RETURN/CONT WITH COMML LIST           
         SPACE 1                                                                
CNTL2S   TM    DISPMD2,SELCTPRF    IF IN PERFORMER SELECT MODE                  
         BZ    CNTL3                                                            
         BAS   RE,VALSPERF         VALIDATE INPUT - ADD ELEMENTS                
         BAS   RE,CKSPFPFS         CHECK FOR PFKEY PRESENCE/CK FOR NEXT         
         BE    PERDSPLY            YES - DISPLAY MESSAGE                        
         B     CNTL3X              ELSE -RETURN TO SELECTABLE PERF LIST         
         SPACE 1                                                                
CNTL3    TM    DISPMODE,LISTPRFS   IF LISTING PERFORMERS                        
         BZ    CNTL3S                                                           
         BAS   RE,VALPERF          VALIDATE REGULAR PERF LIST                   
         BAS   RE,CKPERPFS         CHECK FOR PFKEY PRESENCE                     
         BM    CNTL9               RETURN TO COMM'L LIST                        
         BNE   CNTL4               EXECUTE NEXT COMMERCIAL                      
         CLI   PFAID,0             IF NO PFKEY PRESSED                          
         BNE   *+8                                                              
         BAS   RE,EXECPERF         EXECUTE RHS INPUT FOR PERFS ON PAGE          
         B     CNTL3X                                                           
         SPACE 1                                                                
CNTL3S   TM    DISPMD2,LISTSPRF    IF LISTING SELECTABLE PERFORMERS             
         BZ    CNTL4S                                                           
         BAS   RE,VALPSEL          THEN VALIDATE SELECTIONS                     
         BE    PERDSPLY            SOMETHING'S BEEN SELECTED                    
         BAS   RE,CKPERPFS         CHECK FOR PFKEY PRESENCE                     
         BM    CNTL4               RETURN TO SELECTED COMMERCIAL                
         SPACE 1                                                                
CNTL3X   OC    NXTCSTDA,NXTCSTDA   IF WE HAVE A STARTING D/A                    
         BZ    *+12                                                             
         BAS   RE,PEROUT           DISPLAY A PAGE OF PERFORMERS                 
         BE    PERDSPLY            PERFORMERS DISPLAYED                         
CNTL4    NI    DISPMODE,ALL-LISTPRFS                                            
         NI    DISPMD2,ALL-LISTSPRF                                             
CNTL4A   TM    DISPMODE,SELCTCOM   IF IN COMMERCIAL SELECT MODE                 
         BZ    CNTL4X                                                           
         BAS   RE,CKSCOPFS         CHECK FOR PFKEY PRESENCE/CK FOR NEXT         
         BE    COMDSPLY                                                         
         BAS   RE,LSTTOSCO         GO BACK TO SELECTED COMMERCIAL               
         BNE   CNTL4X                                                           
         BAS   RE,DISSCOM                                                       
         BE    COMDSPLY                                                         
         B     CNTL4X                                                           
         SPACE 1                                                                
CNTL4S   TM    DISPMODE,SELCTCOM   IF IN COMMERCIAL SELECT MODE                 
         BZ    CNTL5                                                            
         BAS   RE,VALSCOM          VALIDATE INPUT - ADD ELEMENTS                
         BAS   RE,CKSCOPFS         CHECK FOR PFKEY PRESENCE/CK FOR NEXT         
         BE    COMDSPLY            YES -DISPLAY MESSAGE                         
         SPACE 1                                                                
CNTL4X   CLI   NEWEST,C'Y'         IF NEW ESTIMATE DISPLAY                      
         BNE   CNTL8                                                            
         BAS   RE,THSCOMDA         DIG OUT THIS KEY                             
         OC    NXTCOMDA,NXTCOMDA   IF WE GOT BACK D/A OF COMMERCIAL             
         BZ    *+8                                                              
         NI    STATUS,ALL-ENDOFLST TURN OFF END OF LIST                         
         B     CNTL8               CONTINUE EXECUTING COMM'L SELECTIONS         
         SPACE 1                                                                
CNTL5    TM    DISPMODE,LISTHYP    IF LISTING HYPOTHETICAL COMM'LS              
         BZ    CNTL5S                                                           
         BAS   RE,VALSEL           VALIDATE HYPO. COMMERCIAL SELECTIONS         
         BE    HYPOOUT             SOMETHING'S BEEN SELECTED                    
         B     CNTL7                                                            
         SPACE 1                                                                
CNTL5S   TM    DISPMODE,LISTSCOM   IF LISTING SELECTABLE COMMERCIALS            
         BZ    CNTL6                                                            
         BAS   RE,VALSSEL          VALIDATE SELECTIONS                          
         BE    COMDSPLY            SOMETHING'S BEEN SELECTED                    
         B     CNTL7                                                            
         SPACE 1                                                                
CNTL6    BAS   RE,VALCOM           VALIDATE COMMERCIAL LIST                     
CNTL7    BAS   RE,CKCOMPFS         CHECK FOR PFKEY PRESENCE                     
         BE    CNTL9               CONTINUE WITH THEM                           
CNTL8    BAS   RE,EXECCOM          EXECUTE COMMERCIAL LIST                      
         SPACE 1                                                                
CNTL9    TM    STATUS,ENDOFLST     IF AT END OF LIST ALREADY,                   
         BZ    CNTL10                                                           
         BAS   RE,COMHEADS         INSURE WE HAVE COMML HEADS FOR HYPOS         
         MVC   HYCODATA,SAVEHCOM   SET START IF AROUND                          
         TM    STATUS2,REDISP      IF WE NEED TO RE-DISPLAY                     
         BZ    *+16                                                             
         XI    STATUS2,REDISP                                                   
         BAS   RE,GETHYCOM         GET THIS HYPO. COMMERCIAL                    
         B     *+8                                                              
         BAS   RE,NXTHYCOM         ELSE GET NEXT HYPO. COMMERCIAL               
         BNE   CNTL9D                                                           
         BAS   RE,LISTHCOM         LIST HYPOTHETICAL COMMERCIALS                
         BNE   HCOMDISP                                                         
         B     CNTL14                                                           
         SPACE 1                                                                
CNTL9D   BAS   RE,GETCLIPR         LOOK FOR ANOTHER CLIENT/PRODUCT              
         BE    YES                 RETURN TO DISPLAY NEXT                       
         BAS   RE,ANYMISC          ANY MISC. ADDITIONS TO DISPLAY               
         BNE   NO                  NO MORE TO DISPLAY                           
         BAS   RE,MISCINIT         YES - LOAD SCREEN                            
         BAS   RE,DISMISC          DISPLAY DETAILS                              
         B     MISCOUT             RETURN NOW                                   
         SPACE 1                                                                
CNTL10   NI    STATUS,ALL-RESETSTA RESET STATUS FOR NEW START                   
         MVI   DISPMD2,0           RESET DISPLAY MODE 2                         
         MVI   DISPMODE,LISTCOMS   RESET DISPLAY MODE                           
         CLI   NEWEST,C'Y'         IF NEW ESTIMATE DISPLAY                      
         BNE   CNTL10X                                                          
         MVI   DISPMODE,LISTSCOM                                                
         SPACE 1                                                                
CNTL10X  BRAS  RE,CLRSCRN          CLEAR SCREEN                                 
         BAS   RE,COMHEADS         INSURE WE HAVE COMMERCIAL HEADINGS           
         SPACE 1                                                                
         OC    STARTHYP,STARTHYP   DISPLAY ONLY HYPO. COMMERCIALS               
         BZ    CNTL11                                                           
         OI    STATUS,ENDOFLST     SET END OF COMMERCIAL LIST                   
         MVC   SAVEHCOM,STARTHYP   AND START OF HYPO COMMERCIALS                
         B     CNTL9                                                            
         SPACE 1                                                                
CNTL11   BAS   RE,LISTCOM          DISPLAY (NEXT) COMMERCIAL LIST               
         BE    COMDSPLY            MORE COMMERCIALS TO COME                     
         SPACE 1                                                                
         OI    STATUS,ENDOFLST     SET END OF LIST                              
         XC    HYCODATA,HYCODATA   SET TO START HYPO COMMERCIALS                
         BAS   RE,GETHYCOM         DO WE HAVE ANY HYPOTHETICAL COMM'LS          
         BNE   CNTL14                                                           
         OC    ESTLHS,ESTLHS       IS ANYTHING DISPLAYED                        
         BZ    CNTL9               NO - DISPLAY HYPOS NOW                       
         B     COMDHNXT            LET USER KNOW HYPOS COMING                   
         SPACE 1                                                                
CNTL14   BAS   RE,GETCLIPR         LOOK FOR ANOTHER CLIENT/PRODUCT              
         BNE   *+14                                                             
         XC    NEXTS,NEXTS         FOUND ONE - CLEAR NEXTS                      
         B     COMDNEXT            END OF COMM'L LIST - NEXT COMING             
         BAS   RE,ANYMISC          NO MORE - WILL THERE BY MISC ADDTION         
         BE    COMDMISC            YES - LET USER KNOW                          
         B     NOMORE              NO MORE TO DISPLAY                           
         EJECT                                                                  
*              ROUTINE INITIALIZES ESTIMATE RECORDS                             
         SPACE 1                                                                
INITREC  NTR1                                                                   
         MVC   AIO,AIO2            INITIALIZE RECORD IN IO2                     
         L     R3,AIO                                                           
         USING TLESD,R3                                                         
         MVC   TLESKEY,ESTKEY                                                   
         MVC   TLESLEN,DATADISP                                                 
         XC    TLESSTAT(10),TLESSTAT                                            
         SPACE 1                                                                
         BAS   RE,INITREV                      VALIDATE REVISION NUMBER         
         GOTO1 NAMIN,DMCB,TANAELQ,(X'80',ESTNAMEH)      ESTIMATE NAME           
         GOTO1 (RF),(R1),(1,TACMELQ),(X'80',ESTNARRH),TACMTYPG                  
**NO-OP* BAS   RE,INITNARR                                                      
         GOTO1 ACTVIN,DMCB,0                   ADD ACTIVITY ELEMENT             
         SPACE 1                                                                
         MVI   ELSEQ,0             INITIALIZE ELEMENT SEQUENCE NO.              
         ZAP   SUBELCNT,=P'0'      CLEAR N'SUB ELEMENTS                         
         MVC   ANXTSUB,ASUBELS     SET A(FIRST SUB-ELEMENT)                     
         XC    ELDATA,ELDATA                                                    
         MVC   ELDATA(6),TGCLI     SET CLIENT                                   
         MVI   ELTYPE,TAESTCLI     AND CLIENT ELEMENT TYPE                      
         OC    TGPRD,TGPRD         IF NO PRODUCT DEFINED                        
         BZ    INITR6              ATTACH PRODUCT OPTIONS TO CLIENT             
         BAS   RE,INESTEL          ELSE ADD CLIENT ESTIMATE EL. TO REC.         
         MVC   ELDATA(6),TGPRD     SET PRODUCT                                  
         MVI   ELTYPE,TAESTPRD     AND PRODUCT ELEMENT TYPE                     
         SPACE 1                                                                
INITR6   TM    STATUS2,BUILDING    IF CHANGING THE RECORD                       
         BZ    INITR8                                                           
         CLI   DISPMODE,SELCTCOM   AND IF COMMERCIAL SELECT MODE                
         BE    INITR7                                                           
         CLI   DISPMODE,LISTSCOM   OR IF LISTING SELECTABLE COMMLS              
         BE    INITR7                                                           
         TM    DISPMODE,LISTHYP    OR IF LISTING HYPO NEW WAY                   
         BZ    *+12                                                             
         CLI   NEWEST,C'Y'                                                      
         BE    INITR7                                                           
         CLI   DISPMODE,LISTCOMS   OR IF REGULAR COMMERCIAL LIST                
         BNE   INITR8                                                           
INITR7   BAS   RE,INITGDTL         VALIDATE GLOBAL DETAILS                      
         BNE   NO                                                               
         SPACE 1                                                                
INITR8   TM    ESTOPTSH+1,X'08'    IF OPTIONS FIELD HIGH INTENSITY              
         BZ    INITR9                                                           
         NI    ESTOPTSH+1,X'F3'    SET TO NORMAL INTENSITY                      
         OI    ESTOPTSH+6,X'80'                                                 
         NI    ESTOPT2H+1,X'F3'    SAME FOR SECOND LINE                         
         OI    ESTOPT2H+6,X'80'                                                 
         SPACE 1                                                                
INITR9   XC    OPTWORK(OPTWKLNQ),OPTWORK   CLEAR OPTION WORK AREAS              
         MVI   OPTOKBIT,OPOKPRD            SET OK BIT FOR OPTION VAL.           
         GOTO1 AOPSCAN,DMCB,ESTOPTSH   VALIDATE PRODUCT LEVEL OPTS              
         GOTO1 (RF),(R1),ESTOPT2H                                               
         OI    ESTOPTSH+4,X'20'    SET FIELDS PREVIOUSLY VALIDATED              
         OI    ESTOPT2H+4,X'20'                                                 
         BAS   RE,INESTEL          ADD ESTIMATE ELEMENT TO RECORD               
         SPACE 1                                                                
         TM    STATUS,DISPLYIT     IF NOT FIRST TIME FOR ESTIMATE               
         BO    INITRX                                                           
         TM    STATUS,RESET        AND WE HAVE TO RESET LIST                    
         BZ    INITRX                                                           
         OI    STATUS2,WRITETOP    SET TO WRITE TOP OF SCREEN ELS. ONLY         
         GOTO1 AWRITEIT            WRITE BACK ANY CHANGES SO FAR                
         XI    STATUS2,WRITETOP                                                 
INITRX   B     YES                                                              
         SPACE 2                                                                
*              ROUTINE TO INITIALIZE AND VALIDATE REVISION NUMBER               
         SPACE                                                                  
INITREV  NTR1                                                                   
         MVC   REVISION,SPACES                                                  
         LA    R2,ESTREVH          REVISION NUMBER IS OPTIONAL                  
         CLI   5(R2),0                                                          
         BE    INITREVX                                                         
         GOTO1 VALINUM                                                          
         UNPK  8(3,R2),DUB+6(2)    EDIT BACK TO SCREEN ZERO-FILLED              
         MVI   5(R2),3             SIMULATE INPUT LENGTH                        
         GOTO1 NAMIN,DMCB,TANUELQ,(R2),TANUTREV                                 
INITREVX B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE NARRATIVE FIELD                              
         SPACE                                                                  
*&&DO                                                                           
INITNARR NTR1                                                                   
         TM    DISPMD2,LISTSPRF    IF SELECTABLE PERFORMER LIST                 
         BO    *+12                                                             
         TM    DISPMD2,SELCTPRF    OR PERFORMER SELECT SCREEN                   
         BZ    INITNAR5                                                         
         OC    SVNARR,SVNARR       IF SAVED NARRATIVE                           
         BZ    INITNARX                                                         
         MVC   ELEMENT,SVNARR      ADD IT TO RECORD                             
         GOTO1 ADDELEM                                                          
         B     INITNARX                                                         
*                                  ELSE VALIDATE NARRATIVE SCREEN FIELD         
INITNAR5 GOTO1 (RF),(R1),(1,TACMELQ),(X'80',ESTNARRH),TACMTYPG                  
INITNARX B     XIT                                                              
*&&                                                                             
         SPACE 2                                                                
*              ROUTINE TO VALIDATE GLOBAL DETAILS FIELD                         
         SPACE 1                                                                
INITGDTL NTR1                                                                   
         CLI   DISPMODE,LISTCOMS   IF REGULAR COMMERCIAL LIST                   
         BE    INITGDT5            ALWAYS VALIDATE                              
         TM    ESTOPTSH+1,X'08'    ELSE IF WAITING FOR PFKEY HIT                
         BO    INITGDT5                                                         
         TM    ESTOPTSH+4,X'20'         OR IF FIELD CHANGED                     
         BZ    INITGDT2                                                         
         TM    ESTOPT2H+4,X'20'                                                 
         BO    INITGDTX                                                         
*                                                                               
INITGDT2 TM    DISPMODE,LISTHYP    IF LISTING HYPO NEW -'STAY ON SCR'           
         BZ    INITGDT5                                                         
         CLI   NEWEST,C'Y'                                                      
         BE    INITGDT7                                                         
*                                                                               
INITGDT5 CLC   =C'DELETE',ESTOPTS  TEST USER WANTS TO DELETE CLI/PRD            
         BNE   *+12                                                             
         BAS   RE,INITGDEL         VALIDATE 'DELETE' IN GLOBAL DETAILS          
         B     NO                                                               
         CLI   DISPMODE,LISTCOMS   IF REGULAR COMMERCIAL LIST                   
         BE    INITGDTX            ALWAYS PAGE                                  
*                                                                               
INITGDT7 MVI   PFAID,PFSTAY        ELSE, FIELD CHANGED - 'STAY ON SCRN'         
INITGDTX B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE 'DELETE' IN GLOBAL DETAILS FIELD             
         SPACE                                                                  
INITGDEL NTR1                                                                   
         TM    TRNSTAT2,CTFLSTER   IF FULL SESSION STEREO                       
         BZ    *+12                                                             
         MVI   PFAID,PFDELPRD      OVERRIDING PFKEY NOT NECESSARY               
         B     INITGD5                                                          
         SPACE 1                                                                
         TM    ESTOPTSH+1,X'08'    TEST CLI/PRD OPTIONS HIGH INTENSTY           
         BZ    PFTODEL                                                          
         CLI   PFAID,PFDELPRD      YES - DID THEY HIT PFKEY                     
         BNE   PFTODEL                                                          
INITGD5  GOTO1 AWRITEIT            WRITE BACK CHANGED RECORD                    
         SPACE 1                                                                
         XC    ESTOPTS,ESTOPTS     CLEAR CLI/PRD OPTIONS                        
         NI    ESTOPTSH+1,X'F3'    SET TO NORMAL INTENSITY                      
         OI    ESTOPTSH+6,X'80'                                                 
         XC    ESTOPT2,ESTOPT2     SAME FOR SECOND LINE                         
         NI    ESTOPT2H+1,X'F3'                                                 
         OI    ESTOPT2H+6,X'80'                                                 
         SPACE 1                                                                
         XC    ESTPRD,ESTPRD       MAKE PRODUCT APPEAR EMPTY                    
         MVI   ESTPRDH+5,0                                                      
         OI    ESTPRDH+6,X'80'                                                  
         XC    ESTPRDN,ESTPRDN                                                  
         OI    ESTPRDNH+6,X'80'                                                 
         SPACE 1                                                                
         TM    STATUS3,CLIDELD     IF CLIENT DELETED AS WELL                    
         BZ    INITGDX                                                          
         XI    STATUS3,CLIDELD                                                  
         XC    ESTCLI,ESTCLI       MAKE CLIENT APPEAR EMPTY                     
         MVI   ESTCLIH+5,0                                                      
         OI    ESTCLIH+6,X'80'                                                  
         XC    ESTCLIN,ESTCLIN                                                  
         OI    ESTCLINH+6,X'80'                                                 
INITGDX  B     XIT                                                              
         EJECT                                                                  
*              ROUTINE LOOKS FOR NEXT CLIENT/PRODUCT ELEMENTS                   
         SPACE 1                                                                
GETCLIPR NTR1                                                                   
         XC    NEXTS,NEXTS         PRE-CLEAR NEXT FIELDS                        
         SPACE 1                                                                
         TM    STATUS2,HAVEREC     DO WE HAVE AN EXISTING RECORD                
         BZ    NO                                                               
         SPACE 1                                                                
         MVI   ELTYPE,TAESTPRD     LOOK FOR ANOTHER PRODUCT                     
         XC    ELDATA,ELDATA                                                    
         MVC   ELDATA(6),TGPRD     USE CURRENT PRODUCT                          
         ZIC   R1,ELDATA+5                                                      
         AH    R1,=H'1'            TO LOOK FOR NEXT                             
         STC   R1,ELDATA+5                                                      
         SPACE 1                                                                
         BAS   RE,LOOKUP           LOOK FOR NEXT ELEMENT                        
         L     R4,FULL                                                          
         USING TAESD,R4                                                         
         CLI   TAESEL,TAESELQ      END-OF-RECORD                                
         BNE   NO                                                               
         CLI   TAESTYPE,TAESTPRD   DID WE GET BACK ANOTHER PRODUCT              
         BH    NO                                                               
         BL    *+14                                                             
         MVC   NEXTPRD,TAESPRD     YES - SAVE IT FOR NEXT                       
         B     GCPX                                                             
         SPACE 1                                                                
         MVC   NEXTCLI,TAESCLI     THIS IS NEW CLIENT - SAVE IT                 
         MVC   NEXTPRD,=CL6'ALL'   SET DEFAULT - ALL PRODUCTS                   
         SPACE 1                                                                
         BAS   RE,NEXTEL           AND LOOK FOR PRODUCT                         
         BNE   GCPX                                                             
         CLI   TAESTYPE,TAESTPRD   IF WE HAVE ANOTHER PRODUCT                   
         BNE   GCPX                                                             
         MVC   NEXTPRD,TAESPRD     SAVE IT                                      
         SPACE 1                                                                
GCPX     B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO HANDLE LISTING COMMERCIALS                            
         SPACE 1                                                                
         USING LINED,R2                                                         
         USING TLCOD,R3                                                         
LISTCOM  NTR1                                                                   
         MVI   ELTYPE,TAESTCOM     SET COMMERCIAL ESTIMATE EL. TYPE             
         XC    ELDATA,ELDATA                                                    
         SPACE 1                                                                
         OC    NXTCOMDA,NXTCOMDA   GET OUT IF WE DON'T HAVE NEXT D/A            
         BZ    NO                                                               
         LA    R3,KEY              R3=A(KEY)                                    
         USING TLDRD,R3                                                         
         MVC   TLDRDA,NXTCOMDA     SET D/A OF FIRST COMML TO DISPLAY            
         GOTO1 GETREC              GET THE RECORD                               
         L     R1,AIO                                                           
         USING TLCOD,R3                                                         
         MVC   TLCOKEY,0(R1)       SET FIRST KEY                                
         SPACE 1                                                                
         LA    R2,ESTDATAH         R2=A(FIRST DATA LINE)                        
         LA    R0,NLINEQU          R0=N'SCREEN LINES                            
         SPACE 1                                                                
         CLI   ACTNUM,ACTADD       IF WE'RE NOT ADDING THE RECORD               
         BE    LSTC1                                                            
         TM    FILTER1,FLACTV+FLINCL AND WE'RE NOT OVERRIDING WHAT'S            
         BNZ   LSTC1               ON THE RECORD ALREADY                        
         BAS   RE,COMEXIST         DISPLAY ONLY EXISTING COMMERCIALS            
         B     XIT                 (RETURNS CC)                                 
         SPACE 1                                                                
LSTC1    GOTO1 HIGH                RE-READ LAST DISPLAYED KEY                   
         TM    STATUS2,REDISP      SHOULD WE DISPLAY FROM LAST SELECTED         
         BO    LSTC4                                                            
         SPACE 1                                                                
LSTC2    GOTO1 SEQ                 GET NEXT                                     
         SPACE 1                                                                
         USING TLCOD,R3                                                         
LSTC4    LA    R1,TLCOCID-TLCOD-1  SAME CLIENT/PRODUCT                          
         OC    TGPRD,TGPRD         IF NO PRODUCT DEFINED                        
         BNZ   *+8                                                              
         LA    R1,TLCOPRD-TLCOD-1  SAME CLIENT                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TLCOKEY(0),KEYSAVE  TEST STILL HAVE GOOD KEY                     
         BNE   NO                                                               
         TM    TLDRSTAT-TLDRD(R3),X'40' IGNORE COPIED COMMLS                    
         BO    LSTC2                                                            
         GOTO1 GETREC                                                           
         MVC   ELDATA(8),TLCOCID   SET ESTIMATE EL. DATA KEY                    
         MVC   ELDATA+8(4),TLCOCOM                                              
         XC    ATHSEST,ATHSEST                                                  
         BAS   RE,LOOKUP           LOOK UP ENTRY                                
         BNE   *+14                                                             
         MVC   ATHSEST,FULL        IN EST - SAVE A(THIS ESTIMATE EL.)           
         B     LSTC6               SKIP ACTIVE CHECK FOR INCLUDED RECS.         
         SPACE 1                                                                
         MVI   ELCODE,TACOELQ                                                   
         L     R4,AIO                                                           
         BAS   RE,GETEL            GET COMMERCIAL DETAILS ELEMENT               
         BNE   LSTC2                                                            
         USING TACOD,R4                                                         
         CLI   MUSIC,C'N'          IF EXCLUDING MUSIC COMMLS                    
         BNE   *+12                                                             
         CLI   TACOTYPE,CTYMUS     IGNORE MUSIC COMMERCIALS                     
         BE    LSTC2                                                            
         CLI   RELEASED,C'Y'       UNLESS EXPRESSLY REQUESTED OTHERWISE         
         BE    *+12                                                             
         TM    TACOSTAT,TACOSTRL   IGNORE RELEASED COMMERCIALS                  
         BO    LSTC2                                                            
         CLI   LOCKED,C'Y'         UNLESS EXPRESSLY REQUESTED OTHERWISE         
         BE    *+12                                                             
         TM    TACOSTAT,TACOSTLO   IGNORE LOCKED COMMERCIALS                    
         BO    LSTC2                                                            
         CLI   ACTNUM,ACTADD       IF WE'RE NOT ADDING                          
         BE    *+12                                                             
         CLI   INCLUDED,C'N'       NEED FILTER TO DISPLAY NON-INCL RECS         
         BNE   LSTC2                                                            
         SPACE 1                                                                
         OC    TACOPDTE,TACOPDTE   IF LAST PAID DATE                            
         BZ    *+18                                                             
         CLC   TACOPDTE,ACTVDATE   CHECK IT AGAINST ACTIVITY DATE               
         BL    LSTC2                                                            
         B     LSTC6                                                            
         SPACE 1                                                                
         MVI   ELCODE,TAACELQ      ELSE GET LAST ACTIVITY ELEMENT               
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   LSTC2                                                            
         USING TAACD,R4                                                         
         CLC   TAACCDTE,ACTVDATE   ACTIVITY SINCE LAST ACTIVE DATE              
         BL    LSTC2               NO, SO SKIP THIS ONE                         
         SPACE 1                                                                
LSTC6    BAS   RE,OUTCOM           DISPLAY THIS COMMERCIAL                      
         BE    LSTC2               OK TO KEEP ON GOING                          
         B     YES                 NO MORE ROOM                                 
         EJECT                                                                  
*              ROUTINE TO DISPLAY EXISTING COMMERCIAL ELEMENTS                  
         SPACE 1                                                                
*                                  R0=N'SCREEN LINES                            
*                                  R2=A(FIRST LINE)                             
         USING TLCOD,R3            R3=A(START KEY)                              
COMEXIST NTR1                                                                   
         MVC   ELDATA(8),TLCOCID   SET STARTING COMMERCIAL IF AROUND            
         MVC   ELDATA+8(4),TLCOCOM                                              
         OI    STATUS3,LOOKCID     SET TO LOOK UP VIA CID                       
         BAS   RE,LOOKUP           RETURNS A(EST. EL. OR NEXT EL.)              
         BE    *+8                                                              
         OI    STATUS2,REDISP      SET TO NOT SKIP IF PREV NOT FOUND            
         L     R4,FULL             R4=A(NEXT ESTIMATE EL. TO DISPLAY)           
         USING TAESD,R4                                                         
         TM    STATUS2,REDISP      IF WE'RE RE-DISPLAYING LAST                  
         BO    COMEX4              USE THIS ONE                                 
         SPACE 1                                                                
COMEX2   LR    R3,R4               SET NEW STARTING POINT                       
         OI    STATUS3,LOOKCID     SET TO LOOK UP VIA CID                       
         BAS   RE,LOOKUP3          LOOK FOR ANOTHER COMMERCIAL                  
         XI    STATUS3,LOOKCID                                                  
         LR    R4,R3               RETURNS R3=A(ELEMENT)                        
         SPACE 1                                                                
COMEX4   CLI   TAESEL,TAESELQ      FINISHED IF REACHED EOR                      
         BNE   NO                                                               
         CLI   TAESTYPE,TAESTCOM   OR IF THIS ISN'T A COMMERCIAL                
         BNE   NO                                                               
         LA    R3,KEY              READ COMMERCIAL VIA INTERNAL NUMBER          
         XC    KEY,KEY                                                          
         USING TLCOPD,R3                                                        
         MVI   TLCOPCD,TLCOCCDQ    RECORD CODE                                  
         MVC   TLCOCCOM,TAESCOM    INTERNAL COMMERCIAL NUMBER                   
         SPACE 1                                                                
         GOTO1 HIGH                                                             
         CLC   TLCOPKEY,KEYSAVE    SHOULD BE ON FILE                            
         BNE   COMEX2                                                           
         GOTO1 GETREC                                                           
         L     R1,AIO              SET ACTIVE KEY IN KEY                        
         USING TLCOD,R3                                                         
         MVC   TLCOKEY,0(R1)                                                    
         SPACE 1                                                                
         ST    R4,ATHSEST          SET A(THIS ESTIMATE EL.)                     
         SPACE 1                                                                
         BAS   RE,OUTCOM           DISPLAY FOR USER                             
         BE    COMEX2              AND LOOK FOR ANOTHER                         
         B     YES                 ELSE NO MORE ROOM                            
         EJECT                                                                  
*              ROUTINE TO HANDLE DISPLAYING A COMMERCIAL                        
         SPACE 1                                                                
         USING TLCOD,R3            R3=A(KEY)                                    
OUTCOM   NTR1                                                                   
         CLM   R0,1,CLINES         RETURN IF NOT ENOUGH LINES LEFT              
         BL    NO                                                               
         BAS   RE,DISCOM           DISPLAY COMMERCIAL                           
         BM    YES                 SKIP THIS ONE                                
         BNE   NO                  NO MORE ROOM                                 
         BAS   RE,SETPGINF         SET CURRENT PAGE INFO.                       
         SPACE 1                                                                
         ZIC   RE,THSLINES         RE=N'LINES USED                              
         ZIC   R1,CLINES           R1=N'LINES REQUIRED                          
         CR    RE,R1                                                            
         BNL   *+6                                                              
         LR    RE,R1               RE=N'LINES TO SKIP                           
         SR    R0,RE               DECREASE N'LINES REMAINING                   
         MH    RE,=AL2(LINLNQ)                                                  
         AR    R2,RE               BUMP TO NEXT AVAILABLE LINE                  
         CR    RE,RE               SET CC EQUAL                                 
         XIT1  REGS=(R0,R2)        RETURN N'LINES LEFT, A(NEXT LINE)            
         EJECT                                                                  
*              ROUTINE TO DISPLAY A COMMERCIAL                                  
         SPACE 1                                                                
         USING LINED,R2            R2=A(LINE)                                   
         USING TLCOD,R3            R3=A(KEY)                                    
DISCOM   NTR1                                                                   
         NI    STATUS2,ALL-REDISP  TURN OFF RE-DISPLAY INDICATOR                
         MVC   TGCOM,TLCOCOM       SAVE INTERNAL NUMBER                         
         SPACE 1                                                                
         L     R4,AIO              AIO=A(RECORD)                                
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL            GET COMMERCIAL DETAILS ELEMENT               
         BNE   LOWX                                                             
         SPACE 1                                                                
         USING TACOD,R4                                                         
         TM    FILTER1,FLMEDIA     DO WE HAVE MEDIA FILTER(S)                   
         BZ    DISC2                                                            
         LA    R1,MEDIAFLT         MUST SATISFY AT LEAST ONE FILTER             
         LA    R0,NMEDIA                                                        
DISC0    CLI   0(R1),0                                                          
         BE    LOWX                                                             
         CLI   1(R1),ALLBUT        ALL BUT                                      
         BNE   DISC1                                                            
         CLC   TACOMED(1),0(R1)                                                 
         BNE   DISC2                                                            
         B     *+14                                                             
DISC1    CLC   TACOMED(1),0(R1)                                                 
         BE    DISC2                                                            
         LA    R1,L'MEDIAFLT(R1)                                                
         BCT   R0,DISC0                                                         
         B     LOWX                                                             
         SPACE 1                                                                
DISC2    BAS   RE,DISCOMLN         DISPLAY COMMERCIAL                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY SELECTED COMMERCIAL                           
         SPACE 1                                                                
DISSCOM  NTR1                                                                   
         OI    DISPMODE,SELCTCOM   SET WE'RE IN COMMERCIAL SELECT MODE          
         BRAS  RE,CLRSCRN          CLEAR THE SCREEN                             
         BAS   RE,COMHEADS         INSURE WE HAVE CORRECT COMML HEADS           
         XC    ATHSEST,ATHSEST     CLEAR A(EST EL.)                             
         SPACE 1                                                                
         L     R4,ATIA             R4=A(ESTIMATE RECORD)                        
         MVI   ELCODE,TAESELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DISSC10  BAS   RE,NEXTEL                                                        
         BE    DISSC20                                                          
*                                  ESTIMATE EL NOT FOUND                        
         CLI   ACTNUM,ACTADD       IF ADDING ESTIMATE                           
         BE    DISSC30             THIS IS POSSIBLE                             
         CLI   INCLUDED,C'N'       IF DISPLAYING NOT INCLUDED COMMLS            
         BE    DISSC30             THIS IS POSSIBLE                             
         B     NO                                                               
         SPACE 1                                                                
         USING TAESD,R4            R4=A(ESTIMATE EL.)                           
DISSC20  CLI   TAESTYPE,TAESTCOM                                                
         BNE   DISSC10                                                          
         CLC   TAESCOM,TGCOM       MATCH ON INTERNAL COMMERCIAL NUMBER          
         BNE   DISSC10                                                          
         ST    R4,ATHSEST          SAVE A(EST EL.)                              
DISSC30  LA    R2,ESTDATAH         R2=A(TOP LINE)                               
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL            R4=A(COMMERCIAL DETAILS EL.)                 
         BNE   NO                                                               
         BAS   RE,DISCOMLN         DISPLAY COMMERCIAL                           
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY INFORMATION FOR ONE COMMERCIAL                
         SPACE 1                                                                
*                                  AIO=A(COMML),ATHSEST=A(COMML EST EL)         
*                                  R2=A(COMMERCIAL DISPLAY LINE)                
         USING TACOD,R4            R4=A(COMMERCIAL DETAILS EL.)                 
DISCOMLN NTR1                                                                   
         MVI   THSLINES,1          INITIALIZE N'LINES USED                      
         MVC   TGCID,TACOCID       SAVE AND                                     
         MVC   LINCID,TACOCID      DISPLAY COMMERCIAL ID                        
         MVC   LINMEDIA,TACOMED    DISPLAY MEDIA                                
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         MVC   LINNAME,TGNAME      DISPLAY TITLE TO SCREEN                      
*                                                                               
**NO-OP* MVI   WRKBYTE,0           CLEAR WORK BYTE                              
*        TM    DISPMODE,SELCTCOM   IF COMMERCIAL SELECT SCREEN                  
*        BZ    *+12                                                             
*        LA    R3,LINCID+LINLNQ    DISPLAY ADDL COMM INFO ON 2ND LINE           
**NO-OP* B     DISCLN2                                                          
         TM    DISPMODE,LISTSCOM   IF SELECTABLE COMMERCIAL LIST                
         BZ    DISCLN4                                                          
         NI    LINCODEH+1,X'DE'    UNPROTECT SELECT FIELD                       
         LA    R3,LINRHS           DISPLAY ADDL COMM INFO ON RHS                
DISCLN2  BAS   RE,DISCINFO                                                      
         B     DISCLN5X                                                         
*                                                                               
DISCLN4  CLI   EXPIRY,C'Y'         IF NEED TO DISPLAY EXPIRATION DATES          
         BNE   DISCLN5L                                                         
         OC    TACOEXP,TACOEXP     AND IF IT EXISTS                             
         BZ    DISCLN5L                                                         
         BAS   RE,TSTROOM          ELSE, CHECK IF THERE'S NO ROOM               
         BNE   NO                  GET OUT                                      
         LA    R3,LINLNQ+LINNAME-LINED(R2) SET TO DISPLAY IT                    
         MVC   0(L'LTEXP,R3),LTEXP         DISPLAY EXPIRATION DATE              
         GOTO1 DATCON,DMCB,(1,TACOEXP),(8,L'LTEXP(R3))                          
DISCLN5L TM    DISPMODE,SELCTCOM   IF NOT COMMERCIAL SELECT SCREEN              
         BO    DISCLN5X                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TALFELQ      LOOK FOR LIFT DETAILS ELEMENT                
         BAS   RE,GETEL                                                         
         BNE   DISCLN5A                                                         
         USING TALFD,R4                                                         
         MVC   LINNAME+L'LINNAME-3(3),=C'(L)' FLAG LIFT ON COMM'L               
         B     DISCLN5X                                                         
*                                                                               
         USING TAVRD,R4                                                         
DISCLN5A L     R4,AIO              R4=A(COMMERCIAL RECORD)                      
         MVI   ELCODE,TAVRELQ      READ VERSION ELEMENTS                        
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
DISCLN5B BRAS  RE,NEXTEL                                                        
         BNE   DISCLN5X                                                         
         CLI   TAVRVERS,2          IF ELEMENT FOR VERSION 2                     
         BNE   DISCLN5B            IS FOUND ...                                 
         MVC   LINNAME+L'LINNAME-3(3),=C'(L)' FLAG LIFT ON COMM'L               
         DROP  R4                                                               
*                                                                               
DISCLN5X ICM   R4,15,ATHSEST       IF WE DON'T HAVE EST. EL. ALREADY            
         BNZ   DISCLN6                                                          
         CLI   ALLYES,C'Y'         AND USER WANTS ALL=Y(ES)                     
         BNE   *+8                                                              
         MVI   LINRHS,C'Y'         MOVE IT TO SCREEN                            
         B     DISCLNX                                                          
         SPACE 1                                                                
         USING TAESD,R4            R4=A(ESTIMATE ELEMENT)                       
DISCLN6  CLC   TAESCID,TGCID       IF CID HAS CHANGED                           
         BE    *+8                                                              
         MVI   LINMOVE,C'*'        MARK DISPLAY THAT COMML WILL MOVE            
         SPACE 1                                                                
         NI    STATUS2,ALL-DOPPERF TURN OFF DISPLAYED OPPERF/OPADDL             
         CLI   DISPMODE,LISTSCOM                                                
         BE    DISCLN8                                                          
         LA    RF,LINLNQ           SHOW COMMERCIAL DETAILS ON RHS               
         GOTO1 AOPUNSCN,DMCB,LINRHSH,((RF),ESTLSTRH) DISP. OPTIONS              
         BNE   DISCLNNO            NOT ENOUGH ROOM                              
         SPACE 1                                                                
DISCLN8  CLI   LINMOVE,C'*'        IF WILL MOVE COMMERCIAL                      
         BNE   DISCLNX                                                          
         TM    STATUS2,BUILDING+DOPPERF   AND CHANGING REC & HAVE PERFS         
         BNO   DISCLNX                                                          
         MVC   LINNAME-1(L'WARNMSG),WARNMSG DISPLAY WARNING                     
         OI    LINDATAH+1,X'08'    SET TO HIGH INTENSITY                        
         SPACE 1                                                                
DISCLNX  B     YES                                                              
DISCLNNO B     NO                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY COMMERCIAL INFO                               
**NO-OP*       RHS IF LISTSCOM AND 2ND LINE IF SELCTCOM                         
         USING LINCOMD,R3          R3=A(SCREEN DSECT LINE)                      
         USING TACOD,R4            R4=A(COMMERCIAL DETAILS ELEMENT)             
         SPACE 1                                                                
DISCINFO NTR1                                                                   
         EDIT  TACOSEC,(3,LINCSEC),ALIGN=LEFT  SHOW LENGTH                      
         GOTO1 CTYPVAL,DMCB,TACOTYPE                                            
         BNE   *+10                                                             
         MVC   LINCTYPE,TGCTNAME               COMMERCIAL TYPE                  
         OC    TACOEXP,TACOEXP                EXPIRATION DATE                   
         BZ    DISCI10                                                          
         LA    R2,LINCEXP          R2=A(EXPIRATION FIELD)                       
**NO-OP* CLI   WRKBYTE,C'E'        IF CHANGE DISPLACEMENT OF EXPDT              
*        BNE   *+6                                                              
**NO-OP* BCTR  R2,0                DISPLAY ONE SPACE BACK                       
         GOTO1 DATCON,DMCB,(1,TACOEXP),(8,(R2))                                 
*                                                                               
DISCI10  L     R4,AIO                                                           
         MVI   ELCODE,TALFELQ      LOOK FOR LIFT DETAILS ELEMENT                
         BAS   RE,GETEL                                                         
         BNE   DISCI20                                                          
         USING TALFD,R4                                                         
         MVC   LINCLID,TALFLID      SHOW LIFT ID AND LIFT LENGTH                
         EDIT  TALFSEC,(3,LINCLSEC),ALIGN=LEFT                                  
         B     DISCI30                                                          
DISCI20  BRAS  RE,DISCIVER         OR VERSION DETAILS ELEMENT                   
*                                                                               
DISCI30  OC    ATHSEST,ATHSEST     IF WE HAVE EST. EL.                          
         BZ    *+8                                                              
         MVI   LINCDTLS,C'Y'       SET COMMERCIAL DETAILS                       
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY PERFORMER LIST                                
         SPACE 1                                                                
         USING SELD,R4             R4=A(CSELTAB ENTRY)                          
LISTPER  NTR1                                                                   
         LA    R3,KEY                                                           
         USING TLDRD,R3                                                         
         MVC   TLDRDA,SELDA        SET D/A OF RECORD                            
         GOTO1 GETREC              GET IT                                       
         MVC   DUB,TGPRD                                                        
         GOTO1 EXTRACT             EXTRACT GLOBAL VALUES                        
         MVC   TGPRD,DUB                                                        
         GOTO1 MEDVAL,DMCB,SELMEDIA  SET MEDIA                                  
         MVI   SELRHS,0            CLEAR THIS ENTRY                             
         SPACE 1                                                                
         XC    PSELTAB,PSELTAB     CLEAR SELECT TABLE                           
         MVI   PGCSTIND,0                                                       
         XC    NXTCSTDA,NXTCSTDA   SET TO START NEW CAST LIST                   
         SPACE 1                                                                
**NO-OP* CLI   NEWEST,C'Y'         IF NEW ESTIMATE DISPLAY                      
*        BNE   LISTPER5                                                         
*        SPACE 1                                                                
*        XC    ESTNARR,ESTNARR     CLEAR NARRATIVE FOR COMML INFO               
*        MVI   ESTNARRH+5,L'ESTNARR                                             
*        OI    ESTNARRH+1,X'20'    PROTECT IT                                   
*        OI    ESTNARRH+6,X'80'    AND TRANSMIT IT                              
*        SPACE 1                                                                
*        L     R4,AIO              DISPLAY COMML INFO IN NARRATIVE              
*        MVI   ELCODE,TACOELQ                                                   
*        BAS   RE,GETEL                                                         
*        BNE   LISTPER5                                                         
*        SPACE 1                                                                
*        USING TACOD,R4            R4=A(COMMERCIAL DETAILS ELEMENT)             
*        LA    R3,ESTNARR          R3=A(NARRATIVE)                              
*        MVC   0(L'LINMEDIA,R3),TACOMED                                         
*        LA    R3,L'LINMEDIA(R3)                                                
*        GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
*        MVC   0(L'LINNAME,R3),TGNAME                                           
*        LA    R3,L'LINNAME(R3)                                                 
*        MVI   WRKBYTE,C'E'        DISPLAY EXPDT ONE SPACE BACK                 
*        BAS   RE,DISCINFO         DISPLAY COMMERCIAL INFO                      
*                                                                               
LISTPER5 BAS   RE,PEROUT           *** DISPLAY A PAGE OF PERFORMERS ***         
         BE    YES                                                              
         B     NO                                                               
         EJECT                                                                  
*              ROUTINE DISPLAYS A PAGE OF PERFORMERS                            
         SPACE 1                                                                
PEROUT   NTR1                                                                   
         CLI   NEWEST,C'Y'         IF NOT NEW ESTIMATE DISPLAY                  
         BE    *+12                                                             
         OI    DISPMODE,LISTPRFS   SET WE'RE LISTING PERFORMERS                 
         B     *+8                                                              
         OI    DISPMD2,LISTSPRF    ELSE,WE'RE LISTING SELECTABLE PERFS          
         BRAS  RE,CLRSCRN          CLEAR BOTTOM HALF OF SCREEN                  
         BAS   RE,PERHEADS         INSURE PERFORMER LIST HEADINGS               
         NI    DISPMODE,ALL-LISTPRFS TURN OFF PERFORMER LIST STATUS             
         NI    DISPMD2,ALL-LISTSPRF                                             
         SPACE 1                                                                
         MVI   ELTYPE,TAESTPER     SET PERFORMER ESTIMATE EL. TYPE              
         XC    ELDATA,ELDATA                                                    
         SPACE 1                                                                
         XC    KEY,KEY             BUILD INITIAL CAST RECORD KEY                
         LA    R3,KEY              R3=A(CAST RECORD KEY)                        
         USING TLCAD,R3                                                         
         MVI   TLCACD,TLCACDQ      RECORD CODE                                  
         MVC   TLCACOM,TGCOM       INTERNAL COMMERCIAL NUMBER                   
         SPACE 1                                                                
         OC    NXTCSTDA,NXTCSTDA   DO WE HAVE STARTING D/A                      
         BZ    PERO1                                                            
         USING TLDRD,R3                                                         
         MVC   TLDRDA,NXTCSTDA     SET D/A OF RECORD                            
         GOTO1 GETREC              GET IT                                       
         L     R1,AIO                                                           
         USING TLCAD,R3                                                         
         MVC   TLCAKEY,0(R1)       SET KEY FOR THIS RECORD                      
         SPACE 1                                                                
PERO1    GOTO1 HIGH                GET FIRST DIRECTORY RECORD                   
         LA    R2,ESTDATAH         R2=A(FIRST LINE)                             
         LA    R0,NLINEQU          R0=N'SCREEN LINES                            
         SPACE 1                                                                
PERO2    CLC   TLCAKEY(TLCASORT-TLCAD),KEYSAVE  TEST STILL SAME COMM'L          
         BE    *+14                                                             
         XC    NXTCSTDA,NXTCSTDA   NO - CLEAR D/A AND EXIT                      
         B     PERO6                                                            
         GOTO1 GETREC              GET THE RECORD                               
         MVC   NXTCSTDA,DMDSKADD   SAVE THIS D/A                                
         SPACE 1                                                                
         CLM   R0,1,PLINES         RETURN IF NOT ENOUGH LINES LEFT              
         BL    PERO6                                                            
         BAS   RE,DISPER           DISPLAY PERFORMER DETAILS                    
         BNE   PERO6                                                            
         CLI   NEWEST,C'Y'         IF NOT NEW ESTIMATE DISPLAY                  
         BE    *+12                                                             
         OI    DISPMODE,LISTPRFS   SET WE'VE LISTED PERFORMERS                  
         B     *+8                                                              
         OI    DISPMD2,LISTSPRF    ELSE,WE'VE LISTED SELECTABLE PERFS           
         SPACE 1                                                                
         BAS   RE,SETPGINF         SET CURRENT PAGE INFO.                       
         SPACE 1                                                                
         ZIC   RE,THSLINES         RE=N'LINES USED                              
         ZIC   R1,PLINES           R1=N'LINES REQUIRED                          
         CR    RE,R1                                                            
         BNL   *+6                                                              
         LR    RE,R1               RE=N'LINES TO SKIP                           
         SR    R0,RE               DECREASE N'LINES REMAINING                   
         MH    RE,=AL2(LINLNQ)                                                  
         AR    R2,RE               BUMP TO NEXT AVAILABLE LINE                  
         SPACE 1                                                                
         GOTO1 SEQ                 READ NEXT DIRECTORY POINTER                  
         B     PERO2               AND PROCESS                                  
         SPACE 1                                                                
PERO6    TM    DISPMODE,LISTPRFS   IF PERFORMERS DISPLAYED                      
         BO    YES                 RETURN CC EQUAL                              
         TM    DISPMD2,LISTSPRF                                                 
         BO    YES                                                              
         B     NO                  ELSE CONTINUE CHECKING COMM'L LIST           
         EJECT                                                                  
*              ROUTINE TO DISPLAY INFORMATION FOR ONE PERFORMER                 
         SPACE 1                                                                
         USING LINED,R2            R2=A(CURRENT LINE)                           
DISPER   NTR1                                                                   
         GOTO1 EXTRACT             EXTRACT GLOBAL VALUES FROM RECORD            
         MVI   THSLINES,1          INITIALIZE N'LINES USED                      
         MVC   LINSSN,TGSSN        DISPLAY S/S NUMBER                           
         TM    TGSYSTAT,TASYSPID   USING PID?                                   
         BZ    DPER1                                                            
         MVC   LINSSN,SPACES                                                    
         GOTO1 SSNPACK,DMCB,TGSSN,LINSSN    CONVERT SSN TO PID                  
         SPACE 1                                                                
DPER1    LA    R4,ELDATA           SET EL. KEY DATA                             
         USING TAESDATA,R4                                                      
         MVC   TAESSORT,TGCSORT    CAST SORT KEY                                
         PACK  DUB,TGSSN                                                        
         CVB   R1,DUB                                                           
         STCM  R1,15,TAESSSN       SOCIAL SECURITY NUMBER                       
         DROP  R4                                                               
         SPACE 1                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ      GET CAST DETAILS ELEMENT                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R4            R4=A(CAST DETAILS ELEMENT)                   
         SPACE 1                                                                
         MVI   WRKBYTE,0           CLEAR LIFT STATUS                            
         TM    TACASTAT,TACASTLF   TEST ON LIFT                                 
         BZ    DPER2                                                            
         MVI   WRKBYTE,C'L'                                                     
         TM    TACASTAT,TACASTLO   TEST ONLY ON LIFT                            
         BZ    DPER2                                                            
         MVI   WRKBYTE,C'O'                                                     
         SPACE 1                                                                
DPER2    DS    0H                                                               
**NO-OP* TM    DISPMD2,SELCTPRF    IF PERFORMER SELECT SCREEN                   
*        BZ    *+12                                                             
*        LA    R3,LINLNQ+LINSSN    DISPLAY ADDL PERF INFO ON 2ND LINE           
**NO-OP* B     DPER4                                                            
         CLI   NEWEST,C'Y'         IF NEW ESTIMATE DISPLAY                      
         BNE   DPER5                                                            
         TM    DISPMD2,SELCTPRF    AND SELECTABLE PERFORMER LIST                
         BO    DPER5                                                            
         LA    R3,LINRHS           DISPLAY ADDL PERF INFO ON RHS                
DPER4    BAS   RE,DISPINFO         DISPLAY DIFFERENT RHS                        
         B     DPER20                                                           
         SPACE 1                                                                
DPER5    LA    R3,LINPNME+LINLNQ   R3=A(2ND LINE UNDER NAME)                    
         BAS   RE,DSPGE            DISPLAY GUARANTEE/EXPDT IF ROOM              
         BNE   DPERNO              GET OUT                                      
         LA    R3,LINOV            R3=A(DISPLAY AREA FOR OVERSCALE)             
         BRAS  RE,DSPOV            DISPLAY OVERSCALE 1,2                        
         SPACE 1                                                                
DPER20   BAS   RE,SETPBUF          SET CAT AND DCW INFO IN BUFFER               
         GOTO1 XNAME,DMCB,TLW4CDQ,TGNAME,KEY  GET W4 NAME                       
         MVC   DMDSKADD,NXTCSTDA   XNAME CREAMS D/A SO RESTORE IT               
         SPACE 1                                                                
         BAS   RE,ADJPBUF          ADJUST C OF DCW IN BUFFER                    
         LA    R3,LINCAT           CATEGORY                                     
         LA    R1,L'LINPNME-1                                                   
         CLI   NEWEST,C'Y'         IF NEW ESTIMATE DISPLAY                      
         BNE   DPER50                                                           
         LA    R3,8(R3)                                                         
         LA    R1,8(R1)                                                         
DPER50   MVC   0(L'PBUFF,R3),PBUFF                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LINPNME(0),TGNAME   MOVE NAME TO SCREEN                          
*                                                                               
         TM    DISPMD2,SELCTPRF    IF NOT PERFORMER SELECT SCREEN               
         BO    DPER55                                                           
         CLI   WRKBYTE,0           IF PERFORMER ON LIFT                         
         BE    *+14                                                             
         MVI   LINPLIFT,C'*'       FLAG AT END OF NAME                          
         MVC   LINPLIFT+1(1),WRKBYTE                                            
         CLI   NEWEST,C'Y'         OR IF NOT NEW ESTIMATE DISPLAY               
         BE    DPERX                                                            
*                                                                               
DPER55   BAS   RE,LOOKUP           LOOK UP ENTRY                                
         BNE   DPERX                                                            
         L     R4,FULL             R4=A(EL.) FROM LOOKUP                        
         LA    RF,LINLNQ                                                        
         GOTO1 AOPUNSCN,DMCB,LINRHSH,((RF),ESTLSTRH) DISP. OPTIONS              
         BNE   DPERNO                                                           
DPERX    B     YES                                                              
         SPACE 1                                                                
DPERNO   B     NO                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY RHS OF SELECTABLE PERFORMER LIST              
         SPACE 1                                                                
         USING TACAD,R4            R4=A(CAST DETAILS ELEMENT)                   
         USING LINPERFD,R3         R3=A(SCREEN DSECT LINE)                      
DISPINFO NTR1                                                                   
         TM    DISPMD2,SELCTPRF    IF NOT PERFORMER SELECT SCREEN               
         BO    *+8                                                              
         NI    LINCODEH+1,X'DE'    UNPROTECT SELECT FIELD                       
*                                                                               
         MVC   LINPUNI,TACAUN      UNION                                        
         MVC   LINPLOCL,TACALOCL   LOCAL                                        
         MVC   LINPYEAR,TACAYEAR   YEAR                                         
         CLI   WRKBYTE,0           IF PERFORMER ON LIFT                         
         BE    DISPI5                                                           
         MVC   LINPLFT,WRKBYTE     DISPLAY LIFT STATUS                          
         CLI   LINPLFT,C'L'                                                     
         BNE   *+8                                                              
         MVI   LINPLFT,C'Y'                                                     
         MVI   WRKBYTE,0                                                        
*                                                                               
DISPI5   LR    R1,R3               SAVE R3                                      
         LA    R3,LINPOVS          R3=A(DISPLAY AREA FOR OVERSCALE)             
         BRAS  RE,DSPOV            DISPLAY OVERSCALE 1,2                        
         LR    R3,R1               RESTORE R3                                   
         OC    TACAGUA,TACAGUA     GUARANTEE                                    
         BZ    *+10                                                             
         MVC   LINPGUAR,TACAGUA                                                 
         OC    TACAEXP,TACAEXP     EXPIRATION DATE                              
         BZ    DISPI10                                                          
         GOTO1 DATCON,DMCB,(1,TACAEXP),(8,LINPEXP)                              
         SPACE 1                                                                
DISPI10  BAS   RE,LOOKUP           LOOK UP ENTRY                                
         BNE   *+8                                                              
         MVI   LINPDTLS,C'Y'       PERFORMER DETAILS                            
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY GUARANTEE AND EXPIRATION DATE                 
         USING TACAD,R4            R4=A(CAST DETAILS ELEMENT)                   
         SPACE                                                                  
DSPGE    NTR1                                                                   
         OC    TACAGUA,TACAGUA     IF ON GUARANTEE                              
         BZ    DSPGE10                                                          
         BAS   RE,TSTROOM          AND THERE'S NO ROOM                          
         BNE   NO                  GET OUT                                      
         MVC   0(L'LTGUAR,R3),LTGUAR ELSE DISPLAY IT                            
         MVC   L'LTGUAR(L'TACAGUA,R3),TACAGUA                                   
         SPACE 1                                                                
DSPGE10  CLI   EXPIRY,C'Y'         IF NEED TO DISPLAY EXPIRATION DATES          
         BNE   DSPGEX                                                           
         OC    TACAEXP,TACAEXP     THEN IF IT EXISTS                            
         BZ    DSPGEX                                                           
         BAS   RE,TSTROOM          AND THERE'S NO ROOM                          
         BNE   NO                  GET OUT                                      
         SPACE 1                                                                
         CLI   0(R3),0             IF SOMETHING'S ALREADY THERE                 
         BE    *+12                                                             
         MVI   L'LTGUAR+L'TACAGUA(R3),C',' SET TO DISPLAY AFTER IT              
         LA    R3,L'LTGUAR+L'TACAGUA+1(R3)                                      
         MVC   0(L'LTEXP,R3),LTEXP         DISPLAY EXPIRATION DATE              
         GOTO1 DATCON,DMCB,(1,TACAEXP),(8,L'LTEXP(R3))                          
         SPACE 1                                                                
DSPGEX   B     YES                                                              
         SPACE 2                                                                
*              ROUTINE DETERMINES IF ROOM ON NEXT LINE FOR COMML/CAST           
         SPACE 1                                                                
*                                  R2=A(CURRENT LINE)                           
TSTROOM  DS    0H                                                               
         LA    RF,ESTLSTDH         RF=A(LAST LINE ON SCREEN)                    
         CR    R2,RF               IF BEFORE LAST LINE                          
         BNL   *+12                                                             
         MVI   THSLINES,2          SET USED 2 LINES ALREADY                     
         B     YESR                   RETURN CC EQ                              
         XC    LINDATA,LINDATA     ELSE CLEAR CURRENT LINE                      
         B     NOR                    AND RETURN CC NE                          
         EJECT                                                                  
*              ROUTINE TO SAVE SOME PERFORMER INFORMATION IN PBUFF              
         SPACE 1                                                                
SETPBUF  NTR1                                                                   
         MVC   PBUFF,SPACES                                                     
         LA    RE,PBUFF                                                         
         USING LINCAT,RE                                                        
*                                                                               
         MVC   LINCAT,TGCAT        CATEGORY                                     
         MVC   LINDBL,TACADBL      N'DOUBLES                                    
*                                                                               
         MVI   LINCAM,C'Y'         ON CAMERA                                    
         CLI   TACAONOF+1,C'F'                                                  
         BNE   *+8                                                              
         MVI   LINCAM,C'N'         OFF CAMERA                                   
*                                                                               
         CLI   TACACORP,C' '       IF CORP CODE ON CAST RECORD                  
         BNH   *+8                                                              
         MVI   LINW4TY,TAW4TYCO    SET W4 TYPE IS CORPORATION                   
         B     XIT                                                              
         DROP  RE                                                               
         SPACE 2                                                                
*              ROUTINE TO ADJUST PERFORMER INFORMATION IN PBUFF                 
         SPACE 1                                                                
ADJPBUF  NTR1                                                                   
         L     R4,AIO                                                           
         MVI   ELCODE,TAW4ELQ      LOOK FOR W4 DETAILS ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAW4D,R4                                                         
         CLI   LINW4TY,0           IF HAVEN'T ALREADY DISPLAYED W4 TYPE         
         BNE   ADJPBX                                                           
         CLI   TAW4TYPE,TAW4TYIN   AND W4 TYPE IS NOT INDIVIDUAL                
         BE    ADJPBX                                                           
*                                                                               
         LA    RE,PBUFF                                                         
         USING LINCAT,RE                                                        
         MVC   LINW4TY,TAW4TYPE    ADJUST BUFFER NOW                            
ADJPBX   B     XIT                                                              
         DROP  RE                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE COMMERCIAL LIST SCREEN                       
         SPACE 1                                                                
VALCOM   NTR1                                                                   
         MVI   ELTYPE,TAESTCOM     SET COMMERCIAL ELEMENT TYPE                  
         MVI   OPTOKBIT,OPOKCOM    SET OK BIT                                   
         GOTO1 VALRHS,DMCB,CSELTAB VALIDATE RHS - ADD ELEMENTS                  
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO VALIDATE SELECTED COMMERCIAL                          
         SPACE                                                                  
VALSCOM  NTR1                                                                   
         BAS   RE,COMHEADS         INSURE WE HAVE COMML HEADS                   
         XC    CSELTAB,CSELTAB     CLEAR SELECTED RHS TABLE                     
         BAS   RE,VALCOM           VALIDATE COMMERCIAL FIELDS                   
         CLI   PFAID,PFPRFLST      IF ASK TO EXECUTE P OR A                     
         BE    VALSC10                                                          
         CLI   PFAID,PFPRFALT                                                   
         BE    VALSC10                                                          
         XC    CSELLAST(SELLNQ),CSELLAST                                        
VALSC10  BAS   RE,EXECCOM          EXECUTE INPUT                                
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO VALIDATE PERFORMER LIST SCREEN                        
         SPACE 1                                                                
VALPERF  NTR1                                                                   
         MVI   ELTYPE,TAESTPER     SET PERFORMER ELEMENT TYPE                   
         MVI   OPTOKBIT,OPOKPER    SET OK BIT                                   
         GOTO1 VALRHS,DMCB,PSELTAB VALIDATE RHS - ADD ELEMENTS                  
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO VALIDATE SELECTED PERFOMER                            
         SPACE                                                                  
VALSPERF NTR1                                                                   
         BAS   RE,VALPERF          VALIDATE PERFORMER FIELDS                    
         BAS   RE,EXECPERF         EXECUTE INPUT                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE USER INPUT ON RHS OF LISTS                   
         SPACE 1                                                                
VALRHS   NTR1                                                                   
         XC    SCRNTAB,SCRNTAB                                                  
         MVC   ATHSTBL,0(R1)       SAVE A(ACTUAL TABLE)                         
         LA    R1,BLOCK            SAVE A(TEMP. LIST FOR SELECTIONS)            
         ST    R1,ATHSSEL                                                       
         ZAP   SELCNT,=P'0'        INITIALIZE SPECIAL SELECT COUNT              
         TM    DISPMODE,SELCTCOM   IF COMMERCIAL SELECT SCREEN                  
         BO    *+12                                                             
         TM    DISPMD2,SELCTPRF    OR IF PERFORMER SELECT SCREEN                
         BZ    VALR1                                                            
         ZAP   SUBELCNT,=P'0'      CLEAR N'SUB ELEMENTS                         
         MVC   ANXTSUB,ASUBELS     SET A(FIRST SUB-ELEMENT)                     
         XC    OPTWORK(OPTWKLNQ),OPTWORK CLEAR OPTION WORK AREAS                
         SPACE 1                                                                
VALR1    LA    R0,NLINEQU          R0=N'SCREEN LINES                            
         LA    R2,ESTDATAH         R2=A(FIRST DATA LINE)                        
         USING LINED,R2                                                         
         LA    R3,SCRNTAB          R3=A(TABLE OF ELEMENT KEYS)                  
         USING SCRND,R3                                                         
         B     VALR2A                                                           
         SPACE 1                                                                
VALR2    TM    DISPMODE,SELCTCOM   IF COMMERCIAL SELECT SCREEN                  
         BO    VALR2B              IGNORE LHS                                   
         TM    DISPMD2,SELCTPRF    IF PERFORMER SELECT SCREEN                   
         BO    VALR2B              IGNORE LHS                                   
         OC    LINDATA(9),LINDATA  IF WE HAVE SOMETHING ON LHS                  
         BZ    VALR2B                                                           
         TM    STATUS2,ADDESTEL    DO WE NEED TO ADD LAST EST. ELEMENT          
         BZ    *+8                                                              
         BAS   RE,INESTEL          ADD ESTIMATE ELEMENT TO RECORD               
         SPACE 1                                                                
         LA    R3,SCRNNEXT         BUMP TO NEXT ENTRY                           
         SPACE 1                                                                
VALR2A   BAS   RE,SETELDAT         SET NEW ESTIMATE ELEMENT KEY DATA            
         MVC   SCRNCODE,LINCODE    BUILD SCREEN TABLE ENTRY - CODE              
         MVC   SCRNDATA,ELDATA     AND KEY DATA                                 
         SPACE 1                                                                
VALR2B   CLI   LINRHSH+5,0         ANY INPUT                                    
         BE    VALR4                                                            
         SPACE 1                                                                
VALR2D   TM    DISPMODE,SELCTCOM   IF COMMERCIAL SELECT SCREEN                  
         BO    VALR3               IGNORE LHS                                   
         TM    DISPMD2,SELCTPRF    IF PERFORMER SELECT SCREEN                   
         BO    VALR3               IGNORE LHS                                   
         OC    LINDATA(9),LINDATA  IF NOTHING ON LHS SKIP                       
         BZ    VALR3                                                            
         ZAP   SUBELCNT,=P'0'      CLEAR N'SUB ELEMENTS                         
         MVC   ANXTSUB,ASUBELS     SET A(FIRST SUB-ELEMENT)                     
         XC    OPTWORK(OPTWKLNQ),OPTWORK CLEAR OPTION WORK AREAS                
         SPACE 1                                                                
         CLI   LINRHSH+5,2         IF ONLY TWO CHARACTERS INPUT                 
         BNE   VALR3                                                            
         CLI   LINRHS,C'='         AND IT STARTS WITH AN EQUAL SIGN             
         BNE   VALR3                                                            
         CLI   LINRHS+1,C'A'       AND ENDS WITH 'A' THROUGH                    
         BL    VALR3                                                            
         CLC   LINRHS+1(1),ESTLSTD LETTER OF LAST DATA LINE                     
         BH    VALR3                                                            
         BAS   RE,INESTEL          ADD A DUMMY EL. FOR THIS ONE                 
         MVC   SCRNCOPY,LINRHS+1   SAVE CODE OF COPY DESTINATION                
         CLI   ELTYPE,TAESTCOM     IF VALIDATING COMMERCIALS                    
         BNE   *+12                                                             
         CLI   LINMOVE,C'*'        AND WILL MOVE EL (CID CHANGE)                
         BE    *+8                 SKIP SO WILL DELETE EXISTING EL.             
         OI    SCRNSTAT,SCRNOK     ELSE SET THIS KEY IS OK                      
VALR2F   LA    R2,LINNEXT          BUMP TO NEXT LINE WITH DATA                  
         BCT   R0,*+8                                                           
         B     VALR5                                                            
         OC    LINDATA(9),LINDATA  IF SOMETHING ON LHS                          
         BNZ   VALR2               CONTINUE                                     
         CLI   LINRHSH+5,0         ELSE INSURE NOTHING ON RHS                   
         BNE   NOINPUT                                                          
         B     VALR2F                                                           
         SPACE 1                                                                
VALR3    ST    R3,ATHSSCRN         SET A(THIS SCREEN ENTRY)                     
         CLI   ELTYPE,TAESTCOM     IF VALIDATING COMMERCIALS                    
         BNE   *+12                                                             
         CLI   LINMOVE,C'*'        AND WILL MOVE EL (CID CHANGE)                
         BE    *+8                 SKIP SO WILL DELETE EXISTING EL.             
         OI    SCRNSTAT,SCRNOK     ELSE SET THIS KEY IS OK                      
         SPACE 1                                                                
VALR4    GOTO1 AOPSCAN,DMCB,LINRHSH   *** SCAN RHS ***                          
         SPACE 1                                                                
         CLI   ELTYPE,TAESTCOM     IF VALIDATING COMMERCIALS                    
         BNE   *+16                                                             
         CLI   LINMOVE,C'*'        AND WILL MOVE EL (CID CHANGE)                
         BNE   *+8                                                              
         NI    SCRNSTAT,ALL-SCRNOKPR-SCRNOKHP  SET TO REMOVE ADDL/PERFS         
         SPACE 1                                                                
         LA    R2,LINNEXT          BUMP LINE POINTER                            
         BCT   R0,VALR2            VALIDATE NEXT LINE                           
         SPACE 1                                                                
VALR5    TM    STATUS2,ADDESTEL    IF WE NEED TO ADD LAST EST. ELEMENT          
         BZ    *+8                                                              
         BAS   RE,INESTEL          ADD ESTIMATE ELEMENT TO RECORD               
         SPACE 1                                                                
         BAS   RE,COPY             TAKE CARE OF COPIES                          
         SPACE 1                                                                
         GOTO1 AWRITEIT            WRITE THE RECORD BACK NOW                    
         SPACE 1                                                                
         ZAP   DUB,SELCNT          TEST SOMETHING SELECTED                      
         BZ    XIT                                                              
         CVB   R1,DUB              R1=N'ENTRIES SELECTED                        
         L     R4,ATHSTBL                                                       
         LA    R4,L'CSELTAB-SELLNQ(R4)  R4=A(LAST ENTRY IN TABLE)               
         LA    R0,NSELS            R0=MAX N'ENTRIES                             
         USING SELD,R4                                                          
         CLI   SELRHS,0                                                         
         BE    *+12                                                             
         SH    R4,=AL2(SELLNQ)     SHUFFLE BACK FOR FIRST EMPTY SLOT            
         BCT   R0,*-12                                                          
         CR    R1,R0               DO WE HAVE ENOUGH ROOM                       
         BH    TOOMANY                                                          
         BCTR  R1,0                YES - SUBTRACT ONE                           
         MH    R1,=AL2(SELLNQ)     R1=ADDITIONAL SPACE NEEDED                   
         SR    R4,R1               BUMP BACK ADDITIONAL AMOUNT                  
         LA    R1,SELLNQ-1(R1)     ADD L'ENTRY-1 FOR EXECUTED MOVE              
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   0(0,R4),BLOCK       MOVE IN NEW DATA                             
         EJECT                                                                  
*              ROUTINE TO SET ELEMENT DATA FOR COMMERCIALS/CASTS                
         SPACE 2                                                                
         USING LINED,R2            R2=A(CURRENT SCREEN LINE)                    
SETELDAT NTR1                                                                   
         LA    RF,ESTDATAH         A(FIRST LINE)                                
         SR    RF,R2                                                            
         LPR   RF,RF               RF=DISPLACEMENT TO THIS LINE                 
         XR    RE,RE                                                            
         D     RE,=AL4(LINLNQ)     DETERMINE RELATIVE LINE NUMBER               
         LR    R3,RF               (SAVE IT)                                    
         SLL   RF,2                *4                                           
         LA    RF,THSPGDAS(RF)                                                  
         MVC   DMDSKADD,0(RF)      SET D/A OF THIS RECORD                       
*                                                                               
         XC    ELDATA,ELDATA       SET ESTIMATE ELEMENT DATA                    
         LA    R4,ELDATA                                                        
         USING TAESDATA,R4                                                      
         CLI   ELTYPE,TAESTCOM     TEST FOR COMMERCIAL TYPE                     
         BNE   SETEL20                                                          
*                                                                               
         TM    DISPMODE,SELCTCOM   IF COMML SELECT MODE                         
         BZ    SETEL10                                                          
         ZIC   RF,PGSCOIND         DETERMINE INDEX INTO TABLE                   
         BCTR  RF,0                                                             
         MH    RF,=AL2(PGLNQ)                                                   
         LA    RF,PGSCOTAB(RF)                                                  
         MVC   TGCOM,0(RF)         SET INTERNAL COMMERCIAL NUMBER               
         MVC   DMDSKADD,4(RF)      RESET D/A OF THIS RECORD                     
         B     SETEL15                                                          
*                                                                               
SETEL10  SLL   R3,2                                                             
         LA    R3,THSPGCOM(R3)                                                  
         MVC   TGCOM,0(R3)         EXTRACT INT. COMML NO. FROM LIST             
*                                                                               
SETEL15  MVC   TAESCOM,TGCOM       MOVE TO ELEMENT DATA                         
         MVC   TAESCID,LINCID      SET COMMERCIAL ID                            
         OC    TAESCID,SPACES                                                   
         GOTO1 MEDVAL,DMCB,LINMEDIA SET GLOBAL MEDIA AS WELL                    
         B     XIT                                                              
*                                                                               
SETEL20  DS    0H                  MUST BE CAST                                 
         TM    DISPMD2,SELCTPRF    IF IN PERFORMER SELECT MODE                  
         BZ    SETEL30                                                          
         ZIC   RF,PGSPFIND         DETERMINE INDEX INTO TABLE                   
         BCTR  RF,0                                                             
         MH    RF,=AL2(PGLNQ)                                                   
         LA    RF,PGSPFTAB(RF)                                                  
         MVC   TGCSORT,0(RF)       SET CAST SORT KEY                            
         MVC   DMDSKADD,6(RF)      SET D/A OF OF THIS RECORD                    
         B     SETEL40                                                          
*                                                                               
SETEL30  MH    R3,=H'6'                                                         
         LA    R3,THSPGSRT(R3)                                                  
         MVC   TGCSORT,0(R3)       EXTRACT CAST SORT KEY                        
*                                                                               
SETEL40  MVC   TAESSORT,TGCSORT    AND MOVE TO ELEMENT DATA                     
         TM    TGSYSTAT,TASYSPID   USING PID?                                   
         BZ    SETEL50                                                          
         GOTO1 SSNUNPK,DMCB,LINSSN,MYWORK  CONVERT PID TO SSN                   
         BE    *+6                                                              
         DC    H'00'                                                            
         PACK  DUB,MYWORK(9)                                                    
         B     *+10                                                             
SETEL50  PACK  DUB,LINSSN                                                       
         CVB   R1,DUB                                                           
         STCM  R1,15,TAESSSN       SET SOCIAL SECURITY NUMBER                   
         MVC   CORPFLAG,LINW4TY    SET CORP FLAG                                
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO EXECUTE SELECTED COMMERCIALS                          
         SPACE 1                                                                
EXECCOM  NTR1                                                                   
         OC    CSELLAST(SELLNQ),CSELLAST DID USER SELECT ANYTHING               
         BZ    XIT                                                              
         MVI   INSRTBIT,INSERTC    SET INSERT BIT FOR COMMERCIALS               
         GOTO1 EXECRHS,DMCB,CSELTAB    AND DEAL WITH USER INPUT                 
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO EXECUTE SELECTED PERFORMERS                           
         SPACE 1                                                                
EXECPERF NTR1                                                                   
         OC    PSELLAST(SELLNQ),PSELLAST DID USER SELECT ANYTHING               
         BZ    XIT                                                              
         MVI   INSRTBIT,INSERTP    SET INSERT BIT FOR PERFORMERS                
         GOTO1 EXECRHS,DMCB,PSELTAB    AND DEAL WITH USER INPUT                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO EXECUTE USER INPUT ON RHS OF LISTS                    
         SPACE 1                                                                
         USING SELD,R4                                                          
EXECRHS  NTR1                                                                   
         L     R4,0(R1)            R4=A(TABLE)                                  
         BRAS  RE,CLRSCRN          CLEAR SCREEN                                 
         MVI   TGBYTE,X'FF'                                                     
         XC    TGBYTE,INSRTBIT     INSRTBIT=INSERT MODE BIT                     
         NC    STATUS,TGBYTE       TURN OFF INSERT MODE INDICATOR               
         LA    R2,ESTDATAH         R2=A(FIRST DATA LINE)                        
         USING LINED,R2                                                         
         LA    R0,NSELS            R0=MAX N'ENTRIES IN TABLE                    
         SPACE 1                                                                
EXECR2   CLI   SELRHS,0            LOOP UNTIL WE FIND SOMETHING                 
         BE    EXECR8                                                           
         CLI   INSRTBIT,INSERTC    IF WE'RE PROCESSING COMMERCIAL LIST          
         BNE   *+10                                                             
         MVC   NXTCOMDA,SELDA      SET TO RESTART LIST FROM HERE                
         SPACE 1                                                                
         TM    DISPMODE,SELCTCOM   IF SELECTED COMMERCIAL DISPLAYED             
         BO    EXECR4              NO NEED FOR INSERTIONS                       
         CLI   SELRHS,NLINEQU      ELSE, IF INSERT NO. OF LINES                 
         BNL   EXECR4                                                           
         ZIC   R1,SELRHS           ADD N'LINES INPUT                            
         MH    R1,=AL2(LINLNQ)     SEE IF WE HAVE ENOUGH ROOM                   
         AR    R1,R2                                                            
         LA    RE,ESTLSTDH         RE=A(LAST DATA LINE)                         
         CR    R1,RE                                                            
         BH    INSERTED            NO - NEED TO DO THIS ONE NEXT TIME           
         BAS   RE,DISTABKY         MOVE KEY DATA TO SCREEN                      
         BNE   INSERTED            NO ROOM FOR THIS ONE                         
         OC    STATUS,INSRTBIT     SET WE HAVE AN INSERT                        
         ZIC   RE,THSLINES         RE=N'LINES USED                              
         ZIC   R1,SELRHS                                                        
         AH    R1,=H'1'            R1=N'LINES DESIRED                           
         CR    RE,R1                                                            
         BNL   *+6                                                              
         LR    RE,R1               RE=N'LINES TO SKIP                           
         MH    RE,=AL2(LINLNQ)                                                  
         AR    R2,RE               BUMP TO NEXT AVAILABLE LINE                  
         B     EXECR8                                                           
         SPACE 1                                                                
EXECR4   CLI   PERFS,C'N'          SUPPRESS PERFORMER DISPLAY                   
         BE    EXECR8                                                           
         CLI   SELRHS,C'P'         SWITCH TO PERFORMER LIST                     
         BNE   EXECR6                                                           
         TM    STATUS,INSERTC      DO WE HAVE AN INSERT PENDING                 
         BO    INSERTED            YES - DO THAT FIRST                          
         TM    DISPMODE,SELCTCOM   IF COMML SELECT SCREEN                       
         BZ    *+12                                                             
         CLI   PFAID,PFPRFLST      CHECK ASKED TO SHOW P                        
         BNE   EXECR6                                                           
         BAS   RE,LISTPER          ELSE DISPLAY PERFORMER LIST                  
         BE    PERDSPLY            PERFORMERS DISPLAYED                         
         SPACE 1                                                                
EXECR6   CLI   SELRHS,C'A'         ADD MORE PERFORMERS                          
         BNE   EXECR8                                                           
         TM    STATUS,INSERTC      DO WE HAVE AN INSERT PENDING                 
         BO    INSERTED            YES - DO THAT FIRST                          
         TM    DISPMODE,SELCTCOM   IF COMML SELECT SCREEN                       
         BZ    *+12                                                             
         CLI   PFAID,PFPRFALT      CHECK ASKED TO SHOW A                        
         BNE   EXECR8                                                           
         BAS   RE,HYPOINIT         ELSE LOAD IN HYPO SCREEN                     
         BAS   RE,HYPOADDL         SET TO ADD ADD'L PERFORMERS                  
         BAS   RE,DISHYPRF         DISPLAY ANY EXISTING ONES                    
         MVI   SELRHS,0            CLEAR THIS ENTRY                             
         B     HYPOOUT             AND RETURN APPROPRIATE MESSAGE               
         SPACE 1                                                                
EXECR8   MVI   SELRHS,0            CLEAR THIS ENTRY                             
         LA    R4,SELNEXT          BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,EXECR2                                                        
         SPACE 1                                                                
         NC    INSRTBIT,STATUS     END OF TABLE-DO WE HAVE INSERT PEND.         
         BNZ   INSERTED            YES - DO THAT FIRST                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE SETS HYPO SCREEN FOR ADDING ADD'L PERFORMERS             
         SPACE 1                                                                
         USING SELD,R4             R4=A(CSELTAB ENTRY)                          
HYPOADDL NTR1                                                                   
         BAS   RE,DISTABKY         READ COMMERCIAL RECORD                       
         MVC   DUB,TGPRD                                                        
         GOTO1 EXTRACT             EXTRACT GLOBAL VALUES                        
         MVC   TGPRD,DUB                                                        
         GOTO1 MEDVAL,DMCB,SELMEDIA  SET MEDIA                                  
         SPACE 1                                                                
         MVC   HYPCOM,TGCID        MOVE COMMERCIAL TO SCREEN                    
         MVC   HYPMED,TGMENAME               MEDIA                              
         GOTO1 CHAROUT,DMCB,TANAELQ,HYPNAMEH NAME                               
         SPACE 1                                                                
         L     R4,AIO              AIO=A(RECORD)                                
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL            GET COMMERCIAL DETAILS ELEMENT               
         USING TACOD,R4                                                         
         EDIT  (1,TACOSEC),(2,HYPLEN)        LENGTH                             
         SPACE 1                                                                
         LA    RF,HYPCOMH          RF=A(FIRST FIELD TO PROTECT)                 
         LA    RE,HYPOPTTH         RE=A(FIRST FIELD TO HIDE/PROTECT)            
         LA    R1,HYPOPT4H         R1=A(LAST FIELD TO HIDE/PROTECT)             
         XR    R0,R0                                                            
HYPAD2   OI    1(RF),X'20'         SET PROTECT                                  
         CR    RF,RE                                                            
         BL    *+8                                                              
         OI    1(RF),X'0C'         SET ZERO INTENSITY                           
         IC    R0,0(RF)                                                         
         AR    RF,R0                                                            
         CR    RF,R1                                                            
         BNH   HYPAD2                                                           
         SPACE 1                                                                
         NI    DISPMODE,ALL-HYPOCOM                                             
         OI    DISPMODE,HYPOPER    SET WE'RE IN HYPO PERFORMER MODE             
         SPACE 1                                                                
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,RE                                                         
         OI    TIOBINDS,TIOBSUBS   SET CHANGING SUB-SCREEN                      
         MVI   TIOBAID,X'01'       SET SPECIAL SUB-SCREEN                       
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY KEY DATA FROM SELECT TABLES                   
         SPACE 1                                                                
         USING LINED,R2            R2=A(CURRENT LINE)                           
         USING SELD,R4             R4=A(CURRENT TABLE ENTRY)                    
DISTABKY NTR1                                                                   
         LA    R3,KEY                                                           
         USING TLDRD,R3                                                         
         MVC   TLDRDA,SELDA        SET D/A OF RECORD                            
         GOTO1 GETREC              GET IT                                       
         L     R1,AIO                                                           
         MVC   TLDRKEY,0(R1)       SET KEY FROM RECORD                          
         SPACE 1                                                                
         TM    DISPMODE,HYPO       GET OUT NOW IF IN HYPO MODE                  
         BNZ   XIT                                                              
         TM    DISPMD2,LISTSPRF    IF LISTING PERFORMERS                        
         BO    *+12                                                             
         TM    DISPMODE,LISTPRFS                                                
         BZ    DISTA4                                                           
         BAS   RE,DISPER           DISPLAY PERFORMER                            
         BNE   NO                                                               
         B     YES                                                              
         SPACE 1                                                                
DISTA4   BAS   RE,DISCOM           ELSE DISPLAY COMMERCIAL                      
         BNE   NO                                                               
         B     YES                                                              
         EJECT                                                                  
*              ROUTINES TO CHECK FOR PFKEY PRESENCE ON COMM'L SCREEN            
         SPACE 1                                                                
CKCOMPFS NTR1                                                                   
         CLI   PFAID,0             RETURN CC NE IF USER HIT ENTER               
         BE    NO                                                               
         CLI   PFAID,PFREPORT      USER WANTS REPORT SCREEN                     
         BE    REPOUT                                                           
         SPACE 1                                                                
         CLI   PFAID,PFSTAY        IF REMAIN PFKEY HIT                          
         BNE   CKCOM2                                                           
         TM    STATUS,FINISHED     IGNORE IF FINISHED                           
         BO    NO                                                               
         BAS   RE,THSCOMDA         DIG OUT THIS KEY                             
         B     CKCOM6              AND START DISPLAY FROM HERE                  
         SPACE 1                                                                
CKCOM2   NI    STATUS,ALL-FINISHED TURN OFF FINISHED STATUS                     
         SPACE 1                                                                
         CLI   PFAID,PFBEGIN       USER WANTS 1ST PAGE OF COMM'L LIST           
         BNE   CKCOM4                                                           
         BAS   RE,FSTCOMDA         DIG OUT FIRST KEY                            
         OC    NXTCOMDA,NXTCOMDA   IF WE DIDN'T GET BACK D/A OF COMML           
         BNZ   *+10                                                             
         MVC   SAVEHCOM,STARTHYP   SET TO START AT HYPO BEG.                    
         B     CKCOM6                                                           
         SPACE 1                                                                
CKCOM4   CLI   PFAID,PFPREV        USER WANTS PREV PAGE OF COMM'L LIST          
         BNE   CKCOM8                                                           
         BAS   RE,LSTCOMDA         DIG OUT PREVIOUS KEY                         
         SPACE 1                                                                
CKCOM6   OC    NXTCOMDA,NXTCOMDA   IF WE GOT BACK D/A OF COMMERCIAL             
         BZ    *+8                                                              
         NI    STATUS,ALL-ENDOFLST TURN OFF END OF COMMERCIAL LIST              
         B     YES                                                              
         SPACE 1                                                                
CKCOM8   CLI   PFAID,PFMSCADD      DOES USER WANT MISC. ADDITIONS SCRN          
         BNE   CKCOM10                                                          
         BAS   RE,MISCINIT         YES - LOAD SCREEN                            
         BAS   RE,DISMISC          DISPLAY DETAILS IF AROUND                    
         B     MISCOUT             RETURN NOW                                   
         SPACE 1                                                                
CKCOM10  TM    STATUS2,BUILDING    IF WE'RE IN BUILD MODE                       
         BZ    NO                                                               
         CLI   PFAID,PFADDHYP      DOES USER WANT TO ADD A HYPO COMM'L          
         BNE   NO                                                               
         BAS   RE,HYPOINIT         YES - LOAD SCREEN                            
         XC    HYCODATA,HYCODATA                                                
         B     HYPOOUT             RETURN NOW                                   
         EJECT                                                                  
*              ROUTINES TO CHECK FOR PFKEY PRESENCE ON CAST SCREEN              
         SPACE 1                                                                
CKPERPFS NTR1                                                                   
         CLI   PFAID,PFREPORT      USER WANTS REPORT SCREEN                     
         BE    REPOUT                                                           
         SPACE 1                                                                
         CLI   PFAID,PFRETURN      USER WANTS TO RETURN TO COMM'L LIST          
         BNE   *+16                                                             
         NI    DISPMODE,ALL-LISTPRFS  RESET DISPLAY MODE                        
         NI    DISPMD2,ALL-LISTSPRF   RESET DISPLAY MODE                        
         B     LOWX                   AND RETURN CC MINUS                       
         SPACE 1                                                                
         CLI   PFAID,PFNXTCOM      USER WANTS TO SKIP TO NEXT COMM'L            
         BE    NO                  RETURN CC NE                                 
         SPACE 1                                                                
         CLI   PFAID,PFBEGIN       USER WANTS 1ST PAGE OF PERF LIST             
         BNE   *+8                                                              
         BAS   RE,FSTCSTDA                                                      
         SPACE 1                                                                
         CLI   PFAID,PFPREV        USER WANTS PREVIOUS PAGE                     
         BNE   *+8                                                              
         BAS   RE,LSTCSTDA                                                      
         SPACE 1                                                                
         CLI   PFAID,PFSTAY        USER WANTS TO STAY ON THIS SCREEN            
         BNE   *+8                                                              
         BAS   RE,THSCSTDA                                                      
         SPACE 1                                                                
         B     YES                 RETURN CC EQ                                 
         EJECT                                                                  
*              ROUTINE TO SET D/A FOR RE-DISPLAYING COMM'L PAGES                
         SPACE 1                                                                
FSTCOMDA ZIC   R1,PGCOMIND         SET TO START AT FIRST PAGE                   
         B     ALLCOMDA                                                         
LSTCOMDA LA    R1,2                SET TO START AT PREVIOUS PAGE                
         B     ALLCOMDA                                                         
THSCOMDA LA    R1,1                SET TO START AT THIS PAGE                    
ALLCOMDA ZIC   RF,PGCOMIND         USE RELATIVE INDEX INTO LIST                 
         SR    RF,R1                                                            
         BM    NOPREV                                                           
         STC   RF,PGCOMIND         RESET INDEX                                  
         MH    RF,=AL2(PGLNQ)      *L'ENTRY                                     
         LA    RF,PGCOMTAB(RF)     RF=A(ENTRY)                                  
         XC    NXTCOMDA,NXTCOMDA                                                
         XC    SAVEHCOM,SAVEHCOM                                                
         CLI   4(RF),0             IF SOMETHING PAST D/A                        
         BE    *+14                                                             
         MVC   SAVEHCOM,0(RF)      THIS MUST BE HYPO COMML CODE                 
         B     *+10                                                             
         MVC   NXTCOMDA,0(RF)      ELSE SET D/A OF START REC.                   
         OI    STATUS2,REDISP      SET TO RE-DISPLAY FROM HERE                  
         BR    RE                                                               
         SPACE 3                                                                
*              ROUTINE TO SET D/A FOR RE-DISPLAYING PERF. PAGES                 
         SPACE 1                                                                
FSTCSTDA ZIC   R1,PGCSTIND         SET TO START AT FIRST PAGE                   
         B     ALLCSTDA                                                         
LSTCSTDA LA    R1,2                SET TO START AT PREVIOUS PAGE                
         B     ALLCSTDA                                                         
THSCSTDA LA    R1,1                SET TO START AT THIS PAGE                    
ALLCSTDA ZIC   RF,PGCSTIND         USE RELATIVE INDEX INTO LIST                 
         SR    RF,R1                                                            
         BM    NOPREV                                                           
         STC   RF,PGCSTIND         RESET INDEX                                  
         MH    RF,=AL2(PGLNQ)      *L'ENTRY                                     
         LA    RF,PGCSTTAB(RF)                                                  
         MVC   NXTCSTDA,0(RF)      SET D/A OF START REC.                        
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE COMMERCIAL SELECT FIELDS                     
         SPACE 1                                                                
VALSSEL  NTR1                                                                   
*        XC    PGSCOTAB,PGSCOTAB                                                
         XCEF  PGSCOTAB,L'PGSCOTAB                                              
*                                                                               
         LA    R3,PGSCOTAB         R3=SELECT TABLE FOR COMMERCIAL LIST          
         LA    R2,ESTDATAH         R2=1ST DATA LINE                             
         LA    R4,NLINEQU          R4=N'SCREEN LINES                            
         SPACE 1                                                                
VALSS2   CLI   LINCODEH+5,0                                                     
         BE    VALSS4                                                           
         TM    STATUS2,BUILDING    IF CHANGING                                  
         BZ    *+12                                                             
         CLI   LINCODE,C'C'        ALLOW 'C'                                    
         BE    *+12                                                             
         CLI   LINCODE,C'S'        ELSE ALLOW ONLY 'S'                          
         BNE   FLDINV1                                                          
         SPACE 1                                                                
         LA    R1,ESTDATAH         A(TOP LINE)                                  
         SR    R1,R2               LESS A(THIS LINE)                            
         LPR   R1,R1               R1=DISPLACEMENT TO THIS LINE                 
         XR    R0,R0                                                            
         D     R0,=AL4(LINLNQ)     DETERMINE RELATIVE LINE NUMBER               
         SLL   R1,2                *4                                           
         LR    RF,R1               SAVE DISPLACEMENT INTO TABLE                 
         LA    R1,THSPGCOM(R1)     FIND SLOT IN INT COMML TABLE                 
         MVC   0(4,R3),0(R1)       SAVE INTERNAL COMMERCIAL NUMBER              
         LA    RF,THSPGDAS(RF)     FIND SLOT IN D/A TABLE                       
         MVC   4(4,R3),0(RF)       SAVE DISK ADDRESS                            
         LA    R3,PGLNQ(R3)                                                     
         SPACE 1                                                                
VALSS4   LA    R2,LINNEXT                                                       
         BCT   R4,VALSS2                                                        
         SPACE 1                                                                
         OI    STATUS2,WRITETOP    SET TO WRITE TOP OF SCREEN ELS. ONLY         
         GOTO1 AWRITEIT            WRITE BACK ANY CHANGES SO FAR                
         XI    STATUS2,WRITETOP                                                 
         SPACE 1                                                                
         NI    DISPMODE,ALL-LISTSCOM                                            
         MVI   PGSCOIND,0          RESET INDEX                                  
         BAS   RE,FSTTOSCO         GET 1ST SELECTION                            
         BNE   NO                                                               
         SPACE 1                                                                
         BAS   RE,DISSCOM          DISPLAY IT IF FOUND                          
         B     YES                 AND RETURN CC EQUAL                          
         EJECT                                                                  
*              ROUTINES TO CHECK FOR PFKEY PRESENCE ON COMML SELECT SCR         
         SPACE 1                                                                
CKSCOPFS NTR1                                                                   
         CLI   PFAID,PFREPORT      USER WANTS REPORT SCREEN                     
         BE    REPOUT                                                           
         SPACE 1                                                                
**NO-OP* XC    ESTNARR,ESTNARR     RESTORE NARRATIVE                            
*        OC    SVNARR,SVNARR       IF ANY                                       
*        BZ    CKSCOPF1                                                         
*        LA    RE,SVNARR                                                        
*        USING TACMD,RE                                                         
*        ZIC   R1,TACMLEN                                                       
*        SH    R1,=Y(TACMLNQ+1)                                                 
*        EX    R1,*+8                                                           
*        B     *+10                                                             
*        MVC   ESTNARR(0),TACMCOMM                                              
*        LA    R1,1(R1)                                                         
*        STC   R1,ESTNARRH+5       SET INPUT LENGTH                             
*        SPACE 1                                                                
*KSCOPF1 NI    ESTNARRH+1,X'DE'    UNPROTECT IT                                 
**NO-OP* OI    ESTNARRH+6,X'80'    TRANSMIT                                     
         SPACE 1                                                                
         CLI   PFAID,PFSTAY        IF REMAIN PFKEY HIT                          
         BNE   CKSCOPF2                                                         
         BAS   RE,THSTOSCO         DISPLAY THIS ONE AGAIN                       
         B     CKSCOPF4                                                         
         SPACE 1                                                                
CKSCOPF2 BAS   RE,NXTTOSCO         CHECK FOR ANOTHER COMMERCIAL                 
         BNE   *+12                                                             
CKSCOPF4 BAS   RE,DISSCOM          DISPLAY IT IF FOUND                          
         BE    YES                                                              
         SPACE                                                                  
         NI    DISPMODE,ALL-SELCTCOM                                            
         B     NO                  CONTINUE WITH COMMERCIAL LIST                
         EJECT                                                                  
*              ROUTINES TO SET CODE FOR (RE-)DISPLAYING SELECTED COMML          
         SPACE 1                                                                
FSTTOSCO ZIC   R1,PGSCOIND         SET TO DISPLAY FIRST ONE                     
         B     ALLTOSCO                                                         
LSTTOSCO LA    R1,2                SET TO DISPLAY PREVIOUS ONE                  
         B     ALLTOSCO                                                         
THSTOSCO LA    R1,1                SET TO DISPLAY THIS ONE AGAIN                
         B     ALLTOSCO                                                         
NXTTOSCO XR    R1,R1               SET TO DISPLAY NEXT ONE                      
         SPACE 1                                                                
ALLTOSCO NTR1                                                                   
         ZIC   RF,PGSCOIND         USE RELATIVE INDEX INTO LIST                 
         SR    RF,R1                                                            
         BM    NOPREV                                                           
         LA    R1,1(RF)                                                         
         STC   R1,PGSCOIND                                                      
         MH    RF,=AL2(PGLNQ)                                                   
         LA    RF,PGSCOTAB(RF)                                                  
         OC    0(PGLNQ,RF),0(RF)                                                
         BZ    NO                                                               
         MVC   TGCOM,0(RF)         SET SELECTED INT COMML NUMBER                
*                                                                               
         LA    R3,KEY              READ COMMERCIAL IN AIO                       
         USING TLDRD,R3                                                         
         MVC   TLDRDA,4(RF)        SELECTED COMML DISK ADDRESS                  
         GOTO1 GETREC                                                           
         B     YES                 RETURN CC EQU IF FOUND SOMETHING             
         EJECT                                                                  
*              ROUTINE TO VALIDATE ACTUAL PERFORMER SELECT FIELDS               
         SPACE 1                                                                
VALPSEL  NTR1                                                                   
*        XC    PGSPFTAB,PGSPFTAB                                                
         XCEF  PGSPFTAB,L'PGSPFTAB                                              
*                                                                               
         LA    R3,PGSPFTAB         R3=SELECT TAB FOR PERF LIST                  
         LA    R2,ESTDATAH         R2=1ST DATA LINE                             
         LA    R4,NLINEQU          R4=N'SCREEN LINES                            
         SPACE 1                                                                
VALPS2   CLI   LINCODEH+5,0                                                     
         BE    VALPS4                                                           
         TM    STATUS2,BUILDING    IF CHANGING                                  
         BZ    *+12                                                             
         CLI   LINCODE,C'C'        ALLOW 'C'                                    
         BE    *+12                                                             
         CLI   LINCODE,C'S'        ELSE ALLOW ONLY 'S'                          
         BNE   FLDINV1                                                          
         SPACE 1                                                                
         LA    R1,ESTDATAH         A(TOP LINE)                                  
         SR    R1,R2               LESS A(THIS LINE)                            
         LPR   R1,R1               R1=DISPLACEMENT TO THIS LINE                 
         XR    R0,R0                                                            
         D     R0,=AL4(LINLNQ)     DETERMINE RELATIVE LINE NUMBER               
         LR    RF,R1               SAVE RELATIVE LINE NUMBER                    
         MH    R1,=H'6'            RELATIVE LINE NUMBER * 6                     
         LA    R1,THSPGSRT(R1)     FIND SLOT IN CAST SORT TABLE                 
         MVC   0(6,R3),0(R1)       SAVE INTERNAL COMMERCIAL NUMBER              
         SLL   RF,2                RELATIVE LINE NUMBER * 4                     
         LA    RF,THSPGDAS(RF)     FIND SLOT IN D/A TABLE                       
         MVC   6(4,R3),0(RF)       SAVE DISK ADDRESS                            
         LA    R3,PGLNQ(R3)                                                     
         SPACE 1                                                                
VALPS4   LA    R2,LINNEXT                                                       
         BCT   R4,VALPS2                                                        
         SPACE 1                                                                
         OI    STATUS2,WRITETOP    SET TO WRITE TOP OF SCREEN ELS. ONLY         
         GOTO1 AWRITEIT            WRITE BACK ANY CHANGES SO FAR                
         XI    STATUS2,WRITETOP                                                 
         SPACE 1                                                                
         NI    DISPMD2,ALL-LISTSPRF                                             
         MVI   PGSPFIND,0          RESET INDEX                                  
         BAS   RE,FSTTOSPF         GET 1ST SELECTION                            
         BNE   NO                                                               
         SPACE 1                                                                
         OI    DISPMD2,SELCTPRF    SET WE'RE IN SELECTED PERFORMER MODE         
         BRAS  RE,CLRSCRN          CLEAR THE SCREEN                             
         BAS   RE,PERHEADS         INSURE PERFORMER DISPLAY HEADINGS            
         LA    R2,ESTDATAH         R2=A(TOP LINE)                               
         MVI   ELTYPE,TAESTPER     SET PERFORMER ESTIMATE EL. TYPE              
         XC    ELDATA,ELDATA                                                    
         BAS   RE,DISPER           DISPLAY IT IF FOUND                          
         B     YES                 AND RETURN CC EQUAL                          
         EJECT                                                                  
*              ROUTINE TO CHECK PFKEY PRESENCE ON PERFORMER SELECT SCR          
         SPACE 1                                                                
CKSPFPFS NTR1                                                                   
         CLI   PFAID,PFREPORT      USER WANTS REPORT SCREEN                     
         BE    REPOUT                                                           
         SPACE 1                                                                
         CLI   PFAID,PFSTAY        IF REMAIN PFKEY HIT                          
         BNE   CKSPFPF2                                                         
         BAS   RE,THSTOSPF         DISPLAY THIS ONE AGAIN                       
         B     CKSPFPF4                                                         
         SPACE 1                                                                
CKSPFPF2 BAS   RE,NXTTOSPF         CHECK FOR ANOTHER COMMERCIAL                 
         BNE   CKSPFPF5                                                         
CKSPFPF4 BRAS  RE,CLRSCRN          CLEAR THE SCREEN                             
         LA    R2,ESTDATAH         R2=A(TOP LINE)                               
         MVI   ELTYPE,TAESTPER     SET PERFORMER ESTIMATE EL. TYPE              
         XC    ELDATA,ELDATA                                                    
         BAS   RE,DISPER           DISPLAY IT IF FOUND                          
         BE    YES                                                              
         SPACE 1                                                                
CKSPFPF5 NI    DISPMD2,ALL-SELCTPRF                                             
         BAS   RE,THSCSTDA         DIG OUT D/A FOR CURRENT PAGE                 
         BAS   RE,PERHEADS         INSURE PERFORMER LIST HEADINGS               
         B     NO                  CONTINUE WITH PERFORMER LIST                 
         SPACE 2                                                                
*              ROUTINES TO SET CODE FOR (RE-)DISPLAYING SELECTED PERF.          
         SPACE 1                                                                
FSTTOSPF ZIC   R1,PGSPFIND         SET TO DISPLAY FIRST ONE                     
         B     ALLTOSPF                                                         
THSTOSPF LA    R1,1                SET TO DISPLAY THIS ONE AGAIN                
         B     ALLTOSPF                                                         
NXTTOSPF XR    R1,R1               SET TO DISPLAY NEXT ONE                      
         SPACE 1                                                                
ALLTOSPF NTR1                                                                   
         ZIC   RF,PGSPFIND         USE RELATIVE INDEX INTO LIST                 
         SR    RF,R1                                                            
         BM    NOPREV                                                           
         LA    R1,1(RF)                                                         
         STC   R1,PGSPFIND                                                      
         MH    RF,=AL2(PGLNQ)                                                   
         LA    RF,PGSPFTAB(RF)                                                  
         OC    0(PGLNQ,RF),0(RF)                                                
         BZ    NO                                                               
*                                                                               
         LA    R3,KEY              READ COMMERCIAL IN AIO                       
         USING TLDRD,R3                                                         
         MVC   TLDRDA,6(RF)        SELECTED COMML DISK ADDRESS                  
         GOTO1 GETREC                                                           
         B     YES                 RETURN CC EQU IF FOUND SOMETHING             
         EJECT                                                                  
*              ROUTINE TO LIST HYPOTHETICAL COMMERCIALS                         
         SPACE 1                                                                
         USING LINED,R2                                                         
         USING TAESD,R4                                                         
LISTHCOM NTR1                                                                   
         OI    DISPMODE,LISTHYP    SET LISTING HYPO COMMERCIALS                 
         BRAS  RE,CLRSCRN                                                       
         LA    R2,ESTDATAH         R2=1ST DATA LINE                             
         LA    R0,NLINEQU          R0=N'SCREEN LINES                            
         B     LSTHC4              WE HAVE FIRST ELEMENT ALREADY                
         SPACE 1                                                                
LSTHC2   MVC   HYCODATA,SAVEHCOM   SET LAST DISPLAYED FOR NEXT                  
         BAS   RE,NXTHYCOM         GET NEXT HYPOTHETICAL COMMERCIAL             
         BNE   YES                                                              
         SPACE 1                                                                
LSTHC4   CLM   R0,1,CLINES         RETURN IF NOT ENOUGH LINES LEFT              
         BL    NO                                                               
         L     R4,FULL             R4=A(EL.) FROM LOOKUP                        
         CLI   TAESHCOM+9,0        HYPO COMML IS MORE THAN 10 CHARS             
         BE    LSTHC4A                                                          
         MVC   LINCID(L'TAESHCOM),TAESHCOM  DISPLAY HYPO COMML CODE             
         B     LSTHC4F                                                          
*                                                                               
LSTHC4A  GOTO1 =A(CHPACK),DMCB,(C'U',TAESHCOM),LINCID,RR=RELO                   
*                                                                               
LSTHC4F  GOTO1 MEDVAL,DMCB,(X'80',TAESHMED)                                     
         MVC   LINMEDIA,TGMENAME   DISPLAY MEDIA                                
*                                                                               
         MVC   HCOMNAME,SPACES                                                  
         MVI   THSLINES,1                                                       
         SPACE 1                                                                
         CLI   NEWEST,C'Y'         IF NEW ESTIMATE DISPLAY                      
         BNE   LSTCH5                                                           
         BAS   RE,COMHEADS         INSURE WE HAVE HYPO COMML HEADS              
         GOTO1 ADISSHRS            DISPLAY STEREO VERSION OF RHS                
         B     LSTCH6                                                           
         SPACE 1                                                                
LSTCH5   LA    RF,LINLNQ           ELSE, DISPLAY NORMAL RHS                     
         GOTO1 AOPUNSCN,DMCB,LINRHSH,((RF),ESTLSTRH) DISP. OPTIONS              
         BNE   NO                  NOT ENOUGH ROOM                              
         SPACE 1                                                                
LSTCH6   MVC   SAVEHCOM,TAESHCOM   SAVE LAST DISPLAYED COMMERCIAL CODE          
         NI    LINCODEH+1,X'DE'    UNPROTECT SELECT FIELD                       
         MVC   LINNAME,HCOMNAME    HYPO COMMERCIAL NAME                         
         SPACE 1                                                                
         BAS   RE,SETPGINF         ADD THIS COMML TO PAGE TABLE IF NEC.         
         SPACE 1                                                                
         ZIC   RE,THSLINES         RE=N'LINES USED                              
         ZIC   R1,CLINES           R1=N'LINES REQUIRED                          
         CR    RE,R1                                                            
         BNL   *+6                                                              
         LR    RE,R1               RE=N'LINES TO SKIP                           
         SR    R0,RE               DECREASE N'LINES REMAINING                   
         MH    RE,=AL2(LINLNQ)                                                  
         AR    R2,RE               BUMP TO NEXT AVAILABLE LINE                  
         B     LSTHC2              LOOK FOR ANOTHER HYPO. COMMERCIAL            
         EJECT                                                                  
*              ROUTINES TO GET FIRST(/NEXT) HYPOTHETICAL COMM'L                 
         SPACE 1                                                                
         USING TAESD,R4                                                         
NXTHYCOM NTR1                                                                   
         ZIC   RE,HYCODATA+L'TAESHCOM-1                                         
         LA    RE,1(RE)            BUMP COMMERCIAL                              
         STC   RE,HYCODATA+L'TAESHCOM-1                                         
         BAS   RE,GETHYCOM                                                      
         BE    YES                                                              
         B     NO                                                               
         SPACE 2                                                                
GETHYCOM NTR1                                                                   
         MVI   ELTYPE,TAESTHCO     SET HYPO. COMMERCIAL EL. TYPE                
         MVC   ELDATA,HYCODATA     AND DATA                                     
         BAS   RE,LOOKUP           LOOK FOR A HYPOTHETICAL COMMERCIAL           
         L     R4,FULL                                                          
         CLI   TAESEL,TAESELQ      IF WE FOUND ESTIMATE ELEMENT                 
         BNE   NO                                                               
         CLI   TAESTYPE,TAESTHCO   ONLY TYPE MUST MATCH                         
         BNE   NO                                                               
         BE    YES                                                              
         B     NO                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE HYPO. COMMERCIAL SELECT FIELDS               
         SPACE 1                                                                
VALSEL   NTR1                                                                   
*        XC    PGHYPTAB,PGHYPTAB                                                
         XCEF  PGHYPTAB,L'PGHYPTAB                                              
*                                                                               
         LA    R3,PGHYPTAB         R3=SELECT TABLE                              
         LA    R2,ESTDATAH         R2=1ST DATA LINE                             
         LA    R0,NLINEQU          R0=N'SCREEN LINES                            
         SPACE 1                                                                
VALS2    CLI   LINCODEH+5,0                                                     
         BE    VALS4                                                            
         TM    STATUS2,BUILDING    IF CHANGING                                  
         BZ    *+12                                                             
         CLI   LINCODE,C'C'        ALLOW 'C'                                    
         BE    *+12                                                             
         CLI   LINCODE,C'S'        ELSE ALLOW ONLY 'S'                          
         BNE   FLDINV1                                                          
         SPACE 1                                                                
         MVC   0(PGLNQ,R3),LINCID  SAVE COMMERCIAL CODE                         
         LA    R3,PGLNQ(R3)                                                     
         SPACE 1                                                                
VALS4    LA    R2,LINNEXT                                                       
         BCT   R0,VALS2                                                         
         SPACE 1                                                                
         OI    STATUS2,WRITETOP    SET TO WRITE TOP OF SCREEN ELS. ONLY         
         GOTO1 AWRITEIT            WRITE BACK ANY CHANGES SO FAR                
         XI    STATUS2,WRITETOP                                                 
         SPACE 1                                                                
         NI    DISPMODE,ALL-LISTHYP                                             
         MVI   PGHYPIND,0          RESET INDEX                                  
         BAS   RE,FSTTOHYP         GET 1ST SELECTION                            
         BNE   NO                                                               
         SPACE 1                                                                
         BAS   RE,HYPOINIT         LOAD SCREEN                                  
         BAS   RE,DISHYCOM         DISPLAY IT IF FOUND                          
         B     YES                 AND RETURN CC EQUAL                          
         EJECT                                                                  
*              ROUTINES TO CHECK FOR PFKEY PRESENCE ON HYPO SCREENS             
         SPACE 1                                                                
CKHYPPFS NTR1                                                                   
         CLI   PFAID,PFREPORT      USER WANTS REPORT SCREEN                     
         BE    REPOUT                                                           
         CLI   NEWEST,C'Y'         IF SELECTABLE HYPO PERFORMER LIST            
         BNE   *+16                                                             
         BAS   RE,VALHPSEL         VALIDATE SELECTIONS                          
         BE    HYPOOUT             SOMETHING'S BEEN SELECTED                    
         BAS   RE,CLRHSCRN         CLEAR/SET SCREEN FIELDS                      
         SPACE 1                                                                
         TM    DISPMODE,HYPOPER    IF WE'RE IN HYPO PERF. MODE                  
         BZ    CKHYP2                                                           
         CLI   PFAID,PFSTAY        IF USER WANTS TO STAY ON THIS SCREEN         
         BE    CKHYP1                                                           
         CLI   PFAID,PFHYPPG       OR USER WANTS ANOTHER SCREEN                 
         BE    CKHYP1                                                           
         CLI   PFAID,0             OR IF USER PRESSED ENTER                     
         BNE   CKHYP6                                                           
         CLC   HYPPAGE(1),HYPPAGES AND HAVEN'T REACHED LAST PAGE                
         BNL   CKHYP6                                                           
CKHYP1   BAS   RE,DISHYPRF         DISPLAY HYPO PERFORMERS                      
         B     YES                                                              
         SPACE 1                                                                
CKHYP2   CLI   PFAID,PFRETURN      USER WANTS TO LEAVE DISPLAY                  
         BNE   *+12                                                             
         MVI   SAVEHCOM,X'FF'      SET END OF HYPO COMM'LS                      
         B     CKHYP6                                                           
         SPACE 1                                                                
         CLI   NEWEST,C'Y'         IF NEW ESTIMATE DISPLAY                      
         BNE   *+12                                                             
         CLI   PFAID,PFSRETRN      AND USER WANTS SAVE AND RETURN               
         BE    CKHYP6              GO BACK TO LIST                              
         CLI   PFAID,PFSTAY        IF USER WANTS TO STAY ON SCRN                
         BE    CKHYP3                                                           
         CLI   PFAID,PFHYPPG       OR USER WANTS ANOTHER SCREEN                 
         BE    CKHYP3                                                           
         CLI   PFAID,0             OR IF USER PRESSED ENTER                     
         BNE   CKHYP3D                                                          
         CLC   HYPPAGE(1),HYPPAGES AND HAVEN'T REACHED LAST PAGE                
         BNL   CKHYP3D                                                          
CKHYP3   BAS   RE,THSTOHYP         DIG OUT THIS KEY                             
         B     CKHYP4                                                           
         SPACE 1                                                                
CKHYP3D  MVI   HYPPAGE,C'1'        RESET PAGE NUMBERS                           
         MVI   HYPPAGES,C'1'                                                    
         CLI   PFAID,PFBEGIN       USER WANTS 1ST HYPO COMM'L                   
         BNE   *+12                                                             
         BAS   RE,FSTTOHYP         DIG OUT FIRST KEY                            
         B     CKHYP4              AND DISPLAY IT                               
         SPACE 1                                                                
         CLI   PFAID,PFPREV        USER WANTS LAST HYPO COMM'L                  
         BNE   *+12                                                             
         BAS   RE,LSTTOHYP         DIG OUT PREVIOUS KEY                         
         B     CKHYP4              AND DISPLAY IT                               
         SPACE 1                                                                
         BAS   RE,NXTTOHYP         CHECK FOR ANOTHER HYPO. COMMERCIAL           
         BNE   *+12                                                             
CKHYP4   BAS   RE,DISHYCOM                                                      
         BE    YES                                                              
         SPACE 1                                                                
         BAS   RE,NXTHYCOM         SET FOR NEXT HYPO IF AROUND                  
CKHYP6   BAS   RE,RESTSCRN         RESTORE LAST SCREEN                          
         B     NO                  CONTINUE WITH COMMERCIAL LIST                
         SPACE 2                                                                
*              ROUTINES TO SET CODE FOR (RE-)DISPLAYING HYPO COMM'LS            
         SPACE 1                                                                
FSTTOHYP ZIC   R1,PGHYPIND         SET TO DISPLAY FIRST ONE                     
         B     ALLTOHYP                                                         
LSTTOHYP LA    R1,2                SET TO DISPLAY PREVIOUS ONE                  
         B     ALLTOHYP                                                         
THSTOHYP LA    R1,1                SET TO DISPLAY THIS ONE AGAIN                
         B     ALLTOHYP                                                         
NXTTOHYP XR    R1,R1               SET TO DISPLAY NEXT ONE                      
         SPACE 1                                                                
ALLTOHYP NTR1                                                                   
         ZIC   RF,PGHYPIND         USE RELATIVE INDEX INTO LIST                 
         SR    RF,R1                                                            
         BM    NOPREV                                                           
         LA    R1,1(RF)                                                         
         STC   R1,PGHYPIND                                                      
         MH    RF,=AL2(PGLNQ)                                                   
         LA    RF,PGHYPTAB(RF)                                                  
         CLI   0(RF),0                                                          
         BE    NO                                                               
         MVC   HYCODATA(PGLNQ),0(RF)                                            
         B     YES                 RETURN CC EQ IF FOUND SOMETHING              
         EJECT                                                                  
*              ROUTINE TO VALIDATE HYPO PERFORMER SELECT FIELDS                 
         SPACE 1                                                                
VALHPSEL NTR1                                                                   
*        XC    PGHPFTAB,PGHPFTAB                                                
         XCEF  PGHPFTAB,L'PGHPFTAB                                              
*                                                                               
         LA    R3,PGHPFTAB         R3=SELECT TABLE FOR HYPO PERF LIST           
         LA    R4,NLINHEQU         R0=N'SCREEN LINES                            
         LA    R2,HYPDATAH         R2=1ST DATA LINE                             
         USING LINHED,R2                                                        
*                                                                               
VALHPS2  CLI   LINHCDH+5,0                                                      
         BE    VALHPS4                                                          
         TM    STATUS2,BUILDING    IF CHANGING                                  
         BZ    *+12                                                             
         CLI   LINHCODE,C'C'       ALLOW 'C'                                    
         BE    *+12                                                             
         CLI   LINHCODE,C'S'       ELSE ALLOW ONLY 'S'                          
         BNE   FLDINV1                                                          
*                                                                               
         LA    R1,HYPDATAH         A(TOP LINE)                                  
         SR    R1,R2               LESS A(THIS LINE)                            
         LPR   R1,R1               R1=DISPLACEMENT TO THIS LINE                 
         XR    R0,R0                                                            
         D     R0,=AL4(LINHLNQ)    DETERMINE RELATIVE LINE NUMBER               
         MH    R1,=H'7'                                                         
         LA    R1,THSPGHPF(R1)     SAVE HYPO PERFORMER INFO                     
         MVC   0(7,R3),0(R1)                                                    
         LA    R3,PGLNQ(R3)                                                     
VALHPS4  LA    R2,LINHNEXT                                                      
         BCT   R4,VALHPS2                                                       
*                                                                               
         MVI   PGHPFIND,0          RESET INDEX                                  
         BAS   RE,FSTTOHPF         GET 1ST SELECTION                            
         BNE   NO                                                               
         L     R4,FULL             R4=A(ESTIMATE EL.)                           
         LA    R3,HYPDATAH         R3=A(1ST DATA LINE)                          
         BAS   RE,DISSHYPF         DISPLAY SELECTED HYPO PERFORMER              
         B     YES                                                              
         EJECT                                                                  
*              ROUTINES TO SET CODE FOR (RE-)DISPLAYING HYPO PERFS              
         SPACE 1                                                                
FSTTOHPF ZIC   R1,PGHPFIND         SET TO DISPLAY FIRST ONE                     
         B     ALLTOHPF                                                         
THSTOHPF LA    R1,1                SET TO DISPLAY THIS ONE AGAIN                
         B     ALLTOHPF                                                         
NXTTOHPF XR    R1,R1               SET TO DISPLAY NEXT ONE                      
         B     ALLTOHPF                                                         
*                                                                               
ALLTOHPF NTR1                                                                   
         ZIC   RF,PGHPFIND         USE RELATIVE INDEX INTO LIST                 
         SR    RF,R1                                                            
         BM    NOPREV                                                           
         LA    R1,1(RF)                                                         
         STC   R1,PGHPFIND                                                      
         MH    RF,=AL2(PGLNQ)                                                   
         LA    RF,PGHPFTAB(RF)                                                  
         OC    0(PGLNQ,RF),0(RF)                                                
         BZ    NO                                                               
         XC    ELDATA,ELDATA                                                    
         MVC   ELDATA(7),0(RF)                                                  
         XC    HYPFDATA,HYPFDATA                                                
         MVC   HYPFDATA(7),0(RF)   SAVE ESTIMATE DATA                           
*                                                                               
         MVI   ELTYPE,TAESTHPE     SET HYPO PERFORMER                           
         ZIC   R1,DISPMD2          SAVE DISPLAY STATUS 2                        
         OI    DISPMD2,SELCTHPF    SET IN SELECTED HYPO PERF MODE               
         BAS   RE,LOOKUP                                                        
         STC   R1,DISPMD2          RESTORE DISPLAY STATUS 2                     
         B     XIT                 CC CODE SET & FULL                           
         EJECT                                                                  
*              ROUTINE TO VALIDATE HYPOTHETICAL INPUT SCREEN                    
         SPACE 1                                                                
         DROP  R2                  WAS BEING USED WITH LINHED                   
         SPACE 1                                                                
         USING LINHED,R3                                                        
VALHYPO  NTR1                                                                   
         TM    STATUS2,BUILDING    GET OUT IF NOT BUILDING THE RECORD           
         BZ    XIT                                                              
         CLI   PFAID,PFRETURN      USER WANTS TO RETURN-IGNORE CHANGES          
         BE    XIT                                                              
         SPACE 1                                                                
         TM    DISPMODE,HYPOCOM    IF THIS IS HYPO COMMERCIAL                   
         BZ    *+12                                                             
         BAS   RE,VALHCOM          VALIDATE HYPO COMMERCIAL FIELDS              
         BNE   VALH15              USER WANTS TO DELETE THIS ONE                
         SPACE 1                                                                
         BAS   RE,VALHPFS          VALIDATE HYPO-PERFORMERS                     
         TM    STATUS2,ADDESTEL    DO WE NEED TO ADD LAST EST. ELEMENT          
         BZ    *+8                                                              
         BAS   RE,INESTEL          ADD ESTIMATE ELEMENT TO RECORD               
         BAS   RE,COPY             TAKE CARE OF COPIES                          
         TM    DISPMODE,HYPOCOM    IF THIS IS HYPO COMMERCIAL                   
         BZ    VALH15                                                           
         MVI   ELTYPE,TAESTHCO     RESET ELEMENT TYPE                           
         SPACE 1                                                                
         LA    RF,PGHYPTAB         RF=A(TABLE OF COMMLS SELECTED)               
         ZIC   R1,PGHYPIND                                                      
         SH    R1,=H'1'                                                         
         BM    VALH11                                                           
         MH    R1,=AL2(PGLNQ)                                                   
         AR    RF,R1               RF=A(CURRENT LOCATION IN TABLE)              
         SPACE 1                                                                
         CLC   0(PGLNQ,RF),TGCID   IF COMML ID HASN'T CHANGED THEN SKIP         
         BE    VALH15                                                           
         LA    RF,PGLNQ(RF)        MUST BE ADDING NEW ONE - NEED                
         LA    R0,2*PGLNQ+1(R1)    TO INSERT AFTER CURRENT IN TABLE             
         LH    R1,=AL2(PGHYPMAX*PGLNQ)                                          
         SR    R1,R0               R1=L'TABLE TO SHIFT OVER                     
         BM    VALH15                                                           
         SPACE 1                                                                
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK(0),0(RF)      MOVE FIRST TO BLOCK                          
         SH    R1,=AL2(PGLNQ)                                                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PGLNQ(0,RF),BLOCK   AND THEN BACK IN                             
         SPACE 1                                                                
VALH11   MVC   0(PGLNQ,RF),TGCID   SAVE NEW COMML FOR PAGING                    
         ZIC   R1,PGHYPIND                                                      
         AH    R1,=H'1'            BUMP INDEX NUMBER FOR NEW COMML              
         STC   R1,PGHYPIND                                                      
         SPACE 1                                                                
VALH15   MVC   SHYPRFPG,HYPRFPG    SAVE PAGE NUMBER                             
         CLI   NEWEST,C'Y'         IF NEW ESTIMATE DISPLAY                      
         BNE   *+8                                                              
         MVI   HYPRFPG,X'FF'       FAKE OUT TRIM -NO HYPO PRFS DELETES          
         GOTO1 AWRITEIT            WRITE THE RECORD BACK NOW                    
         MVC   HYPRFPG,SHYPRFPG    RESTORE PAGE NUMBER                          
         BAS   RE,SETHYPPG         SET PAGE NUMBER DISPLAY                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE HYPOTHETICAL COMMERCIAL DATA                 
         SPACE 1                                                                
VALHCOM  NTR1                                                                   
         MVI   ELTYPE,TAESTHCO     SET HYPO-COMM'L ELEMENT TYPE                 
         XC    ELDATA,ELDATA                                                    
         SPACE 1                                                                
         CLI   PFAID,PFDELHYP      USER WANTS TO DELETE THIS COMM'L             
         BE    NO                  RETURN CC NE                                 
         SPACE 1                                                                
         LA    R2,HYPCOMH          REQUIRE COMMERCIAL CODE                      
         GOTO1 ANY                                                              
         CLI   5(R2),L'TAESHCOM    IF MORE THAN 10 CHARS, PACK IT               
         BNH   VALHC5                                                           
*                                                                               
         GOTO1 =A(CHPACK),DMCB,(C'P',WORK),TGCID,RR=RELO                        
         B     *+10                                                             
*                                                                               
VALHC5   MVC   TGCID,WORK          SET COMMERCIAL FOR THESE PERFORMERS          
         LA    R4,ELDATA           SET ELEMENT DATA                             
         USING TAESDATA,R4                                                      
         MVC   TAESHCOM,TGCID      SAVE COMMERCIAL CODE IN EST. EL.             
         SPACE 1                                                                
         LA    R2,HYPMEDH          REQUIRE MEDIA                                
         GOTO1 ANY                                                              
         GOTO1 MEDVAL,DMCB,WORK                                                 
         BNE   FLDINV1                                                          
         MVC   TAESHMED,TGMEEQU    SAVE EQUATE                                  
         SPACE 1                                                                
         LA    R2,HYPLENH          REQUIRE L'COMMERCIAL                         
         GOTO1 VALINUM                                                          
         MVC   TAESHLEN,ACTUAL     LENGTH                                       
         MVC   HYCODATA,TAESDATA   SAVE HYPO COMMERCIAL EL. DATA                
         SPACE 1                                                                
         ZAP   SUBELCNT,=P'0'      CLEAR N'SUB ELEMENTS                         
         MVC   ANXTSUB,ASUBELS     SET A(FIRST SUB-ELEMENT)                     
         XC    OPTWORK(OPTWKLNQ),OPTWORK CLEAR OPTION WORK AREAS                
         MVI   OPTOKBIT,OPOKHYCO   SET OK BIT FOR OPTION VALIDATION             
         SPACE 1                                                                
         GOTO1 AOPSCAN,DMCB,HYPOPTH    ** SCAN RHS **                           
         GOTO1 (RF),(R1),HYPOPT2H                                               
         GOTO1 (RF),(R1),HYPOPT3H                                               
         GOTO1 (RF),(R1),HYPOPT4H                                               
         SPACE 1                                                                
         CLI   HYPNAMEH+5,0        IS THERE A NAME                              
         BE    VALHCX                                                           
         L     R4,ANXTSUB          R4=A(NEXT OPEN SPOT)                         
         SPACE 1                                                                
         ZIC   RE,HYPNAMEH+5       RE=L'SUB-ELEMENT DATA                        
         LA    RF,2(RE,R4)         ADD 1 FOR CODE AND 1 FOR L'NAME              
         ST    RF,ANXTSUB          SAVE A(NEXT SPOT)                            
         SPACE 1                                                                
         S     RF,ASUBELS          SUBTRACT A(FIRST SPOT)                       
         CH    RF,=AL2(L'SUBELBLK)                                              
         BH    TOOMANY             BLOCK WILL BECOME TOO LARGE                  
         SPACE 1                                                                
         USING TAESSBCD,R4                                                      
         MVI   TAESSBCD,OPCOMNM    SUB-ELEMENT CODE                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TAESSBDT+1(0),HYPNAME  SUB-ELEMENT DATA IS NAME                  
         LA    RE,2(RE)                                                         
         STC   RE,TAESSBDT         L'NAME+1 IS FIRST DATA BYTE                  
         SPACE 1                                                                
         AP    SUBELCNT,=P'1'      ADD ONE TO COUNT                             
         SPACE 1                                                                
VALHCX   BAS   RE,INESTEL          ADD ESTIMATE ELEMENT TO RECORD               
         B     YES                 RETURN CC EQ                                 
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE HYPOTHETICAL PERFORMERS                      
         SPACE 1                                                                
VALHPFS  NTR1                                                                   
         MVI   ELTYPE,TAESTHPE     SET HYPO-PERFORMER ELEMENT TYPE              
         MVI   OPTOKBIT,OPOKHYPE                                                
         LA    R0,NLINHEQU         R0=N'SCREEN LINES                            
         LA    R3,HYPDATAH         R3=A(1ST SCREEN LINE)                        
         XC    SCRNTAB,SCRNTAB                                                  
         LA    R4,SCRNTAB          R4=KEY DATA TABLE FOR SCREEN                 
         SPACE 1                                                                
VALHPFS4 CLI   NEWEST,C'Y'         IF NEW ESTIMATE DISPLAY                      
         BNE   VALHPFS5                                                         
         TM    DISPMD2,SELCTHPF    AND LISTING SELECTABLE HYPO PERFS            
         BO    VALHPFS5                                                         
         TM    LINHNUMH+1,X'20'    IF LINE IS NOT PROTECTED                     
         BO    VALHPFS6                                                         
VALHPFS5 BAS   RE,CHKHPRF          ARE ANY OF THE PERFORMER FIELDS I/P          
         BE    VALHPFS8            NO - CONTINUE WITH SAME KEY AS LAST          
         TM    STATUS2,ADDESTEL    DO WE NEED TO ADD LAST EST. ELEMENT          
         BZ    *+8                                                              
         BAS   RE,INESTEL          ADD ESTIMATE ELEMENT TO RECORD               
         SPACE 1                                                                
         ZAP   SUBELCNT,=P'0'      CLEAR N'SUB ELEMENTS                         
         MVC   ANXTSUB,ASUBELS     SET A(FIRST SUB-ELEMENT)                     
         XC    OPTWORK(OPTWKLNQ),OPTWORK CLEAR OPTION WORK AREAS                
         SPACE 1                                                                
         GOTO1 AVALHPRF            VALIDATE PERFORMER INPUT FIELDS              
         SPACE 1                                                                
         BAS   RE,SETSHPF          SET HYPO PERF IN THSPGHPF TABLE              
         SPACE 1                                                                
         USING SCRND,R4                                                         
         MVC   SCRNCODE,LINHCODE                                                
         MVC   SCRNDATA,ELDATA     SAVE KEY DATA IN SCREEN TABLE                
         SPACE 1                                                                
         CLI   NEWEST,C'Y'         IF NEW ESTIMATE                              
         BE    VALHPFS8            NO COPY BETWEEN LINES ALLOWED                
         CLI   LINHRHSH+5,2        IF ONLY TWO CHARACTERS INPUT                 
         BNE   VALHPFS8                                                         
         CLI   LINHRHS,C'='        AND IT STARTS WITH AN EQUAL SIGN             
         BNE   VALHPFS8                                                         
         CLI   LINHRHS+1,C'A'      AND ENDS WITH 'A' THROUGH                    
         BL    VALHPFS8                                                         
         CLC   LINHRHS+1(1),HYPLSTD LETTER OF LAST DATA LINE                    
         BH    VALHPFS8                                                         
         MVC   SCRNCOPY,LINHRHS+1  SAVE CODE OF COPY DESTINATION                
         BAS   RE,INESTEL          ADD A DUMMY EL. FOR THIS ONE                 
         LA    R4,SCRNNEXT                                                      
VALHPFS6 LA    R3,LINHNEXT         BUMP TO NEXT LINE WITH DATA                  
         BCT   R0,*+8                                                           
         B     VALHPFSX                                                         
*                                                                               
         BAS   RE,CHKHPRF          ARE ANY OF THE PERFORMER FIELDS I/P          
         BNE   VALHPFS4            CONTINUE                                     
         CLI   LINHRHSH+5,0        ELSE INSURE NOTHING ON RHS                   
         BNE   NOINPUT                                                          
         B     VALHPFS6                                                         
         SPACE 1                                                                
VALHPFS8 TM    STATUS2,ADDESTEL    IF HAVEN'T SEEN PERF YET THEN SKIP           
         BZ    VALHPFS9                                                         
         CLI   NEWEST,C'Y'         IF NOT SELECTABLE HYPO PERF LIST             
         BNE   *+12                                                             
         TM    DISPMD2,SELCTHPF                                                 
         BZ    VALHPFS9                                                         
         GOTO1 AOPSCAN,DMCB,LINHRHSH   VALIDATE RHS OPTIONS                     
         SPACE 1                                                                
VALHPFS9 LA    R3,LINHNEXT         BUMP TO NEXT LINE                            
         LA    R4,SCRNNEXT                                                      
         BCT   R0,VALHPFS4                                                      
VALHPFSX B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DETERMINES IF ANY HYPO-PERFORMER KEY DATA INPUT          
         SPACE 1                                                                
         USING LINHED,R3                                                        
CHKHPRF  DS    0H                                                               
         CLI   LINHNUMH+5,0                                                     
         BNER  RE                                                               
         CLI   LINHCATH+5,0                                                     
         BNER  RE                                                               
         CLI   LINHCAMH+5,0                                                     
         BNER  RE                                                               
         CLI   LINHUNIH+5,0                                                     
         BNER  RE                                                               
         CLI   LINHLFTH+5,0                                                     
         BNER  RE                                                               
         CLI   LINHDBLH+5,0                                                     
         BNER  RE                                                               
         CLI   LINHCRPH+5,0                                                     
         BNER  RE                                                               
         BR    RE                                                               
         SPACE 2                                                                
*              ROUTINE TO SET CHANGED HYPO PERFORMER INFO IN TABLE              
         SPACE 1                                                                
SETSHPF  NTR1                                                                   
         CLI   NEWEST,C'Y'         IF NEW ESTIMATE DISPLAY                      
         BNE   SETSHPFX                                                         
         TM    DISPMD2,SELCTHPF    IF VALIDATING SELECTABLE HYPO PERFS          
         BO    SETSHPFX                                                         
         LA    R1,HYPDATAH         A(TOP LINE)                                  
         SR    R1,R3               LESS A(TOP LINE)                             
         LPR   R1,R1               R1=DISPLACEMENT TO THIS LINE                 
         XR    R0,R0                                                            
         D     R0,=AL4(LINHLNQ)    DETERMINE RELATIVE LINE NUMBER               
         MH    R1,=H'7'                                                         
         LA    R1,THSPGHPF(R1)                                                  
         MVC   0(7,R1),ELDATA      SAVE INFO ON HYPO PERFORMER                  
SETSHPFX B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY HYPOTHETICAL COMMERCIALS                      
         SPACE 1                                                                
         USING TAESD,R4                                                         
DISHYCOM NTR1                                                                   
         BAS   RE,GETHYCOM         LOOK UP EST. EL.                             
         BNE   NO                                                               
         GOTO1 FLDVAL,DMCB,(X'21',HYPCOMH),(X'80',HYPLSTRH)  CLEAR SCRN         
         SPACE 1                                                                
         CLI   PGHYPIND,1          IF THIS IS NOT FIRST PAGE                    
         BE    *+12                                                             
         NI    HYPMSG2H+1,X'FB'    SET PF MSG TO HIGH INTENSITY                 
         B     *+8                                                              
         OI    HYPMSG2H+1,X'0C'    ELSE TURN IT OFF                             
         OI    HYPMSG2H+6,X'80'                                                 
         SPACE 1                                                                
         L     R4,FULL             R4=A(HYPO. COMMERCIAL EL.)                   
         MVC   HYCODATA,TAESDATA   SAVE HYPO COMMERCIAL EL. DATA                
*        CLI   TAESTYPE,TAESTHCO                                                
*        BNE   DISHYCO3                                                         
         CLI   TAESHCOM+9,0        HYPO MORE THAN 10 CHARS                      
         BE    DISHYCO3                                                         
         MVC   HYPCOM(L'TAESHCOM),TAESHCOM   DISPLAY COMMERCIAL CODE            
         B     DISHYCO5                                                         
*                                                                               
DISHYCO3 GOTO1 =A(CHPACK),DMCB,(C'U',TAESHCOM),HYPCOM,RR=RELO                   
*                                                                               
DISHYCO5 GOTO1 MEDVAL,DMCB,(X'80',TAESHMED)                                     
         MVC   HYPMED,TGMENAME               MEDIA                              
         EDIT  (1,TAESHLEN),(2,HYPLEN)       LENGTH                             
         MVC   HCOMNAME,SPACES               CLEAR NAME                         
         SPACE 1                                                                
         GOTO1 AOPUNSCN,DMCB,HYPOPTH,HYPOPT4H  DISPLAY OPTIONS                  
         SPACE 1                                                                
         MVC   HYPNAME,HCOMNAME    DISPLAY NAME                                 
         SPACE 1                                                                
         BAS   RE,DISHYPRF         NOW DISPLAY PERFORMERS IF AROUND             
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY HYPOTHETICAL PERFORMERS                       
         SPACE 1                                                                
         USING LINHED,R3                                                        
         USING TAESD,R4                                                         
DISHYPRF NTR1                                                                   
         XR    R2,R2                                                            
         CLI   NEWEST,C'Y'         IF NEW ESTIMATE DISPLAY                      
         BNE   *+12                                                             
         TM    DISPMD2,SELCTHPF    AND SELECTED HYPO PERFORMER                  
         BZ    *+8                                                              
         LA    R2,X'80'            SET TO SKIP PROTECTED FIELDS                 
         GOTO1 FLDVAL,DMCB,(X'21',HYPNUMH),((R2),HYPLSTRH)  CLEAR SCRN          
         SPACE 1                                                                
         ZIC   R1,HYPPFPG          CALC NEXT PAGE NUMBER                        
         CLI   PFAID,PFHYPPG       IF PFKEY TO GO TO ANOTHER PAGE USED          
         BE    DISH1                  THEN DONE                                 
         IC    R1,HYPPAGE          ELSE SET PAGE NUMBER TO THIS ONE             
         CLI   PFAID,PFSTAY        IF PFKEY TO REMAIN ON PAGE USED              
         BE    DISH1                  THEN DONE                                 
         LA    R1,1(R1)            ELSE BUMP TO NEXT PAGE                       
         CLM   R1,1,HYPPAGES       IF PAGE NUMBER IS GT N'PAGES                 
         BNH   *+8                                                              
         LA    R1,C'1'             START OVER AT PAGE 1                         
DISH1    STC   R1,HYPPAGE          MOVE THIS PAGE NUMBER TO MESSAGE             
         SPACE 1                                                                
         BCTR  R1,0                SUBTRACT 1 TO MATCH ELEMENT                  
         STC   R1,HYPRFPG                                                       
         NI    HYPRFPG,X'0F'       CVT PAGE NUMBER TO BINARY                    
         SPACE 1                                                                
         STC   R1,HYPPFPG          SET NEW PFKEY PAGE NO. TO PREV PAGE          
         CLI   HYPPFPG,C'1'                                                     
         BNL   *+8                                                              
         MVI   HYPPFPG,C'1'                                                     
         OI    HYPPFPGH+6,X'80'                                                 
         SPACE 1                                                                
         BAS   RE,SETHYPPG         SET PAGE NUMB. DISPLAY (DOES LOOKUP)         
         SPACE 1                                                                
         CLC   HYPPAGES(1),HYPPAGE IF TOTAL PAGES LT CURRENT PAGE NO.           
         BNL   *+10                                                             
         MVC   HYPPAGES(1),HYPPAGE THEN MAKE THE SAME (FROM ADDING NEW)         
         SPACE 1                                                                
         XC    THSPGHPF,THSPGHPF                                                
         L     R4,FULL             R4=A(ELEMENT FOUND BY LOOKUP)                
         LA    R3,HYPDATAH         R3=A(1ST DATA LINE)                          
         LA    R2,NLINHEQU         R2=MAX N'LINES                               
         B     *+12                                                             
DISH2    BAS   RE,NEXTEL           LOOK FOR ANOTHER                             
         BNE   DISHX                                                            
         CLI   TAESTYPE,TAESTHPE   TYPE MUST MATCH                              
         BNE   DISHX                                                            
         CLC   TAESHPG,HYPRFPG     MUST ALSO MATCH PAGE NUMBER                  
         BNE   DISH2                                                            
         SPACE 1                                                                
         LA    R1,HYPDATAH         A(TOP LINE)                                  
         SR    R1,R3               LESS A(TOP LINE)                             
         LPR   R1,R1               R1=DISPLACEMENT TO THIS LINE                 
         XR    R0,R0                                                            
         D     R0,=AL4(LINHLNQ)    DETERMINE RELATIVE LINE NUMBER               
         MH    R1,=H'7'                                                         
         LA    R1,THSPGHPF(R1)                                                  
         MVC   0(7,R1),TAESDATA    SAVE INFO ON HYPO PERFORMER                  
         SPACE 1                                                                
         GOTO1 ADISHPLN            DISPLAY ONE HYPO PERFORMER                   
         SPACE 1                                                                
         ZIC   R1,THSLINES                                                      
         SR    R2,R1               R2=N'LINES REMAINING                         
         BNP   DISHX               NO MORE ROOM                                 
         MH    R1,=AL2(LINHLNQ)                                                 
         AR    R3,R1               BUMP LINE POINTER BY N'LINES USED            
         B     DISH2               AND PROCESS NEXT                             
         SPACE 1                                                                
DISHX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SET PAGE NUMBER DISPLAY FOR HYPO SCREEN               
         SPACE 1                                                                
*                                  RETURNS FULL=A(EL. FROM LOOKUP)              
SETHYPPG NTR1                                                                   
         OI    HYPPGSH+1,X'0C'     TURN OFF PAGE COUNT MESSAGE                  
         OI    HYPPGSH+6,X'80'                                                  
         SPACE 1                                                                
         MVI   ELTYPE,TAESTHPE     SET TO LOOK FOR TOTAL N'PAGES USED           
         XC    ELDATA,ELDATA                                                    
         BAS   RE,LOOKUP           LOOK FOR A HYPOTHETICAL PERFORMER            
         L     R4,FULL                                                          
         MVI   BYTE,0                                                           
         SPACE 1                                                                
STHPG10  CLI   TAESTYPE,TAESTHPE   TYPE MUST MATCH                              
         BNE   STHPG20                                                          
         CLC   BYTE,TAESHPG        SAVE BIGGEST PAGE NUMBER                     
         BNL   *+10                                                             
         MVC   BYTE,TAESHPG                                                     
         BAS   RE,NEXTEL           LOOK FOR ANOTHER ELEMENT                     
         BE    STHPG10                                                          
         SPACE 1                                                                
STHPG20  ZIC   RE,BYTE             DISPLAY N'PAGES USED                         
         LA    RF,1(RE)                                                         
         EDIT  (RF),(1,HYPPAGES),ZERO=NOBLANK                                   
         SPACE 1                                                                
         CLC   HYPPAGES(1),HYPPAGE IF TOTAL PAGES LT CURRENT PAGE NO.           
         BL    *+10                                                             
         LTR   RE,RE               OR IF THERE'S MORE THAN ONE PAGE             
         BZ    *+8                                                              
         NI    HYPPGSH+1,X'FB'     TURN ON N'PAGES DISPLAY                      
         SPACE 1                                                                
         B     XIT                 RETURN FULL=A(EL. FROM LOOKUP)               
         EJECT                                                                  
*              ROUTINES TO VALIDATE HYPO PERFORMER SELECT SCREEN                
         SPACE 1                                                                
VALSHPRF NTR1                                                                   
         TM    STATUS2,BUILDING    GET OUT IF NOT BUILDING THE RECORD           
         BZ    XIT                                                              
         TM    DISPMODE,HYPOCOM    IF THIS IS HYPO COMMERCIAL                   
         BZ    *+8                                                              
         BAS   RE,VALHCOM          VALIDATE HYPO COMMERCIAL                     
         BAS   RE,VALHPFS          VALIDATE HYPO PERFORMER                      
         TM    STATUS2,ADDESTEL    DO WE NEED TO ADD LAST EST. ELEMENT          
         BZ    *+8                                                              
         BAS   RE,INESTEL          ADD ESTIMATE ELEMENT TO RECORD               
         SPACE 1                                                                
         MVC   SHYPRFPG,HYPRFPG    SAVE PAGE NUMBER                             
         MVI   HYPRFPG,X'FF'       FAKE OUT TRIM -NO HYPO PRFS DELETES          
         GOTO1 AWRITEIT            WRITE THE RECORD BACK NOW                    
         MVC   HYPRFPG,SHYPRFPG    RESTORE PAGE NUMBER                          
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO CHK PFKEY PRESENCE ON HYPO PERF SELECT SCRN           
         SPACE 1                                                                
CKSHPPFS NTR1                                                                   
         CLI   PFAID,PFREPORT      USER WANTS REPORT SCREEN                     
         BE    REPOUT                                                           
         CLI   PFAID,PFSTAY        IF REMAIN PFKEY HIT                          
         BNE   CKSHPPF2                                                         
         BAS   RE,THSTOHPF         DISPLAY THIS ONE AGAIN                       
         B     CKSHPPF4                                                         
*                                                                               
CKSHPPF2 BAS   RE,NXTTOHPF         CHECK FOR ANOTHER HYPO PERFORMER             
         BNE   CKSHPPF5                                                         
CKSHPPF4 L     R4,FULL             R4=A(ESTIMATE EL.)                           
         LA    R3,HYPDATAH         R3=A(1ST DATA LINE)                          
         BAS   RE,DISSHYPF         DISPLAY SELECTED HYPO PERFORMER              
         B     YES                                                              
*                                                                               
CKSHPPF5 NI    DISPMD2,ALL-SELCTHPF                                             
         XC    HYPFDATA,HYPFDATA   CLEAR SAVED ELEMENT DATA                     
         BAS   RE,CLRHSCRN         SET SCREEN FOR SELECTABLE HYPO PERFS         
         BAS   RE,THSHPFKY         DIG OUT KEY FOR CURRENT PAGE                 
         BAS   RE,DISHYPRF         DISPLAY SELECTED HYPO PERFORMERS             
         B     NO                                                               
         SPACE 2                                                                
*              SET CORRECT PAGE FOR HYPO PERFORMERS                             
         SPACE 1                                                                
THSHPFKY LA    R1,THSPGHPF                                                      
         ZIC   RF,TAESHPG-TAESDATA(R1)                                          
         LA    RF,1(RF)                                                         
         EDIT  (RF),(1,HYPPAGE),ZERO=NOBLANK                                    
         MVI   PFAID,PFSTAY                                                     
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY SELECTED HYPO PERFORMER                       
         SPACE 1                                                                
DISSHYPF NTR1                                                                   
         OI    DISPMD2,SELCTHPF    SET SELECTED HYPO PERFORMER                  
         BAS   RE,CLRHSCRN         SET HYPO SCREEN                              
         GOTO1 FLDVAL,DMCB,(X'21',HYPNUMH),(X'80',HYPLSTRH) CLR SCRN            
         GOTO1 ADISHPLN            DISPLAY HYPO PERFORMER LINE                  
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY MISCELLANEOUS ADDITIONS SCREEN                
         SPACE 1                                                                
DISMISC  NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(X'21',MSCTXTH),(X'80',999)  CLEAR SCREEN            
         SPACE 1                                                                
         MVI   ELCODE,TAEMELQ      LOOK FOR ESTIMATE MISC. ADDITION ELS         
         L     R4,ATIA                                                          
         LA    R3,MSCTXTTH                                                      
         USING LINMD,R3            R3=A(SCREEN ENTRY)                           
         BAS   RE,GETEL                                                         
         BNE   DMISCX                                                           
         USING TAEMD,R4            R4=A(ESTIMATE MISC. ADDITION EL.)            
         SPACE 1                                                                
DMISC10  ZIC   R1,TAEMLEN                                                       
         SH    R1,=AL2(TAEMLNQ+1)                                               
         BM    DMISC20             NO TEXT                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LINMTXT(0),TAEMTXT  TEXT TO SCREEN                               
         SPACE 1                                                                
DMISC20  OC    TAEMAMT,TAEMAMT     IF THERE'S AN AMOUNT                         
         BZ    DMISC30                                                          
         EDIT  (4,TAEMAMT),(9,LINMAMT),2,FLOAT=-  DISPLAY IT                    
         SPACE 1                                                                
DMISC30  LA    R3,LINMNEXT         BUMP TO NEXT SCREEN ENTRY                    
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BE    DMISC10                                                          
         SPACE 1                                                                
DMISCX   BAS   RE,DISCVR           DISPLAY COVER SHEET                          
         SPACE 1                                                                
         OI    STATUS,MISCDSPD     SET DISPLAYED MISC. ADDITIONS                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY COVER SHEET                                   
         SPACE 1                                                                
DISCVR   NTR1                                                                   
         MVI   ELCODE,TAENELQ      LOOK FOR ESTIMATE NARRATIVE ELS.             
         L     R4,ATIA                                                          
         LA    R2,MSCNARRH                                                      
         BAS   RE,GETEL                                                         
         BNE   DCVRX                                                            
         USING TAEND,R4            R4=A(ESTIMATE NARRATIVE EL.)                 
         SPACE 1                                                                
DCVR10   ZIC   R1,TAENLEN                                                       
         SH    R1,=AL2(TAENLNQ+1)                                               
         BM    DCVR20              NO TEXT                                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),TAENNARR    TEXT TO SCREEN                               
         SPACE 1                                                                
DCVR20   ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         BAS   RE,NEXTEL           GET NEXT ELEMENT                             
         BE    DCVR10                                                           
         SPACE 1                                                                
DCVRX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CHECK FOR PFKEY I/P ON MISC ADDITIONS SCREEN          
         SPACE 1                                                                
CKMSCPFS NTR1                                                                   
         CLI   PFAID,PFREPORT      USER WANTS REPORT SCREEN                     
         BE    REPOUT                                                           
         SPACE 1                                                                
         CLI   PFAID,PFSTAY        ELSE, IF USER WANTS TO STAY ON SCRN          
         BNE   *+12                                                             
         BAS   RE,DISMISC          RE-DISPLAY THE SCREEN                        
         B     YES                                                              
         SPACE 1                                                                
         CLI   PFAID,PFBEGIN       USER WANTS 1ST PAGE OF COMM'L LIST           
         BNE   CKMSCPF5                                                         
         BAS   RE,FSTCOMDA         DIG OUT FIRST KEY                            
         OC    NXTCOMDA,NXTCOMDA   IF WE DIDN'T GET BACK D/A OF COMML           
         BNZ   *+14                                                             
         MVC   SAVEHCOM,STARTHYP   SET TO START AT HYPO BEG                     
         B     *+8                                                              
         NI    STATUS,ALL-ENDOFLST TURN OFF END OF COMMERCIAL LIST              
         BAS   RE,RESTSCRN         RESTORE LAST SCREEN                          
         B     LOWX                RETURN MINUS CC                              
         SPACE 1                                                                
CKMSCPF5 BAS   RE,RESTSCRN         ELSE RESTORE LAST SCREEN                     
         B     NO                  CONTINUE WITH COMMERCIAL LIST                
         SPACE 3                                                                
*              ROUTINE RETURNS CC EQ IF MISCELLANEOUS ADDITIONS PRESENT         
         SPACE 1                                                                
ANYMISC  NTR1                                                                   
         CLI   NEWEST,C'Y'         IF NOT NEW ESTIMATE DISPLAY                  
         BE    NO                                                               
         TM    STATUS,MISCDSPD     TEST ALREADY DISPLAYED THEM                  
         BO    NO                                                               
         L     R4,ATIA                                                          
         MVI   ELCODE,TAEMELQ      LOOK FOR ESTIMATE MISC ADDITION ELS.         
         BAS   RE,GETEL                                                         
         BE    YES                                                              
         L     R4,ATIA                                                          
         MVI   ELCODE,TAENELQ      ELSE LOOK FOR ESTIMATE NARRATIVE ELS         
         BAS   RE,GETEL                                                         
         B     XIT                 RETURN CC                                    
         EJECT                                                                  
*              ROUTINE TO ADD ESTIMATE ELEMENTS                                 
         SPACE 1                                                                
INESTEL  NTR1                                                                   
         XC    ELEMENT,ELEMENT     BUILD THE ELEMENT                            
         LA    R4,ELEMENT                                                       
         USING TAESD,R4                                                         
         MVI   TAESEL,TAESELQ      ELEMENT CODE                                 
         MVI   TAESLEN,TAESLNQ     INITIAL ELEMENT LENGTH                       
         MVC   TAESSEQ,ELSEQ       SEQUENCE NUMBER                              
         MVC   TAESTYPE,ELTYPE     TYPE                                         
         MVC   TAESDATA,ELDATA     DATA                                         
         SPACE 1                                                                
         ZAP   DUB,SUBELCNT        IF WE HAVE SUB-ELEMENTS TO ADD               
         BZ    INEST4                                                           
         L     RF,ASUBELS          RF=A(START OF SUB-ELS)                       
         L     RE,ANXTSUB                                                       
         SR    RE,RF               RE=L'SUB-EL BLOCK                            
         ZIC   R1,TAESLEN                                                       
         AR    R1,RE               INCREMENT ELEMENT LENGTH                     
         CH    R1,=H'255'                                                       
         BH    TOOMANY             ELEMENT TOO LONG                             
         STC   R1,TAESLEN                                                       
         EX    RE,*+8              (DON'T BOTHER WITH BCTR)                     
         B     *+10                                                             
         MVC   TAESSBCD(0),0(RF)   MOVE SUB-ELEMENTS TO ELEMENT                 
         SPACE 1                                                                
INEST4   CVB   R1,DUB                                                           
         STC   R1,TAESSBNO         STORE N'SUB-ELEMENTS                         
         SPACE 1                                                                
         MVC   AIO,AIO2                                                         
         GOTO1 ADDELEM             ** ADD THE ELEMENT **                        
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         ZIC   R1,ELSEQ            BUMP ELEMENT SEQUENCE NUMBER                 
         AH    R1,=H'1'                                                         
         STC   R1,ELSEQ                                                         
         SPACE 1                                                                
         NI    STATUS2,ALL-ADDESTEL TURN OFF ADD PENDING                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO COPY ESTIMATE ELEMENTS                                
         SPACE 1                                                                
         USING SCRND,R3                                                         
COPY     NTR1                                                                   
         MVC   AIO,AIO2                                                         
         LA    R3,SCRNTAB          R3=A(SCREEN TABLE)                           
         LA    R0,NLINEQU                                                       
COP2     CLI   SCRNCOPY,C'A'       LOOK FOR COPY CODES                          
         BNL   COP5                                                             
COP4     LA    R3,SCRNNEXT                                                      
         BCT   R0,COP2                                                          
         MVC   AIO,AIO1                                                         
         B     XIT                 NO MORE                                      
         SPACE 1                                                                
COP5     MVC   ELDATA,SCRNDATA     SET KEY DATA OF RECEIVING KEY                
         BAS   RE,LOOKUP2          FIND DUMMY ELEMENT ON RECORD                 
         BNE   COP4                                                             
         L     R2,FULL             R2=A(ELEMENT TO REPLACE)                     
         SPACE 1                                                                
         LA    RF,SCRNTAB          LOOK UP ENTRY OF ELEMENT TO COPY             
         LA    RE,NLINEQU                                                       
COP6     CLC   SCRNCOPY,SCRNCODE-SCRND(RF)  MATCH ON LHS LETTER CODE            
         BE    *+16                                                             
         LA    RF,SCRNLNQ(RF)                                                   
         BCT   RE,COP6                                                          
         B     COP4                MISSING CODE OF ELEMENT TO COPY              
         SPACE 1                                                                
         MVC   ELDATA,SCRNDATA-SCRND(RF) SET KEY DATA OF EL. TO COPY            
         BAS   RE,LOOKUP2                                                       
         BNE   COP4                                                             
         L     R4,FULL                                                          
         USING TAESD,R4                                                         
         MVC   ELEMENT,TAESEL      MOVE ELEMENT TO W/S                          
         LA    R4,ELEMENT          R4=A(NEW ELEMENT)                            
         SPACE 1                                                                
         MVC   TAESSEQ,TAESSEQ-TAESD(R2)   SAVE ORIGINAL SEQ. NUMBER            
         MVC   TAESDATA,TAESDATA-TAESD(R2) AND DATA                             
         SPACE 1                                                                
         MVI   0(R2),X'FF'         SET TO DELETE ORIGINAL ELEMENT               
         MVI   ELCODE,X'FF'                                                     
         GOTO1 DELL,DMCB,0         DELETE ORIG EL (WON'T CREAM ELEMENT)         
         SPACE 1                                                                
         GOTO1 ADDELEM             ADD NEW ELEMENT TO RECORD                    
         B     COP4                                                             
         EJECT                                                                  
*              ROUTINE SETS CURRENT PG INFO. FOR COMML/CAST/HYPO LISTS          
         SPACE 1                                                                
*                                  R2=A(CURRENT LINE)                           
SETPGINF NTR1                                                                   
         TM    DISPMODE,LISTHYP    IF LISTING HYPO COMMERCIALS                  
         BO    SETIN4              SKIP TO END                                  
         SPACE 1                                                                
         LA    R1,ESTDATAH         A(TOP LINE)                                  
         SR    R1,R2               LESS A(THIS LINE)                            
         LPR   R1,R1               R1=DISPLACEMENT TO THIS LINE                 
         XR    R0,R0                                                            
         D     R0,=AL4(LINLNQ)     DETERMINE RELATIVE LINE NUMBER               
         LR    RF,R1               (SAVE IT)                                    
         SLL   R1,2                *4                                           
         LA    R1,THSPGDAS(R1)     USE IT TO FIND SLOT IN TABLE                 
         MVC   0(4,R1),DMDSKADD    SAVE D/A OF EACH RECORD ON THIS PAGE         
         SPACE 1                                                                
         TM    DISPMD2,LISTSPRF    TEST LISTING PERFORMERS                      
         BO    *+12                                                             
         TM    DISPMODE,LISTPRFS                                                
         BZ    SETIN2                                                           
         MH    RF,=H'6'                                                         
         LA    RF,THSPGSRT(RF)                                                  
         MVC   0(6,RF),TGCSORT     SAVE ALL CAST SORT KEYS ON THIS PAGE         
         GOTO1 ,DMCB,PGCSTMAX,PGCSTIND,PGCSTTAB  SET PARMS FOR SET1STDA         
         B     SETIN6                                                           
         SPACE 1                                                                
SETIN2   SLL   RF,2                MUST BE DISPLAYING COMMERCIALS               
         LA    RF,THSPGCOM(RF)     SET TO SAVE ALL INTERNAL COMMERCIAL          
         MVC   0(4,RF),TGCOM                       NUMBERS ON THIS PAGE         
         MVC   NXTCOMDA,DMDSKADD   SAVE D/A OF LAST COMM'L DISPLAYED            
         SPACE 1                                                                
SETIN4   GOTO1 ,DMCB,PGCOMMAX,PGCOMIND,PGCOMTAB  SET PARMS FOR SET1STDA         
         SPACE 1                                                                
SETIN6   LA    RE,ESTDATAH         IF WE'RE ON FIRST LINE                       
         CR    RE,R2                                                            
         BNE   SETINX                                                           
         GOTO1 SET1STDA            SAVE CURRENT D/A FOR PAGING                  
         SPACE 1                                                                
SETINX   B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO ADD FIRST D/A FOR PAGE TO PAGE TABLE                  
         SPACE 1                                                                
*                                  P1=MAX. N'ENTRIES IN TABLE                   
*                                  P2=A(PAGE INDEX)                             
*                                  P3=A(PAGE TABLE)                             
SET1STDA NTR1                                                                   
         LM    R2,R4,0(R1)         R2=MAX, R3=A(INDEX), R4=A(TABLE)             
         SPACE 1                                                                
         ZIC   RF,0(R3)            RF=INDEX                                     
         CR    RF,R2               CHECKED IF WE'VE ALREADY SAVED MAX           
         BL    SETDA2                                                           
         BCTR  R2,0                REACHED MAX - SHIFT LAST MAX-1 D/A'S         
         LR    RF,R4                                                            
         MVC   0(PGLNQ,RF),PGLNQ(RF)  TO THE TOP SO THAT WHEN DONE              
         LA    RF,PGLNQ(RF)                                                     
         BCT   R2,*-10                                                          
         B     SETDA4              WE'RE NOW POINTING TO LAST OPEN SLOT         
         SPACE 1                                                                
SETDA2   LA    R1,1(RF)            SET INDEX OF THIS ENTRY FOR PAGING           
         STC   R1,0(R3)                                                         
         MH    RF,=AL2(PGLNQ)      PREV. INDEX * L'ENTRY                        
         AR    RF,R4               NOW HAVE RELATIVE DISP. INTO LIST            
         SPACE 1                                                                
SETDA4   XC    0(PGLNQ,RF),0(RF)   PRE-CLEAR THIS ENTRY                         
         SPACE 1                                                                
         MVC   0(4,RF),DMDSKADD    SET D/A IN NEXT OPEN SLOT IN TABLE           
         SPACE 1                                                                
         TM    DISPMODE,LISTHYP    IF LISTING HYPO COMMERCIALS                  
         BZ    *+10                                                             
         MVC   0(PGLNQ,RF),SAVEHCOM  SAVE HYPO COMML CODE INSTEAD               
         SPACE 1                                                                
         CLI   0(R3),1             IF THIS IS NOT FIRST PAGE                    
         BE    *+12                                                             
         NI    ESTMSG2H+1,X'FB'    SET PF MSG TO HIGH INTENSITY                 
         B     *+8                                                              
         OI    ESTMSG2H+1,X'0C'    ELSE TURN IT OFF                             
         OI    ESTMSG2H+6,X'80'                                                 
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE MOVES COMMERCIAL HEADINGS TO LIST SCREEN                 
         SPACE 2                                                                
COMHEADS DS    0H                                                               
         OI    ESTMSG4H+1,X'0C'    TURN OFF PF MSGS                             
         TM    DISPMODE,SELCTCOM   IF REGULAR COMMERCIAL LIST                   
         BO    COMHEAD5                                                         
         TM    STATUS2,BUILDING    OR IF WE'RE BUILDING                         
         BZ    *+8                                                              
         NI    ESTMSG4H+1,X'FB'    TURN ON PF MSGS                              
*                                                                               
COMHEAD5 OI    ESTMSG4H+6,X'80'                                                 
         MVC   ESTLHED,HDCOMML     MOVE IN COMMERCIAL HEADINGS                  
         MVC   ESTRHED,HDCOMMR                                                  
         CLI   DISPMODE,LISTSCOM   IF SELECTABLE COMMERCIAL LIST                
         BNE   *+10                                                             
         MVC   ESTRHED,HDSCOMMR    SET DIFFERENT RHS TITLE                      
         TM    DISPMODE,LISTHYP    IF HYPO COMML LIST W/STEREO ACTIVE           
         BZ    *+10                                                             
         MVC   ESTRHED,HDSHCOMR    SET DIFFERENT RHS TITLE                      
         MVC   ESTMSG4,HDM4CO      SET COMM'L PFS                               
         OI    ESTMSGH+1,X'0C'     SET CID= FIELD ZERO INTENSITY                
         OI    ESTMSG4H+6,X'80'                                                 
         MVC   ESTMSG5,HDM5CO                                                   
         NI    ESTMSG5H+1,X'FB'    SET TO HIGH INTENSITY, PF15=MISC             
         OI    ESTMSG5H+6,X'80'                                                 
*                                                                               
         TM    DISPMODE,SELCTCOM   IF SELECTED COMMERCIAL DISPLAYED             
         BZ    *+8                                                              
         OI    ESTMSG5H+1,X'0C'    SET TO ZERO-INTENSITY, PF15=MISC             
*                                                                               
         ZIC   RF,SAVEFNUM         RF=COMM'L FIELD NO. FOR HELP                 
         B     ALLHEADS                                                         
         EJECT                                                                  
*              ROUTINE MOVES PERFORMER HEADINGS TO LIST SCREEN                  
         SPACE 2                                                                
PERHEADS DS    0H                                                               
         MVC   ESTMSG+4(12),TGCID  MOVE COMMERCIAL TO SCREEN                    
         MVC   ESTLHED,HDPERFL     MOVE IN PERFORMER HEADINGS                   
         TM    TGSYSTAT,TASYSPID   USING PID?                                   
         BZ    *+10                                                             
         MVC   ESTLHED,HDPERFLP                                                 
         MVC   ESTRHED,HDPERFR                                                  
         TM    DISPMD2,SELCTPRF    IF PERFORMER SELECT SCREEN                   
         BO    PERHEAD2            JUST SET DIFFERENT LEFT HAND SIDE            
         TM    DISPMD2,LISTSPRF    IF SELECTABLE PERFOMER LIST                  
         BZ    PERHEAD5                                                         
         MVC   ESTRHED,HDSPERFR    SET DIFFERENT RIGHT HAND SIDE AND            
PERHEAD2 MVC   ESTLHED,HDSPERFL    SET DIFFERENT LEFT HAND SIDE                 
         TM    TGSYSTAT,TASYSPID   USING PID?                                   
         BZ    *+10                                                             
         MVC   ESTLHED,HDSPERFP                                                 
PERHEAD5 NI    ESTMSGH+1,X'FB'     SET CID= FIELD TO HIGH INTENSITY             
*                                                                               
         MVC   ESTMSG4,HDM4PE      SET PERF PFS                                 
         NI    ESTMSG4H+1,X'FB'    TURN THEM ON                                 
         OI    ESTMSG4H+6,X'80'                                                 
         MVC   ESTMSG5,HDM5PE                                                   
         NI    ESTMSG5H+1,X'FB'    TURN THEM ON                                 
         OI    ESTMSG5H+6,X'80'                                                 
         TM    DISPMD2,LISTSPRF    IF SELECTABLE PERFORMER LIST                 
         BO    *+16                                                             
         TM    DISPMD2,SELCTPRF    IF PERFORMER SELECT SCREEN                   
         BZ    *+12                                                             
         OI    ESTMSG4H+1,X'0C'    SET TO ZERO-INTENSITY, PF14=RETURN           
         OI    ESTMSG5H+1,X'0C'    SET TO ZERO-INTENSITY, PF15=NEXT             
*                                                                               
         ZIC   RF,SAVEFNUM                                                      
         AH    RF,=H'2'            SET RHS FIELD NUMBERS TO SAVE+2              
         B     ALLHEADS                                                         
         SPACE 2                                                                
*              PERFORM HEADING MAINTENANCE FOR COMM'L. & PERF. SCREENS          
         SPACE 1                                                                
         USING LINED,R2                                                         
ALLHEADS NTR1  ,                   RF=FIELD NO. FOR RHS HELP                    
         OI    ESTLHEDH+6,X'80'    TRANSMIT HEADINGS                            
         OI    ESTRHEDH+6,X'80'                                                 
         OI    ESTMSGH+6,X'80'     TRANSMIT COMM'L MEMO                         
         LA    R0,NLINEQU          R0=N'SCREEN LINES                            
         LA    R2,ESTDATAH                                                      
         STC   RF,LINRHSX          RE-SET RHS FIELD NUMBERS                     
         LA    R2,LINNEXT                                                       
         BCT   R0,*-8                                                           
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SAVE CURRENT SCREEN AND LOAD HYPO SCREEN              
         SPACE 1                                                                
         USING LINHED,R2                                                        
HYPOINIT NTR1                                                                   
         BAS   RE,SAVESCRN         FIRST SAVE CURRENT SCREEN                    
         MVI   OVERLAY,SCR0C       SET HYPO SCREEN OVERLAY                      
         BAS   RE,LOADSCRN         LOAD IT                                      
         OI    DISPMODE,HYPOCOM    SET WE'RE IN HYPO COMMERCIAL MODE            
         MVI   HYPRFPG,0           CLEAR HYPO PERFORMER PAGE                    
         XC    HYPFDATA,HYPFDATA   CLEAR SAVED ELEMENT DATA                     
         BAS   RE,CLRHSCRN         CLEAR HYPO SCREEN                            
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO SET HYPO PERFORMER SCREEN                             
         SPACE 1                                                                
CLRHSCRN NTR1                                                                   
         CLI   NEWEST,C'Y'         IF SELECTABLE HYPO PERFORMER LIST            
         BNE   *+8                                                              
         BRAS  RE,SETSSCRN         SET SPECIAL SUB-SCREEN                       
*                                                                               
         MVI   HYPHED,C' '         CLEAR HEADING FOR SELECT COLUMN              
         BRAS  RE,TSTSEL           TEST LIST W/SEL FLD (RETURNS BYTE)           
         CLI   BYTE,C'Y'           IF SELECT FIELD                              
         BNE   *+8                                                              
         MVI   HYPHED,C'S'         SET HEADING FOR SELECT COLUMN                
         OI    HYPHEDH+6,X'80'                                                  
         NI    HYPMSG3H+1,X'FB'    TURN ON PF14=RETURN & PF15=PG                
         OI    HYPMSG3H+6,X'80'                                                 
         TM    STATUS2,BUILDING    IF BUILDING RECORD                           
         BZ    *+12                                                             
         NI    HYPMSG4H+1,X'FB'    TURN ON PF16=DEL                             
         OI    HYPMSG4H+6,X'80'                                                 
         NI    HYPPFPGH+1,X'FB'    TURN ON PG # FOR PF15                        
         NI    HYPPFPGH+1,X'D3'    AND UNPROTECT IT                             
         OI    HYPPFPGH+6,X'80'                                                 
*                                                                               
         TM    DISPMD2,SELCTHPF    IF DISPLAYING A SELECTED HYPO PERF           
         BZ    CLRHSCR2                                                         
         OI    HYPPGSH+1,X'0C'     TURN OFF PAGE COUNT MESSAGE                  
         OI    HYPPGSH+6,X'80'                                                  
         OI    HYPMSG4H+1,X'0C'    TURN OFF PF16=DEL                            
         OI    HYPMSG3H+1,X'0C'    TURN OFF PF14=RETURN & PF15=PG               
         OI    HYPPFPGH+1,X'0C'    TURN OFF PG # FOR PF15                       
         OI    HYPPFPGH+1,X'20'    AND PROTECT IT                               
*                                                                               
CLRHSCR2 LA    R0,NLINHEQU         R0=N'SCREEN LINES                            
         LA    R2,HYPDATAH         R2=A(1ST SCREEN LINE)                        
         LA    R4,=C'ABCDEFGHIJKL' R4=A(LHS LETTER TABLE)                       
*                                                                               
CLRHSCR5 MVC   LINHCODE,0(R4)      SET LHS LETTER CODE                          
         OI    LINHCDH+1,X'20'     AND PROTECT IT                               
         OI    LINHCDH+6,X'80'                                                  
         GOTO1 FLDVAL,DMCB,(X'04',LINHNUMH),LINHCRPH  UNPROTECT LHS             
         NI    LINHRHSH+1,X'D3'    UNPROTECT RHS                                
         OI    LINHRHSH+6,X'80'                                                 
*                                                                               
         CLI   BYTE,C'Y'           IF SELECT FIELD PRESENT                      
         BNE   CLRHSCR6                                                         
         MVI   LINHCODE,0          CLEAR SELECT FIELD                           
         MVI   LINHCDH+5,0                                                      
         NI    LINHCDH+1,X'DE'     AND UNPROTECT IT                             
         OI    LINHRHSH+1,X'20'    PROTECT RHS                                  
         B     CLRHSCR9                                                         
*                                                                               
CLRHSCR6 TM    DISPMD2,SELCTHPF    IF DISPLAYING A SELECTED HYPO PERF           
         BZ    CLRHSCR9                                                         
         LA    R1,HYPDATAH         AND IF FIRST LINE                            
         CR    R2,R1                                                            
         BNE   CLRHSCR7                                                         
         GOTO1 FLDVAL,DMCB,(X'07',LINHNUMH),LINHCRPH  UNPR/XMIT/CLR LHS         
         B     CLRHSCR9                                                         
CLRHSCR7 GOTO1 FLDVAL,DMCB,(X'09',LINHNUMH),LINHCRPH  ELSE, PROTECT/CLR         
*                                                                               
CLRHSCR9 LA    R4,1(R4)            BUMP TO NEXT LETTER                          
         LA    R2,LINHNEXT         BUMP TO NEXT LINE ON SCREEN                  
         BCT   R0,CLRHSCR5         LOOP                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SAVE CURRENT SCREEN AND LOAD MISCADD SCREEN           
         SPACE 1                                                                
MISCINIT NTR1                                                                   
         BAS   RE,SAVESCRN         FIRST SAVE CURRENT SCREEN                    
         MVI   OVERLAY,SCR0B       SET MISC. ADDITIONS SCREEN OVERLAY           
         BAS   RE,LOADSCRN         LOAD IT                                      
         OI    DISPMODE,MISCADD    SET WE'RE IN MISC. ADDITIONS MODE            
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINE TO SAVE CURRENT SCREEN                                   
         SPACE 1                                                                
SAVESCRN NTR1                                                                   
         MVC   SAVTWA(1),ESTMSGH+1 SAVE ATTRIBUTE BYTE OF COMM'L MEMO           
         OI    ESTMSGH+1,X'0C'     INSURE IT'S OFF                              
         OI    ESTMSGH+6,X'80'                                                  
         LA    R0,SAVTWA+1         SAVE CURRENT SCREEN                          
         LA    R1,ESTWORK-ESTOVLYH                                              
         LA    RE,ESTOVLYH                                                      
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO LOAD A SCREEN                                         
*                                  OVERLAY=PHASE NUMBER                         
LOADSCRN NTR1                                                                   
         LA    R3,ESTOVLYH         R3=LOAD ADDRESS                              
         GOTO1 LOADSOPH,DMCB,1     LOAD A SCREEN                                
         XC    CONHEAD,CONHEAD                                                  
         BAS   RE,TRANSMIT         TRANSMIT ENTIRE SCREEN                       
         SPACE 1                                                                
         CLI   NEWEST,C'Y'         IF NEW ESTIMATE DISPLAY                      
         BNE   LOADSX                                                           
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,RE                                                         
         OI    TIOBINDS,TIOBSUBS   SET CHANGING SUB-SCREEN                      
         MVI   TIOBAID,X'00'       SET SUB-SCREEN FOR NORMAL SCREEN             
LOADSX   B     XIT                                                              
         SPACE 2                                                                
*              RESTORE A SAVED SCREEN                                           
RESTSCRN NTR1                                                                   
         MVC   ESTMSGH+1(1),SAVTWA RESTORE ATTRIB BYTE OF COMM'L MEMO           
         OI    ESTMSGH+6,X'80'                                                  
         LA    R0,ESTOVLYH                                                      
         LA    R1,ESTWORK-ESTOVLYH                                              
         LA    RE,SAVTWA+1         SAVED TO ACTIVE                              
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         BAS   RE,TRANSMIT         TRANSMIT ENTIRE SCREEN                       
*                                                                               
         CLI   NEWEST,C'Y'         IF NEW ESTIMATE DISPLAY                      
         BNE   RESTX                                                            
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,RE                                                         
         OI    TIOBINDS,TIOBSCRN   SET OVERRIDING SCREEN FOR F=Y                
         MVI   TIOBCNT,SCR2C       RESTORE ORIGINAL SCREEN CODE                 
RESTX    NI    DISPMODE,ALL-HYPO-MISCADD  SET NO LONGER HYPO/MISCADD            
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO TRANSMIT AN ENTIRE SCREEN                             
TRANSMIT DS    0H                                                               
         LA    R1,ESTOVLYH                                                      
         SR    RF,RF                                                            
TRAN2    CLI   0(R1),0             LOOP UNTIL END OF SCREEN                     
         BER   RE                                                               
         OI    6(R1),X'80'         TRANSMIT                                     
         OI    7(R1),X'80'         AND SET TO RE-BUILD LINE                     
         IC    RF,0(R1)                                                         
         AR    R1,RF                                                            
         B     TRAN2                                                            
         EJECT                                                                  
*              MISC. STUFF                                                      
         SPACE 1                                                                
DISPVER  NTR1                                                                   
         CLI   TGCTSTTY,TASTTYPP   ONLY DISPLAY VERSION FOR PROGRAMMERS         
         BNE   DVERX                                                            
         GOTO1 ACTVOUT,DMCB,(X'20',FULL)                                        
         OC    0(4,R1),0(R1)                                                    
         BZ    DVERX                                                            
         L     R3,0(R1)                                                         
         USING TAACD,R3                                                         
         OC    TAACMAJ(4),TAACMAJ   CHECK IF ZEROS                              
         BZ    DVERX                                                            
         LA    R5,ESTVER                                                        
         EDIT  TAACMAJ,(2,0(R5)),ALIGN=LEFT,ZERO=NOBLANK                        
         AR    R5,R0                                                            
         MVI   0(R5),C'.'                                                       
         LA    R5,1(R5)                                                         
         EDIT  TAACMIN,(2,0(R5)),ALIGN=LEFT,ZERO=NOBLANK                        
         AR    R5,R0                                                            
         MVI   0(R5),C'.'                                                       
         LA    R5,1(R5)                                                         
         EDIT  TAACBLD,(4,0(R5)),ALIGN=LEFT,ZERO=NOBLANK                        
         OI    ESTVERH+6,X'80'                                                  
DVERX    B     XIT                                                              
LOOKUP   NTR1                                                                   
         GOTO1 ALOOKUP,DMCB,0                                                   
         B     XIT                                                              
         SPACE 3                                                                
LOOKUP2  NTR1                                                                   
         GOTO1 ALOOKUP,DMCB,1                                                   
         B     XIT                                                              
         SPACE 3                                                                
LOOKUP3  NTR1                                                                   
         GOTO1 ALOOKUP,DMCB,2                                                   
         B     XITR3                                                            
         EJECT                                                                  
*              ERROR/EXIT ROUTINES                                              
         SPACE 1                                                                
         USING LINED,R2                                                         
         USING LINHED,R3                                                        
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
FLDINV1  MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
RECONFIL MVI   ERROR,RECEXIST      RECORD ALREADY ON FILE                       
         B     THEEND                                                           
RECNTFND MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     THEEND                                                           
IPTOOLNG MVI   ERROR,ERIPLONG      INPUT TOO LONG                               
         B     THEEND                                                           
NOPREV   MVI   ERROR,ERNOPAGE      NO PREVIOUS PAGE                             
         B     RHSERR                                                           
TOOMANY  MVI   ERROR,ERTOOMNY      TOO MANY OPTIONS INPUT                       
         B     RHSERR                                                           
NOINPUT  MVI   ERROR,ERNOINP       INPUT NOT ALLOWED                            
         LA    R2,LINRHSH                                                       
         TM    DISPMODE,HYPO                                                    
         BZ    *+8                                                              
         LA    R2,LINHRHSH                                                      
         B     THEEND                                                           
         SPACE 1                                                                
COMDSPLY MVI   MYMSGNO1,200        COMMERCIALS DISPLAYED - HIT ENTER            
         CLI   DISPMODE,LISTSCOM   IF SELECTABLE COMMERCIAL LIST                
         BNE   *+8                                                              
         MVI   MYMSGNO1,234        COMMLS DSPLYD - SELECT OR HIT ENTER          
         B     RHSEND                                                           
         SPACE 1                                                                
COMDHNXT MVI   MYMSGNO1,221        COMS DISPLAYED - HIT ENTER FOR HYPOS         
         CLI   DISPMODE,LISTSCOM   IF SELECTABLE COMMERCIAL LIST                
         BNE   *+8                                                              
         MVI   MYMSGNO1,233        COMS DSPLYD-SELECT/HIT ENTER HYPOS           
         B     RHSEND                                                           
         SPACE 1                                                                
COMDMISC MVI   MYMSGNO1,227        END OF COMMLS - HIT ENTER FOR MISC.          
         B     RHSEND                                                           
         SPACE 1                                                                
INSERTED MVI   MYMSGNO1,208        ADDITIONAL LINES INSERTED                    
         B     RHSEND2                                                          
         SPACE 1                                                                
PERDSPLY OC    NXTCSTDA,NXTCSTDA   ANY MORE LEFT                                
         BZ    PERDSPL2            NO                                           
         MVI   MYMSGNO1,202        PERFORMERS DISPLAYED - HIT ENTER             
         TM    DISPMD2,LISTSPRF    IF SELECTABLE PERFORMER LIST                 
         BZ    *+8                                                              
         MVI   MYMSGNO1,235        PERFORMERS DSPLY'D -SELECT/HIT ENTER         
         B     RHSEND                                                           
         SPACE 1                                                                
PERDSPL2 MVI   MYMSGNO1,205        END OF PERFORMER LIST - HIT ENTER            
         TM    DISPMD2,LISTSPRF    IF SELECTABLE PERFORMER LIST                 
         BZ    *+8                                                              
         MVI   MYMSGNO1,236        END OF PERF LIST-SELECT/HIT ENTER            
         B     RHSEND                                                           
         SPACE 1                                                                
PFTODEL  MVI   MYMSGNO1,230        PRESS PF20 TO DELETE CLI/PRD                 
         OI    ESTOPTSH+1,X'08'    SET CLI/PRD OPTIONS TO HIGH INTENSTY         
         OI    ESTOPTSH+6,X'80'                                                 
         OI    ESTOPT2H+1,X'08'    SAME FOR SECOND LINE                         
         OI    ESTOPT2H+6,X'80'                                                 
         LA    R2,ESTOPTSH                                                      
         B     INFEND                                                           
         SPACE 1                                                                
NOMORE   MVI   MYMSGNO1,217        END OF COMMERCIAL LIST - PRESS PF12          
         CLI   DISPMODE,LISTSCOM   IF SELECTABLE COMMERCIAL LIST                
         BNE   *+8                                                              
         MVI   MYMSGNO1,232        END OF COMM LIST-SELECT/PRESS PF12           
         CLI   ACTNUM,ACTSEL                                                    
         BNE   RHSEND                                                           
         B     COMDNEXT                                                         
         SPACE 1                                                                
COMDNEXT MVI   MYMSGNO1,215        END OF COMMERCIAL LIST - HIT ENTER           
         TM    ESTDATAH+1,X'20'    OK IF SELECT FIELD PROTECTED                 
         BO    RHSEND                                                           
         CLI   DISPMODE,LISTSCOM   IF SELECTABLE COMMERCIAL LIST                
         BNE   *+12                                                             
         MVI   MYMSGNO1,231        END OF COMM LIST-SELECT/HIT ENTER            
         B     RHSEND                                                           
         SPACE 1                                                                
HCOMDISP MVI   MYMSGNO1,223        HYPO. COMM'LS DISPLAYED - HIT ENTER          
         LA    R2,ESTDATAH                                                      
         B     INFEND                                                           
         SPACE 1                                                                
NOMORE2  CLI   ACTNUM,ACTSEL       IF WE'RE SELECTING                           
         BNE   NOMORE4                                                          
         OI    GENSTAT2,NEXTSEL    HAVE CONTROLLER GO TO NEXT SELECTION         
         XC    ESTKEY,ESTKEY       CLEAR SAVED KEY                              
         B     XIT                                                              
NOMORE4  MVI   MYMSGNO1,207        NOTHING MORE TO DISPLAY                      
         LA    R2,ESTESTH          CURSOR TO ESTIMATE FIELD                     
         TM    STATUS,FINISHED     IF WE HAVEN'T ALREADY DONE THIS              
         BO    INFEND                                                           
         OI    STATUS,FINISHED     SET FINISHED STATUS                          
         BRAS  RE,CLRSCRN          INSURE SCREEN IS CLEARED                     
         NI    ESTMSG2H+1,X'FB'    SET PF MSG TO HIGH INTENSITY                 
         OI    ESTMSG2H+6,X'80'                                                 
         ZIC   R1,PGCOMIND                                                      
         AH    R1,=H'1'            ADD 1 TO LIST INDEX FOR PAGING BACK          
         STC   R1,PGCOMIND                                                      
         B     INFEND                                                           
         SPACE 1                                                                
MISCOUT  LA    R2,MSCTXTH          CURSOR TO 1ST FIELD                          
         MVI   MYMSGNO1,225        MISCELLANEOUS ADDITIONS DISPLAYED            
         TM    STATUS2,BUILDING                                                 
         BZ    *+8                                                              
         MVI   MYMSGNO1,226        - ENTER CHANGES AS DESIRED                   
         NI    MSCMSG2H+1,X'FB'    SET PF MSG TO HIGH INTENSITY                 
         OI    MSCMSG2H+6,X'80'                                                 
         B     HYPEND                                                           
         SPACE 1                                                                
HYPOOUT  TM    DISPMODE,HYPOPER    BRANCH IF WE'RE ADDING PERFORMERS            
         BO    HYPOOUT2                                                         
         LA    R2,HYPCOMH          CURSOR TO COMMERCIAL FIELD                   
         MVI   MYMSGNO1,211        HYPOTHETICAL DATA DISPLAYED                  
         CLI   NEWEST,C'Y'                                                      
         BNE   *+16                                                             
         TM    DISPMD2,SELCTHPF                                                 
         BO    *+8                                                              
         MVI   MYMSGNO1,237        HYPO DATA DISPLY'D-SELECT OR ENTER           
         TM    STATUS2,BUILDING                                                 
         BZ    HYPEND                                                           
         MVI   MYMSGNO1,212        - ENTER CHANGES AS DESIRED                   
         CLI   NEWEST,C'Y'                                                      
         BNE   HYPOOUT1                                                         
         TM    DISPMD2,SELCTHPF                                                 
         BO    *+12                                                             
         MVI   MYMSGNO1,238        HYPO DATA DISPLY'D-SELECT OR ENTER           
         B     HYPOOUT1                                                         
         LA    R2,HYPNUMH                                                       
HYPOOUT1 OC    HYPCOM,HYPCOM                                                    
         BNZ   *+8                                                              
         MVI   MYMSGNO1,209        ENTER HYPOTHETICAL DATA                      
         B     HYPEND                                                           
         SPACE 1                                                                
HYPOOUT2 LA    R2,HYPNUMH          CURSOR TO 1ST PERF. INPUT FIELD              
         MVI   MYMSGNO1,213        ADDITIONAL PERFORMERS DISPLAYED              
         CLI   NEWEST,C'Y'                                                      
         BNE   *+16                                                             
         TM    DISPMD2,SELCTHPF                                                 
         BO    *+8                                                              
         MVI   MYMSGNO1,239        ADD'L PERFS DISPLY'D-SELECT OR ENTER         
         TM    STATUS2,BUILDING                                                 
         BZ    HYPEND                                                           
         MVI   MYMSGNO1,214        - ENTER CHANGES AS DESIRED                   
         CLI   NEWEST,C'Y'                                                      
         BNE   HYPOOUT3                                                         
         TM    DISPMD2,SELCTHPF                                                 
         BO    HYPOOUT3                                                         
         LA    R2,HYPDATAH                                                      
         MVI   MYMSGNO1,240        ADD'L PERFS DISPLY'D-SELECT OR ENTER         
HYPOOUT3 OC    HYPNUM,HYPNUM                                                    
         BNZ   *+8                                                              
         MVI   MYMSGNO1,210        ENTER ADDITIONAL PERFORMERS                  
         B     HYPEND                                                           
         SPACE 1                                                                
REPOUT   GOTO1 INITIAL,DMCB,PFREPTAB  GO TO INITIAL TO HANDLE CHANGE            
         DC    H'0'                                                             
         SPACE 1                                                                
RHSERR   LA    R2,ESTRHSH          CURSOR TO 1ST LINE, RHS                      
         B     THEEND                                                           
RHSEND   TM    STATUS2,BUILDING    IF BUILDING RECORD                           
         BZ    RHSEND2                                                          
         CLI   DISPMODE,LISTSCOM   AND NOT A SELECTABLE LIST                    
         BE    RHSEND2                                                          
         TM    DISPMD2,LISTSPRF                                                 
         BO    RHSEND2                                                          
         AI    MYMSGNO1,1          ADD '' INPUT OR ''                           
RHSEND2  LA    R2,ESTRHSH          CURSOR TO 1ST LINE, RHS                      
         TM    ESTDATAH+1,X'20'    OK IF SELECT FIELD PROTECTED                 
         BO    HYPEND                                                           
         LA    R2,ESTDATAH         ELSE CURSOR TO SELECT FIELD                  
         CLI   DISPMODE,LISTSCOM   OKAY IF SELECTABLE COMMERCIAL LIST           
         BE    INFEND                                                           
         TM    DISPMD2,LISTSPRF    OKAY IF SELECTABLE PERFORMER LIST            
         BO    INFEND                                                           
         MVI   MYMSGNO1,224        END OF HYPO COMML LIST - PRESS PF12          
         BAS   RE,ANYMISC                                                       
         BNE   INFEND                                                           
         MVI   MYMSGNO1,229        END OF HYPO COMMLS - HIT ENTER MISC          
         B     INFEND                                                           
HYPEND   TM    STATUS2,BUILDING    OK IF BUILDING RECORD                        
         BO    INFEND                                                           
         LA    R2,CONACTH          CURSOR TO ACTION FIELD                       
         CLI   ACTNUM,ACTSEL       OK IF IN SELECT MODE                         
         BE    INFEND                                                           
         LA    R2,ESTESTH          CURSOR TO ESTIMATE NUMBER                    
INFEND   OI    GENSTAT2,USGETTXT   SET TO GET INFORMATION MESSAGE               
         B     THEEND                                                           
                                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     THEEND                                                           
                                                                                
CUREND   LH    R2,CURDISP          RETURN CURSOR TO SAME SPOT                   
         AR    R2,RA                                                            
THEEND   GOTO1 EXIT,DMCB,0         ALL EXITS                                    
         SPACE 1                                                                
YESR     CR    RE,RE               RETURN CC EQUAL                              
         BR    RE                                                               
NOR      LTR   RE,RE               RETURN CC NOT EQUAL                          
         BR    RE                                                               
         SPACE 1                                                                
LOWX     LNR   RC,RC               RETURN CC MINUS                              
         B     NO                                                               
YES      XR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
XIT      XIT1                      NORMAL EXIT                                  
XITR3    XIT1  REGS=(R3)           EXIT WITH R3                                 
         EJECT                                                                  
*              CONSTANTS, LITERAL POOL, ETC.                                    
         SPACE 2                                                                
ALLBUT   EQU   C'/'                ALL EXCEPT INDICATOR                         
         SPACE 1                                                                
LTGUAR   DC    C'Grt='                                                          
LTEXP    DC    C'Exp='                                                          
WARNMSG  DC    C'***Will Delete Perfs!-->'                                      
HDCOMML  DC    CL38'Comm''l Id    M Commercial Name'                            
HDPERFL  DC    CL38'S/S Numb  Name         Cat DCW Ov%'                         
HDPERFLP DC    CL38'Pid Numb  Name         Cat DCW Ov%'                         
HDSPERFL DC    CL38'S/S Numb  Name                 Cat DCW'                     
HDSPERFP DC    CL38'Pid Numb  Name                 Cat DCW'                     
HDCOMMR  DC    CL37'Commercial Details'                                         
HDSCOMMR DC    CL37'Len Type    Lift id      Len ExpDt'                         
HDSHCOMR DC    CL37'Len Type    LiftLen'                                        
HDPERFR  DC    CL37'Performer Details'                                          
HDSPERFR DC    CL37'Uni/Lcl Yr  L Ov%     Guar ExpDt    D'                      
HDM4CO   DC    CL11'PF14=AddHyp'                                                
HDM4PE   DC    CL11'PF14=Return'                                                
HDM5CO   DC    CL9'PF15=Misc'                                                   
HDM5PE   DC    CL9'PF15=Next'                                                   
         SPACE 1                                                                
NLINEQU  EQU   (ESTMSG2H-ESTDATAH)/LINLNQ  N'SCREEN LINES ON PRIMARY            
NLINHEQU EQU   (HYPMSG2H-HYPDATAH)/LINHLNQ N'SCREEN LINES ON HYPO I/P           
NLINNEQU EQU   (MSCMSG3H-MSCNARRH)/LINNLNQ N'SCREEN LINES ON MISC NARR          
NLINMEQU EQU   (MSCNARTH-MSCTXTTH)/LINMLNQ N'SCREEN LINES ON MISC ADD           
         SPACE 2                                                                
*              PF KEY DEFINITIONS                                               
         SPACE 1                                                                
PFPREV   EQU   7                   RE-DISPLAY FROM PREVIOUS PAGE                
PFBEGIN  EQU   8                   RE-DISPLAY FROM 1ST PAGE                     
PFSTAY   EQU   13                  REMAIN ON THIS SCREEN                        
PFREPORT EQU   24                  GENERATE REPORT                              
PFADDHYP EQU   14                  ADD HYPO COMMERCIAL                          
PFMSCADD EQU   15                  LOAD MISC. ADDITIONS SCREEN                  
PFHYPPG  EQU   15                  HYPO PERFORMER PAGE CONTROL                  
PFDELHYP EQU   16                  DELETE HYPO COMMERCIAL                       
PFPRFLST EQU   16                  LOAD PERFORMER LIST SCREEN                   
PFPRFALT EQU   17                  LOAD PERFORMER ADD SCREEN                    
PFSRETRN EQU   17                  SAVES/RETURNS FROM HYPO SCREEN               
PFRETURN EQU   14                  RETURN TO PREVIOUS LIST                      
PFNXTCOM EQU   15                  SKIP TO NEXT COMM'L FROM PERF. LIST          
PFDELPRD EQU   20                  DELETE CLI/PRD                               
         SPACE 1                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
PFREPTAB DS    0C                                                               
         DC    AL1(PF24X-*,24,0,0,PFTSETPR)                                     
         DC    CL3' ',CL8'ESTIMATE',CL8'REPORT'                                 
PF24X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
         DROP  R6,R7,R8            DROP SECONDARY BASE REGISTERS                
         EJECT                                                                  
*=====================================================================          
*              ROUTINE CLEARS LOWER HALF OF LIST SCREENS                        
*=====================================================================          
         SPACE 1                                                                
         USING LINED,R2                                                         
CLRSCRN  NTR1  BASE=*,LABEL=*                                                   
         CLI   NEWEST,C'Y'         IF NEW ESTIMATE DISPLAY                      
         BNE   *+8                                                              
         BRAS  RE,SETSSCRN         SET SPECIAL SUB-SCREEN                       
         SPACE 1                                                                
         MVI   ESTOVLY,C' '        CLEAR OVERLAY BYTE                           
         BRAS  RE,TSTSEL           TEST LIST W/SEL FLD (RETURNS BYTE)           
         CLI   BYTE,C'Y'           IF SELECT FIELD                              
         BNE   *+8                                                              
         MVI   ESTOVLY,C'S'        SET FOR SELECT COLUMN                        
         OI    ESTOVLYH+6,X'80'                                                 
         SPACE 1                                                                
         LA    R0,NLINEQU          R0=N'SCREEN LINES                            
         LA    R2,ESTDATAH                                                      
         LA    R1,=C'ABCDEFGHIJKL'                                              
         SPACE 1                                                                
CLR2     MVC   LINCODE,0(R1)       SET LHS LETTER CODE                          
         OI    LINCODEH+1,X'20'    AND PROTECT IT                               
         OI    LINCODEH+4,X'20'    SET PREVIOUSLY VALIDATED                     
         OI    LINCODEH+6,X'80'                                                 
         XC    LINDATA,LINDATA     CLEAR LHS                                    
         NI    LINDATAH+1,X'F7'    INSURE NORMAL INTENSITY                      
         OI    LINDATAH+6,X'80'                                                 
         XC    LINRHS,LINRHS       CLEAR RHS                                    
         NI    LINRHSH+1,X'DE'     UNPROTECT IT                                 
         OI    LINRHSH+4,X'20'     SET PREVIOUSLY VALIDATED                     
         OI    LINRHSH+6,X'80'                                                  
         SPACE 1                                                                
         CLI   BYTE,C'Y'           IF SELECT FIELD PRESENT                      
         BNE   *+16                                                             
         MVI   LINCODE,0           CLEAR CODE                                   
         MVI   LINCODEH+5,0                                                     
         OI    LINRHSH+1,X'20'     PROTECT RHS                                  
         SPACE 1                                                                
         LA    R1,1(R1)                                                         
         LA    R2,LINNEXT                                                       
         BCT   R0,CLR2                                                          
CLRSCRX  XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO SET SPECIAL SUB-SCREEN FOR STEREO                     
         SPACE 1                                                                
SETSSCRN NTR1  BASE=*,LABEL=*                                                   
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,RE                                                         
         OI    TIOBINDS,TIOBSUBS   SET CHANGING SUB-SCREEN                      
         MVI   TIOBAID,X'00'       SET SUB-SCREEN FOR NORMAL SCREEN             
         TM    DISPMODE,LISTHYP    IF LISTING HYPO. COMMERCIALS                 
         BZ    *+8                                                              
         MVI   TIOBAID,X'01'       SET SPECIAL SUB-SCREEN                       
         TM    DISPMODE,SELCTCOM   IF DISPLAYING SELECTED COMMERCIAL            
         BZ    *+8                                                              
         MVI   TIOBAID,X'02'       SET SPECIAL SUB-SCREEN                       
         TM    DISPMD2,LISTSPRF    IF LISTING SELECTABLE PERFORMERS             
         BZ    *+8                                                              
         MVI   TIOBAID,X'03'       SET SPECIAL SUB-SCREEN                       
         TM    DISPMD2,SELCTPRF    IF PERFORMER DISPLAY                         
         BZ    *+8                                                              
         MVI   TIOBAID,X'04'       SET SPECIAL SUB-SCREEN                       
         TM    DISPMODE,HYPO       IF HYPO SCREEN DISPLAYED                     
         BZ    SETSSCX                                                          
         TM    DISPMODE,HYPOCOM    AND NOT HYPO COMMERCIAL                      
         BZ    *+12                                                             
         MVI   TIOBAID,X'00'       RESET SUB-SCREEN                             
         B     *+8                                                              
         MVI   TIOBAID,X'01'       SET SPECIAL SUB-SCREEN                       
         TM    DISPMD2,SELCTHPF    IF DISPLAYING SELECTED HYPO PERFS            
         BZ    *+8                                                              
         MVI   TIOBAID,X'02'       SET SPECIAL SUB-SCREEN                       
SETSSCX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TESTS FOR SELECT FIELD                                   
         SPACE 1                                                                
*                                  RETURNS BYTE=Y, SELECT FIELD PRESENT         
TSTSEL   NTR1  BASE=*,LABEL=*                                                   
         MVI   BYTE,C'N'           NO SELECT FIELD                              
         TM    DISPMD2,LISTSPRF    IF SELECTABLE PERFORMER LIST                 
         BO    TSTSEL5                                                          
         CLI   DISPMODE,LISTSCOM   OR IF SELECTABLE COMMERCIAL LIST             
         BE    TSTSEL5                                                          
         TM    DISPMODE,LISTHYP    OR IF HYPO. COMML LIST                       
         BO    TSTSEL5                                                          
         CLI   NEWEST,C'Y'         OR IF SELECTABLE HYPO PERFORMER LIST         
         BNE   TSTSELX                                                          
         TM    DISPMD2,SELCTHPF                                                 
         BO    TSTSELX                                                          
         TM    DISPMODE,HYPO                                                    
         BZ    TSTSELX                                                          
TSTSEL5  MVI   BYTE,C'Y'           SET SELECT FIELD PRESENT                     
TSTSELX  XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO VALIDATE PFKEY INPUT                                  
         SPACE 1                                                                
PFVAL    DS    0D                                                               
         NMOD1 0,**PFVAL*                                                       
         L     RC,SVRC                                                          
         SPACE 1                                                                
         CLI   PFAID,0             DON'T BOTHER IF USER HIT ENTER               
         BE    PFVX                                                             
         CLI   DISPMODE,LISTSCOM   IF COMMERCIAL                                
         BE    PFVA                                                             
         CLI   DISPMODE,SELCTCOM                                                
         BE    PFVA                                                             
         CLI   DISPMODE,LISTCOMS                                                
         BNE   PFV1                                                             
PFVA     CLC   =C'DELETE',ESTOPTS  AND USER WANTS TO DELETE CLI/PRD             
         BNE   PFV1                                                             
         CLI   PFAID,PFDELPRD      THEN IF THEY HIT PFKEY                       
         BE    PFVX                OK                                           
         SPACE 1                                                                
PFV1     CLI   PFAID,PFSTAY        IF THEY HIT STAY ON SCREEN PFKEY             
         BNE   *+12                                                             
         CLI   NEWEST,C'Y'         OKAY IF WERE NEW ESTIMATE DISPLAY            
         BE    PFVX                                                             
         SPACE 1                                                                
         LA    R1,PFTAB6           SET R1=A(LIST OF VALID PF KEYS)              
         LA    RF,MSCMSG2H+1       RF=A(BEGIN MESSAGE)                          
         TM    DISPMODE,MISCADD                                                 
         BO    PFV2                                                             
         SPACE 1                                                                
         LA    RF,HYPMSG2H+1       RF=A(PREV, BEGIN MESSAGES)                   
         LA    R1,PFTAB1                                                        
         TM    STATUS2,BUILDING                                                 
         BZ    *+8                                                              
         LA    R1,PFTAB2                                                        
         TM    DISPMD2,LISTSPRF    IF LISTING PERFORMERS                        
         BO    *+12                                                             
         TM    DISPMODE,LISTPRFS                                                
         BZ    *+8                                                              
         LA    R1,PFTAB3                                                        
         LA    RF,ESTMSG2H+1       RF=A(PREV, BEGIN MESSAGES)                   
         SPACE 1                                                                
         TM    DISPMODE,HYPO       HYPO SCREEN                                  
         BZ    PFV2                                                             
         CLI   PFAID,PFHYPPG       SPECIAL FOR PAGE NUMBER KEY                  
         BNE   *+16                                                             
         LA    R2,HYPPFPGH                                                      
         CLI   8(R2),C'1'                                                       
         BL    PFLDINV1                                                         
         LA    R1,PFTAB4                                                        
         TM    STATUS2,BUILDING                                                 
         BZ    *+8                                                              
         LA    R1,PFTAB5                                                        
         LA    RF,HYPMSG2H+1       RF=A(PREV, BEGIN MESSAGES)                   
         SPACE 1                                                                
PFV2     TM    0(RF),X'0C'         IF CORRESPONDING MSG ZERO INTENSITY          
         BNO   *+8                                                              
         LA    R1,2(R1)            THEN SKIP THEM                               
         SPACE 1                                                                
PFV4     CLI   0(R1),0             IF REACHED END OF LIST                       
         BE    PFERR               ERROR                                        
         CLC   PFAID,0(R1)                                                      
         BE    *+12                                                             
         LA    R1,1(R1)                                                         
         B     PFV4                                                             
         SPACE 1                                                                
PFVX     XIT1                                                                   
         EJECT                                                                  
PFERR    LH    R2,CURDISP          RETURN CURSOR TO SAME SPOT                   
         AR    R2,RA                                                            
         MVI   ERROR,ERINVPFK      INVALID PFKEY                                
         B     *+8                                                              
PFLDINV1 MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
*              VALID PF KEY TABLES (PFPREV MUST BE FIRST IF IN LIST)            
         SPACE 1                                                                
PFTAB1   DC    AL1(PFPREV,PFBEGIN,PFMSCADD,PFPRFLST,PFPRFALT)                   
         DC    AL1(PFREPORT,0)                                                  
PFTAB2   DC    AL1(PFPREV,PFBEGIN,PFADDHYP,PFMSCADD,PFPRFLST,PFPRFALT)          
         DC    AL1(PFREPORT,0)                                                  
PFTAB3   DC    AL1(PFPREV,PFBEGIN,PFRETURN,PFNXTCOM,PFREPORT,0)                 
PFTAB4   DC    AL1(PFPREV,PFBEGIN,PFRETURN,PFHYPPG,PFSRETRN)                    
         DC    AL1(PFREPORT,0)                                                  
PFTAB5   DC    AL1(PFPREV,PFBEGIN,PFRETURN,PFHYPPG,PFDELHYP,PFSRETRN)           
         DC    AL1(PFREPORT,0)                                                  
PFTAB6   DC    AL1(PFBEGIN,PFREPORT,0)                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE INITIALIZES STORAGE IF NECESSARY                         
         SPACE 1                                                                
INITSTOR DS    0D                                                               
         NMOD1 0,INITSTOR                                                       
         L     RC,SVRC                                                          
         NI    STATUS2,ALL-REDISP  TURN OFF RE-DISPLAY                          
         SPACE 1                                                                
         TM    STATUS,RESET        IF WE'RE STARTING NEW CLI/PRD                
         BO    INITS2                                                           
         CLI   PFAID,PFBEGIN       OR IF USER ASKED TO START OVER               
         BNE   INITS4                                                           
         TM    DISPMD2,LISTSPRF    AND WE'RE ON COMMERCIAL LIST SCREEN          
         BO    *+12                                                             
         TM    DISPMODE,LISTPRFS                                                
         BZ    INITS3              THEN DO SOME INITIALIZATION                  
         B     INITS4                                                           
         SPACE 1                                                                
INITS2   MVI   PGCOMIND,0          CLEAR COMMERCIAL PAGE INDICATOR              
         XC    PGCOMTAB,PGCOMTAB   CLEAR COMMERCIAL PAGE TABLE                  
         XC    NXTCOMDA,NXTCOMDA                                                
         LA    R3,KEY                                                           
         USING TLCOD,R3                                                         
         XC    TLCOKEY,TLCOKEY     BUILD INITIAL COMMERCIAL KEY                 
         MVI   TLCOCD,TLCOCDQ                                                   
         MVC   TLCOAGY,TGAGY       AGENCY                                       
         MVC   TLCOCLI,TGCLI       CLIENT                                       
         MVC   TLCOPRD,TGPRD       PRODUCT                                      
         MVC   TLCOCID,STARTCOM    SET FIRST COMMERCIAL (IF AROUND)             
         GOTO1 HIGH                                                             
         LA    R1,TLCOCID-TLCOD-1  SAME CLIENT/PRODUCT                          
         OC    TGPRD,TGPRD         IF NO PRODUCT DEFINED                        
         BNZ   *+8                                                              
         LA    R1,TLCOPRD-TLCOD-1  SAME CLIENT                                  
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TLCOKEY(0),KEYSAVE  TEST STILL HAVE GOOD KEY                     
         BNE   *+10                                                             
         USING TLDRD,R3                                                         
         MVC   NXTCOMDA,TLDRDA     SAVE D/A OF STARTING COMMERCIAL              
         OI    STATUS2,REDISP      SET TO START HERE                            
         MVC   SAVEHCOM,STARTHYP   RESET START OF HYPO COMMERCIALS              
         SPACE 1                                                                
INITS3   MVI   PGCSTIND,0                                                       
         XC    CSELTAB,CSELTAB     CLEAR SELECT TABLE                           
         SPACE 1                                                                
INITS4   CLI   SAVEFNUM,0          *** FIRST TIME ONLY ***                      
         BNE   INITSTX                                                          
         MVC   SAVEFNUM,ESTRHSX    SAVE FIELD NUMBER OF RHS                     
* NO-OP  NI    ESTMSG3H+1,X'FB'    SET PF MSGS TO HIGH INTENSITY                
* NO-OP  OI    ESTMSG3H+6,X'80'                                                 
         NI    ESTMSG5H+1,X'FB'                                                 
         OI    ESTMSG5H+6,X'80'                                                 
* NO-OP  NI    ESTMSG6H+1,X'FB'    NOW DEFINED HIGH INTENSITY ON SCRN           
* NO-OP  OI    ESTMSG6H+6,X'80'                                                 
INITSTX  XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO VALIDATE CLIENT/PRODUCT                               
         SPACE 1                                                                
VALCP    DS    0D                                                               
         NMOD1 0,*VALCP*                                                        
         L     RC,SVRC                                                          
         LA    R2,ESTCLIH          R2=A(CLIENT FIELD)                           
         CLI   5(R2),0             IF FIELD IS EMPTY                            
         BE    *+12                                                             
         TM    4(R2),X'20'         OR FIELD HASN'T CHANGED                      
         BZ    VCP2                                                             
         OC    NEXTCLI,NEXTCLI     AND WE HAVE A NEXT CLIENT                    
         BNZ   VCP1                MOVE IT TO SCREEN                            
         CLI   5(R2),0                 IF THERE'S NO INPUT                      
         BE    VFLDMISS                   REQUIRE IT                            
         B     VCP4                    ELSE MUST BE OK                          
         SPACE 1                                                                
VCP1     MVC   8(6,R2),NEXTCLI     MOVE IT TO SCREEN                            
         OI    6(R2),X'80'                                                      
         MVI   5(R2),6             SIMULATE L'I/P                               
         MVI   ESTPRDH+5,0         MAKE PRODUCT APPEAR EMPTY                    
         SPACE 1                                                                
VCP2     GOTO1 RECVAL,DMCB,TLCLCDQ,(X'08',(R2)),ESTCLINH  VAL. CLIENT           
         OI    STATUS,RESET+PRDCHGED                      RESET LIST            
         NI    ESTPRDH+4,X'DF'     SET TO RE-VALIDATE PRODUCT                   
         SPACE 1                                                                
VCP4     LA    R2,ESTPRDH          R2=A(PRODUCT FIELD)                          
         CLI   5(R2),0             IF FIELD IS EMPTY                            
         BE    *+12                                                             
         TM    4(R2),X'20'         OR FIELD HASN'T CHANGED                      
         BZ    VCP6                                                             
         OC    NEXTPRD,NEXTPRD     AND WE HAVE A NEXT PRODUCT                   
         BNZ   VCP5                GO USE IT                                    
         CLI   5(R2),0             IF THERE'S NO INPUT                          
         BE    VFLDMISS            REQUIRE IT                                   
         B     VCP8                                                             
         SPACE 1                                                                
VCP5     MVC   8(6,R2),NEXTPRD     MOVE IT TO SCREEN                            
         OI    6(R2),X'80'                                                      
         MVI   5(R2),6             SIMULATE L'I/P                               
         SPACE 1                                                                
VCP6     XC    TGPRD,TGPRD         PRE-CLEAR PRODUCT                            
         XC    ESTPRDN,ESTPRDN                                                  
         OI    ESTPRDNH+6,X'80'                                                 
         OC    8(6,R2),SPACES                                                   
         CLC   8(6,R2),=CL6'ALL'   IF 'ALL' INPUT                               
         BE    VCP7                THEN DONE                                    
         GOTO1 RECVAL,DMCB,TLPRCDQ,(X'08',(R2)),ESTPRDNH  VAL. PRODUCT          
         SPACE 1                                                                
VCP7     OI    4(R2),X'20'         SET VAL IN CASE DIDN'T CALL RECVAL           
         OI    STATUS,RESET+PRDCHGED  RESET LIST                                
         NI    ESTOPTSH+1,X'F3'    SET CLI/PRD OPTIONS TO NORMAL INTENS         
         OI    ESTOPTSH+6,X'80'                                                 
         SPACE 1                                                                
VCP8     XC    NEXTS,NEXTS         CLEAR NEXTS                                  
         XIT1                                                                   
         EJECT                                                                  
VFLDMISS MVI   ERROR,MISSING       MSSING INPUT FIELD                           
         GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO GET AN ESTIMATE RECORD                                
*                                  RETURNS WITH RECORD IN TIA                   
         SPACE 1                                                                
GETEREC  DS    0D                                                               
         NMOD1 0,GETEREC                                                        
         L     RC,0(R1)                                                         
         SPACE 1                                                                
         NI    STATUS2,ALL-HAVEREC                                              
         MVC   KEY(L'ESTKEY),ESTKEY  SET KEY                                    
         GOTO1 HIGH                                                             
         SPACE 1                                                                
         LA    R3,KEY                                                           
         USING TLESD,R3                                                         
         CLC   TLESKEY,KEYSAVE     DID WE GET THE RECORD                        
         BNE   GRECNO                                                           
         SPACE 1                                                                
         MVC   KEYSAVE,TLESKEY     SAVE ENTIRE DIR. REC. IN KEYSAVE             
         MVC   AIO,ATIA                                                         
         GOTO1 GETREC              GET RECORD INTO TIA                          
         MVC   AIO,AIO1                                                         
         OI    STATUS2,HAVEREC                                                  
         NI    DMINBTS,X'F7'       TURN OFF READ-DELETED IN CASE SET            
         SPACE 1                                                                
GREC4    GOTO1 SEQ                             LOOK FOR ANOTHER RECORD          
         LA    R3,KEY                                                           
         CLC   TLESKEY(TLESSEQ-TLESD),KEYSAVE  WITH ALL SAME UP TO SEQ.         
         BNE   GRECX                                                            
         GOTO1 GETREC              GET NEW RECORD INTO IO1                      
         SPACE 1                                                                
         L     RE,AIO              NOW COMBINE THEM - RE=A(NEW RECORD)          
         L     R3,ATIA                                R3=A(MAIN RECORD)         
         LH    RF,TLESLEN-TLESD(RE)  L'NEW RECORD                               
         SH    RF,DATADISP           LESS DATADISP                              
         SPACE 1                                                                
         LH    R1,TLESLEN                                                       
         BCTR  R1,0                                                             
         LR    R0,R1               R0=R1=L'MAIN RECORD (-1 FOR EOR)             
         SPACE 1                                                                
         AR    R1,RF               PLUS L'NEW RECORD                            
         STH   R1,TLESLEN          IS L'COMBINED RECORD                         
         SPACE 1                                                                
         AR    R0,R3               R0=A(END OF MAIN RECORD)                     
         AH    RE,DATADISP         RE=A(1ST EL. IN NEW RECORD)                  
         LR    R1,RF               R1=RF=L'NEW RECORD ELEMENTS                  
         SPACE 1                                                                
         MVCL  R0,RE               MOVE NEW RECORD AFTER MAIN                   
         B     GREC4               LOOK FOR ANOTHER                             
         SPACE 1                                                                
GRECX    MVC   KEY,KEYSAVE         RESTORE ORIG. DIR. REC. TO KEY               
         SPACE 1                                                                
GRECYES  XR    RC,RC               RETURN CC EQUAL                              
GRECNO   LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO VALIDATE HYPOTHETICAL PERFORMER FIELDS                
         SPACE 1                                                                
         USING LINHED,R3           R3=A(SCREEN LINE)                            
         SPACE 1                                                                
VALHPRF  DS    0D                                                               
         NMOD1 0,VALHPRF                                                        
         L     RC,SVRC                                                          
         SPACE 1                                                                
         XC    ELDATA,ELDATA                                                    
         LA    R4,ELDATA           R4=A(ELEMENT DATA)                           
         USING TAESDATA,R4                                                      
         MVC   TAESHPG,HYPRFPG     SET CURRENT PAGE NUMBER                      
         SPACE 1                                                                
         MVI   TAESHNPR,1                                                       
         LA    R2,LINHNUMH         R2=A(NUMBER FIELD)                           
         CLI   5(R2),0                                                          
         BE    VALHP2                                                           
         GOTO1 VALINUM                                                          
         MVC   TAESHNPR,ACTUAL     SAVE BINARY N'PERFORMERS                     
         SPACE 1                                                                
VALHP2   LA    R2,LINHCATH         R2=A(CATEGORY FIELD)                         
         GOTO1 ANY                                                              
         GOTO1 CATVAL,DMCB,WORK    VALIDATE IT                                  
         BNE   VFLDINV1                                                         
         MVC   TAESHCAT,TGCAEQU    MOVE EQUATE TO ELEMENT                       
         SPACE 1                                                                
         LA    R2,LINHUNIH         R2=A(UNION FIELD)                            
         CLI   5(R2),0                                                          
         BNE   VALHP6                                                           
         OI    6(R2),X'80'                                                      
*        TM    TGCAUNI,AFM         IF AFM IS AN ALLOWABLE UNION                 
         GOTO1 UNITEST,DMCB,TGCAUNIS,AFM,0,0,0                                  
         BZ    *+14                                                             
         MVC   8(3,R2),=C'AFM'     SET IT                                       
         B     VALHP6                                                           
*        TM    TGCAUNI,ACT         IF ACT IS AN ALLOWABLE UNION                 
         GOTO1 UNITEST,DMCB,TGCAUNIS,ACT,0,0,0                                  
         BZ    *+14                                                             
         MVC   8(3,R2),=C'ACT'     SET IT                                       
         B     VALHP6                                                           
         CLI   TGMEEQU,RADIO       IF MEDIA IS 'R', THEN ASSUME AFT             
         BE    VALHP4                                                           
*        TM    TGCAUNI,SAG+AFT     IF BOTH SAG AND AFTRA ALLOWED                
         GOTO1 UNITEST,DMCB,TGCAUNIS,SAG+AFT,0,0,0                              
         BO    VFLDMIS2            REQUIRE INPUT                                
*        TM    TGCAUNI,AFT                                                      
         GOTO1 UNITEST,DMCB,TGCAUNIS,AFT,0,0,0                                  
         BZ    *+14                                                             
VALHP4   MVC   8(3,R2),=C'AFT'     SET AFT IF IT'S THE ONE ALLOWED              
         B     VALHP6                                                           
         MVC   8(3,R2),=C'SAG'     ELSE SET SAG                                 
         SPACE 1                                                                
VALHP6   GOTO1 UNIVAL,DMCB,8(R2)   FIND UNION CODE                              
         BNE   VFLDINV1            INVALID UNION                                
*        MVC   BYTE,TGUNEQU        INSURE UNION IS VALID FOR CATEGORY           
*        NC    BYTE,TGCAUNI                                                     
         GOTO1 UNITEST,DMCB,(X'80',TGUNEQUS),TGCAUNIS                           
         BZ    VFLDINV1                                                         
         CLI   TGMEEQU,RADIO       IF MEDIA IS RADIO                            
         BNE   *+12                                                             
         CLI   TGUNEQU,SAG         THEN SAG NOT ALLOWED                         
         BE    VFLDINV1                                                         
*        MVC   TAESHUNI,TGUNEQU    UNION EQUATE TO ELEMENT DATA                 
         MVC   TAESHUNI,TGESUNI    EST UNION EQUATE TO ELEMENT DATA             
         SPACE 1                                                                
         LA    R2,LINHCAMH         VALIDATE ON/OFF                              
         CLI   5(R2),0             NO INPUT IS POSSIBLE                         
         BNE   VALHP8                                                           
         CLI   TGMEEQU,RADIO       IF MEDIA RADIO, MUST BE OFF-CAMERA           
         BE    VALHP7                                                           
*        TM    TGUNEQU,AFM         MUSICIANS SHOULD DEFAULT TO 'OFF'            
         GOTO1 UNITEST,DMCB,TGUNEQUS,AFM,0,0,0                                  
         BO    VALHP7                                                           
         TM    TGCASTAT,OKON+OKOFF ELSE IF ONLY ON OR OFF IS ALLOWED            
         BO    VFLDMIS2                                                         
         MVC   8(2,R2),=C'ON'      SET DEFAULT CAMERA STATUS                    
         TM    TGCASTAT,OKON                                                    
         BO    *+10                                                             
VALHP7   MVC   8(3,R2),=C'OFF'                                                  
         OI    6(R2),X'80'                                                      
         SPACE 1                                                                
VALHP8   CLC   8(2,R2),=C'OFF'     TEST OFF-CAMERA                              
         BNE   VALHP9                                                           
         TM    TGCASTAT,OKOFF      MUST BE ALLOWED FOR THIS CATEGORY            
         BZ    VFLDINV1                                                         
         B     VALHP10             NO BIT SETTING IS NECESSARY                  
         SPACE 1                                                                
VALHP9   CLC   8(2,R2),=C'ON'      TEST ON-CAMERA                               
         BNE   VFLDINV1                                                         
         TM    TGCASTAT,OKON       MUST BE ALLOWED FOR THIS CATEGORY            
         BZ    VFLDINV1                                                         
         CLI   TGMEEQU,RADIO       IF MEDIA RADIO, MUST BE OFF-CAMERA           
         BE    VFLDINV1                                                         
         OI    TAESHSTA,X'80'      SET ON-CAMERA BIT                            
         SPACE 1                                                                
VALHP10  LA    R2,LINHLFTH         R2=A(LIFT FIELD)                             
         CLI   5(R2),1                                                          
         BL    VALHP14                                                          
         BH    VFLDINV1                                                         
         CLI   8(R2),C'N'                                                       
         BE    VALHP14                                                          
         CLI   8(R2),C'Y'          ALLOW Y(ES) ON LIFT                          
         BE    *+12                                                             
         CLI   8(R2),C'O'          OR O(NLY) ON LIFT                            
         BNE   VFLDINV1                                                         
         MVC   TAESHLFT,8(R2)                                                   
         SPACE 1                                                                
VALHP14  LA    R2,LINHDBLH         R2=A(DOUBLES FIELD)                          
         CLI   5(R2),1                                                          
         BL    VALHP18                                                          
         BH    VFLDINV1                                                         
         TM    TGCASTAT,OKDOUBLE   TEST DOUBLES OK FOR THIS CATEGORY            
         BZ    VFLDINV1                                                         
         GOTO1 VALINUM             TEST VALID NUMERIC FIELD                     
         MVC   TAESHDBL,8(R2)      SAVE EBCDIC VALUE                            
         SPACE 1                                                                
VALHP18  LA    R2,LINHCRPH         R2=A(CORP FIELD)                             
         MVI   CORPFLAG,0                                                       
         CLI   5(R2),0                                                          
         BE    VALHPX                                                           
         CLI   8(R2),C'N'                                                       
         BE    VALHPX                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   VFLDINV1                                                         
         OI    TAESHSTA,X'40'                                                   
         MVI   CORPFLAG,TAW4TYCO   SET CORP                                     
         SPACE 1                                                                
VALHPX   OI    STATUS2,ADDESTEL    SET TO ADD THIS ELEMENT                      
         XIT1                                                                   
         DROP  R3,R4                                                            
         SPACE 2                                                                
VFLDINV1 MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     *+8                                                              
         SPACE 1                                                                
VFLDMIS2 MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO DISPLAY ONE HYPO PERFORMER                            
         SPACE 1                                                                
         USING LINHED,R3           R3=A(SCREEN LINE)                            
         USING TAESD,R4            R4=A(ESTIMATE EL.)                           
DISHPLN  DS    0D                                                               
         NMOD1 0,DISHPLN                                                        
         L     RC,SVRC                                                          
         CLI   NEWEST,C'Y'         IF NEW ESTIMATE DISPLAY                      
         BNE   DISHPLN2                                                         
         TM    DISPMD2,SELCTHPF    AND LISTING SELECTABLE HYPO PERFS            
         BO    DISHPLN2                                                         
         GOTO1 FLDVAL,DMCB,(X'08',LINHNUMH),LINHCRPH  PROTECT LHS               
*                                                                               
DISHPLN2 EDIT  (1,TAESHNPR),(3,LINHNUM),ALIGN=LEFT DISPLAY N'PERFS.             
         STC   R0,LINHNUMH+5                                                    
         SPACE 1                                                                
         MVC   LINHCAM(3),=C'ON '                                               
         TM    TAESHSTA,X'80'      ON/OFF CAMERA                                
         BO    *+14                                                             
         MVC   LINHCAM,=C'OFF'                                                  
         MVI   LINHCAMH+5,3                                                     
         SPACE 1                                                                
         MVC   LINHLFT(1),TAESHLFT  LIFT STATUS                                 
         MVI   LINHLFTH+5,1                                                     
         MVC   LINHDBL(1),TAESHDBL  N'DOUBLES                                   
         MVI   LINHDBLH+5,L'TAESHDBL                                            
         SPACE 1                                                                
         TM    TAESHSTA,X'40'      CORP STATUS                                  
         BZ    *+12                                                             
         MVI   LINHCRP,C'Y'                                                     
         MVI   LINHCRPH+5,1                                                     
         SPACE 1                                                                
         GOTO1 CATVAL,DMCB,(X'80',TAESHCAT)  LOOK UP CATEGORY                   
         MVC   LINHCAT,TGCACDE               AND DISPLAY IT                     
         MVI   LINHCATH+5,L'TGCACDE                                             
         SPACE 1                                                                
         GOTO1 UNIVAL,DMCB,(X'20',TAESHUNI)  LOOK UP UNION                      
         MVC   LINHUNI,TGUNCDE               AND DISPLAY IT                     
         MVI   LINHUNIH+5,L'TGUNCDE                                             
         SPACE 1                                                                
         MVI   THSLINES,1                                                       
         CLI   NEWEST,C'Y'         IF NEW ESTIMATE DISPLAY                      
         BNE   DISHPLN5                                                         
         TM    DISPMD2,SELCTHPF    AND IF NOT SELECT HYPO PERFORMER             
         BO    DISHPLN5                                                         
         MVI   LINHRHS,C' '                                                     
         CLI   TAESSBNO,0          IF OVERRIDING DETAILS                        
         BE    *+8                                                              
         MVI   LINHRHS,C'Y'        INDICATE SO                                  
         OI    LINHRHSH+6,X'80'    TRANSMIT                                     
         B     DISHPLNX                                                         
         SPACE 1                                                                
DISHPLN5 LA    RF,LINHLNQ          DISPLAY OPTIONS                              
         GOTO1 AOPUNSCN,DMCB,LINHRHSH,((RF),HYPLSTRH)                           
         BE    *+6                                                              
         DC    H'0'                COULDN'T DISPLAY THIS LINE                   
DISHPLNX XIT1                                                                   
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO VALIDATE MISCELLANEOUS ADDITIONS SCREEN               
         SPACE 1                                                                
VALMISC  DS    0D                                                               
         NMOD1 0,VALMISC                                                        
         L     RC,SVRC                                                          
         SPACE 1                                                                
         MVC   AIO,AIO2            SET A(TEMPORARY ESTIMATE RECORD)             
         SPACE 1                                                                
         LA    R3,MSCTXTTH         R2=A(FIRST MISC. ADDITION FIELD)             
         USING LINMD,R3                                                         
         LA    R0,NLINMEQU         R0=N'MISC ADDITION FIELDS                    
         LA    R4,ELEMENT                                                       
         USING TAEMD,R4            R4=A(EST. MISC. ADDITION ELEMENT)            
         SPACE 1                                                                
VMISC10  CLI   LINMTXTH+5,0        IF NEITHER TEXT FIELD                        
         BNE   *+12                                                             
         CLI   LINMAMTH+5,0        NOR AMOUNT FIELD INPUT                       
         BE    VMISC40             THEN SKIP TO NEXT                            
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     BUILD ELEMENT                                
         MVI   TAEMEL,TAEMELQ                                                   
         MVI   TAEMLEN,TAEMLNQ     SET DEFAULT LENGTH                           
         MVC   TAEMSEQ,LINMTAG     SET SEQUENCE NUMBER FROM TAG                 
         SPACE 1                                                                
         LA    R2,LINMTXTH         R2=A(TEXT FIELD)                             
         CLI   5(R2),0                                                          
         BE    VMISC20                                                          
         ZIC   R1,5(R2)            R1=L'INPUT                                   
         SH    R1,=H'1'                                                         
         BM    VMISC20                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TAEMTXT(0),8(R2)    MOVE IN TEXT                                 
         LA    R1,1+TAEMLNQ(R1)    SET LENGTH                                   
         STC   R1,TAEMLEN          AND SAVE IT                                  
         SPACE 1                                                                
VMISC20  LA    R2,LINMAMTH         R2=A(AMOUNT FIELD)                           
         CLI   5(R2),0                                                          
         BE    VMISC30                                                          
         ZIC   RF,5(R2)            RF=L'INPUT                                   
         GOTO1 CASHVAL,DMCB,8(R2),(RF)                                          
         CLI   0(R1),X'FF'                                                      
         BE    AMTINV1                                                          
         OC    4(4,R1),4(R1)       DON'T ALLOW ZERO                             
         BZ    AMTINV1                                                          
         MVC   TAEMAMT,4(R1)       SAVE AMOUNT                                  
         SPACE 1                                                                
VMISC30  GOTO1 ADDELEM             ADD ELEMENT TO RECORD                        
         SPACE 1                                                                
VMISC40  LA    R3,LINMNEXT         BUMP TO NEXT ENTRY                           
         BCT   R0,VMISC10          AND CONTINUE                                 
         SPACE 1                                                                
         BAS   RE,VALCVR           VALIDATE COVER SHEET LINES                   
         SPACE 1                                                                
         MVC   AIO,AIO1            RESTORE DEFAULT I/O AREA                     
         SPACE 1                                                                
         GOTO1 AWRITEIT            WRITE THE RECORD BACK NOW                    
VALMISCX XIT1                                                                   
         SPACE 2                                                                
AMTINV1  MVI   ERROR,ERINVAMT      INVALID AMOUNT                               
         GOTO1 EXIT,DMCB,0         ALL EXITS                                    
         EJECT                                                                  
*              ROUTINE TO VALIDATE COVER SHEET LINES                            
         SPACE 1                                                                
VALCVR   NTR1                                                                   
         LA    R2,MSCNARXH         R2=A(LAST NARRATIVE FIELD)                   
         USING LINND,R2                                                         
         LA    R0,NLINNEQU         R0=N'NARRATIVE FIELDS                        
         XR    R3,R3               R3=FORCE ELEMENT ADD SWITCH                  
         LA    R4,ELEMENT                                                       
         USING TAEND,R4            R4=A(EST. NARRATIVE ELEMENT)                 
         SPACE 1                                                                
VCVR10   LTR   R3,R3               IF FORCING ADD, SKIP INPUT TEST              
         BNZ   *+16                                                             
         CLI   5(R2),0             ELSE SKIP THIS FIELD IF NO INPUT             
         BE    VCVR30                                                           
         LA    R3,1                THERE'S INPUT, SO ADD ALL REMAINING          
         SPACE 1                                                                
         XC    ELEMENT,ELEMENT     BUILD ELEMENT                                
         MVI   TAENEL,TAENELQ                                                   
         MVI   TAENLEN,TAENLNQ                                                  
         STC   R0,TAENSEQ          SET SEQUENCE NUMBER                          
         ZIC   R1,5(R2)                                                         
         SH    R1,=H'1'                                                         
         BM    VCVR20                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TAENNARR(0),8(R2)   MOVE IN TEXT                                 
         LA    R1,1+TAENLNQ(R1)    SET LENGTH                                   
         STC   R1,TAENLEN          AND SAVE IT                                  
         SPACE 1                                                                
VCVR20   GOTO1 ADDELEM             ADD ELEMENT TO RECORD                        
         SPACE 1                                                                
VCVR30   SH    R2,=AL2(LINNLNQ)    BUMP TO PREVIOUS LINE                        
         BCT   R0,VCVR10           AND CONTINUE                                 
         B     VALMISCX                                                         
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO VALIDATE/SET DISPLAY FILTERS                          
         SPACE 1                                                                
VALFILT  DS    0D                                                               
         NMOD1 0,VALFILT                                                        
         L     RC,SVRC                                                          
         LA    R2,ESTFLTSH         R2=A(DISPLAY FILTERS FIELD)                  
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VXIT                                                             
         MVC   ELEMENT(FRESETLQ),FRESET SAVE CURRENT FILTER VALUES              
         XC    FILTERS,FILTERS     INITIALIZE                                   
         SPACE 1                                                                
* NO-OP  GOTO1 ADDAY,DMCB,TGTODAY0,WORK,F'-180'  GO BACK 180 DAYS               
* NO-OP  GOTO1 DATCON,DMCB,(0,WORK),(1,ACTVDATE) DEF. LAST ACTIVE DATE          
         MVI   CLINES,2            N'LINES PER COMML RECORD DISPLAYED           
         CLI   NEWEST,C'Y'         IF NEW ESTIMATE DISPLAY                      
         BNE   *+8                                                              
         MVI   CLINES,1            SET TO ONE LINE PER COMML                    
         MVI   PLINES,1            N'LINES PER PERF. RECORD DISPLAYED           
         CLI   5(R2),0                                                          
         BE    VALFX                                                            
         SPACE 1                                                                
         USING SCAND,R3                                                         
         LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         GOTO1 SCANNER,DMCB,(R2),(X'80',(R3))                                   
         CLI   4(R1),0                                                          
         BE    FLDINVF                                                          
         ZIC   R0,4(R1)            R0=N'SCAN ENTRIES                            
         SPACE 1                                                                
VALF2    MVC   ERRDISP,SCDISP1     SET DISP. INTO FLD OF LHS FOR ERRORS         
         SPACE 1                                                                
         ZIC   R1,SCLEN1                                                        
         LTR   R1,R1               MUST HAVE LHS                                
         BZ    FLDINVF                                                          
         BCTR  R1,0                R1=L'LHS-1                                   
         SPACE 1                                                                
         USING FILTD,R4                                                         
         LA    R4,FILTTAB          R4=A(FILTER TABLE ENTRY)                     
VALF4    CLI   0(R4),0                                                          
         BE    FLDINVF             END-OF-TABLE                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FILTLHS(0),SCDATA1  MATCH ON L'INPUT                             
         BE    *+12                                                             
         LA    R4,FILTNEXT         TRY NEXT                                     
         B     VALF4                                                            
         SPACE 1                                                                
         MVC   BYTE,FILTEQU        SAVE FILTER EQUATE                           
         IC    R1,FILTDSP                                                       
         LA    R1,FILTERS(R1)      R1=A(THIS FILTER)                            
         CLI   BYTE,FLMEDIA        ALLOW MULTIPLE MEDIA FILTERS                 
         BE    VALF6                                                            
         NC    BYTE,0(R1)                                                       
         BNZ   DUPERRF             FIELD ALREADY INPUT                          
         SPACE 1                                                                
VALF6    MVC   ERRDISP,SCDISP2     SET DISP. INTO FLD OF RHS FOR ERRORS         
         SPACE 1                                                                
         LH    RF,FILTRTN                                                       
         AR    RF,RB               RF=A(VALIDATION ROUTINE)                     
         BASR  RE,RF               ** VALIDATE FILTER **                        
         OC    0(1,R1),FILTEQU     TURN ON FILTER EQUATE                        
         SPACE 1                                                                
         LA    R3,SCANNEXT         BUMP TO NEXT SCAN FIELD                      
         BCT   R0,VALF2            AND VALIDATE IT                              
         MVI   ERRDISP,0                                                        
         SPACE 1                                                                
VALFX    CLC   ELEMENT(FRESETLQ),FRESET IF SELECTED FILT VALUES CHANGED         
         BE    *+12                                                             
         OI    STATUS,RESET        SET TO RE-START LIST                         
         B     *+8                                                              
         MVI   PFAID,PFSTAY        ELSE SIM USER PRESSED 'STAY ON SCRN'         
         SPACE 1                                                                
         OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
VXIT     XIT1                                                                   
         EJECT                                                                  
*              DISPLAY FILTER VALIDATION ROUTINES                               
         SPACE 1                                                                
         USING SCAND,R3            R3=A(SCAN BLOCK ENTRY)                       
ACTVRTN  NTR1                                                                   
         TM    SCVAL2,X'80'        TEST VALID NUMERIC                           
         BZ    ACTVR2                                                           
         XR    RF,RF                                                            
         ICM   RF,7,SCBIN2+1       YES, MOVE TO RF                              
         LCR   RF,RF               AND COMPLEMENT                               
         GOTO1 ADDAY,DMCB,TGTODAY0,WORK,(RF) GO BACK N'DAYS                     
         B     ACTVRX                                                           
         SPACE 1                                                                
ACTVR2   GOTO1 DATVAL,DMCB,SCDATA2,WORK ELSE MUST BE A DATE                     
         OC    DMCB(4),DMCB                                                     
         BZ    FLDINVF                                                          
         SPACE 1                                                                
ACTVRX   GOTO1 DATCON,DMCB,(0,WORK),(1,ACTVDATE) LAST ACTIVE DATE               
         B     VXIT                                                             
         SPACE 2                                                                
MEDRTN   NTR1                                                                   
         LA    RF,MEDIAFLT         LOOP FOR THREE MEDIA                         
         LA    R0,NMEDIA                                                        
         CLI   0(RF),0                                                          
         BE    *+16                                                             
         LA    RF,L'MEDIAFLT(RF)                                                
         BCT   R0,*-12                                                          
         B     FLDINVF                                                          
         MVC   0(1,RF),SCDATA2                                                  
         CLI   SCDATA2,ALLBUT      ALL BUT                                      
         BNE   VXIT                                                             
         MVC   0(1,RF),SCDATA2+1                                                
         MVC   1(1,RF),SCDATA2                                                  
         B     VXIT                                                             
         EJECT                                                                  
*              DISPLAY FILTER VALIDATION ROUTINES  (CONT'D)                     
         SPACE 1                                                                
         USING SCAND,R3            R3=A(SCAN BLOCK ENTRY)                       
LINESRTN DS    0H                                                               
         BAS   RF,LINESCHK         VALIDATE * NOTE RF *                         
         MVC   CLINES,SCBIN2+3     SAVE N'LINES PER RECORD DISPLAYED            
         MVC   PLINES,SCBIN2+3                                                  
         BR    RE                                                               
         SPACE 1                                                                
CLINERTN DS    0H                                                               
         BAS   RF,LINESCHK         VALIDATE * NOTE RF *                         
         MVC   CLINES,SCBIN2+3     SAVE N'LINES PER RECORD DISPLAYED            
         BR    RE                                                               
         SPACE 1                                                                
PLINERTN DS    0H                                                               
         BAS   RF,LINESCHK         VALIDATE * NOTE RF *                         
         MVC   PLINES,SCBIN2+3     SAVE N'LINES PER RECORD DISPLAYED            
         BR    RE                                                               
         SPACE 2                                                                
LINESCHK DS    0H                                                               
         TM    SCVAL2,X'80'        TEST VALID NUMERIC                           
         BZ    FLDINVF                                                          
         CLI   SCBIN2+3,NLINEQU-1  TEST LARGEST VALUE                           
         BH    FLDINVF                                                          
         BR    RF                  * NOTE RF *                                  
         EJECT                                                                  
*              DISPLAY FILTER VALIDATION ROUTINES  (CONT'D)                     
         SPACE 1                                                                
         USING SCAND,R3            R3=A(SCAN BLOCK ENTRY)                       
STARTRTN DS    0H                                                               
         MVC   STARTCOM,SCDATA2    SAVE START COMMERCIAL ID                     
         BR    RE                                                               
         SPACE 1                                                                
HYPORTN  DS    0H                                                               
         MVC   STARTHYP,SCDATA2    STARTING HYPOTHETICAL COMMERCIAL             
         ZIC   RF,STARTHYP+L'STARTHYP-1                                         
         BCTR  RF,0                                                             
         STC   RF,STARTHYP+L'STARTHYP-1                                         
         BR    RE                                                               
         SPACE 1                                                                
PERFSRTN DS    0H                                                               
         LA    RF,PERFS            DISPLAY PERFORMERS                           
         B     YORNRTN                                                          
         SPACE 1                                                                
INCLRTN  DS    0H                                                               
         LA    RF,INCLUDED         DISPLAY NON-INCLUDED RECORDS                 
         B     YORNRTN2                                                         
         SPACE 1                                                                
ALLRTN   DS    0H                                                               
         TM    STATUS2,BUILDING    NOT VALID IF NOT BUILDING RECORD             
         BZ    FLDINVF                                                          
         LA    RF,ALLYES           WANT ALL RECORDS TO BE Y(ES)                 
         B     YORNRTN                                                          
         SPACE 1                                                                
LOCKRTN  DS    0H                                                               
         LA    RF,LOCKED           OK TO INCLUDE LOCKED COMMERCIALS             
         B     YORNRTN                                                          
         SPACE 1                                                                
RELRTN   DS    0H                                                               
         LA    RF,RELEASED         OK TO INCLUDE RELEASED COMMERCIALS           
         B     YORNRTN                                                          
         SPACE 1                                                                
EXPRTN   DS    0H                                                               
         LA    RF,EXPIRY           DISPLAY EXPIRATION DATES                     
         B     YORNRTN                                                          
         SPACE 2                                                                
MUSRTN   DS    0H                                                               
         LA    RF,MUSIC            EXCLUDE MUSIC COMMERCIALS                    
         B     YORNRTN                                                          
         SPACE 2                                                                
YORNRTN  DS    0H                                                               
         CLI   SCDATA2,C'Y'        VALID IS YES OR NO                           
         BE    *+12                                                             
YORNRTN2 CLI   SCDATA2,C'N'                                                     
         BNE   FLDINVF                                                          
         MVC   0(1,RF),SCDATA2     SAVE RHS VALUE                               
         BR    RE                                                               
         EJECT                                                                  
*              EXIT ROUTINES                                                    
         SPACE 1                                                                
DUPERRF  MVI   ERROR,ERDUPFLD      DUPLICATE INPUT FIELD                        
         B     *+8                                                              
FLDINVF  MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         GOTO1 EXIT,DMCB,0         ALL EXITS                                    
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              DISPLAY FILTER TABLE  (SEE DSECT FILTD)                          
         SPACE 1                                                                
FILTTAB  DS    0C                                                               
         DC    AL1(FLACTV,FILTER1-FILTERS)                                      
         DC    AL2(ACTVRTN-VALFILT)                                             
         DC    CL10'ACTIVE'                                                     
*                                                                               
         DC    AL1(FLALL,FILTER1-FILTERS)                                       
         DC    AL2(ALLRTN-VALFILT)                                              
         DC    CL10'ALL'                                                        
*                                                                               
         DC    AL1(FLSTART,FILTER1-FILTERS)                                     
         DC    AL2(STARTRTN-VALFILT)                                            
         DC    CL10'COMMERCIAL'                                                 
*                                                                               
         DC    AL1(FLCLINES,FILTER2-FILTERS)                                    
         DC    AL2(CLINERTN-VALFILT)                                            
         DC    CL10'CLINES'                                                     
*                                                                               
         DC    AL1(FLEXPIRY,FILTER2-FILTERS)                                    
         DC    AL2(EXPRTN-VALFILT)                                              
         DC    CL10'EXPIRY'                                                     
*                                                                               
         DC    AL1(FLHYPOS,FILTER1-FILTERS)                                     
         DC    AL2(HYPORTN-VALFILT)                                             
         DC    CL10'HYPOS'                                                      
*                                                                               
         DC    AL1(FLINCL,FILTER1-FILTERS)                                      
         DC    AL2(INCLRTN-VALFILT)                                             
         DC    CL10'INCLUDED'                                                   
*                                                                               
         DC    AL1(FLLINES,FILTER1-FILTERS)                                     
         DC    AL2(LINESRTN-VALFILT)                                            
         DC    CL10'LINES'                                                      
*                                                                               
         DC    AL1(FLOKLOCK,FILTER2-FILTERS)                                    
         DC    AL2(LOCKRTN-VALFILT)                                             
         DC    CL10'LOCKED'                                                     
*                                                                               
         DC    AL1(FLMEDIA,FILTER1-FILTERS)                                     
         DC    AL2(MEDRTN-VALFILT)                                              
         DC    CL10'MEDIA'                                                      
*                                                                               
         DC    AL1(FLPERFS,FILTER1-FILTERS)                                     
         DC    AL2(PERFSRTN-VALFILT)                                            
         DC    CL10'PERFORMERS'                                                 
*                                                                               
         DC    AL1(FLPLINES,FILTER2-FILTERS)                                    
         DC    AL2(PLINERTN-VALFILT)                                            
         DC    CL10'PLINES'                                                     
*                                                                               
         DC    AL1(FLOKREL,FILTER2-FILTERS)                                     
         DC    AL2(RELRTN-VALFILT)                                              
         DC    CL10'RELEASED'                                                   
*                                                                               
         DC    AL1(FLMUSIC,FILTER2-FILTERS)                                     
         DC    AL2(MUSRTN-VALFILT)                                              
         DC    CL10'MUSIC'                                                      
         DC    X'00'                                                            
         EJECT                                                                  
*              ROUTINE DISPLAYS RHS OF HYPO COMML LIST W/STEREO ACTIVE          
         SPACE 1                                                                
         USING LINED,R2                                                         
         USING TAESD,R4                                                         
DISSHRS  DS    0D                                                               
         NMOD1 0,*DISSHRS                                                       
         L     RC,SVRC                                                          
*                                                                               
         EDIT  TAESHLEN,(2,LINHSEC),ALIGN=LEFT                                  
         CLI   TAESSBNO,0          IF ANYTHING TO DISPLAY                       
         BE    DISSHRSX                                                         
         ZIC   R1,TAESSBNO                                                      
         CVD   R1,DUB                                                           
         ZAP   SUBELCNT,DUB                                                     
         LA    R4,TAESSBCD         R4=A(1ST SUB-ELEMENT)                        
         USING TAESSBCD,R4                                                      
*                                                                               
DISSHRS2 L     R1,AOPTTAB          LOOK FOR ENTRY IN TABLE                      
         USING OPTD,R1                                                          
*                                                                               
DISSHRS4 CLI   0(R1),X'FF'         TEST END OF TABLE                            
         BE    DISSHRSX                                                         
*                                                                               
         CLC   TAESSBCD,OPTCODE                                                 
         BE    DISSHRS6                                                         
         TM    OPTSTAT,OPTNOLHS    BASED ON L'CURRENT ENTYR                     
         BZ    *+12                                                             
         LA    R1,OPTNEXT                                                       
         B     *+8                                                              
         LA    R1,OPTNEXT2                                                      
         B     DISSHRS4                                                         
*                                                                               
         USING TAESSBCD,R4                                                      
DISSHRS6 CLI   TAESSBCD,OPLLIFT    IF LIFT LEN                                  
         BNE   DISSHRS7                                                         
         EDIT  (1,TAESSBDT),(2,LINHLSEC)                                        
         B     DISSHR10                                                         
*                                                                               
DISSHRS7 CLI   TAESSBCD,OPCTYP     IF COMMERCIAL TYPE                           
         BNE   DISSHRS8                                                         
         LR    R0,R1               SAVE R1                                      
         GOTO1 CTYPVAL,DMCB,TAESSBDT                                            
         BNE   *+10                                                             
         MVC   LINHTYPE,TGCTNAME                                                
         LR    R1,R0               RESTORE R1                                   
         B     DISSHR10                                                         
*                                                                               
DISSHRS8 CLI   TAESSBCD,OPCOMNM    IF COMML NAME                                
         BNE   DISSHRS9                                                         
         ZIC   RE,TAESSBDT                                                      
         BCTR  RE,0                                                             
         CHI   RE,34               ONLY SHOW FIRST 34 CHARACTERS                
         BNH   *+8                                                              
         LHI   RE,34                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   HCOMNAME(0),TAESSBDT+1                                           
         B     DISSHR10                                                         
*                                                                               
         USING TAESSBCD,R4                                                      
DISSHRS9 CLI   TAESSBCD,OPLLIF2    IF LIFT LEN                                  
         BNE   DISSHR10                                                         
         EDIT  (1,TAESSBDT),(2,LINHLSEC)                                        
*                                                                               
DISSHR10 ZIC   RE,OPTSUBLN         L'SUB EL IS IN TABLE                         
         LA    R4,1(RE,R4)         BUMP TO NXT SUB-ELEMENT                      
         SP    SUBELCNT,=P'1'                                                   
         BP    DISSHRS2                                                         
*                                                                               
DISSHRSX XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO VALIDATE AN OPTIONS FIELD                             
         SPACE 1                                                                
OPSCAN   DS    0D                                                               
         NMOD1 0,**OPSCAN,R8                                                    
         L     R2,0(R1)            P1 = R2=A(OPTIONS FIELD)                     
         L     RC,SVRC                                                          
         CLI   5(R2),0                                                          
         BE    OPSC10                                                           
         L     R3,AIO1             R3=A(SCAN BLOCK DATA)                        
         USING SCAND,R3                                                         
         LA    RF,RHSLNQ           SET NON-STANDARD RHS LENGTH                  
         GOTO1 SCANNER,DMCB,((RF),(R2)),(X'80',(R3)),0                          
         CLI   4(R1),0                                                          
         BE    FLDINV                                                           
         ZIC   R4,4(R1)            R4=N'SCAN ENTRIES                            
         SPACE 1                                                                
OPSC1    MVC   ERRDISP,SCDISP1     SET DISP. INTO FLD OF LHS FOR ERRORS         
         SPACE 1                                                                
         L     R1,AOPTTAB          LOOK UP ENTRY IN OPTION TABLE                
         USING OPTD,R1                                                          
OPSC2    CLI   0(R1),X'FF'                                                      
         BNE   *+16                                                             
         TM    STATUS2,BUILDING    IF WE'RE NOT BUILDING                        
         BZ    OPSC9               THEN IGNORE THIS ONE                         
         B     FLDINV                                                           
         TM    OPTSTAT,OPTIGN      IGNORE THIS OPTION                           
         BO    OPSC4                                                            
         MVC   BYTE,OPTOKBIT       IS THIS OPTION OK FOR THIS LEVEL             
         NC    BYTE,OPTOK                                                       
         BZ    OPSC4                                                            
         TM    STATUS2,BUILDING    IF WE'RE NOT BUILDING                        
         BO    *+12                                                             
         TM    OPTSTAT,OPTOKDIS    DO WE CARE ABOUT THIS OPTION                 
         BZ    OPSC4                                                            
         TM    OPTSTAT,OPTFORUS    DOES THIS OPTION REQUIRE USE TYPE            
         BZ    *+12                                                             
         CLI   OPTLAST,OPUSE       DO WE HAVE ONE                               
         BNE   OPSC4                                                            
         TM    OPTSTAT,OPTNORHS    IS RHS ALLOWED                               
         BZ    *+14                                                             
         CLC   SCDATA2,SPACES      NO - INSURE IT'S SPACES                      
         BNE   OPSC4                                                            
         TM    OPTSTAT,OPTNOLHS    USE I/P RTN TO CHECK ENTRY IF NO LHS         
         BZ    OPSC6                                                            
         XR    RF,RF                                                            
         ICM   RF,3,OPTIDSP        DISP. TO ROUTINE                             
         AR    RF,RB                                                            
         LA    RE,OPSC3            SET RE FOR COMMON NTR1                       
         NTR1                                                                   
         BR    RF                  *** GO TO INPUT ROUTINE ***                  
OPSC3    BE    OPSC8               THIS IS A MATCH                              
         SPACE 1                                                                
OPSC4    TM    OPTSTAT,OPTNOLHS    BASED ON L'CURRENT ENTRY                     
         BZ    *+12                                                             
         LA    R1,OPTNEXT          BUMP TO NEXT                                 
         B     *+8                                                              
         LA    R1,OPTNEXT2                                                      
         B     OPSC2                                                            
         SPACE 1                                                                
OPSC6    ZIC   RF,SCLEN1           MATCH FOR L'I/P                              
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SCDATA1(0),OPTLHS   LHS MUST MATCH                               
         BNE   OPSC4                                                            
         CLI   SCDATA1,C'E'        IF I/P STARTS WITH E                         
         BNE   OPSC6B                                                           
         LTR   RF,RF               L'I/P MUST BE AT LEAST 2                     
         BZ    OPSC4               (TO ALLOW "E" FOR EURO LOC FOR FGR)          
OPSC6B   XR    RF,RF                                                            
         ICM   RF,3,OPTIDSP        DISP. TO INPUT ROUTINE                       
         AR    RF,RB                                                            
         LA    RE,OPSC7            SET RE FOR COMMON NTR1                       
         NTR1                                                                   
         BR    RF                  *** GO TO INPUT ROUTINE ***                  
OPSC7    BNE   OPSC4                                                            
         SPACE 1                                                                
OPSC8    BAS   RE,DUPCHK           CHECK TO SEE IF OPTION INPUT ALREADY         
         BNE   *+10                                                             
         MVC   0(1,RF),OPTCODE     RETURNS RF=A(NEXT SLOT) - SAVE THIS          
         SPACE 1                                                                
         TM    OPTSTAT,OPTNOEL     IF THERE IS A SUB-EL FOR THIS OPTION         
         BO    *+8                                                              
         BAS   RE,INSUBEL          ADD THIS SUB-ELEMENT TO SUB-EL BLOCK         
         SPACE 1                                                                
         TM    OPTSTAT,OPTSVLST    SHOULD WE SET LAST OPTION                    
         BZ    *+14                                                             
         BAS   RE,PENDCHK          YES - 1ST SEE IF ANYTHING'S PENDING          
         MVC   OPTLAST,OPTCODE     SAVE THIS OPTION EQUATE                      
         SPACE 1                                                                
OPSC9    BCT   R4,*+8              IF HAVEN'T REACHED END                       
         B     *+12                                                             
         LA    R3,SCANNEXT+3       BUMP TO NEXT FLD (RHS IS 13 NOT 10)          
         B     OPSC1               AND VALIDATE IT                              
         SPACE 1                                                                
         ZIC   R1,ERRDISP          END OF LINE - BUMP ERRDISP BY L'LAST         
         ZIC   RE,SCLEN1                                         INPUT          
         LA    R1,1(RE,R1)                                                      
         ICM   RE,1,SCLEN2                                                      
         BZ    *+8                                                              
         LA    R1,1(RE,R1)                                                      
         STC   R1,ERRDISP                                                       
         SPACE 1                                                                
OPSC10   LA    R1,ESTOPT2H-ESTOPTSH(R2) R1=A(NEXT RHS FIELD)                    
         LA    RE,ESTOPT2H              RE=A(LAST RHS FIELD)                    
         CLI   OPTOKBIT,OPOKPRD    SET FOR PRODUCT OPTIONS                      
         BE    OPSC12                                                           
         SPACE 1                                                                
         ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R1,R2                                                            
         LA    RE,HYPOPT4H                                                      
         CLI   OPTOKBIT,OPOKHYCO   SET FOR HYPO. COMM'L. OPTIONS                
         BE    OPSC12                                                           
         SPACE 1                                                                
         LA    R1,LINHLNQ(R2)                                                   
         LA    RE,HYPLSTRH                                                      
         LA    RF,LINHLNQ-(LINHRHSH-LINHCATH)(R2) RF=A(NEXT LHS FIELD)          
         CLI   OPTOKBIT,OPOKHYPE   SET FOR HYPO. PERF. OPTIONS                  
         BE    OPSC11                                                           
         SPACE 1                                                                
         LA    R1,LINLNQ(R2)                                                    
         LA    RE,ESTLSTRH                                                      
         LA    RF,LINLNQ-(LINRHSH-LINDATAH)(R2) SET FOR OTHER OPTIONS           
         SPACE 1                                                                
OPSC11   TM    DISPMD2,SELCTPRF    IF PERFORMER SELECT SCREEN                   
         BO    OPSC12              IGNORE LHS                                   
         TM    DISPMODE,SELCTCOM   IF COMMERCIAL SELECT FIELD                   
         BO    OPSC12              IGNORE LHS                                   
         OC    8(3,RF),8(RF)       IF THERE'S DATA ON LHS OF NEXT               
         BNZ   OPSC14                                                           
         SPACE 1                                                                
OPSC12   CR    R2,RE               OR IF THIS IS THE LAST LINE                  
         BE    OPSC14                                                           
         CLI   5(R1),0             OR IF THERE'S NO DATA ON NEXT RHS            
         BNE   *+8                                                              
OPSC14   BAS   RE,PENDCHKU         SEE IF ANYTHING IS STILL PENDING             
         MVI   ERRDISP,0                                                        
         B     OXIT                                                             
         EJECT                                                                  
*              OPTION INPUT ROUTINES                                            
         SPACE 1                                                                
*                                  R2=A(OPTIONS FIELD)                          
         USING SCAND,R3            R3=A(SCAN ENTRY)                             
INI      DS    0H                                                               
         CLI   SCDATA1,C'I'        ** INSERT NUMBER OF LINES **                 
         BNE   ONO                                                              
         LA    RF,1                DEFAULT INSERT                               
         ZIC   RE,SCLEN1                                                        
         SH    RE,=H'2'                                                         
         BM    INI2                                                             
         LA    RF,1(RE)                                                         
         LA    R4,SCDATA1+1                                                     
         BAS   RE,NUMVL                                                         
         BNE   ONO                                                              
INI2     CLM   RF,1,=AL1(NLINEQU-1) MUST BE LESS THAN N'SCREEN LINES            
         BH    ONO                                                              
         STC   RF,SCDATA1          SAVE N'LINES TO INSERT                       
         B     INSEL2              SET SELECT TABLE CODE BELOW                  
         SPACE 3                                                                
         USING SCRND,R4                                                         
INADDL   DS    0H                                                               
         L     R4,ATHSSCRN                                                      
         OI    SCRNSTAT,SCRNOKHP   DON'T DELETE HYPO PERFORMERS                 
         B     INSEL2                                                           
         SPACE 1                                                                
INPERF   DS    0H                                                               
         L     R4,ATHSSCRN                                                      
         OI    SCRNSTAT,SCRNOKPR   DON'T DELETE PERFORMERS                      
         SPACE 1                                                                
         USING SELD,R4                                                          
INSEL2   CLI   PFAID,PFBEGIN       IGNORE IF RESTART PFKEY PRESSED              
         BE    OYES                                                             
         CLI   PFAID,PFPREV        OR IF USER WANTS PREVIOUS PAGE               
         BE    OYES                                                             
         CLI   PFAID,PFSTAY        OR IF USER WANTS THIS PAGE AGAIN             
         BE    OYES                                                             
         L     R4,ATHSSEL          R4=A(THIS SELECT TABLE ENTRY)                
         MVC   SELRHS,SCDATA1      SAVE RHS                                     
         MVC   WORK(1),SCDATA1     RETURN FOR SUB-ELEMENT                       
         B     INSELSET            SET SELECT TABLE KEY                         
         EJECT                                                                  
*              OPTION INPUT ROUTINES  (CONT'D)                                  
         SPACE 1                                                                
         USING LINED,R2            R2=A(OPTIONS FIELD)                          
         USING SCAND,R3            R3=A(SCAN ENTRY)                             
         USING SELD,R4             R4=A(SELECT TABLE ENTRY)                     
         SPACE 1                                                                
INSELSET DS    0H                  ** SET SELECT TABLE KEY **                   
         AP    SELCNT,=P'1'        ADD ONE TO COUNT                             
         LA    RF,SELNEXT          BUMP TABLE ENTRY                             
         ST    RF,ATHSSEL          SAVE A(NEXT OPEN SPOT)                       
         MVC   SELDA,DMDSKADD      SET D/A OF THIS RECORD                       
         TM    DISPMD2,LISTSPRF    IF WE'RE NOT LISTING PERFORMERS              
         BO    INSELS10                                                         
         TM    DISPMODE,LISTPRFS                                                
         BO    *+10                                                             
         MVC   SELMEDIA,TGMENAME   TAKE MEDIA FROM GLOBAL STORAGE               
INSELS10 B     OYES                                                             
         SPACE 2                                                                
ININTEG  DS    0H                                                               
         ZIC   RF,SCLEN2           L'INPUT                                      
         LA    R4,SCDATA2          A(INPUT)                                     
         MVI   BYTE,2              SET FOR TWO DEC. PLACES                      
         BAS   RE,CASHVL           VALIDATE AMOUNT                              
         BE    OYES                                                             
         B     AMTINV                                                           
         SPACE 2                                                                
INEXCH   DS    0H                                                               
         ZIC   RF,SCLEN2           L'INPUT                                      
         LA    R4,SCDATA2          A(INPUT)                                     
         MVI   BYTE,4              SET FOR FOUR DEC. PLACES                     
         BAS   RE,CASHVL           VALIDATE AMOUNT                              
         BE    OYES                                                             
         B     AMTINV                                                           
         SPACE 2                                                                
INCHAR   DS    0H                                                               
         MVC   WORK(1),SCDATA1     SAVE 1 CHARACTER INPUT                       
         B     OYES                                                             
         SPACE 2                                                                
INAPPLY  DS    0H                  APPLY SESSION/HLD                            
         CLI   SCLEN2,1            RHS MUST BE 1 CHAR                           
         BNE   ONO                                                              
         CLI   SCDATA2,C'S'        SESSION                                      
         BE    INAPPLX                                                          
         CLI   SCDATA2,C'H'        OR HLD                                       
         BE    INAPPLX                                                          
         CLI   SCDATA2,C'N'        OR DON'T APPLY                               
         BNE   ONO                                                              
INAPPLX  MVC   WORK(1),SCDATA2                                                  
         B     OYES                                                             
         EJECT                                                                  
*              OPTION INPUT ROUTINES  (CONT'D)                                  
         SPACE 1                                                                
INUSE    DS    0H                  CHECK FOR USE TYPE INPUT                     
         SPACE 1                                                                
         GOTO1 USEVAL,DMCB,(X'10',SCDATA1),SCDATA1+3                            
         BNE   ONO                                                              
         BAS   RE,PENDCHKU         SEE IF ANYTHING IS PENDING                   
         SPACE 1                                                                
         CLI   OPTOKBIT,OPOKPRD    IF THIS ISN'T PRODUCT LEVEL                  
         BE    INUSE2                                                           
         SPACE 1                                                                
         LA    RE,SCANNEXT+3       IF USE IS BEING EXCLUDED                     
         CLI   0(RE),1             SKIP THE MEDIA CHECK                         
         BNE   INUSE1                                                           
         CLC   SCDATA1-SCAND(10,RE),=CL10'N'                                    
         BE    INUSE2                                                           
*                                                                               
INUSE1   MVC   WORK(1),TGUSTYMD    INSURE USE TYPE IS OK FOR MEDIA              
         NC    WORK(1),TGMEEQU                                                  
         BZ    BADMEDIA                                                         
         SPACE 1                                                                
INUSE2   XC    OPUSLIST,OPUSLIST   CLEAR OPTIONS FOR USE                        
         TM    TGUSTYST,USES       SET PENDING BITS                             
         BZ    *+8                                                              
         OI    OPTPEND,OPTPUSES    N'USES REQUIRED                              
         TM    TGUSTYST,MAJORS                                                  
         BZ    *+8                                                              
         OI    OPTPEND,OPTPMAJ     MAJORS REQUIRED                              
         TM    TGUSTYST,UNITS                                                   
         BO    INUSE5                                                           
         CLI   TGUSEQU,UDIO        DIO: UNITS=NUMBER OF PROGRAMS                
         BE    INUSE5                                                           
         CLI   TGUSEQU,UCNM        CNM: UNITS=NUMBER OF CUTS                    
         BNE   *+8                                                              
INUSE5   OI    OPTPEND,OPTPUNIT    UNITS REQUIRED                               
         CLI   TGUSEQU,UGRT        GUARANTEES REQUIRE PERIOD                    
         BNE   *+8                                                              
         OI    OPTPEND,OPTPCYC     CYCLE DATES REQUIRED                         
         TM    TGUSSTA2,NORATES    IF RATES AREN'T DEFINED                      
         BZ    *+8                                                              
         OI    OPTPEND,OPTPAMT     AMOUNT REQUIRED                              
         TM    TGUSTYST,INSERTS                                                 
         BZ    *+8                                                              
         OI    OPTPEND,OPTPINS     INSERTS REQUIRED                             
         CLI   TGUSEQU,UTAG                                                     
         BNE   *+8                                                              
         OI    OPTPEND2,OPTPTAG    TAGS REQUIRED                                
         CLI   TGUSEQU,UDEM                                                     
         BE    *+12                                                             
         CLI   TGUSEQU,UCDM                                                     
         BNE   *+8                                                              
         OI    OPTPEND2,OPTPDEMO   DEMO REQUIRED                                
         SPACE 1                                                                
         MVC   WORK(1),TGUSEQU     RETURN USE EQUATE                            
         MVC   WORK+1(1),TGUSTYP      AND USE TYPE EQUATE                       
         B     OYES                                                             
         SPACE 2                                                                
INLCLAU  DS    0H                  CHECK FOR USE TYPE INPUT                     
*                                                                               
         GOTO1 USEVAL,DMCB,(X'10',SCDATA1),SCDATA1+3                            
         BNE   ONO                                                              
*                                                                               
         B     OYES                                                             
*                                                                               
INNO     DS    0H                  USER DOESN'T WANT THIS USE PAID              
         MVI   OPTPEND,0           DON'T CARE WHAT'S PENDING                    
         MVI   OPTPEND2,0                                                       
         SPACE 1                                                                
         CLI   AUSESUB,L'TGUSCDE   IF ONLY CODE WAS INPUT (NO TYPE)             
         BH    INNOX                                                            
         L     R4,AUSESUB          THEN SET A(USE SUB-ELEMENT)                  
         USING TAESSBCD,R4                                                      
         MVI   TAESSBDT+1,0        CLEAR TYPE                                   
         DROP  R4                                                               
INNOX    B     INCHAR                                                           
         EJECT                                                                  
*              OPTION INPUT ROUTINES  (CONT'D)                                  
         SPACE 1                                                                
INGRTNO  DS    0H                                                               
         CLI   SCDATA1,C'#'        MUST START WITH POUND SIGN                   
         BNE   ONO                                                              
         CLI   SCLEN1,5            MAX 4 DIGITS AFTER                           
         BH    ONO                                                              
         ZIC   RF,SCLEN1                                                        
         BCTR  RF,0                                                             
         LA    R4,SCDATA1+1                                                     
         BAS   RE,NUMVL            VALIDATE NUMERIC INPUT                       
         BNE   ONO                                                              
         LR    R4,RF               RETURNS BINARY AMOUNT IN RF                  
         OC    OPTSSN,OPTSSN       IF S/S NUMBER WAS INPUT                      
         BZ    INGRTX                                                           
         UNPK  TGGUA,DUB           SET GLOBAL GUARANTEE CODE                    
         XC    TGGUA,=4X'FF'       (COMPLEMENT)                                 
         GOTO1 RECVAL,DMCB,TLGUCDQ,0  INSURE IT'S ON FILE                       
         BNE   ONTFOUND                                                         
INGRTX   STH   R4,WORK             RETURN CODE IN BINARY                        
         STH   R4,OPTGRTNO         SAVE LOCALLY AS WELL                         
         NI    OPTPEND,ALL-OPTPGRT SET NO LONGER PENDING                        
         B     OYES                                                             
         SPACE 1                                                                
INSSN    DS    0H                                                               
         TM    TGSYSTAT,TASYSPID   USING PID?                                   
         BZ    ISSN10                                                           
         CLI   SCLEN1,6            PID IS 6 CHARACTERS                          
         BNE   ISSN10                                                           
         GOTO1 SSNUNPK,DMCB,SCDATA1,MYWORK  CONVERT PID TO SSN                  
         BNE   ONO                                                              
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'80',MYWORK)                               
         B     ISSN20                                                           
*                                                                               
ISSN10   TM    SCVAL1,X'80'        MUST BE NUMERIC                              
         BZ    ONO                                                              
         CLI   SCLEN1,9            MUST BE 9 DIGITS                             
         BNE   ONO                                                              
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'80',SCDATA1)                              
ISSN20   BNE   ONO                                                              
         MVC   OPTSSN,TGSSN                                                     
         PACK  DUB,TGSSN                                                        
         CVB   R1,DUB                                                           
         ST    R1,WORK             RETURN BINARY VALUE                          
         OC    OPTGRTNO,OPTGRTNO   IF NOT ALREADY INPUT                         
         BNZ   *+8                                                              
         OI    OPTPEND,OPTPGRT     SET GUARANTEE NUMBER PENDING                 
         B     OYES                                                             
         SPACE 1                                                                
INPRI    DS    0H                  PRIMARY COMMERCIAL INDICATOR                 
         CLI   SCLEN1,3            MUST BE AT LEAST 3 CHARS                     
         BL    ONO                                                              
         OC    OPTGRTNO,OPTGRTNO   IF NOT ALREADY INPUT                         
         BNZ   *+8                                                              
         OI    OPTPEND,OPTPGRT     SET GUARANTEE NUMBER PENDING                 
         B     OYES                OK - NOTHING TO SAVE                         
         SPACE 1                                                                
INGUAR   DS    0H                  GUARANTEE CREDITTING STATUS                  
         CLI   SCDATA2,C'N'        VALID RHS IS 'N'O                            
         BE    *+12                                                             
         CLI   SCDATA2,C'Y'                  OR 'Y'ES                           
         BNE   ONO                                                              
         MVC   WORK(1),SCDATA2     SAVE 1ST CHAR OF RHS                         
         B     OYES                                                             
         EJECT                                                                  
*              OPTION INPUT ROUTINES  (CONT'D)                                  
         SPACE 1                                                                
INCYC    DS    0H                  * CYCLE DATES *                              
         LA    R4,SCDATA1          R4=A(DATE)                                   
         ZIC   RE,SCLEN1                                                        
         LTR   RE,RE                                                            
         BZ    ONO                                                              
         MVC   MYWORK,SPACES       SET FIRST CYCLE DATE IN MYWORK               
         LA    R1,MYWORK                                                        
INCYC1   CLI   0(R4),C'-'          LOOK FOR END OF CYCLE START                  
         BE    INCYC1A                                                          
         MVC   0(1,R1),0(R4)                                                    
         LA    R1,1(R1)                                                         
         LA    R4,1(R4)                                                         
         BCT   RE,INCYC1                                                        
         B     ONO                 NOT CYCLE DATES                              
         SPACE 1                                                                
INCYC1A  LA    R4,MYWORK           PASS DTVAL FIRST DATE SPACE PADDED           
         MVI   BYTE,1              RETURN FORMAT PWOS                           
         BAS   RE,DATEVAL                                                       
         LA    R4,SCDATA1          THEN RESET BACK TO CYCLE DATES               
         BNE   ONO                                                              
         A     R4,FULL             FULL=L'FIRST DATE FROM DATVAL                
         CLI   0(R4),C'-'          HYPHEN BETWEEN DATES                         
         BNE   ONO                                                              
         SPACE 1                                                                
         CLI   TGUSEQU,UGRT        IF THIS IS A GUARANTEE                       
         BNE   INCYC2                                                           
         TM    OPTPEND,OPTPCYC     DATES SHOULD BE PENDING                      
         BZ    DUPERR                                                           
         XI    OPTPEND,OPTPCYC                                                  
         SPACE 1                                                                
INCYC2   MVC   WORK+40(3),WORK     SAVE 1ST DATE                                
         LA    R4,1(R4)                                                         
         BAS   RE,DATEVAL          VALIDATE 2ND DATE                            
         BNE   DATINV                                                           
         CLC   WORK+40(3),WORK     START MUST BE LT END                         
         BH    ENDINV                                                           
         MVC   WORK+50(3),WORK     SAVE 2ND DATE                                
         SPACE 1                                                                
         CLI   TGUSWKS,0           IF NO CYCLE LENGTH DEFINED                   
         BE    INCYC4              FINISHED                                     
****************************************************************                
         B     INCYC4              BYPASS CODE - NEED PDVAL CALL                
****************************************************************                
* NO-OP                                                                         
* NO-OP  MVC   TGBYTE,TGUSWKS      SAVE L'CYCLE                                 
* NO-OP  GOTO1 DATCON,DMCB,(2,WORK+40),(0,WORK+42) CVT START TO EBCDIC          
* NO-OP  GOTO1 (RF),(R1),(2,WORK+50),(0,WORK+52)       END TO EBCDIC            
* NO-OP  GOTO1 CALEND,DMCB,(TGBYTE,WORK+42),WORK+30 CALC ACTUAL CYC END         
* NO-OP  CLC   WORK+52(6),WORK+30  END DATES MUST MATCH                         
* NO-OP  BE    INCYC4                                                           
* NO-OP  SPACE 1                                                                
* NO-OP  CLI   TGBYTE,13           IF FIXED CYCLE IS NORMALLY 13 WEEKS          
* NO-OP  BNE   ENDINV                                                           
* NO-OP  GOTO1 (RF),(R1),(X'83',WORK+42),WORK+30 TRY THREE MONTHS               
* NO-OP  CLC   WORK+52(6),WORK+30  END DATES MUST MATCH                         
* NO-OP  BNE   ENDINV                                                           
         SPACE 1                                                                
INCYC4   MVC   WORK(3),WORK+40     BOTH DATES OK - PUT BOTH IN WORK             
         MVC   WORK+3(3),WORK+50                                                
         B     OYES                                                             
         EJECT                                                                  
*              OPTION INPUT ROUTINES  (CONT'D)                                  
         SPACE 1                                                                
INUNITS  DS    0H                  N'UNITS                                      
         CLI   TGUSEQU,URTK        RTK: UNITS=SCRIPT TYPE                       
         BE    INUNTS20                                                         
         CLI   TGUSEQU,UDIO        DIO: UNITS=NUMBER OF PROGRAMS                
         BE    INUNTS10                                                         
         CLI   TGUSEQU,UCNM        CNM: UNITS=NUMBER OF CUTS                    
         BE    INUNTS10                                                         
*                                                                               
         TM    TGUSTYST,UNITS      DO WE NEED UNITS                             
         BZ    ONO                                                              
INUNTS10 TM    SCVAL1,X'80'        TEST FOR NUMERIC FIELD                       
         BZ    ONO                                                              
         TM    OPTPEND,OPTPUNIT    UNITS SHOULD BE PENDING                      
         BZ    ONO                 IF NOT, RETURN BECAUSE MAY BE N'USES         
         XI    OPTPEND,OPTPUNIT                                                 
         MVC   WORK(2),SCBIN1+2    RETURN BINARY VALUE                          
         MVC   OPTUNITS,WORK                                                    
         B     INWSPCHK            INSURE WE HAVE MAJORS OR UNITS               
*                                                                               
INUNTS20 CLC   SCDATA1,=CL10'ENTR'  ENTIRE SCRIPT                               
         BNE   *+14                                                             
         XC    OPTUNITS,OPTUNITS                                                
         B     INUNTS50                                                         
*                                                                               
         CLC   SCDATA1,=CL10'PART'  PARTIAL SCRIPT                              
         BNE   ONO                                                              
         MVC   OPTUNITS,=X'0001'                                                
INUNTS50 MVC   WORK(2),OPTUNITS                                                 
         B     OYES                                                             
         SPACE 2                                                                
INMAJ    DS    0H                                                               
         CLI   TGUSEQU,URTK        RTK: >60 DAYS                                
         BE    INMAJ5                                                           
         TM    TGUSTYST,MAJORS     DO WE NEED MAJORS                            
         BZ    ONO                                                              
         CLI   SCLEN1,9            MAKE SURE LENGTH NOT MORE THAN 9             
         BH    ONO                                                              
         GOTO1 MAJVAL,DMCB,SCDATA1 VALIDATE THEM                                
         BNE   ONO                                                              
         TM    OPTPEND,OPTPMAJ     MAJORS SHOULD BE PENDING                     
         BZ    DUPERR                                                           
         XI    OPTPEND,OPTPMAJ                                                  
         MVC   WORK(1),TGMAEQU     SAVE MAJORS EQUATE                           
         MVC   OPTMAJOR,TGMAEQU                                                 
         B     INWSPCHK                                                         
*                                                                               
INMAJ5   CLC   SCDATA1,=CL10'>60'  RTK: >60 DAYS                                
         BNE   ONO                                                              
         MVI   OPTMAJOR,X'01'                                                   
         MVC   WORK(1),OPTMAJOR                                                 
         B     OYES                                                             
         SPACE 2                                                                
INWSPCHK DS    0H                                                               
         TM    TGUSTYST,MAJORS+UNITS     ARE MAJORS AND UNITS REQUIRED          
         BNO   OYES                                                             
         TM    OPTPEND,OPTPMAJ+OPTPUNIT  HAVE THEY BOTH BEEN INPUT              
         BNZ   OYES                                                             
         OC    OPTMAJOR(3),OPTMAJOR      INSURE THEY'RE BOTH NOT ZERO           
         BZ    FLDINV                                                           
         B     OYES                                                             
         SPACE 2                                                                
INSTATE  DS    0H                  ADDENDUM STATE                               
         CLI   SCLEN2,2                                                         
         BNE   ONO                                                              
         GOTO1 TAXVAL,DMCB,(2,SCDATA2)  VALIDATE STATE CODE                     
         BNE   ONO                                                              
         TM    TGTASTAT,TALUOKAD   TEST ADDENDUM CONTRACT INSTALLED             
         BZ    FLDINVR                                                          
         MVC   WORK(LOPSTATE),SCDATA2                                           
         MVC   OPTSTATE,SCDATA2                                                 
         NI    OPTPEND,ALL-OPTPSTA SET NO LONGER PENDING                        
         B     OYES                                                             
         EJECT                                                                  
*              OPTION INPUT ROUTINES  (CONT'D)                                  
         SPACE 1                                                                
ININS    DS    0H                  N'INSERTS                                    
         TM    TGUSTYST,INSERTS    DO WE NEED INSERTS                           
         BZ    ONO                                                              
         TM    SCVAL1,X'80'        TEST FOR NUMERIC FIELD                       
         BZ    ONO                                                              
         OC    SCBIN1+1(3),SCBIN1+1  MUST BE GT ZERO                            
         BZ    ONO                                                              
         TM    OPTPEND,OPTPINS     INSERTS SHOULD BE PENDING                    
         BZ    ONO                                                              
         XI    OPTPEND,OPTPINS                                                  
         MVC   WORK(2),SCBIN1+2    RETURN BINARY VALUE                          
         B     OYES                                                             
         SPACE 2                                                                
INTAG    DS    0H                  N'TAGS                                       
         CLI   TGUSEQU,UTAG        DO WE NEED TAGS                              
         BNE   ONO                                                              
         TM    SCVAL1,X'80'        TEST FOR NUMERIC FIELD                       
         BZ    ONO                                                              
         XR    RE,RE                                                            
         ICM   RE,7,SCBIN1+1                                                    
         LTR   RE,RE               MUST BE GT ZERO                              
         BZ    ONO                                                              
         C     RE,=F'255'          BUT LESS THAN 256                            
         BH    ONO                                                              
         TM    OPTPEND2,OPTPTAG    TAGS SHOULD BE PENDING                       
         BZ    ONO                                                              
         XI    OPTPEND2,OPTPTAG                                                 
         MVC   WORK(1),SCBIN1+3    RETURN BINARY VALUE                          
         B     OYES                                                             
         SPACE 2                                                                
INDEMO   DS    0H                  N'DEMOS                                      
         CLI   TGUSEQU,UDEM        DO WE NEED DEMOS                             
         BE    *+12                                                             
         CLI   TGUSEQU,UCDM                                                     
         BNE   ONO                                                              
         TM    SCVAL1,X'80'        TEST FOR NUMERIC FIELD                       
         BZ    ONO                                                              
         XR    RE,RE                                                            
         ICM   RE,7,SCBIN1+1                                                    
         LTR   RE,RE               MUST BE GT ZERO                              
         BZ    ONO                                                              
         C     RE,=F'255'          BUT LESS THAN 256                            
         BH    ONO                                                              
         TM    OPTPEND2,OPTPDEMO   DEMOS SHOULD BE PENDING                      
         BZ    ONO                                                              
         XI    OPTPEND2,OPTPDEMO                                                
         MVC   WORK(1),SCBIN1+3    RETURN BINARY VALUE                          
         B     OYES                                                             
         SPACE 2                                                                
INAFM    DS    0H                  AFM RATE                                     
         CLI   SCLEN2,1                                                         
         BNE   ONO                                                              
         CLI   SCDATA2,C'1'        CHOICES ARE '1'                              
         BE    INAFMX                                                           
         CLI   SCDATA2,C'2'                    '2'                              
         BE    INAFMX                                                           
         CLI   SCDATA2,C'5'                    '5'                              
         BNE   ONO                                                              
INAFMX   MVC   WORK(1),SCDATA2                                                  
         B     OYES                                                             
         EJECT                                                                  
*              OPTION INPUT ROUTINES  (CONT'D)                                  
         SPACE 1                                                                
INPAY1   DS    0H                  N'USES                                       
         TM    SCVAL1,X'80'        TEST FOR NUMERIC FIELD                       
         BZ    ONO                                                              
         OC    SCBIN1+1(3),SCBIN1+1  MUST BE GT ZERO                            
         BZ    ONO                                                              
         MVC   WORK(2),SCBIN1+2    RETURN BINARY N'USES                         
         B     OYES                                                             
         SPACE 2                                                                
         SPACE 1                                                                
INUSES   DS    0H                  N'USES                                       
         TM    TGUSTYST,USES       WE MUST NEED USE NUMBER                      
         BZ    ONO                                                              
         TM    SCVAL1,X'80'        TEST FOR NUMERIC FIELD                       
         BZ    ONO                                                              
         OC    SCBIN1+1(3),SCBIN1+1  MUST BE GT ZERO                            
         BZ    ONO                                                              
         TM    OPTPEND,OPTPUSES    N'USES SHOULD BE PENDING                     
         BZ    DUPERR                                                           
         XI    OPTPEND,OPTPUSES                                                 
         MVC   WORK(2),SCBIN1+2    RETURN BINARY N'USES                         
         B     OYES                                                             
         SPACE 2                                                                
INUSES2  DS    0H                  SPECIFIC USE NUMBERS                         
         TM    TGUSTYST,USES       WE MUST NEED USE NUMBER                      
         BZ    ONO                                                              
         MVC   ELEMENT(80),SPACES  SET TO SCAN FIELD                            
         MVC   ELEMENT(L'SCDATA1),SCDATA1                                       
         LA    R3,WORK+4                                                        
         GOTO1 SCANNER,DMCB,(C'C',ELEMENT),(R3),C',=,-'                         
         TM    SCVAL1,X'80'        STARTING USE NUMBER MUST BE NUMERIC          
         BZ    ONO                                                              
         OC    SCBIN1,SCBIN1       AND MUST BE GT ZERO                          
         BZ    ONO                                                              
         SPACE 1                                                                
         CLI   SCLEN2,0            OK TO NOT HAVE RHS                           
         BNE   INUSES2B                                                         
         CLI   4(R1),2             FOR SOME REASON INPUT OF NNN- CAUSES         
         BNE   ONO                 SCANNER TO RETURN 2 ENTRIES FOUND            
         MVC   SCBIN2,SCBIN1       USE SAME AS LHS                              
         B     INUSES2X                                                         
         SPACE 1                                                                
INUSES2B CLI   4(R1),1             ELSE THERE SHOULD ONLY BE ONE ENTRY          
         BNE   ONO                                                              
         TM    SCVAL2,X'80'        ENDING USE NUMBER MUST BE NUMERIC            
         BZ    ONO                                                              
         OC    SCBIN2,SCBIN2       AND MUST BE GT ZERO                          
         BZ    ONO                                                              
         CLC   SCBIN2,SCBIN1       END MUST BE GE START                         
         BL    ONO                                                              
         SPACE 1                                                                
INUSES2X TM    OPTPEND,OPTPUSES    N'USES SHOULD BE PENDING                     
         BZ    DUPERR                                                           
         XI    OPTPEND,OPTPUSES                                                 
         MVC   WORK(2),SCBIN1+2    RETURN BINARY START AND                      
         MVC   WORK+2(2),SCBIN2+2  END USE NUMBERS                              
         B     OYES                                                             
         EJECT                                                                  
*              OPTION INPUT ROUTINES  (CONT'D)                                  
         SPACE 1                                                                
INDATE   DS    0H                  DATE INPUT                                   
         LA    R4,SCDATA1          R4=A(DATE)                                   
         ZIC   RF,SCLEN1           RF=L'INPUT                                   
         SPACE 2                                                                
INDATE2  MVI   BYTE,1              RETURN FORMAT PWOS                           
         BAS   RE,DATEVAL                                                       
         BNE   ONO                                                              
         CLM   RF,1,FULL+3         INSURE RF=L'DATE                             
         BNE   ONO                                                              
         B     OYES                                                             
         SPACE 2                                                                
INDATER  DS    0H                  DATE INPUT ON RHS OF '='                     
         LA    R4,SCDATA2          R4=A(DATE)                                   
         MVI   BYTE,1              RETURN FORMAT PWOS                           
         BAS   RE,DATEVAL                                                       
         BE    OYES                                                             
         B     DATINV                                                           
         SPACE 2                                                                
INEXP    DS    0H                  EXPIRY DATE                                  
         CLI   SCLEN2,1                                                         
         BNE   INDATER                                                          
         CLI   SCDATA2,C'Y'        ALLOW 'Y'                                    
         BNE   INDATER                                                          
         MVI   WORK,X'FF'          SAVE ARTIFICIAL DATE                         
         B     OYES                                                             
         SPACE 2                                                                
INASOF   DS    0H                  AS OF DATE                                   
*NO-OP   TM    TGUSSTA2,APPREUSE   TEST USE APPLICABLE AGAINST REUSE            
*4/29    BZ    ONO                                                              
         CLI   SCDATA1,C'>'        CHECK FOR GREATER THAN SIGN                  
         BNE   ONO                                                              
         LA    R4,SCDATA1+1        R4=A(DATE)                                   
         ZIC   RF,SCLEN1                                                        
         BCTR  RF,0                RF=L'INPUT                                   
         B     INDATE2                                                          
         EJECT                                                                  
*              OPTION INPUT ROUTINES  (CONT'D)                                  
         SPACE 1                                                                
INOVER2  DS    0H                  SECOND OVERSCALE FOR USE                     
         CLI   SCDATA2,C'/'                                                     
         BNE   ONO                                                              
         LA    R4,SCDATA2+1                                                     
         ZIC   RF,SCLEN2                                                        
         BCTR  RF,0                                                             
         BAS   RE,PCTVAL           VALIDATE PERCENTAGE                          
         BNE   AMTINV                                                           
         B     OYES                                                             
         SPACE 1                                                                
INOV12   DS    0H                  OVERSCALE 1 & 2                              
         MVC   ELEMENT+4(80),SPACES SET TO SCAN FIELD                           
         MVC   ELEMENT+4(RHSLNQ),SCDATA2                                        
         LA    R3,WORK+4                                                        
         GOTO1 SCANNER,DMCB,(C'C',ELEMENT+4),(R3),C',=,/'                       
         CLI   4(R1),1             SHOULD ONLY BE ONE ENTRY                     
         BNE   ONO                                                              
         CLI   SCLEN2,0            WITH SECOND HALF                             
         BE    ONO                                                              
         ZIC   RF,SCLEN1           VALIDATE FIRST OVERSCALE                     
         LA    R4,SCDATA1                                                       
         BAS   RE,PCTVAL                                                        
         BNE   AMTINV                                                           
         MVC   ELEMENT(4),WORK                                                  
         ZIC   RF,SCLEN2           VALIDATE SECOND OVERSCALE                    
         LA    R4,SCDATA2                                                       
         BAS   RE,PCTVAL                                                        
         BNE   AMTINV                                                           
         MVC   WORK+4(4),WORK      SET OV2                                      
         MVC   WORK(4),ELEMENT     SET OV1                                      
         B     OYES                                                             
         SPACE 1                                                                
INOVER   DS    0H                  OVERSCALE FOR USE                            
         ZIC   RF,SCLEN2           L'INPUT                                      
         LA    R4,SCDATA2          A(INPUT)                                     
         BAS   RE,PCTVAL           VALIDATE PERCENTAGE                          
         BNE   AMTINV                                                           
         B     OYES                                                             
         EJECT                                                                  
*              OPTION INPUT ROUTINES  (CONT'D)                                  
         SPACE 1                                                                
INAMT    DS    0H                                                               
         ZIC   RF,SCLEN1           L'INPUT                                      
         MVI   BYTE,2              2 DECIMAL PLACES                             
         LA    R4,SCDATA1          A(INPUT)                                     
         BAS   RE,CASHVL           VALIDATE AMOUNT                              
         BNE   ONO                                                              
         TM    TGUSSTA2,NORATES    IF RATES AREN'T DEFINED IN TABLE             
         BZ    OYES                                                             
         TM    OPTPEND,OPTPAMT     AMOUNT SHOULD BE PENDING                     
         BZ    DUPERR                                                           
         XI    OPTPEND,OPTPAMT                                                  
         B     OYES                                                             
         SPACE 1                                                                
INAMTR   DS    0H                                                               
         ZIC   RF,SCLEN2           L'INPUT                                      
         MVI   BYTE,2              2 DECIMAL PLACES                             
         LA    R4,SCDATA2          A(INPUT)                                     
         BAS   RE,CASHVL           VALIDATE AMOUNT                              
         BNE   ONO                                                              
         B     OYES                                                             
         SPACE 2                                                                
INPCT    DS    0H                                                               
         ZIC   RF,SCLEN1                                                        
         LA    R1,SCDATA1-1(RF)                                                 
         CLI   0(R1),C'%'          CHECK FOR TRAILING PERCENT SIGN              
         BNE   ONO                 IF NONE, RETURN CC NOT EQUAL                 
         LA    R4,SCDATA1          A(INPUT)                                     
         BAS   RE,PCTVAL           VALIDATE AMOUNT                              
         BNE   ONO                                                              
         L     R1,WORK             DON'T WANT DECIMAL PLACES                    
         XR    R0,R0                                                            
         D     R0,=F'100'                                                       
         LTR   R0,R0                                                            
         BNZ   AMTINV                                                           
         STH   R1,WORK             RETURN 2 BYTES, NO DEC. PLACES               
         B     OYES                                                             
         SPACE 2                                                                
INPCTR   DS    0H                  PCT. INPUT ON RHS OF '='                     
         ZIC   RF,SCLEN2           L'INPUT                                      
         LA    R4,SCDATA2          A(INPUT)                                     
         BAS   RE,PCTVAL           VALIDATE PERCENTAGE                          
         BE    OYES                                                             
         B     AMTINV                                                           
         EJECT                                                                  
*              OPTION INPUT ROUTINES  (CONT'D)                                  
         SPACE 1                                                                
INWPCTR  DS    0H                  WHOLE PCT. INPUT ON RHS OF '='               
         ZIC   RF,SCLEN2           L'INPUT                                      
         LA    R1,SCDATA2-1(RF)                                                 
         CLI   0(R1),C'%'          CHECK FOR TRAILING PERCENT SIGN              
         BNE   ONO                 IF NONE, RETURN CC NOT EQUAL                 
         LA    R4,SCDATA2          A(INPUT)                                     
         BAS   RE,PCTVAL           VALIDATE PERCENTAGE                          
         BE    OYES                                                             
         B     AMTINV                                                           
         SPACE 2                                                                
INYEAR   DS    0H                  VALIDATE CONTRACT YEAR                       
         GOTO1 YRVAL,DMCB,SCDATA2                                               
         BNE   FLDINV                                                           
         MVC   WORK(1),TGYREQU     RETURN EQUATE                                
         B     OYES                                                             
         SPACE 2                                                                
INBINR   DS    0H                  BINARY AMOUNT ON RHS                         
         TM    SCVAL2,X'80'        TEST FOR NUMERIC FIELD                       
         BZ    ONO                                                              
         OC    SCBIN2+1(3),SCBIN2+1  MUST BE GT ZERO                            
         BZ    ONO                                                              
         MVC   WORK(1),SCBIN2+3    RETURN 1 BYTE BINARY                         
         B     OYES                                                             
         SPACE 2                                                                
INCTYP   DS    0H                  COMMERCIAL TYPE                              
         GOTO1 CTYPVAL,DMCB,SCDATA2                                             
         BNE   ONO                                                              
         MVC   WORK(L'TGCTEQU),TGCTEQU                                          
         CLI   TGCTEQU,CTYADD      IF COMMERCIAL TYPE IS ADDENDUM               
         BNE   INCTYX                                                           
         OC    OPTSTATE,OPTSTATE   AND NOT ALREADY INPUT                        
         BNZ   INCTYX                                                           
         OI    OPTPEND,OPTPSTA     SET ADDENDUM STATE PENDING                   
INCTYX   B     OYES                                                             
         SPACE 2                                                                
INATYP   DS    0H                  COMMERCIAL ACTRA TYPE                        
         GOTO1 CCTYPVAL,DMCB,SCDATA2                                            
         BNE   ONO                                                              
         MVC   WORK(L'TGCCTEQU),TGCCTEQU                                        
         B     OYES                                                             
         EJECT                                                                  
*              OPTION INPUT ROUTINES  (CONT'D)                                  
         SPACE 1                                                                
INCSF    DS    0H                  CONTRACT SERVICE FEE                         
         CLI   TGUSEQU,UBSC        VALID FOR BSC, CDM, & OTC                    
         BE    INCSF5                                                           
         CLI   TGUSEQU,UCDM                                                     
         BE    INCSF5                                                           
         CLI   TGUSEQU,UBSC                                                     
         BNE   ONO                                                              
INCSF5   CLI   SCDATA2,C'N'        VALID RHS IS 'N'O                            
         BE    *+12                                                             
         CLI   SCDATA2,C'Y'                  OR 'Y'ES                           
         BNE   INAMTR                        OR AMOUNT                          
         XC    WORK(4),WORK                                                     
         MVC   WORK(1),SCDATA2     SAVE 1ST CHAR OF RHS                         
         B     OYES                                                             
         SPACE 2                                                                
INHND    DS    0H                  INCORPORATED HANDLING AMT OVERRIDE           
         CLI   TGUSEQU,UGRT        VALID FOR GRT ONLY                           
         BNE   ONO                                                              
         CLI   CORPFLAG,TAW4TYCO   AND PERF MUST BE A CORP OR TRUSTEE           
         BE    INAMTR                                                           
         CLI   CORPFLAG,TAW4TYTR                                                
         BE    INAMTR                                                           
         B     ONO                                                              
         SPACE 2                                                                
INPRBR   DS    0H                  WHERE PRODUCED OR BROADCAST                  
         ZIC   RF,SCLEN2                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SCDATA2(0),=C'US'                                                
         BE    INPRBR4                                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SCDATA2(0),=C'CANADA'                                            
         BNE   ONO                                                              
INPRBR4  MVC   WORK(1),SCDATA2     SAVE 1ST CHAR OF COUNTRY                     
         B     OYES                                                             
         SPACE 2                                                                
INPNHR   DS    0H                  P&H RATE                                     
         CLC   =C'P&&HR',SCDATA1   INSURE ALL 4 CHARS INPUT                     
         BNE   ONO                                                              
         B     INPCTR                                                           
         SPACE 2                                                                
INHLDR   DS    0H                  HOLDING FEE PCT                              
         CLI   TGMEEQU,RADIO                                                    
         BE    FLDINV              NOT VALID ON RADIO COMMERCIALS               
         B     INWPCTR                                                          
         EJECT                                                                  
*              OPTION INPUT ROUTINES  (CONT'D)                                  
         SPACE 1                                                                
INOV2AS  DS    0H                  OVERSCALE 2 AS OF DATE                       
         BAS   RE,CKASOF           CHECK FOR '>MM/DD/YY' ON LHS                 
         BNE   ONO                 (RETURNS PWOS DATE IN WORK)                  
         LA    R4,SCDATA2          A(INPUT)                                     
         CLI   0(R4),C'/'          IF SECOND OVERSCALE                          
         BNE   ONO                                                              
         LA    R4,1(R4)                                                         
         ZIC   RF,SCLEN2           NOW VALIDATE RATE  - SET L'INPUT-1           
         BCTR  RF,0                                                             
         MVC   SCDATA1(3),WORK     SAVE DATE IN SCAN BLOCK                      
         BAS   RE,PCTVAL           VALIDATE PERCENTAGE                          
         BNE   AMTINV                                                           
         MVC   SCDATA2(4),WORK     MOVE RATE TO SCAN BLOCK                      
         MVC   WORK(3),SCDATA1     RETURN DATE                                  
         MVC   WORK+3(4),SCDATA2   AND RATE                                     
         B     OYES                                                             
         SPACE 1                                                                
INOV12A  DS    0H                  OVERSCALE 1 & 2 AS/OF A DATE                 
         BAS   RE,CKASOF           CHECK FOR '>MM/DD/YY' ON LHS                 
         BNE   ONO                                                              
         MVC   ELEMENT(3),WORK     SAVE DATE                                    
         MVC   ELEMENT+11(80),SPACES SET TO SCAN FIELD                          
         MVC   ELEMENT+11(RHSLNQ),SCDATA2                                       
         LA    R3,WORK+11                                                       
         GOTO1 SCANNER,DMCB,(C'C',ELEMENT+11),(R3),C',=,/'                      
         CLI   4(R1),1             SHOULD ONLY BE ONE ENTRY                     
         BNE   ONO                                                              
         CLI   SCLEN2,0            WITH SECOND HALF                             
         BE    ONO                                                              
         ZIC   RF,SCLEN1           VALIDATE FIRST OVERSCALE                     
         LA    R4,SCDATA1                                                       
         BAS   RE,PCTVAL                                                        
         BNE   AMTINV                                                           
         MVC   ELEMENT+3(4),WORK                                                
         ZIC   RF,SCLEN2           VALIDATE SECOND OVERSCALE                    
         LA    R4,SCDATA2                                                       
         BAS   RE,PCTVAL                                                        
         BNE   AMTINV                                                           
         MVC   WORK+7(4),WORK      SET OV2                                      
         MVC   WORK+3(4),ELEMENT+3 SET OV1                                      
         MVC   WORK(3),ELEMENT     SET AS/OF DATE                               
         B     OYES                                                             
         SPACE 1                                                                
         EJECT                                                                  
*              OPTION INPUT ROUTINES  (CONT'D)                                  
         SPACE 1                                                                
INOVAS   DS    0H                  OVERSCALE AS OF DATE                         
         BAS   RE,CKASOF           CHECK FOR '>MM/DD/YY' ON LHS                 
         BNE   ONO                 (RETURNS PWOS DATE IN WORK)                  
         MVC   SCDATA1(3),WORK     SAVE DATE IN SCAN BLOCK                      
         ZIC   RF,SCLEN2           NOW VALIDATE RATE  - SET L'INPUT             
         LA    R4,SCDATA2          A(INPUT)                                     
         BAS   RE,PCTVAL           VALIDATE PERCENTAGE                          
         BNE   AMTINV                                                           
         MVC   SCDATA2(4),WORK     MOVE RATE TO SCAN BLOCK                      
         MVC   WORK(3),SCDATA1     RETURN DATE                                  
         MVC   WORK+3(4),SCDATA2   AND RATE                                     
         B     OYES                                                             
         SPACE 1                                                                
INYRAS   DS    0H                  CONTRACT YEAR AS OF DATE                     
         BAS   RE,CKASOF           CHECK FOR '>MM/DD/YY' ON LHS                 
         BNE   ONO                 (RETURNS PWOS DATE IN WORK)                  
         CLI   SCDATA2,C'Y'        1ST CHAR OF RHS MUST BE 'Y'EAR               
         BNE   ONO                                                              
         GOTO1 YRVAL,DMCB,SCDATA2+1  VALIDATE YEAR                              
         BNE   FLDINV                                                           
         MVC   WORK+3(1),TGYREQU   RETURN YEAR EQUATE AFTER DATE                
         B     OYES                                                             
         SPACE 1                                                                
INHLDRA  DS    0H                  HOLDING FEE RATE AS OF DATE                  
         BAS   RE,CKASOF           CHECK FOR '>MM/DD/YY' ON LHS                 
         BNE   ONO                 (RETURNS PWOS DATE IN WORK)                  
         CLI   SCDATA2,C'H'        1ST CHAR OF RHS MUST BE 'H'LD                
         BNE   ONO                                                              
         CLI   TGMEEQU,RADIO                                                    
         BE    FLDINV              NOT VALID ON RADIO COMMERCIALS               
         ZIC   RF,SCLEN2                                                        
         LA    R1,SCDATA2-1(RF)                                                 
         CLI   0(R1),C'%'          CHECK FOR TRAILING PERCENT SIGN              
         BNE   ONO                                                              
         BCTR  RF,0                                                             
         MVC   SCDATA1(3),WORK     SAVE DATE IN SCAN BLOCK                      
         LA    R4,SCDATA2+1                                                     
         MVI   BYTE,2                                                           
         BAS   RE,PCTVAL                                                        
         BNE   AMTINV                                                           
         MVC   SCDATA2(4),WORK     MOVE RATE TO SCAN BLOCK                      
         MVC   WORK(3),SCDATA1     RETURN DATE                                  
         MVC   WORK+3(4),SCDATA2   RETURN RATE                                  
         B     OYES                                                             
         EJECT                                                                  
*              OPTION INPUT ROUTINES  (CONT'D)                                  
         SPACE 1                                                                
INHLDAS  DS    0H                  HOLDING FEE AMT AS OF DATE                   
         BAS   RE,CKASOF           CHECK FOR '>MM/DD/YY' ON LHS                 
         BNE   ONO                 (RETURNS PWOS DATE IN WORK)                  
         CLI   SCDATA2,C'H'        1ST CHAR OF RHS MUST BE 'H'LD                
         BNE   ONO                                                              
         MVC   SCDATA1(3),WORK     SAVE DATE IN SCAN BLOCK                      
         ZIC   RF,SCLEN2                                                        
         BCTR  RF,0                                                             
         MVI   BYTE,2                                                           
         LA    R4,SCDATA2+1                                                     
         BAS   RE,CASHVL                                                        
         BNE   AMTINV                                                           
         MVC   SCDATA2(4),WORK     MOVE AMT TO SCAN BLOCK                       
         MVC   WORK(3),SCDATA1     RETURN DATE                                  
         MVC   WORK+3(4),SCDATA2   RETURN AMT                                   
         B     OYES                                                             
         SPACE 2                                                                
CKASOF   NTR1  ,                   CHECK FOR LHS='>MM/DD/YY'                    
         CLI   SCDATA1,C'>'        CHECK FOR GREATER THAN SIGN                  
         BNE   ONO                                                              
         CLI   SCLEN2,0            INSURE THERE'S A RHS                         
         BE    ONO                                                              
         LA    R4,SCDATA1+1        R4=A(DATE)                                   
         MVI   BYTE,1              RETURN PWOS DATE                             
         BAS   RE,DATEVAL                                                       
         BNE   ONO                                                              
         L     RE,FULL             RE=L'DATE                                    
         LA    RE,1(RE)                                                         
         CLM   RE,1,SCLEN1         L'DATE+1 SHOULD = L'LHS                      
         BNE   ONO                                                              
         B     OYES                                                             
         EJECT                                                                  
*              GENERAL OPTION INPUT ROUTINES                                    
         SPACE 1                                                                
*                                  * VALIDATE DATE INPUT *                      
*                                  R4=A(DATA)  BYTE=OUTPUT FORMAT               
DATEVAL  NTR1                                                                   
         LR    R2,R4               SET R2=A(DATA)                               
         GOTO1 DTVAL,DMCB,(X'40',WORK)                                          
         BNE   ONO                                                              
         XC    FULL,FULL           RETURN L'DATE IN FULL                        
         MVC   FULL+3(1),ACTUAL    ACTUAL IS RETURNED BY DTVAL                  
         B     OYES                                                             
         SPACE 2                                                                
*                                  * VALIDATE PERCENT INPUT *                   
*                                  R4=A(DATA)  RF=L'DATA                        
PCTVAL   NTR1                                                                   
         LA    R1,0(RF,R4)         SET TO POINT TO LAST CHAR.                   
         BCTR  R1,0                                                             
         CLI   0(R1),C'%'          CHECK FOR TRAILING PERCENT SIGN              
         BNE   *+6                                                              
         BCTR  RF,0                IF SO, SUBTRACT ONE FROM L'INPUT             
         MVI   BYTE,2              2 DECIMAL PLACES                             
         BAS   RE,CASHVL           VALIDATE AMOUNT                              
         BE    OYES                                                             
         B     ONO                                                              
         SPACE 2                                                                
*                                  * VALIDATE AMOUNT INPUT *                    
*                                  R4=A(DATA)  RF=L'DATA  BYTE=N'DEC.           
CASHVL   NTR1                                                                   
         GOTO1 CASHVAL,DMCB,(BYTE,(R4)),(RF)                                    
         CLI   0(R1),X'FF'                                                      
         BE    ONO                                                              
         TM    4(R1),X'80'         NO NEGATIVE NUMBERS                          
         BO    ONO                                                              
         MVC   WORK(4),4(R1)       RETURN IN WORK                               
         B     OYES                                                             
         EJECT                                                                  
*              GENERAL OPTION INPUT ROUTINES  (CONT'D)                          
         SPACE 1                                                                
*                                  * VALIDATE NUMERIC INPUT *                   
*                                  R4=A(DATA)  RF=L'DATA                        
NUMVL    DS    0H                                                               
         MVC   WORK(9),=9X'F0'     INSURE NUMERIC INPUT                         
         SH    RF,=H'1'                                                         
         BM    NVLNO                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),0(R4)                                                    
         CLC   WORK(9),=9X'F0'                                                  
         BNE   NVLNO                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)                                                      
         CVB   RF,DUB                                                           
         LTR   RF,RF               DON'T ALLOW ZERO                             
         BZ    NVLNO                                                            
         CR    RE,RE                                                            
         BR    RE                  RETURN RF=BINARY VALUE                       
NVLNO    LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO ADD SUB-ELEMENTS TO SUB-EL BLOCK                      
         SPACE 1                                                                
         USING OPTD,R1             R1=A(OPTION TABLE ENTRY)                     
         USING SCAND,R3            R3=A(SCAN BLOCK ENTRY)                       
INSUBEL  NTR1                                                                   
         L     R4,ANXTSUB          A(NEXT OPEN SPOT)                            
         SPACE 1                                                                
         ZIC   RE,OPTSUBLN         RE=L'SUB-ELEMENT DATA                        
         LA    RF,1(RE,R4)         ADD 1 FOR CODE TO GET NEXT                   
         ST    RF,ANXTSUB          SAVE A(NEXT SPOT)                            
         SPACE 1                                                                
         S     RF,ASUBELS          SUBTRACT A(FIRST SPOT)                       
         CH    RF,=AL2(L'SUBELBLK)                                              
         BH    SUBELERR            BLOCK WILL BECOME TOO LARGE                  
         SPACE 1                                                                
         USING TAESSBCD,R4                                                      
         MVC   TAESSBCD,OPTCODE    SUB-ELEMENT CODE                             
         CLI   OPTCODE,OPUNITS     USE UNITS FOR LAST CLA USE                   
         BNE   INSUB5                                                           
         CLI   TGUSEQU,UITN        IF USE = ITN                                 
         BNE   INSUB5                                                           
         MVI   TAESSBCD,OPLCLAU                                                 
INSUB5   LTR   RE,RE                                                            
         BZ    INSUBX              NO SUB-ELEMENT DATA                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   TAESSBDT(0),WORK    SUB-ELEMENT DATA IS IN WORK                  
         SPACE 1                                                                
         CLI   OPTCODE,OPUSE       IF THIS IS USE TYPE OPTION                   
         BNE   INSUBX                                                           
         ST    R4,AUSESUB          THEN SAVE A(THIS SUB-ELEMENT)                
         MVC   AUSESUB(1),SCLEN1   AND L'INPUT IN HOB                           
         SPACE 1                                                                
INSUBX   OI    STATUS2,ADDESTEL    SET TO ADD THIS ELEMENT                      
         AP    SUBELCNT,=P'1'      ADD ONE TO COUNT                             
         B     OXIT                                                             
         DROP  R4                                                               
         EJECT                                                                  
*              OPTION PENDING CHECK ROUTINE                                     
         SPACE 1                                                                
         USING OPTD,R1             R1=A(OPTION TABLE ENTRY)                     
PENDCHK  CLI   OPTCODE,OPUSE       DON'T BOTHER IF THIS IS USE TYPE             
         BER   RE                                                               
PENDCHKU DS    0H                                                               
         TM    OPTPEND,OPTPMAJ     MAJORS                                       
         BO    PENDMAJ                                                          
         TM    OPTPEND,OPTPUNIT    UNITS                                        
         BO    PENDUNIT                                                         
         TM    OPTPEND,OPTPUSES    USES                                         
         BO    PENDUSES                                                         
         TM    OPTPEND,OPTPCYC     CYCLE DATES                                  
         BO    PENDCYC                                                          
         TM    OPTPEND,OPTPAMT     AMOUNT                                       
         BO    PENDAMT                                                          
         TM    OPTPEND,OPTPGRT     GUARANTEE NUMBER                             
         BO    PENDGRT                                                          
         TM    OPTPEND,OPTPINS     INSERTS                                      
         BO    PENDINS                                                          
         TM    OPTPEND,OPTPSTA     ADDENDUM STATE                               
         BO    PENDSTA                                                          
         TM    OPTPEND2,OPTPTAG    TAGS                                         
         BO    PENDTAG                                                          
         TM    OPTPEND2,OPTPDEMO   DEMOS                                        
         BO    PENDDEMO                                                         
         BR    RE                                                               
         SPACE 3                                                                
*              OPTION DUPLICATION CHECK ROUTINE                                 
         SPACE 1                                                                
         USING OPTD,R1             R1=A(OPTION TABLE ENTRY)                     
DUPCHK   DS    0H                                                               
         CLI   OPTCODE,OPUSE       DUPLICATION OK FOR USES THEMSELVES           
         BE    DUPC4                                                            
         TM    OPTSTAT,OPTFORUS    IF USE-RELATED                               
         BZ    DUPC1                                                            
         LA    RF,OPUSLIST         USE TABLE OF USE OPTIONS                     
         LA    R0,NUSOPTS                                                       
         B     DUPC2                                                            
DUPC1    LA    RF,OPTLIST          ELSE USE TABLE OF ALL OPTIONS                
         LA    R0,NOPTS                                                         
DUPC2    CLI   0(RF),0                                                          
         BER   RE                  RETURN RF=A(NEXT SLOT IN LIST)               
         CLC   OPTCODE,0(RF)                                                    
         BE    DUPERR              FOUND A MATCH - THAT'S AN ERROR              
         LA    RF,1(RF)                                                         
         BCT   R0,DUPC2                                                         
DUPC4    LTR   RE,RE               RETURN CC NE IF NOT SAVING THIS ONE          
         BR    RE                                                               
         EJECT                                                                  
*              ERROR/EXIT ROUTINES FOR OPTION VALIDATION ROUTINES               
         SPACE 1                                                                
         USING LINED,R2            R2=A(LINE)                                   
         USING SCAND,R3            R3=A(SCAN BLOCK ENTRY)                       
         SPACE 1                                                                
FLDINVR  MVC   ERRDISP,SCDISP2     SET DISP. INTO FLD OF RHS                    
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEENDO                                                          
DATINV   MVI   ERROR,INVDATE       INVALID DATE                                 
         B     THEENDO                                                          
ONTFOUND MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         B     THEENDO                                                          
AMTINV   MVI   ERROR,ERINVAMT      INVALID AMOUNT                               
         B     THEENDO                                                          
ENDINV   MVI   ERROR,ERINVEDT      END LESS THAN START                          
         B     THEENDO                                                          
DUPERR   MVI   ERROR,ERDUPFLD      DUPLICATE INPUT FIELD                        
         B     THEENDO                                                          
BADMEDIA MVI   ERROR,ERUSEMED      INVALID USE FOR MEDIA                        
         B     THEENDO                                                          
         SPACE 1                                                                
PENDUSES MVI   ERROR,ERMISUSE      MISSING NUMBER OF USES                       
         B     PENDALL                                                          
PENDMAJ  MVI   ERROR,ERMISMAJ      MISSING MAJORS (MARKETS)                     
         B     PENDALL                                                          
PENDUNIT MVI   ERROR,ERMISUNT      MISSING NUMBER OF UNITS                      
         B     PENDALL                                                          
PENDCYC  MVI   ERROR,ERMISCYC      MISSING PERIOD (CYCLE DATES)                 
         B     PENDALL                                                          
PENDAMT  MVI   ERROR,ERMISAMT      MISSING AMOUNT                               
         B     PENDALL                                                          
PENDGRT  MVI   ERROR,ERMISGRT      MISSING GUARANTEE CODE                       
         B     PENDALL                                                          
PENDINS  MVI   ERROR,ERMISINS      MISSING NUMBER OF INSERTS                    
         B     PENDALL                                                          
PENDSTA  MVI   ERROR,ERMISSTA      MISSING ADDENDUM STATE                       
         B     PENDALL                                                          
PENDTAG  MVC   MYMSGNO,=Y(ERMISTAG) MISSING NUMBER OF TAGS                      
         B     PENDALL                                                          
PENDDEMO MVC   MYMSGNO,=Y(ERMISDEM) MISSING NUMBER OF DEMOS                     
PENDALL  ZIC   R1,ERRDISP                                                       
         SH    R1,=H'1'            BACK UP TO LAST COMMA                        
         BM    *+8                                                              
         STC   R1,ERRDISP                                                       
         LH    R0,=AL2(LINLNQ)                                                  
         TM    DISPMODE,HYPOCOM                                                 
         BZ    *+8                                                              
         LH    R0,=AL2(L'HYPOPT2H+L'HYPOPT2+L'HYPOPT2X)                         
         TM    DISPMODE,HYPOPER                                                 
         BZ    *+8                                                              
         LH    R0,=AL2(LINHLNQ)                                                 
         CLI   5(R2),0             IF NOTHING INPUT ON RHS OF LINE              
         BNE   THEENDO                                                          
         SR    R2,R0               DROP BACK UNTIL WE FIND SOMETHING            
         B     *-10                                                             
         SPACE 1                                                                
SUBELERR MVI   ERROR,ERTOOMNY      TOO MANY OPTIONS SELECTED                    
         B     THEENDO                                                          
         SPACE 1                                                                
THEENDO  TM    STATUS2,BUILDING    IF WE'RE NOT BUILDING                        
         BZ    ONO                 THEN RETURN CC NOT EQUAL                     
         SPACE 1                                                                
         CLI   ERROR,0             IF NEW TWO BYTE ERROR MESSAGES               
         BNE   THEENDOX                                                         
         OI    GENSTAT2,USGETTXT                                                
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
THEENDOX GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
OYES     XR    RC,RC                                                            
ONO      LTR   RC,RC                                                            
OXIT     XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS, ETC. FOR OPTION VALIDATION ROUTINES                   
         SPACE 2                                                                
         LTORG                                                                  
         DROP  R8                  DROP SECONDARY BASE REGISTERS                
         EJECT                                                                  
*              ROUTINE TO DISPLAY ESTIMATE SUB-ELEMENTS                         
         SPACE 1                                                                
         USING TAESD,R4            R4=A(ESTIMATE ELEMENT)                       
OPUNSCAN DS    0D                                                               
         NMOD1 0,OPUNSCAN                                                       
         L     RC,SVRC                                                          
         L     R2,0(R1)            R2=A(SCREEN LINE)                            
         ST    R2,FULL             SAVE A(THIS LINE)                            
         MVC   PARAS,4(R1)         DISP. TO NEXT LINE, A(LAST LINE)             
         ZAP   HALF,=P'1'          TRACK N'LINES USED                           
         CLI   TAESSBNO,0          ANYTHING TO DISPLAY                          
         BE    UNSCYES                                                          
         ZIC   R1,TAESSBNO                                                      
         CVD   R1,DUB                                                           
         ZAP   SUBELCNT,DUB                                                     
         LA    R4,TAESSBCD         R4=A(1ST SUB-ELEMENT)                        
         USING TAESSBCD,R4                                                      
UNSC1    LR    R3,RA               R3=A(UNSCAN BLOCK)                           
         AH    R3,=AL2(TWAENDLQ)                                                
         XR    R0,R0               R0=N'ENTRIES IN UNSCAN BLOCK                 
         SPACE 1                                                                
UNSC2    MVC   0(RHSLNQ+LHSLNQ,R3),SPACES INITIALIZE UNSCAN BLOCK ENTRY         
         SPACE 1                                                                
         USING OPTD,R1                                                          
         L     R1,AOPTTAB          LOOK FOR ENTRY IN TABLE                      
UNSC3    CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                UNDEFINED SUB-ELEMENT                        
         CLC   TAESSBCD,OPTCODE                                                 
         BE    UNSC4                                                            
         TM    OPTSTAT,OPTNOLHS    BASED ON L'CURRENT ENTRY                     
         BZ    *+12                                                             
         LA    R1,OPTNEXT          BUMP TO NEXT                                 
         B     *+8                                                              
         LA    R1,OPTNEXT2                                                      
         B     UNSC3                                                            
         SPACE 1                                                                
UNSC4    XR    RF,RF                                                            
         ICM   RF,3,OPTODSP        RF=DISPLACEMENT TO O/P ROUTINE               
         BNZ   *+6                                                              
         DC    H'0'                MISSING O/P ROUTINE                          
         AR    RF,RB                                                            
         LA    RE,UNSC5            SET RE=A(RETURN)                             
         NTR1  ,                   COMMON NTR1                                  
         BR    RF                                                               
UNSC5    DS    0H                                                               
         ZIC   RE,TAESSBDT         ASSUME LENGTH IS IN FIRST DATA BYTE          
         CLI   TAESSBCD,OPCOMNM    IF THIS ISN'T COMMERCIAL NAME                
         BE    *+16                                                             
         LA    R3,RHSLNQ+LHSLNQ(R3) BUMP UNSCAN BLOCK ENTRY                     
         AH    R0,=H'1'            AND ADD 1 TO COUNT                           
         IC    RE,OPTSUBLN         L'SUB EL IS IN TABLE                         
         SPACE 1                                                                
         LA    R4,1(RE,R4)         BUMP TO NEXT SUB-ELEMENT                     
         SP    SUBELCNT,=P'1'                                                   
         BP    UNSC2               AND PROCESS                                  
         SPACE 1                                                                
         LTR   R0,R0               ANYTHING TO DO                               
         BZ    UNSCYES                                                          
         LR    R1,RA               R1=A(UNSCAN BLOCK)                           
         AH    R1,=AL2(TWAENDLQ)                                                
         ST    R1,DMCB             SET 1ST PARAMETER FOR UNSCAN                 
         STC   R0,DMCB                                                          
UNSC7    LA    RF,LHSLNQ           SET NON-STANDARD LHS LENGTH                  
         LA    R0,RHSLNQ           SET NON-STANDARD RHS LENGTH                  
         GOTO1 UNSCAN,DMCB,,((R0),(R2)),0,((RF),C' $LT') UNSCAN BLOCK           
         CLI   0(R1),0             ANYTHING LEFT                                
         BE    UNSC11                                                           
         ZIC   RF,PARAS            RF=DISP TO NEXT LINE                         
         LTR   RF,RF               IF NOT PASSED                                
         BNZ   *+8                                                              
         IC    RF,0(R2)            ASSUME IT'S NEXT FIELD                       
         AR    R2,RF               BUMP TO NEXT LINE                            
         AP    HALF,=P'1'                                                       
         L     RE,PARAS            RE=A(LAST LINE)                              
         SLL   RE,8                                                             
         SRL   RE,8                                                             
         CR    R2,RE               IF THERE'S ROOM, UNSCAN AGAIN                
         BNH   UNSC7                                                            
         CLI   PARAS,LINLNQ        IF WE'RE ON COMM'L OR PERF. LIST             
         BNE   UNSCNO                                                           
         L     R2,FULL             CLEAR ENTIRE ENTRY                           
         SH    R2,=AL2(LINRHSH-LINED)                                           
         USING LINED,R2                                                         
UNSC9    XC    LINDATA,LINDATA                                                  
         XC    LINRHS,LINRHS                                                    
         LA    R2,LINLNQ(R2)                                                    
         CR    R2,RE                                                            
         BL    UNSC9                                                            
         B     UNSCNO              AND RETURN CC NOT EQUAL                      
         SPACE 1                                                                
UNSC11   CP    SUBELCNT,=P'0'      HAVE WE PROCESSED ALL SUB-ELEMENTS           
         BE    UNSCYES                                                          
         LA    R2,LINLNQ(R2)       NO, BUMP TO NEXT LINE                        
         AP    HALF,=P'1'                                                       
         B     UNSC1               AND CONTINUE                                 
         SPACE 1                                                                
UNSCYES  ZAP   DUB,HALF            RETURN N'LINES USED                          
         CVB   R1,DUB                                                           
         CLM   R1,1,THSLINES       ONLY IF MORE THAN ALREADY SET                
         BNH   *+8                                                              
         STC   R1,THSLINES                                                      
         CR    R2,R2                                                            
         B     UXIT                                                             
UNSCNO   LTR   R2,R2                                                            
UXIT     XIT1                                                                   
UXITRF   XIT1  REGS=(RF)                                                        
         EJECT                                                                  
       ++INCLUDE TAESTOPTR                                                      
         EJECT                                                                  
*              CONSTANTS FOR OPTION UNSCAN ROUTINES                             
         SPACE 2                                                                
LHSLNQ   EQU   17                  NON-STANDARD L'LHS OF UNSCAN BLOCK           
RHSLNQ   EQU   13                  NON-STANDARD L'RHS OF SCAN & UNSCAN          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD/WRITE ESTIMATE RECORDS                            
         SPACE 1                                                                
*                                  IO2=NEW RECORD                               
         DS    0D                                                               
WRITEIT  NMOD1 0,WRITEIT                                                        
         L     RC,SVRC                                                          
         TM    STATUS2,BUILDING    IF NOT BUILDING RECORD                       
         BO    WRI2                                                             
         GOTO1 AGETEREC,DMCB,(RC)  JUST GET THE RECORD AGAIN                    
         B     WXIT                AND RETURN                                   
         SPACE 1                                                                
WRI2     MVI   RDUPDATE,C'Y'         SET TO READ FOR UPDATE                     
         NI    GENSTAT1,ALL-RDUPAPPL THIS WILL KEEP IT ON UNTIL END             
         OI    DMINBTS,X'88'         ALSO SET TO READ DELETED                   
         SPACE 1                                                                
         GOTO1 AGETEREC,DMCB,(RC)  LOOK FOR EXISTING RECORD                     
         NI    DMINBTS,X'F7'                                                    
         MVC   AIO,AIO2            SET AIO TO NEW RECORD                        
         SPACE 1                                                                
         TM    STATUS2,HAVEREC     IF RECORD WAS FOUND                          
         BZ    WRI4                                                             
         LA    R3,KEY                                                           
         USING TLDRD,R3                                                         
         TM    TLDRSTAT,X'80'      AND IT'S NOT MARKED DELETED                  
         BZ    WRI6                GO MERGE/SPLIT                               
         SPACE 1                                                                
WRI4     L     R3,AIO              R3=A(NEW RECORD)                             
         GOTO1 ACTVOUT,DMCB,ESTLCHGH  DISPLAY NEW LAST CHANGED INFO             
         BAS   RE,RELEASE          WRITE IT BACK NOW                            
         GOTO1 AGETEREC,DMCB,(RC)  READ IT BACK INTO TIA                        
         B     WRIX                ALL DONE                                     
         SPACE 1                                                                
WRI6     MVC   AIO,ATIA            *** FROM THIS POINT ON, AIO IS ATIA          
*                                  *** UNLESS EXPLICITLY CHANGED                
         SPACE 1                                                                
         BAS   RE,TRIM             TRIM ELEMENTS FROM EXISTING RECORD           
         BAS   RE,MERGE            MERGE ELS. IN IO2 (NEW REC.) INTO            
*                                  THOSE IN TIA (EXISTING REC.)                 
         SPACE 1                                                                
         BAS   RE,SPLIT            SPLIT UP RECORD AND WRITE BACK               
         SPACE 1                                                                
WRIX     MVC   AIO,AIO1            RESTORE DEFAULT I/O AREA                     
         MVI   RDUPDATE,C'N'       TURN OFF READ FOR UPDATE                     
         OI    GENSTAT1,RDUPAPPL   RESET GENCON CONTROL SWITCH                  
WXIT     XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TRIMS EXISTING RECORD IN PREPARATION FOR MERGE           
         SPACE 1                                                                
TRIM     NTR1                                                                   
         TM    STATUS2,WRITETOP    DON'T BOTHER IF WRITING ONLY TOP             
         BO    TRMX                                                             
         TM    DISPMODE,MISCADD    SKIP IF HAVE MISC. ADDITION SCREEN           
         BO    TRMX                                                             
         TM    DISPMODE,HYPOPER    FOR ADDITIONAL PERFORMERS                    
         BNO   TRM2                                                             
         XC    ELDATA,ELDATA                                                    
         BAS   RE,WLOOKUP          LOOK FOR FIRST ELEMENT OF THIS TYPE          
         L     R4,FULL                                                          
         USING TAESD,R4                                                         
         CLC   TAESTYPE,ELTYPE     WE WANT TO DELETE STARTING HERE              
         BE    TRM6                                                             
         B     TRMX                NOTHING TO DELETE                            
         SPACE 1                                                                
TRM2     TM    DISPMODE,HYPOCOM    FOR HYPOTHETICAL COMMERCIALS                 
         BNO   TRM3                                                             
         MVC   ELDATA,HYCODATA     USE SAVED EL. DATA                           
         BAS   RE,WLOOKUP          LOOK FOR IT ON RECORD                        
         BE    TRM6                                                             
         B     TRMX                NOT FOUND - SKIP IT                          
         SPACE 1                                                                
TRM3     CLI   PFAID,PFDELPRD      DELETE CLI/PRD REQUESTED                     
         BNE   TRM3D                                                            
         L     R3,ATIA             R3=A(COMBINED ESTIMATE RECORD)               
         MVI   ELTYPE,TAESTCLI     SET TYPE                                     
         XC    ELDATA,ELDATA                                                    
         MVC   ELDATA(6),TGCLI     AND DATA FOR WLOOKUP                         
         OC    TGPRD,TGPRD         IF PRODUCT DEFINED                           
         BZ    *+14                                                             
         MVI   ELTYPE,TAESTPRD     SET TYPE                                     
         MVC   ELDATA(6),TGPRD     AND DATA FOR WLOOKUP                         
         BAS   RE,WLOOKUP                                                       
         BNE   TRMX                                                             
         B     TRM6                GO DELETE THE ELEMENTS                       
         SPACE 1                                                                
TRM3D    LA    R3,SCRNTAB          R3=A(TABLE OF SCREEN KEYS)                   
         LA    R0,NLINEQU                                                       
         USING SCRND,R3                                                         
TRM4     CLI   SCRNSTAT,SCRNALOK   LOOKING FOR DELETE STATUS CODES ONLY         
         BL    *+16                                                             
TRM5     LA    R3,SCRNNEXT         CHECK NEXT TABLE ENTRY                       
         BCT   R0,TRM4                                                          
         B     TRMX                                                             
         SPACE 1                                                                
         MVC   ELDATA,SCRNDATA     SET ELEMENT KEY DATA                         
         BAS   RE,WLOOKUP          LOOK FOR IT ON RECORD                        
         BNE   TRM5                NOT FOUND - SKIP IT                          
         SPACE 1                                                                
TRM6     L     R4,FULL             R4=A(ELEMENT WE FOUND FROM WLOOKUP)          
         XR    R2,R2               R2=N'ELEMENTS DELETED                        
         MVC   BYTE,ELTYPE                                                      
         NI    BYTE,X'0F'          BYTE=LOBS OF 1ST EL. TO DELETE               
         SPACE 1                                                                
         CLI   PFAID,PFDELPRD      IF DELETE CLI/PRD REQUESTED                  
         BE    TRM8                   GO DELETE                                 
         TM    DISPMODE,HYPOPER    OR IF ADDITIONAL PERFORMER SCREEN            
         BO    TRM9B                  NEED TO CHECK PAGE NUMBER                 
         TM    DISPMODE,HYPOCOM    HYPO COMMERCIAL SCREEN                       
         BZ    *+16                                                             
         CLI   PFAID,PFDELHYP      IF DELETING THIS COMMERCIAL                  
         BE    TRM8                   GO DELETE                                 
         B     TRM9                ELSE SKIP TO NEXT- WILL MERGE LATER          
         TM    SCRNSTAT,SCRNOK     DON'T DELETE 1ST IF IT'S OK                  
         BO    TRM8D                                                            
TRM8     MVI   0(R4),X'FF'         SET TO DELETE THE ELEMENT                    
         AH    R2,=H'1'                                                         
         CH    R2,=H'1'            IF THIS IS THE FIRST DELETED ELEMENT         
         BH    TRM9                                                             
         ST    R4,FULL             SAVE ITS ADDRESS                             
TRM8D    MVC   FULL(1),TAESSEQ     AND IT'S SEQUENCE NUMBER                     
         SPACE 1                                                                
TRM9     BAS   RE,NEXTEL2          GET NEXT ELEMENT                             
         BNE   TRM10                                                            
         MVC   TGBYTE,TAESTYPE                                                  
         NI    TGBYTE,X'0F'        TGBYTE=LOBS OF THIS ELEMENT                  
         CLC   TGBYTE,BYTE         CANDIDATE FOR DELETE UNTIL LOBS              
         BL    TRM10               ARE LESS FOR NEW ELEMENT                     
         BH    TRM9D                                                            
         TM    DISPMODE,HYPOPER    EQUAL LOBS - IF ADDL. PERFORMERS             
         BZ    TRM10                                                            
         CLI   TAESTYPE,TAESTHPE   AND THIS ISN'T HYPO PERFORMER EL             
         BNE   TRM10                    THEN WE'RE FINISHED                     
         SPACE 1                                                                
TRM9B    CLC   TAESHPG,HYPRFPG     MUST MATCH PAGE NUMBER                       
         BE    TRM8                                                             
         B     TRM9                ELSE KEEP ON LOOKING                         
         SPACE 1                                                                
TRM9D    TM    DISPMODE,HYPOCOM    HYPO COMM'L SCREEN                           
         BZ    *+16                                                             
         CLI   PFAID,PFDELHYP      IF DELETING THIS COMMERCIAL                  
         BE    TRM8                GO DELETE                                    
         B     TRM9B               ELSE NEED TO TEST PAGE NUMBER                
         SPACE 1                                                                
         CLI   PFAID,PFDELPRD      DELETE CLI/PRD REQUESTED                     
         BE    TRM8                                                             
         CLI   TAESTYPE,TAESTPER   IF THIS IS A PERFORMER ELEMENT               
         BNE   *+16                                                             
         TM    SCRNSTAT,SCRNOKPR   IS IT OK TO NOT DELETE THEM                  
         BO    TRM9                YES, BUT KEEP ON TRYING                      
         B     TRM8                NO - DELETE IT                               
         SPACE 1                                                                
         CLI   TAESTYPE,TAESTHPE   IF THIS IS A HYPO. PERFORMER ELEMENT         
         BNE   TRM8                                                             
         TM    SCRNSTAT,SCRNOKHP   IS IT OK TO NOT DELETE THEM                  
         BO    TRM9                YES, BUT KEEP ON TRYING                      
         B     TRM8                NO - DELETE IT                               
         SPACE 1                                                                
TRM10    CLI   PFAID,PFDELPRD      IF DELETING CLI/PRD                          
         BNE   TRM11                                                            
         OC    TGPRD,TGPRD         AND IF DELETING PRODUCT                      
         BZ    TRM11                                                            
         CLI   TAESEL,TAESELQ      THEN IF NO MORE ESTIMATE ELS.                
         BNE   *+12                                                             
         CLI   TAESTYPE,TAESTPRD   OR IF NEXT HIGH LVL ISNT ANOTHER PRD         
         BE    TRM11                                                            
         OI    STATUS3,CLIDELD     SET DELETING CLIENT AS WELL                  
         L     R3,ATIA             FIND CLIENT ELEMENT                          
         MVI   ELTYPE,TAESTCLI     SET TYPE                                     
         XC    ELDATA,ELDATA                                                    
         MVC   ELDATA(6),TGCLI     AND DATA FOR WLOOKUP                         
         BAS   RE,WLOOKUP                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,FULL             RETURNS FULL=A(ELEMENT)                      
         MVI   0(R4),X'FF'         SET TO DELETE                                
         MVC   FULL(1),TAESSEQ     SAVE SEQUENCE NO. OF 1ST EL DELETED          
         SPACE 1                                                                
TRM11    LTR   R2,R2               IF WE MARKED ANY ELS FOR DELETION            
         BZ    TRM14               DELETE ALL MARKED ELEMENTS                   
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
*        GOTO1 HELLO,DMCB,(C'D',=CL8'TALBIG'),(X'FF',AIO),0                     
         SPACE 1                                                                
         L     R4,FULL             RESTORE A(1ST DELETED ELEMENT)               
         CLI   0(R4),TAESELQ       IF WE'RE NOW AT END-OF-RECORD                
         BNE   TRMX                THEN STOP                                    
         MVI   ELCODE,TAESELQ                                                   
         ZIC   R1,FULL             RE-SEQUENCE REMAINING ELEMENTS               
TRM12    STC   R1,TAESSEQ          BASED ON ORIGINAL SEQUENCE NUMBER            
         AH    R1,=H'1'                                                         
         BAS   RE,NEXTEL2          LOOK FOR ANOTHER ELEMENT                     
         BE    TRM12                                                            
         SPACE 1                                                                
TRM14    CLI   PFAID,PFDELPRD      DONE IF DELETE CLI/PRD REQUESTED             
         BE    TRMX                                                             
         TM    ELTYPE,TAESTHYP     DONE IF WE'RE PROCESSING HYPO. ELS.          
         BZ    TRM5                                                             
         SPACE 1                                                                
TRMX     B     WXIT                                                             
         EJECT                                                                  
*              ROUTINE MERGES ELS IN NEW REC INTO THOSE IN EXISTING REC         
         SPACE 1                                                                
MERGE    NTR1                                                                   
         CLI   PFAID,PFDELPRD      SKIP IF DELETE CLI/PRD REQUESTED             
         BE    MER55                                                            
         NI    STATUS2,ALL-HAVESC40  CLEAR STATUS BIT                           
         L     R4,ATIA               R4=A(COMBINED ESTIMATE RECORD)             
         MVI   ELCODE,TAACELQ        GET ACTIVITY EL                            
         BAS   RE,GETEL2                                                        
         B     *+8                                                              
MER5     BAS   RE,NEXTEL2                                                       
         BNE   MER8                                                             
         USING TAACD,R4                                                         
         CLI   TAACSCR,SCR40       TEST IF SCREEN IS X'40' (EST REPORT)         
         BNE   MER5                IF NOT, GET NEXT EL                          
         OI    STATUS2,HAVESC40    ELSE SET STATUS BIT                          
         SPACE                                                                  
         USING TAESD,R4                                                         
MER8     L     R3,ATIA             R3=A(COMBINED ESTIMATE RECORD)               
         L     R4,AIO2             R4=A(NEW RECORD)                             
         MVI   ELCODE,TAESELQ      GET FIRST ESTIMATE EL. FROM NEW REC.         
         LA    R1,1                                                             
         BAS   RE,GETEL2           RETURNS R4=A(CLIENT EST. EL.)                
         B     *+12                                                             
MER10    MVI   ELCODE,TAESELQ      GET FIRST ESTIMATE EL. FROM NEW REC.         
         BAS   RE,NEXTEL2          GET NEXT NEW ESTIMATE EL.                    
         BNE   MER50                                                            
         BCT   R1,MER10            SKIP N'ELEMENTS INSERTED                     
         SPACE 1                                                                
         MVC   ELTYPE,TAESTYPE     SET TYPE                                     
         MVC   ELDATA,TAESDATA     AND DATA FOR WLOOKUP                         
         OI    STATUS3,LOOKCID     SET TO LOOK UP VIA CID                       
         CLI   ELTYPE,TAESTHPE     IF HYPO PERFORMER ELEMENT                    
         BNE   MER15                                                            
         CLI   NEWEST,C'Y'         AND NEW ESTIMATE DISPLAY                     
         BNE   MER15                                                            
         TM    DISPMD2,SELCTHPF    AND CHANGING SELECTED HYPO PERFORMER         
         BZ    MER13                                                            
         MVC   ELDATA,HYPFDATA     SEARCH WITH OLD EL. DATA                     
         XC    HYPFDATA,HYPFDATA                                                
         B     MER15                                                            
MER13    TM    DISPMODE,HYPO       AND IF SELECTABLE HYP PERF LIST              
         BZ    MER15                                                            
         OI    STATUS3,LOOKHIN     SET LOOK FOR LAST INSERTION POINT            
MER15    BAS   RE,WLOOKUP                                                       
         L     R3,FULL             RETURNS R3=A(MATCH IF CC EQUAL)              
         BNE   MER30                       R3=A(INSERT POINT IF CC NE)          
         SPACE 1                                                                
         MVC   TAESSEQ,TAESSEQ-TAESD(R3) USE SEQUENCE NO. OF FOUND EL.          
         MVI   0(R3),X'FF'         DELETE THE ONE WE FOUND                      
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
*        GOTO1 HELLO,DMCB,(C'D',=CL8'TALBIG'),(X'FF',AIO),0                     
         SPACE 1                                                                
         CLI   ELTYPE,TAESTHCO     IF THIS IS HYPO COMML                        
         BE    MER20               INSERT ALL REMAINING ELS.                    
         SPACE 1                                                                
         BAS   RE,MYADDL           ELSE ADD THIS ONE                            
         LA    R1,1                SET WE INSERTED 1                            
         B     MER40                                                            
         SPACE 1                                                                
MER20    ZIC   R1,LSTELSEQ         HYPO COMMERCIALS NEED TO SUBTRACT            
         BCTR  R1,0                ONE BECAUSE DELETED HYPO COMML EL.           
         STC   R1,LSTELSEQ                                                      
         SPACE 1                                                                
MER30    ZIC   R1,ELSEQ            SET TO INSERT ALL                            
         CLI   NEWEST,C'Y'         IF NEW ESTIMATE DISPLAY                      
         BNE   MER35                                                            
         TM    DISPMODE,HYPO       AND DEALING WITH HYPO'S                      
         BZ    MER35                                                            
         TM    ELTYPE,TAESTHYP     AND IF HYPO COMMERCIAL                       
         BO    MER38                                                            
*                                                                               
MER35    TM    ELTYPE,TAESTCLI+TAESTPRD+TAESTHYP OR NOT CLI,PRD,HYPO            
         BNZ   *+8                                                              
MER38    LA    R1,1                INSERT ONE AT A TIME                         
         BAS   RE,INSERT           INSERT NEW ELEMENT(S)                        
         SPACE 1                                                                
MER40    ZIC   RE,ELSEQ            RE=N'ELEMENTS REMAINING                      
         SR    RE,R1               SUBTRACT N'INSERTED FROM N'REMAINING         
         STC   RE,ELSEQ                                                         
         BP    MER10               IF THERE ARE ANY LEFT, LOOK FOR NEXT         
         SPACE 1                                                                
MER50    TM    DISPMD2,SELCTHPF    IF SELECTED HYPO PERFORMER MODE              
         BZ    MER55                                                            
         OC    HYPFDATA,HYPFDATA   AND STILL HAVE OLD HYPO PERF DATA            
         BZ    MER55                                                            
         MVI   ELTYPE,TAESTHPE     DELETE IT FROM RECORD                        
         MVC   ELDATA,HYPFDATA                                                  
         OI    STATUS3,LOOKCID                                                  
         BAS   RE,WLOOKUP                                                       
         BNE   MER55                                                            
         L     R4,FULL                                                          
         MVC   FULL(1),TAESSEQ     SAVE SEQUENCE NO. OF EL DELETED              
         MVI   0(R4),X'FF'                                                      
         MVI   ELCODE,X'FF'                                                     
         GOTO1 REMELEM                                                          
*        GOTO1 HELLO,DMCB,(C'D',=CL8'TALBIG'),(X'FF',AIO),0                     
         XC    HYPFDATA,HYPFDATA                                                
         CLI   0(R4),TAESELQ       IF WE'RE NOT AT END OF EST. ELS              
         BNE   MER55                                                            
         MVI   ELCODE,TAESELQ      RESET ELEMENT CODE                           
         ZIC   R1,FULL             RESEQUENCE REMAINING ELEMENTS                
MER52    STC   R1,TAESSEQ          BASED ON SEQ NUMBER DELETED                  
         AH    R1,=H'1'                                                         
         BAS   RE,NEXTEL2                                                       
         BE    MER52                                                            
         SPACE 1                                                                
MER55    TM    STATUS3,ADDACTV     IF NEED TO REPLACE ACTIVITY ELEMENT          
         BZ    MER60                                                            
         GOTO1 ACTVIN,DMCB,(X'40',0)  REMOVE EXISTING ACTV EL - BLD NEW         
         LA    R4,ELEMENT                                                       
         BAS   RE,MYADDL           ADD NEW ACTIVITY EL. TO TARGET REC.          
         GOTO1 ACTVOUT,DMCB,ESTLCHGH  DISPLAY NEW LAST CHANGED INFO             
         SPACE 1                                                                
MER60    L     R4,AIO2             R4=A('SOURCE' RECORD)                        
         MVI   ELCODE,TANUELQ      REPLACE REVISION NUMBER                      
         BAS   RE,REPLACE                                                       
         MVI   ELCODE,TANAELQ              ESTIMATE NAME                        
         BAS   RE,REPLACE                                                       
         MVI   ELCODE,TACMELQ              NARRATIVE                            
         BAS   RE,REPLACE                                                       
         SPACE 1                                                                
         TM    DISPMODE,MISCADD    IF MISC. ADDITION SCREEN LOADED              
         BZ    MERX                                                             
         MVI   ELCODE,TAEMELQ              ESTIMATE MISC. ADDITION ELS.         
         BAS   RE,REPLACE                                                       
         MVI   ELCODE,TAENELQ              ESTIMATE NARRATIVE ELS.              
         BAS   RE,REPLACE                                                       
         SPACE 1                                                                
MERX     B     WXIT                                                             
         EJECT                                                                  
*              ROUTINE TO REPLACE AN ELEMENT                                    
         SPACE 1                                                                
*                                  AIO=ATIA ('TARGET' RECORD)                   
*                                  R4=A('SOURCE' RECORD)                        
*                                  ELCODE=CODE OF ELEMENT TO REPLACE            
REPLACE  NTR1                                                                   
         GOTO1 REMELEM                                                          
*        ZIC   R2,ELCODE                                                        
*        GOTO1 HELLO,DMCB,(C'D',=CL8'TALBIG'),((R2),AIO),0                      
         BAS   RE,GETEL2           IF ELEMENT IS ON SOURCE RECORD               
         BNE   REPX                                                             
         BAS   RE,MYADDL           ADD IT TO TARGET RECORD                      
         BAS   RE,NEXTEL2          LOOK FOR MORE                                
         BE    *-8                                                              
REPX     B     WXIT                                                             
         SPACE 3                                                                
*              ROUTINE TO ADD ELEMENTS FOR 'BIG' RECORDS                        
         SPACE 1                                                                
*                                  R4=A(ELEMENT)                                
MYADDL   NTR1                                                                   
         L     R3,ATIA                                                          
         GOTO1 HELLO,DMCB,(C'P',SYSFIL),(R3),(R4),0                             
         USING TLESD,R3                                                         
         LH    RE,=H'5700'         CUMULATIVE MAX RECORD SIZE                   
         TM    STATUS2,HAVESC40    IF HAVE EST REPRT ACTV EL                    
         BZ    *+8                                                              
         AH    RE,=AL2(TAACLNQ)    INCREASE MAX SIZE BY TAACLNQ                 
         CH    RE,TLESLEN                                                       
         BL    RCTOOLNG                                                         
         B     WXIT                                                             
         EJECT                                                                  
*              ROUTINE TO INSERT ESTIMATE ELEMENTS                              
         SPACE 1                                                                
*                                  R3=A(WHERE TO INSERT TO)                     
         USING TAESD,R4            R4=A(WHERE TO INSERT FROM)                   
INSERT   NTR1                                                                   
         ST    R4,FULL             SAVE A(SOURCE)                               
         LR    R2,R1               R2=N'ELEMENTS TO INSERT                      
         SPACE 1                                                                
         LR    R0,R2               R0=N'TO RESEQUENCE REMAINING ELS.            
         CLI   ELCODE,X'FF'        IF JUST DELETED ELEMENT                      
         BNE   *+12                                                             
         SH    R0,=H'1'            NEED TO RESEQUENCE BY ONE LESS               
         MVI   ELCODE,TAESELQ      RESET ELEMENT CODE                           
         SPACE 1                                                                
         CLI   0(R3),TAESELQ       AT END OF RECORD ALREADY                     
         BNE   INSER4                                                           
         LR    R4,R3               RESET SEQUENCE NOS. OF REMAINING ELS         
         XR    R1,R1               ON TARGET RECORD                             
         SPACE 1                                                                
INSER2   IC    R1,TAESSEQ          OLD SEQUENCE NUMBER                          
         AR    R1,R0               + RESEQUENCE NUMBER                          
         CH    R1,=H'256'          INSURE DON'T EXCEED MAX SEQUENCE NO.         
         BNL   MAXCOM                                                           
         STC   R1,TAESSEQ          = NEW SEQUENCE NUMBER                        
         BAS   RE,NEXTEL2          LOOK FOR ANOTHER ELEMENT                     
         BE    INSER2                                                           
         SPACE 1                                                                
INSER4   DS    0H                  NOW RESET SEQUENCE NOS. OF SOURCE            
         L     R4,FULL             R4=A(SOURCE)                                 
         LR    R0,R2               R0=N'ELEMENTS TO INSERT                      
         ZIC   R1,LSTELSEQ         BASED ON LAST ONE WE SAW                     
         CLI   LSTELSEQ,X'FF'      IF LAST SEQUENCE NUMBER IS X'FF'             
         BNE   INSER6                                                           
         XR    R1,R1               THEN ASSUME INSERTING AT BEGINNING           
         B     *+16                                                             
INSER6   AH    R1,=H'1'            PLUS ONE                                     
         CH    R1,=H'256'          INSURE DON'T EXCEED MAX SEQUENCE NO.         
         BNL   MAXCOM                                                           
         STC   R1,LSTELSEQ                                                      
         STC   R1,TAESSEQ          UPDATE NEW SEQUENCE NUMBER                   
         BCT   R0,*+8                                                           
         B     INSER8                                                           
         BAS   RE,NEXTEL2                                                       
         BE    INSER6                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
INSER8   L     R4,FULL             R4=A(SOURCE)                                 
         L     R3,ATIA                                                          
INSER10  BAS   RE,MYADDL           NOW ADD THE ELEMENTS                         
         BCT   R2,*+8                                                           
         B     WXIT                                                             
         BAS   RE,NEXTEL2                                                       
         BE    INSER10                                                          
         DC    H'0'                                                             
         EJECT                                                                  
*              ROUTINE SPLITS 'BIG' RECORDS INTO SMALLER ONES                   
         SPACE 1                                                                
SPLIT    NTR1                                                                   
         L     R4,ATIA             R4=A(NEW COMBINED/MERGED RECORD)             
         L     R3,AIO2                                                          
         ST    R3,AIO              R3=A(RECORD TO BE WRITTEN BACK)              
         USING TLESD,R3                                                         
         MVC   TLESKEY,0(R4)       SET KEY IN RECORD                            
         SPACE 1                                                                
         LA    R4,TLESELEM-TLESD(R4)  R4=A(FIRST ELEMENT)                       
         MVI   ELCODE,0            SET TO LOOP THROUGH ALL ELEMENTS             
         SPACE 1                                                                
SPL2     MVC   TLESLEN,DATADISP    INIT. RECORD LENGTH                          
         XC    TLESSTAT(10),TLESSTAT     STATUS, BEG. OF REC.                   
         SPACE 1                                                                
SPL4     ZIC   R1,1(R4)            R1 = L'ELEMENT                               
         AH    R1,TLESLEN             + L'RECORD                                
         CH    R1,=H'1930'         WILL RECORD BECOME TOO LONG                  
         BH    SPL6                YES - GO RELEASE IT                          
         MVC   ELEMENT,0(R4)       NO - MOVE ELEMENT TO W/S                     
         GOTO1 ADDELEM,DMCB,,,,0   AND ADD TO NEW REC.                          
         BAS   RE,NEXTEL2          GET NEXT ELEMENT                             
         BE    SPL4                                                             
         SPACE 1                                                                
SPL6     BAS   RE,RELEASE          WRITE BACK RECORD IN IO2                     
         SPACE 1                                                                
         CLI   0(R4),0             IF THERE ARE MORE ELS. TO PROCESS            
         BNE   SPL2                GO BACK                                      
         SPACE 1                                                                
         GOTO1 ADELETE             DELETE ANY REMAINING RECORDS                 
         B     WXIT                                                             
         EJECT                                                                  
*              ROUTINE RELEASES SPLIT-UP ESTIMATE RECORD                        
         SPACE 1                                                                
         USING TLESD,R3            R3=A(RECORD TO BE WRITTEN BACK)              
RELEASE  NTR1                                                                   
         MVC   KEY,TLESKEY         MOVE KEY FROM RECORD TO KEY                  
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         GOTO1 HIGH                SEE IF RECORD ALREADY ON FILE                
         SPACE 1                                                                
         LA    R3,KEY              R3=A(DIRECTORY RECORD)                       
         CLC   TLESKEY,KEYSAVE     IS RECORD ALREADY ON FILE                    
         BE    REL8                                                             
         MVC   TLESKEY,KEYSAVE     NO, SO RESTORE SAVED KEY                     
         GOTO1 ADDREC              AND ADD NEW RECORD TO FILE                   
         B     RELX                                                             
         SPACE 1                                                                
         USING TLDRD,R3                                                         
REL8     TM    TLDRSTAT,X'80'      IF DIRECTORY MARKED DELETED                  
         BZ    REL10                                                            
         XI    TLDRSTAT,X'80'      UNMARK IT                                    
         GOTO1 WRITE               AND WRITE IT BACK                            
         SPACE 1                                                                
REL10    MVC   AIO,AIO1            RECORD EXISTS - READ IT INTO IO1             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO2            RESET AIO TO A(NEW RECORD)                   
         GOTO1 PUTREC              WRITE BACK NEW FILE RECORD                   
         SPACE 1                                                                
RELX     NI    DMINBTS,X'F7'       TURN OFF READ FOR DELETED                    
         SPACE 1                                                                
         L     R3,AIO              R3=A(RECORD WE JUST ADDED/WROTE)             
         USING TLESD,R3                                                         
         ZIC   R1,TLESSEQ          BUMP SEQUENCE NUMBER IN KEY OF REC.          
         LA    R1,2(R1)                                                         
         STC   R1,TLESSEQ                                                       
         B     WXIT                                                             
         EJECT                                                                  
*              MISC. STUFF                                                      
         SPACE 2                                                                
WLOOKUP  NTR1                                                                   
         GOTO1 ALOOKUP,DMCB,0                                                   
         B     WXIT                                                             
         SPACE 2                                                                
MAXCOM   MVI   ERROR,ERMAXCOM      REACHED MAX N'COMMLS FOR CLI/PRD             
         B     *+8                                                              
RCTOOLNG MVI   ERROR,TOOLONG       RECORD TOO LONG                              
         LH    R2,CURDISP          RETURN CURSOR TO SAME SPOT                   
         AR    R2,RA                                                            
         GOTO1 EXIT,DMCB,0         ALL EXITS                                    
         SPACE 2                                                                
         GETEL2 (R4),DATADISP,ELCODE                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE DELETES SUBSEQUENT ESTIMATE RECORDS                      
         SPACE 1                                                                
DELETE   DS    0D                                                               
         NMOD1 0,**DELETE                                                       
         L     RC,SVRC                                                          
         SPACE 1                                                                
         L     R3,AIO              R3=A(FILE RECORD)                            
         USING TLESD,R3                                                         
DEL2     GOTO1 HIGH                RE-READ RECORD WE JUST WROTE                 
         NI    DMINBTS,X'F7'       TURN OFF READ DELETED                        
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 SEQ                 GET NEXT                                     
         SPACE 1                                                                
         CLC   KEY(TLESSEQ-TLESD),KEYSAVE  TEST STILL SAME ESTIMATE             
         BNE   DELX                                                             
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC              GET THE RECORD                               
         OI    TLESSTAT,X'80'      MARK IT DELETED                              
         GOTO1 PUTREC              AND WRITE IT BACK                            
         SPACE 1                                                                
         OI    KEY+TLDRSTAT-TLDRD,X'80'  MARK DIRECTORY DELETED                 
         GOTO1 WRITE                     AND WRITE IT BACK                      
         OI    DMINBTS,X'08'       SET TO READ DELETED                          
         B     DEL2                LOOK FOR MORE                                
         SPACE 1                                                                
DELX     XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINES TO LOOK UP ESTIMATE ELEMENTS                            
         SPACE 1                                                                
         DS    0D                                                               
LOOK     NMOD1 0,LOOKUP            ELDATA=KEY DATA OF ELEMENT TO FIND           
         L     RC,SVRC                                                          
         CLI   3(R1),2             SEARCH ONLY                                  
         BNE   *+12                                                             
         BAS   RE,SEARCH                                                        
         B     LXITR3                                                           
         SPACE 1                                                                
         CLI   3(R1),1                                                          
         BE    LOOK2                                                            
         TM    STATUS2,HAVEREC     INSURE WE HAVE RECORD                        
         BZ    LOOKNO                                                           
         L     R3,ATIA                                                          
         B     LOOK3                                                            
         SPACE 1                                                                
LOOK2    L     R3,AIO2                                                          
         SPACE 1                                                                
LOOK3    ST    R3,DMCB             DMCB HAS A(RECORD)                           
         MVC   WORK(L'ELDATA),ELDATA   SAVE ELEMENT DATA                        
         MVC   WORK+L'ELDATA(1),ELTYPE  AND ELEMENT TYPE                        
         XC    ELDATA,ELDATA                                                    
         MVI   LSTELSEQ,X'FF'      INIT. LAST EL SEQUENCE NUMBER SEEN           
         SPACE 1                                                                
         CLI   WORK+L'ELDATA,TAESTCLI SKIP IF WE'RE LOOKING UP CLIENT           
         BE    LOOK10                                                           
         MVI   ELTYPE,TAESTCLI     FIND CLIENT ELEMENT                          
         MVC   ELDATA(6),TGCLI                                                  
         BAS   RE,SEARCH                                                        
         BNE   LOOKNO                                                           
         SPACE 1                                                                
         CLI   WORK+L'ELDATA,TAESTPRD SKIP IF WE'RE LOOKING UP PRODUCT          
         BE    LOOK10                                                           
         OC    TGPRD,TGPRD         DON'T BOTHER WITH IF NONE DEFINED            
         BZ    LOOK4                                                            
         MVC   ELDATA(6),TGPRD                                                  
         MVI   ELTYPE,TAESTPRD     FIND PRODUCT ELEMENT                         
         BAS   RE,SEARCH                                                        
         BNE   LOOKNO                                                           
         SPACE 1                                                                
LOOK4    TM    WORK+L'ELDATA,TAESTCOM SKIP IF WE'RE LOOKING UP COMML            
         BO    LOOK10                                                           
         MVC   ELDATA(8),TGCID                                                  
         MVC   ELDATA+8(4),TGCOM                                                
         MVI   ELTYPE,TAESTCOM     FIND COMMERCIAL ELEMENT                      
         TM    DISPMODE,HYPOCOM    IF HYPO. COMMERCIAL SCREEN                   
         BZ    LOOK6                                                            
         MVC   ELDATA,HYCODATA     THEN LOOK FOR HYPO. COMML. EL.               
         MVI   ELTYPE,TAESTHCO                                                  
         B     LOOK8                                                            
LOOK6    CLC   DMCB,ATIA           SKIP IF NOT LOOKING AT ENTIRE RECORD         
         BNE   LOOK10              (COMM'L ISN'T IN TEMP RECORD-IO2)            
LOOK8    BAS   RE,SEARCH                                                        
         BNE   LOOKNO                                                           
         SPACE 1                                                                
LOOK10   MVC   ELDATA,WORK         LOOK UP SAVED DATA                           
         MVC   ELTYPE,WORK+L'ELDATA                                             
         BAS   RE,SEARCH                                                        
         BNE   LOOKNO                                                           
*                                  TURN OFF STATUS BITS IF ON                   
         NI    STATUS3,ALL-LOOKCID-LOOKSORT-LOOKHIN                             
         B     LYES                RETURN CC EQ                                 
LOOKNO   DS    0H                  TURN OFF STATUS BITS IF ON                   
         NI    STATUS3,ALL-LOOKCID-LOOKSORT-LOOKHIN                             
         B     LNO                 RETURN CC NE                                 
         SPACE 1                                                                
LYES     XR    RC,RC                                                            
LNO      LTR   RC,RC                                                            
LXIT     XIT1                                                                   
LXITR3   XIT1  REGS=(R3)                                                        
         EJECT                                                                  
*=====================================================================          
*              ROUTINE TO SEARCH FOR AN ESTIMATE ELEMENT                        
*=====================================================================          
*                                  R3=A(WHERE TO START LOOKING)                 
*                                  DMCB HAS ADDRESS OF RECORD                   
         USING TAESD,R4            ELDATA=KEY DATA OF ELEMENT TO FIND           
SEARCH   NTR1                                                                   
         MVC   BYTE,ELTYPE                                                      
         NI    BYTE,X'0F'          BYTE=LOBS OF EL. TYPE TO SEARCH FOR          
         MVI   ELCODE,TAESELQ                                                   
         LR    R4,R3               R4=A(WHERE TO START LOOKING)                 
         C     R4,DMCB             IF WE'RE NOT AT BEGININNING                  
         BNE   SEA2                START LOOKING AT NEXT ELEMENT                
         BAS   RE,GETEL3                                                        
         B     *+8                                                              
SEA2     BAS   RE,NEXTEL3                                                       
         BNE   SEAX                NOT FOUND - RETURN CC NOT EQUAL              
         SPACE 1                                                                
         MVC   TGBYTE,TAESTYPE                                                  
         NI    TGBYTE,X'0F'        TGBYTE=LOBS OF THIS ELEMENT                  
         CLC   TGBYTE,BYTE         MUST MATCH ON LOBS OF TYPE                   
         BL    SEAX                AT NEXT HIGHER LEVEL - STOP                  
         BH    SEA8                NOT AT THIS LEVEL YET - TRY AGAIN            
*                                                                               
         CLC   TAESTYPE,ELTYPE     HAVE WE REALLY FOUND CORRECT TYPE            
         BNE   SEA8                NO - TRY AGAIN                               
*                                                                               
SEA3     CLI   ELTYPE,TAESTCOM     IF LOOKING UP COMMERCIAL                     
         BNE   SEA4                                                             
         TM    STATUS3,LOOKCID     AND NOT LOOKING UP BY CID                    
         BO    SEA4                                                             
         CLC   TAESCOM,ELDATA+TAESCOM-TAESDATA  MATCH INT COML NO. ONLY         
         B     SEA8                                                             
         SPACE 1                                                                
SEA4     CLI   ELTYPE,TAESTPER     IF LOOKING UP PERFORMER                      
         BNE   SEA5                                                             
         TM    STATUS3,LOOKSORT    AND NOT LOOKING UP BY SORT KEY               
         BO    SEA5                                                             
         CLC   TAESSORT+4(2),ELDATA+TAESSORT+4-TAESDATA  MATCH ON SEQ           
         B     SEA8                                                             
         SPACE 1                                                                
*                                                                               
SEA5     CLI   NEWEST,C'Y'         IF NEW ESTIMATE DISPLAY                      
         BNE   SEA6                                                             
         TM    STATUS3,LOOKHIN     AND LOOKING TO INSERT HYPO PERFORMER         
         BZ    SEA6                                                             
         CLI   ELTYPE,TAESTHPE                                                  
         BNE   SEA6                                                             
         CLI   TAESTYPE,TAESTHPE   AND THIS IS A HYPO PERFORMER EL              
         BE    SEA6X               (SEARCH BY HYPO PAGE NUMBER)                 
*                                                                               
SEA6     LA    R1,L'TAESDATA-1     DETERMINE L'COMPARE                          
         CLI   TAESTYPE,TAESTHCO   HYPOTHETICAL COMMERCIALS?                    
         BNE   *+8                                                              
         LA    R1,L'TAESHCOM-1     USE ONLY L'COMML CODE                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   TAESDATA(0),ELDATA  MUST ALSO MATCH ON DATA                      
         BNH   SEA8                                                             
SEA6H    TM    DISPMD2,SELCTHPF    OR IN HYPO PERF SELECT MODE                  
         BO    SEA6X               (CHECK SEARCH BY PAGE NUMBER)                
         CLC   DMCB,AIO2           ELSE, IF WE ARE LOOKING AT TEMP. REC         
*                                  (IMPLIES WE'RE COMING FROM COPY RTN)         
         BNE   SEAX                                                             
         SPACE 1                                                                
SEA6X    CLI   ELTYPE,TAESTHPE     HYPO. PERF. ELS. ARE NOT SORTED              
         BNE   SEAX                OTHERS ARE PAST IT - STOP                    
         TM    STATUS3,LOOKHIN     IF LOOKING TO INSERT A HYPO PERF             
         BZ    SEA7                                                             
         CLC   TAESHPG,SHYPRFPG    SEARCH TILL PAST CURRENT PAGE                
         BNH   SEA7                                                             
         LTR   RE,RE               SET FOUND INSERTION POINT                    
         B     SEAX                                                             
         SPACE 1                                                                
SEA7     LTR   RE,RE               RESET CC NE TO CONTINUE SEARCH               
         SPACE 1                                                                
SEA8     MVC   LSTELSEQ,TAESSEQ    SAVE THIS SEQUENCE NUMBER                    
         BNE   SEA2                IF CC NOT EQUAL, TRY AGAIN                   
         SPACE 1                                                                
SEAX     LR    R3,R4               RETURN ADDRESS IN R3                         
         ST    R3,FULL             AND IN FULL                                  
         B     LXITR3              ALSO RETURNING CC                            
         EJECT                                                                  
         GETELN (R4),DATADISP,ELCODE,3                                          
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENEOPTS        OPTION EQUATES                               
         EJECT                                                                  
*              OPTION TABLE  (SEE DSECT OPTD)                                   
*              IF ADDING ENTRY, MUST ADD TO OPTTAB IN TAESTPRT AND              
*              IN TAGENDF                                                       
         SPACE 1                                                                
OPTTAB   DS    0C                                                               
         DC    AL1(OPADDL,LOPADDL,OPTNORHS+OPTSVLST+OPTOKDIS)                   
         DC    AL1(OPOKCOM)                                                     
         DC    AL2(INADDL-OPSCAN,OUTPERF-OPUNSCAN)                              
         DC    CL10'ADDITIONAL'          ADD PERFORMERS                         
*                                                                               
         DC    AL1(OPI,LOPI,OPTNOLHS+OPTNORHS+OPTSVLST+OPTNOEL)                 
         DC    AL1(OPOKCOM+OPOKPER)                                             
         DC    AL2(INI-OPSCAN,0)         INSERT NO. OF LINES                    
*                                                                               
         DC    AL1(OPPERF,LOPPERF,OPTNORHS+OPTSVLST+OPTOKDIS)                   
         DC    AL1(OPOKCOM)                                                     
         DC    AL2(INPERF-OPSCAN,OUTPERF-OPUNSCAN)                              
         DC    CL10'PERFORMERS'          DISPLAY PERFORMER LIST                 
*                                                                               
         DC    AL1(OPX,LOPX,OPTNORHS+OPTSVLST)                                  
         DC    AL1(OPOKPER+OPOKHYPE)                                            
         DC    AL2(INCHAR-OPSCAN,OUTLHS1-OPUNSCAN)                              
         DC    CL10'XCLUDE'              EXCLUDE PERFORMER                      
*                                                                               
         DC    AL1(OPYES,LOPYES,OPTNORHS+OPTSVLST)                              
         DC    AL1(OPOKCOM)                                                     
         DC    AL2(INCHAR-OPSCAN,OUTLHS1-OPUNSCAN)                              
         DC    CL10'YES'                 INCLUDE COMMERCIAL                     
*                                                                               
         DC    AL1(OPBROAD,LOPBROAD,OPTSVLST)                                   
         DC    AL1(OPOKHYCO)                                                    
         DC    AL2(INPRBR-OPSCAN,OUTPRBR-OPUNSCAN)                              
         DC    CL10'BROADCAST'           WHERE BROADCAST                        
*                                                                               
         DC    AL1(OPCANTX,LOPCANTX,OPTSVLST)                                   
         DC    AL1(ALL)                                                         
         DC    AL2(INPCTR-OPSCAN,OUTAMT3-OPUNSCAN)                              
         DC    CL10'CANTAX'              CANADIAN TAX RATE OVERRIDE             
*                                                                               
         DC    AL1(OPCOMM,LOPCOMM,OPTSVLST)                                     
         DC    AL1(ALL)                                                         
         DC    AL2(INPCTR-OPSCAN,OUTAMT3-OPUNSCAN)                              
         DC    CL10'COMMISSION'          COMMISSION RATE OVERRIDE               
*                                                                               
         DC    AL1(OPEORTX,LOPEORTX,OPTSVLST)                                   
         DC    AL1(ALL)                                                         
         DC    AL2(INPCTR-OPSCAN,OUTAMT3-OPUNSCAN)                              
         DC    CL10'EORTAX'              E-O-R TAX RATE OVERRIDE                
*                                                                               
         DC    AL1(OPEXCH,LOPEXCH,OPTSVLST)                                     
         DC    AL1(ALL)                                                         
         DC    AL2(INEXCH-OPSCAN,OUTEXCH-OPUNSCAN)                              
         DC    CL10'EXCHANGE'            EXCHANGE RATE                          
*                                                                               
         DC    AL1(OPEXP,LOPEXP,OPTSVLST)                                       
         DC    AL1(ALL-OPOKPER-OPOKHYPE)                                        
         DC    AL2(INEXP-OPSCAN,OUTEXP-OPUNSCAN)                                
         DC    CL10'EXPIRY'              EXPIRY DATE OVERRIDE                   
*                                                                               
         DC    AL1(OPINCTX,LOPINCTX,OPTSVLST)                                   
         DC    AL1(ALL)                                                         
         DC    AL2(INPCTR-OPSCAN,OUTAMT3-OPUNSCAN)                              
         DC    CL10'INCTAX'              INCORPORATED TAX RATE OVERRIDE         
*                                                                               
         DC    AL1(OPINTEG,LOPINTEG,OPTSVLST)                                   
         DC    AL1(ALL)                                                         
         DC    AL2(ININTEG-OPSCAN,OUTAMT3-OPUNSCAN)                             
         DC    CL10'INTEGRATE'           INTEGRATION FEES                       
*                                                                               
         DC    AL1(OPLLIFT,LOPLLIFT,OPTSVLST)                                   
         DC    AL1(OPOKHYCO+OPOKCOM)                                            
         DC    AL2(INBINR-OPSCAN,OUTBINR-OPUNSCAN)                              
         DC    CL10'LLIFT'               L'LIFT                                 
*                                                                               
         DC    AL1(OPLLIF2,LOPLLIF2,OPTSVLST)                                   
         DC    AL1(OPOKHYCO+OPOKCOM)                                            
         DC    AL2(INBINR-OPSCAN,OUTBINR-OPUNSCAN)                              
         DC    CL10'LLIF2'               L'LIFT                                 
*                                                                               
         DC    AL1(OPMULT,LOPMULT,OPTSVLST)                                     
         DC    AL1(OPOKPRD+OPOKCOM+OPOKHYCO)                                    
         DC    AL2(INPCTR-OPSCAN,OUTAMT3-OPUNSCAN)                              
         DC    CL10'MULTIPLIER'          MULTIPLIER                             
*                                                                               
         DC    AL1(OPOVER2,LOPOV2,OPTSVLST)                                     
         DC    AL1(OPOKPER+OPOKHYPE)                                            
         DC    AL2(INOVER2-OPSCAN,OUTOVER2-OPUNSCAN)                            
         DC    CL10'OVERSCALE'          SECOND USE OVERSCALE PERCENTAGE         
*                                                                               
         DC    AL1(OPOV12,LOPOV12,OPTSVLST)                                     
         DC    AL1(OPOKPER+OPOKHYPE)                                            
         DC    AL2(INOV12-OPSCAN,OUTOV12-OPUNSCAN)                              
         DC    CL10'OVERSCALE'           USE OVERSCALE PCT 1&2                  
*                                                                               
         DC    AL1(OPOVER,LOPOVER,OPTSVLST)                                     
         DC    AL1(OPOKPER+OPOKHYPE)                                            
*********DC    AL1(ALL)                                                         
         DC    AL2(INOVER-OPSCAN,OUTAMT2-OPUNSCAN)                              
         DC    CL10'OVERSCALE'           USE OVERSCALE PERCENTAGE               
*                                                                               
         DC    AL1(OPHLDRAS,LOPHLDRA,OPTSVLST+OPTNOLHS)                         
         DC    AL1(OPOKPER+OPOKHYPE)                                            
         DC    AL2(INHLDRA-OPSCAN,OUTHLDRA-OPUNSCAN) HLD FEE RATE AS/OF         
*                                                                               
         DC    AL1(OPHLDAS,LOPHLDAS,OPTSVLST+OPTNOLHS)                          
         DC    AL1(OPOKPER+OPOKHYPE)                                            
         DC    AL2(INHLDAS-OPSCAN,OUTHLDAS-OPUNSCAN) HLD FEE AMT AS/OF          
*                                                                               
         DC    AL1(OPYRAS,LOPYRAS,OPTSVLST+OPTNOLHS)                            
         DC    AL1(ALL)                                                         
         DC    AL2(INYRAS-OPSCAN,OUTYRAS-OPUNSCAN) YEAR AS/OF DATE              
*                                                                               
         DC    AL1(OPOV2AS,LOPOV2AS,OPTSVLST+OPTNOLHS)                          
         DC    AL1(OPOKPER+OPOKHYPE)                                            
         DC    AL2(INOV2AS-OPSCAN,OUTOV2AS-OPUNSCAN) OV2 AS/OF DATE             
*                                                                               
         DC    AL1(OPOV12AS,LOPOV12A,OPTSVLST+OPTNOLHS)                         
         DC    AL1(OPOKPER+OPOKHYPE)                                            
         DC    AL2(INOV12A-OPSCAN,OUTOV12A-OPUNSCAN) OV1&2 AS/OF DATE           
*                                                                               
         DC    AL1(OPOVAS,LOPOVAS,OPTSVLST+OPTNOLHS)                            
         DC    AL1(OPOKPER+OPOKHYPE)                                            
*********DC    AL1(ALL)                                                         
         DC    AL2(INOVAS-OPSCAN,OUTOVAS-OPUNSCAN) OVERSCALE AS/OF DATE         
*                                                                               
         DC    AL1(OPPROD,LOPPROD,OPTSVLST)                                     
         DC    AL1(OPOKHYCO)                                                    
         DC    AL2(INPRBR-OPSCAN,OUTPRBR-OPUNSCAN)                              
         DC    CL10'PRODUCED'            WHERE PRODUCED                         
*                                                                               
         DC    AL1(OPCTYP,LOPCTYP,OPTSVLST)                                     
         DC    AL1(OPOKHYCO)                                                    
         DC    AL2(INCTYP-OPSCAN,OUTRHS2-OPUNSCAN)                              
         DC    CL10'TYPE'                COMMERCIAL TYPE                        
*                                                                               
         DC    AL1(OPYEAR,LOPYEAR,OPTSVLST)                                     
         DC    AL1(ALL-OPOKPRD)                                                 
         DC    AL2(INYEAR-OPSCAN,OUTYEAR-OPUNSCAN)                              
         DC    CL10'YEAR'                CONTRACT YEAR OVERRIDE                 
*                                                                               
         DC    AL1(OPUSE,LOPUSE,OPTNOLHS+OPTNORHS+OPTSVLST)                     
         DC    AL1(ALL)                                                         
         DC    AL2(INUSE-OPSCAN,OUTUSE-OPUNSCAN) USE TYPES                      
*                                                                               
         DC    AL1(OPASOF,LOPASOF,OPTNOLHS+OPTNORHS+OPTFORUS)                   
         DC    AL1(ALL)                                                         
         DC    AL2(INASOF-OPSCAN,OUTASOF-OPUNSCAN)  AS OF DATES                 
*                                                                               
         DC    AL1(OPGRTNO,LOPGRTNO,OPTNOLHS+OPTNORHS+OPTSVLST)                 
         DC    AL1(OPOKPER+OPOKHYPE)                                            
         DC    AL2(INGRTNO-OPSCAN,OUTGRTNO-OPUNSCAN) GUARANTEE NUMBER           
*                                                                               
         DC    AL1(OPPRI,LOPPRI,OPTNORHS)                                       
         DC    AL1(OPOKPER+OPOKHYPE)                                            
         DC    AL2(INPRI-OPSCAN,OUTLHS3-OPUNSCAN) PRIMARY COMML IND.            
         DC    CL10'PRIMARY'                                                    
*                                                                               
         DC    AL1(OPSSN,LOPSSN,OPTNOLHS+OPTNORHS)                              
         DC    AL1(OPOKHYPE)                                                    
         DC    AL2(INSSN-OPSCAN,OUTSSN-OPUNSCAN) SOCIAL SECURITY NO.            
*                                                                               
         DC    AL1(OPGUAR,LOPGUAR,OPTSVLST)                                     
         DC    AL1(ALL)                                                         
         DC    AL2(INGUAR-OPSCAN,OUTRHS1-OPUNSCAN) GRT CREDITTING STAT          
         DC    CL10'GUARANTEE'                                                  
*                                                                               
         DC    AL1(OPMAJOR,LOPMAJOR,OPTNOLHS+OPTNORHS+OPTFORUS)                 
         DC    AL1(ALL)                                                         
         DC    AL2(INMAJ-OPSCAN,OUTMAJ-OPUNSCAN) MAJORS                         
*                                                                               
         DC    AL1(OPUNITS,LOPUNITS,OPTNOLHS+OPTNORHS+OPTFORUS)                 
         DC    AL1(ALL)                                                         
         DC    AL2(INUNITS-OPSCAN,OUTUNITS-OPUNSCAN) UNITS                      
*                                                                               
         DC    AL1(OPUSES,LOPUSES,OPTNOLHS+OPTNORHS+OPTFORUS)                   
         DC    AL1(ALL)                                                         
         DC    AL2(INUSES-OPSCAN,OUTBIN-OPUNSCAN) NUMBER OF USES                
*                                                                               
         DC    AL1(OPUSES2,LOPUSES2,OPTNOLHS+OPTNORHS+OPTFORUS)                 
         DC    AL1(ALL)                                                         
         DC    AL2(INUSES2-OPSCAN,OUTUSES2-OPUNSCAN) SPECIFIC USE NOS.          
*                                                                               
         DC    AL1(OPINS,LOPINS,OPTNOLHS+OPTNORHS+OPTFORUS)                     
         DC    AL1(ALL)                                                         
         DC    AL2(ININS-OPSCAN,OUTBIN-OPUNSCAN)  NUMBER OF INSERTS             
*                                                                               
         DC    AL1(OPTAG,LOPTAG,OPTNOLHS+OPTNORHS+OPTFORUS)                     
         DC    AL1(ALL)                                                         
         DC    AL2(INTAG-OPSCAN,OUTBIN1-OPUNSCAN)  NUMBER OF TAGS               
*                                                                               
         DC    AL1(OPDEMO,LOPDEMO,OPTNOLHS+OPTNORHS+OPTFORUS)                   
         DC    AL1(ALL)                                                         
         DC    AL2(INDEMO-OPSCAN,OUTBIN1-OPUNSCAN)  NUMBER OF DEMOS             
*                                                                               
         DC    AL1(OPPCT,LOPPCT,OPTNOLHS+OPTNORHS+OPTFORUS)                     
         DC    AL1(ALL)                                                         
         DC    AL2(INPCT-OPSCAN,OUTPCT-OPUNSCAN) PERCENTAGES                    
*                                                                               
         DC    AL1(OPAMT,LOPAMT,OPTNOLHS+OPTNORHS+OPTFORUS)                     
         DC    AL1(ALL)                                                         
         DC    AL2(INAMT-OPSCAN,OUTAMTL-OPUNSCAN) AMOUNTS                       
*                                                                               
         DC    AL1(OPCYC,LOPCYC,OPTNOLHS+OPTFORUS)                              
         DC    AL1(ALL)                                                         
         DC    AL2(INCYC-OPSCAN,OUTCYC-OPUNSCAN) CYCLE DATES                    
*                                                                               
         DC    AL1(OPDATE,LOPDATE,OPTNOLHS+OPTNORHS+OPTFORUS)                   
         DC    AL1(ALL)                                                         
         DC    AL2(INDATE-OPSCAN,OUTDATEL-OPUNSCAN) DATES                       
*                                                                               
         DC    AL1(OPHIST,LOPHIST,OPTNORHS+OPTFORUS)                            
         DC    AL1(ALL)                                                         
         DC    AL2(INCHAR-OPSCAN,OUTLHS1-OPUNSCAN)                              
         DC    CL10'HISTORY'             HISTORY ONLY - DON'T PAY               
*                                                                               
         DC    AL1(OPL,LOPL,OPTNORHS+OPTFORUS)                                  
         DC    AL1(ALL)                                                         
         DC    AL2(INCHAR-OPSCAN,OUTLHS1-OPUNSCAN)                              
         DC    CL10'L'                   PAYMENT TO LIFT                        
*                                                                               
         DC    AL1(OPS,LOPS,OPTNORHS+OPTFORUS)                                  
         DC    AL1(ALL)                                                         
         DC    AL2(INCHAR-OPSCAN,OUTLHS1-OPUNSCAN)                              
         DC    CL10'S'                   PAYMENT TO 2ND LIFT                    
*                                                                               
         DC    AL1(OPLIFT,LOPLIFT,OPTNORHS+OPTSVLST)                            
         DC    AL1(OPOKPER)                                                     
         DC    AL2(INCHAR-OPSCAN,OUTLHS2-OPUNSCAN)                              
         DC    CL10'LIFT'                PERFORMER ON LIFT                      
*                                                                               
         DC    AL1(OPLONLY,LOPLONLY,OPTNORHS+OPTSVLST)                          
         DC    AL1(OPOKPER)                                                     
         DC    AL2(INCHAR-OPSCAN,OUTLHS2-OPUNSCAN)                              
         DC    CL10'LONLY'               PERFORMER ON LIFT ONLY                 
*                                                                               
         DC    AL1(OPPNH,LOPPNH,OPTFORUS)                                       
         DC    AL1(ALL)                                                         
         DC    AL2(INAMTR-OPSCAN,OUTAMT3-OPUNSCAN)                              
         DC    CL10'P&&H'                P&H AMOUNT                             
*                                                                               
         DC    AL1(OPSPNH,LOPSPNH,OPTFORUS)                                     
         DC    AL1(ALL)                                                         
         DC    AL2(INAMTR-OPSCAN,OUTAMT4-OPUNSCAN)                              
         DC    CL10'SP&&H'               SUBJECT TO P&H AMOUNT                  
*                                                                               
         DC    AL1(OPPNHR,LOPPNHR,OPTSVLST)                                     
         DC    AL1(ALL)                                                         
         DC    AL2(INPNHR-OPSCAN,OUTAMT4-OPUNSCAN)                              
         DC    CL10'P&&HR'               P&H RATE                               
*                                                                               
         DC    AL1(OPNO,LOPNO,OPTNORHS+OPTFORUS)                                
         DC    AL1(ALL)                                                         
         DC    AL2(INNO-OPSCAN,OUTLHS1-OPUNSCAN)                                
         DC    CL10'NO'                  DON'T INCLUDE                          
*                                                                               
         DC    AL1(OPUK,LOPUK,OPTNORHS+OPTFORUS)                                
         DC    AL1(0)                                                           
         DC    AL2(0,OUTLHS2-OPUNSCAN)                                          
         DC    CL10'UK'                  UK FOR FOREIGN USE                     
*                                                                               
         DC    AL1(OPAPPLY,LOPAPPLY,OPTFORUS)                                   
         DC    AL1(ALL)                                                         
         DC    AL2(INAPPLY-OPSCAN,OUTRHS1-OPUNSCAN)                             
         DC    CL10'APPLY'               APPLY SESSION/HLD                      
*                                                                               
         DC    AL1(OPLAST,LOPLAST,OPTSVLST)                                     
         DC    AL1(OPOKPER+OPOKHYPE)                                            
         DC    AL2(INDATER-OPSCAN,OUTDATER-OPUNSCAN)                            
         DC    CL10'LAST SERV'           LAST SERVICES DATE                     
*                                                                               
         DC    AL1(OPSTATE,LOPSTATE,OPTSVLST)                                   
         DC    AL1(OPOKHYCO)                                                    
         DC    AL2(INSTATE-OPSCAN,OUTRHS2-OPUNSCAN)                             
         DC    CL10'STATE'               ADDENDUM STATE                         
*                                                                               
         DC    AL1(OPAFM,LOPAFM,OPTSVLST)                                       
         DC    AL1(OPOKCOM+OPOKHYCO)                                            
         DC    AL2(INAFM-OPSCAN,OUTRHS3-OPUNSCAN)                               
         DC    CL10'AFM RATE'            AFM RATE                               
*                                                                               
         DC    AL1(OPCOMNM,LOPCOMNM,OPTNOLHS+OPTIGN)                            
         DC    AL1(OPOKHYCO)                                                    
         DC    AL2(0,OUTCOMNM-OPUNSCAN)  HYPO COMML NAME                        
*                                                                               
         DC    AL1(OPNAP,LOPNAP,OPTSVLST)                                       
         DC    AL1(ALL)                                                         
         DC    AL2(INDATER-OPSCAN,OUTDATER-OPUNSCAN)                            
         DC    CL10'NAP'                 NO AUTO PAYMENTS                       
*                                                                               
         DC    AL1(OPATYP,LOPATYP,OPTSVLST)                                     
         DC    AL1(OPOKHYCO)                                                    
         DC    AL2(INATYP-OPSCAN,OUTATYPE-OPUNSCAN)                             
         DC    CL10'ATYPE'                COMMERCIAL ACTRA TYPE                 
*                                                                               
         DC    AL1(OPCSF,LOPCSF,OPTFORUS)                                       
         DC    AL1(ALL)                                                         
         DC    AL2(INCSF-OPSCAN,OUTCSF-OPUNSCAN)                                
         DC    CL10'CSF'                  CONTRACT SERVICE FEE                  
*                                                                               
         DC    AL1(OPHLDR,LOPHLDR,OPTSVLST)                                     
         DC    AL1(OPOKPER+OPOKHYPE)                                            
         DC    AL2(INHLDR-OPSCAN,OUTWPCTR-OPUNSCAN)                             
         DC    CL10'HLD'                  HOLDING FEE PERCENTAGE                
*                                                                               
         DC    AL1(OPHLD,LOPHLD,OPTSVLST)                                       
         DC    AL1(OPOKPER+OPOKHYPE)                                            
         DC    AL2(INAMTR-OPSCAN,OUTAMT3-OPUNSCAN)                              
         DC    CL10'HLD'                  HOLDING FEE AMOUNT                    
*                                                                               
         DC    AL1(OP1ST,LOP1ST,OPTSVLST)                                       
         DC    AL1(OPOKHYCO)                                                    
         DC    AL2(INDATER-OPSCAN,OUTDATER-OPUNSCAN)                            
         DC    CL10'1ST'                  FIRST FIXED CYCLE                     
*                                                                               
         DC    AL1(OPHNDTX,LOPHNDTX,OPTFORUS)                                   
         DC    AL1(OPOKPER+OPOKHYPE)                                            
         DC    AL2(INHND-OPSCAN,OUTAMT3-OPUNSCAN)                               
         DC    CL10'HNDTAX'              INCORPORATED TAX AMT OVERRIDE          
*                                                                               
         DC    AL1(OPLCLAU,LOPLCLAU,OPTNOLHS+OPTNORHS+OPTFORUS)                 
         DC    AL1(ALL)                                                         
         DC    AL2(INLCLAU-OPSCAN,OUTBIN-OPUNSCAN) LAST CLA USE                 
*                                                                               
         DC    AL1(OPPAY1,LOPPAY1,OPTNOLHS+OPTNORHS+OPTFORUS)                   
         DC    AL1(ALL)                                                         
         DC    AL2(INPAY1-OPSCAN,OUTBIN-OPUNSCAN) PAY 1ST USE                   
*                                                                               
         DC    X'FF'                                                            
*                                                                               
NUSOPTS  EQU   22                  N'OPTIONS FOR USE (HAS OPTFORUS)             
         EJECT                                                                  
*              ROUTINE PERFORMS GENERAL INITIALIZATION ROUTINES                 
INIT     NTR1  BASE=*,LABEL=*                                                   
         TM    TRNSTAT2,CTFLSTER    IF FULL SESSION STEREO                      
         BZ    *+8                                                              
         NI    GENSTAT4,ALL-CONFDEL NO NEED TO CONFIRM DELETES                  
*                                                                               
         CLI   ACTNUM,ACTDEL       IF ACTION NOT DELETE                         
         BE    *+8                                                              
         OI    ESTREVH+6,X'81'     SET REVISION FIELD ALWAYS MOD.               
         SPACE 1                                                                
         NI    STATUS,ALL-RESET                                                 
         CLI   SVSTEREO,0          IF NOT FIRST TIME IN                         
         BE    *+14                                                             
         CLC   SVSTEREO,NEWEST     & CHANGE IN CT BTWN TRANSACTIONS             
         BNE   *+12                                                             
         CLI   SCRSTAT,0           OR IF A HEADING FIELD CHANGED                
         BE    INIT10                                                           
         OI    STATUS,RESET+DISPLYIT  SET TO START AGAIN                        
         NI    STATUS,ALL-MISCDSPD                                              
         NI    ESTESTH+4,X'DF'     INSURE EST FIELD SET NOT VALIDATED           
         NI    ESTFLTSH+4,X'DF'    INSURE FILTERS FLD SET NOT VALIDATED         
*                                  *** DO SOME RELOCATION ***                   
INIT10   MVI   SVSTEREO,C'N'       SAVE NOT CONNECTED                           
         TM    TRNSTAT2,CTFLSTER   IF FULL SESSION STEREO                       
         BZ    INIT15                                                           
         MVI   SVSTEREO,C'Y'       SAVE CONNECTED                               
         L     RE,SYSPARMS                                                      
         L     RE,0(RE)            RE=A(TRANSLATOR I/O BLOCK)                   
         USING TIOBD,RE                                                         
         TM    STATUS,RESET                                                     
         BZ    INIT15                                                           
         OI    TIOBINDS,TIOBSCRN   SET OVERRIDING SCREEN                        
         MVI   TIOBCNT,SCR2C       INSURE ORIGINAL SCREEN CODE                  
         DROP  RE                                                               
         SPACE 1                                                                
INIT15   MVI   SPACES,C' '         INITIALIZE SPACES                            
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                  *** DO SOME RELOCATION ***                   
         LA    R1,RTNTAB           R1=A(ROUTINES TO RELOCATE)                   
         LA    R2,AROUTINS         R2=A(ADDRESS OF ROUTINES)                    
         LA    R0,NRTNS            R0=(NUMBER OF ROUTINES)                      
         SPACE 1                                                                
INIT20   L     RE,0(R1)                                                         
         A     RE,RELO                                                          
         ST    RE,0(R2)                                                         
         LA    R1,4(R1)            BUMP TO NEXT TABLE ENTRY                     
         LA    R2,4(R2)            BUMP TO NEXT ADDRESS                         
         BCT   R0,INIT20                                                        
         SPACE 1                                                                
         LA    R1,SUBELBLK         A(SUB-ELEMENT BLOCK)                         
         ST    R1,ASUBELS                                                       
         GOTO1 APFVAL              VALIDATE PFKEYS                              
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINES TO RELOCATE                                             
         SPACE 1                                                                
RTNTAB   DS    0A                                                               
         DC    AL4(OPSCAN)                                                      
         DC    AL4(OPUNSCAN)                                                    
         DC    AL4(VALCP)                                                       
         DC    AL4(PFVAL)                                                       
         DC    AL4(INITSTOR)                                                    
         DC    AL4(VALFILT)                                                     
         DC    AL4(GETEREC)                                                     
         DC    AL4(DISSHRS)                                                     
         DC    AL4(VALHPRF)                                                     
         DC    AL4(DISHPLN)                                                     
         DC    AL4(VALMISC)                                                     
         DC    AL4(OPTTAB)                                                      
         DC    AL4(WRITEIT)                                                     
         DC    AL4(DELETE)                                                      
         DC    AL4(LOOK)                                                        
         SPACE 1                                                                
NRTNS    EQU   (*-RTNTAB)/L'RTNTAB                                              
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
*        ROUTINE LOOKS FOR VERSION ELEMENTS AND SHOWS AS LIFT         *         
*        ON ENTRY ... R3=A(SCREEN DSECT LINE)                                   
***********************************************************************         
                                                                                
         USING LINCOMD,R3          R3=A(SCREEN DSECT LINE)                      
DISCIVER NTR1  BASE=*,LABEL=*                                                   
         USING TAVRD,R4                                                         
         L     R4,AIO              R4=A(COMMERCIAL RECORD)                      
         MVI   ELCODE,TAVRELQ      READ VERSION ELEMENTS                        
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
DISCLN52 BRAS  RE,NEXTEL                                                        
         BNE   DCIVX                                                            
         CLI   TAVRVERS,2          IF ELEMENT FOR VERSION 2                     
         BNE   DISCLN52            IS FOUND ...                                 
         MVC   LINCLID,TAVRCID     DISPLAY VERSION INFO AS LIFT                 
         EDIT  TAVRSEC,(3,LINCLSEC),ALIGN=LEFT                                  
DCIVX    XIT1                                                                   
         DROP  R3,R4                                                            
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO DISPLAY PERFOMER OVERSCALES                           
         USING TACAD,R4            R4=A(CAST DETAILS ELEMENT)                   
*                                  R3=A(DISLAY AREA FOR OVERSCALE)              
         SPACE 1                                                                
DSPOV    NTR1  BASE=*,LABEL=*                                                   
         GOTO1 GETOV1,DMCB,SPACES,FULL  GET OVERSCALE 1                         
         CLI   0(R1),X'FF'                                                      
         JNE   *+10                                                             
         XC    FULL,FULL           RETURNED AMOUNT - IGNORE                     
                                                                                
         XR    RE,RE               HANDLE 1ST OVERSCALE                         
         L     RF,FULL                                                          
         D     RE,=F'100'          PUSH OUT PENNIES                             
         EDIT  (RF),(3,(R3)),ALIGN=LEFT                                         
         AR    R3,R0               BUMP PAST AMOUNT                             
                                                                                
         GOTO1 SETOV2,DMCB,(R4),AIO,SPACES                                      
         ICM   RF,15,TACAOV2       DO WE HAVE 2ND OVERSCALE                     
         JZ    XIT                                                              
         MVI   0(R3),C','          ADD COMMA                                    
         XR    RE,RE                                                            
         D     RE,=F'100'          PUSH OUT PENNIES                             
         EDIT  (RF),(3,1(R3)),ALIGN=LEFT                                        
         J     XIT                                                              
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TACHPACK                                                       
*              DSECT TO COVER LOCAL SAVED STORAGE FOR PROGRAM                   
         SPACE 1                                                                
ESTD     DSECT                                                                  
SVRC     DS    F                                                                
RELO     DS    F                   A(RELOCATION FACTOR)                         
*                                                                               
AROUTINS DS    0A                  A(ROUTINES) RELOCATED                        
AOPSCAN  DS    A                   A(OPTION SCAN ROUTINE)                       
AOPUNSCN DS    A                   A(OPTION UNSCAN ROUTINE)                     
AVALCP   DS    A                   A(VALIATE CLIENT/PRODUCT ROUTINE)            
APFVAL   DS    A                   A(VALIDATE PFKEY INPUT ROUTINE)              
AINITSTR DS    A                   A(INITIALIZE STORAGE ROUTINE)                
AVALFILT DS    A                   A(DISPLAY FILTER VALIDATION ROUTINE)         
AGETEREC DS    A                   A(GET AN ESTIMATE RECORD)                    
ADISSHRS DS    A                   A(DISPLAY RHS HYPO LIST/STEREO RTN)          
AVALHPRF DS    A                   A(VALIDATE HYPO PERFOMER FLD RTN)            
ADISHPLN DS    A                   A(VALIDATE HYPO PERFOMER FLD RTN)            
AVALMISC DS    A                   A(VALIDATE MISC ADDS SCREEN)                 
AOPTTAB  DS    A                   A(OPTION TABLE)                              
AWRITEIT DS    A                   A(WRITE BACK ESTIMATE RECORD(S))             
ADELETE  DS    A                   A(DELETE ROUTINE)                            
ALOOKUP  DS    A                   A(LOOK UP ESTIMATE ELEMENT)                  
*                                                                               
SPACES   DS    CL80                ** WAS 132 CHANGED ON 11/12/97               
REVISION DS    CL3                 REVISION NUMBER                              
*                                                                               
DISPMODE DS    CL1                 DISPLAY MODE                                 
LISTCOMS EQU   X'80'               LISTING COMMERCIALS                          
LISTHYP  EQU   X'40'               LISTING HYPOTHETICAL COMMERCIALS             
LISTPRFS EQU   X'20'               LISTING PERFORMERS                           
HYPOCOM  EQU   X'10'               HYPO COMMERCIAL SCREEN LOADED                
HYPOPER  EQU   X'08'               HYPO PERFORMER SCREEN LOADED                 
HYPO     EQU   HYPOCOM+HYPOPER                                                  
MISCADD  EQU   X'04'               MISCELLANEOUS ADDITION SCREEN LOADED         
LISTSCOM EQU   X'02'               LISTING SELECTABLE COMMERCIALS               
SELCTCOM EQU   X'01'               COMMERCIAL SELECT MODE                       
*                                                                               
DISPMD2  DS    CL1                 2ND DISPLAY MODE STATUS                      
LISTSPRF EQU   X'80'               LISTING SELECTABLE PERFORMERS                
SELCTPRF EQU   X'40'               PERFORMER SELECT MODE                        
SELCTHPF EQU   X'20'               HYPO PERFORMER SELECT MODE                   
*                                                                               
STATUS   DS    CL1                                                              
RESET    EQU   X'80'               CHANGES TO SCREEN REQUIRING NEW LIST         
DISPLYIT EQU   X'40'               CHANGES TO SCREEN REQ. DISPLAY               
PRDCHGED EQU   X'20'               PRODUCT FIELD CHANGED                        
ENDOFLST EQU   X'10'               END OF COMMERCIAL LIST                       
INSERTC  EQU   X'08'               INSERT MODE FOR COMMERCIALS                  
INSERTP  EQU   X'04'               INSERT MODE FOR PERFORMERS                   
FINISHED EQU   X'02'               TOLD USER WE'RE FINISHED                     
MISCDSPD EQU   X'01'               DISPLAYED MISC. ADDITIONS ALREADY            
RESETSTA EQU   ENDOFLST+FINISHED+RESET+DISPLYIT+PRDCHGED                        
*                                                                               
STATUS2  DS    CL1                                                              
ADDESTEL EQU   X'80'               NEED TO ADD LAST ESTIMATE ELEMENT            
HAVEREC  EQU   X'40'               WE HAVE PREVIOUS ESTIMATE RECORD             
REDISP   EQU   X'20'               RE-DISPLAY FROM THIS KEY                     
BUILDING EQU   X'10'               WE'RE BUILDING RECORD                        
KEYVALTD EQU   X'08'               VALKEY HAS BEED PERFORMED                    
WRITETOP EQU   X'04'               WRITE TOP OF SCREEN ELS. ONLY                
DOPPERF  EQU   X'02'               DISPLAYED OPPERF OR OPADDL OPTION            
HAVESC40 EQU   X'01'               HAVE TAAC EL FOR SCR40 (EST REPORT)          
*                                                                               
STATUS3  DS    CL1                                                              
CLIDELD  EQU   X'80'               CLI ALSO DELETED WHEN DELETING PRD           
LOOKCID  EQU   X'40'               LOOK UP COMMERCIAL VIA CID                   
LOOKSORT EQU   X'20'               LOOK UP PERFORMER VIA CAST SORT KEY          
LOOKHIN  EQU   X'10'               LOOK TO INSERT HYPO PRF AT END OF PG         
ADDACTV  EQU   X'08'               REPLACE ACTIVITY ELEMENT (TAACD)             
*                                                                               
PBUFF    DS    CL7                 BUFFER FOR PARTIAL LHS OF PRF LIST           
SVSTEREO DS    CL1                 Y=LAST TIME W/STEREO,N=W/O STEREO            
NEWEST   DS    CL1                 FLAG FOR NEW ESTIMATE DISPLAY                
WRKBYTE  DS    XL1                 WORKING BYTE                                 
CORPFLAG DS    XL1                 C=PERFORMER IS A CORP                        
SAVEFNUM DS    XL1                 FIELD NUMBER OF RHS OF BASE SCREEN           
ELTYPE   DS    XL1                 ESTIMATE ELEMENT TYPE                        
ELDATA   DS    CL(L'TAESDATA)                       DATA                        
HYCODATA DS    CL(L'TAESDATA)      HYPO COMML EL. DATA FOR LOOKUP               
HYPFDATA DS    CL(L'TAESDATA)      HYPO PERF EL. DATA FOR TRIM                  
TMPHYPCO DS    CL(L'TGCID)                                                      
ATHSEST  DS    A                   A(CURRENT EST. EL.)                          
ATHSTBL  DS    A                   A(ACTUAL SELECT TABLE)                       
ATHSSEL  DS    A                   A(SELECT TABLE ENTRY IN TEMP. BLOCK)         
SELCNT   DS    PL2                 N'NEW ENTRIES IN TABLE                       
ELSEQ    DS    XL1                 CURRENT EL. SEQUENCE NUMBER                  
LSTELSEQ DS    XL1                 LAST SEQ. NO. FOR INSERT                     
SUBELCNT DS    PL2                 N'SUB-ELEMENTS TO ADD                        
ASUBELS  DS    A                   A(SUB-ELEMENT BLOCK)                         
ANXTSUB  DS    A                   A(NEXT SUB-ELEMENT)                          
AUSESUB  DS    A                   A(USE SUB-ELEMENT)                           
ATHSSCRN DS    A                   A(THIS SCRNTAB ENTRY)                        
SCRNTAB  DS    CL(NLINEQU*SCRNLNQ) LIST OF ELEMENT DATA KEYS ON SCREEN          
*                                                                               
OPTWORK  EQU   *                   OPTION VALIDATION WORK AREAS                 
OPTLIST  DS    CL(NOPTS)           OPTIONS INPUT SO FAR                         
OPTLAST  DS    XL1                 LAST OPTION VALIDATED                        
OPTPEND  DS    XL1                 PENDING BITS FOR OPTIONS                     
OPTPUSES EQU   X'80'                   USES                                     
OPTPMAJ  EQU   X'40'                   MAJORS                                   
OPTPUNIT EQU   X'20'                   UNITS                                    
OPTPCYC  EQU   X'10'                   CYCLE DATES                              
OPTPAMT  EQU   X'08'                   AMOUNT                                   
OPTPGRT  EQU   X'04'                   GUARANTEE NUMBER                         
OPTPINS  EQU   X'02'                   INSERTS                                  
OPTPSTA  EQU   X'01'                   ADDENDUM STATE                           
OPTPEND2 DS    XL1                 MORE PENDING BITS FOR OPTIONS                
OPTPTAG  EQU   X'80'                   TAGS                                     
OPTPDEMO EQU   X'40'                   DEMOS                                    
OPTMAJOR DS    XL1                 MAJORS INPUT                                 
OPTUNITS DS    XL2                 UNITS INPUT                                  
OPTSSN   DS    CL9                 S/S NUMBER INPUT                             
OPTGRTNO DS    H                   GUARANTEE NUMBER INPUT                       
OPTSTATE DS    CL2                 ADDENDUM STATE                               
OPTWKLNQ EQU   *-OPTWORK                                                        
*                                                                               
OPTOKBIT DS    XL1                 OKAY BIT FOR CURRENT FIELD                   
OPOKPRD  EQU   X'80'                   PRODUCT LEVEL                            
OPOKCOM  EQU   X'40'                   COMMERCIAL                               
OPOKPER  EQU   X'20'                   PERFORMER                                
OPOKHYCO EQU   X'10'                   HYPOTHETICAL COMMERCIAL                  
OPOKHYPE EQU   X'08'                   HYPOTHETICAL PERFORMER                   
*                                                                               
OPUSLIST DS    CL(NUSOPTS)         OPTIONS INPUT SO FAR FOR USE                 
*                                                                               
FILTERS  DS    0CL(FILTEND-FILTER1) DISPLAY FILTERS                             
FILTER1  DS    XL1                                                              
FLACTV   EQU   X'80'               ACTIVITY DATE OVERRIDE                       
FLLINES  EQU   X'40'               N'LINES DISPLAYED PER RECORD                 
FLPERFS  EQU   X'20'               DISPLAY PERFORMER LEVEL                      
FLINCL   EQU   X'10'               DISPLAY NON-INCLUDED RECORDS                 
FLHYPOS  EQU   X'08'               DISPLAY HYPOTHETICAL COMMERCIALS             
FLALL    EQU   X'04'               WANT ALL INCLUDED                            
FLMEDIA  EQU   X'02'               FILTER BY MEDIA                              
FLSTART  EQU   X'01'               START COMMERCIAL                             
*                                                                               
FILTER2  DS    XL1                                                              
FLOKLOCK EQU   X'80'               DISPLAY LOCKED COMMERCIALS                   
FLOKREL  EQU   X'40'               DISPLAY RELEASED COMMERCIALS                 
FLCLINES EQU   X'20'               N'LINES DISPLAYED PER COMML RECORD           
FLPLINES EQU   X'10'               N'LINES DISPLAYED PER PERF. RECORD           
FLEXPIRY EQU   X'08'               DISPLAY EXPIRATION DATES                     
FLMUSIC  EQU   X'04'               EXCLUDE MUSIC COMMERCIALS                    
*                                                                               
FRESET   EQU   *                   * IF THE FOLLOWING CHANGE, RESET             
ACTVDATE DS    PL3                 PWOS LAST ACTIVE DATE FOR COMML LIST         
INCLUDED DS    CL1                 DISPLAY INCLUDED RECORDS                     
STARTCOM DS    CL(L'TLCOCID)       STARTING COMMERCIAL ID                       
STARTHYP DS    CL(L'TAESHCOM)      STARTING HYPOTHETICAL COMMERCIAL             
MEDIAFLT DS    CL2,CL2,CL2         MEDIA FILTER(S)                              
NMEDIA   EQU   (*-MEDIAFLT)/L'MEDIAFLT                                          
FRESETLQ EQU   *-FRESET            * END OF FILTERS WHICH CAUSE RESET           
*                                                                               
CLINES   DS    XL1                 N'LINES PER COMMERCIAL RECORD                
PLINES   DS    XL1                 N'LINES PER PERFORMER RECORD                 
PERFS    DS    CL1                 DISPLAY PERFORMERS                           
ALLYES   DS    CL1                 WANT ALL INCLUDED                            
LOCKED   DS    CL1                 DISPLAY LOCKED COMMERCIALS                   
RELEASED DS    CL1                 DISPLAY RELEASED COMMERCIALS                 
EXPIRY   DS    CL1                 DISPLAY EXPIRATION DATES                     
MUSIC    DS    CL1                 EXCLUDE MUSIC COMMERCIALS                    
FILTEND  EQU   *                   * END OF FILTERS                             
*                                                                               
ESTKEY   DS    CL32                ESTIMATE RECORD KEY                          
SAVECKEY DS    CL32                SAVED COMMERCIAL KEY                         
**SVNARR DS    CL63                SAVED NARRATIVE                              
SAVEHCOM DS    CL(L'TAESHCOM)      SAVED HYPOTHETICAL COMMERCIAL                
HCOMNAME DS    CL(L'HYPNAME)       HYPO COMMERCIAL NAME                         
THSLINES DS    XL1                 N'LINES USED FOR THIS RECORD                 
*                                                                               
NEXTS    DS    0CL12                                                            
NEXTCLI  DS    CL6                 NEXT CLIENT FOR DISPLAY                      
NEXTPRD  DS    CL6                 NEXT PRODUCT FOR DISPLAY                     
NXTCOMDA DS    XL4                 NEXT COMMERCIAL D/A                          
NXTCSTDA DS    XL4                 NEXT CAST D/A                                
*                                                                               
THSPGDAS DS    XL(NLINEQU*4)       D/A'S ON THIS PAGE                           
THSPGCOM DS    XL(NLINEQU*4)       INTERNAL COMM'L NUMBERS ON THIS PAGE         
THSPGSRT DS    XL(NLINEQU*6)       CAST SORT KEYS ON THIS PAGE                  
THSPGHPF DS    XL(NLINHEQU*7)      HYPO PERFORMER KEY SON THIS PAGE             
*                                                                               
*              THE FOLLOWING ARE USED FOR PAGING                                
*GLNQ    EQU   L'TAESHCOM          SET L'PAGE ENTRIES=L'HYPO COML CODE          
PGLNQ    EQU   L'TGCID             SET L'PAGE ENTRIES=L'HYPO COML CODE          
*                                                                               
PGCOMIND DS    XL1                 CURRENT SAVED COMML PAGE INDEX               
PGCOMTAB DS    (PGCOMMAX)XL(PGLNQ) SAVED D/A'S/HYP CODES FOR COMML PGS          
PGCOMMAX EQU   40                  MAX N'SAVED COMS IN PGCOMTAB                 
*                                                                               
PGCSTIND DS    XL1                 CURRENT SAVED CAST PAGE INDEX                
PGCSTTAB DS    (PGCSTMAX)XL(PGLNQ) SAVED D/A'S FOR CAST LIST PAGES              
PGCSTMAX EQU   30                  MAX N'SAVED CASTS IN PGCSTTAB                
*                                                                               
PGHYPIND DS    XL1                 CURRENT SAVED HYPO COMML PAGE INDEX          
PGHYPTAB DS    CL(PGHYPMAX*PGLNQ)  SAVED CODES FOR HYPO COML PAGES              
PGHYPMAX EQU   25                  MAX N'SAVED HYPO COMS IN PGHYPTAB            
*                                                                               
PGSCOIND DS    XL1                 CURRENT SAVED SEL COMML PAGE INDEX           
PGSCOTAB DS    CL(PGSCOMAX*PGLNQ)  SAVED CODES FOR SELECTED COMMERCIALS         
PGSCOMAX EQU   25                  MAX N'SAVED STER COMS IN PGSCOTAB            
*                                                                               
PGSPFIND DS    XL1                 CURRENT SAVED SEL PERF PAGE INDEX            
PGSPFTAB DS    CL(PGSPFMAX*PGLNQ)  SAVED CODES FOR SELECTED PERFORMERS          
PGSPFMAX EQU   25                  MAX N'SAVED STER PERFS IN PGSPFTAB           
*                                                                               
PGHPFIND DS    XL1                 CURRENT SAVED SEL HYPO PRF PG INDEX          
PGHPFTAB DS    CL(PGSPFMAX*PGLNQ)  SAVED CODES FOR HYPO PERFOMERS               
PGHPFMAX EQU   7                   MAX N'SAVED HYPO PERFOMERS                   
*                                                                               
*              THE FOLLOWING ARE FOR USER SELECTED INSTRUCTIONS                 
INSRTBIT DS    XL1                                                              
NSELS    EQU   35                   MAX N'SELECTED INSTRUCTIONS                 
CSELTAB  DS    CL(SELLNQ*NSELS)     SAVED COMM'L CODES FOR RHS REQUESTS         
CSELLAST EQU   *-SELLNQ                                                         
PSELTAB  DS    CL(SELLNQ*NSELS)     SAVED PERF CODES FOR RHS REQUESTS           
PSELLAST EQU   *-SELLNQ                                                         
*                                                                               
HYPRFPG  DS    XL1                 CURRENT HYPO PERFORMER PAGE                  
SHYPRFPG DS    XL1                 SAVED HYPO PERFORMER PAGE                    
*                                                                               
MYWORK   DS    CL20                MISC. WORKING AREA                           
*                                                                               
*              ALL ADDITIONS MUST BE BEFORE THIS POINT                          
*                                                                               
SAVTWA   DS    CL(1+(ESTWORK-ESTOVLYH)) SAVED SCREEN DURING HYPO INPUT          
MYSPARE  DS    CL(L'TWAHOLE-(*-ESTD))                                           
         EJECT                                                                  
*              DSECT TO COVER DISPLAY FILTER TABLE                              
         SPACE 1                                                                
FILTD    DSECT                                                                  
FILTEQU  DS    XL1                 FILTER VALUE EQUATE                          
FILTDSP  DS    AL1                 DISPLACEMENT TO THIS FILTER BYTE             
FILTRTN  DS    AL2                 DISPLACEMENT TO VALIDATION ROUTINE           
FILTLHS  DS    CL10                LHS CHARACTER INPUT                          
FILTNEXT EQU   *                                                                
         SPACE 3                                                                
*              DSECT TO COVER SELECT TABLES (CSELTAB AND PSELTAB)               
         SPACE 1                                                                
SELD     DSECT                                                                  
SELRHS   DS    XL1                 INSTRUCTION (BINARY NO. ==> INSERT)          
SELDA    DS    XL4                 D/A OF THIS RECORD                           
SELMEDIA DS    CL1                 MEDIA                                        
SELLNQ   EQU   *-SELD                                                           
SELNEXT  EQU   *                                                                
         SPACE 3                                                                
*              DSECT TO COVER OPTION TABLE                                      
         SPACE 1                                                                
OPTD     DSECT                                                                  
OPTCODE  DS    XL1                 EQUATE IDENTIFYING THIS OPTION               
OPTSUBLN DS    XL1                 L'SUB-ELEMENT DATA                           
OPTSTAT  DS    XL1                                                              
OPTNOLHS EQU   X'80'               NO LHS IN TABLE-USE I/P RTN TO CHECK         
OPTNORHS EQU   X'40'               INSURE NO RHS INPUT                          
OPTSVLST EQU   X'20'               SAVE OPTION EQUATE FOR SEQ. CHECKING         
OPTOKDIS EQU   X'10'               CHECK FOR THIS OPTION DURING DISPLAY         
OPTNOEL  EQU   X'08'               DON'T ADD SUB-ELEMENT FOR OPTION             
OPTFORUS EQU   X'04'               OPTLAST MUST BE A USE TYPE                   
OPTIGN   EQU   X'02'               IGNORE - NOT VALID IN OPTIONS FIELD          
OPTOK    DS    XL1                 OPTION OK FOR THIS LEVEL                     
OPTIDSP  DS    AL2                 DISPLACEMENT TO INPUT ROUTINE                
OPTODSP  DS    AL2                 DISPLACEMENT TO OUTPUT ROUTINE               
OPTNEXT  EQU   *                                                                
OPTLHS   DS    CL10                LHS OF OPTION  (OPTIONAL)                    
OPTNEXT2 EQU   *                                                                
         EJECT                                                                  
*              DSECT TO COVER SCREEN TABLE                                      
         SPACE 1                                                                
SCRND    DSECT                                                                  
SCRNCODE DS    XL1                 LHS CHARACTER FROM SCREEN                    
SCRNSTAT DS    XL1                 X'00'=DELETE ALL                             
SCRNOK   EQU   X'01'               THIS ELEMENT IS OK                           
SCRNOKPR EQU   X'02'               SUBSEQUENT PERFORMER ELS OK                  
SCRNOKHP EQU   X'04'               SUBSEQUENT HYPO. PERFORMER ELS OK            
SCRNALOK EQU   SCRNOK+SCRNOKPR+SCRNOKHP  DON'T NEED TO CHANGE ANYTHING          
SCRNCOPY DS    CL1                 LHS CHARACTER OF COPY SOURCE                 
SCRNDATA DS    CL(L'TAESDATA)      ELEMENT KEY DATA                             
SCRNLNQ  EQU   *-SCRND                                                          
SCRNNEXT EQU   *                                                                
         EJECT                                                                  
*              DSECT TO COVER PRIMARY SCREEN LINES                              
         SPACE 1                                                                
LINED    DSECT                                                                  
LINCODEH DS    CL8                                                              
LINCODE  DS    CL1                                                              
LINCODEX DS    CL8                                                              
LINDATAH DS    CL8                                                              
LINDATA  DS    CL(L'ESTLHS)                                                     
         ORG   LINDATA             COMMERCIAL DISPLAY                           
LINCID   DS    CL12                                                             
LINMOVE  DS    CL1                                                              
LINMEDIA DS    CL1                                                              
         DS    CL1                                                              
LINNAME  DS    CL23                                                             
         ORG   LINDATA             PERFORMER DISPLAY                            
LINSSN   DS    CL9                                                              
         DS    CL1                                                              
LINPNME  DS    CL12                                                             
         ORG   *-2                                                              
LINPLIFT DS    CL2                                                              
         DS    CL1                                                              
LINCAT   DS    CL3                                                              
         DS    CL1                                                              
LINDBL   DS    CL1                                                              
LINCAM   DS    CL1                                                              
LINW4TY  DS    CL1                                                              
         DS    CL1                                                              
LINOV    DS    CL7                                                              
         ORG                                                                    
LINRHSH  DS    CL8                                                              
LINRHS   DS    CL(L'ESTRHS)        INPUT AREA                                   
         ORG   LINRHS                                                           
*                                  BREAKOUT FOR LISTHYP W/STEREO ACTIVE         
LINHSEC  DS    CL3                 LENGTH                                       
         DS    CL1                                                              
LINHTYPE DS    CL7                 COMMERCIAL TYPE                              
         DS    CL3                                                              
LINHLSEC DS    CL2                 LIFTLEN (CENTERED)                           
         DS    CL21                                                             
         ORG                                                                    
LINRHSX  DS    CL8                                                              
LINLNQ   EQU   *-LINED                                                          
LINNEXT  EQU   *                                                                
         EJECT                                                                  
*              DSECT TO COVER COMMERCIAL INFO                                   
*              RIGHT HAND SIDE FOR LISTSCOM AND 2ND LINE FOR SELCTCOM           
         SPACE                                                                  
LINCOMD  DSECT                                                                  
LINCSEC  DS    CL3                 LENGTH                                       
         DS    CL1                                                              
LINCTYPE DS    CL7                 COMMERCIAL TYPE                              
         DS    CL1                                                              
LINCLID  DS    CL12                LIFT ID                                      
         DS    CL1                                                              
LINCLSEC DS    CL3                 LENGTH OF LIFT ID                            
LINCDTLS DS    CL1                                                              
LINCEXP  DS    CL8                                                              
         SPACE 2                                                                
*              DSECT TO COVER PERFORMER INFO                                    
*              RIGHT HAND SIDE FOR LISTSPRF AND 2ND LINE FOR SELCTPRF           
         SPACE                                                                  
LINPERFD DSECT                                                                  
LINPUNI  DS    CL3                 UNION                                        
         DS    CL1                                                              
LINPLOCL DS    CL3                 LOCAL                                        
         DS    CL1                                                              
LINPYEAR DS    CL3                 YEAR                                         
         DS    CL1                                                              
LINPLFT  DS    CL1                 LIFT                                         
         DS    CL1                                                              
LINPOVS  DS    CL7                                                              
         DS    CL1                                                              
LINPGUAR DS    CL4                 GUARANTEE                                    
         DS    CL1                                                              
LINPEXP  DS    CL8                 EXPIRATION DATE                              
         DS    CL1                                                              
LINPDTLS DS    CL1                 PERFORMER DETAILS                            
         EJECT                                                                  
*              DSECT TO COVER HYPOTHETICAL SCREEN LINES                         
         SPACE 1                                                                
LINHED   DSECT                                                                  
LINHCDH  DS    CL8                                                              
LINHCODE DS    CL1                                                              
LINHNUMH DS    CL8                                                              
LINHNUM  DS    CL3                 N'PERFORMERS                                 
LINHNUMX DS    CL8                                                              
LINHCATH DS    CL8                                                              
LINHCAT  DS    CL3                 CATEGORY                                     
LINHCATX DS    CL8                                                              
LINHCAMH DS    CL8                                                              
LINHCAM  DS    CL3                 ON/OFF CAMERA                                
LINHCAMX DS    CL8                                                              
LINHUNIH DS    CL8                                                              
LINHUNI  DS    CL3                 UNION                                        
LINHUNIX DS    CL8                                                              
LINHLFTH DS    CL8                                                              
LINHLFT  DS    CL2                 LIFT STATUS                                  
LINHLFTX DS    CL8                                                              
LINHDBLH DS    CL8                                                              
LINHDBL  DS    CL2                 N'DOUBLES                                    
LINHDBLX DS    CL8                                                              
LINHCRPH DS    CL8                                                              
LINHCRP  DS    CL1                 CORP STATUS                                  
LINHCRPX DS    CL8                                                              
LINHRHSH DS    CL8                                                              
LINHRHS  DS    CL(L'HYPRHS)        OPTIONS                                      
LINHRHSX DS    CL8                                                              
LINHLNQ  EQU   *-LINHED                                                         
LINHNEXT EQU   *                                                                
         EJECT                                                                  
*              DSECT TO COVER MISC ADDITION FIELDS                              
         SPACE 1                                                                
LINMD    DSECT                                                                  
LINMTAGH DS    CL8                                                              
LINMTAG  DS    CL1                 TAG                                          
LINMTXTH DS    CL8                                                              
LINMTXT  DS    CL(L'MSCTXT)        TEXT                                         
LINMTXTX DS    CL8                                                              
LINMAMTH DS    CL8                                                              
LINMAMT  DS    CL(L'MSCAMT)        AMOUNT                                       
LINMAMTX DS    CL8                                                              
LINMLNQ  EQU   *-LINMD                                                          
LINMNEXT EQU   *                                                                
         SPACE 3                                                                
*              DSECT TO COVER MISC ADDITION NARRATIVE LINES                     
         SPACE 1                                                                
LINND    DSECT                                                                  
LINNARRH DS    CL8                                                              
LINNARR  DS    CL(L'MSCNARR)                                                    
LINNARRX DS    CL8                                                              
LINNLNQ  EQU   *-LINND                                                          
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR2CD                                                       
         EJECT                                                                  
         ORG   ESTOVLYH                                                         
       ++INCLUDE TASCR0BD                                                       
         EJECT                                                                  
         ORG   ESTOVLYH                                                         
       ++INCLUDE TASCR0CD                                                       
         SPACE 2                                                                
HYPPAGE  EQU   HYPPGS+5                                                         
HYPPAGES EQU   HYPPGS+10                                                        
***********************  END  OF  SCREENS  ****************************         
         SPACE 2                                                                
*              ADDITIONAL SAVED STORAGE                                         
         SPACE 1                                                                
SUBELBLK DS    CL(254-TAESLNQ)                                                  
         SPACE 1                                                                
TWASPARE DS    0CL(CONHEADH+3520-*) INSURE NOT PAST END OF USER TWA0            
         EJECT                                                                  
* TASCR40D                                                                      
* DDGENTWA  (MUST FOLLOW LAST SCREEN)                                           
* TAGENWORKD                                                                    
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* DDSPLWORKD                                                                    
* TAGENFILE                                                                     
* TAGENEQUS                                                                     
* FATIOB                                                                        
         PRINT OFF                                                              
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR40D                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'099TAGEN2C   01/16/14'                                      
         END                                                                    
