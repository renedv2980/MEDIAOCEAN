*          DATA SET TAGENDF    AT LEVEL 108 AS OF 06/27/13                      
*PHASE T702DFE,*                                                                
         TITLE 'T702DF - STEREO DOWNLOAD/UPLOAD'                                
T702DF   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 SCRNELQ,T702DF,R7,RR=R2,CLEAR=YES                                
         SPACE 2                                                                
         LR    R3,RC                                                            
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8==A(SPOOL DSECT)                           
         USING SPOOLD,R8                                                        
         LA    R6,TWAHOLE          R6=A(LOCAL SAVED STORAGE)                    
         USING STEREOD,R6                                                       
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 3                                                                
         GOTO1 INITIAL,DMCB,0      INITIALIZE                                   
         SPACE 3                                                                
         ST    R2,RELO             RELOCATION FACTOR                            
         ST    R3,ASCRNEL          R3=A(SCRNEL)                                 
*&&DO                                                                           
         TM    PRGSTAT,TESTSYS     IF NOT THE TEST SYSTEM                       
         BO    *+12                                                             
         TM    TRNSTAT2,CTSTEREO   AND STEREO IS NOT ACTIVE                     
         BZ    ERRECINV            GIVE ERROR RECORD INVALID                    
*&&                                                                             
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,VALREC         BUILD RECORD                                 
         BNE   XIT                                                              
         CLI   ACTNUM,ACTDWN       IF DOWNLOADING                               
         BNE   *+12                                                             
         BAS   RE,DWNLOAD          DOWNLOAD RECORD(S) TO SCREEN                 
         B     XIT                                                              
*                                                                               
         BAS   RE,UPLOAD           ELSE, BUILD RECORD FROM SCREEN               
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY                                                     
         SPACE 1                                                                
VKEY     NTR1                                                                   
         BAS   RE,INITRTN          RELOCATE ROUTINES                            
         MVI   KEYCHG,C'N'         DEFAULT TO KEY NOT CHANGED                   
         SPACE 1                                                                
         TM    STESTAT2,STEUHMR    IF MIDDLE OF READING IN COMMLS               
         BZ    VKEY5                                                            
         TM    STEOPTH+4,X'20'     AND CHANGE TO OPTION OR CODE FIELD           
         BZ    *+12                                                             
         TM    STECDEH+4,X'20'                                                  
         BO    VKEY5                                                            
         NI    STESTAT2,X'FF'-STEUHMR RESET READ INDICATOR                      
         SPACE 1                                                                
VKEY5    BAS   RE,VKOPT            VALIDATE OPTION FIELD                        
         SPACE 1                                                                
         BAS   RE,VKCODE           VALIDATE CODE FIELD                          
         SPACE 1                                                                
         BAS   RE,VKKEYF           VALIDATE KEY FIELD                           
         SPACE 1                                                                
         CLI   KEYCHG,C'Y'         IF KEY CHANGED                               
         BNE   VKEY15                                                           
         ZIC   R2,MYKCODE          IF NO KEY EQUATE SET                         
         LTR   R2,R2                                                            
         BNZ   *+12                                                             
         BAS   RE,MYHIGH           BUILD KEY MYSELF                             
         B     VKEY20                                                           
         CLM   R2,1,=AL1(TLESCDQ)  IF ESTIMATE RECORD                           
         BNE   VKEY10                                                           
         TM    STOPT,STREST        AND WANT TO RESTORE IT                       
         BZ    VKEY10                                                           
         BAS   RE,RESTORE          RESTORE IT NOW THEN                          
VKEY10   GOTO1 RECVAL,DMCB,(R2),0  READ HIGH USING TGD FIELDS                   
         B     VKEY20                                                           
         SPACE 1                                                                
VKEY15   MVC   KEY,SMYKEY          ELSE, USE SAVED KEY                          
         GOTO1 HIGH                                                             
*                                                                               
VKEY20   CLI   ACTNUM,ACTDWN       IF DOWNLOADING                               
         BNE   VKEYX                                                            
         BAS   RE,CLCKEY           IF KEY NOT FOUND                             
         BE    VKEYX                                                            
         L     R5,AKEYTAB                                                       
         AH    R5,KEYDSP                                                        
         USING KEYTABD,R5          R5=A(CURRENT KEY ENTRY)                      
         CLC   =C'CP ',KEYCODE     AND CLIENT/PRODUCT READ                      
         BE    ERRCLI              (GIVE SPECIFIC ERROR)                        
         TM    KEYSTAT,KEYSL       OR READING A RECORD                          
         BZ    ERRNTFND            RECORD MUST EXIST                            
         XC    KEY,KEY                                                          
*                                                                               
VKEYX    B     XIT                                                              
         EJECT                                                                  
*              RELOCATE ROUTINES                                                
         SPACE 1                                                                
INITRTN  NTR1                                                                   
         LA    R1,RTNTAB           R1=A(ROUTINES TO RELOCATE)                   
         LA    R2,ARTNS            R2=A(ADDRESS OF ROUTINES)                    
         LA    R0,NRTNS            R0=(NUMBER OF ROUTINES)                      
*                                                                               
INITRTN2 L     RE,0(R1)                                                         
         A     RE,RELO                                                          
         ST    RE,0(R2)                                                         
         LA    R1,4(R1)            BUMP TO NEXT TABLE ENTRY                     
         LA    R2,4(R2)            BUMP TO NEXT ADDRESS                         
         BCT   R0,INITRTN2                                                      
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO CHECK KEY RETURNED AFTER HIGH                         
         SPACE 1                                                                
CLCKEY   DS    0H                                                               
         ZIC   RF,MYKLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BR    RE                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE OPTIONS FIELD                                
         SPACE 1                                                                
VKOPT    NTR1                                                                   
         LA    R2,STEOPTH          R2=A(OPTION FIELD)                           
         CLI   SCRSTAT,0           IF SCREEN CHANGED                            
         BNE   *+12                                                             
         TM    4(R2),X'20'         OR IF PREVIOUSLY VALIDATED                   
         BO    VKOPTX                                                           
*                                                                               
         MVI   KEYCHG,C'Y'         SET KEY CHANGED                              
         NI    STECDEH+4,X'DF'     SET NEXT FIELD NOT VALIDATED                 
*                                                                               
         MVI   STOPT,0             PRE CLEAR OPTION STATUS                      
         CLI   5(R2),0                                                          
         BE    VKOPTX                                                           
*                                                                               
         CLI   8(R2),C'E'          IF NOT EXPAND OPTION                         
         BNE   *+12                                                             
         OI    STOPT,STEXPAND                                                   
         B     VKOPT10                                                          
         CLI   8(R2),C'X'          IF DOWNLOAD WITH XTRA INFO                   
         BNE   *+12                                                             
         OI    STOPT,STEXTRA                                                    
         B     VKOPT10                                                          
         CLI   8(R2),C'R'          IF RESTORING ESTIMATE RECORD                 
         BNE   ERRINV                                                           
         OI    STOPT,STREST                                                     
*                                                                               
VKOPT10  MVI   STEOPT,0            CLEAR IT OUT                                 
         OI    STEOPTH+6,X'80'     FOR NEXT TIME AROUND                         
VKOPTX   OI    STEOPTH+4,X'20'     SET PREVIOUSLY VALIDATED                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES CODE FIELD                                     
*                                  XIT SETS MYKCODE                             
         SPACE 1                                                                
VKCODE   NTR1                                                                   
         LA    R2,STECDEH          R2=A(CODE FIELD)                             
         OC    STECDE,SPACES       PAD WITH SPACES                              
         TM    4(R2),X'20'         IF PREVIOUSLY VALIDATED                      
         BO    VKCODE2                                                          
*                                                                               
         MVI   KEYCHG,C'Y'         SET KEY CHANGED                              
         NI    STEKEYH+4,X'DF'     SET NEXT FIELD NOT VALIDATED                 
         GOTO1 ANY                 INPUT REQUIRED                               
*                                                                               
VKCODE2  L     R5,AKEYTAB          R5=A(KEY TABLE)                              
         USING KEYTABD,R5                                                       
VKCODE5  CLI   KEYCODE,X'FF'       TEST END OF TABLE                            
         BE    ERRINV                                                           
         CLC   KEYCODE,STECDE      IF MATCH ON CODE                             
         BNE   VKCODE8                                                          
         CLI   ACTNUM,ACTDWN       IF ACTION IS UPLOAD                          
         BE    VKCODE10                                                         
         TM    KEYSTAT,KEYSUP      AND KEY CAN BE UPLOADED                      
         BO    VKCODE10                                                         
         B     ERRINV                                                           
VKCODE8  BAS   RE,BMPKEYR5                                                      
         B     VKCODE5                                                          
         SPACE 1                                                                
VKCODE10 MVC   MYKCODE,KEYCDEQ     SAVE KEY EQUATE                              
VKCODEX  OI    STECDEH+4,X'20'                                                  
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY FIELDS                                   
*                                  XIT - KEYDSP AND 'TG' FIELDS SET             
         SPACE 1                                                                
VKKEYF   NTR1                                                                   
         LA    R2,STEKEYH          R2=A(KEY COMPONENTS FIELD)                   
         TM    4(R2),X'20'         IF NOT PREVIOUSLY VALIDATED                  
         BO    VKKEYFX                                                          
         MVI   KEYCHG,C'Y'         SET KEY CHANGED                              
         SPACE 1                                                                
         XC    FILTDATA,FILTDATA                                                
         BAS   RE,SETKDSP          SETS KEYDSP                                  
         L     R5,AKEYTAB                                                       
         AH    R5,KEYDSP                                                        
         USING KEYTABD,R5          R5=A(CURRENT KEY ENTRY)                      
         MVC   MYKLEN,KEYCMPLN     SET KEY COMPARE LENGTH                       
*                                                                               
         CLC   =C'INI',KEYCODE     IF NOT INITIALIZATION                        
         BE    VKKEYFX                                                          
         CLI   5(R2),0             ERROR IF NO INPUT                            
         BE    ERRINV                                                           
         BAS   RE,FORDAVID         CHANGE X'4F' TO X'6A' FOR DAVID              
         CLC   =C'UHM',KEYCODE     IF MULTIPLE USAGE HISTORY                    
         BNE   VKKEYF1                                                          
         BAS   RE,VKSCRN           KEY MAYBE WHOLE SCREEN                       
         B     VKKEYFX                                                          
*                                                                               
VKKEYF1  LA    R3,BLOCK            R3=A(SCAN BLOCK)                             
         USING SCAND,R3                                                         
         GOTO1 SCANNER,DMCB,(R2),(R3),C',=º='                                   
         ZIC   R0,4(R1)            R0=N'SCAN ENTRIES                            
         CLM   R0,1,KEYNFLDS                                                    
         BH    ERRINV              CAN'T HAVE MORE KEY FLDS                     
         SPACE 1                                                                
VKKEYF2  XR    RF,RF                                                            
         ICM   RF,3,KEYCLR         PASS CONTROL TO CLEAR TGD                    
         BZ    VKKEYF5                                                          
         AR    RF,RB                                                            
         LA    RE,VKKEYF5                                                       
         NTR1                                                                   
         BR    RF                                                               
         SPACE 1                                                                
VKKEYF5  LA    R5,KEYLNQ1(R5)      BUMP TO KEY FIELD ENTRIES                    
         USING KEYFLDS,R5                                                       
         SPACE 1                                                                
VKKEYF10 XR    RF,RF                                                            
         ICM   RF,3,KEYFDISP                                                    
         LA    R2,TGD                                                           
         TM    KEYFSTAT,KEYFLOCL                                                
         BZ    *+8                                                              
         LA    R2,FILTDATA                                                      
         AR    R2,RF               R2=A(FIELD TO SET)                           
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,KEYFRTN        RF=A(ROUTINE TO SET FIELD)                   
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    RF,RB                                                            
         LA    RE,VKKEYF30                                                      
         NTR1                                                                   
         BR    RF                                                               
         SPACE 1                                                                
VKKEYF30 CLI   SCLEN1,0            VALIDATE DATA IF ANY                         
         BE    VKKEYF35                                                         
         XR    RF,RF                                                            
         ICM   RF,3,KEYFVRTN                                                    
         BZ    VKKEYF35                                                         
         AR    RF,RB                                                            
         LA    RE,VKKEYF35                                                      
         NTR1                                                                   
         BR    RF                                                               
         SPACE 1                                                                
VKKEYF35 LA    R3,SCANNEXT         BUMP TO NEXT SCAN ENTRY                      
         LA    R5,KEYLNQ2(R5)      BUMP TO NEXT KEY FIELD ENTRY                 
         BCT   R0,VKKEYF10                                                      
         SPACE 1                                                                
VKKEYFX  OI    STEKEYH+4,X'20'                                                  
         B     XIT                                                              
         DROP  R5,R3                                                            
*              ROUTINE SETS KEYDSP - DISPLACEMENT INTO KEYTAB                   
         SPACE 1                                                                
SETKDSP  NTR1                                                                   
         L     R5,AKEYTAB          R5=A(KEY TABLE)                              
         USING KEYTABD,R5                                                       
*                                                                               
SETKDSP5 CLI   KEYCODE,X'FF'       TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                MUST BE THERE NOW                            
         CLC   KEYCODE,STECDE      IF MATCH ON CODE                             
         BE    *+12                                                             
         BAS   RE,BMPKEYR5                                                      
         B     SETKDSP5                                                         
         SPACE 1                                                                
         L     RE,AKEYTAB                                                       
         SR    R5,RE                                                            
         STH   R5,KEYDSP           SAVE DISPLACEMENT TO KEY ENTRY               
         B     XIT                                                              
         DROP  R5                                                               
         SPACE 2                                                                
*              JUST FOR DAVID CHANGE X'4F' TO X'6A'                             
         SPACE 2                                                                
FORDAVID NTR1                                                                   
         ZIC   R1,5(R2)            CHANGE ALL OCCURANCES OF X'4F' TO            
         LA    RE,8(R2)            X'6A' IN KEY FIELD INPUT                     
         SPACE 1                                                                
FORDAV5  CLI   0(RE),X'4F'                                                      
         BNE   *+8                                                              
         MVI   0(RE),X'6A'                                                      
         LA    RE,1(RE)                                                         
         BCT   R1,FORDAV5                                                       
         B     XIT                                                              
         EJECT                                                                  
*              KEY IS STRING OF INTERNAL COMMERCIAL NUMBERS EACH GETS           
*              CONVERTED AND STORED IN TABLE (MAYBE MULT. SCREENS)              
*                                  NTRY - R2=A(KEY FIELD)                       
VKSCRN   NTR1                                                                   
         BAS   RE,CLRSCREL                                                      
         L     R3,ASCRNEL                                                       
         LA    R5,MISCTAB2         SET A(TABLE) TO BEGINNING                    
         TM    STESTAT2,STEUHMR    IF FIRST TIME READ                           
         BO    VKSCRN2                                                          
         BAS   RE,CLRMISC2         CLEAR TABLE                                  
         OI    STESTAT2,STEUHMR    SET READING IN COMMLS                        
         B     VKSCRN4                                                          
*                                                                               
VKSCRN2  AH    R5,MISC2DSP         CONTINUE WHERE WE LEFT OFF                   
         MVC   0(L'PARTCOM,R3),PARTCOM   RESTORE PARTIAL COMML NUMBER           
VKSCRN3  CLI   0(R3),0                   TO SCREEN AREA                         
         BE    VKSCRN4                                                          
         LA    R3,1(R3)                                                         
         B     VKSCRN3                                                          
*                                                                               
VKSCRN4  ZIC   R1,5(R2)            MOVE DATA IN FROM KEY FIELD                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)                                                    
         LA    R1,1(R1)                                                         
         AR    R3,R1                                                            
         CH    R1,=AL2(STEKEYLQ)   IF DIDN'T USE UP WHOLE LINE                  
         BNE   VKSCRN6             CONSIDER US DONE                             
*                                                                               
         LA    R0,NLINES           R0=(MAX N'LINES)                             
         LA    R2,STERECH          R2=A(FIRST SCREEN LINE)                      
VKSCRN5  ZIC   R1,5(R2)            MOVE DATA FROM REST OF SCREEN                
         LTR   R1,R1                                                            
         BZ    VKSCRN6                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)                                                    
         LA    R1,1(R1)                                                         
         AR    R3,R1                                                            
         CH    R1,=AL2(LINLNQ)     IF DIDN'T USE UP WHOLE LINE                  
         BNE   VKSCRN6             CONSIDER US DONE                             
         ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         BCT   R0,VKSCRN5                                                       
*                                                                               
VKSCRN6  MVI   0(R3),X'00'         MARK END WITH BINARY ZERO                    
*                                                                               
         L     R3,ASCRNEL          R3=CONTINGOUS SCREEN DATA                    
VKSCRN8  BAS   RE,PARTIAL          IF FULL INTERNAL COMML NUMBER                
         BNE   VKSCRN9                                                          
         GOTO1 HEXIN,DMCB,(R3),(R5),8   CONVERT IT                              
         OC    12(4,R1),12(R1)                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R5,4(R5)            BUMP TO NEXT POSITION IN TABLE               
         LA    R3,8(R3)            BUMP TO NEXT INT COMML TO CONVERT            
         CLI   0(R3),X'40'         LOOP TILL END OF INPUT                       
         BH    VKSCRN8                                                          
*                                                                               
VKSCRN9  CLI   STECNT,C'Y'         IF MORE TO COME                              
         BE    *+12                                                             
         NI    STESTAT2,X'FF'-STEUHMR                                           
         B     XIT                                                              
         OI    STESTAT2,STEUHMR    SET INDICATOR READING COMMLS                 
         LA    RE,MISCTAB2                                                      
         SR    R5,RE                                                            
         STH   R5,MISC2DSP         SAVE DISPLACEMENT INTO TABLE                 
         B     DONE                AND EXIT TO USER                             
         SPACE 2                                                                
PARTIAL  NTR1                                                                   
         LR    RE,R3                                                            
         LA    R0,8                                                             
PARTIAL5 CLI   0(RE),0                                                          
         BNE   *+14                                                             
         MVC   PARTCOM,0(R3)       SAVE OFF PARTIAL COMML NUMBER                
         B     NO                  AND RETURN CC NOT EQUAL                      
         LA    RE,1(RE)                                                         
         BCT   R0,PARTIAL5                                                      
         B     YES                 FULL INTERNAL COMML NUMBER FOUND             
         EJECT                                                                  
*              ROUTINE TO BUILD AND READ HIGH FOR COMML KEYS ON COL             
*              DOWNLOAD, USAGE HISTORY KEYS ON UHL & UHM DOWNLOAD, AND          
*              POSSIBLE AGENCY KEYS ON INI DOWNLOAD                             
         SPACE 1                                                                
MYHIGH   NTR1                                                                   
         LA    R4,KEY              R4=A(KEY TO BUILD)                           
         XC    KEY,KEY                                                          
         XC    KEYSAVE,KEYSAVE                                                  
         L     R5,AKEYTAB                                                       
         AH    R5,KEYDSP                                                        
         USING KEYTABD,R5          R5=A(CURRENT KEY ENTRY)                      
         CLC   =C'INI',KEYCODE     IF INITIALZATION                             
         BNE   MYHIGH1                                                          
*                                                                               
         GOTO1 RECVAL,DMCB,TLSTCDQ,(X'A0',0),0                                  
         BNE   XIT                                                              
*                                                                               
         USING TAVAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAVAELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         OC    TAVAAGY,TAVAAGY     IF STAFF HAS AGENCY LIMITS                   
         BZ    XIT                                                              
         MVC   TGAGY,TAVAAGY       SET FIRST LIMITED AGENCY                     
         MVC   FRSTAGY,TAVAAGY                                                  
         XC    LASTAGY,LASTAGY                                                  
         DROP  R4                                                               
*                                                                               
         GOTO1 RECVAL,DMCB,TLAYCDQ,0                                            
         B     XIT                                                              
*                                                                               
         USING TLUHD,R4                                                         
MYHIGH1  CLC   =C'UHM',KEYCODE     IF MULTIPLE USAGE HISTORY LIST               
         BNE   MYHIGH2                                                          
         LA    R5,MISCTAB2                                                      
         MVC   TGCOM,0(R5)         SHOULD BE AT LEAST ONE                       
         MVC   MISC2DSP,=H'4'                                                   
         OI    STESTAT2,STECOM     SHOW EL=INT. COMML #                         
         B     MYHIGH3                                                          
*                                                                               
MYHIGH2  CLC   =C'UHL',KEYCODE     IF USAGE HISTORY LIST                        
         BNE   MYHIGH4                                                          
MYHIGH3  MVI   MYKCODE,TLUHCDQ                                                  
         MVI   TLUHCD,TLUHCDQ                                                   
         MVC   TLUHCOM,TGCOM                                                    
         MVC   MYKLEN,LUHCOM                                                    
         B     MYHIGHX                                                          
*                                                                               
MYHIGH4  CLI   FILTKEYQ,C'C'       IF COMML LIST BY CODE                        
         BNE   MYHIGH5                                                          
*                                                                               
         CLC   FILTCLG,SPACES      AND CLIENT GROUP SPECIFIED                   
         BNH   *+12                                                             
         BAS   RE,MYHICOG          READ TLCOGCDQ                                
         B     *+8                                                              
         BAS   RE,MYHICO           ELSE READ TLCOCDQ                            
         B     MYHIGHX                                                          
*                                                                               
MYHIGH5  DS    0H                  ASSUME BY TITLE                              
         CLC   FILTCLG,SPACES      IF CLIENT GROUP SPECIFIED                    
         BNH   *+12                                                             
         BAS   RE,MYHICOL          READ TLCOLCDQ                                
         B     *+8                                                              
         BAS   RE,MYHICON          ELSE READ TLCONCDQ                           
*                                                                               
MYHIGHX  GOTO1 HIGH                                                             
         B     XIT                                                              
         DROP  R5                                                               
         SPACE 2                                                                
*        ROUTINE TO READ HIGH FOR ACTIVE COMML PTR                              
         SPACE 1                                                                
         USING TLCOD,R4                                                         
MYHICO   NTR1                                                                   
         MVI   MYKCODE,TLCOCDQ     SAVE KEY READING                             
         MVI   TLCOCD,TLCOCDQ                                                   
         MVC   TLCOAGY,FILTAGY     SET AGENCY                                   
         MVC   TLCOCLI,FILTCLI     SET CLIENT                                   
         MVC   MYKLEN,LCOCLI       MINIMUM KEY EXPECTED                         
         CLC   FILTSTRT,SPACES                                                  
         BNH   *+10                                                             
         MVC   TLCOCID,FILTSTRT                                                 
         CLC   FILTPRD,SPACES      IF PRODUCT                                   
         BNH   XIT                                                              
         MVC   TLCOPRD,FILTPRD     SET IT                                       
         MVC   MYKLEN,LCOPRD                                                    
         B     XIT                                                              
         SPACE 2                                                                
*        ROUTINE TO READ HIGH FOR CLIENT GROUP COMML PTR                        
         SPACE 1                                                                
         USING TLCOPD,R4                                                        
MYHICOG  NTR1                                                                   
         MVI   MYKCODE,TLCOGCDQ                                                 
         MVI   TLCOPCD,TLCOGCDQ                                                 
         MVC   TLCOGCLG,FILTCLG    SET CLIENT GROUP                             
         MVC   MYKLEN,LCOGCLG                                                   
         CLC   FILTSTRT,SPACES                                                  
         BNH   *+10                                                             
         MVC   TLCOGCID,FILTSTRT                                                
         CLC   FILTAGY,SPACES      IF AGENCY SPECIFIED                          
         BNH   XIT                                                              
         MVC   TLCOGAGY,FILTAGY    SET IT                                       
         B     XIT                                                              
         EJECT                                                                  
*        ROUTINE TO READ HIGH FOR COMML NAME PTR                                
         SPACE 1                                                                
         USING TLCOPD,R4                                                        
MYHICON  NTR1                                                                   
         MVI   MYKCODE,TLCONCDQ                                                 
         MVI   TLCOPCD,TLCONCDQ                                                 
         MVC   TLCONCLI,FILTCLI    SET CLIENT                                   
         MVC   MYKLEN,LCONCLI      MINIMUM KEY EXPECTED                         
         CLC   FILTSTRT,SPACES                                                  
         BNH   *+10                                                             
         MVC   TLCONAME,FILTSTRT                                                
         B     XIT                                                              
         SPACE 2                                                                
*        ROUTINE TO READ HIGH FOR COMML CLIENT GROUP NAME PTR                   
         SPACE 1                                                                
         USING TLCOPD,R4                                                        
MYHICOL  NTR1                                                                   
         MVI   MYKCODE,TLCOLCDQ                                                 
         MVI   TLCOPCD,TLCOLCDQ                                                 
         MVC   TLCOLCLG,FILTCLG    SET CLIENT GROUP                             
         MVC   MYKLEN,LCOLCLG      MINIMUM KEY EXPECTED                         
         CLC   FILTSTRT,SPACES                                                  
         BNH   *+10                                                             
         MVC   TLCOLNAM,FILTSTRT                                                
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO RESTORE ESTIMATE RECORD(S)                            
         SPACE 1                                                                
RESTORE  NTR1                                                                   
         MVI   DMINBTS,X'08'       SET READ HIGH FOR DELETED                    
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 RECVAL,DMCB,TLESCDQ,0                                            
         B     REST10                                                           
*                                                                               
REST5    GOTO1 HIGH                RE-READ RECORD JUST RESTORED                 
         MVI   DMINBTS,X'08'       SET READ FOR DELETED                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 SEQ                 CHECK FOR ANOTHER RECORD                     
*                                                                               
REST10   CLC   KEY(TLESSEQ-TLESD),KEYSAVE                                       
         BNE   RESTX                                                            
*                                                                               
         MVI   DMINBTS,X'08'       SET READ FOR DELETED                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING TLESD,R4                                                         
         NI    TLESSTAT,X'7F'                                                   
         GOTO1 PUTREC              UNMARK RECORD AND WRITE IT BACK              
         NI    KEY+TLDRSTAT-TLDRD,X'7F'                                         
         GOTO1 WRITE               UNMARK DIRECTORY AND WRITE IT BACK           
         MVI   ELCODE,TAACELQ                                                   
         BAS   RE,GETEL            IF FIND ACTIVITY ELEMENT                     
         BE    RESTX               THEN DONE WITH RESTORE                       
         B     REST5               ELSE, KEEP LOOKING FOR MORE RECS             
*                                                                               
RESTX    NI    DMINBTS,X'F7'                                                    
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO DOWNLOAD RECORD(S) TO SCREEN                          
         SPACE 1                                                                
DWNLOAD  NTR1                                                                   
         BAS   RE,INITDWN          INIT. SCREEN, FLAGS & CURRENT VALUES         
*                                                                               
         CLI   KEYCHG,C'Y'         IF NO KEY CHANGE                             
         BE    DWNLD6                                                           
         TM    STESTAT,STETAB      IF PROCESSING TABLE                          
         BO    DWNLD15                                                          
         BAS   RE,CHKCHG           AND RECORD HASN'T CHANGED                    
         BL    DWNLDX              ERROR NOTHING MATCHES REQUEST                
         BE    DWNLD8              CONTINUE WHERE WE LEFT OFF => R4             
*                                  OR START AT BEGINNING                        
DWNLD6   MVC   SMYKEY,KEY          SAVE CURRENT KEY                             
         BAS   RE,FILTKEY          FILTER KEY                                   
         BNE   DWNLD10                                                          
         BAS   RE,GETDWNRC         GET RECORD AND RETURN R4=A(ELEMENT)          
DWNLD8   CLI   0(R4),0             IF ELEMENT TO PROCESS                        
         BE    DWNLD10                                                          
*                                                                               
         GOTO1 AFILTREC,DMCB,(RC)  SET GLOBALS & FILTER RECORD                  
         BNE   DWNLD10                                                          
         MVC   SVAIO,AIO1          SET SAVED IOAREA                             
         BAS   RE,PROCREC          DISPLAY ELEMENTS STARTING AT R4              
         BNE   DWNLDX              (NO MORE ROOM ON SCREEN)                     
*                                                                               
DWNLD10  BAS   RE,NEXTKEY          DO A SEQUENTIAL ON THE DIRECTORY             
         BE    DWNLD6              LOOP TO PROCESS                              
*                                                                               
DWNLD15  BAS   RE,PROCTAB          PROCESS TABLE PT'D TO BY R4                  
         BNE   DWNLDX                                                           
         MVI   CONTFLAG,C' '       SET ON LAST SCREEN                           
         SPACE 1                                                                
DWNLDX   MVC   STECNT,CONTFLAG     SET CONTINUE FLAG TO SCREEN                  
         OI    STECNTH+6,X'80'                                                  
         MVC   STEOVR,OVRFLAG      SET OVER FLAG TO SCREEN                      
         OI    STEOVRH+6,X'80'                                                  
         BAS   RE,SVRCVALS         SAVE NECESSARY INFORMATION FOR NEXT          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE INITIALIZES SCREEN AND FLAGS                             
         SPACE 1                                                                
INITDWN  NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(X'23',STERECH),999   VAL/CLR/XMIT SCREEN            
         LA    R3,STEREC           R3=A(FIRST DATA LINE)                        
         ST    R3,ASCRPOS          SAVE A(SCREEN POSITION)                      
         MVI   CONTFLAG,C'Y'       SET CONTINUE AS DEFAULT                      
         MVI   CURLINE,1           SET SCREEN NUMBER                            
         MVC   SCRLLEN,=AL2(LINLNQ) & INITIALIZE L'SCREEN LINE LEFT             
         SPACE 1                                                                
INITDWN2 CLI   KEYCHG,C'Y'         IF NO KEY CHANGE                             
         BE    INITDWN5                                                         
         MVI   OVRFLAG,C' '        SET NOT STARTING OVER AS DEFAULT             
         TM    STESTAT,STEWTWA     IF SAVED SCRNEL IN TWA4                      
         BZ    INITDWNX                                                         
         GOTO1 GETTWA,DMCB,(X'14',ATIA) READ DATA FROM TWA4 TO TIA              
         L     RE,ATIA             RE=A(DATA TO MOVE FROM)                      
         L     R0,ASCRNEL          R0=A(DATA TO MOVE TO)                        
         LH    R1,=Y(SCRNELQ)      R1 AND RF = LENGTH TO MOVE                   
         LR    RF,R1                                                            
         MVCL  R0,RE               RESTORE DATA TO SCRNEL                       
         NI    STESTAT,X'FF'-STEWTWA                                            
         B     INITDWNX                                                         
*                                                                               
INITDWN5 MVC   RECDSP,DATADISP     DISPLACEMENT TO FIRST ELEMENT                
         XC    SCRELDSP,SCRELDSP   CLEAR DISPLACEMENT INTO SCRNEL               
         BAS   RE,CLRSCREL         CLEAR SCRNEL                                 
         BAS   RE,CLRMISC          CLEAR MISCTAB                                
         MVI   STESTAT,0           CLEAR PROCESSING STATUS                      
         XC    SVLSTINF,SVLSTINF   CLEAR SPECIFIC REC INFO FOR LISTING          
         XC    SVCODES,SVCODES     CLEAR SV CODES CAL/COL OPTIMIZATION          
         MVC   SVPRD,HEXFFS                                                     
         MVI   OVRFLAG,C'O'        SET STARTING OVER FOR FIRST TIME             
         MVC   FRSTKEY,KEY         SAVE FIRST KEY                               
         XC    FRSTKEYA,FRSTKEYA   AND CLR FIRST KEY OF SECONDARY READ          
         L     R5,AKEYTAB                                                       
         AH    R5,KEYDSP                                                        
         USING KEYTABD,R5          R5=A(CURRENT KEY ENTRY)                      
         CLC   =C'INI',KEYCODE     IF INITIALIZATION                            
         BNE   INITDWNX                                                         
         XR    R4,R4               INDICATE NO RECORD                           
         GOTO1 AADDTAB,DMCB,(RC)   ADD INFO TO MISCTAB                          
*                                                                               
INITDWNX B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*              ROUTINE TO CHECK IF RECORD CHANGED BETWEEN                       
*              DOWNLOADING HITS OF ENTER                                        
         SPACE 1                                                                
CHKCHG   NTR1                                                                   
         CLI   KEY,TLCAGCDQ        IF KEY IS CAST GRT KEY                       
         BNE   CHKCHG2                                                          
         OI    STESTAT,STERERD     SET REREAD STATUS                            
         MVC   TGCOM,KEY+TLCAGCOM-TLCAPD                                        
         GOTO1 RECVAL,DMCB,TLCOCCDQ,0  PROCESS COMMERCIAL RECORD                
*                                                                               
CHKCHG2  BAS   RE,GETDWNRC         GET RECORD PARTIALLY DOWNLOADED              
*                                                                               
         OC    SVTIM,SVTIM         IF NO TIME SAVED FROM PREV. TRANS            
         BNZ   *+14                                                             
         MVC   RECDSP,DATADISP     START AT BEGINNING                           
         B     CHKCHG8                                                          
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TAACELQ      GET ACTIVITY ELEMENT TO CHK CHG              
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CHKCHG5  BAS   RE,NEXTEL                                                        
         BNE   CHKCHG8                                                          
         USING TAACD,R4                                                         
*                                                                               
         CLI   TAACSCR,0           SKIP ELEMENTS WITH SCREEN NUMBER             
         BNE   CHKCHG5                                                          
         CLC   TAACCDTE,TGTODAY1   IF REC CHGD SINCE PREV. HIT OF ENTER         
         BNE   CHKCHG5                                                          
         CLC   TAACCTIM,SVTIM                                                   
         BL    CHKCHG5                                                          
*                                                                               
         MVI   OVRFLAG,C'O'        SET STARTING OVER FLAG                       
         MVC   KEY,FRSTKEY         TRY TO START OVER                            
         GOTO1 HIGH                                                             
         BAS   RE,CLCKEY           CHECK KEY RETURNED                           
         BNE   LOWXIT                                                           
         B     CHKCHGNO                                                         
*                                                                               
CHKCHG8  L     R4,AIO                                                           
         AH    R4,RECDSP                                                        
         MVC   RECDSP,DATADISP                                                  
         B     YESXITR4            RETURN CC EQUAL WITH R4 SET                  
*                                                                               
CHKCHGNO MVC   RECDSP,DATADISP                                                  
         XC    SCRELDSP,SCRELDSP   CLEAR DISPLACEMENT INTO SCRNEL               
         MVI   STESTAT,0           CLEAR PROCESSING STATUS                      
         XC    SVLSTINF,SVLSTINF   CLEAR SPECIFIC REC INFO FOR LISTING          
         XC    SVCODES,SVCODES     CLEAR SV CODES CAL/COL OPTIMIZATION          
         MVC   SVPRD,HEXFFS                                                     
         BAS   RE,CLRSCREL         CLEAR SCRNEL                                 
         BAS   RE,CLRMISC          CLEAR MISCTAB                                
         B     NO                  START OVER                                   
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO FILTER AT DIRECTORY KEY LEVEL                         
         SPACE 1                                                                
FILTKEY  NTR1                                                                   
         CLI   KEY,TLCACDQ         IF CAST KEY                                  
         BNE   FILTK10                                                          
         TM    KEY+TLCASORT-TLCAD,X'20'                                         
         BO    NO                  SKIP EXTRAS                                  
         B     YES                                                              
*                                                                               
FILTK10  CLI   KEY,TLCAGCDQ        IF READING THROUGH GUAR FOR CAST             
         BNE   FILTK20                                                          
         OI    STESTAT,STERERD     SET REREAD STATUS                            
         MVC   TGCOM,KEY+TLCAGCOM-TLCAPD                                        
         GOTO1 RECVAL,DMCB,TLCOCCDQ,0  PROCESS COMMERCIAL RECORD                
         B     XIT                 CC SET                                       
*                                                                               
FILTK20  CLI   KEY,TLCOCDQ         IF READING COMMLS                            
         BNE   FILTK30                                                          
         TM    KEY+TLDRSTAT-TLDRD,X'40'                                         
         BO    NO                                                               
         CLC   FILTPRD,SPACES                                                   
         BNH   *+14                                                             
         CLC   FILTPRD,KEY+TLCOPRD-TLCOD                                        
         BNE   NO                                                               
         B     YES                 CC SET                                       
*                                                                               
FILTK30  CLI   KEY,TLCONCDQ        IF READING COMMLS                            
         BNE   YES                                                              
         TM    KEY+TLDRSTAT-TLDRD,X'40'                                         
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO SAVE CURRENT KEY AND GET RECORD                       
*              INTO AIO AND RETURN R4=A(FIRST/NEXT ELEMENT)                     
         SPACE 1                                                                
GETDWNRC NTR1                                                                   
         OC    KEY,KEY             IF NO KEY                                    
         BNZ   GETDWNR2                                                         
         L     R4,AIO                                                           
         XC    0(10,R4),0(R4)      CLEAR BEGINNING                              
         B     GETDWNRX                                                         
*                                                                               
GETDWNR2 CLI   KEY,TLESCDQ         IF ESTIMATE                                  
         BNE   GETDWNR4                                                         
         GOTO1 AGETEREC,DMCB,(RC)  GET WHOLE REC INTO AIO (FOR 1,2,&3)          
         B     GETDWNR5                                                         
GETDWNR4 GOTO1 GETREC              ELSE, JUST DO REGULAR GET INTO IO1           
*                                                                               
GETDWNR5 L     R4,AIO                                                           
         AH    R4,RECDSP                                                        
*                                                                               
GETDWNRX XIT1  REGS=(R4)                                                        
         EJECT                                                                  
*              ROUTINE TO DO SEQUENTIAL READ IN LIST SITUATION                  
         SPACE 1                                                                
NEXTKEY  NTR1                                                                   
         L     R5,AKEYTAB                                                       
         AH    R5,KEYDSP                                                        
         USING KEYTABD,R5          R5=A(CURRENT KEY ENTRY)                      
*                                                                               
         TM    KEYSTAT,KEYSL       IF NOT LISTING RECORDS                       
         BO    *+12                                                             
         BAS   RE,NXTNSEQ          CHECK FOR SECOND NON-SEQUENTIAL READ         
         B     XIT                 CC SET                                       
*                                                                               
         TM    STESTAT,STERERD     IF NEED TO REREAD KEY                        
         BZ    NEXTKEY5                                                         
         NI    STESTAT,X'FF'-STERERD SET TO REREAD DIRECTORY POINTER            
         XC    KEY,KEY             RE-ESTABLISH DIRECTORY KEY                   
         MVC   KEY,SMYKEY                                                       
         CLI   KEY,TLCAGCDQ        IF READING FOR COMML FOR SSN/GUAR            
         BNE   NEXTKEY4                                                         
         MVC   KEY+TLCAGCAT-TLCAPD(L'TLCAGCAT),=4X'FF'                          
         GOTO1 HIGH                                                             
         CLC   KEY(TLCAGCOM-TLCAPD),KEYSAVE                                     
         B     XIT                                                              
*                                                                               
NEXTKEY4 GOTO1 HIGH                                                             
NEXTKEY5 GOTO1 SEQ                                                              
*                                                                               
         LA    RE,FRSTKEY                                                       
         OC    FRSTKEYA,FRSTKEYA   IF FIRST KEY OF SECOND READ SET              
         BZ    *+8                                                              
         LA    RE,FRSTKEYA         USE IT                                       
*                                                                               
         ZIC   RF,MYKLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),0(RE)        IF STILL GOOD                                
         BNE   NEXTKEYN                                                         
*                                                                               
         CLI   KEY,TLESCDQ         IF READING ESTIMATE RECORDS                  
         BNE   *+12                                                             
         CLI   KEY+TLESSEQ-TLESD,0 SKIP NON-BASE RECORDS                        
         BNE   NEXTKEY5                                                         
*                                                                               
         CLI   KEY,TLUHCDQ         AND USAGE HISTORY                            
         BNE   YES                                                              
         OC    KEY+TLUHCSEQ-TLUHD(L'TLUHCSEQ),KEY+TLUHCSEQ-TLUHD                
         BZ    YES                                                              
*                                                                               
NEXTKEYN CLC   =C'UHM',KEYCODE     OR IT IS A UHM REQUEST                       
         BNE   NO                                                               
         CLI   KEYSAVE,TLCACDQ     IF JUST FINISHED CAST RECORDS                
         BE    NEXTKEY8            PROCESS NEXT COMMERCIAL                      
NEXTKEY7 CLI   KEYSAVE,TLUHCDQ     IF JUST FINISHED USAGE HISTORY REC'S         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   MYKCODE,TLCACDQ     READ CAST                                    
         MVC   MYKLEN,LCACOM                                                    
         XC    KEY,KEY                                                          
         MVI   KEY,TLCACDQ                                                      
         MVC   KEY+TLCACOM-TLCAD(L'TGCOM),TGCOM                                 
         GOTO1 HIGH                                                             
         CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         BNE   NEXTKEY8                                                         
         MVC   FRSTKEYA,KEY                                                     
         B     YES                                                              
NEXTKEY8 LA    R5,MISCTAB2                                                      
         AH    R5,MISC2DSP                                                      
         OC    0(4,R5),0(R5)                                                    
         BNZ   *+12                                                             
         NI    STESTAT2,X'FF'-STECOM                                            
         B     NO                                                               
         MVC   TGCOM,0(R5)                                                      
         LH    R5,MISC2DSP                                                      
         LA    R5,4(R5)                                                         
         STH   R5,MISC2DSP                                                      
         OI    STESTAT2,STECOM     SHOW EL=INT. COMML #                         
         MVI   MYKCODE,TLUHCDQ                                                  
         MVC   MYKLEN,LUHCOM                                                    
         XC    KEY,KEY                                                          
         MVI   KEY,TLUHCDQ                                                      
         MVC   KEY+TLUHCOM-TLUHD(L'TGCOM),TGCOM                                 
         GOTO1 HIGH                READ HIGH FOR TLUHD KEY                      
         CLC   KEY(TLUHCSEQ-TLUHD),KEYSAVE  IF NONE                             
         BNE   NEXTKEY7            TRY FOR CAST                                 
         MVC   FRSTKEYA,KEY        ELSE, SAVE FIRST KEY                         
         B     YES                 AND PROCESS                                  
         EJECT                                                                  
*              ROUTINE SEES IF NEED TO READ DIFFERENT RECORD                    
         SPACE 1                                                                
NXTNSEQ  NTR1                                                                   
         CLC   =C'GU ',KEYCODE     BUT IT IS A GU REQUEST                       
         BNE   NXTNSEQ2                                                         
         MVC   KEYDSP,=AL2(KEYSCAG-KEYTAB)   RESET TO CAG                       
         MVI   MYKLEN,TLCAGCOM-TLCAPD-1                                         
         XC    TGCAT,TGCAT                                                      
         XC    TGCSORT,TGCSORT                                                  
         GOTO1 RECVAL,DMCB,TLCAGCDQ,0  READ HIGH FOR TLCAGD KEY                 
         CLC   KEY(TLCAGCOM-TLCAPD),KEYSAVE                                     
         B     XIT                                                              
*                                                                               
NXTNSEQ2 CLC   =C'CP',KEYCODE      OR IT IS A CP (CLI THEN PRD) REQUEST         
         BNE   NXTNSEQ4                                                         
         OC    TGPRD,TGPRD         AND PRODUCT REQUESTED                        
         BZ    NO                                                               
         GOTO1 RECVAL,DMCB,TLPRCDQ,0  READ HIGH FOR TLPRD KEY                   
         CLC   KEY(L'TLDRKEY),KEYSAVE                                           
         BNE   ERRPRD                                                           
         XC    TGPRD,TGPRD         CLEAR SO DON'T LOOP                          
         B     YES                                                              
*                                                                               
NXTNSEQ4 CLC   =C'COH',KEYCODE     OR IT IS A COH REQUEST                       
         BNE   NXTNSEQ6                                                         
         MVC   KEYDSP,=AL2(KEYSUHL-KEYTAB)   RESET TO UHL                       
         MVI   MYKLEN,TLUHCSEQ-TLUHD-1                                          
         XC    KEY,KEY                                                          
         MVI   KEY,TLUHCDQ                                                      
         MVC   KEY+TLUHCOM-TLUHD(L'TGCOM),TGCOM                                 
         GOTO1 HIGH                READ HIGH FOR TLUHD KEY                      
         MVC   FRSTKEYA,KEY                                                     
         CLC   KEY(TLUHCSEQ-TLUHD),KEYSAVE                                      
         B     XIT                                                              
*                                                                               
NXTNSEQ6 CLC   =C'GUL',KEYCODE      OR IT IS A GUL REQUEST                      
         BNE   NXTNSEQ8                                                         
         MVC   KEYDSP,=AL2(KEYSGUL-KEYTAB) RESET TO REAL GUL KEY                
         MVI   MYKLEN,TLGUGUA-TLGUD-1                                           
         GOTO1 RECVAL,DMCB,TLGUCDQ,0  READ HIGH FOR TLGUD KEY                   
         MVC   FRSTKEYA,KEY                                                     
         CLC   KEY(TLGUGUA-TLGUD),KEYSAVE                                       
         B     XIT                                                              
*                                                                               
NXTNSEQ8 CLC   =C'INI',KEYCODE      OR IT IS AN INI REQUEST                     
         BNE   NO                                                               
*                                                                               
         XR    R2,R2                                                            
*                                                                               
         GOTO1 RECVAL,DMCB,TLSTCDQ,(X'C0',0),0                                  
         GOTO1 HIGH                                                             
         B     NXTNSEQA                                                         
NXTNSEQ9 GOTO1 SEQ                                                              
NXTNSEQA CLC   KEY(TLSSPR3-TLSTD),KEYSAVE                                       
         BNE   NO                                                               
         GOTO1 GETREC                                                           
*                                                                               
         USING TAVAD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAVAELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
NXTNSEQB BAS   RE,NEXTEL                                                        
         BNE   NXTNSEQ9                                                         
         OC    TAVAAGY,TAVAAGY     IF STAFF HAS AGENCY LIMITS                   
         BZ    NO                                                               
         CLC   TAVAAGY,FRSTAGY                                                  
         BE    NXTNSEQB                                                         
         CLC   TAVAAGY,LASTAGY     BEEN PROCESSED                               
         BNH   NXTNSEQB                                                         
         MVC   TGAGY,TAVAAGY       AND GO READ AGENCY RECORD                    
         MVC   LASTAGY,TAVAAGY                                                  
         GOTO1 RECVAL,DMCB,TLAYCDQ,0                                            
         BNE   NXTNSEQB                                                         
         B     XIT                                                              
         DROP  R4,R5                                                            
         EJECT                                                                  
*              ROUTINE TO DOWNLOAD TABLE CONTAINING COMBINED                    
*              MUSICIANS(CAL) OR COMML TYPS/ADDEN STATES(INI)                   
         SPACE 1                                                                
PROCTAB  NTR1                                                                   
         LA    R4,MISCTAB                                                       
         TM    STESTAT,STETAB      IF IN MIDDLE OF TABLE                        
         BZ    *+8                                                              
         AH    R4,RECDSP           BUMP TO WHERE WE LEFT OFF                    
         ST    R4,AIO                                                           
         OC    0(1,R4),0(R4)       IF DATA                                      
         BZ    PROCTABX                                                         
         OI    STESTAT,STETAB      SET IN MIDDLE OF TABLE                       
         MVC   SVAIO,AIO           SET SAVED IOAREA                             
         BAS   RE,PROCREC          PROCESS TABLE OF ELEMENTS                    
         BNE   NOXITR4                                                          
*                                                                               
PROCTABX NI    STESTAT,X'FF'-STETAB DONE WITH TABLE                             
         B     YES                                                              
         SPACE 2                                                                
*              ROUTINE TO SAVE RECORD VALUES                                    
*                                  NTRY - R4=A(ELEMENT PROCESSING)              
         SPACE 1                                                                
SVRCVALS NTR1                                                                   
         CLI   STECNT,C'Y'         IF FINISHED DISPLAYING                       
         BE    SVRCVAL5                                                         
         NI    STEOPTH+4,X'DF'     SET TO RE-START W/NEXT HIT OF ENTER          
         B     XIT                                                              
*                                                                               
SVRCVAL5 XC    SVLSTUH,SVLSTUH     CLEAR TLUHD INFO - SO SAME REC READ          
         L     RE,AIO                                                           
         SR    R4,RE                                                            
         STH   R4,RECDSP           SAVE DISPLACEMENT INTO RECORD                
         TIME                                                                   
         STCM  R0,14,SVTIM         SAVE TIME OF DOWNLOAD                        
*                                                                               
         TM    STESTAT,STEWTWA     IF NEED TO SAVE SCRNEL                       
         BZ    SVRCVALX                                                         
         LA    R2,X'84'            SAVE OFF SCRNEL IN TWA 4                     
         GOTO1 GETTWA,DMCB,((R2),ASCRNEL)                                       
SVRCVALX B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO DOWNLOAD ONE RECORD - LOOPS THROUGH ELEMENTS,         
*              STARTING AT R4, AND FOR EACH ELE BUILDS SCREEN FORMATTED         
*              ELE IN SCRNEL.  SCRNEL THEN GETS PUT TO THE SCREEN               
*                                  R4=A(FIRST/NEXT ELEMENT TO PROCESS)          
*                                  XIT - CC NEQ R4=A(ELEMENT UP TO)             
         SPACE 1                                                                
PROCREC  NTR1                                                                   
         MVI   ELCODE,0            CLEAR ELEMENT CODE                           
*                                                                               
PROCREC2 L     R2,ASCRNEL          R2=A(AREA TO BUILD SCREEN ELEMENT)           
         CLI   0(R2),0             IF PREVIOUSLY BUILT                          
         BNE   PROCREC7            FINISH PUTTING TO SCREEN                     
*                                                                               
         CLI   0(R4),TAACELQ       IF ACTIVITY ELEMENT                          
         BNE   PROCREC3                                                         
         TM    STOPT,STEXTRA       IF SPECIAL DOWNLOAD X                        
         BO    PROCREC9            SKIP IT                                      
         B     PROCREC6            ELSE, GO PROCESS ELEMENT                     
*                                                                               
         USING TAESD,R4                                                         
PROCREC3 CLI   0(R4),TAESELQ       IF ESTIMATE ELE. FOR ACTUAL COMML            
         BNE   PROCREC6                                                         
         CLI   TAESTYPE,TAESTCOM                                                
         BNE   PROCREC6                                                         
         TM    STOPT,STEXTRA       IF SPECIAL DOWNLOAD X                        
         BO    PROCREC6            PROCESS NORMALLY                             
         BAS   RE,CLRMISC2         ELSE, CLR D/A TABLE OF USAGE HISTORY         
         GOTO1 APRESCID,DMCB,(RC)  PROCESS SEPERATELY                           
         BL    YES                 COMML NOT ON FILE AND NO MORE ELES           
         BNE   PROCREC2            COMML NOT ON FILE- R4=A(NEXT ELE.)           
         B     PROCREC8            GOOD - PUT TO SCREEN (ASCRNXT SET)           
*                                                                               
PROCREC6 BRAS  RE,PROVERS          PROCESS VERSIONS INTO LIFT FORMAT            
*                                                                               
         GOTO1 APROCEL,DMCB,(RC)   PROCESS ALL OTHER ELS INTO SCRNEL            
         BNE   PROCREC9            ASCRNXT SET                                  
PROCREC8 L     R1,ASCRNEL                                                       
         L     R2,ASCRNXT                                                       
         SR    R2,R1                                                            
         STH   R2,SCRELLEN         SAVE LENGTH OF DATA IN SCRNEL                
*                                                                               
PROCREC7 NI    STESTAT,X'FF'-STEWTWA                                            
         BAS   RE,MVCSCRN          MOVE SCREEN FORMAT'D ELEMENT TO SCRN         
         BE    PROCREC9                                                         
         OI    STESTAT,STEWTWA     SET FLAG TO SAVE SCRNEL IN TWA4              
         B     NOXITR4                                                          
*                                                                               
PROCREC9 BAS   RE,NEXTEL           IF NO MORE ELEMENTS                          
         BNE   YES                                                              
         B     PROCREC2                                                         
         DROP  R4                                                               
         EJECT                                                                  
*              MOVE SCREEN ELEMENT TO SCREEN                                    
         SPACE 1                                                                
MVCSCRN  NTR1                                                                   
         TM    STOPT,STEXPAND      IF EXPAND OPTION                             
         BZ    *+12                                                             
         BAS   RE,SETNLINE         SET NEW LINE VALUES                          
         BNE   NO                                                               
*                                                                               
         L     R1,ASCRNEL          A(START OF DATA)                             
         AH    R1,SCRELDSP         + WHERE WE LEFT OFF                          
         L     R2,ASCRNEL                                                       
         AH    R2,SCRELLEN                                                      
         SR    R2,R1               - A(END DATA)                                
         STH   R2,ELSCRLEN         IS TOTAL LENGTH OF DATA                      
*                                                                               
         OC    SCRLLEN,SCRLLEN     IF NO ROOM ON THIS LINE                      
         BNZ   *+12                                                             
         BAS   RE,SETNLINE         GET NEW LINE                                 
         BNE   NO                                                               
         L     R1,ASCRNEL          R1=A(SCREEN ELEMENT)                         
         AH    R1,SCRELDSP                                                      
*                                                                               
MVCSCRN5 L     R3,ASCRPOS          MOVE AS MUCH DATA TO SCR AS POSSIBLE         
         LH    R5,ELSCRLEN                                                      
         CLC   ELSCRLEN,SCRLLEN                                                 
         BNH   *+8                                                              
         LH    R5,SCRLLEN                                                       
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R1)       MOVE TO CURRENT LINE                         
         LA    R5,1(R5)            RESTORE LENGTH                               
         AR    R3,R5               BUMP TO NEXT SCREEN POSITION                 
         AR    R1,R5               BUMP TO NEXT PORTION OF DATA                 
*                                                                               
         LH    RE,ELSCRLEN         IF MORE ELEMENT TO DISPLAY                   
         CR    R5,RE                                                            
         BE    MVCSCRNX                                                         
         SR    RE,R5                                                            
         STH   RE,ELSCRLEN         SAVE LENGTH OF ELEMENT LEFT TO MOVE          
         BAS   RE,SETNLINE         SET NEW LINE VALUES                          
         BE    MVCSCRN5            AND LOOP                                     
         L     RE,ASCRNEL                                                       
         SR    R1,RE                                                            
         STH   R1,SCRELDSP         SAVE DISPLACEMENT INTO SCRNEL                
         B     NO                                                               
*                                                                               
MVCSCRNX ST    R3,ASCRPOS          SAVE A(SCREEN POSITION)                      
         LH    RE,SCRLLEN                                                       
         SR    RE,R5                                                            
         STH   RE,SCRLLEN          SAVE LENGTH OF LINE REMAINING                
         XC    SCRELDSP,SCRELDSP   CLEAR DISPLACEMENT INTO SCRNEL               
         BAS   RE,CLRSCREL         CLEAR AREA FOR ELEMENT SCREEN FORMAT         
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO SET NEW LINE LENGTH & POSITION BASED ON               
*              LINE NUMBER                                                      
         SPACE 1                                                                
SETNLINE NTR1                                                                   
         ZIC   R2,CURLINE                                                       
         CH    R2,=AL2(NLINES)     IF REACHED END OF SCREEN                     
         BE    NO                  RETURN WITH CC NOT EQUAL                     
*                                                                               
         MH    R2,=AL2(LINLNQ+8)   (N'LINES USED)*L'LINE                        
         LA    R3,STEREC                                                        
         AR    R3,R2                                                            
         ST    R3,ASCRPOS          IS CURRENT SCREEN CURSOR POSITION            
*                                                                               
         ZIC   R2,CURLINE                                                       
         LA    R2,1(R2)                                                         
         STC   R2,CURLINE          SAVE NEW LINE NUMBER                         
*                                                                               
         MVC   SCRLLEN,=AL2(LINLNQ) SET SCREEN LINE LENGTH                      
         CH    R2,=AL2(NLINES)                                                  
         BNE   *+10                                                             
         MVC   SCRLLEN,=AL2(LINLNQ-1) LAST LINE IS ONE LESS                     
         B     YES                                                              
         EJECT                                                                  
*              CLEAR NECESSARY GLOBAL FIELDS BEFORE READ HIGH ON KEY            
         SPACE 2                                                                
CLRCAG   DS    0H                                                               
         XC    TGCOM,TGCOM                                                      
         XC    TGCAT,TGCAT                                                      
         XC    TGCSORT,TGCSORT                                                  
         B     XIT                                                              
*                                                                               
CLRCA    XC    TGCSORT,TGCSORT                                                  
         XC    TGSSN,TGSSN                                                      
         XC    TGCAT,TGCAT                                                      
         B     XIT                                                              
*                                                                               
CLRGU    XC    TGGUA,TGGUA                                                      
         XC    TGAGY,TGAGY                                                      
         XC    TGCLI,TGCLI                                                      
         B     XIT                                                              
*                                                                               
CLRCO    XC    TGAGY,TGAGY                                                      
         XC    TGCLI,TGCLI                                                      
         XC    TGPRD,TGPRD                                                      
         XC    TGCID,TGCID                                                      
         XC    TGCLG,TGCLG                                                      
         B     XIT                                                              
*                                                                               
CLRCP    XC    TGAGY,TGAGY                                                      
         XC    TGCLI,TGCLI                                                      
         XC    TGPRD,TGPRD                                                      
         B     XIT                                                              
*                                                                               
CLRESL   XC    TGAGY,TGAGY                                                      
         XC    TGEST,TGEST                                                      
         XC    TGCLI,TGCLI                                                      
         XC    TGPRD,TGPRD                                                      
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE MOVES SCANNED DATA TO APPROP TGD FIELD                   
*              FOR DOWNLOAD AND UPLOAD                                          
*                                                                               
         USING SCAND,R3                                                         
         USING KEYFLDS,R5          R5=A(KEY FIELD ENTRY)                        
         SPACE 1                                                                
KEYCHAR  ZIC   R1,KEYFLEN          LENGTH OF FIELD - 1                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SCDATA1                                                  
*                                                                               
         TM    KEYFSTAT,KEYFCMP    IF ALSO NEED TO COMPLEMENT FIELD             
         BZ    XIT                                                              
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R2),HEXFFS                                                   
         B     XIT                                                              
         SPACE 1                                                                
KEYCOM   ZIC   RF,SCLEN1                   HEXIN INTERNAL COMML NUMBER          
         GOTO1 HEXIN,DMCB,SCDATA1,TGCOM,(RF)                                    
         OC    12(4,R1),12(R1)                                                  
         BZ    ERRINV                                                           
         B     XIT                                                              
         SPACE 1                                                                
KEYDATE  XC    WORK,WORK                                                        
         CLI   SCLEN1,0                                                         
         BE    KEYDAT5                                                          
         LR    R0,R2                                                            
         LA    R2,SCDATA1                                                       
         CLI   MYKCODE,TLESCDQ     IF ESTIMATE                                  
         BNE   KEYDAT2                                                          
         CLI   0(R2),C'-'          ALLOW <= DATES                               
         BNE   KEYDAT2                                                          
         LA    R2,1(R2)            BUMP PAST IT FOR DATE VALIDATION             
*                                                                               
KEYDAT2  GOTO1 DTVAL,DMCB,(X'40',WORK)                                          
         LR    R2,R0                                                            
         BNE   ERRINV                                                           
KEYDAT5  ZIC   R1,KEYFLEN                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),WORK                                                     
         CLI   SCDATA1,C'-'        IF <= DATE FLAG                              
         BNE   XIT                                                              
         MVI   3(R2),C'-'          SAVE FLAG BEHIND DATE                        
         B     XIT                                                              
         DROP  R5,R3                                                            
         EJECT                                                                  
*              ROUTINE VALIDATES KEY FIELD ON DOWNLOAD                          
*                                                                               
         USING KEYFLDS,R5          R5=A(KEY FIELD ENTRY)                        
         SPACE 1                                                                
VALAGY   DS    0H                                                               
         TM    KEYFSTAT,KEYFLOCL                                                
         BZ    *+10                                                             
         MVC   TGAGY,FILTAGY       SET GLOBAL FOR READ                          
         GOTO1 RECVAL,DMCB,TLAYCDQ,0                                            
         BNE   ERRAGY                                                           
         CLC   TGAGY,SPACES        IF NO AGENCY INPUT                           
         BE    ERRAGY              TRAP BUG                                     
         B     XIT                                                              
         SPACE 1                                                                
VALCLI   DS    0H                                                               
         TM    KEYFSTAT,KEYFLOCL                                                
         BZ    *+10                                                             
         MVC   TGCLI,FILTCLI       SET GLOBAL FOR READ                          
         GOTO1 RECVAL,DMCB,TLCLCDQ,0                                            
         BNE   ERRCLI                                                           
         B     XIT                                                              
         SPACE 1                                                                
VALPRD   DS    0H                                                               
         TM    KEYFSTAT,KEYFLOCL                                                
         BZ    *+10                                                             
         MVC   TGPRD,FILTPRD       SET GLOBAL FOR READ                          
         TM    KEYFSTAT,KEYFNOCL   IF PRODUCT OKAY WITH CLIENT                  
         BZ    *+14                                                             
         OC    TGCLI,TGCLI         IF NO CLIENT                                 
         BZ    XIT                 DON'T BOTHER VALIDATING                      
         GOTO1 RECVAL,DMCB,TLPRCDQ,0                                            
         BNE   ERRPRD                                                           
         B     XIT                                                              
         SPACE 1                                                                
VALSSN   DS    0H                                                               
         GOTO1 RECVAL,DMCB,TLW4CDQ,0                                            
         BNE   ERRSSN                                                           
         B     XIT                                                              
         SPACE 1                                                                
VALCLG   DS    0H                                                               
         TM    KEYFSTAT,KEYFLOCL                                                
         BZ    *+10                                                             
         MVC   TGCLG,FILTCLG       SET GLOBAL FOR READ                          
         GOTO1 RECVAL,DMCB,TLCGCDQ,0                                            
         BNE   ERRCLG                                                           
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*              ROUTINE TO UPLOAD ESTIMATE RECORD AND                            
*              WRITE IT BACK TO THE FILE                                        
         SPACE 1                                                                
UPLOAD   NTR1                                                                   
         MVI   STEOVR,C' '         NOT USED ON UPLOAD                           
         OI    STEOVRH+6,X'80'                                                  
*                                                                               
         CLI   STEREC,X'6A'        IF VERTICAL BAR FOLLOWED BY DELETE           
         BNE   UPLOAD20                                                         
         CLC   =C'DELETE',STEREC+1                                              
         BNE   UPLOAD20                                                         
         MVI   RDUPDATE,C'Y'       DELETE THE FIRST MAIN RECORD                 
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         USING TLESD,R3                                                         
         OI    TLESSTAT,X'80'                                                   
         GOTO1 PUTREC                                                           
         OI    KEY+TLDRSTAT-TLDRKEY,X'80'                                       
         GOTO1 WRITE                                                            
*                                                                               
         OI    DMINBTS,X'08'       READ FOR DELETED                             
         GOTO1 ADELETE,DMCB,(RC)   DELETE ANY REMAINING RECORDS                 
         MVI   RDUPDATE,C'N'       TURN OFF READ FOR UPDATE                     
         B     XIT                                                              
         DROP  R3                                                               
         SPACE 2                                                                
UPLOAD20 BAS   RE,UPEREC           INITIALIZE/READ EST REC. FROM TWA4           
         BAS   RE,MVSCRNEL         MOVE FROM SCRN TO SCRNEL AT ASCRPOS          
         L     R3,ASCRNEL          R3=A(SCREEN DATA)                            
         ST    R3,ASCRPOS          SAVE A(SCREEN POSITION)                      
         BAS   RE,UPSCRN           CONVERT SCRN ELES. TO FILE ELEMENTS          
         SPACE 1                                                                
         GOTO1 AWRITEIT,DMCB,(RC)  WRITE EST RECORD TO TWA4 OR FILE             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO INITIALIZE OR GET ESTIMATE RECORD SO FAR              
         SPACE 1                                                                
UPEREC   NTR1                                                                   
         L     R3,ASCRNEL          R3=A(SCREEN DATA)                            
         ST    R3,ASCRPOS          SAVE A(SCREEN POSITION)                      
         MVC   AIO,ATIA            SET IOAREA                                   
         SPACE 1                                                                
         CLI   KEYCHG,C'Y'         IF KEY CHANGED                               
         BE    *+12                                                             
         TM    STESTAT,STEWTWA     OR FIRST FOR KEY                             
         BO    UPEREC5                                                          
         SPACE 1                                                                
         MVI   EMSEQ,X'F1'         START TAEMSEQ AT 241 LIKE MAINT. PGM         
         MVI   ENSEQ,X'01'         START TAENSEQ AT 1 LIKE MAINT. PGM           
         XC    ESSEQS,ESSEQS       START ALL TAESSEQ'S TO ZERO                  
         SPACE 1                                                                
         L     R4,AIO              INITIALIZE ESTIMATE RECORD                   
         USING TLESD,R4                                                         
         XC    TLESKEY,TLESKEY                                                  
         MVI   TLESCD,TLESCDQ                                                   
         MVC   TLESAGY,TGAGY                                                    
         MVC   TLESEST,TGEST                                                    
         MVC   TLESLEN,DATADISP    SET STARTING LENGTH                          
         XC    TLESSTAT(10),TLESSTAT                                            
*                                                                               
         AH    R4,=AL2(SVDSPLNQ)   CLEAR SAVED SCREEN LENGTH                    
         XC    0(2,R4),0(R4)       AT TIA+9000                                  
         B     XIT                                                              
         DROP  R4                                                               
*                                                                               
UPEREC5  DS    0H                  READ RECORD FROM TWA4 TO TIA FOR MAX         
         GOTO1 GETTWA,DMCB,(X'14',ATIA)                                         
         L     RE,AIO              RESTORE DATA FROM TIA+9000 TO SCRNEL         
         AH    RE,=AL2(SVDSPLNQ)                                                
         XR    R1,R1                                                            
         ICM   R1,3,0(RE)          IF SAVED SCREEN LENGTH                       
         BZ    XIT                                                              
*                                                                               
         LR    R4,R1                                                            
         LA    RE,2(RE)            RE=A(DATA TO MOVE FROM)                      
         LR    R0,R3               R0=A(DATA TO MOVE TO)                        
         LR    RF,R1               RF,R1 IS LENGTH TO MOVE                      
         MVCL  R0,RE               RESTORE DATA TO SCRNEL                       
*                                                                               
         AR    R3,R4               SET A(SCRNEL) TO NEXT POSITION               
         MVI   0(R3),0                                                          
         ST    R3,ASCRPOS                                                       
         B     XIT                                                              
         EJECT                                                                  
*              MOVE DATA FROM SCREEN TO SCRNEL (CONTIGUOUS AREA)                
         SPACE 1                                                                
MVSCRNEL NTR1                                                                   
         LA    R0,NLINES           R0=(MAX N'LINES)                             
         LA    R2,STERECH          R2=A(FIRST SCREEN LINE)                      
         L     R3,ASCRPOS          R3=A(CONTIGUOUS AREA)                        
*                                                                               
MVSCRN5  ZIC   R1,5(R2)            MOVE DATA FROM SCREEN TO AREA                
         LTR   R1,R1                                                            
         BZ    MVSCRN6                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)                                                    
         LA    R1,1(R1)                                                         
         AR    R3,R1                                                            
         ZIC   R1,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,R1                                                            
         BCT   R0,MVSCRN5                                                       
*                                                                               
MVSCRN6  CLI   STECNT,C'Y'         IF ON LAST SCREEN                            
         BE    MVSCRN10                                                         
         BCTR  R3,0                PT TO LAST CHARACTER OF DATA                 
MVSCRN8  CLI   0(R3),X'6A'         AND SCAN BACK FOR BAR INDICATING END         
         BE    MVSCRN9                                                          
         BCT   R3,MVSCRN8                                                       
         DC    H'0'                MUST AT LEAST ONE BAR ON THE SCREEN          
MVSCRN9  LA    R3,1(R3)                                                         
         B     MVSCRNX                                                          
*                                                                               
MVSCRN10 XR    R1,R1               ELSE, SCAN BACK FOR EL START                 
MVSCRN15 CLC   =C'EL=',0(R3)                                                    
         BE    MVSCRN18                                                         
         LA    R1,1(R1)                                                         
         BCT   R3,MVSCRN15                                                      
         DC    H'0'                                                             
MVSCRN18 L     RE,ATIA             SAVE FROM 0(R3) LENGTH(2),DATA               
         AH    RE,=AL2(SVDSPLNQ)   AT TIA+9000                                  
         STCM  R1,3,0(RE)                                                       
         LA    RE,2(RE)                                                         
         LR    R0,R3                                                            
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
MVSCRNX  MVI   0(R3),0             THEN MARK END OF AREA                        
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE BUMPS THROUGH SCREEN AND CONVERTS SCREEN                 
*              ELEMENT TO FILE ELEMENT WHICH IT ADDS TO AIO                     
         SPACE 1                                                                
UPSCRN   NTR1                                                                   
*                                                                               
UPSCRN2  MVI   ELETYPE,0           PRE-CLEAR ELEMENT TYPE                       
         L     R3,ASCRPOS          R3=A(FIRST/NEXT POSITION IN SCRNEL)          
         CLC   =C'ES',3(R3)        AND ESTIMATE ELEMENT                         
         BNE   UPSCRN4                                                          
*                                                                               
         CLI   6(R3),C'A'          MUST BE FOLLOWED BY TYPE                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,8(R3)            CONVERT CHAR NUM TO ELEMENT TYPE             
         LA    RF,1                                                             
         CLI   9(R3),X'6A'                                                      
         BE    *+8                                                              
         LA    RF,2                                                             
         BAS   RE,NUMVL                                                         
         STC   RF,ELETYPE                                                       
*                                                                               
UPSCRN4  GOTO1 AGETNTRY,DMCB,(RC),(X'80',3(R3))                                 
         L     R5,AELENTRY         R5=A(TABLE ENTRY FOR ELEMENT)                
         LTR   R5,R5                                                            
         BZ    ERRINPUT                                                         
*                                                                               
         LA    R3,6(R3)            PT TO FIRST FIELD ELEMENT IN SCRNEL          
         ST    R3,ASCRPOS                                                       
*                                                                               
         USING ELTABD,R5                                                        
         XC    ELEMENT,ELEMENT     SET UP NEW ELEMENT                           
         LA    R4,ELEMENT                                                       
         MVC   0(1,R4),ELCDEQ      ELEMENT CODE                                 
         MVC   1(1,R4),ELLEN       MINIMUM ELEMENT LENGTH                       
*                                                                               
UPSCRN8  GOTO1 AUPEFLD,DMCB,(RC)   BUILD FIELD IN ELEMENT                       
         L     R3,ASCRPOS          R3=A(SCREEN DATA)                            
         CLI   0(R3),0             IF END OF SCRNEL                             
         BNE   UPSCRN10                                                         
         GOTO1 AMYADDL,DMCB,(RC)   ADD LAST ELEMENT TO EST. RECORD              
         B     XIT                                                              
*                                                                               
UPSCRN10 CLC   =C'EL=',0(R3)       IF NEW ELEMENT START                         
         BNE   UPSCRN8                                                          
         GOTO1 AMYADDL,DMCB,(RC)   ADD PREV. ELEMENT TO EST. RECORD             
         B     UPSCRN2             THEN PROCESS NEXT ELEMENT                    
         EJECT                                                                  
*                                  R4=A(DATA), RF=(L'DATA)                      
NUMVL    NTR1                                                                   
         MVC   WORK(9),=9X'F0'     INSURE NUMERIC INPUT                         
         SH    RF,=H'1'                                                         
         BM    NO                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),0(R4)                                                    
         CLC   WORK(9),=9X'F0'                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)                                                      
         CVB   RF,DUB                                                           
         XIT1  REGS=(RF)                                                        
         EJECT                                                                  
*              ROUTINE BUMPS R5 TO NEXT KEY TABLE ENTRY                         
         SPACE 1                                                                
         USING KEYTABD,R5                                                       
BMPKEYR5 DS    0H                                                               
         ZIC   RF,KEYNFLDS         (N'KEY FIELDS * L'OF A KEY FIELD)            
         MH    RF,=Y(KEYLNQ2)      + L'KEY HEADER                               
         LA    R5,KEYLNQ1(RF,R5)                                                
         BR    RE                  RETURNS R5=A(NEXT KEY ENTRY)                 
         SPACE 2                                                                
*              ROUTINE TO CLEAR SCREEN ELEMENT                                  
         SPACE 1                                                                
CLRSCREL NTR1                                                                   
         L     RE,ASCRNEL                                                       
         LH    RF,=Y(SCRNELQ)                                                   
         XCEFL                                                                  
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO CLEAR MISC. TABLE                                     
         SPACE 1                                                                
CLRMISC  NTR1                                                                   
         LA    RE,MISCTAB                                                       
         LA    RF,L'MISCTAB                                                     
         XCEFL                                                                  
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO CLEAR SECOND  MISC. TABLE                             
*              USED AS TLUHCDQ D/A OR TLCOCDQ INTERNAL COMML NUMBERS            
         SPACE 1                                                                
CLRMISC2 NTR1                                                                   
         LA    RE,MISCTAB2         CLEAR D/A TABLE OF USAGE HISTORY             
         LA    RF,L'MISCTAB2                                                    
         XCEFL                                                                  
         B     XIT                                                              
         EJECT                                                                  
*              EXITS, CONSTANTS, ETC.                                           
         SPACE 2                                                                
         GETEL (R4),DATADISP,ELCODE                                             
         SPACE 2                                                                
HEXFFS   DC    6X'FF'                                                           
         SPACE 2                                                                
LCOCLI   DC    AL1(TLCOCLI+L'TLCOCLI-TLCOD-1)                                   
LCOPRD   DC    AL1(TLCOPRD+L'TLCOPRD-TLCOD-1)                                   
LCONCLI  DC    AL1(TLCONCLI+L'TLCONCLI-TLCOPD-1)                                
LCOGCLG  DC    AL1(TLCOGCLG+L'TLCOGCLG-TLCOPD-1)                                
LCOGAGY  DC    AL1(TLCOGAGY+L'TLCOGAGY-TLCOPD-1)                                
LCOLCLG  DC    AL1(TLCOLCLG+L'TLCOLCLG-TLCOPD-1)                                
LUHCOM   DC    AL1(TLUHCOM+L'TLUHCOM-TLUHD-1)                                   
LCACOM   DC    AL1(TLCACOM+L'TLCACOM-TLCAD-1)                                   
         SPACE 2                                                                
LOWXIT   LNR   RC,RC               RETURN CC LOW                                
         B     NO                                                               
YES      XR    RC,RC               RETURN CC EQUAL                              
NO       LTR   RC,RC               RETURN CC NOT EQUAL                          
XIT      XIT1                      RETURN                                       
         SPACE 2                                                                
YESXITR4 XR    RC,RC               RETURN CC EQUAL WITH R4 SET                  
NOXITR4  LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1  REGS=(R4)           RETURN                                       
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
ERRMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     CUREND                                                           
         SPACE 1                                                                
ERRECINV MVI   ERROR,INVREC        RECORD INVALID                               
         LA    R2,CONRECH                                                       
         B     THEEND                                                           
         SPACE 1                                                                
ERRNTFND MVI   ERROR,NOTFOUND      RECORD NOT FOUND                             
         NI    STEOPTH+4,X'DF'     SET TO RE-START W/NEXT HIT OF ENTER          
         B     CUREND                                                           
*                                  RESERVED FOR UPLOAD SCREEN ERRORS            
ERRINPUT MVI   ERROR,ERCOMPT       OPTION INCOMPATIABLE W/PREV INPUT            
         NI    STEOPTH+4,X'DF'     SET TO RE-START W/NEXT HIT OF ENTER          
         B     CUREND                                                           
         SPACE 1                                                                
ERRCLI   MVC   MYMSGNO,=Y(ERCLINFD)   CLIENT NOT FOUND                          
         B     *+10                                                             
ERRPRD   MVC   MYMSGNO,=Y(ERPRDNFD)   PRODUCT NOT FOUND                         
         B     *+10                                                             
ERRAGY   MVC   MYMSGNO,=Y(ERAGYNFD)   AGENCY NOT FOUND                          
         B     *+10                                                             
ERRSSN   MVC   MYMSGNO,=Y(ERPERNFD)   PERFORMER NOT FOUND                       
         B     *+10                                                             
ERRCLG   MVC   MYMSGNO,=Y(ERCLGNFD)  CLIENT GROUP NOT FOUND                     
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         NI    STEOPTH+4,X'DF'     SET TO RE-START W/NEXT HIT OF ENTER          
         B     CUREND                                                           
         SPACE 1                                                                
ERRINV   MVI   ERROR,INVALID                                                    
         B     CUREND                                                           
         SPACE 1                                                                
DONE     MVI   MYMSGNO1,77                                                      
         LA    R2,STERECH                                                       
         OI    GENSTAT2,USGETTXT                                                
         B     THEEND                                                           
         SPACE 1                                                                
CUREND   LA    R2,STEKEYH                                                       
THEEND   GOTO1 EXIT,DMCB,0                                                      
         EJECT                                                                  
*              TABLE OF ROUTINES TO RELOCATE                                    
*                                                                               
RTNTAB   DS    0A                                                               
         DC    AL4(GETEREC)                                                     
         DC    AL4(FILTREC)                                                     
         DC    AL4(PRESCID)                                                     
         DC    AL4(PROCEL)                                                      
         DC    AL4(UPEFLD)                                                      
         DC    AL4(FMTEL)                                                       
         DC    AL4(SETELF)                                                      
         DC    AL4(SETETYPE)                                                    
         DC    AL4(GETNTRY)                                                     
         DC    AL4(ADDTAB)                                                      
         DC    AL4(MYADDL)                                                      
         DC    AL4(WRITEIT)                                                     
         DC    AL4(DELETE)                                                      
         DC    AL4(OPTELS)                                                      
         DC    AL4(OPTTAB)                                                      
         DC    AL4(KEYTAB)                                                      
         DC    AL4(ELTAB)                                                       
NRTNS    EQU   (*-RTNTAB)/L'RTNTAB                                              
*                                                                               
         SPACE 2                                                                
         EJECT                                                                  
*              ROUTINE READS WHOLE ESTIMATE RECORD IN IO 1,2 & 3                
         SPACE 1                                                                
         DS    0D                                                               
GETEREC  NMOD1 0,*GETEREC                                                       
         L     RC,0(R1)                                                         
*                                                                               
         GOTO1 GETREC              GET RECORD INTO IO1                          
         MVC   AIO,ATIA                                                         
GREC4    GOTO1 SEQ                 LOOK FOR ANOTHER RECORD                      
         SPACE 1                                                                
         LA    R3,KEY                                                           
         USING TLESD,R3                                                         
         CLC   TLESKEY(TLESSEQ-TLESD),SMYKEY   WITH ALL SAME UP TO SEQ.         
         BNE   GRECX                                                            
         GOTO1 GETREC              GET NEW RECORD INTO TEMPORARY IO             
         SPACE 1                                                                
         L     RE,AIO              NOW COMBINE THEM - RE=A(NEW RECORD)          
         L     R3,AIO1                                R3=A(MAIN RECORD)         
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
GRECX    MVC   AIO,AIO1            RESET IO AREA                                
         OI    STESTAT,STERERD     SET REREAD STATUS                            
         XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO FILTER AT RECORD LEVEL                                
*              IN ESL DOWNLOAD - FUDGING IOAREA TO CONTAIN 05 EL ONLY           
         SPACE 1                                                                
         DS    0D                                                               
FILTREC  NMOD1 0,*FILTREC                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R5,AKEYTAB                                                       
         AH    R5,KEYDSP                                                        
         USING KEYTABD,R5          R5=A(CURRENT KEY ENTRY)                      
*                                                                               
         L     R4,AIO              R4=A(RECORD TO PROCESS)                      
         BAS   RE,SETGBL           SET GLOBAL FIELDS                            
*                                                                               
         CLI   0(R4),TLVRCDQ       NEVER PROCESS VERSION RECORDS                
         BE    FILTRNO                                                          
*                                                                               
         CLI   0(R4),TLGUCDQ       IF GUARANTEE RECORD                          
         BNE   FILTR5                                                           
         BAS   RE,FLTTLGU          FILTER AGENCY AND/OR CLIENT                  
         B     FILTRX                                                           
*                                                                               
FILTR5   CLI   0(R4),TLCACDQ       IF CAST RECORD                               
         BNE   FILTR10                                                          
         CLC   =C'UHM',KEYCODE     IF UHM DOWNLOAD                              
         BNE   FILTR8                                                           
         TM    TGCSORT,X'80'       SKIP MUSICIANS                               
         BO    FILTRNO                                                          
         TM    TGCATYPE,NOHLD      SKIP CATS THAT DON'T GET HLDS                
         BO    FILTRNO                                                          
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL6                                                        
         BNE   FILTRNO                                                          
         USING TACAD,R4                                                         
         CLC   TACAONOF,=C'OFF'    OR HLDS IF OFF CAMERA                        
         BNE   FILTRYES                                                         
         TM    TGCATYPE,NOHLDOFF                                                
         BO    FILTRNO                                                          
         CLI   TGYREQU,CN88        OR HLDS IF OFF CAM AS/OF'88 CONTRACT         
         BL    FILTRYES                                                         
         TM    TGCATYPE,NHLDOF88                                                
         BO    FILTRNO                                                          
         B     FILTRYES                                                         
*                                                                               
FILTR8   TM    TGCSORT,X'80'       AND IF CATEGORY IS MUSICIAN                  
         BZ    FILTRYES                                                         
         GOTO1 AADDTAB,DMCB,(RC)   ADD TO MISCTAB                               
         B     FILTRNO             AND SKIP                                     
*                                                                               
FILTR10  CLI   0(R4),TLUHCDQ       IF USAGE HISTORY RECORD                      
         BNE   FILTR20                                                          
         BAS   RE,FLTTLUH          FILTER USE BY CYCLE DATES                    
         B     FILTRX                                                           
*                                                                               
FILTR20  CLI   0(R4),TLESCDQ       IF ESTIMATE RECORD                           
         BNE   FILTR21                                                          
         CLC   =C'ESL',KEYCODE     AND DOING LIST                               
         BNE   FILTR21                                                          
         BAS   RE,FILTESL          FILTER ESTIMATE RECORD ON LIST               
         BNE   FILTRNO                                                          
         BAS   RE,FUDGREC          FUDGE RECORD TO CONTAIN 05 EL ONLY           
         B     FILTRYES                                                         
*                                                                               
FILTR21  CLI   0(R4),TLCOCDQ       IF COMMERCIAL RECORD                         
         BNE   FILTRYES                                                         
         CLC   =C'COL',KEYCODE     AND DOING LIST                               
         BNE   FILTRYES                                                         
         BAS   RE,FILTCOL          FILTER COMMERCIAL RECORD ON LIST             
         B     FILTRX                                                           
         SPACE 1                                                                
FILTRYES XR    RC,RC               RETURN CC EQUAL                              
FILTRNO  LTR   RC,RC               RETURN CC NOT EQUAL                          
FILTRX   XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
*              ROUTINE TO SET GLOBAL FIELDS BASED ON RECORD                     
*                                  R4=A(RECORD)                                 
         SPACE 1                                                                
SETGBL   NTR1                                                                   
         CLI   0(R4),TLGUCDQ       IF GUARANTEE RECORD                          
         BNE   SETGBL2                                                          
         USING TLGUD,R4                                                         
         MVC   TGSSN,TLGUSSN       SET SOCIAL SECURITY NUMBER                   
         MVC   TGGUA,TLGUGUA       SET GUARANTEE NUMBER                         
         B     FILTRX                                                           
*                                                                               
SETGBL2  CLI   0(R4),TLCOCDQ       IF COMMERCIAL RECORD                         
         BNE   SETGBL5                                                          
         L     R4,AIO                                                           
         USING TLCOD,R4                                                         
         MVC   TGAGY,TLCOAGY       NOT SET IF TLCONCDQ                          
         MVC   TGCOM,TLCOCOM                                                    
         B     FILTRX                                                           
*                                                                               
SETGBL5  CLI   0(R4),TLW4CDQ       IF W4 RECORD                                 
         BE    SETGBL8                                                          
         CLI   0(R4),TLCACDQ       IF CAST RECORD                               
         BNE   SETGBL10                                                         
         USING TLCAD,R4                                                         
         MVC   TGSSN,TLCASSN       SET SOCIAL SECURITY NUMBER                   
         MVC   TGCSORT,TLCASORT    SET CAST SORT                                
         GOTO1 CATVAL,DMCB,TLCACAT SET CATEGORY CODE                            
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL6                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R4                                                         
         GOTO1 YRVAL,DMCB,TACAYEAR SET YEAR EQUATE                              
*                                                                               
         OI    STESTAT,STERERD     SET TO REREAD DIRECTORY POINTER              
         MVC   AIO,ATIA                                                         
         MVC   TGNAME,=CL36'** NOT FOUND **'                                    
         MVI   TGTYPE,TAW4TYCO                                                  
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',0)                                    
         BNE   FILTRX                                                           
         MVC   AIO,AIO1                                                         
         L     R4,ATIA                                                          
*                                                                               
SETGBL8  BAS   RE,SETGNM           SET NAME AND TYPE                            
         B     FILTRX                                                           
*                                                                               
SETGBL10 CLI   0(R4),TLUHCDQ       IF USAGE HISTORY                             
         BNE   FILTRX                                                           
         USING TLUHD,R4                                                         
         GOTO1 USEVAL,DMCB,TLUHUSE,0  SET USE CODE                              
         B     FILTRX                                                           
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO SET TYPE AND NAME FROM W4 RECORD                      
         SPACE 1                                                                
SETGNM   NTR1                                                                   
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL6                                                        
         BNE   FILTRX                                                           
         USING TAW4D,R4                                                         
         MVC   TGTYPE,TAW4TYPE                                                  
         MVC   TGNAME(L'TAW4CRPN),TAW4NAM2     SAVE FULL NAME                   
         B     FILTRX                                                           
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE FILTERS RECORD BY CHECKING TAGUD ELEMENT                 
*              BASED ON REQUESTED KEY FIELDS                                    
         SPACE 1                                                                
         USING TLGUD,R4            R4=A(GUARANTEE RECORD)                       
FLTTLGU  NTR1                                                                   
         MVI   ELCODE,TAGUELQ                                                   
         BAS   RE,GETEL6                                                        
         BNE   FILTRNO                                                          
*                                                                               
         USING TAGUD,R4                                                         
         CLC   FILTAGY,SPACES                                                   
         BNH   FTG10                                                            
         CLC   FILTAGY,TAGUAGY                                                  
         BNE   FTG20                                                            
*                                                                               
FTG10    CLC   FILTCLI,SPACES                                                   
         BNH   FILTRYES                                                         
         CLC   FILTCLI,TAGUCLI                                                  
         BE    FILTRYES                                                         
         DROP  R4                                                               
*                                                                               
         USING TAVAD,R4                                                         
FTG20    L     R4,AIO                                                           
         MVI   ELCODE,TAVAELQ      GET FIRST VALID AGENCY/CLIENT                
         BAS   RE,GETEL6           ELEMENT                                      
         BNE   FILTRNO             IF NOT FOUND, ACCESS IS GRANTED              
*                                                                               
FTG30    CLC   FILTAGY,SPACES      IF AGENCY IS FOUND IN GRT LIMITS             
         BNH   FTG40                                                            
         CLC   FILTAGY,TAVAAGY     AND CLIENT LIMITS ARE NOT DEFINED            
         BNE   FTG60               ACCESS IS GRANTED                            
*                                                                               
FTG40    CLC   FILTCLI,SPACES                                                   
         BNH   FILTRYES                                                         
         CLI   TAVALEN,TAVALNQ                                                  
         BE    FTG60                                                            
*                                                                               
         ZIC   RE,TAVALEN                                                       
         SHI   RE,TAVALNQ                                                       
         LA    RF,TAVACLI                                                       
FTG50    CLC   FILTCLI,0(RF)       IF CLIENT IS FOUND IN GRT LIMITS             
         BE    FILTRYES            ACCESS IS GRANTED                            
         LA    RF,L'TAVACLI(RF)                                                 
         SHI   RE,L'TAVACLI                                                     
         LTR   RE,RE                                                            
         BNZ   FTG50                                                            
*                                                                               
FTG60    BAS   RE,NEXTEL6          GET NEXT VALID AGENCY/CLIENT                 
         BE    FTG30               ELEMENT                                      
         B     FILTRNO             IF NOT FOUND, RETURN ERROR                   
         DROP  R4                                                               
         SPACE 2                                                                
*              ROUTINE FILTERS USAGE HISTORY RECORDS - PASS ONLY LATEST         
*              CYCLE FOR EACH USE                                               
         SPACE 1                                                                
         USING TLUHD,R4            R4=A(USAGE RECORD)                           
FLTTLUH  NTR1                                                                   
         OC    SVUHUSE,SVUHUSE     IF SAVED USE                                 
         BZ    *+14                                                             
         CLC   SVUHUSE,TLUHUSE     IF STILL ON SAME USE                         
         BE    FLTTLUH5            CONTINUE                                     
         CLC   =C'GRT',TLUHUSE     IF NOT GRT PAYMENT                           
         BE    FILTRNO                                                          
         MVC   SVUHUSE,TLUHUSE     SAVE NEW USE                                 
         XC    SVUHCYC,SVUHCYC                                                  
*                                                                               
FLTTLUH5 MVI   ELCODE,TAUHELQ                                                   
         BAS   RE,GETEL6                                                        
         BNE   FILTRNO                                                          
         USING TAUHD,R4                                                         
         OC    TAUHSTRT(6),TAUHSTRT IF NO CYCLE DATES                           
         BZ    FILTRNO                                                          
         OC    SVUHCYC,SVUHCYC     OR SAME CYCLE DATES                          
         BZ    *+14                                                             
         CLC   SVUHCYC,TAUHSTRT                                                 
         BE    FILTRNO             DON'T BOTHER READING IT                      
         MVC   SVUHCYC,TAUHSTRT    ELSE, SAVE CYCLE                             
         B     FILTRYES            AND SET TO READ                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE FILTERS ESTIMATES ON LIST                                
         SPACE 1                                                                
         USING TLESD,R4                                                         
FILTESL  NTR1                                                                   
         CLC   FILTSTRT,SPACES     IF START FILTER                              
         BNH   FLTESL2                                                          
         CLC   TLESEST,FILTSTRT                                                 
         BL    FILTRNO                                                          
*                                                                               
FLTESL2  CLC   FILTCLI,SPACES      IF CLIENT FILTER                             
         BNH   FLTESL4                                                          
         MVI   BYTE,TAESTCLI                                                    
         MVC   WORK(L'FILTCLI),FILTCLI                                          
         BAS   RE,CKMATCH                                                       
         BNE   FILTRNO                                                          
*                                                                               
FLTESL4  CLC   FILTPRD,SPACES      IF PRODUCT FILTER                            
         BNH   FLTESL6                                                          
         MVI   BYTE,TAESTPRD                                                    
         MVC   WORK(L'FILTPRD),FILTPRD                                          
         BAS   RE,CKMATCH                                                       
         BNE   FILTRNO                                                          
*                                                                               
FLTESL6  OC    FILTRDTE,FILTRDTE   IF REPORT DATE FILTER                        
         BZ    FLTESL8                                                          
         MVI   BYTE,SCR40                                                       
         MVC   WORK(L'FILTRDTE+1),FILTRDTE                                      
         BAS   RE,CKDATE                                                        
         BNE   FILTRNO                                                          
*                                                                               
FLTESL8  OC    FILTCDTE,FILTCDTE   IF CHANGED DATE FILTER                       
         BZ    FILTRYES                                                         
         MVI   BYTE,0                                                           
         MVC   WORK(L'FILTCDTE+1),FILTCDTE                                      
         BAS   RE,CKDATE                                                        
         B     FILTRX                                                           
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO CHECK MATCH ON CLIENT OR PRODUCT                      
*                                  NTRY - BYTE = TAESTCLI OR TAESTPRD           
*                                         WORK = CLIENT OR PRODUCT CODE         
*                                  XIT -  CC SET                                
CKMATCH  NTR1                                                                   
         L     R4,AIO              R4=A(FULL ESTIMATE RECORD)                   
         MVI   ELCODE,TAESELQ                                                   
         BAS   RE,GETEL6                                                        
         B     *+8                                                              
CKMATCH5 BAS   RE,NEXTEL6                                                       
         BNE   FILTRX                                                           
         SPACE 1                                                                
         USING TAESD,R4                                                         
         CLC   TAESTYPE,BYTE             MATCH ON ELEMENT TYPE                  
         BNE   CKMATCH5                                                         
         CLC   TAESDATA(L'FILTCLI),WORK  MATCH ON ELEMENT DATA                  
         BNE   CKMATCH5                                                         
         B     FILTRYES                                                         
         DROP  R4                                                               
         SPACE 2                                                                
*              ROUTINE TO CHECK MATCH ON REPORT OR CHANGED DATE                 
*                                  NTRY - BYTE = SCREEN CODE IN ELE             
*                                         WORK(3)- DATE                         
*                                         WORK+3 - FLAG FOR <= MATCH            
*                                  XIT -  CC SET                                
CKDATE   NTR1                                                                   
         L     R4,AIO              R4=A(FULL ESTIMATE RECORD)                   
         MVI   ELCODE,TAACELQ                                                   
         BAS   RE,GETEL6                                                        
         B     *+8                                                              
CKDATE5  BAS   RE,NEXTEL6                                                       
         BNE   FILTRX                                                           
         SPACE 1                                                                
         USING TAACD,R4                                                         
         CLC   TAACSCR,BYTE        MATCH ON SCREEN CODE                         
         BNE   CKDATE5                                                          
         SPACE 1                                                                
         CLI   WORK+3,C'-'         IF WANT ESTIMATES <= DATE                    
         BNE   CKDATE8                                                          
         CLC   TAACCDTE,WORK       REJECT DATES W/HIGH ACTIVITY                 
         BL    FILTRYES                                                         
         B     CKDATE5                                                          
*                                                                               
CKDATE8  CLC   TAACCDTE,WORK       REJECT DATES W/LOWER ACTIVITY                
         BH    FILTRYES                                                         
         B     CKDATE5                                                          
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE CREAMS ESTIMATE RECORD DETAILS WITH JUST                 
*              ONE FUDGED ELEMENT X'05'                                         
         SPACE 1                                                                
FUDGREC  NTR1                                                                   
         GOTO1 CHAROUT,DMCB,TANAELQ,0,0                                         
         BE    *+10                                                             
         XC    TGNAME,TGNAME                                                    
*                                                                               
         L     R4,AIO                                                           
         USING TLESD,R4                                                         
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    R3,ELEMENT                                                       
         USING TA05D,R3                                                         
         MVI   TA05EL,TA05ELQ                                                   
         MVI   TA05LEN,TA05LNQ                                                  
         MVC   TA05ID,TLESEST                                                   
         MVC   TA05NAME,TGNAME                                                  
*                                                                               
         MVI   ELCODE,TAACELQ                                                   
         BAS   RE,GETEL6                                                        
         B     *+8                                                              
FUDGR2   BAS   RE,NEXTEL6                                                       
         BNE   FUDGR8                                                           
*                                                                               
         USING TAACD,R4                                                         
         CLI   TAACSCR,SCR40                                                    
         BNE   FUDGR5                                                           
         CLC   TA05RDTE,TAACCDTE                                                
         BH    *+10                                                             
         MVC   TA05RDTE,TAACCDTE                                                
         B     FUDGR2                                                           
*                                                                               
FUDGR5   CLI   TAACSCR,0                                                        
         BNE   FUDGR2                                                           
         CLC   TA05CDTE,TAACCDTE                                                
         BH    *+10                                                             
         MVC   TA05CDTE,TAACCDTE                                                
         B     FUDGR2                                                           
*                                                                               
FUDGR8   L     R4,AIO                                                           
         USING TLESD,R4                                                         
         LH    R1,DATADISP                                                      
         LA    R1,TA05LNQ(R1)                                                   
         STH   R1,TLESLEN                                                       
*                                                                               
         LR    R1,R4                                                            
         AH    R1,DATADISP                                                      
         MVC   0(TA05LNQ,R1),ELEMENT                                            
         LA    R1,TA05LNQ(R1)                                                   
         MVI   0(R1),0                                                          
         B     FILTRX                                                           
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO FILTER COMMERCIAL RECORD ON LIST                      
         SPACE 1                                                                
         USING TLCOD,R4                                                         
FILTCOL  NTR1                                                                   
         CLC   FILTAGY,SPACES                                                   
         BNH   *+14                                                             
         CLC   FILTAGY,TLCOAGY                                                  
         BNE   FILTRNO                                                          
*                                                                               
         CLC   FILTPRD,SPACES                                                   
         BNH   *+14                                                             
         CLC   FILTPRD,TLCOPRD                                                  
         BNE   FILTRNO                                                          
*                                                                               
         MVI   ELCODE,TAOCELQ                                                   
         USING TAOCD,R4                                                         
         BAS   RE,GETEL6                                                        
         B     *+8                                                              
FLTCOL2  BAS   RE,NEXTEL6                                                       
         BNE   FLTCOL5                                                          
         TM    TAOCSTAT,TAOCSTO                                                 
         BO    FILTRNO                                                          
         B     FLTCOL2                                                          
*                                                                               
FLTCOL5  L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL6                                                        
         BNE   FILTRNO                                                          
         USING TACOD,R4                                                         
         SPACE 1                                                                
         BAS   RE,COLOCK           CHECK PASS LOCK TEST                         
         BNE   FILTRNO                                                          
         BAS   RE,CORELEAS         CHECK PASS RELEASED TEST                     
         BNE   FILTRNO                                                          
         BAS   RE,COMUSIC          CHECK PASS MUSIC TEST                        
         BNE   FILTRNO                                                          
         BAS   RE,COACTV           CHECK PASS ACTIVITY TEST                     
         BNE   FILTRNO                                                          
*                                                                               
         CLC   FILTMED,SPACES                                                   
         BNH   *+14                                                             
         CLC   FILTMED,TACOMED                                                  
         BNE   FILTRNO                                                          
*                                                                               
         CLC   FILTCLG,SPACES                                                   
         BNH   FILTRYES                                                         
         CLC   FILTCLG,TACOCLG                                                  
         B     FILTRX                                                           
         EJECT                                                                  
*              ROUTINE CHECKS COMMERCIAL LOCKED FILTER                          
*                                                                               
COLOCK   NTR1                                                                   
         CLI   FILTLOCK,C'O'       IF ONLY LOCKED COMMERCIALS                   
         BNE   *+16                                                             
         TM    TACOSTAT,TACOSTLO   JUST SHOW LOCKED COMMERCIALS                 
         BZ    FILTRNO                                                          
         B     FILTRYES                                                         
         CLI   FILTLOCK,C'Y'       ELSE, UNLESS REQUESTED TO INCLUDE            
         BE    *+12                                                             
         TM    TACOSTAT,TACOSTLO   IGNORE LOCKED COMMERCIALS                    
         BO    FILTRNO                                                          
         B     FILTRYES                                                         
         SPACE 2                                                                
*              ROUTINE CHECKS COMMERCIAL RELEASED FILTER                        
*                                                                               
CORELEAS NTR1                                                                   
         CLI   FILTREL,C'O'        IF ONLY RELEASED COMMERCIALS                 
         BNE   *+16                                                             
         TM    TACOSTAT,TACOSTRL   JUST SHOW RELEASED COMMERCIALS               
         BZ    FILTRNO                                                          
         B     FILTRYES                                                         
         CLI   FILTREL,C'Y'        ELSE, UNLESS REQUESTED TO INCLUDE            
         BE    *+12                                                             
         TM    TACOSTAT,TACOSTRL   IGNORE RELEASED COMMERCIALS                  
         BO    FILTRNO                                                          
         B     FILTRYES                                                         
         EJECT                                                                  
*              ROUTINE CHECKS COMMERCIAL ACTIVITY FILTER                        
*                                                                               
COMUSIC  NTR1                                                                   
         CLI   FILTMUS,C'N'        IF EXCLUDING MUSIC COMMLS                    
         BNE   FILTRYES                                                         
         CLI   TACOTYPE,CTYMUS     IGNORE THEM                                  
         BE    FILTRNO                                                          
         B     FILTRYES                                                         
         SPACE 2                                                                
*              ROUTINE CHECKS COMMERCIAL ACTIVITY FILTER                        
*                                                                               
COACTV   NTR1                                                                   
         OC    TACOPDTE,TACOPDTE   IF LAST PAID DATE                            
         BZ    COACT10                                                          
         CLC   TACOPDTE,FILTPDTE   CHECK AGAINST REQUESTED DATE                 
         BL    FILTRNO                                                          
         B     FILTRYES                                                         
*                                                                               
COACT10  MVI   ELCODE,TAACELQ      ELSE, GET LAST ACTIVITY ELE                  
         L     R4,AIO                                                           
         BAS   RE,GETEL6                                                        
         BNE   FILTRNO                                                          
         USING TAACD,R4                                                         
         CLC   TAACCDTE,FILTPDTE   CHECK ACTIVITY SINCE LAST ACTV DATE          
         BL    FILTRNO                                                          
         B     FILTRYES                                                         
         DROP  R4                                                               
         GETELN (R4),DATADISP,ELCODE,6                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*        ROUTINE PROCESSES ONE ELEMENT (HEADER AND FIELDS)                      
*        TO SCREEN FORMATTED ELEMENT IN SCRNEL                                  
*                                  NTRY - R4=A(ELEMENT)                         
*                                         R2=A(SCRNEL)                          
*                                  XIT  - ASCRNXT=NEXT POS. IN SCRNEL           
         SPACE 1                                                                
         DS    0D                                                               
PROCEL   NMOD1 0,*PROCEL*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         MVC   SVELCODE,ELCODE     SAVE ELEMENT CODE                            
         GOTO1 ASETETYP,DMCB,(RC)  SET ELETYPE                                  
         GOTO1 AGETNTRY,DMCB,(RC),(0,(R4))                                      
         L     R5,AELENTRY         IF TABLE ENTRY FOR ELEMENT FOUND             
         LTR   R5,R5                                                            
         BZ    PROCELNO                                                         
         USING ELTABD,R5                  R5=A(ELEMENT ENTRY)                   
*                                                                               
         TM    STESTAT2,STECOM     IF NEW COMML (UHM DWNLD)                     
         BZ    PROCELA                                                          
         MVC   ELFLABEL,=CL4'EL'   PUT EL=INT COMML #º                          
         XC    ELFDATA,ELFDATA                                                  
         GOTO1 HEXOUT,DMCB,TGCOM,ELFDATA,4,0                                    
         GOTO1 AFMTEL,DMCB,(RC)    FORMAT AT R2                                 
         NI    STESTAT2,X'FF'-STECOM                                            
*                                  PUT HEADER - EL=XXº                          
PROCELA  MVC   ELFLABEL,=CL4'EL'                                                
         XC    ELFDATA,ELFDATA                                                  
         MVC   ELFDATA(L'ELCD),ELCD CODE FOR ELEMENT                            
         GOTO1 AFMTEL,DMCB,(RC)    FORMAT AT R2                                 
         SPACE 2                                                                
*                                  PUT FIELDS - FLD=DATAºFLD=DATAº ETC          
         ZIC   R0,ELNFLDS          R0=A(N'ELEMENT FIELDS)                       
         MVC   ELMINLEN,ELLEN      SAVE BASE ELEMENT LENGTH                     
         MVC   ELCDEQU,ELCDEQ      SAVE ELEMENT EQUATE                          
         LA    R5,ELLNQ1(R5)                                                    
         USING ELFLDS,R5           R5=A(FIRST ELEMENT FIELD)                    
*                                                                               
PROCEL1  TM    ELFSTAT,ELFXUHM     IF NEEDS TO BE EXCLUDED FROM UHM             
         BZ    PROCEL1A                                                         
         L     RE,AKEYTAB          AND THIS IS UHM DOWNLOAD                     
         AH    RE,KEYDSP                                                        
         USING KEYTABD,RE                                                       
         CLC   =C'UHM',KEYCODE                                                  
         BE    PROCEL6             THEN SKIP                                    
         DROP  RE                                                               
PROCEL1A TM    STOPT,STEXTRA       IF NOT DOWNLOAD WITH 'X'                     
         BO    *+16                                                             
         TM    ELFSTAT,ELFIDWNX    SKIP ONES FOR JUST DOWNLOAD X                
         BO    PROCEL6                                                          
         B     PROCEL1X                                                         
         TM    ELFSTAT,ELFXDWNX    ELSE, SKIP ONES FOR REGULAR DOWNLOAD         
         BO    PROCEL6                                                          
*                                                                               
PROCEL1X ZIC   R3,ELFDISP                                                       
         AR    R3,R4               R3=A(FIELD IN ELEMENT)                       
*                                                                               
         MVC   ELFLABEL,SPACES                                                  
         CLC   =CL5'EL',ELFSCODE   IF SPECIAL EL=                               
         BNE   *+14                                                             
         MVC   ELFLABEL(2),=C'EL'  USE EL NOMATTER THE OPTION                   
         B     PROCEL2                                                          
         TM    STOPT,STEXPAND      IF EXPAND OPTION                             
         BZ    *+14                                                             
         MVC   ELFLABEL,ELFCODE    USE FOUR CHARACTER LABEL                     
         B     *+10                                                             
         MVC   ELFLABEL(L'ELFSCODE),ELFSCODE ELSE, JUST USE LETTER              
PROCEL2  XC    ELFDATA,ELFDATA                                                  
*                                                                               
         ZIC   RE,ELFLEN                                                        
         LTR   RE,RE               ZERO LENGTHS ARE SPECIAL                     
         BZ    PROCEL4                                                          
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         OC    0(0,R3),0(R3)       SKIP FIELDS WITH NO DATA                     
         BZ    PROCEL6                                                          
*                                                                               
PROCEL4  XR    R1,R1               FLAG - CONTINUE WITH ELEMENT FIELDS          
         XR    RF,RF                                                            
         ICM   RF,3,ELFDWN         WHICH SETS ELFDATA                           
         BZ    PROCEL5                                                          
         AR    RF,RB               MAY PASS BACK R1 AND R4                      
         LA    RE,PROCEL5                                                       
         NTR1                                                                   
         BR    RF                                                               
*                                                                               
PROCEL5  CH    R1,=H'0'            CONTINUE TO NEXT ELEMENT FIELDS              
         BL    PROCEL6                                                          
         BH    PROCELY                                                          
         GOTO1 AFMTEL,DMCB,(RC)    FORMAT AT R2                                 
PROCEL6  LA    R5,ELLNQ2(R5)       BUMP TO NEXT FIELD ELEMENT ENTRY             
         BCT   R0,PROCEL1                                                       
*                                                                               
PROCELY  XR    R1,R1               RETURN CC EQUAL                              
         B     *+8                                                              
PROCELNO LA    R1,1                RETURN CC NOT EQUAL                          
         LTR   R1,R1                                                            
         MVC   ELCODE,SVELCODE     RESET ELEMENT CODE                           
         MVC   AIO,SVAIO           RESET AIO TO MAIN RECORD/TABLE               
         ST    R2,ASCRNXT          RETURN ASCRNXT=(NEXT POS. IN SCRNEL)         
PROCXIT  XIT1                                                                   
         SPACE 1                                                                
PXITR1R2 LH    R1,=H'-1'           RETURN WITH REGISTER 1 LOW                   
         XIT1  REGS=(R1,R2)        AND REGISTER TWO SET                         
         SPACE 1                                                                
PLOWXIT1 LH    R1,=H'-1'           REG1 LOW SKIPS MOVING FLD TO SCR             
         B     *+8                                                              
PHIXITR1 LH    R1,=H'1'            REG1 HIGH SKIPS THIS & REMAIN FLDS           
         XIT1  REGS=(R1)                                                        
         SPACE 2                                                                
PXITR3   XIT1  REGS=(R3)           RETURN WITH REGISTER 3 SET                   
PXITR4   XIT1  REGS=(R4)           RETURN                                       
         DROP  R5                                                               
         EJECT                                                                  
*              DOWNLOAD ELEMENT FIELD ROUTINES TO ELFDATA                       
*                                  R2=A(AREA TO FORMAT ELEMENT)                 
*                                  R3=A(ELEMENT DATA)                           
         USING ELFLDS,R5           R5=A(ELEMENT FIELD ENTRY)                    
         SPACE 2                                                                
DWNECHAR DS    0H                  MOVE CHARACTERS                              
         ZIC   R1,ELFLEN           IF FIELD LENGTH NOT DEFINED                  
         LTR   R1,R1                                                            
         BNZ   DWNECH10                                                         
         ZIC   R1,1(R4)            TAKE ELEMENT LENGTH                          
         ZIC   RE,ELMINLEN         LESS BASE LENGTH                             
         SR    R1,RE                                                            
         LTR   R1,R1                                                            
         BZ    PLOWXIT1                                                         
*                                                                               
DWNECH10 BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ELFDATA(0),0(R3)    MOVE FROM ELEMENT TO SCREEN                  
         LA    R1,1(R1)            RESTORE LENGTH                               
         BAS   RE,CHKBAR           CHANGE BARS TO SLASH                         
         B     PROCXIT                                                          
         SPACE 2                                                                
DWNEDIT  DS    0H                  CONVERT FROM HEX TO EBCDIC                   
         XR    R1,R1                                                            
         CLI   ELFLEN,1                                                         
         BNE   *+12                                                             
         IC    R1,0(R3)                                                         
         B     *+8                                                              
         ICM   R1,3,0(R3)                                                       
         BAS   RE,EDITR1F5         EDIT R1 FOR 5 CHARACTERS                     
         B     PROCXIT                                                          
         SPACE 2                                                                
DWNEZERO DS    0H                  PASS ZERO IN THIS CASE                       
         XR    R1,R1                                                            
         ICM   R1,3,0(R3)                                                       
         EDIT  (R1),(5,ELFDATA),ALIGN=LEFT,ZERO=NOBLANK                         
         B     PROCXIT                                                          
         SPACE 2                                                                
DWNECNT  DS    0H                  NUMBER OF HYPO PERFS, NUMBER OF MUS          
         XR    R1,R1                                                            
         CLI   ELFLEN,1                                                         
         BNE   *+12                                                             
         IC    R1,0(R3)                                                         
         B     *+8                                                              
         ICM   R1,3,0(R3)                                                       
         CH    R1,=H'1'            DEFAULT IF NOT MORE THAN ONE                 
         BNH   PLOWXIT1                                                         
         BAS   RE,EDITR1F5         EDIT R1 FOR 5 CHARACTERS                     
         B     PROCXIT                                                          
         EJECT                                                                  
*              DOWNLOAD ELEMENT FIELD RTNS TO ELFDATA (CONTINUED)               
         SPACE 2                                                                
DWNEHEX  ZIC   RF,ELFLEN           CONVERT HEX TO EBCDIC FOR L'DATA             
         BAS   RE,HEXOUTRF                                                      
         B     PROCXIT                                                          
         SPACE 2                                                                
DWNECSEQ LA    R3,TGCSORT          HEXOUT CAST SORT SEQUENCE                    
         LA    RF,L'TGCSORT                                                     
         BAS   RE,HEXOUTRF                                                      
         B     PROCXIT                                                          
         SPACE 2                                                                
DWNEMEDQ DS    0H                  CONVERT MEDIA - FIRST CHAR ONLY              
         GOTO1 MEDVAL,DMCB,(X'80',0(R3))                                        
         OC    TGMENAME,TGMENAME                                                
         BZ    PLOWXIT1                                                         
         B     DWNEMEDX                                                         
DWNEMED  GOTO1 MEDVAL,DMCB,0(R3)   MEDIA                                        
         BNE   PLOWXIT1                                                         
DWNEMEDX MVC   ELFDATA(1),TGMENAME                                              
         BAS   RE,IFOPTIM          IF CAL/COL/UHM OPTIMIZATION NEC.             
         BNE   PROCXIT                                                          
         CLC   SVMED,ELFDATA       IF NO CHANGE IN MEDIA CODE                   
         BE    PLOWXIT1            SKIP                                         
         MVC   SVMED,ELFDATA       ELSE, SAVE NEW MEDIA AND DISPLAY             
         B     PROCXIT                                                          
         SPACE 2                                                                
DWNEUSE  MVC   ELFDATA(3),TGUSCDE  PASS USE CODE                                
         B     PROCXIT                                                          
         SPACE 2                                                                
DWNEUSET DS    0H                  PASS USE TYPE TO GET DESCRIPTION             
         GOTO1 USEVAL,DMCB,TGUSCDE,0(R3)                                        
         CLC   TGUSTYCD,SPACES                                                  
         BNH   PLOWXIT1                                                         
         MVC   ELFDATA(L'TGUSTYCD),TGUSTYCD                                     
         B     PROCXIT                                                          
         SPACE 2                                                                
DWNECATQ DS    0H                  CONVERT EQUATE TO 3 CHAR CATEGORY            
         GOTO1 CATVAL,DMCB,(X'80',0(R3))                                        
DWNECAT  DS    0H                  CATEGORY                                     
         MVC   ELFDATA(L'TGCAT),TGCAT                                           
         BAS   RE,IFOPTIM          IF CAL/COL/UHM OPTIMIZATION NEC.             
         BNE   PROCXIT                                                          
         CLC   SVCAT,ELFDATA       IF NO CHANGE IN CATEGORY                     
         BE    PLOWXIT1            SKIP                                         
         MVC   SVCAT,ELFDATA       ELSE, SAVE NEW CATEGORY AND DISPLAY          
         B     PROCXIT                                                          
         SPACE 2                                                                
DWNEUNIQ DS    0H                  CONVERT UNION                                
         GOTO1 UNIVAL,DMCB,(X'20',0(R3))                                        
         MVC   ELFDATA(L'TGUNI),TGUNI                                           
         B     PROCXIT                                                          
         SPACE 2                                                                
DWNEREL  DS    0H                  COMMERCIAL STATUS                            
         TM    0(R3),TACOSTRL      IF COMMERCIAL RELEASED                       
         BZ    PLOWXIT1                                                         
         MVI   ELFDATA,C'Y'        INDICATE SO                                  
         B     PROCXIT                                                          
         SPACE 2                                                                
DWNEUHST DS    0H                  USAGE HISTORY STATUS                         
         TM    0(R3),TAUHSUPN+TAUHSUPW  IF UPGRADED TO SNW FROM SNT/SWS         
         BZ    PLOWXIT1                                                         
         B     DWNEDIT             PASS THIS                                    
         EJECT                                                                  
*              DOWNLOAD ELEMENT FIELD RTNS TO ELFDATA (CONTINUED)               
         SPACE 2                                                                
DWNECOMG DS    0H                  SET TGCOM                                    
         MVC   TGCOM,0(R3)                                                      
         OC    TGCOM,TGCOM                                                      
         BZ    PHIXITR1                                                         
         B     PLOWXIT1                                                         
         SPACE 2                                                                
DWNECOM  MVC   TGCOM,0(R3)         INTERNAL COMMERCIAL NUMBER                   
         TM    STOPT,STEXTRA       IF SPECIAL DOWNLOAD WITH AN X                
         BZ    PLOWXIT1                                                         
DWNECOM2 DS    0H                                                               
         LA    R3,TGCOM            HEXOUT INTERNAL COMML NUMBER                 
         LA    RF,L'TGCOM                                                       
         BAS   RE,HEXOUTRF                                                      
         B     PROCXIT                                                          
         SPACE 2                                                                
DWNECID  DS    0H                  READ COMML RECORD AND SET CID                
         OC    TGCOM,TGCOM                                                      
         BZ    PLOWXIT1                                                         
         OI    STESTAT,STERERD     SET TO REREAD DIRECTORY POINTER              
         MVC   AIO,ATIA                                                         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A0',0)                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   ELETYPE,TAESTCOM    IF NOT WITHIN ESTIMTE ELEMENT                
         BE    DWNECID5                                                         
         L     R1,AIO              SHOW CID                                     
         MVC   ELFDATA(L'TLCOCID),TLCOCID-TLCOD(R1)                             
         B     PROCXIT                                                          
DWNECID5 MVC   ELFDATA(2),=C'CO'   ELSE, JUST SET CO                            
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PXITR4                                                           
         SPACE 2                                                                
DWNECIDN DS    0H                  SET COMMERCIAL NAME                          
         GOTO1 CHAROUT,DMCB,TANAELQ,0                                           
         BNE   PLOWXIT1                                                         
         MVC   ELFDATA(L'TGNAME),TGNAME                                         
         LA    R1,L'TGNAME                                                      
         BAS   RE,CHKBAR           MAKE SURE NO VERTICAL BARS                   
         B     PROCXIT                                                          
         EJECT                                                                  
*              DOWNLOAD ELEMENT FIELD RTNS TO ELFDATA (CONTINUED)               
         SPACE 2                                                                
DWNECIDF DS    0H                  FIRST FIXED CYCLE FROM COMML                 
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         LA    R3,TACOFCYC         USE FIRST FIXED CYCLE                        
*                                                                               
         CLI   TACOTYPE,CTYMUS     IF MUSIC                                     
         BNE   *+16                                                             
         MVI   ELCODE,TACSELQ                                                   
         MVI   WORK,TACSTYPM       USE MUSIC DATE                               
         B     DWNECDF4                                                         
         CLI   TACOMED,TACOMEDR    IF RADIO                                     
         BNE   DWNECDF5                                                         
         MVI   ELCODE,TACSELQ                                                   
         MVI   WORK,TACSTYPR       USE RECORDING DATE                           
DWNECDF4 GOTO1 GETL,DMCB,(1,WORK)                                               
         BNE   PLOWXIT1                                                         
         L     R4,TGELEM                                                        
         USING TACSD,R4                                                         
         LA    R3,TACSDATE                                                      
*                                                                               
DWNECDF5 OC    0(3,R3),0(R3)       MAKE SURE THERE IS A DATE                    
         BZ    PLOWXIT1                                                         
         BAS   RE,DWNEDATL                                                      
         B     PROCXIT                                                          
         SPACE 1                                                                
DWNECIDE DS    0H                  EXPIRATION DATE FROM COMML                   
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         LA    R3,TACOEXP                                                       
         BAS   RE,DWNEDATL                                                      
         B     PROCXIT                                                          
         EJECT                                                                  
*              DOWNLOAD ELEMENT FIELD RTNS TO ELFDATA (CONTINUED)               
         SPACE 2                                                                
DWNECLI  L     R4,AIO                                                           
         MVC   ELFDATA(L'TLCOCLI),TLCOCLI-TLCOD(R4)                             
         BAS   RE,IFOPTIM          IF CAL/COL/UHM OPTIMIZATION NEC.             
         BNE   PROCXIT                                                          
         CLC   SVCLI,ELFDATA       IF NO CHANGE IN CLIENT CODE                  
         BE    PLOWXIT1            SKIP                                         
         MVC   SVCLI,ELFDATA       ELSE, SAVE NEW CODE AND DISPLAY              
         B     PROCXIT                                                          
         SPACE 1                                                                
DWNEPRD  L     R4,AIO                                                           
         MVC   ELFDATA(L'TLCOPRD),TLCOPRD-TLCOD(R4)                             
         BAS   RE,IFOPTIM          IF NO CAL/COL/UHM OPTIMIZATION NEC.          
         BE    DWNEPRD5                                                         
         OC    ELFDATA(L'TLCOPRD),ELFDATA  SKIP IF NO PRD                       
         BZ    PLOWXIT1                                                         
         B     PROCXIT                                                          
DWNEPRD5 CLC   SVPRD,ELFDATA       ELSE, IF NO CHANGE IN PRODUCT CODE           
         BE    PLOWXIT1            SKIP                                         
         MVC   SVPRD,ELFDATA       SAVE NEW CODE AND DISPLAY                    
         B     PROCXIT                                                          
         SPACE 1                                                                
DWNECAST DS    0H                  CHECKS TO SEE IF ACTUAL CAST                 
         XC    TGCSORT,TGCSORT                                                  
         XC    TGSSN,TGSSN                                                      
         XC    TGCAT,TGCAT                                                      
         OI    STESTAT,STERERD     SET TO REREAD DIRECTORY POINTER              
         GOTO1 RECVAL,DMCB,TLCACDQ,0                                            
         CLC   KEY(TLCASORT-TLCAD),KEYSAVE                                      
         BE    PLOWXIT1                                                         
         MVI   ELFDATA,C'N'                                                     
         B     PROCXIT                                                          
         SPACE 1                                                                
DWNELIFT DS    0H                  CHECKS TO SEE IF LIFT DETAILS                
         L     R4,AIO                                                           
         MVI   ELCODE,TALFELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   DL10                                                             
         MVI   ELFDATA,C'Y'                                                     
         B     PROCXIT                                                          
         SPACE 1                                                                
         USING TAVRD,R4                                                         
DL10     L     R4,AIO              R4=A(COMMERCIAL RECORD)                      
         MVI   ELCODE,TAVRELQ      READ VERSION ELEMENTS                        
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
DL20     BRAS  RE,NEXTEL                                                        
         BNE   PLOWXIT1                                                         
         SPACE 1                                                                
         CLI   TAVRVERS,2          IF ELEMENT FOR VERSION 2                     
         BNE   *+12                IS FOUND ...                                 
         MVI   ELFDATA,C'Y'        SAVE LIFT DETAILS                            
         B     DL20                                                             
         SPACE 1                                                                
         CLI   TAVRVERS,3          IF ELEMENT FOR VERSION 3                     
         BNE   DL20                IS FOUND                                     
         CLI   ELFDATA,C'Y'        ALREADY FOUND A 2ND LIFT?                    
         BNE   *+12                                                             
         MVI   ELFDATA,C'B'                                                     
         B     *+8                                                              
         MVI   ELFDATA,C'S'        SAVE LIFT DETAILS                            
         B     DL20                                                             
         DROP  R4                                                               
         SPACE 2                                                                
DWNESTAF DS    0H                  READ STAFF RECORD FOR NAME                   
         MVC   TGUSER,0(R3)                                                     
         MVC   TGSTAF,L'TGUSER(R3)                                              
         OI    STESTAT,STERERD     SET TO REREAD DIRECTORY POINTER              
         MVC   AIO,ATIA                                                         
         GOTO1 RECVAL,DMCB,TLSTCDQ,(X'A8',0),0                                  
         BE    *+10                                                             
         MVC   TGNAME,=CL36'** NOT FOUND **'                                    
         MVC   ELFDATA(L'TGNAME),TGNAME                                         
         B     PROCXIT                                                          
         EJECT                                                                  
*              DOWNLOAD ELEMENT FIELD RTNS TO ELFDATA (CONTINUED)               
         SPACE 2                                                                
DWNECASH DS    0H                  CONVERT FULL WORD AMT TO EBCDIC              
         EDIT  (4,0(R3)),(11,ELFDATA),ALIGN=LEFT,FLOAT=-                        
         B     PROCXIT                                                          
         SPACE 1                                                                
DWNEDATL NTR1                      LOCAL ENTRY                                  
DWNEDATE DS    0H                  CONVERT PWOS DATE TO YYYY-MM-DD              
         GOTO1 DATCON,DMCB,(5,0),(23,WORK+20)   TODAYS DATE                     
         GOTO1 DATCON,DMCB,(1,0(R3)),(23,WORK)                                  
         LA    R3,ELFDATA                                                       
         CLC   WORK(4),WORK+20     IF NOT CURRENT YEAR                          
         BE    DWNEDAT5                                                         
         MVC   0(10,R3),WORK       MOVE YYYY-MM-DD                              
         LA    R3,10(R3)                                                        
         B     DWNEDATX                                                         
DWNEDAT5 MVC   0(5,R3),WORK+5      ELSE, JUST MM-DD                             
         LA    R3,5(R3)                                                         
DWNEDATX B     PXITR3                                                           
         EJECT                                                                  
*              DOWNLOAD ELEMENT FIELD RTNS TO ELFDATA (CONTINUED)               
         SPACE 2                                                                
DWNEDTIM DS    0H                  CONVERT DATE AND TIME                        
         GOTO1 TIMECON,DMCB,3(R3),0(R3),(8,ACTIME)                              
         BAS   RE,DWNEDATL         YY/MM/DD                                     
         MVI   0(R3),C' '          SPACE                                        
         TM    STOPT,STEXTRA       IF SPECIAL DOWNLOAD WITH AN X                
         BZ    *+8                                                              
         MVI   0(R3),C'-'                                                       
         MVC   1(L'ACTIME,R3),ACTIME TIME                                       
         B     PROCXIT                                                          
         SPACE 2                                                                
DWNESORT DS    0H                  SET TGCSORT                                  
         MVC   TGCSORT,0(R3)                                                    
         TM    STOPT,STEXTRA       IF SPECIAL DOWNLOAD WITH AN X                
         BO    DWNECSEQ            SHOW IT NOW                                  
         B     PLOWXIT1            ELSE SAVE FOR LATER TLCACDQ READ             
         SPACE 2                                                                
DWNESSNG DS    0H                  SET GLOBAL SSN ONLY                          
         EDIT  (4,0(R3)),(9,TGSSN),FILL=0                                       
         TM    STOPT,STEXTRA       IF SPECIAL DOWNLOAD WITH AN X                
         BZ    PLOWXIT1            DROP DOWN TO SHOW SSN NOW                    
         SPACE 2                                                                
DWNESSN  DS    0H                  SHOW SSN                                     
         MVC   ELFDATA(L'TGSSN),TGSSN                                           
         B     PROCXIT                                                          
         SPACE 2                                                                
DWNESSNF DS    0H                  SHOW FIRST NAME FROM W4 RECORD               
         CLI   TGTYPE,TAW4TYCO                                                  
         BE    *+20                                                             
         CLI   TGTYPE,TAW4TYTR                                                  
         BE    *+12                                                             
         CLI   TGTYPE,TAW4TYES                                                  
         BNE   *+14                                                             
         MVC   ELFDATA(L'TGNAME),TGNAME                                         
         B     *+10                                                             
         MVC   ELFDATA(L'TAW4NAM1),TGNAME+L'TAW4NAM2                            
         LA    R1,L'TGNAME                                                      
         BAS   RE,CHKBAR           MAKE SURE NO VERTICAL BARS                   
         B     PROCXIT                                                          
         SPACE 2                                                                
DWNESSNL DS    0H                  SHOW LAST NAME FROM W4 RECORD                
         CLI   TGTYPE,TAW4TYCO                                                  
         BE    PLOWXIT1                                                         
         CLI   TGTYPE,TAW4TYTR                                                  
         BE    PLOWXIT1                                                         
         CLI   TGTYPE,TAW4TYES                                                  
         BE    PLOWXIT1                                                         
         MVC   ELFDATA(L'TAW4NAM2),TGNAME                                       
         LA    R1,L'TGNAME                                                      
         BAS   RE,CHKBAR           MAKE SURE NO VERTICAL BARS                   
         B     PROCXIT                                                          
         EJECT                                                                  
*              DOWNLOAD ELEMENT FIELD RTNS TO ELFDATA (CONTINUED)               
         SPACE 2                                                                
DWNESSNT DS    0H                  W4 TYPE ONLY IF CORP/ESTATE                  
         CLI   TGTYPE,TAW4TYCO                                                  
         BE    *+20                                                             
         CLI   TGTYPE,TAW4TYTR                                                  
         BE    *+12                                                             
         CLI   TGTYPE,TAW4TYES                                                  
         BNE   PLOWXIT1                                                         
         MVC   ELFDATA(1),TGTYPE                                                
         B     PROCXIT                                                          
         SPACE 2                                                                
DWNEDBL  DS    0H                                                               
         CLI   0(R3),C' '          IF DEFAULT                                   
         BE    PLOWXIT1            SKIP SHOWING                                 
         B     DWNECHAR                                                         
         SPACE 2                                                                
DWNECAM  DS    0H                                                               
         BAS   RE,IFOPTIM          IF CAL/COL/UHM OPTIMIZATION NEC.             
         BNE   DWNECAM5                                                         
         CLC   SVCAM,0(R3)         AND NO CHANGE IN ON/OFF CAMERA               
         BE    PLOWXIT1            SKIP IT                                      
         MVC   SVCAM,0(R3)         ELSE, SAVE NEW CAMERA AND DISPLAY            
         B     DWNECHAR                                                         
DWNECAM5 CLC   =C'OFF',0(R3)       IF DEFAULT                                   
         BE    PLOWXIT1            SKIP SHOWING                                 
         B     DWNECHAR                                                         
         SPACE 2                                                                
DWNEYEAR DS    0H                                                               
         BAS   RE,IFOPTIM          IF CAL/COL/UHM OPTIMIZATION NEC.             
         BNE   *+14                                                             
         CLC   SVYEAR,0(R3)        AND NO CHANGE IN YEAR                        
         BE    PLOWXIT1            SKIP IT                                      
         MVC   SVYEAR,0(R3)        ELSE, SAVE NEW YEAR AND DISPLAY              
         B     DWNECHAR                                                         
         SPACE 2                                                                
DWNEUNI  DS    0H                                                               
         BAS   RE,IFOPTIM          IF CAL/COL/UHM OPTIMIZATION NEC.             
         BNE   DWNECHAR                                                         
         CLC   SVUNI,0(R3)         AND NO CHANGE IN UNION CODE                  
         BE    PLOWXIT1                                                         
         MVC   SVUNI,0(R3)         ELSE, SAVE NEW UNION AND DISPLAY             
         B     DWNECHAR                                                         
         SPACE 2                                                                
DWNEAGY  DS    0H                                                               
         MVC   ELFDATA(L'TGAGY),TGAGY                                           
         B     PROCXIT                                                          
         SPACE 2                                                                
DWNEGUAG DS    0H                  SET TGGUA AND SHOW GUARANTEE NUMBER          
         MVC   TGGUA,0(R3)                                                      
         XC    TGGUA,=4X'FF'                                                    
         B     DWNEGUA                                                          
         SPACE 2                                                                
DWNEGUA  DS    0H                  SHOW GUARANTEE NUMBER                        
         MVC   WORK(L'TGGUA),TGGUA                                              
         XC    WORK(L'TGGUA),=4X'FF'                                            
         LA    R1,L'TGGUA          REMOVE LEADING ZEROS                         
         LA    RE,WORK                                                          
DWNEGUA5 CLI   0(RE),C'0'                                                       
         BNE   DWNEGUA8                                                         
         LA    RE,1(RE)                                                         
         BCT   R1,DWNEGUA5                                                      
         LA    R1,L'TGGUA                                                       
DWNEGUA8 BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     PROCXIT                                                          
         MVC   ELFDATA(0),0(RE)                                                 
         SPACE 2                                                                
DWNEGC   DS    0H                                                               
         XC    FULL,FULL                                                        
         L     R4,AIO              R4=A(GUARANTEE RECORD)                       
         USING TAGCD,R4                                                         
         MVI   ELCODE,TAGCELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DWNEGC5  BAS   RE,NEXTEL                                                        
         BNE   *+14                                                             
         MVC   FULL(L'TAGCEND),TAGCEND                                          
         B     DWNEGC5                                                          
*                                                                               
         OC    FULL,FULL                                                        
         BZ    PLOWXIT1                                                         
         LA    R3,FULL                                                          
         B     DWNEDATE                                                         
         EJECT                                                                  
*              DOWNLOAD ELEMENT FIELD RTNS TO ELFDATA (CONTINUED)               
         SPACE 2                                                                
DWNETLW4 DS    0H                  READ W4 RECORD INTO ATIA                     
         OI    STESTAT,STERERD     SET TO REREAD DIRECTORY POINTER              
         MVC   AIO,ATIA                                                         
         MVC   TGNAME,=CL36'** NOT FOUND **'                                    
         MVI   TGTYPE,TAW4TYCO                                                  
         GOTO1 RECVAL,DMCB,TLW4CDQ,(X'A0',0)                                    
         BNE   PHIXITR1                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAW4ELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAW4D,R4                                                         
         MVC   TGNAME(L'TAW4CRPN),TAW4NAM2                                      
         MVC   TGTYPE,TAW4TYPE                                                  
         B     PLOWXIT1                                                         
         EJECT                                                                  
*              DOWNLOAD ELEMENT FIELD RTNS TO ELFDATA (CONTINUED)               
         SPACE 2                                                                
DWNECAC  DS    0H                  READ CAST RECORD INTO ATIA                   
         OI    STESTAT,STERERD     SET TO REREAD DIRECTORY POINTER              
         MVC   AIO,ATIA                                                         
         XC    TGCAT,TGCAT                                                      
         GOTO1 RECVAL,DMCB,TLCACCDQ,0                                           
         B     DWNECAC8                                                         
DWNECAC5 GOTO1 SEQ                                                              
DWNECAC8 CLC   KEY(TLCACCAT-TLCAPD),KEYSAVE                                     
         BNE   PHIXITR1                                                         
         LA    R4,KEY                                                           
         USING TLCAPD,R4                                                        
         CLC   TLCACSEQ,TGCSORT+4                                               
         BNE   DWNECAC5                                                         
         MVC   TGCAT,TLCACCAT                                                   
         GOTO1 GETREC                                                           
         B     PLOWXIT1                                                         
         SPACE 2                                                                
DWNECAG  DS    0H                  READ CAST RECORD INTO ATIA                   
         OI    STESTAT,STERERD     SET TO REREAD DIRECTORY POINTER              
         MVC   AIO,ATIA                                                         
         GOTO1 RECVAL,DMCB,TLCAGCDQ,0  READ HIGH FOR TLCAGD KEY                 
         CLC   KEY(TLCACCAT-TLCAPD),KEYSAVE                                     
         BNE   PHIXITR1                                                         
         GOTO1 GETREC                                                           
         B     PLOWXIT1                                                         
         EJECT                                                                  
*              DOWNLOAD ELEMENT FIELD RTNS TO ELFDATA (CONTINUED)               
         SPACE 2                                                                
DWNEELCA DS    0H                                                               
         MVC   ELFDATA(2),=C'CA'                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     PXITR4                                                           
         SPACE 2                                                                
DWNETAOA MVI   ELCODE,TAOAELQ                                                   
         B     *+8                                                              
DWNETAOP MVI   ELCODE,TAOPELQ                                                   
         L     R4,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   PLOWXIT1                                                         
*                                                                               
         MVC   ELFDATA(2),=C'OA'                                                
         CLI   ELCODE,TAOAELQ                                                   
         BE    *+10                                                             
         MVC   ELFDATA(2),=C'OP'                                                
         GOTO1 AFMTEL,DMCB,(RC)    FORMAT AT R2                                 
         LA    R3,TAOANUM-TAOAD(R4)                                             
         B     DWNELAP             PROCESS SUB-ELEMENTS                         
         SPACE 2                                                                
         EJECT                                                                  
*              DOWNLOAD ELEMENT FIELD RTNS TO ELFDATA (CONTINUED)               
         SPACE 2                                                                
DWNELAP  DS    0H                  ROUTINE TO HANDLE TAOAD/TAOPD ELS.           
         ZIC   R5,0(R3)            N'SUB ELEMENTS                               
         LTR   R5,R5                                                            
         BZ    PLOWXIT1                                                         
         LA    R4,0(R3)                                                         
         LA    R4,1(R4)            R4=A(SUBDATA)                                
         USING TAOASBEL,R4                                                      
*                                                                               
DWNELAP5 XC    ELFLABEL,ELFLABEL                                                
         XC    ELFDATA,ELFDATA                                                  
         CLC   =C'HLD',TAOAUSE     PROCESS HLD                                  
         BE    DWNELAP7                                                         
         CLC   =C'SHL',TAOAUSE                                                  
         BE    DWNELAP7                                                         
         CLC   =C'ADH',TAOAUSE                                                  
         BE    DWNELAP7                                                         
         CLI   ELCODE,TAOAELQ      IF AMOUNT OVERRIDE ELEMENT OR                
         BE    *+12                                                             
         CLI   ELCDEQU,TAGUELQ     IF COMING FROM GUARANTEE ELEMENT             
         BNE   DWNELAP6                                                         
         CLC   =C'BSS',TAOAUSE     ALSO PROCESS BSS                             
         BE    DWNELAP7                                                         
         B     DWNELAP8                                                         
DWNELAP6 CLC   TAOAUSE,SPACES      ELSE, ALSO PROCESS ALL                       
         BNE   DWNELAP8                                                         
*                                                                               
DWNELAP7 MVI   ELFLABEL,C'A'                                                    
         TM    STOPT,STEXPAND                                                   
         BZ    *+10                                                             
         MVC   ELFLABEL(3),=C'USE'                                              
         MVC   ELFDATA(L'TAOAUSE),TAOAUSE                                       
         CLC   TAOAUSE,SPACES                                                   
         BNE   *+10                                                             
         MVC   ELFDATA(3),=C'ALL'                                               
         GOTO1 AFMTEL,DMCB,(RC)     FORMAT AT R2                                
*                                                                               
         XC    ELFLABEL,ELFLABEL                                                
         XC    ELFDATA,ELFDATA                                                  
         MVI   ELFLABEL,C'B'                                                    
         TM    STOPT,STEXPAND                                                   
         BZ    *+10                                                             
         MVC   ELFLABEL(3),=C'A/P'                                              
         EDIT  (4,TAOAAMT),(10,ELFDATA),ALIGN=LEFT,FLOAT=-                      
         GOTO1 AFMTEL,DMCB,(RC)    FORMAT AT R2                                 
*                                                                               
DWNELAP8 LA    R4,L'TAOASBEL(R4)                                                
         BCT   R5,DWNELAP5                                                      
         B     PXITR1R2            XIT R1 LOW AND R2 SET                        
         EJECT                                                                  
*              DOWNLOAD ELEMENT FIELD RTNS TO ELFDATA (CONTINUED)               
         SPACE 2                                                                
DWNEOPTS DS    0H                  ROUTINE TO HANDLE SPECIAL TAESD ELS          
         ZIC   R0,0(R3)            R0=N'SUB ENTRIES                             
         LTR   R0,R0                                                            
         BZ    PLOWXIT1                                                         
*                                                                               
         LA    R5,0(R3)                                                         
         LA    R5,1(R5)            R5=A(FIRST SUB ENTRY)                        
         GOTO1 AOPTELS,DMCB,(RC)                                                
         B     PXITR1R2            XIT R1 LOW AND R2 SET                        
         EJECT                                                                  
*              COMMON DOWNLOAD ELEMENT FIELD RTNS TO ELFDATA                    
         SPACE 2                                                                
*              ROUTINE CHANGES BARS TO SLASHES                                  
*                                  NTRY R1=L'DATA                               
CHKBAR   NTR1                                                                   
         LA    RE,ELFDATA                                                       
CHKBAR15 CLI   0(RE),X'6A'                                                      
         BNE   *+8                                                              
         MVI   0(RE),C'/'                                                       
         LA    RE,1(RE)                                                         
         BCT   R1,CHKBAR15                                                      
         B     PROCXIT                                                          
         SPACE 2                                                                
EDITR1F5 NTR1                                                                   
         EDIT  (R1),(5,ELFDATA),ALIGN=LEFT                                      
         B     PROCXIT                                                          
         SPACE 2                                                                
HEXOUTRF NTR1                                                                   
         LR    R0,RF               SAVE LENGTH OF INPUT                         
         GOTO1 HEXOUT,DMCB,0(R3),WORK,(RF),0                                    
         LR    RF,R0               RESTORE LENGTH OF INPUT                      
         MH    RF,=H'2'                                                         
         TM    STOPT,STEXPAND+STEXTRA  FOR EXPANDED OR X VERSION                
         BZ    HEXRF1                                                           
         BCTR  RF,0                USE FULL NUMBER OF CHARACTERS                
         EX    RF,*+8                                                           
         B     PROCXIT                                                          
         MVC   ELFDATA(0),WORK                                                  
         SPACE 1                                                                
HEXRF1   LA    RE,WORK             ELSE SUPPRESS LEADING ZEROES                 
         LA    R1,ELFDATA                                                       
HEXRF2   CLI   0(RE),C'0'                                                       
         BNE   HEXRF4                                                           
         LA    RE,1(RE)                                                         
         BCT   RF,HEXRF2                                                        
         B     PROCXIT                                                          
*                                                                               
HEXRF4   BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     PROCXIT                                                          
         MVC   ELFDATA(0),0(RE)                                                 
         B     PROCXIT                                                          
         SPACE 2                                                                
*              ROUTINE CHECKS IF NEED TO PERFORM CAL/COL OPTIMIZATION           
         SPACE 1                                                                
IFOPTIM  NTR1                                                                   
         TM    STOPT,STEXPAND      IF REGULAR DOWNLOAD                          
         BO    NO                                                               
         TM    STOPT,STEXTRA                                                    
         BO    NO                                                               
         L     R5,AKEYTAB                                                       
         AH    R5,KEYDSP                                                        
         USING KEYTABD,R5          R5=A(CURRENT KEY ENTRY)                      
         CLC   =C'COL',KEYCODE                                                  
         BE    YES                                                              
         CLC   =C'CAL',KEYCODE                                                  
         BE    YES                                                              
         CLC   =C'UHM',KEYCODE                                                  
         B     XIT                                                              
         SPACE 1                                                                
         DROP  R5                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE DISPLAYS EL=CO ELEMENT, POSSIBLY FOLLOWED BY             
*              EL=UH ELEMENT(S), THEN FINALLY THE EL=ES ELEMENT FOR             
*              AN ACTUAL COMMERCIAL                                             
*                                                                               
         USING TAESD,R4            R4=A(ACTUAL COMMERCIAL EST. ELEMENT)         
*                                  R2=A(SCRNEL)                                 
         DS    0D                                                               
PRESCID  NMOD1 0,*PRESCID                                                       
         L     RC,0(R1)                                                         
*                                                                               
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'80',TAESCOM)                             
         BE    PRESCID5                                                         
PRESCID2 BAS   RE,NEXTEL5          AND IT IS NOT ON FILE                        
         BNE   PRESCIDL                                                         
         CLI   0(R4),TAESELQ                                                    
         BNE   *+12                                                             
         TM    TAESTYPE-TAESD(R4),TAESTPER  SKIP IT & ANY PERF ELS W/IT         
         BO    PRESCID2                                                         
         B     PRESCIDN                                                         
*                                                                               
PRESCID5 BAS   RE,DOTACO           PROCESS EL=CO ELEMENT                        
*                                                                               
         BAS   RE,DOTAUH           PROCESS EL=UH ELEMENT(S)                     
*                                                                               
         BAS   RE,DOTAESC          PROCESS EL=ES (TAESTCOM) ELEMENT             
         MVI   ELCODE,0            RESET ELEMENT CODE                           
         B     PRESCIDY                                                         
*                                                                               
PRESCIDL LNR   RC,RC               RETURN CC LOW                                
         B     PRESCIDN                                                         
PRESCIDY XR    RC,RC               RETURN CC EQUAL                              
PRESCIDN LTR   RC,RC               RETURN CC NOT EQUAL                          
         XIT1  REGS=(R4)           EXIT WITH R4 SET                             
*                                                                               
PRESCIDX XIT1                      NORMAL EXIT                                  
         EJECT                                                                  
*              ROUTINE TO DISPLAY EL=CO ELEMENT                                 
*                                  R2=A(SCRNEL)                                 
DOTACO   NTR1                                                                   
         MVC   AIO,ATIA            SET WORKING IOAREA                           
         GOTO1 GETREC              GET COMMERCIAL RECORD JUST READ              
*                                                                               
         L     R4,AIO                                                           
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL5                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 APROCEL,DMCB,(RC)   BUILD ELEMENT IN SCRNEL (RESETS AIO)         
         BE    PRESCIDX                                                         
         DC    H'0'                                                             
         SPACE 2                                                                
*              ROUTINE DISPLAYS EL=UH ELEMENTS FOR EACH ESTIMATED CLA           
*              OR UPGRADED USE ON ACTUAL COMMERCIAL ESTIMATE ELEMENT            
*              AND ITS RELATED PERFORMER ELEMENTS                               
         SPACE 1                                                                
         USING TAESD,R4                                                         
DOTAUH   NTR1                                                                   
*                                                                               
DOTAUH1  ZIC   R0,TAESSBNO         R0=N'OPTIONS                                 
         LTR   R0,R0                                                            
         BZ    DOTAUH10                                                         
         LA    R5,TAESSBCD         R5=A(FIRST SUB-ELEMENT)                      
         USING TAESSBCD,R5                                                      
         SPACE 1                                                                
DOTAUH2  CLI   TAESSBCD,OPUSE      IF USE SUB-ELEMENT                           
         BNE   DOTAUH8                                                          
         GOTO1 USEVAL,DMCB,(X'80',TAESSBDT),TAESSBDT+1                          
         TM    TGUSSTA3,NWKUSE     IF NETWORK OR UPGRADED USE                   
         BO    DOTAUH4                                                          
         TM    TGUSTYST,UPGRADE                                                 
         BZ    DOTAUH8                                                          
*                                                                               
DOTAUH4  AH    R5,=AL2(LOPUSE+1)   BUMP TO NEXT SUB-ENTRY                       
         BCT   R0,*+8              DECREMENT COUNT                              
         B     DOTAUH10                                                         
         BAS   RE,LOOKUSE          LOOK FOR ACTUAL CLA OR BASE USAGE            
*                                                                               
DOTAUH8  BAS   RE,LOOKUP2          RETURNS R1=L'USE SUB-ENTRY                   
         LA    R5,1(R1,R5)                                                      
         BCT   R0,DOTAUH2          CHECK NEXT SUB-ELEMENT                       
*                                                                               
DOTAUH10 MVI   ELCODE,0                                                         
         BAS   RE,NEXTEL5          ALSO PROCESS RELATED PERFORMER ELES.         
         BNE   PRESCIDX                                                         
         CLI   0(R4),TAESELQ                                                    
         BNE   PRESCIDX                                                         
         TM    TAESTYPE,TAESTPER                                                
         BO    DOTAUH1                                                          
         B     PRESCIDX                                                         
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE GETS OTHER NECESSARY INFO ESTIMATED ABOUT USE            
*                                  R5=A(SUB-ENTRY IN ELEMENT)                   
LOOKUSE  NTR1                                                                   
         XC    FULL,FULL           CLEAR DATE                                   
*                                                                               
LOOKUSE2 BAS   RE,LOOKUP           LOOK UP SUB ENTRY IN TABLE                   
         USING OPTD,R4             RETURNS R4=A(TABLE ENTRY)                    
         TM    OPTSTAT,OPFORUSE    MUST BE USE - RELATED                        
         BZ    LOOKUSE8                                                         
         CLI   OPTCODE,OPUSE       STOP IF WE'VE REACHED NEXT USE TYPE          
         BE    LOOKUSE8                                                         
*                                                                               
         CLI   0(R4),OPDATE        IF DATE SPECIFIED                            
         BE    LOOKUSE4                                                         
         CLI   0(R4),OPCYC                                                      
         BE    LOOKUSE4                                                         
         CLI   0(R4),OPASOF                                                     
         BNE   *+10                                                             
LOOKUSE4 MVC   FULL(3),TAESSBDT    SAVE START DATE                              
*                                                                               
         CLI   0(R4),OPUSES2       IF SPECIFIC USES FOR CLA SPECIFIED           
         BNE   LOOKUSE6                                                         
         XR    RE,RE                                                            
         ICM   RE,3,TAESSBDT                                                    
         CH    RE,=H'1'            AND NOT ONE                                  
         BE    PRESCIDX                                                         
*                                                                               
LOOKUSE6 BAS   RE,LOOKUP2          RETURNS R1=L'USE SUB-ENTRY                   
         LA    R5,1(R1,R5)                                                      
         BCT   R0,LOOKUSE2         CHECK NEXT SUB-ELEMENT                       
*                                                                               
LOOKUSE8 OC    FULL(3),FULL        IF DATE DEFINED                              
         BZ    *+8                                                              
         BAS   RE,RDTLUH           READ USAGE HISTORY                           
         B     PRESCIDX                                                         
         EJECT                                                                  
*              ROUTINE BUILDS EL=UH ELEMENT IF IT FINDS ACTUAL USAGE            
*              FOR SAME CYCLE AS ESTIMATED UPGRADED USE                         
         SPACE 1                                                                
*                                  TGCOM AND TGUSCDE SET                        
RDTLUH   NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TLUHD,R4                                                         
         MVI   TLUHCD,TLUHCDQ                                                   
         MVC   TLUHCOM,TGCOM                                                    
         MVC   TLUHUSE,TGUSCDE                                                  
         GOTO1 HIGH                                                             
         B     RDTLUH5                                                          
*                                                                               
RDTLUH2  GOTO1 SEQ                                                              
*                                                                               
RDTLUH5  OC    KEY+TLUHCSEQ-TLUHD(L'TLUHCSEQ),KEY+TLUHCSEQ-TLUHD                
         BNZ   RDTLUHX                                                          
         CLC   KEY(TLUHINV-TLUHD),KEYSAVE                                       
         BNE   RDTLUHX                                                          
*                                                                               
         MVC   AIO,ATIA                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R4,ATIA                                                          
         MVI   ELCODE,TAUHELQ                                                   
         BAS   RE,GETEL5                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAUHD,R4                                                         
         CLC   FULL(3),TAUHSTRT                                                 
         BL    RDTLUH2                                                          
         CLC   FULL(3),TAUHEND                                                  
         BH    RDTLUH2                                                          
*                                  PROCESS UH ELE UNLESS ALREADY SHOWN          
         LA    R0,MAXMISC2                                                      
         LA    RE,MISCTAB2                                                      
RDTLUH8  OC    0(4,RE),0(RE)                                                    
         BZ    RDTLUH10                                                         
         CLC   0(4,RE),KEY+TLDRDA-TLDRD                                         
         BE    RDTLUHX                                                          
         LA    RE,4(RE)                                                         
         BCT   R0,RDTLUH8                                                       
         DC    H'0'                NEED TO MAKE TEMP. TABLE BIGGER              
*                                                                               
RDTLUH10 MVC   0(4,RE),KEY+TLDRDA-TLDRD                                         
         L     R2,ASCRNXT          R2=A(NEXT POSITION IN SCRNEL)                
         GOTO1 APROCEL,DMCB,(RC)   BUILD ELEMENT IN SCRNEL (RESETS AIO)         
         BE    PRESCIDX                                                         
         DC    H'0'                                                             
*                                                                               
RDTLUHX  MVC   AIO,SVAIO                                                        
         B     PRESCIDX                                                         
         EJECT                                                                  
*              ROUTINES LOOK UP SUB-ELEMENT CODE IN TABLE AND                   
*              PASSES BACK EITHER LENGTH OR TABLE ENTRY                         
         SPACE 1                                                                
         USING TAESSBCD,R5         R5=A(SUB-ELEMENT)                            
LOOKUP   NTR1                      LOOK UP SUB ENTRY IN TABLE                   
         LA    RF,1                PASS BACK A(TABLE ENTRY) IN R4               
         B     LOOK5                                                            
         SPACE 1                                                                
LOOKUP2  NTR1                                                                   
         XR    RF,RF               PASS BACK L'USE IN SUB-ELEMENT IN R1         
*                                                                               
LOOK5    L     RE,AOPTTAB          RE=A(OPTION TABLE)                           
         USING OPTD,RE                                                          
         SPACE 1                                                                
LOOK10   CLI   OPTCODE,X'FF'       TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   OPTCODE,TAESSBCD    TEST MATCH                                   
         BE    *+12                                                             
         LA    RE,OPTNEXT                                                       
         B     LOOK10                                                           
*                                                                               
         LTR   RF,RF               IF ONLY WANT LENGTH                          
         BNZ   LOOK15                                                           
         ZIC   R1,OPTSUBLN         RETURN R1=(LENGTH OF SUB-ELEMENT)            
         CLI   TAESSBCD,OPCOMNM    IF THIS IS COMML NAME                        
         BNE   *+8                                                              
         IC    R1,TAESSBDT         LENGTH IS IN FIRST DATA BYTE                 
         XIT1  REGS=(R1)                                                        
*                                                                               
LOOK15   LR    R4,RE               RETURN R4=A(TABLE ENTRY)                     
         XIT1  REGS=(R4)                                                        
         DROP  RE,R5                                                            
         EJECT                                                                  
*        ROUTINE TO PROCESS EL=ES (TAESTCOM) ELEMENT                            
*                                                                               
         USING TAESD,R4                                                         
DOTAESC  NTR1                                                                   
         L     R2,ASCRNXT          R2=A(NEXT POSITION IN SCRNEL)                
         GOTO1 APROCEL,DMCB,(RC)                                                
         BE    PRESCIDX                                                         
         DC    H'0'                                                             
         DROP  R4                                                               
         EJECT                                                                  
         GETELN (R4),DATADISP,ELCODE,5                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE FINDS FIELD IN TABLE AND PROCESSES IT                    
         SPACE 1                                                                
         USING ELTABD,R5                                                        
         DS    0D                                                               
UPEFLD   NMOD1 0,*UPEFLD*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         GOTO1 ASETELF,DMCB,(RC)   SET ELFAREA & ASCRPOS                        
*                                                                               
         ZIC   R0,ELNFLDS          R0=(N'ELEMENT FIELDS)                        
         LA    R5,ELLNQ1(R5)       R5=A(FIRST FIELD IN ELEMENT)                 
         USING ELFLDS,R5                                                        
*                                                                               
UPEFLD10 TM    ELFSTAT,ELFXUP      IF NOT EXCLUDING                             
         BO    *+14                                                             
         CLC   ELFSCODE,ELFLABEL   MATCH ON SHORT LABEL                         
         BE    UPEFLD20                                                         
         LA    R5,ELLNQ2(R5)       BUMP TO NEXT ELEMENT FIELD ENTRY             
         BCT   R0,UPEFLD10         LOOP                                         
         DC    H'0'                                                             
*                                                                               
UPEFLD20 ZIC   R3,ELFDISP                                                       
         AR    R3,R4               R3=A(FIELD IN ELEMENT)                       
*                                                                               
         XR    RF,RF               CONVERT ELFDATA TO ELEMENT FIELD             
         ICM   RF,3,ELFUP                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    RF,RB                                                            
         LA    RE,UPEFLD25                                                      
         NTR1                                                                   
         BR    RF                                                               
*                                                                               
UPEFLD25 TM    ELFSTAT,ELFADDLN    TEST ADD FIELD LENGTH TO EL. LENGTH          
         BZ    UPEFLDX                                                          
         ZIC   RE,1(R4)            GET ELEMENT LENGTH                           
         ZIC   R1,ELFEQU           ADD FIELD LENGTH                             
         CLI   0(R4),TANUELQ       UNLESS THIS IS REVISION NUMBER               
         BNE   *+8                                                              
         LA    R1,3                WHICH WE MUST FORCE TO THREE                 
         AR    RE,R1                                                            
         STC   RE,1(R4)            SAVE UPDATED ELEMENT LENGTH                  
*                                                                               
UPEFLDX  XIT1                                                                   
         EJECT                                                                  
*                                                                               
*              UPLOAD ELEMENT FIELD ROUTINES TO APPROPRIATE                     
*              DISPLACEMENT INTO ELEMENT                                        
*                                                                               
*                                  R3=A(ELEMENT FIELD)                          
         USING ELFLDS,R5           R5=A(ELEMENT FIELD TABLE ENTRY)              
         SPACE 2                                                                
UPECHAR  DS    0H                  MOVE CHARACTERS                              
         ZIC   R1,ELFEQU           MOVE FOR LENGTH OF DATA                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),ELFDATA                                                  
*                                                                               
         ZIC   R1,ELFLEN           IF LENGTH SPECIFIED IN TABLE                 
         LTR   R1,R1               PAD WITH SPACES                              
         BZ    UPEFLDX                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R3),SPACES                                                   
         B     UPEFLDX                                                          
         SPACE 2                                                                
UPENUM   DS    0H                  VALIDATE NUMBER                              
         LA    R4,ELFDATA                                                       
         ZIC   RF,ELFEQU                                                        
         BAS   RE,UPENUMVL                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   ELFLEN,1                                                         
         BNE   UPENUM2                                                          
         STC   RF,0(R3)                                                         
         B     UPEFLDX                                                          
UPENUM2  CLI   ELFLEN,2                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         STCM  RF,3,0(R3)                                                       
         B     UPEFLDX                                                          
         EJECT                                                                  
*              UPLOAD ELEMENT FIELD ROUTINES (CONTINUED)                        
         SPACE 1                                                                
UPECASH  DS    0H                  AMOUNT                                       
         LA    R4,ELFDATA                                                       
         ZIC   RF,ELFEQU                                                        
         BAS   RE,AMTVL                                                         
         ST    RF,0(R3)            SET BINARY AMOUNT                            
         B     UPEFLDX                                                          
         SPACE 2                                                                
AMTVL    NTR1                                                                   
         XR    R2,R2                                                            
         LA    R4,ELFDATA                                                       
         ZIC   RF,ELFEQU                                                        
         CLI   0(R4),C'-'          IF NEGATIVE AMOUNT                           
         BNE   AMTVL30                                                          
         LA    R2,1(R2)            SET FLAG                                     
         LA    R4,1(R4)            BUMP TO NUMBER                               
         BCTR  RF,0                                                             
         LTR   RF,RF               IF NO AMT SOMETHING WRONG                    
         BNZ   AMTVL30                                                          
         DC    H'0'                                                             
*                                                                               
AMTVL30  BCTR  RF,0                ONE FOR EXECUTE                              
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)                                                      
         ZAP   DUB,DUB             CHANGE X'0F' SIGN TO X'0C' SIGN              
         LTR   R2,R2               IF NEGATIVE AMOUNT                           
         BZ    *+10                                                             
         MP    DUB,=P'-1'          MAKE NEGATIVE                                
         CVB   RF,DUB                                                           
         XIT1  REGS=(RF)                                                        
         EJECT                                                                  
*              UPLOAD ELEMENT FIELD ROUTINES (CONTINUED)                        
         SPACE 1                                                                
UPEHEXIN DS    0H                  CONVERT INTERNAL COMML & CAST SORT           
         ZIC   RF,ELFEQU                                                        
         GOTO1 HEXIN,DMCB,ELFDATA,0(R3),(RF)                                    
         B     UPEFLDX                                                          
         SPACE 2                                                                
UPESSN   DS    0H                  SOCIAL SECURITY NUMBER                       
         PACK  DUB,ELFDATA(9)                                                   
         CVB   R1,DUB                                                           
         ST    R1,0(R3)                                                         
         B     UPEFLDX                                                          
         SPACE 2                                                                
UPEMED   DS    0H                  GET MEDIA EQUATE                             
         GOTO1 MEDVAL,DMCB,ELFDATA                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(L'TGMEEQU,R3),TGMEEQU                                          
         B     UPEFLDX                                                          
         SPACE 2                                                                
UPEUNI   DS    0H                  GET UNION EQUATE                             
         GOTO1 UNIVAL,DMCB,ELFDATA                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(L'TGUNEQU,R3),TGUNEQU                                          
         B     UPEFLDX                                                          
         SPACE 2                                                                
UPECAT   DS    0H                  GET CATEGORY EQUATE                          
         OC    ELFDATA,SPACES                                                   
         GOTO1 CATVAL,DMCB,ELFDATA                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(L'TGCAEQU,R3),TGCAEQU                                          
         B     UPEFLDX                                                          
         SPACE 2                                                                
UPETANU  LA    R0,3                FORCE INPUT TO BE 3 CHAR NUMERIC             
         ZIC   RF,ELFEQU           SO FILL WITH LEADING ZEROES                  
         CR    RF,R0                                                            
         BE    UPECHAR                                                          
         MVC   0(3,R3),=3C'0'                                                   
         SR    R0,RF                                                            
         AR    R3,R0                                                            
         B     UPECHAR                                                          
         EJECT                                                                  
*              UPLOAD ELEMENT FIELD ROUTINES (CONTINUED)                        
         SPACE 2                                                                
UPEOPTS  DS    0H                  CONVERT TAESD SUB-ELEMENTS                   
         LA    R4,ELFDATA                                                       
         ZIC   RF,ELFEQU                                                        
         BAS   RE,UPENUMVL                                                      
         LR    R0,RF               R0=(N'SUB-ELEMENTS)                          
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                SHOULD BE ZERO                               
         STC   R0,0(R3)                                                         
         LA    R3,1(R3)                                                         
         GOTO1 AOPTELS,DMCB,(RC)                                                
         B     UPEFLDX                                                          
         SPACE 2                                                                
*                                  R4=A(DATA), RF=(L'DATA)                      
UPENUMVL NTR1                                                                   
         MVC   WORK(9),=9X'F0'     INSURE NUMERIC INPUT                         
         SH    RF,=H'1'                                                         
         BM    NO                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),0(R4)                                                    
         CLC   WORK(9),=9X'F0'                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)                                                      
         CVB   RF,DUB                                                           
         XIT1  REGS=(RF)                                                        
         DROP  R5                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*        ROUTINE TO SETS ELEMENT COMPONENT IN (LABEL)=(DATA)º FORMAT            
*        IN REGULAR DOWNLOAD TO SAVE SPACE EL=CD NOW JUST =CD AND               
*        A=CCC NOW JUST ACCC                                                    
*                                  NTRY - R2=A(AREA TO SET RESULT)              
         SPACE 1                                                                
         DS    0D                                                               
FMTEL    NMOD1 0,*FMTEL**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         TM    STOPT,STEXPAND          IF REGULAR DOWNLOAD                      
         BO    FMTEL2                                                           
         TM    STOPT,STEXTRA                                                    
         BO    FMTEL2                                                           
         CLC   =C'EL',ELFLABEL         ELEMENT CODE NOT NECESSARY               
         BE    FMTEL4                                                           
FMTEL2   MVC   0(L'ELFLABEL,R2),ELFLABEL                                        
         LA    R2,L'ELFLABEL(R2)                                                
         CLI   0(R2),C' '          PT R2 TO LAST NON-SPACE                      
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,1(R2)                                                         
*                                                                               
         TM    STOPT,STEXPAND+STEXTRA  IF REGULAR DOWNLOAD                      
         BZ    *+12                FIELD = SEPERATOR NOT NEEDED                 
FMTEL4   MVI   0(R2),C'='                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         MVC   0(L'ELFDATA,R2),ELFDATA                                          
         LA    R2,L'ELFDATA(R2)                                                 
         CLI   0(R2),C' '          PT R2 TO LAST NON-SPACE                      
         BH    *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,1(R2)                                                         
*                                                                               
         MVI   0(R2),X'6A'         º                                            
         LA    R2,1(R2)                                                         
         MVI   0(R2),0             MARK END OF ELEMENTS                         
*                                                                               
         CLI   ACTNUM,ACTDWN       IF DOWNLOADING                               
         BNE   FMTELX                                                           
         L     R1,ASCRNEL          CHECK NOT PAST END OF BLOCK                  
         AH    R1,=Y(SCRNELQ)                                                   
         CR    R2,R1                                                            
         BNH   *+6                                                              
         DC    H'0'                                                             
FMTELX   XIT1  REGS=(R2)                                                        
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE SETS XX=YYº INTO ELFAREA AND                             
*              SETS ASCRPOS TO NEXT POSITION IN SCRNEL                          
*              XX   INTO ELFLABEL                                               
*              L'YY INTO ELFEQU                                                 
*              YY   INTO ELFDATA                                                
         SPACE 1                                                                
         DS    0D                                                               
SETELF   NMOD1 0,*FAREA**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R3,ASCRPOS          R3=A(CURRENT POSITION IN SCRNEL)             
*                                                                               
         LA    R2,ELFLABEL                                                      
         XC    0(L'ELFLABEL,R2),0(R2)                                           
         MVI   BYTE,C'='                                                        
         BAS   RE,MVCCHARS                                                      
*                                                                               
         LA    R2,ELFDATA                                                       
         XC    0(L'ELFDATA,R2),0(R2)                                            
         MVI   BYTE,X'6A'                                                       
         BAS   RE,MVCCHARS                                                      
         STC   R1,ELFEQU           SAVE LENGTH OF ELFDATA                       
*                                                                               
         ST    R3,ASCRPOS                                                       
         XIT1                                                                   
         SPACE 2                                                                
*              MOVE LABEL OR DATA INTO ELFAREA                                  
         SPACE 1                                                                
MVCCHARS DS    0H                                                               
         XR    R1,R1               LENGTH OF DATA                               
*                                                                               
MVCCHAR5 CLI   0(R3),0             TEST END OF SCREEN DATA                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R3),BYTE        FIND MATCH ON END DELIMITER                  
         BNE   MVCCHAR7                                                         
         LA    R3,1(R3)            BUMP PAST DELIMITER                          
         BER   RE                  AND EXIT                                     
*                                                                               
MVCCHAR7 MVC   0(1,R2),0(R3)       MOVE CHARACTER                               
         LA    R1,1(R1)            INCREMENT LENGTH COUNTER                     
         LA    R2,1(R2)            BUMP TO NEXT POSITION IN SCRNEL              
         LA    R3,1(R3)            BUMP TO NEXT POSITION ON SCREEN              
         B     MVCCHAR5            LOOP                                         
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO SET ELETYPE FOR TABLE TRAVERSING ON DOWNLOAD          
*                                  NTRY - R4=A(ELEMENT)                         
         SPACE 1                                                                
         DS    0D                                                               
SETETYPE NMOD1 0,SETETYPE                                                       
         L     RC,0(R1)                                                         
         L     R5,AKEYTAB                                                       
         AH    R5,KEYDSP                                                        
         USING KEYTABD,R5          R5=A(CURRENT KEY ENTRY)                      
*                                                                               
         MVI   ELETYPE,0           PRE-CLEAR ELEMENT TYPE                       
*                                                                               
         L     RE,AIO                                                           
         CLI   0(RE),TLESCDQ       IF ESTIMATE RECORD                           
         BNE   SETETYP5                                                         
         CLI   0(R4),TAESELQ       IF ESTIMATE ELEMENT                          
         BNE   SETETYPX                                                         
         MVC   ELETYPE,3(R4)                                                    
         B     SETETYPX                                                         
*                                                                               
SETETYP5 CLI   0(RE),TLUHCDQ       IF USAGE HISTORY RECORD                      
         BNE   SETETYP8                                                         
         BAS   RE,SETUHTYP                                                      
         B     SETETYPX                                                         
*                                                                               
SETETYP8 CLI   0(RE),TLCOCDQ       IF COMMERCIAL RECORD                         
         BNE   SETETYP9                                                         
         CLI   0(R4),TACOELQ       AND COMMERCIAL ELEMENT                       
         BNE   SETETYPX                                                         
         CLC   =C'ES',KEYCODE      AND FROM ESTIMATE OR COMMERCIAL READ         
         BE    *+14                                                             
         CLC   =C'CO',KEYCODE                                                   
         BNE   SETETYPX                                                         
         MVI   ELETYPE,C'A'        DISTINGUSH FROM TAOCD W/ JUST CID            
         B     SETETYPX                                                         
*                                                                               
SETETYP9 CLI   0(RE),TLAYCDQ       IF AGENCY RECORD                             
         BNE   SETETYPX                                                         
         CLC   =C'INI',KEYCODE     AND FROM INITIALIZATION                      
         BNE   SETETYPX                                                         
         MVI   ELETYPE,C'A'                                                     
SETETYPX XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
*              ROUTINE TO SET ELETYPE FOR TAUHD ELEMENT                         
         SPACE 1                                                                
SETUHTYP NTR1                                                                   
         TM    TGUSSTA3,NWKUSE                                                  
         BZ    *+12                                                             
         MVI   ELETYPE,C'A'                                                     
         B     SETETYPX                                                         
*                                                                               
         CLI   TGUSEQU,UDEM                                                     
         BE    *+12                                                             
         CLI   TGUSEQU,UCDM                                                     
         BNE   *+12                                                             
         MVI   ELETYPE,C'B'                                                     
         B     SETETYPX                                                         
*                                                                               
         CLI   TGUSEQU,UTAG                                                     
         BNE   *+12                                                             
         MVI   ELETYPE,C'C'                                                     
         B     SETETYPX                                                         
*                                                                               
         CLI   TGUSEQU,URRN                                                     
         BNE   *+12                                                             
         MVI   ELETYPE,C'D'                                                     
         B     SETETYPX                                                         
*                                                                               
*        CLI   TGUSEQU,UIFB                                                     
*        BNE   *+12                                                             
*        MVI   ELETYPE,C'E'                                                     
*        B     SETETYPX                                                         
*                                                                               
         CLI   TGUSEQU,UFGR                                                     
         BNE   *+12                                                             
         MVI   ELETYPE,C'F'                                                     
         B     SETETYPX                                                         
*                                                                               
         CLI   TGUSEQU,UCBL                                                     
         BE    *+12                                                             
         CLI   TGUSEQU,USCB                                                     
         BNE   *+12                                                             
         MVI   ELETYPE,C'G'                                                     
         B     SETETYPX                                                         
*                                                                               
         CLI   TGUSEQU,ULCB                                                     
         BNE   *+12                                                             
         MVI   ELETYPE,C'H'                                                     
         B     SETETYPX                                                         
*                                                                               
         CLI   TGUSEQU,USNW                                                     
         BE    SETUHTY5                                                         
         CLI   TGUSEQU,UWSP                                                     
         BE    SETUHTY5                                                         
         CLI   TGUSEQU,UWSC                                                     
         BE    SETUHTY5                                                         
         CLI   TGUSEQU,UWSM                                                     
         BE    SETUHTY5                                                         
         CLI   TGUSEQU,UNET                                                     
         BE    SETUHTY5                                                         
         CLI   TGUSEQU,UADC                                                     
         BE    SETUHTY5                                                         
         CLI   TGUSEQU,UADW                                                     
         BE    SETUHTY5                                                         
         CLI   TGUSEQU,USWS                                                     
         BNE   SETETYPX                                                         
SETUHTY5 MVI   ELETYPE,C'I'                                                     
         B     SETETYPX                                                         
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE SETS CORRECT ENTRY ADDRESS OF ELEMENT                    
*                                  NTRY P2 BYTE 0 X'80' MATCH ON CODE           
*                                       P2 A(ELEMENT)                           
*                                  XIT - AELENTRY SET                           
         SPACE 1                                                                
         DS    0D                                                               
GETNTRY  NMOD1 0,*GETNTRY                                                       
         L     RC,0(R1)                                                         
*                                                                               
         MVC   BYTE,4(R1)                                                       
         L     R4,4(R1)                                                         
         XC    AELENTRY,AELENTRY                                                
*                                                                               
         L     R5,AELTAB           R5=A(ELEMENT TABLE)                          
         USING ELTABD,R5                                                        
*                                                                               
GETNTRY1 CLI   0(R5),X'FF'         TEST END OF TABLE                            
         BE    GETNTRYX                                                         
*                                                                               
         TM    BYTE,X'80'          IF WANT MATCH ON CODE                        
         BZ    GETNTRY3                                                         
         CLC   ELCD,0(R4)          MATCH ON TWO CHARACTER CODE                  
         BNE   GETNTRY5                                                         
         B     *+14                                                             
*                                                                               
GETNTRY3 CLC   ELCDEQ,0(R4)        ELSE, MATCH ON ELEMENT EQUATE                
         BNE   GETNTRY5                                                         
*                                                                               
         CLC   ELTYPE,ELETYPE      MATCH ON TYPE                                
         BNE   GETNTRY5                                                         
*                                                                               
         CLI   ELSTAT,0            MATCH ON SPECIFIC RECORD                     
         BE    GETNTRY8                                                         
*                                                                               
         TM    ELSTAT,ELSFRESL                                                  
         BO    *+12                                                             
         TM    ELSTAT,ELSFORES                                                  
         BZ    *+12                                                             
         CLI   MYKCODE,TLESCDQ                                                  
         BE    GETNTRY8                                                         
         TM    ELSTAT,ELSFORCP                                                  
         BZ    *+12                                                             
         CLI   MYKCODE,TLCLCDQ                                                  
         BE    GETNTRY8                                                         
         TM    ELSTAT,ELSFORTB                                                  
         BZ    *+12                                                             
         TM    STESTAT,STETAB                                                   
         BO    GETNTRY8                                                         
*                                                                               
GETNTRY5 BAS   RE,BMPELR5          BUMP TO NEXT ELEMENT ENTRY IN TAB            
         B     GETNTRY1                                                         
*                                                                               
GETNTRY8 ST    R5,AELENTRY         SAVE A(ELEMENT ENTRY)                        
GETNTRYX XIT1                                                                   
         EJECT                                                                  
*              ROUTINE BUMPS R5 TO NEXT ELEMENT TABLE ENTRY                     
         SPACE 1                                                                
         USING ELTABD,R5                                                        
BMPELR5  DS    0H                                                               
         ZIC   RF,ELNFLDS          (N'EL FIELDS * L'OF EACH EL FIELD)           
         MH    RF,=Y(ELLNQ2)                                                    
         LA    R5,ELLNQ1(RF,R5)    +L'ELEMENT HEADER                            
         BR    RE                  RETURNS R5=A(NEXT ELEMENT ENTRY)             
         SPACE 2                                                                
         DROP  R5                                                               
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD NECESSARY INFO TO MISCTAB                         
*                                                                               
*                                  R4=A(RECORD)                                 
         DS    0D                                                               
ADDTAB   NMOD1 0,*ADDTAB*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         LTR   R4,R4               IF NO RECORD                                 
         BNZ   ADDTAB5                                                          
*                                                                               
         LA    R0,MAXTBL                                                        
         LA    R2,MISCTAB          R2=A(TABLE)                                  
         BAS   RE,ADDCTYP          ADD COMMERCIAL TYPES AS ELS                  
         BAS   RE,ADDADEM          ADD ADDENDUM STATES AS ELS                   
         BAS   RE,ADDCCTYP         ADD CANADIAN COMMERCIAL TYPES AS ELS         
         B     ADDTABX                                                          
*                                                                               
ADDTAB5  CLI   0(R4),TLCACDQ       IF CAST RECORD                               
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,ADDMUS           ADD MUSICIANS AS COMBINED ELS.               
ADDTABX  XIT1                                                                   
         EJECT                                                                  
*              ROUTINE ADDS DUMMY ELEMENT FOR COMMERCIAL TYPES                  
         SPACE 1                                                                
*                                  R2=A(FIRST AVAIL. ENTRY IN MISCTAB)          
ADDCTYP  NTR1                                                                   
         L     R1,TGACOMT          R1=A(COMMERCIAL TYPE TABLE)                  
         USING CTYD,R1                                                          
         LA    R4,ELEMENT                                                       
         USING TA02D,R4                                                         
*                                                                               
ADDCTYP2 XC    ELEMENT,ELEMENT     BUILD DUMMY ELEMENT                          
         MVI   TA02EL,TA02ELQ                                                   
         MVI   TA02LEN,TA02LNQ                                                  
*                                                                               
         CLI   0(R1),X'FF'         IF NOT END OF TABLE                          
         BE    ADDCTYPX                                                         
         MVC   TA02CTYP,CTYEQU     SET CODE IN ELEMENT                          
         MVC   TA02CTYN,CTYNAME    SET NAME IN ELEMENT                          
         BAS   RE,SETAHLD          SET TA02AHLD                                 
         MVC   0(TA02LNQ,R2),ELEMENT THEN SET ELEMENT IN TABLE                  
         LA    R2,TA02LNQ(R2)                                                   
         BCT   R0,*+6                                                           
         DC    H'0'                                                             
         LA    R1,CTYNEXT          PT TO NEXT ENTRY IN TABLE                    
         B     ADDCTYP2                                                         
*                                                                               
ADDCTYPX XIT1  REGS=(R2)           EXIT WITH R2 AT TABLE END                    
         DROP  R1                                                               
         SPACE 2                                                                
*              ROUTINE SETS Y IN ELEMENT IF COMML TYPE GENERATES                
*              AUTO HOLDING FEES                                                
         SPACE 1                                                                
         USING TA02D,R4                                                         
SETAHLD  NTR1                                                                   
         LA    RE,NAHLDTAB         RE=A(COMML'S DON'T GET HOLDING FEES)         
SETAHLD5 CLI   0(RE),0             IF COMMERCIAL TYPE NOT IN TABLE              
         BE    SETAHLD8                                                         
         CLC   TA02CTYP,0(RE)                                                   
         BE    ADDTABX                                                          
         LA    RE,1(RE)                                                         
         B     SETAHLD5                                                         
*                                                                               
SETAHLD8 MVI   TA02AHLD,C'Y'       SET Y IN ELEMENT( GETS AUTO HLDS)            
         B     ADDTABX                                                          
         EJECT                                                                  
*              ROUTINE ADDS DUMMY ELEMENT FOR ADDENDUM STATES                   
         SPACE 1                                                                
*                                  R2=A(FIRST AVAIL. ENTRY IN MISCTAB)          
ADDADEM  DS    0H                                                               
         L     R1,TGAUNITS         R1=A(UNITS TABLE)                            
         USING TALUNITD,R1                                                      
*                                                                               
         XC    ELEMENT,ELEMENT     BUILD DUMMY ELEMENT                          
         LA    R4,ELEMENT                                                       
         USING TA03D,R4                                                         
         MVI   TA03EL,TA03ELQ                                                   
         MVI   TA03LEN,TA03LNQ                                                  
*                                                                               
ADDADEM2 CLI   0(R1),X'FF'         IF NOT END OF TABLE                          
         BER   RE                                                               
         TM    TALUSTAT,TALUOKAD                                                
         BZ    ADDADEM4                                                         
         MVC   TA03ADST,TALUCODE   SET STATE CODE IN ELEMENT                    
         MVC   TA03ADEM,TALUNAME   SET STATE NAME IN ELEMENT                    
         MVC   0(TA03LNQ,R2),ELEMENT THEN SET ELEMENT IN TABLE                  
         LA    R2,TA03LNQ(R2)                                                   
         BCT   R0,*+6                                                           
         DC    H'0'                                                             
*                                                                               
ADDADEM4 LA    R1,TALUNEXT         PT TO NEXT ENTRY IN TABLE                    
         B     ADDADEM2                                                         
         DROP  R1                                                               
         EJECT                                                                  
*              ROUTINE ADDS DUMMY ELEMENT FOR COMMERCIAL TYPES                  
         SPACE 1                                                                
*                                  R2=A(FIRST AVAIL. ENTRY IN MISCTAB)          
ADDCCTYP DS    0H                                                               
         L     R1,TGACTYPS         R1=A(CANADIAN COMML TYPE TABLE)              
         USING CCTYPD,R1                                                        
*                                                                               
         XC    ELEMENT,ELEMENT     BUILD DUMMY ELEMENT                          
         LA    R4,ELEMENT                                                       
         USING TA04D,R4                                                         
         MVI   TA04EL,TA04ELQ                                                   
         MVI   TA04LEN,TA04LNQ                                                  
*                                                                               
ADDCCTY2 CLI   0(R1),X'FF'         IF NOT END OF TABLE                          
         BER   RE                                                               
         MVC   TA04CCDE,CCTYPCDE   SET CODE IN ELEMENT                          
         MVC   TA04CCN,CCTYPNME    SET NAME IN ELEMENT                          
         MVC   0(TA04LNQ,R2),ELEMENT THEN SET ELEMENT IN TABLE                  
         LA    R2,TA04LNQ(R2)                                                   
         BCT   R0,*+6                                                           
         DC    H'0'                                                             
*                                                                               
         LA    R1,CCTYPNXT         PT TO NEXT ENTRY IN TABLE                    
         B     ADDCCTY2                                                         
         DROP  R1                                                               
         EJECT                                                                  
*              TABLE CONSISTS OF COMBINED MUSICIANS                             
         SPACE 1                                                                
         USING TLCAD,R4            R4=A(CAST RECORD)                            
ADDMUS   NTR1                                                                   
*                                  GET OV1% FOR ALL USE INTO FULL               
         GOTO1 GETOV1,DMCB,(X'80',SPACES),FULL                                  
         BAS   RE,ADJCNT           RTNS HALF W/ADJUSTED COUNT FOR CAT           
*                                                                               
         MVI   ELCODE,TACAELQ                                                   
         BAS   RE,GETEL2                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACAD,R4                                                         
         GOTO1 SETOV2,DMCB,(R4),AIO,SPACES                                      
*                                                                               
         CLC   =C'AFM',TACAUN      IF UNION IS AFM                              
         BNE   *+10                                                             
         XC    TACAUN,TACAUN       DEFAULT IT                                   
         CLI   TACACORP,C' '       IF CORP                                      
         BNH   *+8                                                              
         MVI   TACACORP,C'Y'       LUMP THEM TOGETHER                           
*                                                                               
         LA    R0,MAXTBL           R0=MAX N'ENTRIES                             
         LA    R2,MISCTAB          R2=A(TABLE)                                  
         USING TA01D,R2                                                         
*                                                                               
ADDMUS2  OC    0(TA01LNQ,R2),0(R2) TEST END OF TABLE                            
         BZ    ADDMUS10                                                         
         BAS   RE,CKMUSDUP         TEST DUPLICATE MUSICIAN                      
         BE    ADDMUS20                                                         
         LA    R2,TA01LNQ(R2)      BUMP TO NEXT ENTRY                           
         BCT   R0,ADDMUS2                                                       
         DC    H'0'                INCREASE MAXTBL                              
*                                                                               
ADDMUS10 MVI   TA01EL,TA01ELQ      ADD NEW MUSICIAN ENTRY                       
         MVI   TA01LEN,TA01LNQ                                                  
         MVC   TA01CAM,TACAONOF                                                 
         MVC   TA01UN,TACAUN                                                    
         MVC   TA01CORP,TACACORP                                                
         MVC   TA01DBL,TACADBL                                                  
         MVC   TA01OV1,FULL                                                     
         MVC   TA01OV2,TACAOV2                                                  
*                                                                               
ADDMUS20 XR    R1,R1               INCREMENT COUNT FOR ROW                      
         ICM   R1,3,TA01CNT                                                     
         AH    R1,HALF                                                          
         STCM  R1,3,TA01CNT                                                     
         B     ADDTABX                                                          
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO CHECK CURRENT MUSICIAN DETAILS                        
*              MATCH ENTRY IN THE TABLE                                         
         SPACE 1                                                                
         USING TA01D,R2            R2=A(MUSICIAN TABLE ENTRY)                   
         USING TACAD,R4            R4=A(CAST DETAILS ELEMENT)                   
CKMUSDUP NTR1                                                                   
         CLC   TACAONOF,TA01CAM    CHECK MATCH ON CAMERA                        
         BNE   ADDTABX                                                          
         CLC   TACADBL,TA01DBL     CHECK MATCH ON DOUBLES                       
         BNE   ADDTABX                                                          
         CLC   TACACORP,TA01CORP   CHECK MATCH ON CORP                          
         BNE   ADDTABX                                                          
         CLC   TACAUN,TA01UN       CHECK MATCH ON UNION                         
         BNE   ADDTABX                                                          
         CLC   TACAOV2,TA01OV2     CHECK MATCH ON OV2                           
         BNE   ADDTABX                                                          
         CLC   FULL,TA01OV1        CHECK MATCH ON OV1                           
         B     ADDTABX                                                          
         DROP  R2,R4                                                            
         SPACE 2                                                                
*        ADJUST MUSICIAN CATEGORY COUNT - RETURNS HALF                          
         SPACE 1                                                                
ADJCNT   NTR1                                                                   
         XR    R1,R1                                                            
         LA    R2,AFMTAB           R2=A(ADJUSTED COUNTS FOR MUSICIANS)          
*                                                                               
ADJCNT2  CLI   0(R2),X'FF'         TEST END OF TABLE                            
         BE    ADJCNT8                                                          
         CLC   TGCAEQU,2(R2)       TEST MATCH ON CATEGORY EQUATE                
         BE    *+12                                                             
         LA    R2,L'AFMTAB(R2)                                                  
         B     ADJCNT2                                                          
*                                                                               
         XR    R0,R0               CALCULATE ADJUSTED COUNT                     
         LH    R1,0(R2)                                                         
         LTR   R1,R1                                                            
         BZ    ADJCNT8                                                          
         D     R0,=F'100'                                                       
*                                                                               
ADJCNT8  LA    R1,1(R1)                                                         
         STH   R1,HALF             RETURN IN HALF                               
         B     ADDTABX                                                          
         EJECT                                                                  
         GETEL2 (R4),DATADISP,ELCODE                                            
         SPACE 2                                                                
       ++INCLUDE TASYSAFM                                                       
         SPACE 2                                                                
       ++INCLUDE TASYSNAHLD                                                     
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO ADD ELEMENT TO ESTIMATE IOAREA                        
         SPACE 1                                                                
         DS    0D                                                               
MYADDL   NMOD1 0,*MYADDL*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         LA    R4,ELEMENT                                                       
         CLI   0(R4),TACMELQ       IF COMMENT ELEMENT                           
         BNE   MYADDL1                                                          
         USING TACMD,R4                                                         
         MVI   TACMTYPE,TACMTYPG   SET TYPE                                     
         B     MYADDL9                                                          
*                                                                               
MYADDL1  CLI   0(R4),TANUELQ       IF ESTIMATE REVISION NUMBER ELEMENT          
         BNE   MYADDL2                                                          
         USING TANUD,R4                                                         
         MVI   TANUTYPE,TANUTREV   SET TYPE IN ELEMENT                          
         B     MYADDL9                                                          
*                                                                               
MYADDL2  CLI   0(R4),TAENELQ       IF ESTIMATE NARRATIVE ELEMENT                
         BNE   MYADDL3                                                          
         USING TAEND,R4                                                         
         MVC   TAENSEQ,ENSEQ                                                    
         ZIC   R1,ENSEQ                                                         
         LA    R1,1(R1)                                                         
         STC   R1,ENSEQ                                                         
         B     MYADDL9                                                          
*                                                                               
MYADDL3  CLI   0(R4),TAEMELQ       IF ESTIMATE MISCELLANOUS ADDITIONS           
         BNE   MYADDL4                                                          
         USING TAEMD,R4                                                         
         MVC   TAEMSEQ,EMSEQ                                                    
         ZIC   R1,EMSEQ                                                         
         LA    R1,1(R1)                                                         
         STC   R1,EMSEQ                                                         
         B     MYADDL9                                                          
*                                                                               
MYADDL4  CLI   0(R4),TAESELQ       IF ESTIMATE DETAILS ELEMENT                  
         BNE   MYADDL9                                                          
         BAS   RE,MYADDES          SPECIAL HANDLING NEEDED                      
         BNE   MYADDLER                                                         
*                                  ADD ELEMENT TO RECORD                        
MYADDL9  GOTO1 HELLO,DMCB,(C'P',SYSFIL),AIO,(R4),0                              
         CLI   12(R1),5                                                         
         BE    MYADDL15                                                         
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
MYADDL15 L     R4,AIO                                                           
         USING TLESD,R4                                                         
         LH    RE,=H'5700'         CHECK REACHED MAX LENGTH                     
         CLI   HAVEAC40,C'Y'                                                    
         BNE   *+8                                                              
         AH    RE,=AL2(TAACLNQ)                                                 
         CH    RE,TLESLEN                                                       
         BNL   MYADDLX                                                          
*                                                                               
MYADDLER NI    STEOPTH+4,X'DF'     SET TO RE-START W/NEXT HIT OF ENTER          
         MVI   ERROR,TOOLONG       IF HIGHER, GIVE RECORD TOO LONG MSG          
         LA    R2,STEKEYH                                                       
         GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
MYADDLY  XR    RC,RC               RETURN CC EQUAL                              
MYADDLN  LTR   RC,RC               RETURN CC NOT EQUAL                          
MYADDLX  XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO FIGURE OUT CORRECT SEQUENCE NUMBER TO                 
*              ADD ELEMENT WITH, SO ELEMENTS ARE IN SORTED ORDER                
         SPACE 1                                                                
         USING TAESD,R4                                                         
MYADDES  NTR1                                                                   
         CLI   TAESTYPE,TAESTCLI   IF CLIENT ELEMENT                            
         BNE   *+12                                                             
         BAS   RE,MYADDCL          FIND SEQUENCE NUMBER                         
         B     MYADDLX             CC SET                                       
*                                                                               
         CLI   TAESTYPE,TAESTPRD   IF PRODUCT ELEMENT                           
         BNE   *+12                                                             
         BAS   RE,MYADDPR          FIND SEQ # UNDER CLIENT                      
         B     MYADDLX             CC SET                                       
*                                                                               
         TM    TAESTYPE,TAESTCOM   IF HYPO OR REGULAR COMMERCIAL                
         BZ    *+18                                                             
         XC    HYPCNTS,HYPCNTS     CLEAR HYPO PERFORMER COUNTS                  
         BAS   RE,MYADDCO          FIND SEQUENCE # UNDER CLI/PRD                
         B     MYADDLX             CC SET                                       
*                                                                               
         TM    TAESTYPE,TAESTPER   IF CAST (HYPO OR REQGULAR)                   
         BO    *+6                                                              
         DC    H'0'                                                             
         CLI   TAESTYPE,TAESTHPE   IF HYPO PERFORMER                            
         BNE   MYADDES6                                                         
         CLI   TAESHNPR,0          IF N'PERFORMERS OF THIS TYPE NOT SET         
         BNE   *+8                                                              
         MVI   TAESHNPR,1          SET TO ONE                                   
         CLI   HYPCNT,MAXHYP       IF ADDED MAX FOR PAGE                        
         BL    MYADDES2                                                         
         ZIC   R1,HPGCNT           INCREMENT PAGE COUNT                         
         LA    R1,1(R1)                                                         
         STC   R1,HPGCNT                                                        
         MVI   HYPCNT,0            RESET NUMBER OF PERFS ON PAGE                
MYADDES2 MVC   TAESHPG,HPGCNT      AND SET PAGE COUNT IN ELEMENT                
         ZIC   R1,HYPCNT           INCREMENT NUMBER OF PERFS ON PAGE            
         LA    R1,1(R1)                                                         
         STC   R1,HYPCNT                                                        
MYADDES6 BAS   RE,MYADDCA          FIND SEQUENCE # UNDER COMMERCIAL             
         B     MYADDLX             CC SET                                       
         EJECT                                                                  
*              ROUTINE TO LOOP THROUGH RECORD TRYING TO FIND                    
*              APPROPRIATE SEQUENCE # FOR CLIENT ELEMENT                        
         SPACE 1                                                                
MYADDCL  NTR1                                                                   
         XR    R2,R2               INITIALIZE SEQUENCE NUMBER                   
         MVI   ANYMORE,C'Y'        MORE ELEMENTS TO COME                        
*                                                                               
         USING TAESD,R4                                                         
         L     R4,AIO                                                           
         MVI   ELCODE,TAESELQ                                                   
         BAS   RE,GETEL3                                                        
         B     *+8                                                              
MYADDCL5 BAS   RE,NEXTEL3                                                       
         BE    *+12                                                             
         MVI   ANYMORE,C'N'        NO MORE ELEMENTS                             
         B     MYADDCL8                                                         
*                                                                               
         ZIC   R2,TAESSEQ          SET R2=(LATEST SEQUENCE NUMBER)              
         CLC   TAESTYPE,ELEMENT+TAESTYPE-TAESD                                  
         BNE   MYADDCL5                                                         
         CLC   TAESDATA,ELEMENT+TAESDATA-TAESD                                  
         BL    MYADDCL5                                                         
*                                                                               
         STC   R2,ELEMENT+TAESSEQ-TAESD USE LATEST SEQ # FOR THIS EL.           
         B     MYADDCLX                                                         
*                                                                               
MYADDCL8 LTR   R2,R2               SET SEQUENCE # IN CLIENT EL. TO ADD          
         BZ    *+8                                                              
         LA    R2,1(R2)                                                         
         STC   R2,ELEMENT+TAESSEQ-TAESD                                         
*                                                                               
MYADDCLX MVC   SVCLISEQ,ELEMENT+TAESSEQ-TAESD  SAVE LAST CLIENT SEQ #           
         XC    SVPRDSEQ(ESSEQSX-SVPRDSEQ),SVPRDSEQ  CLEAR SEQ'S BELOW           
         BAS   RE,DORESEQ               RESET SEQ #'S ON REMAINING ELS          
         B     MYADDLX                  CC SET                                  
         EJECT                                                                  
*              ROUTINE TO FIND SEQUENCE NUMBER FOR PRODUCT ELEMENT              
*              BY LOOPING THROUGH RECORD STARTING AT CLIENT ELEMENT             
         SPACE 1                                                                
MYADDPR  NTR1                                                                   
         MVI   ELCODE,TAESELQ      FIND LAST CLIENT ELEMENT PROCESSED           
         GOTO1 GETL,DMCB,(1,SVCLISEQ)                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,TGELEM                                                        
         ZIC   R2,SVCLISEQ                                                      
         MVI   ANYMORE,C'Y'        SET MORE ELEMENTS TO COME                    
*                                                                               
         USING TAESD,R4                                                         
MYADDPR5 BAS   RE,NEXTEL3          GET NEXT ELEMENT                             
         BE    *+16                                                             
         MVI   ANYMORE,C'N'        SET NO MORE ELEMENTS                         
         LA    R2,1(R2)            SET TO USE NEXT SEQUENCE NUMBER              
         B     MYADDPR8                                                         
*                                                                               
         ZIC   R2,TAESSEQ          SET R2=LATEST SEQUENCE NUMBER                
         CLI   TAESTYPE,TAESTCLI   IF NOT ANOTHER CLIENT EL.                    
         BE    MYADDPR8                                                         
         CLC   TAESTYPE,ELEMENT+TAESTYPE-TAESD  LOOP THROUGH FOR PRDS           
         BNE   MYADDPR5                                                         
         CLC   TAESDATA,ELEMENT+TAESDATA-TAESD  UNTIL FIND CORRECT SPOT         
         BL    MYADDPR5                                                         
*                                                                               
MYADDPR8 STC   R2,ELEMENT+TAESSEQ-TAESD USE LATEST SEQ # FOR THIS EL.           
         STC   R2,SVPRDSEQ              SAVE LAST PRD SEQ NUMBER                
         XC    SVCOSEQ(ESSEQSX-SVCOSEQ),SVCOSEQ CLEAR SEQ #'S BELOW             
         BAS   RE,DORESEQ               RESET SEQ #'S ON REMAINING ELS          
         B     MYADDLX                  CC SET                                  
         EJECT                                                                  
*              ROUTINE FIND SEQUENCE NUMBER FOR ACTUAL OR HYPO COMML            
*              ELEMENT, BY LOOPING THROUGH RECORD STARTING AT LAST              
*              CLIENT AND/OR PRODUCT ELEMENT                                    
         SPACE 1                                                                
MYADDCO  NTR1                                                                   
         MVC   BYTE,SVCLISEQ       USE LAST CLIENT SEQUENCE NUMBER              
         CLI   SVPRDSEQ,0          IF PRODUCT SEQUENCE SPECIFIED                
         BE    *+10                                                             
         MVC   BYTE,SVPRDSEQ       USE LAST PRODUCT SEQUENCE NUMBER             
*                                                                               
         MVI   ELCODE,TAESELQ      FIND LAST CLIENT/PRODUCT ELEMENT             
         GOTO1 GETL,DMCB,(1,BYTE)                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,TGELEM                                                        
         ZIC   R2,BYTE                                                          
         MVI   ANYMORE,C'Y'        SET MORE ELEMENTS TO COME                    
*                                                                               
         USING TAESD,R4                                                         
MYADDCO5 BAS   RE,NEXTEL3          GET NEXT ELEMENT                             
         BE    *+16                                                             
         MVI   ANYMORE,C'N'        SET NO MORE ELEMENTS                         
         LA    R2,1(R2)            SET TO USE NEXT SEQUENCE NUMBER              
         B     MYADDCO8                                                         
*                                                                               
         ZIC   R2,TAESSEQ          SET R2=(LASTEST SEQUENCE NUMBER)             
         CLI   TAESTYPE,TAESTCLI   LOOP TILL HIT NEXT CLI/PRD EL.               
         BE    MYADDCO8                                                         
         CLI   TAESTYPE,TAESTPRD                                                
         BE    MYADDCO8                                                         
         CLI   ELEMENT+TAESTYPE-TAESD,TAESTHCO                                  
         BE    *+12                                                             
         CLI   TAESTYPE,TAESTHCO                                                
         BE    MYADDCO8                                                         
*                                                                               
         CLC   TAESTYPE,ELEMENT+TAESTYPE-TAESD LOOP THROUGH COMML               
         BNE   MYADDCO5                                                         
         CLC   TAESDATA,ELEMENT+TAESDATA-TAESD UNTIL FIND CORRECT SPOT          
         BL    MYADDCO5                                                         
*                                                                               
MYADDCO8 STC   R2,ELEMENT+TAESSEQ-TAESD USE LATEST SEQ # FOR THIS EL.           
         STC   R2,SVCOSEQ               SAVE LAST COMML SEQ NUMBER              
         MVI   SVCASEQ,0                CLEAR CAST SEQUENCE NUMBER              
         BAS   RE,DORESEQ               RESET SEQ #'S ON REMAINING ELS          
         B     MYADDLX                  CC SET                                  
         EJECT                                                                  
*              ROUTINE TO SET SEQUENCE NUMBER FOR ACTUAL OR HYPO                
*              PERFORMER EL BASED ON SEQUENCE NUMBER OF LAST COMML EL           
         SPACE 1                                                                
MYADDCA  NTR1                                                                   
         MVC   BYTE,SVCOSEQ        LAST COMML SEQUENCE NUMBER                   
         CLI   SVCASEQ,0           IF LAST CAST SEQUENCE SPECIFIED              
         BE    *+10                                                             
         MVC   BYTE,SVCASEQ        USE THAT INSTEAD                             
*                                                                               
         MVI   ELCODE,TAESELQ      FIND LAST COMML OR PERF. ELEMENT             
         GOTO1 GETL,DMCB,(1,BYTE)                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,TGELEM                                                        
         ZIC   R2,BYTE                                                          
*                                                                               
         LA    R2,1(R2)            BUMP TO NEXT SEQUENCE NUMBER                 
         STC   R2,SVCASEQ          SAVE NEW LAST CAST SEQUENCE NUMBER           
         STC   R2,ELEMENT+TAESSEQ-TAESD AND SET IN ELEMENT                      
*                                                                               
         MVI   ANYMORE,C'Y'                                                     
         BAS   RE,NEXTEL3          IF MORE ELEMENTS TO COME                     
         BE    *+8                                                              
         MVI   ANYMORE,C'N'                                                     
         BAS   RE,DORESEQ          RESET SEQ #'S ON REMAINING ELS               
         B     MYADDLX             CC SET                                       
         SPACE 2                                                                
*              ROUTINE TO RESET SEQUENCE NUMBERS AFTER ELEMENT                  
*              TO BE INSERTED - SETS CC TO NO IF EXCEEEDED MAX                  
         SPACE 1                                                                
DORESEQ  NTR1                                                                   
         CLI   ANYMORE,C'Y'        IF MORE ELEMENTS                             
         BNE   MYADDLY                                                          
         ZIC   R1,ELEMENT+TAESSEQ-TAESD                                         
         B     DORESEQ8                                                         
*                                                                               
DORESEQ5 BAS   RE,NEXTEL                                                        
         BNE   MYADDLY                                                          
         USING TAESD,R4                                                         
*                                                                               
         CLI   TAESEL,TAESELQ      IF ESTIMATE ELEMENT                          
         BNE   MYADDLY                                                          
DORESEQ8 CH    R1,=H'255'          IF MORE THAN 255 ELEMENTS                    
         BH    MYADDLN             PASS BACK NO                                 
         LA    R1,1(R1)            INCREMENT SEQUENCE NUMBER                    
         STC   R1,TAESSEQ                                                       
         B     DORESEQ5            AND LOOP FOR MORE ELEMENTS                   
         DROP  R4                                                               
         EJECT                                                                  
         GETELN (R4),DATADISP,ELCODE,3                                          
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO WRITE ESTIMATE RECORD TO FILE OR TWA4                 
*                                                                               
         DS    0D                                                               
WRITEIT  NMOD1 0,*WRITEIT                                                       
         L     RC,0(R1)                                                         
*                                                                               
         CLI   STECNT,C'Y'         IF MORE TO COME                              
         BNE   WRITEIT5                                                         
         LA    R2,X'84'            SAVE OFF EST. IN TWA 4                       
         GOTO1 GETTWA,DMCB,((R2),ATIA)                                          
         OI    STESTAT,STEWTWA     SET FLAG                                     
         B     WRITEITX                                                         
*                                                                               
WRITEIT5 BAS   RE,SPLIT            SPLIT UP AND WRITE BACK EST REC(S)           
         NI    STESTAT,X'FF'-STEWTWA  TURN OFF FLAG                             
         NI    STEOPTH+4,X'DF'     SET TO RE-START W/NEXT HIT OF ENTER          
*                                                                               
WRITEITX XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO SPLIT UP BIG ESTIMATE RECORD TO SMALLER               
*              RECORD(S) AND WRITE IT TO THE FILE                               
         SPACE 1                                                                
SPLIT    NTR1                                                                   
         L     R4,ATIA             R4=A(NEW ESTIMATE RECORD)                    
         BAS   RE,PROCAC           ADD TAACELQ'S FROM ORG. REC(S)               
*                                                                               
         L     R3,AIO2             R3=A(RECORD TO BE WRITTEN BACK)              
         ST    R3,AIO                                                           
         USING TLESD,R3                                                         
         MVC   TLESKEY,0(R4)       SET KEY IN RECORD                            
         CLC   TLESAGY,SPACES                                                   
         BE    WRITERR1            WRITE ERROR                                  
         SPACE 1                                                                
         MVC   TLESLEN,DATADISP    INIT. RECORD LENGTH                          
         XC    TLESSTAT(10),TLESSTAT     STATUS, BEG. OF REC.                   
         SPACE 1                                                                
         MVI   ELCODE,0              SET TO LOOP THROUGH ALL ELEMENTS           
         LA    R4,TLESELEM-TLESD(R4) R4=A(FIRST ELEMENT)                        
         MVI   RDUPDATE,C'Y'         SET TO READ FOR UPDATE                     
         NI    GENSTAT1,ALL-RDUPAPPL THIS WILL KEEP IT ON UNTIL END             
         SPACE 1                                                                
SPL2     CLI   TLESSEQ,0           IF NOT BASE RECORD                           
         BE    SPL4                                                             
         MVC   TLESLEN,DATADISP    INIT. RECORD LENGTH                          
         XC    TLESSTAT(10),TLESSTAT     STATUS, BEG. OF REC.                   
         SPACE 1                                                                
SPL4     ZIC   R1,1(R4)            R1 = L'ELEMENT                               
         AH    R1,TLESLEN             + L'RECORD                                
         CH    R1,=H'1930'         WILL RECORD BECOME TOO LONG                  
         BH    SPL6                YES - GO RELEASE IT                          
         MVC   ELEMENT,0(R4)       NO - MOVE ELEMENT TO W/S                     
         GOTO1 ADDELEM,DMCB,,,,0   AND ADD TO NEW REC.                          
         BAS   RE,NEXTEL4          GET NEXT ELEMENT                             
         BE    SPL4                                                             
         SPACE 1                                                                
SPL6     BAS   RE,RELEASE          WRITE BACK RECORD IN IO2                     
         SPACE 1                                                                
         CLI   0(R4),0             IF THERE ARE MORE ELS. TO PROCESS            
         BNE   SPL2                GO BACK                                      
         SPACE 1                                                                
         GOTO1 ADELETE,DMCB,(RC)   DELETE ANY REMAINING RECORDS                 
         MVI   RDUPDATE,C'N'       TURN OFF READ FOR UPDATE                     
         OI    GENSTAT1,RDUPAPPL   RESET GENCON CONTROL SWITCH                  
         B     WRITEITX                                                         
         EJECT                                                                  
*              ROUTINE COPIES NON-MAINT. ACTIVITY ELEMENTS FROM                 
*              ORIGINAL ESTIMATE REC(S) TO NEW RECORD AND ADDS NEW              
*              MAINT. ACTIVITY ELEMENT                                          
         SPACE 1                                                                
PROCAC   NTR1                                                                   
         MVI   HAVEAC40,C'N'       NO ACTIVITY ELEMENT FROM 40 SCREEN           
         MVC   KEY,0(R4)           READ HIGH FOR ESTIMATE                       
         GOTO1 HIGH                                                             
         B     PROCAC5                                                          
*                                                                               
PROCAC2  GOTO1 SEQ                                                              
PROCAC5  CLC   KEY(TLESSEQ-TLESD),KEYSAVE                                       
         BNE   PROCACX                                                          
*                                                                               
         MVC   AIO,AIO1            SET AIO TO READ RECORD                       
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         MVI   ELCODE,TAACELQ                                                   
         BAS   RE,GETEL4                                                        
         B     *+8                                                              
PROCAC10 BAS   RE,NEXTEL4                                                       
         BNE   PROCAC2                                                          
         USING TAACD,R4                                                         
*                                                                               
         CLI   TAACSCR,0           IF NOT MAINT. ACTIVITY ELEMENT               
         BE    PROCAC10                                                         
         CLI   TAACSCR,SCR40       IF 40 SCREEN                                 
         BNE   *+8                                                              
         MVI   HAVEAC40,C'Y'       SET HAVE ELEMENT FROM 40 SCREEN              
         MVC   ELEMENT,0(R4)                                                    
         MVC   AIO,ATIA                                                         
         GOTO1 AMYADDL,DMCB,(RC)   AND ADD TO NEW REC.                          
         B     PROCAC10                                                         
*                                                                               
PROCACX  MVC   AIO,ATIA            ADD NEW MAINT. ACTIVITY ELEMENT              
         GOTO1 ACTVIN,DMCB,(X'40',0)                                            
         GOTO1 AMYADDL,DMCB,(RC)                                                
         B     WRITEITX                                                         
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
REL10    MVC   AIO,AIO1            RECORD EXISTS - READ IT INTO AIO1            
         GOTO1 GETREC                                                           
         MVC   AIO,AIO2            RESET AIO TO A(NEW RECORD)                   
         GOTO1 PUTREC              WRITE BACK NEW FILE RECORD                   
         SPACE 1                                                                
RELX     NI    DMINBTS,X'F7'       TURN OFF READ FOR DELETED                    
         L     R3,AIO              R3=A(RECORD WE JUST ADDED/WROTE)             
         USING TLESD,R3                                                         
         ZIC   R1,TLESSEQ          BUMP SEQUENCE NUMBER IN KEY OF REC.          
         LA    R1,2(R1)                                                         
         STC   R1,TLESSEQ                                                       
         B     WRITEITX                                                         
         EJECT                                                                  
         GETELN (R4),DATADISP,ELCODE,4                                          
         SPACE 2                                                                
         LTORG                                                                  
WRITERR1 MVC   MYMSGNO,=Y(ERAGYNFD)   BAD AGENCY CODE                           
         MVI   BLOCK,0                                                          
         MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         NI    STEOPTH+4,X'DF'     SET TO RE-START W/NEXT HIT OF ENTER          
         LA    R2,STEKEYH                                                       
         GOTO1 EXIT,DMCB,0                                                      
         EJECT                                                                  
*              ROUTINE DELETES SUBSEQUENT ESTIMATE RECORDS                      
         SPACE 1                                                                
         DS    0D                                                               
DELETE   NMOD1 0,*DELETE*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         L     R3,AIO              R3=A(FILE RECORD)                            
         USING TLESD,R3                                                         
         SPACE 1                                                                
DEL2     GOTO1 HIGH                RE-READ RECORD WE JUST WROTE                 
         NI    DMINBTS,X'F7'       TURN OFF READ DELETED                        
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 SEQ                 GET NEXT                                     
         SPACE 1                                                                
         CLC   KEY(TLESSEQ-TLESD),KEYSAVE  TEST STILL SAME ESTIMATE             
         BNE   DELETEX                                                          
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 GETREC              GET THE RECORD                               
         OI    TLESSTAT,X'80'      MARK IT DELETED                              
         GOTO1 PUTREC              AND WRITE IT BACK                            
         SPACE 1                                                                
         OI    KEY+TLDRSTAT-TLDRD,X'80'  MARK DIRECTORY DELETED                 
         GOTO1 WRITE                     AND WRITE IT BACK                      
         SPACE 1                                                                
         OI    DMINBTS,X'08'       SET TO READ DELETED                          
         B     DEL2                LOOK FOR MORE                                
*                                                                               
DELETEX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINE TO HANDLE OPTION SUB-ELEMENTS ON TAESD ELEMENT           
*                                                                               
*                                  ON DOWNLD-R0=(N'SUB-ENTRIES)                 
*                                            R2=A(AREA TO FORMAT ELE)           
*                                            R5=A(FIRST SUB-ENTRY)              
*                                                                               
*                                  ON UPLOAD-R0=A(N'OPTION OVERRIDES)           
*                                            R3=A(AREA TO SET ELEMENT)          
         DS    0D                                                               
OPTELS   NMOD1 0,*OPTELS*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         CLI   ACTNUM,ACTDWN       IF UPLOAD                                    
         BE    *+12                                                             
         BAS   RE,OPTEUP           CONVERT OVERRIDE OPT TO ELE. FORMAT          
         B     OPTXIT                                                           
*                                                                               
         TM    STOPT,STEXPAND      IF REGULAR DOWNLOAD                          
         BO    OPTELS2                                                          
         TM    STOPT,STEXTRA                                                    
         BO    OPTELS2                                                          
         MVI   0(R2),C'Z'          SHOW Z TO INDICATE OPTION OVERRIDES          
         MVI   1(R2),X'6A'         COMING NEXT                                  
         LA    R2,2(R2)                                                         
*                                                                               
         USING TAESSBCD,R5                                                      
OPTELS2  GOTO1 HEXOUT,DMCB,0(R5),ELFLABEL,1,0                                   
         XC    ELFDATA,ELFDATA                                                  
         L     R4,AOPTTAB           FIND SUB-ENTRY CODE IN TABLE                
         USING OPTD,R4                                                          
*                                                                               
OPTELS5  CLI   OPTCODE,X'FF'       TEST END OF TABLE                            
         BE    OPTELS9                                                          
         CLC   OPTCODE,0(R5)       TEST MATCH                                   
         BE    *+12                                                             
         LA    R4,OPTNEXT                                                       
         B     OPTELS5                                                          
*                                                                               
         LA    R3,ELFDATA          R3=A(DATA)                                   
         XR    RF,RF                                                            
         ICM   RF,3,OPTODSP        RF=DISPLACEMENT TO O/P ROUTINE               
         BZ    OPTELS7             IF NONE JUST DISPLAY LEFT HAND SIDE          
         AR    RF,RB                                                            
         LA    RE,OPTELS7          RE=A(RETURN)                                 
         NTR1 ,                    COMMON NTR1                                  
         BR    RF                                                               
         SPACE 1                                                                
OPTELS7  GOTO1 AFMTEL,DMCB,(RC)    FORMAT AT R2                                 
         SPACE 1                                                                
OPTELS9  DS    0H                  BUMP R5 TO NEXT SUB-ENTRY                    
         ZIC   RF,OPTSUBLN         L'OF CURRENT SUB-ENTRY                       
         CLI   TAESSBCD,OPCOMNM    IF THIS IS COMML NAME                        
         BNE   *+8                                                              
         IC    RF,TAESSBDT         LENGTH IS IN FIRST DATA BYTE                 
         LA    R5,1(RF,R5)                                                      
         BCT   R0,OPTELS2          LOOP                                         
         XIT1  REGS=(R2)           PASS R2 POINTING TO END OF SCRNEL            
         SPACE 2                                                                
OPTXIT   XIT1                      REGULAR EXIT                                 
         DROP  R4                                                               
         EJECT                                                                  
*                                  R0=(N'SUB-ELEMENT OPTION OVERRIDES)          
*                                  R3=(APPROP. POSITION IN FILE ELE.)           
OPTEUP   NTR1                                                                   
*                                                                               
OPTEUP1  GOTO1 ASETELF,DMCB,(RC)   SET ELFAREA & UPDATE ASCRPOS                 
         GOTO1 HEXIN,DMCB,ELFLABEL,BYTE,2                                       
*                                                                               
         L     R4,AOPTTAB                                                       
         USING OPTD,R4                                                          
OPTEUP5  CLI   OPTCODE,X'FF'       TEST END OF TABLE                            
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   OPTCODE,BYTE        TEST MATCH                                   
         BE    *+12                                                             
         LA    R4,OPTNEXT                                                       
         B     OPTEUP5                                                          
*                                                                               
         MVC   0(1,R3),OPTCODE     SET CODE                                     
         LA    R3,1(R3)                                                         
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,OPTIDSP        RF=DISPLACEMENT TO I/P ROUTINE               
         BZ    OPTEUP7                                                          
         AR    RF,RB                                                            
         LA    RE,OPTEUP7                                                       
         NTR1  ,                                                                
         BR    RF                                                               
*                                                                               
OPTEUP7  ZIC   R1,ELEMENT+1        UPDATE LENGTH OF ELEMENT                     
         ZIC   RE,OPTSUBLN                                                      
         LA    RE,1(RE)                                                         
         CLI   OPTCODE,OPCOMNM     IF HYPO COMMERCIAL                           
         BNE   OPTEUP9                                                          
         ZIC   RF,ELFEQU           NEED TO ADD LENGTH OF NAME                   
         AR    RE,RF                                                            
*                                                                               
OPTEUP9  AR    R1,RE                                                            
         CH    R1,=H'255'          CHECK AGAINST MAX ELEMENT LENGTH             
         BH    OPTEUPER                                                         
         STC   R1,ELEMENT+1                                                     
         BCTR  RE,0                                                             
         AR    R3,RE               PT TO NEXT SUB-ELEMENT POSITION              
*                                  FIX UPLOAD BUG W/INCORRECT Z=#               
         CLI   OPTCODE,OPNO        IF HAVE EXCLUDE OPTION ON CLA USE            
         BNE   OPTEUP12                                                         
         CLI   TGUSEQU,UCLA                                                     
         BNE   OPTEUP12                                                         
         ZIC   RE,ELEMENT+TAESSBNO-TAESD DECREMENT BY ONE ON ELEMENT            
         BCTR  RE,0                                                             
         STC   RE,ELEMENT+TAESSBNO-TAESD                                        
         BCT   R0,OPTEUP12                                & COUNTER             
         B     OPTXIT                                                           
*                                                                               
OPTEUP12 BCT   R0,OPTEUP1                                                       
         B     OPTXIT                                                           
         SPACE 1                                                                
OPTEUPER NI    STEOPTH+4,X'DF'     SET TO RE-START W/NEXT HIT OF ENTER          
         MVC   MYMSGNO,=AL2(ERELEBIG) ELEMENT TOO BIG                           
         LA    R4,ELEMENT                                                       
         USING TAESD,R4                                                         
         SPACE 1                                                                
         XC    BLOCK(50),BLOCK                                                  
         EDIT  TAESTYPE,(2,BLOCK+1),ALIGN=LEFT,FILL=0                           
         CLI   TAESTYPE,TAESTCLI                                                
         BE    *+12                                                             
         CLI   TAESTYPE,TAESTPRD                                                
         BNE   OPTEUPE1                                                         
         MVI   BLOCK,10                                                         
         MVC   BLOCK+4(L'TAESDATA),TAESDATA                                     
         B     OPTEUPEX                                                         
         SPACE 1                                                                
OPTEUPE1 CLI   TAESTYPE,TAESTCOM                                                
         BNE   OPTEUPE2                                                         
         MVI   BLOCK,20                                                         
         MVC   BLOCK+4(L'TAESCID),TAESCID                                       
         GOTO1 HEXOUT,DMCB,TAESCOM,BLOCK+13,4,0                                 
         B     OPTEUPEX                                                         
         SPACE 1                                                                
OPTEUPE2 CLI   TAESTYPE,TAESTPER                                                
         BNE   OPTEUPE4                                                         
         MVI   BLOCK,25                                                         
         GOTO1 HEXOUT,DMCB,TAESSORT,BLOCK+4,6,0                                 
         EDIT  TAESSSN,(9,BLOCK+17),ALIGN=LEFT,FILL=0                           
         B     OPTEUPEX                                                         
         SPACE 1                                                                
OPTEUPE4 CLI   TAESTYPE,TAESTHCO                                                
         BNE   OPTEUPE6                                                         
         CLI   TAESHCOM+9,0            HYPO COMML PACKED                        
         BE    OPTEUPE5                                                         
         MVI   BLOCK,18                                                         
         MVC   BLOCK+4(L'TAESHCOM),TAESHCOM                                     
         MVC   BLOCK+15(L'TAESHMED),TAESHMED                                    
         EDIT  TAESHLEN,(3,BLOCK+18),ALIGN=LEFT,FILL=0                          
         B     OPTEUPEX                                                         
*                                                                               
OPTEUPE5 MVI   BLOCK,20                                                         
         GOTO1 =A(CHPACK),DMCB,(C'U',TAESHCOM),BLOCK+4                          
         MVC   BLOCK+17(L'TAESHMED),TAESHMED                                    
         EDIT  TAESHLEN,(3,BLOCK+20),ALIGN=LEFT,FILL=0                          
         B     OPTEUPEX                                                         
*                                                                               
OPTEUPE6 CLI   TAESTYPE,TAESTHPE                                                
         BNE   OPTEUPEX                                                         
         MVI   BLOCK,18                                                         
         EDIT  TAESHSTA,(1,BLOCK+4),ALIGN=LEFT,FILL=0                           
         MVC   BLOCK+6(L'TGUNI),TGUNI                                           
         MVC   BLOCK+10(L'TGCAT),TGCAT                                          
         EDIT  TAESHNPR,(3,BLOCK+14),ALIGN=LEFT,FILL=0                          
         MVC   BLOCK+18(L'TAESHLFT),TAESHLFT                                    
         EDIT  TAESHDBL,(3,BLOCK+20),ALIGN=LEFT,FILL=0                          
         SPACE 1                                                                
OPTEUPEX MVI   MYMTYP,GTMERR                                                    
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,STEKEYH                                                       
         GOTO1 EXIT,DMCB,0                                                      
         SPACE 1                                                                
         DROP  R4                                                               
         EJECT                                                                  
*        OPTION OUTPUT ROUTINES FOR DOWNLOAD                                    
         SPACE 1                                                                
*                                  R3=A(ELFDATA)                                
         USING OPTD,R4             R4=A(OPTION TABLE ENTRY)                     
         USING TAESSBCD,R5         R5=A(SUB-ELEMENT)                            
         SPACE 1                                                                
OUTCHAR  DS    0H                  MOVE CHARACTERS                              
         ZIC   RF,OPTSUBLN                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),TAESSBDT                                                 
OUTCHAR5 LA    RF,1(RF)                                                         
         BAS   RE,OUTCKBAR         CHECK FOR BAR                                
         B     OPTXIT                                                           
         SPACE 2                                                                
OUTAMT   DS    0H                  BINARY AMOUNT - NO DEC. PLACES               
         EDIT  (4,TAESSBDT),(10,(R3)),ALIGN=LEFT,ZERO=NOBLANK                   
         B     OPTXIT                                                           
         SPACE 2                                                                
OUTAMT2  DS    0H                  BINARY AMOUNT - NO DEC. PLACES               
         EDIT  (2,TAESSBDT),(10,(R3)),ALIGN=LEFT,ZERO=NOBLANK                   
         B     OPTXIT                                                           
         SPACE 2                                                                
OUTNUM   DS    0H                  L'LFT,MAJORS,GRT,UNTS,#USES,#INSERTS         
         CLI   OPTSUBLN,1          #DEMOS,#TAGS,LCLAU                           
         BNE   OUTNUM2                                                          
         EDIT  (1,TAESSBDT),(3,0(R3)),ALIGN=LEFT,ZERO=NOBLANK                   
         B     OPTXIT                                                           
         SPACE 2                                                                
OUTNUM2  CLI   OPTSUBLN,2                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         EDIT  (2,TAESSBDT),(5,0(R3)),ALIGN=LEFT,ZERO=NOBLANK                   
         B     OPTXIT                                                           
         EJECT                                                                  
*        OPTION OUTPUT ROUTINES FOR DOWNLOAD (CONTINUED)                        
         SPACE 1                                                                
OUTDATEL NTR1                                                                   
OUTDATE  DS    0H                                                               
         GOTO1 DATCON,DMCB,(5,0),(23,WORK+20)   TODAYS DATE                     
         GOTO1 DATCON,DMCB,(1,TAESSBDT),(23,WORK)                               
         CLC   WORK(4),WORK+20     IF NOT CURRENT YEAR                          
         BE    OUTDATE5                                                         
         MVC   0(10,R3),WORK       MOVE YYYY-MM-DD                              
         LA    R3,10(R3)                                                        
         B     OUTDATEX                                                         
OUTDATE5 MVC   0(5,R3),WORK+5      ELSE, JUST MM-DD                             
         LA    R3,5(R3)                                                         
OUTDATEX XIT1  REGS=(R3)                                                        
         EJECT                                                                  
*        OPTION OUTPUT ROUTINES FOR DOWNLOAD (CONTINUED)                        
         SPACE 1                                                                
OUTUSE   DS    0H                  USE TYPE                                     
         GOTO1 USEVAL,DMCB,(X'80',TAESSBDT),TAESSBDT+1 CVT TO CHAR.             
         BE    OUTUSE1                                                          
         GOTO1 USEVAL,DMCB,(X'C0',TAESSBDT)       TRY W/O TYPE                  
         BE    *+6                                                              
         DC    H'0'                                                             
OUTUSE1  MVC   0(L'TGUSCDE,R3),TGUSCDE                                          
         MVC   L'TGUSCDE(L'TGUSTYCD,R3),TGUSTYCD                                
         B     OPTXIT                                                           
         SPACE 2                                                                
OUTUSES2 DS    0H                  SPECIFIC USE NUMBERS                         
         EDIT  (2,TAESSBDT),(10,(R3)),ALIGN=LEFT                                
         AR    R3,R0                                                            
         MVI   0(R3),C' '                                                       
         TM    STOPT,STEXTRA                                                    
         BZ    *+8                                                              
         MVI   0(R3),C'-'                                                       
         CLC   TAESSBDT(2),TAESSBDT+2                                           
         BE    OPTXIT              DONE IF START = END                          
         LA    R3,1(R3)                                                         
         LA    R5,2(R5)                                                         
         EDIT  (2,TAESSBDT),(10,(R3)),ALIGN=LEFT                                
         B     OPTXIT                                                           
         SPACE 2                                                                
OUTYEAR  DS    0H                  CONVERT CONTRACT YEAR TO CHAR                
         GOTO1 YRVAL,DMCB,(X'80',TAESSBDT)                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(L'TGYRCDE,R3),TGYRCDE                                          
         B     OPTXIT                                                           
         SPACE 2                                                                
OUTYRAS  DS    0H                  CONTRACT YEAR AS OF DATE                     
         BAS   RE,OUTDATEL                                                      
         MVI   0(R3),C' '                                                       
         TM    STOPT,STEXTRA                                                    
         BZ    *+8                                                              
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         LA    R5,3(R5)            PT TO YR IN ELEMENT                          
         B     OUTYEAR                                                          
         EJECT                                                                  
*        OPTION OUTPUT ROUTINES FOR DOWNLOAD (CONTINUED)                        
         SPACE 1                                                                
OUTHLDAS DS    0H                  HOLDING FEE AMT OR RATE AS/OF DATE           
         BAS   RE,OUTDATEL                                                      
         MVI   0(R3),C' '                                                       
         TM    STOPT,STEXTRA                                                    
         BZ    *+8                                                              
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         LA    R5,3(R5)                                                         
         B     OUTAMT                                                           
         SPACE 1                                                                
OUTEXP   DS    0H                  EXPIRY DATE                                  
         CLI   TAESSBDT,X'FF'                                                   
         BNE   OUTDATE                                                          
         MVI   0(R3),C'Y'                                                       
         B     OPTXIT                                                           
         SPACE 1                                                                
OUTCSF   DS    0H                  CONTRACT SERVICE FEE                         
         CLI   TAESSBDT,C'Y'                                                    
         BE    *+12                                                             
         CLI   TAESSBDT,C'N'                                                    
         BNE   OUTAMT                                                           
         MVC   0(1,R3),TAESSBDT                                                 
         B     OPTXIT                                                           
         SPACE 1                                                                
OUTCYC   DS    0H                  CYCLE DATES                                  
         BAS   RE,OUTDATEL                                                      
         MVI   0(R3),C' '                                                       
         TM    STOPT,STEXTRA                                                    
         BZ    *+8                                                              
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         LA    R5,3(R5)                                                         
         BAS   RE,OUTDATEL                                                      
         B     OPTXIT                                                           
         SPACE 1                                                                
OUTSSN   DS    0H                  SOCIAL SECURITY NUMBER                       
         ICM   RE,15,TAESSBDT                                                   
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(9,R3),DUB                                                      
         B     OPTXIT                                                           
         EJECT                                                                  
*        OPTION OUTPUT ROUTINES FOR DOWNLOAD (CONTINUED)                        
         SPACE 1                                                                
OUTOV12  DS    0H                  OVERSCALE 1 & 2 PCT                          
         EDIT  (4,TAESSBDT),(10,(R3)),ALIGN=LEFT,ZERO=NOBLANK                   
         AR    R3,R0                                                            
         MVI   0(R3),C' '                                                       
         TM    STOPT,STEXTRA                                                    
         BZ    *+8                                                              
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         LA    R5,4(R5)                                                         
         EDIT  (4,TAESSBDT),(10,(R3)),ALIGN=LEFT,ZERO=NOBLANK                   
         B     OPTXIT                                                           
         SPACE 1                                                                
OUTOVAS  DS    0H                  OVERSCALE AS OF DATE                         
         BAS   RE,OUTDATEL                                                      
         MVI   0(R3),C' '                                                       
         TM    STOPT,STEXTRA                                                    
         BZ    *+8                                                              
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         LA    R5,3(R5)                                                         
         B     OUTAMT                                                           
         SPACE 1                                                                
OUTOV12A DS    0H                  OVERSCALE 1 & 2 PCT AS OF DATE               
         BAS   RE,OUTDATEL                                                      
         MVI   0(R3),C' '                                                       
         TM    STOPT,STEXTRA                                                    
         BZ    *+8                                                              
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         LA    R5,3(R5)                                                         
         EDIT  (4,TAESSBDT),(10,(R3)),ALIGN=LEFT,ZERO=NOBLANK                   
         AR    R3,R0                                                            
         MVI   0(R3),C' '                                                       
         TM    STOPT,STEXTRA                                                    
         BZ    *+8                                                              
         MVI   0(R3),C'-'                                                       
         LA    R3,1(R3)                                                         
         LA    R5,4(R5)                                                         
         EDIT  (4,TAESSBDT),(10,(R3)),ALIGN=LEFT,ZERO=NOBLANK                   
         B     OPTXIT                                                           
         EJECT                                                                  
*        OPTION OUTPUT ROUTINES FOR DOWNLOAD (CONTINUED)                        
         SPACE 1                                                                
OUTCOMNM DS    0H                  HYPO COMMERCIAL NAME                         
         ZIC   RF,TAESSBDT                                                      
         SH    RF,=H'2'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),TAESSBDT+1                                               
         LA    RF,1(RF)                                                         
         BAS   RE,OUTCKBAR         MAKE SURE NO BARS                            
         B     OPTXIT                                                           
         SPACE 2                                                                
OUTATYPE DS    0H                  COMMERCIAL ACTRA TYPE                        
         GOTO1 CCTYPVAL,DMCB,(X'80',TAESSBDT)                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(L'TGCCTCDE,R3),TGCCTCDE                                        
         B     OPTXIT                                                           
         SPACE 2                                                                
*                                  RF=(L'DATA)                                  
OUTCKBAR NTR1                                                                   
OUTCKBR8 CLI   0(R3),X'6A'                                                      
         BNE   *+8                                                              
         MVI   0(R3),C'/'                                                       
         LA    R3,1(R3)                                                         
         BCT   RF,OUTCKBR8                                                      
         B     OPTXIT                                                           
         DROP  R4                                                               
         EJECT                                                                  
*        OPTION INPUT ROUTINES FOR UPLOAD                                       
         SPACE 1                                                                
*                                  R3=A(AREA TO SET ELEMENT RESULT)             
         USING OPTD,R4             R4=A(OPTION TABLE ENTRY)                     
         SPACE 1                                                                
INCHAR   DS    0H                  MOVE CHARACTERS                              
         ZIC   RF,ELFEQU                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     OPTXIT                                                           
         MVC   0(0,R3),ELFDATA                                                  
         SPACE 2                                                                
INCOMNM  DS    0H                  HYPO COMMERCIAL NAME                         
         ZIC   R1,ELFEQU                                                        
         LA    R1,1(R1)                                                         
         STC   R1,0(R3)                                                         
         LA    R3,1(R3)                                                         
         B     INCHAR                                                           
         SPACE 2                                                                
INUSE    DS    0H                  USE CODE                                     
         OC    ELFDATA,SPACES                                                   
         GOTO1 USEVAL,DMCB,(X'10',ELFDATA),ELFDATA+3                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(1,R3),TGUSEQU     RETURN USE EQUATE                            
         MVC   1(1,R3),TGUSTYP        AND USE TYPE EQUATE                       
         B     OPTXIT                                                           
         SPACE 2                                                                
INUSENMS DS    0H                  SPECIFIC USE NUMBERS                         
         OC    ELFDATA,SPACES                                                   
         GOTO1 SCANNER,DMCB,(C'C',ELFDATA),(2,BLOCK),C',=,-'                    
         CLI   4(R1),1             SHOULD ONLY BE ONE ENTRY                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,BLOCK                                                         
         USING SCAND,R2                                                         
         MVC   0(2,R3),SCBIN1+2                                                 
         MVC   2(2,R3),SCBIN2+2                                                 
         B     OPTXIT                                                           
         EJECT                                                                  
*        OPTION INPUT ROUTINES FOR UPLOAD (CONTINUED)                           
         SPACE 1                                                                
INAMT    DS    0H                                                               
         ZIC   R0,OPTSUBLN         LENGTH TO STORE                              
         LA    R4,ELFDATA                                                       
         ZIC   RF,ELFEQU                                                        
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                ZERO IS 0                                    
*                                                                               
         XR    R2,R2                                                            
         CLI   0(R4),C'-'          IF NEGATIVE AMOUNT                           
         BNE   INAMT30                                                          
         LA    R2,1(R2)            SET FLAG                                     
         LA    R4,1(R4)            BUMP TO NUMBER                               
         BCTR  RF,0                                                             
         LTR   RF,RF               IF NO AMT SOMETHING WRONG                    
         BNZ   INAMT30                                                          
         DC    H'0'                                                             
*                                                                               
INAMT30  BCTR  RF,0                ONE FOR EXECUTE                              
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)                                                      
         ZAP   DUB,DUB             CHANGE X'0F' SIGN TO X'0C' SIGN              
         LTR   R2,R2               IF NEGATIVE AMOUNT                           
         BZ    *+10                                                             
         MP    DUB,=P'-1'          MAKE NEGATIVE                                
         CVB   RF,DUB                                                           
*                                                                               
         CH    R0,=H'2'                                                         
         BNE   INAMT35                                                          
         STCM  RF,3,0(R3)                                                       
         B     OPTXIT                                                           
INAMT35  CH    R0,=H'4'                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         ST    RF,0(R3)            SET BINARY AMOUNT                            
         B     OPTXIT                                                           
         EJECT                                                                  
*        OPTION INPUT ROUTINES FOR UPLOAD (CONTINUED)                           
         SPACE 1                                                                
INNUM    DS    0H                  HANDLE NUMERIC INPUT                         
         ZIC   R0,OPTSUBLN         LENGTH TO STORE                              
*                                                                               
         ZIC   RF,ELFEQU                                                        
         LA    R4,ELFDATA                                                       
*                                                                               
         MVC   WORK(9),=9X'F0'     INSURE NUMERIC INPUT                         
         SH    RF,=H'1'                                                         
         BM    NO                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVZ   WORK(0),0(R4)                                                    
         CLC   WORK(9),=9X'F0'                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)                                                      
         CVB   RF,DUB                                                           
*                                                                               
         CH    R0,=H'1'            IF LENGTH IS ONE BYTE                        
         BNE   INNUM2                                                           
         STC   RF,0(R3)            STICK IT IN                                  
         B     OPTXIT                                                           
INNUM2   CH    R0,=H'2'            IF LENGTH IS 2 BYTES                         
         BE    *+6                                                              
         DC    H'0'                                                             
         STCM  RF,3,0(R3)          ELSE STICK THEM IN                           
         B     OPTXIT                                                           
         EJECT                                                                  
*        OPTION INPUT ROUTINES FOR UPLOAD (CONTINUED)                           
         SPACE 1                                                                
INDATE   DS    0H                                                               
         OC    ELFDATA,SPACES                                                   
         LA    R2,ELFDATA                                                       
         GOTO1 DTVAL,DMCB,(X'40',0(R3))                                         
         BE    OPTXIT                                                           
         DC    H'0'                                                             
         SPACE 2                                                                
INCYC    DS    0H                  CYCLE DATES                                  
         GOTO1 PERVAL,DMCB,(ELFEQU,ELFDATA),BLOCK                               
         LA    R1,BLOCK                                                         
         USING PERVALD,R1                                                       
         MVC   0(3,R3),PVALPSTA                                                 
         MVC   3(3,R3),PVALPEND                                                 
         B     OPTXIT                                                           
         SPACE 2                                                                
INEXP    CLI   ELFDATA,C'Y'                                                     
         BNE   INDATE                                                           
         MVI   0(R3),X'FF'                                                      
         B     OPTXIT                                                           
         EJECT                                                                  
*        OPTION INPUT ROUTINES FOR UPLOAD (CONTINUED)                           
         SPACE 1                                                                
INCSF    DS    0H                  CONTRACT SERVICE FEE                         
         CLI   ELFDATA,C'Y'                                                     
         BE    *+12                                                             
         CLI   ELFDATA,C'N'                                                     
         BNE   INAMT                                                            
         MVC   0(1,R3),ELFDATA                                                  
         B     OPTXIT                                                           
         SPACE 1                                                                
INYRAS   DS    0H                  CONTRACT YEAR AS OF DATE                     
         OC    ELFDATA,SPACES                                                   
         GOTO1 SCANNER,DMCB,(C'C',ELFDATA),(2,BLOCK),C',=,-'                    
         CLI   4(R1),1             SHOULD ONLY BE ONE ENTRY                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,BLOCK                                                         
         USING SCAND,R2                                                         
         MVC   3(1,R3),SCBIN2+3    SET YEAR IN ELEMENT                          
         LA    R2,SCDATA1                                                       
         GOTO1 DTVAL,DMCB,(X'40',0(R3))                                         
         BE    OPTXIT                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
INHLDAS  DS    0H                  HOLDING FEE AMT OR RATE AS/OF DATE           
         OC    ELFDATA,SPACES                                                   
         GOTO1 SCANNER,DMCB,(C'C',ELFDATA),(2,BLOCK),C',=,-'                    
         CLI   4(R1),1             SHOULD ONLY BE ONE ENTRY                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,BLOCK                                                         
         USING SCAND,R2                                                         
         MVC   3(4,R3),SCBIN2      SET RATE OR AMT IN ELEMENT                   
         LA    R2,SCDATA1                                                       
         GOTO1 DTVAL,DMCB,(X'40',0(R3))                                         
         BE    OPTXIT                                                           
         DC    H'0'                                                             
         EJECT                                                                  
*        OPTION INPUT ROUTINES FOR UPLOAD (CONTINUED)                           
         SPACE 1                                                                
INOV12   DS    0H                  OVERSCALE 1 & 2 PCT                          
         OC    ELFDATA,SPACES                                                   
         GOTO1 SCANNER,DMCB,(C'C',ELFDATA),(2,BLOCK),C',=,-'                    
         CLI   4(R1),1             SHOULD ONLY BE ONE ENTRY                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,BLOCK                                                         
         USING SCAND,R2                                                         
         MVC   0(4,R3),SCBIN1                                                   
         MVC   4(4,R3),SCBIN2                                                   
         B     OPTXIT                                                           
         SPACE 1                                                                
INOVAS   DS    0H                  OVERSCALE AS OF DATE                         
         OC    ELFDATA,SPACES                                                   
         GOTO1 SCANNER,DMCB,(C'C',ELFDATA),(1,BLOCK),C',=,-'                    
         CLI   4(R1),1             SHOULD ONLY BE ONE ENTRY                     
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,BLOCK                                                         
         USING SCAND,R2                                                         
         MVC   3(4,R3),SCBIN2      SET OV RATE IN ELEMENT                       
         LA    R2,SCDATA1                                                       
         GOTO1 DTVAL,DMCB,(X'40',0(R3))                                         
         BE    OPTXIT                                                           
         DC    H'0'                                                             
         SPACE 1                                                                
INOV12A  DS    0H                  OVERSCALE 1& 2 AS OF DATE                    
         OC    ELFDATA,SPACES                                                   
         GOTO1 SCANNER,DMCB,(C'C',ELFDATA),(3,BLOCK),C',=-='                    
         CLI   4(R1),3             SHOULD BE THREE ENTRIES                      
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,BLOCK                                                         
         USING SCAND,R2                                                         
         LR    R0,R2                                                            
         LA    R2,SCDATA1                                                       
         GOTO1 DTVAL,DMCB,(X'40',0(R3))                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    R2,R0                                                            
         LA    R2,SCANNEXT                                                      
         MVC   3(4,R3),SCBIN1      SET OV1 RATE IN ELEMENT                      
         LA    R2,SCANNEXT                                                      
         MVC   7(4,R3),SCBIN1      AND SET OV2 RATE IN ELEMENT                  
         B     OPTXIT                                                           
         EJECT                                                                  
*        OPTION INPUT ROUTINES FOR UPLOAD (CONTINUED)                           
         SPACE 1                                                                
INSSN    DS    0H                  SOCIAL SECURITY NUMBER                       
         PACK  DUB,ELFDATA(9)                                                   
         CVB   R1,DUB                                                           
         ST    R1,0(R3)                                                         
         B     OPTXIT                                                           
         SPACE 2                                                                
INYEAR   DS    0H                  CONVERT CONTRACT YEAR TO EQUATE              
         LA    R4,ELFDATA                                                       
         OC    0(L'ELFDATA,R4),SPACES                                           
         GOTO1 YRVAL,DMCB,0(R4)                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(L'TGYREQU,R3),TGYREQU                                          
         B     OPTXIT                                                           
         SPACE 2                                                                
INATYPE  DS    0H                  COMMERCIAL ACTRA TYPE                        
         OC    ELFDATA,SPACES                                                   
         GOTO1 CCTYPVAL,DMCB,ELFDATA                                            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(L'TGCCTEQU,R3),TGCCTEQU                                        
         B     OPTXIT                                                           
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TAGENEOPTS        OPTION EQUATES                               
         EJECT                                                                  
*         OPTIONS TABLE                                                         
         SPACE 2                                                                
*              IF YOU CHANGE THIS TABLE YOU MUST ALSO CHANGE                    
*              OPTTAB IN TAGEN2C AND IN TAESTPRT                                
         SPACE 1                                                                
OPTTAB   DS    0C                                                               
         DC    AL1(OPADDL,LOPADDL,0)   ADD PERFORMERS                           
         DC    AL2(0,0)                                                         
*                                                                               
         DC    AL1(OPPERF,LOPPERF,0)   DISPLAY PERFORMER LIST                   
         DC    AL2(0,0)                                                         
*                                                                               
         DC    AL1(OPX,LOPX,0)         EXCLUDE PERFORMER                        
         DC    AL2(0,0)                                                         
*                                                                               
         DC    AL1(OPYES,LOPYES,0)     INCLUDE COMMERCIAL                       
         DC    AL2(0,0)                                                         
*                                                                               
         DC    AL1(OPBROAD,LOPBROAD,0) WHERE BROADCAST                          
         DC    AL2(OUTCHAR-OPTELS,INCHAR-OPTELS)                                
*                                                                               
         DC    AL1(OPCANTX,LOPCANTX,0) CANADIAN TAX RATE OVERRIDE               
         DC    AL2(OUTAMT-OPTELS,INAMT-OPTELS)                                  
*                                                                               
         DC    AL1(OPCOMM,LOPCOMM,0)   COMMISSION RATE OVERRIDE                 
         DC    AL2(OUTAMT-OPTELS,INAMT-OPTELS)                                  
*                                                                               
         DC    AL1(OPEORTX,LOPEORTX,0)  E-O-R TAX RATE OVERRIDE                 
         DC    AL2(OUTAMT-OPTELS,INAMT-OPTELS)                                  
*                                                                               
         DC    AL1(OPEXCH,LOPEXCH,0)   EXCHANGE RATE                            
         DC    AL2(OUTAMT-OPTELS,INAMT-OPTELS)                                  
*                                                                               
         DC    AL1(OPEXP,LOPEXP,0)     EXPIRY DATE OVERRIDE                     
         DC    AL2(OUTEXP-OPTELS,INEXP-OPTELS)                                  
*                                                                               
         DC    AL1(OPINCTX,LOPINCTX,0) INCORPORATED TAX RATE OVERRIDE           
         DC    AL2(OUTAMT-OPTELS,INAMT-OPTELS)                                  
*                                                                               
         DC    AL1(OPINTEG,LOPINTEG,0) INTEGRATION FEES                         
         DC    AL2(OUTAMT-OPTELS,INAMT-OPTELS)                                  
*                                                                               
         DC    AL1(OPLLIFT,LOPLLIFT,0) L'LIFT                                   
         DC    AL2(OUTNUM-OPTELS,INNUM-OPTELS)                                  
*                                                                               
         DC    AL1(OPLLIF2,LOPLLIF2,0) L'2ND LIFT                               
         DC    AL2(OUTNUM-OPTELS,INNUM-OPTELS)                                  
*                                                                               
         DC    AL1(OPMULT,LOPMULT,0)   MULTIPLIER                               
         DC    AL2(OUTAMT-OPTELS,INAMT-OPTELS)                                  
*                                                                               
         DC    AL1(OPOVER2,LOPOV2,0)   SECOND USE OVERSCALE PERCENTAGE          
         DC    AL2(OUTAMT-OPTELS,INAMT-OPTELS)                                  
*                                                                               
         DC    AL1(OPOV12,LOPOV12,0)   USE OVERSCALE 1 & 2 PERCENTAGE           
         DC    AL2(OUTOV12-OPTELS,INOV12-OPTELS)                                
*                                                                               
         DC    AL1(OPOVER,LOPOVER,0)   USE OVERSCALE PERCENTAGE                 
         DC    AL2(OUTAMT-OPTELS,INAMT-OPTELS)                                  
*                                                                               
         DC    AL1(OPYRAS,LOPYRAS,0)   YEAR AS/OF DATE                          
         DC    AL2(OUTYRAS-OPTELS,INYRAS-OPTELS)                                
*                                                                               
         DC    AL1(OPOV2AS,LOPOV2AS,0)  OV2 PCT AS/OF DATE                      
         DC    AL2(OUTOVAS-OPTELS,INOVAS-OPTELS)                                
*                                                                               
         DC    AL1(OPOV12AS,LOPOV12A,0) OV 1&2 PCT AS/OF DATE                   
         DC    AL2(OUTOV12A-OPTELS,INOV12A-OPTELS)                              
*                                                                               
         DC    AL1(OPOVAS,LOPOVAS,0)   OVERSCALE PCT AS/OF DATE                 
         DC    AL2(OUTOVAS-OPTELS,INOVAS-OPTELS)                                
*                                                                               
         DC    AL1(OPOV2AS,LOPOV2AS,0) OV2 PCT AS/OF DATE                       
         DC    AL2(OUTOVAS-OPTELS,INOVAS-OPTELS)                                
*                                                                               
         DC    AL1(OPPROD,LOPPROD,0)   WHERE PRODUCED                           
         DC    AL2(OUTCHAR-OPTELS,INCHAR-OPTELS)                                
*                                                                               
         DC    AL1(OPCTYP,LOPCTYP,0)   COMMERCIAL TYPE                          
         DC    AL2(OUTCHAR-OPTELS,INCHAR-OPTELS)                                
*                                                                               
         DC    AL1(OPYEAR,LOPYEAR,0)   CONTRACT YEAR OVERRIDE                   
         DC    AL2(OUTYEAR-OPTELS,INYEAR-OPTELS)                                
*                                                                               
         DC    AL1(OPUSE,LOPUSE,OPFORUSE)  USE TYPES                            
         DC    AL2(OUTUSE-OPTELS,INUSE-OPTELS)                                  
*                                                                               
         DC    AL1(OPASOF,LOPASOF,OPFORUSE) AS OF DATES                         
         DC    AL2(OUTDATE-OPTELS,INDATE-OPTELS)                                
*                                                                               
         DC    AL1(OPGRTNO,LOPGRTNO,0) GUARANTEE NUMBER                         
         DC    AL2(OUTNUM-OPTELS,INNUM-OPTELS)                                  
*                                                                               
         DC    AL1(OPPRI,LOPPRI,0)     PRIMARY COMML IND.                       
         DC    AL2(0,0)                                                         
*                                                                               
         DC    AL1(OPSSN,LOPSSN,0)     SOCIAL SECURITY NUMBER                   
         DC    AL2(OUTSSN-OPTELS,INSSN-OPTELS)                                  
*                                                                               
         DC    AL1(OPGUAR,LOPGUAR,0)   GRT CREDITTING STAT                      
         DC    AL2(OUTCHAR-OPTELS,INCHAR-OPTELS)                                
*                                                                               
         DC    AL1(OPMAJOR,LOPMAJOR,OPFORUSE) MAJORS                            
         DC    AL2(OUTNUM-OPTELS,INNUM-OPTELS)                                  
*                                                                               
         DC    AL1(OPUNITS,LOPUNITS,OPFORUSE) UNITS                             
         DC    AL2(OUTNUM-OPTELS,INNUM-OPTELS)                                  
*                                                                               
         DC    AL1(OPUSES,LOPUSES,OPFORUSE)   NUMBER OF USES                    
         DC    AL2(OUTNUM-OPTELS,INNUM-OPTELS)                                  
*                                                                               
         DC    AL1(OPUSES2,LOPUSES2,OPFORUSE) SPECIFIC USE NOS.                 
         DC    AL2(OUTUSES2-OPTELS,INUSENMS-OPTELS)                             
*                                                                               
         DC    AL1(OPLCLAU,LOPLCLAU,OPFORUSE)   LAST CLA USE                    
         DC    AL2(OUTNUM-OPTELS,INNUM-OPTELS)                                  
*                                                                               
         DC    AL1(OPINS,LOPINS,OPFORUSE)     NUMBER OF INSERTS                 
         DC    AL2(OUTNUM-OPTELS,INNUM-OPTELS)                                  
*                                                                               
         DC    AL1(OPTAG,LOPTAG,OPFORUSE)     NUMBER OF TAGS                    
         DC    AL2(OUTNUM-OPTELS,INNUM-OPTELS)                                  
*                                                                               
         DC    AL1(OPDEMO,LOPDEMO,OPFORUSE)   NUMBER OF DEMOS                   
         DC    AL2(OUTNUM-OPTELS,INNUM-OPTELS)                                  
*                                                                               
         DC    AL1(OPPCT,LOPPCT,OPFORUSE)     PERCENTAGES                       
         DC    AL2(OUTAMT2-OPTELS,INAMT-OPTELS)                                 
*                                                                               
         DC    AL1(OPAMT,LOPAMT,OPFORUSE)     AMOUNTS                           
         DC    AL2(OUTAMT-OPTELS,INAMT-OPTELS)                                  
*                                                                               
         DC    AL1(OPCYC,LOPCYC,OPFORUSE)     CYCLE DATES                       
         DC    AL2(OUTCYC-OPTELS,INCYC-OPTELS)                                  
*                                                                               
         DC    AL1(OPDATE,LOPDATE,OPFORUSE)   DATES                             
         DC    AL2(OUTDATE-OPTELS,INDATE-OPTELS)                                
*                                                                               
         DC    AL1(OPHIST,LOPHIST,OPFORUSE)   HISTORY ONLY - DON'T PAY          
         DC    AL2(0,0)                                                         
*                                                                               
         DC    AL1(OPL,LOPL,OPFORUSE)         PAYMENT TO LIFT                   
         DC    AL2(0,0)                                                         
*                                                                               
         DC    AL1(OPS,LOPS,OPFORUSE)         PAYMENT TO 2ND LIFT               
         DC    AL2(0,0)                                                         
*                                                                               
         DC    AL1(OPLIFT,LOPLIFT,0)   PERFORMER ON LIFT                        
         DC    AL2(0,0)                                                         
*                                                                               
         DC    AL1(OPLONLY,LOPLONLY,0) PERFORMER ON LIFT ONLY                   
         DC    AL2(0,0)                                                         
*                                                                               
         DC    AL1(OPPNH,LOPPNH,OPFORUSE)     PNH AMOUNT                        
         DC    AL2(OUTAMT-OPTELS,INAMT-OPTELS)                                  
*                                                                               
         DC    AL1(OPSPNH,LOPSPNH,OPFORUSE)   SUBJECT TO PNH AMOUNT             
         DC    AL2(OUTAMT-OPTELS,INAMT-OPTELS)                                  
*                                                                               
         DC    AL1(OPPNHR,LOPPNHR,0)   PNH RATE                                 
         DC    AL2(OUTAMT-OPTELS,INAMT-OPTELS)                                  
*                                                                               
         DC    AL1(OPNO,LOPNO,OPFORUSE)       DON'T INCLUDE                     
         DC    AL2(0,0)                                                         
*                                                                               
**NO-OP**DC    AL1(OPUK,LOPUK,0)       UK FOR FOREIGN USE                       
**NO-OP**DC    AL2(0,0)                                                         
*                                                                               
         DC    AL1(OPAPPLY,LOPAPPLY,OPFORUSE) APPLY SESSION/HLD                 
         DC    AL2(OUTCHAR-OPTELS,INCHAR-OPTELS)                                
*                                                                               
         DC    AL1(OPLAST,LOPLAST,0)   LAST SERVICE DATE                        
         DC    AL2(OUTDATE-OPTELS,INDATE-OPTELS)                                
*                                                                               
         DC    AL1(OPSTATE,LOPSTATE,0) ADDENDUM STATE                           
         DC    AL2(OUTCHAR-OPTELS,INCHAR-OPTELS)                                
*                                                                               
         DC    AL1(OPAFM,LOPAFM,0)     AFM RATE                                 
         DC    AL2(OUTCHAR-OPTELS,INCHAR-OPTELS)                                
*                                                                               
         DC    AL1(OPCOMNM,LOPCOMNM,0) HYPO COMML NAME                          
         DC    AL2(OUTCOMNM-OPTELS,INCOMNM-OPTELS)                              
*                                                                               
         DC    AL1(OPNAP,LOPNAP,0)     NO AUTO PAYMENTS                         
         DC    AL2(OUTDATE-OPTELS,INDATE-OPTELS)                                
*                                                                               
         DC    AL1(OPATYP,LOPATYP,0)   COMMERCIAL ACTRA TYPE                    
         DC    AL2(OUTATYPE-OPTELS,INATYPE-OPTELS)                              
*                                                                               
         DC    AL1(OPCSF,LOPCSF,OPFORUSE)  CONTRACT SERVICE FEE                 
         DC    AL2(OUTCSF-OPTELS,INCSF-OPTELS)                                  
*                                                                               
         DC    AL1(OPHLD,LOPHLD,0)     HOLDING FEE AMT                          
         DC    AL2(OUTAMT-OPTELS,INAMT-OPTELS)                                  
*                                                                               
         DC    AL1(OPHLDR,LOPHLDR,0)   HOLDING FEE RATE                         
         DC    AL2(OUTAMT-OPTELS,INAMT-OPTELS)                                  
*                                                                               
         DC    AL1(OPHLDAS,LOPHLDAS,0) HOLDING FEE AMT AS/OF DATE               
         DC    AL2(OUTHLDAS-OPTELS,INHLDAS-OPTELS)                              
*                                                                               
         DC    AL1(OPHLDRAS,LOPHLDRA,0) HOLDING FEE RATE AS/OF DATE             
         DC    AL2(OUTHLDAS-OPTELS,INHLDAS-OPTELS)                              
*                                                                               
         DC    AL1(OP1ST,LOP1ST,0)      FIRST FIXED CYCLE                       
         DC    AL2(OUTDATE-OPTELS,INDATE-OPTELS)                                
*                                                                               
         DC    AL1(OPHNDTX,LOPHNDTX,OPFORUSE) INCORP. TAX AMT OVERRIDE          
         DC    AL2(OUTAMT-OPTELS,INAMT-OPTELS)                                  
*                                                                               
         DC    X'FF'                                                            
         LTORG                                                                  
         EJECT                                                                  
*              TABLE OF KEYS AND THEIR COMPONENTS                               
KEYTAB   DS    0F                                                               
*                                                                               
         DC    CL3'ES',AL1(TLESCDQ,KEYSUP),AL2(0)                               
         DC    AL1(L'TLDRKEY-1),AL1((KEYESX-KEYES)/KEYLNQ2)                     
KEYES    DC    AL2(TGAGY-TGD),AL1(L'TGAGY-1),AL1(0)                             
         DC    AL2(KEYCHAR-T702DF),AL2(VALAGY-T702DF)                           
         DC    AL2(TGEST-TGD),AL1(L'TGEST-1),AL1(0)                             
         DC    AL2(KEYCHAR-T702DF),AL2(0)                                       
KEYESX   EQU   *                                                                
*                                                                               
         DC    CL3'ESL',AL1(TLESCDQ,KEYSL),AL2(CLRESL-T702DF)                   
         DC    AL1(TLESEST-TLESD-1),AL1((KEYESLX-KEYESL)/KEYLNQ2)               
KEYESL   DC    AL2(TGAGY-TGD),AL1(L'TGAGY-1),AL1(0)                             
         DC    AL2(KEYCHAR-T702DF),AL2(VALAGY-T702DF)                           
         DC    AL2(FILTSTRT-FILTDATA),AL1(L'FILTSTRT-1),AL1(KEYFLOCL)           
         DC    AL2(KEYCHAR-T702DF),AL2(0)                                       
         DC    AL2(FILTCLI-FILTDATA),AL1(L'FILTCLI-1),AL1(KEYFLOCL)             
         DC    AL2(KEYCHAR-T702DF),AL2(VALCLI-T702DF)                           
         DC    AL2(FILTPRD-FILTDATA),AL1(L'FILTPRD-1)                           
         DC    AL1(KEYFLOCL+KEYFNOCL)                                           
         DC    AL2(KEYCHAR-T702DF),AL2(VALPRD-T702DF)                           
         DC    AL2(FILTRDTE-FILTDATA),AL1(L'FILTRDTE-1),AL1(KEYFLOCL)           
         DC    AL2(KEYDATE-T702DF),AL2(0)                                       
         DC    AL2(FILTCDTE-FILTDATA),AL1(L'FILTCDTE-1),AL1(KEYFLOCL)           
         DC    AL2(KEYDATE-T702DF),AL2(0)                                       
KEYESLX  EQU   *                                                                
*                                                                               
         DC    CL3'CAL',AL1(TLCACDQ,KEYSL),AL2(CLRCA-T702DF)                    
         DC    AL1(TLCASORT-TLCAD-1),AL1((KEYCALX-KEYCAL)/KEYLNQ2)              
KEYCAL   DC    AL2(TGCOM-TGD),AL1(0),AL1(KEYFHEX)                               
         DC    AL2(KEYCOM-T702DF),AL2(0)                                        
KEYCALX  EQU   *                                                                
*                                                                               
         DC    CL3'CO',AL1(TLCOICDQ,0),AL2(0)                                   
         DC    AL1(TLCOICOM-TLCOPD-1),AL1((KEYCOX-KEYCO)/KEYLNQ2)               
KEYCO    DC    AL2(TGAGY-TGD),AL1(L'TGAGY-1),AL1(0)                             
         DC    AL2(KEYCHAR-T702DF),AL2(VALAGY-T702DF)                           
         DC    AL2(TGCID-TGD),AL1(L'TGCID-1),AL1(0)                             
         DC    AL2(KEYCHAR-T702DF),AL2(0)                                       
KEYCOX   EQU   *                                                                
*                                                                               
         DC    CL3'GUL',AL1(TLW4CDQ,0),AL2(CLRGU-T702DF)                        
         DC    AL1(L'TLDRKEY-1),AL1((KEYW4X-KEYW4)/KEYLNQ2)                     
KEYW4    DC    AL2(TGSSN-TGD),AL1(L'TGSSN-1),AL1(0)                             
         DC    AL2(KEYCHAR-T702DF),AL2(VALSSN-T702DF)                           
         DC    AL2(FILTAGY-FILTDATA),AL1(L'FILTAGY-1),AL1(KEYFLOCL)             
         DC    AL2(KEYCHAR-T702DF),AL2(VALAGY-T702DF)                           
         DC    AL2(FILTCLI-FILTDATA),AL1(L'FILTCLI-1),AL1(KEYFLOCL)             
         DC    AL2(KEYCHAR-T702DF),AL2(VALCLI-T702DF)                           
KEYW4X   EQU   *                                                                
*                                                                               
KEYSGUL  DC    CL3'GUL',AL1(TLGUCDQ,KEYSL),AL2(CLRGU-T702DF)                    
         DC    AL1(TLGUGUA-TLGUD-1),AL1((KEYGULX-KEYGUL)/KEYLNQ2)               
KEYGUL   DC    AL2(TGSSN-TGD),AL1(L'TGSSN-1),AL1(0)                             
         DC    AL2(KEYCHAR-T702DF),AL2(VALSSN-T702DF)                           
         DC    AL2(FILTAGY-FILTDATA),AL1(L'FILTAGY-1),AL1(KEYFLOCL)             
         DC    AL2(KEYCHAR-T702DF),AL2(VALAGY-T702DF)                           
         DC    AL2(FILTCLI-FILTDATA),AL1(L'FILTCLI-1),AL1(KEYFLOCL)             
         DC    AL2(KEYCHAR-T702DF),AL2(VALCLI-T702DF)                           
KEYGULX  EQU   *                                                                
*                                                                               
         DC    CL3'GU ',AL1(TLGUCDQ,0),AL2(0)                                   
         DC    AL1(L'TLDRKEY-1),AL1((KEYGUX-KEYGU)/KEYLNQ2)                     
KEYGU    DC    AL2(TGSSN-TGD),AL1(L'TGSSN-1),AL1(0)                             
         DC    AL2(KEYCHAR-T702DF),AL2(VALSSN-T702DF)                           
         DC    AL2(TGGUA-TGD),AL1(L'TGGUA-1),AL1(KEYFCMP)                       
         DC    AL2(KEYCHAR-T702DF),AL2(0)                                       
KEYGUX   EQU   *                                                                
*                                                                               
KEYSCAG  DC    CL3'CAG',AL1(TLCAGCDQ,KEYSL),AL2(CLRCAG-T702DF)                  
         DC    AL1(TLCAGCOM-TLCAPD-1),AL1((KEYCAGX-KEYCAG)/KEYLNQ2)             
KEYCAG   DC    AL2(TGSSN-TGD),AL1(L'TGSSN-1),AL1(0)                             
         DC    AL2(KEYCHAR-T702DF),AL2(VALSSN-T702DF)                           
         DC    AL2(TGGUA-TGD),AL1(L'TGGUA-1),AL1(KEYFCMP)                       
         DC    AL2(KEYCHAR-T702DF),AL2(0)                                       
KEYCAGX  EQU   *                                                                
*                                                                               
         DC    CL3'CP',AL1(TLCLCDQ,0),AL2(CLRCP-T702DF)                         
         DC    AL1(L'TLDRKEY-1),AL1((KEYCPX-KEYCP)/KEYLNQ2)                     
KEYCP    DC    AL2(TGAGY-TGD),AL1(L'TGAGY-1),AL1(0)                             
         DC    AL2(KEYCHAR-T702DF),AL2(VALAGY-T702DF)                           
         DC    AL2(TGCLI-TGD),AL1(L'TGCLI-1),AL1(0)                             
         DC    AL2(KEYCHAR-T702DF),AL2(0)                                       
         DC    AL2(TGPRD-TGD),AL1(L'TGPRD-1),AL1(0)                             
         DC    AL2(KEYCHAR-T702DF),AL2(0)                                       
KEYCPX   EQU   *                                                                
*                                                                               
         DC    CL3'COH',AL1(TLCOICDQ,0),AL2(0)                                  
         DC    AL1(TLCOICOM-TLCOPD-1),AL1((KEYCOHX-KEYCOH)/KEYLNQ2)             
KEYCOH   DC    AL2(TGAGY-TGD),AL1(L'TGAGY-1),AL1(0)                             
         DC    AL2(KEYCHAR-T702DF),AL2(VALAGY-T702DF)                           
         DC    AL2(TGCID-TGD),AL1(L'TGCID-1),AL1(0)                             
         DC    AL2(KEYCHAR-T702DF),AL2(0)                                       
KEYCOHX  EQU   *                                                                
*                                                                               
KEYSUHL  DC    CL3'UHL',AL1(0,KEYSL),AL2(0)                                     
         DC    AL1(TLUHCSEQ-TLUHD-1),AL1((KEYUHLX-KEYUHL)/KEYLNQ2)              
KEYUHL   DC    AL2(TGCOM-TGD),AL1(0),AL1(KEYFHEX)                               
         DC    AL2(KEYCOM-T702DF),AL2(0)                                        
KEYUHLX  EQU   *                                                                
*                                                                               
         DC    CL3'UHM',AL1(0,KEYSL),AL2(0) MULT. UHL-SPECIAL KEY PROC.         
         DC    AL1(0),AL1(0)                                                    
*                                                                               
         DC    CL3'COL',AL1(0,KEYSL),AL2(CLRCO-T702DF)                          
         DC    AL1(0),AL1((KEYCOLX-KEYCOL)/KEYLNQ2)                             
KEYCOL   DC    AL2(FILTKEYQ-FILTDATA),AL1(L'FILTKEYQ-1),AL1(KEYFLOCL)           
         DC    AL2(KEYCHAR-T702DF),AL2(0)                                       
         DC    AL2(FILTAGY-FILTDATA),AL1(L'FILTAGY-1),AL1(KEYFLOCL)             
         DC    AL2(KEYCHAR-T702DF),AL2(VALAGY-T702DF)                           
         DC    AL2(FILTCLI-FILTDATA),AL1(L'FILTCLI-1),AL1(KEYFLOCL)             
         DC    AL2(KEYCHAR-T702DF),AL2(VALCLI-T702DF)                           
         DC    AL2(FILTPRD-FILTDATA),AL1(L'FILTPRD-1),AL1(KEYFLOCL)             
         DC    AL2(KEYCHAR-T702DF),AL2(VALPRD-T702DF)                           
         DC    AL2(FILTMED-FILTDATA),AL1(L'FILTMED-1),AL1(KEYFLOCL)             
         DC    AL2(KEYCHAR-T702DF),AL2(0)                                       
         DC    AL2(FILTCLG-FILTDATA),AL1(L'FILTCLG-1),AL1(KEYFLOCL)             
         DC    AL2(KEYCHAR-T702DF),AL2(VALCLG-T702DF)                           
         DC    AL2(FILTSTRT-FILTDATA),AL1(L'FILTSTRT-1),AL1(KEYFLOCL)           
         DC    AL2(KEYCHAR-T702DF),AL2(0)                                       
         DC    AL2(FILTPDTE-FILTDATA),AL1(L'FILTPDTE-1),AL1(KEYFLOCL)           
         DC    AL2(KEYDATE-T702DF),AL2(0)                                       
         DC    AL2(FILTLOCK-FILTDATA),AL1(L'FILTLOCK-1),AL1(KEYFLOCL)           
         DC    AL2(KEYCHAR-T702DF),AL2(0)                                       
         DC    AL2(FILTREL-FILTDATA),AL1(L'FILTREL-1),AL1(KEYFLOCL)             
         DC    AL2(KEYCHAR-T702DF),AL2(0)                                       
         DC    AL2(FILTMUS-FILTDATA),AL1(L'FILTMUS-1),AL1(KEYFLOCL)             
         DC    AL2(KEYCHAR-T702DF),AL2(0)                                       
KEYCOLX  EQU   *                                                                
*                                                                               
         DC    CL3'INI',AL1(0,0),AL2(0) INITIALIZATION NO KEY FLDS              
         DC    AL1(0),AL1(0)                                                    
*                                                                               
         DC    CL3'AY ',AL1(TLAYCDQ,0),AL2(0)                                   
         DC    AL1(L'TLDRKEY-1),AL1((KEYAYX-KEYAY)/KEYLNQ2)                     
KEYAY    DC    AL2(TGAGY-TGD),AL1(L'TGAGY-1),AL1(0)                             
         DC    AL2(KEYCHAR-T702DF),AL2(VALAGY-T702DF)                           
KEYAYX   EQU   *                                                                
*                                                                               
         DC    X'FF'               END OF KEY TABLE                             
         LTORG                                                                  
         EJECT                                                                  
*              TABLE OF ELEMENTS AND THEIR COMPONENTS                           
ELTAB    DS    0F                                                               
         SPACE 1                                                                
         DC    CL2'01',AL1(TA01ELQ,TA01LNQ,ELSFORTB,0)                          
         DC    AL1((ELTA01X-ELTA01)/ELLNQ2)                                     
ELTA01   DC    CL1'A',CL4'CNT',AL1(0,L'TA01CNT,TA01CNT-TA01D)                   
         DC    AL2(DWNECNT-PROCEL,0)                                            
         DC    CL1'B',CL4'CAM',AL1(0,L'TA01CAM,TA01CAM-TA01D)                   
         DC    AL2(DWNECAM-PROCEL,0)                                            
         DC    CL1'C',CL4'UNI',AL1(0,L'TA01UN,TA01UN-TA01D)                     
         DC    AL2(DWNECHAR-PROCEL,0)                                           
         DC    CL1'D',CL4'CORP',AL1(0,L'TA01CORP,TA01CORP-TA01D)                
         DC    AL2(DWNECHAR-PROCEL,0)                                           
         DC    CL1'E',CL4'DBL',AL1(0,L'TA01DBL,TA01DBL-TA01D)                   
         DC    AL2(DWNEDBL-PROCEL,0)                                            
         DC    CL1'F',CL4'OV1',AL1(0,L'TA01OV1,TA01OV1-TA01D)                   
         DC    AL2(DWNECASH-PROCEL,0)                                           
         DC    CL1'G',CL4'OV2',AL1(0,L'TA01OV2,TA01OV2-TA01D)                   
         DC    AL2(DWNECASH-PROCEL,0)                                           
ELTA01X  EQU   *                                                                
*                                                                               
         DC    CL2'02',AL1(TA02ELQ,TA02LNQ,ELSFORTB,0)                          
         DC    AL1((ELTA02X-ELTA02)/ELLNQ2)                                     
ELTA02   DC    CL1'A',CL4'CODE',AL1(0,L'TA02CTYP,TA02CTYP-TA02D)                
         DC    AL2(DWNECHAR-PROCEL,0)                                           
         DC    CL1'B',CL4'CTYP',AL1(0,L'TA02CTYN,TA02CTYN-TA02D)                
         DC    AL2(DWNECHAR-PROCEL,0)                                           
         DC    CL1'C',CL4'HLD',AL1(0,L'TA02AHLD,TA02AHLD-TA02D)                 
         DC    AL2(DWNECHAR-PROCEL,0)                                           
ELTA02X  EQU   *                                                                
*                                                                               
         DC    CL2'03',AL1(TA03ELQ,TA03LNQ,ELSFORTB,0)                          
         DC    AL1((ELTA03X-ELTA03)/ELLNQ2)                                     
ELTA03   DC    CL1'A',CL4'ADST',AL1(0,L'TA03ADST,TA03ADST-TA03D)                
         DC    AL2(DWNECHAR-PROCEL,0)                                           
         DC    CL1'B',CL4'ADEM',AL1(0,L'TA03ADEM,TA03ADEM-TA03D)                
         DC    AL2(DWNECHAR-PROCEL,0)                                           
ELTA03X  EQU   *                                                                
*                                                                               
         DC    CL2'04',AL1(TA04ELQ,TA04LNQ,ELSFORTB,0)                          
         DC    AL1((ELTA04X-ELTA04)/ELLNQ2)                                     
ELTA04   DC    CL1'A',CL4'CCDE',AL1(0,L'TA04CCDE,TA04CCDE-TA04D)                
         DC    AL2(DWNECHAR-PROCEL,0)                                           
         DC    CL1'B',CL4'CCNM',AL1(0,L'TA04CCN,TA04CCN-TA04D)                  
         DC    AL2(DWNECHAR-PROCEL,0)                                           
ELTA04X  EQU   *                                                                
*                                                                               
         DC    CL2'05',AL1(TA05ELQ,TA05LNQ,ELSFRESL,0)                          
         DC    AL1((ELTA05X-ELTA05)/ELLNQ2)                                     
ELTA05   DC    CL1'A',CL4'ID',AL1(0,L'TA05ID,TA05ID-TA05D)                      
         DC    AL2(DWNECHAR-PROCEL,0)                                           
         DC    CL1'B',CL4'NAME',AL1(0,L'TA05NAME,TA05NAME-TA05D)                
         DC    AL2(DWNECHAR-PROCEL,0)                                           
         DC    CL1'C',CL4'RDTE',AL1(0,L'TA05RDTE,TA05RDTE-TA05D)                
         DC    AL2(DWNEDATE-PROCEL,0)                                           
         DC    CL1'D',CL4'CDTE',AL1(0,L'TA05CDTE,TA05CDTE-TA05D)                
         DC    AL2(DWNEDATE-PROCEL,0)                                           
ELTA05X  EQU   *                                                                
*                                                                               
         DC    CL2'AC',AL1(TAACELQ,TAACLNQ,ELSFORES,0)                          
         DC    AL1((ELTAACX-ELTAAC)/ELLNQ2)                                     
ELTAAC   DC    CL1'A',CL4'STAF',AL1(0,L'TAACSTAF,TAACSTAF-TAACD)                
         DC    AL2(DWNECHAR-PROCEL,0)                                           
         DC    CL1'B',CL4'DATE',AL1(0,L'TAACCDTE+L'TAACCTIM)                    
         DC    AL1(TAACCDTE-TAACD)                                              
         DC    AL2(DWNEDTIM-PROCEL,0)                                           
         DC    CL1'C',CL4'SCRN',AL1(0,L'TAACSCR,TAACSCR-TAACD)                  
         DC    AL2(DWNEHEX-PROCEL,0)                                            
         DC    CL1'D',CL4'NAME',AL1(0,L'TAACID,TAACID-TAACD)                    
         DC    AL2(DWNESTAF-PROCEL,0)                                           
ELTAACX  EQU   *                                                                
*                                                                               
         DC    CL2'AY',AL1(TAAYELQ,TAAYLNQ,0,0)                                 
         DC    AL1((ELTAAYX-ELTAAY)/ELLNQ2)                                     
ELTAAY   DC    CL1'A',CL4'STAT',AL1(0,L'TAAYSTAT,TAAYSTAT-TAAYD)                
         DC    AL2(DWNEDIT-PROCEL,0)                                            
ELTAAYX  EQU   *                                                                
*                                  ** FOR INITIALIZATION READ                   
         DC    CL2'AY',AL1(TAAYELQ,TAAYLNQ,0,C'A')                              
         DC    AL1((ELTAAY2X-ELTAAY2)/ELLNQ2)                                   
ELTAAY2  DC    CL1'A',CL4'STAT',AL1(0,L'TAAYSTAT,TAAYSTAT-TAAYD)                
         DC    AL2(DWNEDIT-PROCEL,0)                                            
         DC    CL1'B',CL4'AGY',AL1(0,L'TGAGY,0)                                 
         DC    AL2(DWNEAGY-PROCEL,0)                                            
ELTAAY2X EQU   *                                                                
*                                                                               
         DC    CL2'CA',AL1(TACAELQ,TACALNQ,0,0)                                 
         DC    AL1((ELTACAX-ELTACA)/ELLNQ2)                                     
ELTACA   DC    CL1'A',CL4'SSN',AL1(ELFXUHM,0,0)                                 
         DC    AL2(DWNESSN-PROCEL,0)                                            
         DC    CL1'B',CL4'FRST',AL1(ELFXUHM,0,0)                                
         DC    AL2(DWNESSNF-PROCEL,0)                                           
         DC    CL1'C',CL4'LAST',AL1(ELFXUHM,0,0)                                
         DC    AL2(DWNESSNL-PROCEL,0)                                           
**NO-OP**DC    CL1'K',CL4'W4TY',AL1(0,0,0)                                      
**NO-OP**DC    AL2(DWNESSNT-PROCEL,0)                                           
         DC    CL1'D',CL4'SEQ',AL1(0,0,0)                                       
         DC    AL2(DWNECSEQ-PROCEL,0)                                           
         DC    CL1'E',CL4'CAT',AL1(0,0,0)                                       
         DC    AL2(DWNECAT-PROCEL,0)                                            
         DC    CL1'F',CL4'CAM',AL1(0,L'TACAONOF,TACAONOF-TACAD)                 
         DC    AL2(DWNECAM-PROCEL,0)                                            
         DC    CL1'G',CL4'UNI',AL1(0,L'TACAUN,TACAUN-TACAD)                     
         DC    AL2(DWNEUNI-PROCEL,0)                                            
         DC    CL1'H',CL4'OV2',AL1(0,L'TACAOV2,TACAOV2-TACAD)                   
         DC    AL2(DWNECASH-PROCEL,0)                                           
         DC    CL1'I',CL4'STAT',AL1(ELFXUHM,L'TACASTAT,TACASTAT-TACAD)          
         DC    AL2(DWNEDIT-PROCEL,0)                                            
         DC    CL1'J',CL4'GUA',AL1(ELFXUHM,L'TACAGUA,TACAGUA-TACAD)             
         DC    AL2(DWNEGUAG-PROCEL,0)                                           
         DC    CL1'K',CL4'YEAR',AL1(0,L'TACAYEAR,TACAYEAR-TACAD)                
         DC    AL2(DWNEYEAR-PROCEL,0)                                           
         DC    CL1'L',CL4'STA2',AL1(ELFXUHM,L'TACASTA4,TACASTA4-TACAD)          
         DC    AL2(DWNEDIT-PROCEL,0)                                            
ELTACAX  EQU   *                                                                
*                                                                               
         DC    CL2'CM',AL1(TACMELQ,TACMLNQ,ELSFORES,0)                          
         DC    AL1((ELTACMX-ELTACM)/ELLNQ2)                                     
ELTACM   DC    CL1'A',CL4'COMM',AL1(ELFADDLN,0,TACMCOMM-TACMD)                  
         DC    AL2(DWNECHAR-PROCEL,UPECHAR-UPEFLD)                              
ELTACMX  EQU   *                                                                
*                                  THIS TACOD IS FOR GU READ                    
         DC    CL2'CO',AL1(TACOELQ,TACOLNQ,0,0)                                 
         DC    AL1((ELTACOX-ELTACO)/ELLNQ2)                                     
ELTACO   DC    CL1'A',CL4'CID',AL1(0,L'TACOCID,TACOCID-TACOD)                   
         DC    AL2(DWNECHAR-PROCEL,0)                                           
ELTACOX  EQU   *                                                                
*                                  THIS TACOD IS FOR CO READ                    
         DC    CL2'CO',AL1(TACOELQ,TACOLNQ,0,C'A')                              
         DC    AL1((ELTACO2X-ELTACO2)/ELLNQ2)                                   
ELTACO2  DC    CL1'A',CL4'CID',AL1(0,L'TACOCID,TACOCID-TACOD)                   
         DC    AL2(DWNECHAR-PROCEL,0)                                           
         DC    CL1'B',CL4'NAME',AL1(0,0,0)                                      
         DC    AL2(DWNECIDN-PROCEL,0)                                           
         DC    CL1'C',CL4'MED',AL1(0,L'TACOMED,TACOMED-TACOD)                   
         DC    AL2(DWNEMED-PROCEL,0)                                            
         DC    CL1'D',CL4'FCYC',AL1(0,0,0)                                      
         DC    AL2(DWNECIDF-PROCEL,0)                                           
         DC    CL1'E',CL4'EXP',AL1(0,L'TACOEXP,TACOEXP-TACOD)                   
         DC    AL2(DWNEDATE-PROCEL,0)                                           
         DC    CL1'F',CL4'COM',AL1(0,0,0)                                       
         DC    AL2(DWNECOM2-PROCEL,0)                                           
         DC    CL1'G',CL4'CLI',AL1(0,0,0)                                       
         DC    AL2(DWNECLI-PROCEL,0)                                            
         DC    CL1'H',CL4'PRD',AL1(0,0,0)                                       
         DC    AL2(DWNEPRD-PROCEL,0)                                            
         DC    CL1'I',CL4'CAST',AL1(0,0,0)                                      
         DC    AL2(DWNECAST-PROCEL,0)                                           
         DC    CL1'J',CL4'LIFT',AL1(0,0,0)                                      
         DC    AL2(DWNELIFT-PROCEL,0)                                           
         DC    CL1'K',CL4'REL',AL1(0,L'TACOSTAT,TACOSTAT-TACOD)                 
         DC    AL2(DWNEREL-PROCEL,0)                                            
         DC    CL1'L',CL4'TYPE',AL1(0,L'TACOTYPE,TACOTYPE-TACOD)                
         DC    AL2(DWNECHAR-PROCEL,0)                                           
         DC    CL1'M',CL4'ADST',AL1(0,L'TACOADST,TACOADST-TACOD)                
         DC    AL2(DWNECHAR-PROCEL,0)                                           
ELTACO2X EQU   *                                                                
*                                                                               
         DC    CL2'EM',AL1(TAEMELQ,TAEMLNQ,ELSFORES,0)                          
         DC    AL1((ELTAEMX-ELTAEM)/ELLNQ2)                                     
ELTAEM   DC    CL1'A',CL4'CASH',AL1(0,L'TAEMAMT,TAEMAMT-TAEMD)                  
         DC    AL2(DWNECASH-PROCEL,UPECASH-UPEFLD)                              
         DC    CL1'B',CL4'TXT',AL1(ELFADDLN,0,TAEMTXT-TAEMD)                    
         DC    AL2(DWNECHAR-PROCEL,UPECHAR-UPEFLD)                              
ELTAEMX  EQU   *                                                                
*                                                                               
         DC    CL2'EN',AL1(TAENELQ,TAENLNQ,ELSFORES,0)                          
         DC    AL1((ELTAENX-ELTAEN)/ELLNQ2)                                     
ELTAEN   DC    CL1'A',CL4'NARR',AL1(ELFADDLN,0,TAENNARR-TAEND)                  
         DC    AL2(DWNECHAR-PROCEL,UPECHAR-UPEFLD)                              
ELTAENX  EQU   *                                                                
*                                                                               
         DC    CL2'NA',AL1(TANAELQ,TANALNQ,ELSFORES+ELSFORCP,0)                 
         DC    AL1((ELTANAX-ELTANA)/ELLNQ2)                                     
ELTANA   DC    CL1'A',CL4'NAME',AL1(ELFADDLN,0,TANANAME-TANAD)                  
         DC    AL2(DWNECHAR-PROCEL,UPECHAR-UPEFLD)                              
ELTANAX  EQU   *                                                                
*                                                                               
         DC    CL2'NU',AL1(TANUELQ,TANULNQ,ELSFORES,0)                          
         DC    AL1((ELTANUX-ELTANU)/ELLNQ2)                                     
ELTANU   DC    CL1'A',CL4'NUM',AL1(ELFADDLN,0,TANUMBER-TANUD)                   
         DC    AL2(DWNECHAR-PROCEL,UPETANU-UPEFLD)                              
ELTANUX  EQU   *                                                                
*                                                                               
         DC    CL2'ES',AL1(TAESELQ,TAESLNQ,ELSFORES,TAESTCLI)                   
         DC    AL1((ELESCLIX-ELESCLI)/ELLNQ2)                                   
ELESCLI  DC    CL1'A',CL4'TYPE',AL1(0,L'TAESTYPE,TAESTYPE-TAESD)                
         DC    AL2(DWNEDIT-PROCEL,UPENUM-UPEFLD)                                
         DC    CL1'B',CL4'CLI',AL1(0,L'TAESCLI,TAESDATA-TAESD)                  
         DC    AL2(DWNECHAR-PROCEL,UPECHAR-UPEFLD)                              
         DC    CL5'Z',AL1(ELFIDWNX+ELFXUP,L'TAESSBNO,TAESSBNO-TAESD)            
         DC    AL2(DWNEDIT-PROCEL,0)                                            
         DC    CL5'Z',AL1(0,0,TAESSBNO-TAESD)                                   
         DC    AL2(DWNEOPTS-PROCEL,UPEOPTS-UPEFLD)                              
ELESCLIX EQU   *                                                                
*                                                                               
         DC    CL2'ES',AL1(TAESELQ,TAESLNQ,ELSFORES,TAESTPRD)                   
         DC    AL1((ELESPRDX-ELESPRD)/ELLNQ2)                                   
ELESPRD  DC    CL1'A',CL4'TYPE',AL1(0,L'TAESTYPE,TAESTYPE-TAESD)                
         DC    AL2(DWNEDIT-PROCEL,UPENUM-UPEFLD)                                
         DC    CL1'C',CL4'PRD',AL1(0,L'TAESPRD,TAESDATA-TAESD)                  
         DC    AL2(DWNECHAR-PROCEL,UPECHAR-UPEFLD)                              
         DC    CL5'Z',AL1(ELFIDWNX+ELFXUP,L'TAESSBNO,TAESSBNO-TAESD)            
         DC    AL2(DWNEDIT-PROCEL,0)                                            
         DC    CL5'Z',AL1(0,0,TAESSBNO-TAESD)                                   
         DC    AL2(DWNEOPTS-PROCEL,UPEOPTS-UPEFLD)                              
ELESPRDX EQU   *                                                                
*                                                                               
         DC    CL2'ES',AL1(TAESELQ,TAESLNQ,ELSFORES,TAESTCOM)                   
         DC    AL1((ELESCIDX-ELESCID)/ELLNQ2)                                   
ELESCID  DC    CL1'A',CL4'TYPE',AL1(0,L'TAESTYPE,TAESTYPE-TAESD)                
         DC    AL2(DWNEDIT-PROCEL,UPENUM-UPEFLD)                                
         DC    CL1'D',CL4'COM',AL1(0,L'TAESCOM,TAESCOM-TAESD)                   
         DC    AL2(DWNECOM-PROCEL,UPEHEXIN-UPEFLD)                              
         DC    CL1'E',CL4'CID',AL1(ELFIDWNX,L'TAESCID,TAESCID-TAESD)            
         DC    AL2(DWNECHAR-PROCEL,UPECHAR-UPEFLD)                              
         DC    CL5'Z',AL1(ELFIDWNX+ELFXUP,L'TAESSBNO,TAESSBNO-TAESD)            
         DC    AL2(DWNEDIT-PROCEL,0)                                            
         DC    CL5'Z',AL1(0,0,TAESSBNO-TAESD)                                   
         DC    AL2(DWNEOPTS-PROCEL,UPEOPTS-UPEFLD)                              
ELESCIDX EQU   *                                                                
*                                                                               
         DC    CL2'ES',AL1(TAESELQ,TAESLNQ,ELSFORES,TAESTPER)                   
         DC    AL1((ELESPERX-ELESPER)/ELLNQ2)                                   
ELESPER  DC    CL1'A',CL4'TYPE',AL1(0,L'TAESTYPE,TAESTYPE-TAESD)                
         DC    AL2(DWNEDIT-PROCEL,UPENUM-UPEFLD)                                
         DC    CL1'J',CL4'SORT',AL1(0,0,TAESSORT-TAESD)                         
         DC    AL2(DWNESORT-PROCEL,UPEHEXIN-UPEFLD)                             
         DC    CL1'K',CL4'SSN',AL1(0,0,TAESSSN-TAESD)                           
         DC    AL2(DWNESSNG-PROCEL,UPESSN-UPEFLD)                               
         DC    CL5'Z',AL1(ELFIDWNX+ELFXUP,L'TAESSBNO,TAESSBNO-TAESD)            
         DC    AL2(DWNEDIT-PROCEL,0)                                            
         DC    CL5'Z',AL1(0,0,TAESSBNO-TAESD)                                   
         DC    AL2(DWNEOPTS-PROCEL,UPEOPTS-UPEFLD)                              
*                                                                               
         DC    CL5'EL',AL1(ELFXDWNX,0,0)  READS TLW4D RECORD                    
         DC    AL2(DWNETLW4-PROCEL,0)                                           
         DC    CL5'EL',AL1(ELFXDWNX,0,0)  READS TLCAD REC VIA TLCACCDQ          
         DC    AL2(DWNECAC-PROCEL,0)                                            
*                                                                               
         DC    CL5'EL',AL1(ELFXDWNX,0,0)    PROCESS TAOAD ELEMENT               
         DC    AL2(DWNETAOA-PROCEL,0)                                           
         DC    CL5'EL',AL1(ELFXDWNX,0,0)    PROCESS TAOPD ELEMENT               
         DC    AL2(DWNETAOP-PROCEL,0)                                           
         DC    CL5'EL',AL1(ELFXDWNX,0,0)    SETS EL=CA                          
         DC    AL2(DWNEELCA-PROCEL,0)                                           
*                                  NEXT ENTRY(S) MUST MATCH ELTACA              
         DC    CL1'A',CL4'SSN',AL1(ELFXDWNX,0,0)                                
         DC    AL2(DWNESSN-PROCEL,0)                                            
         DC    CL1'B',CL4'FRST',AL1(ELFXDWNX,0,0)                               
         DC    AL2(DWNESSNF-PROCEL,0)                                           
         DC    CL1'C',CL4'LAST',AL1(ELFXDWNX,0,0)                               
         DC    AL2(DWNESSNL-PROCEL,0)                                           
**NO-OP**DC    CL1'K',CL4'W4TY',AL1(ELFXDWNX,0,0)                               
**NO-OP**DC    AL2(DWNESSNT-PROCEL,0)                                           
         DC    CL1'D',CL4'SEQ',AL1(ELFXDWNX,0,0)                                
         DC    AL2(DWNECSEQ-PROCEL,0)                                           
         DC    CL1'E',CL4'CAT',AL1(ELFXDWNX,0,0)                                
         DC    AL2(DWNECAT-PROCEL,0)                                            
         DC    CL1'F',CL4'CAM',AL1(ELFXDWNX,L'TACAONOF,TACAONOF-TACAD)          
         DC    AL2(DWNECAM-PROCEL,0)                                            
         DC    CL1'G',CL4'UNI',AL1(ELFXDWNX,L'TACAUN,TACAUN-TACAD)              
         DC    AL2(DWNECHAR-PROCEL,0)                                           
         DC    CL1'H',CL4'OV2',AL1(ELFXDWNX,L'TACAOV2,TACAOV2-TACAD)            
         DC    AL2(DWNECASH-PROCEL,0)                                           
         DC    CL1'I',CL4'STAT',AL1(ELFXDWNX,L'TACASTAT,TACASTAT-TACAD)         
         DC    AL2(DWNEDIT-PROCEL,0)                                            
         DC    CL1'J',CL4'GUA',AL1(ELFXDWNX,L'TACAGUA,TACAGUA-TACAD)            
         DC    AL2(DWNEGUAG-PROCEL,0)                                           
         DC    CL1'K',CL4'YEAR',AL1(ELFXDWNX,L'TACAYEAR,TACAYEAR-TACAD)         
         DC    AL2(DWNEYEAR-PROCEL,0)                                           
ELESPERX EQU   *                                                                
*                                                                               
         DC    CL2'ES',AL1(TAESELQ,TAESLNQ,ELSFORES,TAESTHCO)                   
         DC    AL1((ELESHCOX-ELESHCO)/ELLNQ2)                                   
ELESHCO  DC    CL1'A',CL4'TYPE',AL1(0,L'TAESTYPE,TAESTYPE-TAESD)                
         DC    AL2(DWNEDIT-PROCEL,UPENUM-UPEFLD)                                
         DC    CL1'O',CL4'HCOM',AL1(0,L'TAESHCOM,TAESHCOM-TAESD)                
         DC    AL2(DWNECHAR-PROCEL,UPECHAR-UPEFLD)                              
         DC    CL1'P',CL4'HMED',AL1(0,L'TAESHMED,TAESHMED-TAESD)                
         DC    AL2(DWNEMEDQ-PROCEL,UPEMED-UPEFLD)                               
         DC    CL1'Q',CL4'HLEN',AL1(0,L'TAESHLEN,TAESHLEN-TAESD)                
         DC    AL2(DWNEDIT-PROCEL,UPENUM-UPEFLD)                                
         DC    CL5'Z',AL1(ELFIDWNX+ELFXUP,L'TAESSBNO,TAESSBNO-TAESD)            
         DC    AL2(DWNEDIT-PROCEL,0)                                            
         DC    CL5'Z',AL1(0,0,TAESSBNO-TAESD)                                   
         DC    AL2(DWNEOPTS-PROCEL,UPEOPTS-UPEFLD)                              
ELESHCOX EQU   *                                                                
*                                                                               
         DC    CL2'ES',AL1(TAESELQ,TAESLNQ,ELSFORES,TAESTHPE)                   
         DC    AL1((ELESHPEX-ELESHPE)/ELLNQ2)                                   
ELESHPE  DC    CL1'A',CL4'TYPE',AL1(0,L'TAESTYPE,TAESTYPE-TAESD)                
         DC    AL2(DWNEDIT-PROCEL,UPENUM-UPEFLD)                                
         DC    CL1'T',CL4'HSTA',AL1(0,L'TAESHSTA,TAESHSTA-TAESD)                
         DC    AL2(DWNEDIT-PROCEL,UPENUM-UPEFLD)                                
         DC    CL1'U',CL4'HUNI',AL1(0,L'TAESHUNI,TAESHUNI-TAESD)                
         DC    AL2(DWNEUNIQ-PROCEL,UPEUNI-UPEFLD)                               
         DC    CL1'V',CL4'HCAT',AL1(0,L'TAESHCAT,TAESHCAT-TAESD)                
         DC    AL2(DWNECATQ-PROCEL,UPECAT-UPEFLD)                               
         DC    CL1'W',CL4'HNPR',AL1(0,L'TAESHNPR,TAESHNPR-TAESD)                
         DC    AL2(DWNECNT-PROCEL,UPENUM-UPEFLD)                                
         DC    CL1'X',CL4'HLFT',AL1(0,L'TAESHLFT,TAESHLFT-TAESD)                
         DC    AL2(DWNECHAR-PROCEL,UPECHAR-UPEFLD)                              
         DC    CL1'Y',CL4'HDBL',AL1(0,L'TAESHDBL,TAESHDBL-TAESD)                
         DC    AL2(DWNECHAR-PROCEL,UPECHAR-UPEFLD)                              
         DC    CL5'Z',AL1(ELFIDWNX+ELFXUP,L'TAESSBNO,TAESSBNO-TAESD)            
         DC    AL2(DWNEDIT-PROCEL,0)                                            
         DC    CL5'Z',AL1(0,0,TAESSBNO-TAESD)                                   
         DC    AL2(DWNEOPTS-PROCEL,UPEOPTS-UPEFLD)                              
ELESHPEX EQU   *                                                                
*                                                                               
         DC    CL2'GU',AL1(TAGUELQ,TAGULNQ,0,0)                                 
         DC    AL1((ELTAGUX-ELTAGU)/ELLNQ2)                                     
ELTAGU   DC    CL1'A',CL4'GUAR',AL1(0,0,0)                                      
         DC    AL2(DWNEGUA-PROCEL,0)                                            
         DC    CL1'B',CL4'STRT',AL1(0,L'TAGUSTRT,TAGUSTRT-TAGUD)                
         DC    AL2(DWNEDATE-PROCEL,0)                                           
         DC    CL1'C',CL4'END',AL1(0,L'TAGUEND,TAGUEND-TAGUD)                   
         DC    AL2(DWNEDATE-PROCEL,0)                                           
         DC    CL1'D',CL4'BAL',AL1(0,L'TAGUBAL,TAGUBAL-TAGUD)                   
         DC    AL2(DWNECASH-PROCEL,0)                                           
*                                  NEXT ENTRY XITS IF TAGUCOM = 0               
         DC    CL5' ',AL1(0,0,TAGUCOM-TAGUD)                                    
         DC    AL2(DWNECOMG-PROCEL,0)                                           
         DC    CL1'E',CL4'EDAT',AL1(0,0,0)                                      
         DC    AL2(DWNEGC-PROCEL,0)                                             
         DC    CL1'F',CL4'CID',AL1(0,0,TAGUCOM-TAGUD)                           
         DC    AL2(DWNECID-PROCEL,0)                                            
         DC    CL5'  ',AL1(0,0,0)    READS TLCAD THROUGH TLCAGDQ PTR            
         DC    AL2(DWNECAG-PROCEL,0)                                            
         DC    CL5'EL',AL1(0,0,0)    PROCESS TAOAD ELEMENT                      
         DC    AL2(DWNETAOA-PROCEL,0)                                           
ELTAGUX  EQU   *                                                                
*                                                                               
         DC    CL2'OA',AL1(TAOAELQ,TAOALNQ,0,0)                                 
         DC    AL1((ELTAOAX-ELTAOA)/ELLNQ2)                                     
ELTAOA   DC    CL5' ',AL1(0,0,TAOANUM-TAOAD)                                    
         DC    AL2(DWNELAP-PROCEL,0)                                            
ELTAOAX  EQU   *                                                                
*                                                                               
         DC    CL2'OP',AL1(TAOPELQ,TAOPLNQ,0,0)                                 
         DC    AL1((ELTAOPX-ELTAOP)/ELLNQ2)                                     
ELTAOP   DC    CL5' ',AL1(0,0,TAOPNUM-TAOPD)                                    
         DC    AL2(DWNELAP-PROCEL,0)                                            
ELTAOPX  EQU   *                                                                
*                                                                               
         DC    CL2'UH',AL1(TAUHELQ,TAUHLNQ,0,0)                                 
         DC    AL1((ELTAUHX-ELTAUH)/ELLNQ2)                                     
ELTAUH   DC    CL1'A',CL4'USE',AL1(0,0,0)                                       
         DC    AL2(DWNEUSE-PROCEL,0)                                            
         DC    CL1'B',CL4'TYPE',AL1(0,L'TAUHTYPE,TAUHTYPE-TAUHD)                
         DC    AL2(DWNEUSET-PROCEL,0)                                           
         DC    CL1'C',CL4'CYCS',AL1(0,L'TAUHSTRT,TAUHSTRT-TAUHD)                
         DC    AL2(DWNEDATE-PROCEL,0)                                           
         DC    CL1'D',CL4'CYCE',AL1(0,L'TAUHEND,TAUHEND-TAUHD)                  
         DC    AL2(DWNEDATE-PROCEL,0)                                           
ELTAUHX  EQU   *                                                                
         SPACE 1                                                                
         DC    CL2'UH',AL1(TAUHELQ,TAUHLNQ,0,C'A')                              
         DC    AL1((ELTAUHAX-ELTAUHA)/ELLNQ2)                                   
ELTAUHA  DC    CL1'A',CL4'USE',AL1(0,0,0)                                       
         DC    AL2(DWNEUSE-PROCEL,0)                                            
         DC    CL1'B',CL4'TYPE',AL1(0,L'TAUHTYPE,TAUHTYPE-TAUHD)                
         DC    AL2(DWNEUSET-PROCEL,0)                                           
         DC    CL1'C',CL4'CYCS',AL1(0,L'TAUHSTRT,TAUHSTRT-TAUHD)                
         DC    AL2(DWNEDATE-PROCEL,0)                                           
         DC    CL1'D',CL4'CYCE',AL1(0,L'TAUHEND,TAUHEND-TAUHD)                  
         DC    AL2(DWNEDATE-PROCEL,0)                                           
         DC    CL1'E',CL4'USE',AL1(0,L'TAUHUSN,TAUHUSN-TAUHD)                   
         DC    AL2(DWNEDIT-PROCEL,0)                                            
*NOTWANTEDC    CL1'F',CL4'USEL',AL1(0,L'TAUHUSNL,TAUHUSNL-TAUHD)                
*NOTWANTEDC    AL2(DWNEDIT-PROCEL,0)                                            
ELTAUHAX EQU   *                                                                
         SPACE 1                                                                
         DC    CL2'UH',AL1(TAUHELQ,TAUHLNQ,0,C'B')                              
         DC    AL1((ELTAUHBX-ELTAUHB)/ELLNQ2)                                   
ELTAUHB  DC    CL1'A',CL4'USE',AL1(0,0,0)                                       
         DC    AL2(DWNEUSE-PROCEL,0)                                            
         DC    CL1'B',CL4'TYPE',AL1(0,L'TAUHTYPE,TAUHTYPE-TAUHD)                
         DC    AL2(DWNEUSET-PROCEL,0)                                           
         DC    CL1'C',CL4'CYCS',AL1(0,L'TAUHSTRT,TAUHSTRT-TAUHD)                
         DC    AL2(DWNEDATE-PROCEL,0)                                           
         DC    CL1'D',CL4'CYCE',AL1(0,L'TAUHEND,TAUHEND-TAUHD)                  
         DC    AL2(DWNEDATE-PROCEL,0)                                           
         DC    CL1'G',CL4'DEMS',AL1(0,L'TAUHDEMS,TAUHDEMS-TAUHD)                
         DC    AL2(DWNEDIT-PROCEL,0)                                            
ELTAUHBX EQU   *                                                                
         SPACE 1                                                                
         DC    CL2'UH',AL1(TAUHELQ,TAUHLNQ,0,C'C')                              
         DC    AL1((ELTAUHCX-ELTAUHC)/ELLNQ2)                                   
ELTAUHC  DC    CL1'A',CL4'USE',AL1(0,0,0)                                       
         DC    AL2(DWNEUSE-PROCEL,0)                                            
         DC    CL1'B',CL4'TYPE',AL1(0,L'TAUHTYPE,TAUHTYPE-TAUHD)                
         DC    AL2(DWNEUSET-PROCEL,0)                                           
         DC    CL1'C',CL4'CYCS',AL1(0,L'TAUHSTRT,TAUHSTRT-TAUHD)                
         DC    AL2(DWNEDATE-PROCEL,0)                                           
         DC    CL1'D',CL4'CYCE',AL1(0,L'TAUHEND,TAUHEND-TAUHD)                  
         DC    AL2(DWNEDATE-PROCEL,0)                                           
         DC    CL1'H',CL4'TAGS',AL1(0,L'TAUHTAGS,TAUHTAGS-TAUHD)                
         DC    AL2(DWNEDIT-PROCEL,0)                                            
ELTAUHCX EQU   *                                                                
         SPACE 1                                                                
         DC    CL2'UH',AL1(TAUHELQ,TAUHLNQ,0,C'D')                              
         DC    AL1((ELTAUHDX-ELTAUHD)/ELLNQ2)                                   
ELTAUHD  DC    CL1'A',CL4'USE',AL1(0,0,0)                                       
         DC    AL2(DWNEUSE-PROCEL,0)                                            
         DC    CL1'B',CL4'TYPE',AL1(0,L'TAUHTYPE,TAUHTYPE-TAUHD)                
         DC    AL2(DWNEUSET-PROCEL,0)                                           
         DC    CL1'C',CL4'CYCS',AL1(0,L'TAUHSTRT,TAUHSTRT-TAUHD)                
         DC    AL2(DWNEDATE-PROCEL,0)                                           
         DC    CL1'D',CL4'CYCE',AL1(0,L'TAUHEND,TAUHEND-TAUHD)                  
         DC    AL2(DWNEDATE-PROCEL,0)                                           
         DC    CL1'I',CL4'RMAJ',AL1(0,L'TAUHRMAJ,TAUHRMAJ-TAUHD)                
         DC    AL2(DWNEDIT-PROCEL,0)                                            
ELTAUHDX EQU   *                                                                
         SPACE 1                                                                
         DC    CL2'UH',AL1(TAUHELQ,TAUHLNQ,0,C'E')                              
         DC    AL1((ELTAUHEX-ELTAUHE)/ELLNQ2)                                   
ELTAUHE  DC    CL1'A',CL4'USE',AL1(0,0,0)                                       
         DC    AL2(DWNEUSE-PROCEL,0)                                            
         DC    CL1'B',CL4'TYPE',AL1(0,L'TAUHTYPE,TAUHTYPE-TAUHD)                
         DC    AL2(DWNEUSET-PROCEL,0)                                           
         DC    CL1'C',CL4'CYCS',AL1(0,L'TAUHSTRT,TAUHSTRT-TAUHD)                
         DC    AL2(DWNEDATE-PROCEL,0)                                           
         DC    CL1'D',CL4'CYCE',AL1(0,L'TAUHEND,TAUHEND-TAUHD)                  
         DC    AL2(DWNEDATE-PROCEL,0)                                           
         DC    CL1'J',CL4'INS',AL1(0,L'TAUHINS,TAUHINS-TAUHD)                   
         DC    AL2(DWNEDIT-PROCEL,0)                                            
*NOTWANTEDC    CL1'K',CL4'STAT',AL1(0,L'TAUHSTAT,TAUHSTAT-TAUHD)                
*NOTWANTEDC    AL2(DWNEDIT-PROCEL,0)                                            
ELTAUHEX EQU   *                                                                
         SPACE 1                                                                
         DC    CL2'UH',AL1(TAUHELQ,TAUHLNQ,0,C'F')                              
         DC    AL1((ELTAUHFX-ELTAUHF)/ELLNQ2)                                   
ELTAUHF  DC    CL1'A',CL4'USE',AL1(0,0,0)                                       
         DC    AL2(DWNEUSE-PROCEL,0)                                            
         DC    CL1'B',CL4'TYPE',AL1(0,L'TAUHTYPE,TAUHTYPE-TAUHD)                
         DC    AL2(DWNEUSET-PROCEL,0)                                           
         DC    CL1'C',CL4'CYCS',AL1(0,L'TAUHSTRT,TAUHSTRT-TAUHD)                
         DC    AL2(DWNEDATE-PROCEL,0)                                           
         DC    CL1'D',CL4'CYCE',AL1(0,L'TAUHEND,TAUHEND-TAUHD)                  
         DC    AL2(DWNEDATE-PROCEL,0)                                           
         DC    CL1'L',CL4'FMAJ',AL1(0,L'TAUHFMAJ,TAUHFMAJ-TAUHD)                
         DC    AL2(DWNEDIT-PROCEL,0)                                            
ELTAUHFX EQU   *                                                                
         SPACE 1                                                                
         DC    CL2'UH',AL1(TAUHELQ,TAUHLNQ,0,C'G')                              
         DC    AL1((ELTAUHGX-ELTAUHG)/ELLNQ2)                                   
ELTAUHG  DC    CL1'A',CL4'USE',AL1(0,0,0)                                       
         DC    AL2(DWNEUSE-PROCEL,0)                                            
         DC    CL1'B',CL4'TYPE',AL1(0,L'TAUHTYPE,TAUHTYPE-TAUHD)                
         DC    AL2(DWNEUSET-PROCEL,0)                                           
         DC    CL1'C',CL4'CYCS',AL1(0,L'TAUHSTRT,TAUHSTRT-TAUHD)                
         DC    AL2(DWNEDATE-PROCEL,0)                                           
         DC    CL1'D',CL4'CYCE',AL1(0,L'TAUHEND,TAUHEND-TAUHD)                  
         DC    AL2(DWNEDATE-PROCEL,0)                                           
         DC    CL1'M',CL4'CBUN',AL1(0,L'TAUHCBUN,TAUHCBUN-TAUHD)                
         DC    AL2(DWNEDIT-PROCEL,0)                                            
*NOTWANTEDC    CL1'N',CL4'STAT',AL1(0,L'TAUHSTAT,TAUHSTAT-TAUHD)                
*NOTWANTEDC    AL2(DWNEDIT-PROCEL,0)                                            
ELTAUHGX EQU   *                                                                
         SPACE 1                                                                
         DC    CL2'UH',AL1(TAUHELQ,TAUHLNQ,0,C'H')                              
         DC    AL1((ELTAUHHX-ELTAUHH)/ELLNQ2)                                   
ELTAUHH  DC    CL1'A',CL4'USE',AL1(0,0,0)                                       
         DC    AL2(DWNEUSE-PROCEL,0)                                            
         DC    CL1'B',CL4'TYPE',AL1(0,L'TAUHTYPE,TAUHTYPE-TAUHD)                
         DC    AL2(DWNEUSET-PROCEL,0)                                           
         DC    CL1'C',CL4'CYCS',AL1(0,L'TAUHSTRT,TAUHSTRT-TAUHD)                
         DC    AL2(DWNEDATE-PROCEL,0)                                           
         DC    CL1'D',CL4'CYCE',AL1(0,L'TAUHEND,TAUHEND-TAUHD)                  
         DC    AL2(DWNEDATE-PROCEL,0)                                           
ELTAUHHX EQU   *                                                                
         SPACE 1                                                                
         DC    CL2'UH',AL1(TAUHELQ,TAUHLNQ,0,C'I')                              
         DC    AL1((ELTAUHIX-ELTAUHI)/ELLNQ2)                                   
ELTAUHI  DC    CL1'A',CL4'USE',AL1(0,0,0)                                       
         DC    AL2(DWNEUSE-PROCEL,0)                                            
         DC    CL1'B',CL4'TYPE',AL1(0,L'TAUHTYPE,TAUHTYPE-TAUHD)                
         DC    AL2(DWNEUSET-PROCEL,0)                                           
         DC    CL1'C',CL4'CYCS',AL1(0,L'TAUHSTRT,TAUHSTRT-TAUHD)                
         DC    AL2(DWNEDATE-PROCEL,0)                                           
         DC    CL1'D',CL4'CYCE',AL1(0,L'TAUHEND,TAUHEND-TAUHD)                  
         DC    AL2(DWNEDATE-PROCEL,0)                                           
         DC    CL1'P',CL4'UNIT',AL1(0,0,TAUHUNT-TAUHD)                          
         DC    AL2(DWNEZERO-PROCEL,0)                                           
         DC    CL1'Q',CL4'MAJ',AL1(0,L'TAUHMAJ,TAUHMAJ-TAUHD)                   
         DC    AL2(DWNEDIT-PROCEL,0)                                            
         DC    CL1'F',CL4'STAT',AL1(0,L'TAUHSTAT,TAUHSTAT-TAUHD)                
         DC    AL2(DWNEUHST-PROCEL,0)                                           
ELTAUHIX EQU   *                                                                
*                                                                               
         DC    CL2'W4',AL1(TAW4ELQ,TAW4LNQ,0,0)                                 
         DC    AL1((ELTAW4X-ELTAW4)/ELLNQ2)                                     
ELTAW4   DC    CL1'A',CL4'FRST',AL1(0,0,0)                                      
         DC    AL2(DWNESSNF-PROCEL,0)                                           
         DC    CL1'B',CL4'LAST',AL1(0,0,0)                                      
         DC    AL2(DWNESSNL-PROCEL,0)                                           
ELTAW4X  EQU   *                                                                
*                                                                               
         DC    X'FF'               END OF ELEMENT TABLE                         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ROUTINE TRANSLATES VERSION INFORMATION INTO LIFT FORMAT      *         
*        ON ENTRY ... R4=A(CURRENT ELEMENT)                           *         
***********************************************************************         
                                                                                
PROVERS  NTR1  BASE=*,LABEL=*                                                   
         CLI   0(R4),TACAELQ       AND CURRENT ELEMENT IS CAST DETAILS          
         BNE   PVERSX                                                           
         BAS   RE,PROVTACO         PROCESS COMMERCIAL LEVEL VERSIONS            
         BAS   RE,PROVTACA         PROCESS CAST LEVEL VERSIONS                  
PVERSX   XIT1                                                                   
                                                                                
***********************************************************************         
*        ROUTINE TRANSLATES COMMERCIAL-LEVEL VERSION INFORMATION      *         
*        INTO LIFT FORMAT                                             *         
***********************************************************************         
                                                                                
PROVTACO NTR1                                                                   
         MVI   TGVER,0                                                          
                                                                                
         USING TLCAD,R4                                                         
         L     R4,AIO                                                           
         MVC   TGCOM,TLCACOM                                                    
         DROP  R4                                                               
                                                                                
         MVC   SVKEY,KEY                                                        
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A4',0)                                   
         BNE   PVTACO20                                                         
                                                                                
         USING TAVRD,R4                                                         
         L     R4,AIO              R4=A(COMMERCIAL RECORD)                      
         MVI   ELCODE,TAVRELQ      READ VERSION ELEMENTS                        
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
PVTACO10 BRAS  RE,NEXTEL                                                        
         BNE   PVTACO20                                                         
                                                                                
         CLI   TAVRVERS,2          IF ELEMENT FOR VERSION 2                     
         BNE   *+8                 IS FOUND ...                                 
         OI    TGVER,TAVRV2Q       VERSION 2                                    
         CLI   TAVRVERS,3          IF ELEMENT FOR VERSION 3                     
         BNE   PVTACO10            IS FOUND ...                                 
         OI    TGVER,TAVRV3Q       VERSION 3                                    
         B     PVTACO10                                                         
         DROP  R4                                                               
                                                                                
PVTACO20 MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                                                             
         MVC   AIO,AIO1                                                         
         B     PVERSX                                                           
                                                                                
***********************************************************************         
*        ROUTINE TRANSLATES CAST-LEVEL VERSION INFORMATION INTO       *         
*        LIFT FORMAT                                                  *         
***********************************************************************         
                                                                                
PROVTACA CLI   TGVER,0             EXIT IF NOT TREATING COMMERCIAL WITH         
         BE    PVERSX              VERSIONS LIKE COMMERCIAL WITH LIFT           
                                                                                
         MVI   BYTE1,0                                                          
         MVI   BYTE2,0                                                          
                                                                                
         USING TAFND,R4                                                         
         L     R4,AIO              AIO=A(CAST RECORD)                           
         MVI   ELCODE,TAFNELQ      GET CAST'S VERSION ELEMENT                   
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
PVTACA10 BRAS  RE,NEXTEL                                                        
         BNE   PVTACA70                                                         
         CLI   TAFNTYPE,TAFNTVER                                                
         BNE   PVTACA10                                                         
                                                                                
         CLI   TAFNNAME,251        IF CAST IS ON ALL VERSIONS                   
         BNE   *+12                                                             
         OI    BYTE2,TACASALL                                                   
         B     PVTACA70            CAST IS ON MASTER AND LIFT                   
                                                                                
         MVI   TGBYTE,0                                                         
                                                                                
         ZIC   R3,TAFNLEN                                                       
         SHI   R3,3                                                             
         LA    RF,TAFNNAME                                                      
PVTACA30 CLI   0(RF),1                                                          
         BNE   *+12                                                             
         OI    TGBYTE,MASTERQ      ON MASTER                                    
         B     PVTACA40                                                         
         CLI   0(RF),2                                                          
         BNE   *+12                                                             
         OI    TGBYTE,FIRSTQ       ON 1ST LIFT                                  
         B     PVTACA40                                                         
         CLI   0(RF),3                                                          
         BNE   *+8                                                              
         OI    TGBYTE,SECONDQ      ON 2ND LIFT                                  
PVTACA40 AHI   RF,1                                                             
         BCT   R3,PVTACA30                                                      
                                                                                
         TM    TGBYTE,MASTERQ+FIRSTQ+SECONDQ ON MASTER+1ST+2ND LIFT?            
         BNO   *+12                                                             
         MVI   BYTE2,TACASALL      ON MASTER AND ALL LIFTS                      
         B     PVERSX                                                           
                                                                                
         TM    TGBYTE,MASTERQ      ON MASTER?                                   
         BZ    PVTACA50                                                         
         TM    TGBYTE,FIRSTQ       AND 1ST LIFT?                                
         BZ    *+12                                                             
         MVI   BYTE1,TACASTLF      ON MASTER + 1ST LIFT                         
         B     PVERSX                                                           
         TM    TGBYTE,SECONDQ      AND 2ND LIFT?                                
         BZ    *+12                                                             
         MVI   BYTE2,TACAS2LF      ON MASTER + 2ND LIFT                         
         B     PVERSX                                                           
                                                                                
PVTACA50 TM    TGBYTE,FIRSTQ       ON 1ST LIFT?                                 
         BZ    PVTACA60                                                         
         TM    TGBYTE,SECONDQ      AND 2ND LIFT?                                
         BZ    *+12                                                             
         MVI   BYTE2,TACASL2O      ON BOTH                                      
         B     *+8                                                              
         MVI   BYTE1,TACASTLO      ONLY ON 1ST LIFT                             
         B     PVERSX                                                           
                                                                                
PVTACA60 TM    TGBYTE,SECONDQ      ON 2ND LIFT?                                 
         BZ    *+8                                                              
         OI    BYTE2,TACAST2O      ON 2ND LIFT ONLY                             
                                                                                
         USING TACAD,R4                                                         
PVTACA70 L     R4,AIO              GET CAST DETAILS ELEMENT                     
         MVI   ELCODE,TACAELQ                                                   
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         OC    TACASTAT,BYTE1      AND SET LIFT STATUS                          
         OC    TACASTA4,BYTE2      AND SET LIFT STATUS                          
         B     PVERSX                                                           
         DROP  R4                                                               
                                                                                
BYTE1    DS    XL1                                                              
BYTE2    DS    XL1                                                              
                                                                                
MASTERQ  EQU   X'80'               ON MASTER                                    
FIRSTQ   EQU   X'40'               ON 1ST LIFT                                  
SECONDQ  EQU   X'20'               ON 2ND LIFT                                  
                                                                                
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE TACHPACK                                                       
       ++INCLUDE FALINKBLK                                                      
*              DSECT TO COVER KEY HEADER AND FIELDS TABLE                       
         SPACE 1                                                                
KEYTABD  DSECT                                                                  
KEYCODE  DS    CL3                 KEY CODE                                     
KEYCDEQ  DS    XL1                 KEY EQUATE                                   
KEYSTAT  DS    XL1                 STATUS                                       
KEYSL    EQU   X'80'               THIS INVOLVES A LIST                         
KEYSUP   EQU   X'40'               KEY CAN BE UPLOADED                          
KEYCLR   DS    XL2                 DISP. OF RTN TO CLEAR TGD FIELDS             
KEYCMPLN DS    XL1                 LENGTH OF KEY COMPARE ON HIGH                
KEYNFLDS DS    XL1                 NUMBER OF KEY FIELDS ALLOWED                 
KEYLNQ1  EQU   *-KEYTABD           LENGTH OF KEY HEADER INFO                    
KEYFLDS  DS    0C                                                               
KEYFDISP DS    XL2                 DISP. OF TGD FIELD OR FILTDATA               
KEYFLEN  DS    XL1                 L'FIELD                                      
KEYFSTAT DS    XL1                                                              
KEYFLOCL EQU   X'80'               DISPLACEMENT IS FILTDATA                     
KEYFCMP  EQU   X'40'               COMPLEMENT DATA                              
KEYFHEX  EQU   X'20'               HEXIN DATA                                   
KEYFNOCL EQU   X'10'               NO CLIENT REQUIRED FOR PRD INPUT             
KEYFRTN  DS    XL2                 DISP. OF FIELD ROUTINE                       
KEYFVRTN DS    XL2                 DISP. OF VALIDATION ROUTINE                  
KEYLNQ2  EQU   *-KEYFLDS           LENGTH OF EACH KEY FIELD ENTRY               
         SPACE 2                                                                
*              DSECT TO ELEMENT HEADER AND FIELDS TABLE                         
         SPACE 1                                                                
ELTABD   DSECT                                                                  
ELCD     DS    CL2                 ELEMENT CODE                                 
ELCDEQ   DS    XL1                 ELEMENT EQUATE                               
ELLEN    DS    XL1                 ELEMENT LENGTH (BASE)                        
ELSTAT   DS    XL1                 STATUS                                       
ELSFORCP EQU   X'20'               FOR CLI/PROD RECORD                          
ELSFORES EQU   X'10'               FOR ESTIMATE RECORD                          
ELSFRESL EQU   X'08'               FOR ESTIMATE LIST PROCESSING                 
ELSFORTB EQU   X'01'               FOR MISCTAB PROCESSING                       
ELTYPE   DS    XL1                 ELEMENT TYPE                                 
ELNFLDS  DS    XL1                 N'ELEMENT FIELDS                             
ELLNQ1   EQU   *-ELTABD            LENGTH OF ELEMENT HEADER INFO                
ELFLDS   DS    0C                  KEY FIELDS START HERE                        
ELFSCODE DS    CL1                 ELEMENT FIELD SHORT CODE                     
ELFCODE  DS    CL4                 ELEMENT FIELD CODE                           
ELFSTAT  DS    XL1                 ELEMENT STATUS                               
ELFIDWNX EQU   X'80'               INCLUDE IN SPECIAL DOWNLOAD 'X'              
ELFADDLN EQU   X'40'               ADD LENGTH OF FIELD TO ELE. LENGTH           
ELFXDWNX EQU   X'20'               EXCLUDE IN SPECIAL UPLOAD 'X'                
ELFXUP   EQU   X'10'               SKIP ON UPLOAD                               
ELFXUHM  EQU   X'08'               SKIP ON UHM DOWNLOAD                         
ELFLEN   DS    XL1                 MAX LENGTH OF FIELD                          
ELFDISP  DS    XL1                 DISP. OF FIELD INTO ELEMENT                  
ELFDWN   DS    XL2                 DISP. OF FIELD RTN ON DOWNLOAD               
ELFUP    DS    XL2                 DISP. OF FIELD RTN. ON UPLOAD                
ELLNQ2   EQU   *-ELFLDS            LENGTH OF EACH ELEMENT FIELD ENTRY           
         EJECT                                                                  
*              DSECT TO COVER OPTION TABLE                                      
         SPACE 1                                                                
OPTD     DSECT                                                                  
OPTCODE  DS    XL1                 EQUATE IDENTIFYING THIS OPTION               
OPTSUBLN DS    XL1                 L'SUB-ELEMENT DATA                           
OPTSTAT  DS    XL1                 L'SUB-ELEMENT DATA                           
OPFORUSE EQU   X'40'               USE TYPE-RELATED SUB-ELEMENT                 
OPTODSP  DS    AL2                 DISP TO OUTPUT ROUTINE (DWNLD)               
OPTIDSP  DS    AL2                 DISP TO INPUT ROUTINE (UPLD)                 
OPTNEXT  EQU   *                                                                
         SPACE 2                                                                
*              DUMMY ELEMENT FOR COMBINED MUSICIANS RECORD                      
         SPACE 1                                                                
TA01D    DSECT                                                                  
TA01EL   DS    XL1                 ELEMENT CODE                                 
TA01ELQ  EQU   X'01'                                                            
TA01LEN  DS    XL1                 ELEMENT LENGTH                               
TA01CAM  DS    CL3                 CAMERA                                       
TA01UN   DS    CL3                 UNION                                        
TA01CORP DS    CL1                 CORP NUMBER                                  
TA01DBL  DS    CL1                 DOUBLES                                      
TA01OV1  DS    XL4                 FIRST OVERSCALE                              
TA01OV2  DS    XL4                 SECOND OVERSCALE                             
TA01CNT  DS    XL2                 COUNT OF MUSICIANS                           
TA01LNQ  EQU   *-TA01D             ELEMENT LENGTH EQUATE                        
         SPACE 2                                                                
*              DUMMY ELEMENT FOR COMMERCIAL TYPES KEPT IN TABLE                 
         SPACE 1                                                                
TA02D    DSECT                                                                  
TA02EL   DS    XL1                 ELEMENT CODE                                 
TA02ELQ  EQU   X'02'                                                            
TA02LEN  DS    XL1                 ELEMENT LENGTH                               
TA02CTYP DS    CL1                 COMMERCIAL TYPE CODE                         
TA02CTYN DS    CL(L'TGCTNAME-1)    COMMERCIAL TYPE NAME                         
TA02AHLD DS    CL1                 Y=COMM'L GET AUTO HOLDING FEES               
TA02LNQ  EQU   *-TA02D             ELEMENT LENGTH EQUATE                        
         SPACE 2                                                                
*              DUMMY ELEMENT FOR COMMERCIAL TYPES KEPT IN TABLE                 
         SPACE 1                                                                
TA03D    DSECT                                                                  
TA03EL   DS    XL1                 ELEMENT CODE                                 
TA03ELQ  EQU   X'03'                                                            
TA03LEN  DS    XL1                 ELEMENT LENGTH                               
TA03ADST DS    CL2                 ADDENDUM STATE CODE                          
TA03ADEM DS    CL(L'TGTANAME)      ADDENDUM STATE NAME                          
TA03LNQ  EQU   *-TA03D             ELEMENT LENGTH EQUATE                        
         EJECT                                                                  
*              DUMMY ELEMENT FOR CAN. COMMERCIAL TYPES KEPT IN TABLE            
         SPACE 1                                                                
TA04D    DSECT                                                                  
TA04EL   DS    XL1                 ELEMENT CODE                                 
TA04ELQ  EQU   X'04'                                                            
TA04LEN  DS    XL1                 ELEMENT LENGTH                               
TA04CCDE DS    CL4                 CANADIAN COMML TYPE CODE                     
TA04CCN  DS    CL(L'TGCCTNM)       CANADIAN COMML NAME                          
TA04LNQ  EQU   *-TA04D             ELEMENT LENGTH EQUATE                        
         SPACE 2                                                                
*              DUMMY ELEMENT FOR ESTIMATE RECORD IN ESL DOWNLOAD                
         SPACE 1                                                                
TA05D    DSECT                                                                  
TA05EL   DS    XL1                 ELEMENT CODE                                 
TA05ELQ  EQU   X'05'                                                            
TA05LEN  DS    XL1                 ELEMENT LENGTH                               
TA05ID   DS    CL20                ESTIMATE ID                                  
TA05NAME DS    CL36                ESTIMATE NAME                                
TA05RDTE DS    XL3                 REPORT DATE                                  
TA05CDTE DS    XL3                 LAST CHANGED DATE                            
TA05LNQ  EQU   *-TA05D             ELEMENT LENGTH EQUATE                        
         SPACE 2                                                                
*              DSECT TO SCREEN LINE                                             
         SPACE 1                                                                
LINED    DSECT                                                                  
LINLABEL DS    CL4                 ELEMENT CODE                                 
LINEQU   DS    CL1                                                              
LINDATA  DS    CL74                                                             
LINLNQ   EQU   *-LINED                                                          
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         SPACE                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRDFD                                                       
         ORG   STEWORK                                                          
         EJECT                                                                  
*              LOCAL SAVED STORAGE AT BOTTOM OF TWA0                            
         SPACE 1                                                                
WORKD    EQU   *                                                                
*                                                                               
STEKEYLQ EQU   63                  LENGTH OF KEY FIELD                          
SVDSPLNQ EQU   9000                DISPLACEMENT INTO TIA/TWA4 THAT              
*                                  SCREEN DATA IS SAVED IF ELEMENT IS           
*                                  SPLIT BETWEEN TWO UPLOAD SCREENS             
*                                                                               
MYKCODE  DS    XL1                 KEY EQUATE (MAIN READ)                       
*                                                                               
SMYKEY   DS    CL(L'TLDRKEY)       SAVED KEY FOR CONTINUATION                   
FRSTKEY  DS    CL(L'TLDRKEY)       FIRST KEY                                    
FRSTKEYA DS    CL(L'TLDRKEY)       FIRST KEY OF SECOND READ                     
SVLSTINF DS    0XL12               SAVED RECORD INFO FOR LISTING                
SVLSTUH  DS    0XL12               TLUHD INFO                                   
SVUHUSE  DS    CL3                 USE                                          
SVUHCYC  DS    CL6                 CYCLE DATES                                  
SVUHCYCS DS    0XL3                                                             
SVUHCYCE DS    0XL3                                                             
         DS    CL3                 SPARE                                        
*                                                                               
FILTDATA DS    0CL(FILTLNQ)        FILTER RECORD ELEMENTS                       
FILTKEYQ DS    XL1                 CODE OR NAME                                 
FILTAGY  DS    CL6                 AGENCY                                       
FILTCLI  DS    CL6                 CLIENT                                       
FILTPRD  DS    CL6                 PRODUCT                                      
FILTMED  DS    CL1                 MEDIA                                        
FILTCLG  DS    CL6                 CLIENT GROUP                                 
FILTSTRT DS    CL20                START CODE OR NAME                           
FILTREL  DS    CL1                 SHOW RELEASED COMMERCIALS                    
FILTLOCK DS    CL1                 SHOW LOCKED COMMERCIALS                      
FILTPDTE DS    XL3                 COMML ACTIVITY DATE                          
FILTRDTE DS    XL3                 ESTIMATE REPORT DATE                         
         DS    XL1                 - MEANS WANT <= REPORT DATE                  
FILTCDTE DS    XL3                 ESTIMATE CHANGED DATE                        
         DS    XL1                 - MEANS WANT <= CHANGED DATE                 
FILTMUS  DS    CL1                 EXCLUDE MUSIC COMMERCIALS                    
FILTLNQ  EQU   *-FILTKEYQ                                                       
*                                                                               
STOPT    DS    XL1                 STATUS OPTIONS                               
STEXPAND EQU   X'80'               EXPAND ELMEENTS                              
STEXTRA  EQU   X'40'               PROCESS EXTRA ELE. ENTRIES ON DWNLD          
STREST   EQU   X'10'               RESTORE ESTIMATE RECORD                      
*                                                                               
STESTAT  DS    XL1                 STATUS OPTIONS (CLR'D FIRST FOR KEY)         
STERERD  EQU   X'40'               RE-ESTABLISH DIRECTORY PTR                   
STETAB   EQU   X'20'               PROCESSING TABLE                             
STEWTWA  EQU   X'10'               TWA 4 WRITTEN PREVIOUSLY                     
*                                                                               
STESTAT2 DS    XL1                 STATUS OPTIONS (INDIVID. CLEARED)            
STECOM   EQU   X'80'               NEW COMML ON UHM DOWNLOAD                    
STEUHMR  EQU   X'40'               READING IN COMMLS (MULT. HITS ENTER)         
*                                                                               
TAESCNTS DS    0X                  SEQUENCE NUMBERS & COUNTS FOR TAESD          
EMSEQ    DS    XL1                 FOR TAEMD                                    
ENSEQ    DS    XL1                 FOR TAEND                                    
HYPCNTS  DS    0XL2                                                             
HPGCNT   DS    XL1                 PAGE NUMBER HYPO PERF DISPLAYED ON           
HYPCNT   DS    XL1                 COUNT OF HYPO PERFORMERS PER PAGE            
MAXHYP   EQU   7                   MAX NUM OF HYP PERFS PER PAGE                
*                                                                               
ESSEQS   DS    0XL4                SEQUENCE NUMBERS FOR ALL TAESD ELS           
SVCLISEQ DS    XL1                                                              
SVPRDSEQ DS    XL1                                                              
SVCOSEQ  DS    XL1                                                              
SVCASEQ  DS    XL1                                                              
ESSEQSX  EQU   *                                                                
*                                                                               
SVCODES  DS    0XL25               SAVED CODES FOR COL/CAL/UHM                  
SVMED    DS    CL1                 OPTIMIZATION                                 
SVCLI    DS    CL6                                                              
SVPRD    DS    CL6                                                              
SVCAT    DS    CL3                                                              
SVCAM    DS    CL3                                                              
SVUNI    DS    CL3                                                              
SVYEAR   DS    CL3                                                              
SVCODESX EQU   *                                                                
*                                                                               
FRSTAGY  DS    CL6                 FIRST LIMITED AGENCY READ                    
LASTAGY  DS    CL6                 LAST LIMITED AGENCY READ                     
*                                                                               
MISCTAB  DS    CL(MAXTBL*TA01LNQ+1) LARGEST POSSIBLE ELE IN TABLE               
MAXTBL   EQU   50                                                               
*                                                                               
TAVRV2Q  EQU   X'80'               VERSION 2                                    
TAVRV3Q  EQU   X'40'               VERSION 3                                    
*                                                                               
         DS    0X                  MAKE SURE DON'T BLOW PAST E00                
WORKLNQ  EQU   *-WORKD                                                          
         EJECT                                                                  
*              DSECT TO COVER LOCAL SAVED STORAGE                               
         SPACE 2                                                                
STEREOD  DSECT                                                                  
*                                                                               
RELO     DS    A                   RELOCATION FACTOR                            
*                                                                               
ARTNS    DS    0A                  RELO'D RTNS (SET EACH HIT OF ENTER)          
AGETEREC DS    A                                                                
AFILTREC DS    A                                                                
APRESCID DS    A                                                                
APROCEL  DS    A                                                                
AUPEFLD  DS    A                                                                
AFMTEL   DS    A                                                                
ASETELF  DS    A                                                                
ASETETYP DS    A                                                                
AGETNTRY DS    A                                                                
AADDTAB  DS    A                                                                
AMYADDL  DS    A                                                                
AWRITEIT DS    A                                                                
ADELETE  DS    A                                                                
AOPTELS  DS    A                                                                
AOPTTAB  DS    A                                                                
AKEYTAB  DS    A                                                                
AELTAB   DS    A                                                                
*                                                                               
AELENTRY DS    A                   A(CURRENT ELEMENT ENTRY IN TABLE)            
ASCRNEL  DS    A                   A(AREA TO FORMAT SCREEN ELEMENT)             
ASCRPOS  DS    A                   A(CURRENT SCREEN POSITION)                   
ASCRNXT  DS    A                   A(NEXT POSITION IN SCRNEL)                   
SVAIO    DS    A                   A(SAVED IOAREA)                              
*                                                                               
KEYDSP   DS    H                   DISP. INTO AKEYNTRY FOR CURRENT NTRY         
RECDSP   DS    H                   DISP. TO CURR LOCATION IN RECORD             
ELSCRLEN DS    H                   TOTAL DISPLAY ELEMENT SIZE                   
SCRLLEN  DS    H                   CURRENT LENGTH OF SCREEN LINE LEFT           
SCRELDSP DS    H                   CURRENT DISPLACMENT INTO SCRNEL              
SCRELLEN DS    H                   LENGTH OF DATA IN SCRNEL                     
MISC2DSP DS    H                   DISPLACEMENT INTO MISCTAB2                   
PARTCOM  DS    CL8                 PARTIAL SCREEN INTERNAL COMML NUMBER         
SVTIM    DS    XL3                 TIME OF DOWNLOAD (HMS)                       
SVKEY    DS    XL(L'KEY)           SAVED CAST KEY                               
*                                                                               
CONTFLAG DS    CL1                 CONTINUATION FLAG                            
OVRFLAG  DS    CL1                 START OVER FLAG                              
CURLINE  DS    XL1                 CURRENT LINE NUMBER                          
NLINES   EQU   20                  NUMBER OF LINES ON SCREEN                    
MYKLEN   DS    XL1                 CURRENT KEY COMPARE LENGTH                   
ELETYPE  DS    CL1                 ELEMENT TYPE FOR TRAVERSING ELTAB            
ELCDEQU  DS    XL1                 ELEMENT CODE                                 
ELMINLEN DS    XL1                 MINIMUM ELEMENT LENGTH                       
KEYCHG   DS    CL1                 Y=KEY CHANGE                                 
SVELCODE DS    XL1                 SAVED ELEMENT CODE                           
ACTIME   DS    CL8                 ACTIVITY TIME                                
HAVEAC40 DS    CL1                 HAVE 40 ACTIVITY ELEMENT                     
ANYMORE  DS    CL1                 Y=MORE ELEMENTS TO COME                      
*                                                                               
DUMHDR   DS    CL8                 DUMMY FIELD HEADER                           
DUMFLD   DS    CL12                                                             
*                                                                               
ELFAREA  DS    0CL260                                                           
ELFLABEL DS    CL4                 ELEMENT FIELD LABEL                          
ELFEQU   DS    XL1                 (ON DWNLD =, ON UPLD L'ELFDATA)              
ELFDATA  DS    CL255               ELEMENT FIELD DATA                           
*                                                                               
*                                  USED FOR UH D/A NEED 200*4 AND               
*                                  USED FOR CO INT COMML NEED 300*4             
MISCTAB2 DS    CL(MAXMISC2*4)                                                   
MAXMISC2 EQU   300                                                              
*                                                                               
SCRNELQ  EQU   6000                L'OF NMOD'D AREA FOR ELEMENT IN              
*                                  SCREEN FORMAT                                
*              ALL ADDITIONS MUST BE BEFORE THIS POINT                          
MYSPARE  DS    CL(L'TWAHOLE-(*-STEREOD))                                        
         DS    0X                                                               
         SPACE 2                                                                
         EJECT                                                                  
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* TASYSEQUS                                                                     
* TASYSDSECT                                                                    
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDPERVALD                                                                     
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'108TAGENDF   06/27/13'                                      
         END                                                                    
