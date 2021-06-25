*          DATA SET CTSFM14    AT LEVEL 138 AS OF 02/28/18                      
*PHASE TA0A14A                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE: TA0A14 - DARE AGENCY ROUTING MAINT/LIST                     *         
*                                                                     *         
*  COMMENTS: MAINTAINS DARE AGENCY ROUTING RECORDS                    *         
*                                                                     *         
*  CALLED FROM: SFM CONTROLLER (TA0A00), WHICH CALLS                  *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  CALLS TO:    DATAMGR                                               *         
*                                                                     *         
*  INPUTS: SCREENS CTSFMBA (TA0ABA) -- MAINTENANCE                    *         
*                  CTSFMBB (TA0ABB) -- LIST                           *         
*                                                                     *         
*  OUTPUTS: UPDATED DARE AGENCY ROUTING RECORDS                       *         
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
         TITLE 'TA0A14 DARE AGENCY ROUTING RECORDS'                             
TA0A14   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*TA0A14*                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         XC    DMCB(24),DMCB       CLEAR 1ST PARAMETER OF DMCB                  
         MVC   DMCB+4(4),=X'D9000A1F' GET ADDR OF DDDAREREPS VIA CALLOV         
         GOTO1 CALLOV,DMCB         RETURN WITH ADDR IN 1ST PARAM.               
         CLI   DMCB+4,X'FF'        COULDN'T GET ADDRESS?                        
         BNE   *+6                 NO, IT'S IN 1ST PARAMETER                    
         DC    H'0'                YES, DIE HORRIBLY                            
         MVC   VREPIDS(4),DMCB     SAVE THAT ADDRESS FOR LATER                  
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
*        CLI   MODE,PRINTREP                                                    
*        BE    PR                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
VK       DS    0H                                                               
*                                                                               
         CLI   ACTNUM,ACTLIST      SKIP TESTS IF LIST                           
         BNE   VK20                                                             
         OC    SFLMEDA,SFLMEDA                                                  
         BNZ   VK10                                                             
         LA    R2,SFMMEDAH                                                      
VKMEDERR MVC   GERROR,=AL2(300)                                                 
         B     SFMERROR                                                         
*                                                                               
VK10     OC    SFLFILT,SFLFILT     ANY FILTER?                                  
         BZ    VKBKEY                                                           
         CLC   =C'REP=',SFLFILT    ONLY REP=  FILTER IS ALLOWED                 
         BE    VKBKEY                                                           
         LA    R2,SFLFILTH                                                      
INVLFLD  MVC   GERROR,=AL2(INVALID)                                             
         J     SFMERROR                                                         
*                                                                               
VK20     DS    0H                                                               
         LA    R2,SFMMEDAH                                                      
         CLI   ACTNUM,ACTCHA       SELECT CHANGED TO CHANGE ?                   
         BE    VK30_K                                                           
         TM    4(R2),X'20'                                                      
         BZ    VK30                                                             
         TM    SFMTREPH+4,X'20'                                                 
         BNZ   VK50                                                             
*                                                                               
VK30     TWAXC SFMREP1H,SFMLOVLH,PROT=Y                                         
*                                                                               
VK30_K   CLI   5(R2),1             IS THE MEDIA 1 CHAR?                         
         BNE   VKMEDERR                                                         
         CLI   8(R2),C'T'          IS THE MEDIA T?                              
         BE    VK40                                                             
         CLI   8(R2),C'R'          IS THE MEDIA R?                              
         BNE   VKMEDERR                                                         
VK40     OI    4(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         OI    SFMTREPH+4,X'20'                                                 
         OI    SFMTREPH+6,X'80'                                                 
         MVC   SFMRLBL(4),=C'Live'                                              
         CLI   SFMTREP,C'Y'                                                     
         BNE   *+10                                                             
         MVC   SFMRLBL(4),=C'Test'                                              
         OI    SFMRLBLH+6,X'80'                                                 
*                                                                               
         BRAS  RE,DISPREPS                                                      
*                                                                               
VK50     LA    R2,SFMAGRTH                                                      
*                                                                               
* BUILD THE KEY FOR MAINT...                                                    
*                                                                               
VKBKEY   XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING AGRKEYD,R4                                                       
         MVI   AGRKSYS,AGRKSYSQ    BUILD KEY FOR MAINT...                       
         MVI   AGRKTYP,AGRKTYPQ                                                 
         MVC   AGRKMEDA,SFMMEDA                                                 
         MVC   AGRKAGRT,SFMAGRT                                                 
         CLI   ACTNUM,ACTLIST                                                   
         BNE   EXIT                                                             
*                                                                               
         MVC   AGRKMEDA,SFLMEDA    ...OR FOR LIST                               
         OC    SFLAGRT,SFLAGRT                                                  
         BZ    EXIT                                                             
         MVC   AGRKAGRT,SFLAGRT                                                 
         B     EXIT                                                             
         EJECT                                                                  
         DROP  R4                                                               
***********************************************************************         
* VALIDATE THE RECORD                                                           
***********************************************************************         
VR       DS    0H                                                               
************                                                                    
* DEL FROM REC THE AGY OFFICE OVERRIDE ELEMENTS DISPLAYED ON SCREEN             
************                                                                    
         L     R3,AIO                                                           
         USING AGROVRD,R3                                                       
*                                                                               
         MVI   ELCODE,AGROVRCQ     X'10' - ANY AGY RT ELEMT?                    
         BRAS  RE,GETEL                                                         
         BNE   VR20                                                             
VR00     LA    R5,SFMREPCH                                                      
*                                                                               
VR10LOOP LA    RE,SFMLRCLH                                                      
         CR    R5,RE               AT END OF SCREEN LIST?                       
         BH    VR10NXEL             YES                                         
         CLI   8(R5),C'A'          END OF DISPLAYED REP LIST?                   
         BL    VR10NXEL             YES                                         
*                                                                               
         CLC   AGROVRCR,8(R5)      MATCH ON THIS REP AND LIVE/TEST?             
         BE    VR14                                                             
VR12     LA    R5,SFMREP2H-SFMREP1H(R5) NO, CHECK NEXT REP                      
         B     VR10LOOP                                                         
*                                                                               
VR14     XC    WORK,WORK                                                        
         MVC   WORK(L'AGROVRCR+L'AGROVROF),AGROVRCR                             
         GOTO1 HELLO,DMCB,(C'D',SYSFIL),(ELCODE,AIO),                  X        
               (L'AGROVRCR+L'AGROVROF,WORK),0                                   
*                                                                               
         CLI   0(R3),AGROVRCQ      DELETE AN ELEM, THEN R3 IS NEXT ELEM         
         BE    VR00                                                             
         B     VR15                                                             
*                                                                               
VR10NXEL DS    0H     READ EDI-PARTNER TO SEE IF I NEED TO DELETE               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTEPRECD,R4                                                      
         MVI   CTEPKSTY,CTEPKSTQ   X'0039' - TYPE AND SUBTYPE                   
         MVC   CTEPKREP,AGROVRCR                                                
         GOTO1 HIGH                READ FOR THE REQUESTED REP                   
         JNE   *+2                                                              
         CLC   KEY(L'CTEPKEY),KEYSAVE                                           
         JNE   VR14                                                             
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    VR00                                                             
         DROP  R3                                                               
*                                                                               
VR15     L     R4,AIO              RESTORE DAGYROUTE                            
         USING AGRKEYD,R4                                                       
         MVC   KEY(L'AGRKEY),0(R4)                                              
         GOTO1 HIGH                                                             
         JNE   *+2                                                              
         MVC   AIO,AIO3            DON'T BLOW AWAY OUR RECORD                   
         GOTO1 GETREC                                                           
         JNE   *+2                                                              
         MVC   AIO,AIO1                                                         
*                                                                               
************                                                                    
* ADD TO REC THE AGY OFFICE OVERRIDE ELEMENTS DISPLAYED ON SCREEN               
************                                                                    
VR20     L     R4,AIO                                                           
         USING AGRKEYD,R4                                                       
         XC    ELEM,ELEM                                                        
         LA    R3,ELEM                                                          
         USING AGROVRD,R3          AGY RTING  ELEMENT DSECT                     
         MVI   AGROVRC,AGROVRCQ                                                 
         MVI   AGROVRLN,AGROVRLQ                                                
*                                                                               
         LA    R2,SFMOVR1H         R2 = A(REGULAR REPS FLDHDR)                  
VR40     CLI   SFMREPCH+8-SFMOVR1H(R2),C'A' ANY MORE ENTRIES?                   
         BL    VR100                        NO                                  
         LA    R0,SFMLRPLH         ...CHECK FOR END OF SCREEN                   
         CR    R2,R0                                                            
         BH    VR100                                                            
*                                                                               
VR55     CLI   8(R2),C'A'          IF NO OVERRIDE                               
         BNL   VR60                                                             
         LA    R2,SFMOVR2H-SFMOVR1H(R2)                                         
         B     VR40                THEN THERE SHLDN'T BE AN ELEMENT             
*                                                                               
VR60     CLI   5(R2),2                                                          
         BNE   VRERR                                                            
         CLC   SFMAGRT+3(2),8(R2)  OVRIDE & RTING CODE SHLD BE DIFF             
         BE    VRERR                                                            
*                                                                               
         CLI   8(R2),C'A'          ONLY ALLOW ALPHA                             
         BL    VRERR                                                            
         CLI   8(R2),C'Z'                                                       
         BH    VRERR                                                            
         CLI   9(R2),C'A'                                                       
         BL    VRERR                                                            
         CLI   9(R2),C'Z'                                                       
         BH    VRERR                                                            
*                                                                               
         MVC   AGROVRCR,SFMREPC-SFMOVR1H(R2)                                    
         MVC   AGROVROF,8(R2)      BUILD OFFICE OVERRIDE OF ELEMENT             
         B     VR70                                                             
*                                                                               
VRERR    MVC   GERROR,=AL2(305)                                                 
         B     SFMERROR                                                         
*                                                                               
VR70     GOTO1 ADDELEM                                                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR80     LA    R2,SFMOVR2H-SFMOVR1H(R2)   PT R2 TO NEXT FIELD                   
         B     VR40                                                             
*                                                                               
VR100    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE RECORD                                                            
***********************************************************************         
DR       DS    0H                                                               
         L     R4,AIO              CHECK KEY SAME AS RECORD                     
         USING AGRKEYD,R4                                                       
         TWAXC SFMREP1H                                                         
*                                                                               
         CLC   KEY(AGRKLENQ),AGRKEY                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO                                                           
         USING AGROVRD,R3                                                       
         MVI   ELCODE,AGROVRCQ                                                  
         BAS   RE,GETEL                                                         
         BNE   EXIT                GET OUT IF NO MORE ELEMENTS                  
*                                                                               
DR5      LA    R2,SFMREP1H                                                      
DR10     LA    RF,SFMREPCH-SFMREP1H(R2)  RF=A(3CHAR REP FLDHDR)                 
         CLC   AGROVRCR,8(RF)            MATCH ON 3-CHAR REP CODE?              
         BE    DR40                                                             
*                                                                               
         LA    R2,SFMREP2H-SFMREP1H(R2)                                         
         LA    R0,SFMLRPLH         ...CHECK FOR END OF SCREEN                   
         CR    R2,R0                                                            
         BH    DR100               DIE - HAVE ENTRY NOT ON SCREEN               
*                                                                               
         CLI   8(R2),C' '          ANY MORE ENTRIES?                            
         BH    DR10                                                             
         B     DR100               DIE - HAVE ENTRY NOT ON SCREEN               
*                                                                               
DR40     LA    R2,SFMOVR1H-SFMREP1H(R2)                                         
         MVC   8(2,R2),AGROVROF                                                 
         MVI   5(R2),2             SO CHANGE WILL WORK                          
         OI    6(R2),X'80'                                                      
*                                                                               
DR100    BAS   RE,NEXTEL                                                        
         BE    DR5                                                              
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*   LIST RECORDS                                                                
***********************************************************************         
LR       DS    0H                                                               
         OI    SFLREPOH+6,X'80'                                                 
         OI    SFLREPOH+1,X'0C'                                                 
         OC    SFLFILT,SFLFILT                                                  
         BZ    *+8                                                              
         NI    SFLREPOH+1,X'FF'-X'04'  MAKE HIGH INTENSITY                      
*                                                                               
         LA    R4,KEY                                                           
         USING AGRKEYD,R4                                                       
*                                                                               
         OC    KEY(L'AGRKSYS+L'AGRKTYP),KEY                                     
         BNZ   LR10                                                             
*                                                                               
         MVI   AGRKSYS,AGRKSYSQ    BUILD SEARCH KEY USE INPUT                   
         MVI   AGRKTYP,AGRKTYPQ                                                 
         LA    R2,SFLMEDAH                                                      
         CLI   SFLMEDAH+5,1        IS THE MEDIA 1 CHAR?                         
         BNE   VKMEDERR                                                         
         MVC   AGRKMEDA,SFLMEDA    BUILD SEARCH KEY                             
         OC    SFLAGRT,SFLAGRT                                                  
         BZ    *+10                                                             
         MVC   AGRKAGRT,SFLAGRT                                                 
         MVC   SAVEKEY,KEY                                                      
*                                                                               
LR10     GOTO1 HIGH                                                             
*                                                                               
LR20     CLC   KEY(AGRKAGRT-AGRKEYD),SAVEKEY                                    
         BNE   EXIT                                                             
*                                                                               
         XC    LISTAR,LISTAR                                                    
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         OC    SFLFILT,SFLFILT                                                  
         BZ    LR30                                                             
         LA    R1,AGRFSTEL         1ST ELEMENT                                  
LR22     CLI   0(R1),0             EOR?                                         
         BE    LR50                                                             
         CLI   0(R1),AGROVRCQ                                                   
         BE    LR26                                                             
LR24     LLC   R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     LR22                                                             
*                                                                               
         USING AGROVRD,R1                                                       
LR26     CLC   AGROVRCR,SFLFILT+4                                               
         BNE   LR24                                                             
         MVC   LSTREPO,AGROVROF                                                 
         DROP  R1                                                               
*                                                                               
LR30     MVC   LSTMEDIA,AGRKMEDA                                                
         MVC   LSTAGYRT,AGRKAGRT                                                
         GOTO1 LISTMON             DISPLAY RECORD                               
*                                                                               
LR50     GOTO1 SEQ                 GET NEXT RECORD AND CHECK FOR MATCH          
         LA    R4,KEY                                                           
         B     LR20                                                             
         EJECT                                                                  
***********************************************************************         
* DISPLAY KEY / IN SELECT                                                       
***********************************************************************         
DK       DS    0H                                                               
         BRAS  RE,DISPREPS                                                      
*                                                                               
         L     R4,AIO                                                           
         USING AGRKEYD,R4                                                       
         MVC   KEY(L'AGRKEY),0(R4)                                              
         GOTO1 HIGH                                                             
         JNE   *+2                                                              
         GOTO1 GETREC                                                           
         JNE   *+2                                                              
*                                                                               
         MVC   SFMMEDA,AGRKMEDA    RECORD TO SCREEN                             
         OI    SFMMEDAH+4,X'20'                                                 
         OI    SFMMEDAH+6,X'80'                                                 
         MVC   SFMAGRT,AGRKAGRT                                                 
         OI    SFMAGRTH+6,X'80'                                                 
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
***********************************************************************         
* DISPLAY THE REPS ON THE SCREEN                                                
***********************************************************************         
DISPREPS NTR1                                                                   
         MVC   AIO,AIO2                                                         
*                                                                               
         LA    R2,SFMREP1H                                                      
DSRP010  XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CTEPRECD,R4                                                      
         MVI   CTEPKSTY,CTEPKSTQ   X'0039' - TYPE AND SUBTYPE                   
         GOTO1 HIGH                READ FOR THE REQUESTED REP                   
         JNE   *+2                                                              
*                                                                               
DSRP020  CLI   CTEPKTYP,CTEPKTYQ   X'00' - TYPE AND                             
         BNE   DRPPROT                                                          
         CLI   CTEPKSTY,CTEPKSTQ   X'39' - SUBTYPE                              
         BNE   DRPPROT                                                          
         CLI   CTEPKRCT,0          ONLY WANT REPS                               
         BH    DRPPROT                                                          
         CLC   CTEPKREP,=C'NOR'    IF 'NOR' REP THEN..                          
         BE    DRPSEQ                 SKIP IT, DON'T DISPLAY                    
         CLC   CTEPKREP,=C'NON'    IF 'NON' REP THEN..                          
         BE    DRPSEQ                 SKIP IT, DON'T DISPLAY                    
*                                                                               
         GOTO1 GETREC                                                           
         JNE   *+2                                                              
*                                                                               
         L     R3,AIO                                                           
         MVI   ELCODE,CTEPRELQ     X'02' - PRIMARY INFO ELEMENT                 
         BRAS  RE,GETEL                                                         
         JNE   *+2                                                              
         USING CTEPRD,R3                                                        
*                                                                               
DSRP030  CLI   SFMTREP,C'Y'        ONLY WANT TEST REP?                          
         BE    DSRP040                                                          
         TM    CTEPFLG1,CTE1TSTR   NO, IS THIS A TEST REP?                      
         BNZ   DRPSEQ                  YES, SKIP TO NEXT REP IN TABLE           
         B     DSRP050                                                          
*                                                                               
DSRP040  TM    CTEPFLG1,CTE1TSTR   YES, IS THIS A TEST REP?                     
         BZ    DRPSEQ              NO                                           
*                                                                               
DSRP050  CLC   SFMMEDA,CTEPRMED    MATCH ON MEDIA?                              
         BNE   DRPSEQ               ONLY OTHER MEDIA IS RADIO FOR NOW           
*                                                                               
         CLI   SFMMEDA,C'R'        FOR RADIO                                    
         BNE   DSRP070                                                          
         TM    CTEPFLG1,CTE1XMLR   X'08' - XML REP (REDI)?                      
         BO    DRPSEQ              YES, SKIP IT, REDI NOT SUPPORTED             
*&&DO                                                                           
         LA    RF,REDIREPS         POINT TO LIST OF REDI VENDOR                 
DSRP060  CLC   CTEPVNAM,0(RF)      IS THIS REP A REDI VENDOR?                   
         BE    DRPSEQ               YES, SKIP IT, REDI NOT SUPPORTED            
         AHI   RF,L'REDIREPS                                                    
         CLI   0(RF),X'FF'         E-O-L?                                       
         BNE   DSRP060              NO, CHECK IT                                
*&&                                                                             
*                                                                               
DSRP070  MVC   8(L'CTEPPNAM,R2),CTEPPNAM                                        
         OI    6(R2),X'80'                                                      
         NI    SFMOVR1H+1-SFMREP1H(R2),X'FF'-X'20'                              
         OI    SFMOVR1H+6-SFMREP1H(R2),X'80'                                    
*                                                                               
         MVC   SFMREPC-SFMREP1H(L'SFMREPC,R2),CTEPKREP                          
         NI    SFMREPCH+6-SFMREP1H(R2),X'80'       TRASNMIT FIELD               
         NI    SFMREPCH+1-SFMREP1H(R2),X'FF'-X'04' REMOVE LOW INTENSITY         
         ORG   *-4                                                              
         DC    X'4700'                                                          
         ORG                                                                    
*                                                                               
         LA    R2,SFMREP2H-SFMREP1H(R2)                                         
         LA    RF,SFMLRPLH                                                      
         CR    R2,RF                                                            
         BH    DRPPROT                                                          
*                                                                               
DRPSEQ   GOTO1 SEQ                                                              
         BE    DSRP020                                                          
         DC    H'0'                                                             
         DROP  R3,R4                                                            
*                                                                               
* PROTECT THE REST OF THE ENTRIES                                               
*                                                                               
DRPPROT  LA    R2,SFMOVR1H-SFMREP1H(R2)                                         
         LA    R6,SFMLOVLH                                                      
DRPPRO10 CR    R2,R6               PROTECT THE REST OF THE ENTRIES              
         BH    DRPX                                                             
         OI    1(R2),X'20'                                                      
         OI    6(R2),X'80'                                                      
         LA    R2,SFMOVR2H-SFMOVR1H(R2)                                         
         B     DRPPRO10                                                         
DRPX     MVC   AIO,AIO1                                                         
         B     EXIT                                                             
SFMERROR GOTO1 SFMERR                                                           
*                                                                               
RELO     DS    A                                                                
*&&DO                                                                           
REDIREPS DC    0CL10                                                            
         DC    CL10'KATZRADIO'                                                  
         DC    CL10'MFS'                                                        
         DC    CL10'REGIONAL'                                                   
         DC    CL10'NPM'                                                        
         DC    X'FF'                                                            
*&&                                                                             
INVALCMB DC    CL60'INVALID: OVERRIDE SAME AS ROUTING CODE'                     
*                                                                               
         GETEL  R3,DATADISP,ELCODE                                              
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE CTGENAGRD                                                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTGENDEPT                                                      
       ++INCLUDE GEGENEDI                                                       
         EJECT                                                                  
         PRINT ON                                                               
       ++INCLUDE CTSFMFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMBAD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE CTSFMBBD                                                       
*                                                                               
*                                                                               
         EJECT                                                                  
       ++INCLUDE CTSFMWORKD                                                     
         SPACE 5                                                                
         EJECT                                                                  
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
*                                                                               
         ORG   SYSSPARE                                                         
VREPIDS  DS    V                                                                
LREPIDS  EQU   25    <-- SEE DDDARETAB                                          
*                                                                               
SAVEKEY  DS    CL32                                                             
TEMP     DS    CL10                                                             
*                                                                               
         EJECT                                                                  
* ONLINE LIST (1 LINE)                                                          
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
         DS    CL3                                                              
LSTMEDIA DS    CL1                                                              
         DS    CL9                                                              
LSTAGYRT DS    CL5                                                              
         DS    CL20                                                             
LSTREPO  DS    CL2                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'138CTSFM14   02/28/18'                                      
         END                                                                    
