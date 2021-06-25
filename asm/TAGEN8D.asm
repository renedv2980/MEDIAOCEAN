*          DATA SET TAGEN8D    AT LEVEL 102 AS OF 03/20/15                      
*PHASE T7028DA,*                                                                
         TITLE 'T7028D - ADVICE LIST/RECEIVE'                                   
T7028D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 STOREEQU,T7028D,R6,CLEAR=YES                                     
         LR    R3,RC                                                            
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LA    R7,TWAHOLE          R7=A(LOCAL W/S)                              
         USING DVLISTD,R7                                                       
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         LHI   RF,3520                                                          
         LH    RE,=AL2(TIEND-T702FFD)                                           
         SR    RF,RE                                                            
         XCEFL TIEND,(RF)                                                       
         SPACE 1                                                                
         OI    FIX1,KEYTYXCM                                                    
         OI    FIX2,KEYTYXCM                                                    
         SPACE 1                                                                
         XR    R2,R2                                                            
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+8                                                              
         LA    R2,PFTAB                                                         
         GOTO1 INITIAL,DMCB,(R2)                                                
         ST    R3,ADCREC           A(ADVICE CAST RECORD)                        
         SPACE 1                                                                
         CLI   MODE,VALKEY         VALIDATE KEY FIELDS                          
         BNE   ADV20                                                            
         CLI   ACTNUM,ACTLIST                                                   
         BNE   *+12                                                             
         BAS   RE,LVKEY            LIST SCREEN                                  
         B     XIT                                                              
         BAS   RE,RVKEY            RECEIVE SCREEN                               
         B     XIT                                                              
         SPACE 1                                                                
ADV20    CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   ADV30                                                            
         LA    R2,LISTAR           R2=A(OUTPUT AREA)                            
         B     ADV40                                                            
         SPACE 1                                                                
ADV30    CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   XIT                                                              
         MVI   OVERLAY,X'8E'       LOAD PRINTING OVERLAY                        
         GOTO1 LOADSOPH,DMCB,0                                                  
         ST    R3,ADVPRNT          RETURNS R3=A(PHASE)                          
         MVI   MYAGY,X'FF'                                                      
         BAS   RE,INIT             INSURE START AT TOP OF LIST                  
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         LA    R2,MYSPECS          SET A(SPECS)                                 
         ST    R2,SPECS                                                         
         LA    R2,P+1              R2=A(OUTPUT AREA)                            
         SPACE 1                                                                
ADV40    BAS   RE,LREC             GO LIST THE RECORDS                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY FIELDS (LIST)                            
         SPACE 1                                                                
LVKEY    NTR1                                                                   
         GOTO1 FLDVAL,DMCB,(X'40',SDVAGYH),(X'80',SDVKEYXH)                     
         BE    LVK2                IF SOME FIELD CHANGED                        
         LH    RF,=AL2(TIEND-TASYSIOD)                                          
         XCEFL TASYSIOD,(RF)       CLEAR SYSIO &                                
         MVI   TGAYSTA7,0                                                       
         NI    SDVAGYH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         SPACE 1                                                                
LVK2     LA    R2,SDVAGYH          AGENCY                                       
         TM    4(R2),X'20'         IF FIELD CHANGED                             
         BO    LVK4                                                             
         NI    SDVCLIH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         BAS   RE,VALAGY           VALIDATE AGENCY                              
         GOTO1 RAVPPLSA,DMCB,1     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
                                                                                
LVK4     LA    R2,SDVCLIH          CLIENT                                       
         TM    4(R2),X'20'         IF FIELD CHANGED                             
         BO    *+12                                                             
         NI    SDVOFFH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         BAS   RE,VALCLI           VALIDATE CLIENT                              
         SPACE 1                                                                
         LA    R2,SDVOFFH          OFFICE                                       
         TM    4(R2),X'20'         IF FIELD CHANGED                             
         BO    *+12                                                             
         NI    SDVVERH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         BAS   RE,VALOFF           VALIDATE OFFICE                              
         SPACE 1                                                                
         OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SDVVERH          R2=A(VERIFIED? FIELD)                        
         TM    4(R2),X'20'         IF FIELD CHANGED                             
         BO    *+12                                                             
         NI    SDVSNTH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         BAS   RE,YORNVAL          ALLOW Y OR N                                 
         SPACE 1                                                                
         OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SDVSNTH          R2=A(SENT? FIELD)                            
         TM    4(R2),X'20'         IF FIELD CHANGED                             
         BO    *+12                                                             
         NI    SDVDTEH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         BAS   RE,YORNVAL          ALLOW Y OR N                                 
         SPACE 1                                                                
         OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SDVDTEH          R2=A(SEND DATE FIELD)                        
         TM    4(R2),X'20'         IF FIELD CHANGED                             
         BO    LVK6                                                             
         NI    SDVCOMH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         XC    TIQPSTR,TIQPSTR     PRE-CLEAR DATE                               
         CLI   5(R2),0             IF DATE INPUT                                
         BE    LVK6                                                             
         CLI   SDVSNT,C'N'         BUT NOT ASKING FOR SENT INFO                 
         BE    FLDINV              THEN DATE INPUT INVALID                      
         GOTO1 DTVAL,DMCB,TIQPSTR  VALIDATE DATE                                
         SPACE 1                                                                
LVK6     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SDVCOMH          R2=A(COMPLETE? FIELD)                        
         TM    4(R2),X'20'         IF FIELD CHANGED                             
         BO    *+12                                                             
         NI    SDVCIDH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         BAS   RE,YORNVAL          ALLOW Y OR N                                 
         MVC   COMPLETE,8(R2)                                                   
         OI    COMPLETE,X'40'                                                   
         SPACE 1                                                                
LVK8     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SDVCIDH          R2=A(START CID FIELD)                        
         TM    4(R2),X'20'         IF FIELD CHANGED                             
         BO    LVK10                                                            
         NI    SDVTYPH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         XC    TIQSTART,TIQSTART   PRE-CLEAR SYSIO FILTER                       
         CLI   5(R2),0             IF INPUT                                     
         BE    LVK11               (DON'T SET PREVIOUSLY VALIDATED)             
         OC    TIFAGY,TIFAGY       MUST HAVE AGENCY SPECIFIED                   
         BZ    FLDINV              (NOT OFF=X)                                  
         CLI   SDVDUEH+5,0                                                      
         BNE   FLDINV                                                           
         MVC   TIQSTART(L'SDVCID),8(R2) SAVE INPUT                              
         OC    TIQSTART,SPACES                                                  
         SPACE 1                                                                
LVK10    OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
LVK11    LA    R2,SDVTYPH          R2=A(TYPE FIELD)                             
         TM    4(R2),X'20'         IF FIELD CHANGED                             
         BO    *+12                                                             
         NI    SDVRECH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         BAS   RE,VALLTYPE         VALIDATE LIST TYPE                           
         SPACE 1                                                                
         OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SDVRECH          R2=A(RECEIVED FIELD)                         
         TM    4(R2),X'20'         IF FIELD CHANGED                             
         BO    *+12                                                             
         NI    SDVPAIDH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         BAS   RE,YORNVAL          ALLOW Y OR N                                 
         SPACE 1                                                                
         OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SDVPAIDH         R2=A(PAID? FIELD)                            
         TM    4(R2),X'20'         IF FIELD CHANGED                             
         BO    *+12                                                             
         NI    SDVPDTEH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         BAS   RE,YORNVAL          ALLOW Y OR N                                 
         SPACE 1                                                                
         OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SDVPDTEH         R2=A(PAID DATE FIELD)                        
         TM    4(R2),X'20'         IF FIELD CHANGED                             
         BO    LVKX                                                             
         NI    SDVDUEH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         XC    PAIDDTE,PAIDDTE     PRE-CLEAR DATE                               
         CLI   5(R2),0             IF DATE INPUT                                
         BE    LVK30                                                            
         CLI   SDVPAID,C'N'        BUT NOT ASKING FOR PAID INFO                 
         BE    FLDINV              THEN DATE INPUT INVALID                      
         GOTO1 DTVAL,DMCB,PAIDDTE  VALIDATE DATE                                
         SPACE 1                                                                
LVK30    OI    4(R2),X'20'         SET FIELD VALIDATED                          
         MVI   DUE,0                                                            
         LA    R2,SDVDUEH          R2=A(DUE DATE? FIELD)                        
         TM    4(R2),X'20'         IF FIELD CHANGED                             
         BO    LVK40                                                            
         CLI   5(R2),0                                                          
         BE    LVK40                                                            
         CLI   RECNUM,PV           (INPUT NOT ALLOWED FOR PADVICE)              
         BE    FLDINV                                                           
         MVI   DUE,TLDVEDDQ                                                     
         CLI   8(R2),C'Y'          Y=BY EARLIEST DUE DATE                       
         BE    LVK40                                                            
         MVI   DUE,TLDVRDDQ                                                     
         CLI   8(R2),C'U'          U=BY REGULAR DUE DATE                        
         BE    LVK40                                                            
         MVI   DUE,TLDVMDDQ                                                     
         CLI   8(R2),C'M'          M=BY MUSIC DUE DATE                          
         BNE   FLDINV                                                           
         SPACE 1                                                                
LVK40    OI    4(R2),X'20'                                                      
         BAS   RE,INIT             RE-INITIALIZE LIST                           
         SPACE 1                                                                
LVKX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES AGENCY FIELD INPUT                             
*                                  R2=A(FIELD HEADER)                           
         SPACE 1                                                                
VALAGY   NTR1                                                                   
         CLI   TGCTSTTY,TASTTYPC   IF THIS ISN'T A CLIENT                       
         BE    VALAGY10                                                         
         CLI   TGCTSTTY,TASTTYPD                                                
         BE    VALAGY10                                                         
         CLI   TGCTSTTY,TASTTYPF                                                
         BE    VALAGY10                                                         
         XC    TIFOFF,TIFOFF                                                    
         CLI   5(R2),0             OK TO LIST ALL ADVICES                       
         BNE   VALAGY10                                                         
         CLI   SDVCLIH+5,0         AS LONG AS CLIENT ISN'T SPECIFIED            
         BNE   FLDMISS                                                          
         B     XIT                                                              
         SPACE 1                                                                
VALAGY10 GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',(R2))                                 
         GOTO1 RAVPPLSA,DMCB,1     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
         MVC   TIFAGY,TGAGY        SET SYSIO FILTER                             
         SPACE 1                                                                
         L     R3,AIO                                                           
         MVI   ELCODE,TAAYELQ      GET AGENCY DETAILS EL.                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TAAYD,R3                                                         
         MVC   TIFOFF,TAAYTPOF     SET SYSIO FILTER FOR TP OFFICE               
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE VALIDATES CLIENT FIELD INPUT                             
*                                  R2=A(FIELD HEADER)                           
         SPACE 1                                                                
VALCLI   NTR1                                                                   
         MVI   TIFCLI,0                                                         
         CLI   5(R2),0             OK TO LIST ALL ADVICES                       
         BE    XIT                                                              
         GOTO1 RECVAL,DMCB,TLCLCDQ,(X'20',(R2))                                 
         MVC   TIFCLI,TGCLI        SET SYSIO FILTER                             
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE VALIDATES YES OR NO FIELDS                               
*                                  R2=A(FIELD HEADER)                           
YORNVAL  DS    0H                                                               
         CLI   5(R2),0                                                          
         BER   RE                                                               
         CLI   8(R2),C'Y'                                                       
         BER   RE                                                               
         CLI   8(R2),C'N'                                                       
         BER   RE                                                               
         B     FLDINV                                                           
         EJECT                                                                  
*              ROUTINE VALIDATES OFFICE INPUT                                   
*                                  R2=A(FIELD HEADER)                           
         SPACE 1                                                                
VALOFF   NTR1                                                                   
         CLI   SDVAGYH+5,0                                                      
         BNE   VOFF10                                                           
         MVI   TIFOFF,0                                                         
         SPACE 1                                                                
VOFF10   CLI   5(R2),0                                                          
         BE    XIT                                                              
         CLI   TGCTSTTY,TASTTYPC   IF THIS ISN'T A CLIENT                       
         BE    FLDINV              INPUT IS OK                                  
         CLI   TGCTSTTY,TASTTYPD                                                
         BE    FLDINV                                                           
         CLI   TGCTSTTY,TASTTYPF                                                
         BE    FLDINV                                                           
         MVC   TIFOFF,8(R2)        SET SYSIO OFFICE FILTER                      
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE VALIDATES TYPE FIELD                                     
*                                  R2=A(FIELD HEADER)                           
VALLTYPE NTR1                                                                   
         MVI   LISTTYP,0           PRE-CLEAR LIST TYPE                          
         CLI   5(R2),0                                                          
         BE    XIT                                                              
         CLI   5(R2),1             IF ONE CHARACTER                             
         BNE   VALLTYP5                                                         
         CLI   8(R2),C'S'          IF WANT SESSION ONLY                         
         BNE   *+12                                                             
         OI    LISTTYP,LISTTYPS                                                 
         B     XIT                                                              
         CLI   8(R2),C'R'          IF WANT REUSE ONLY                           
         BNE   *+12                                                             
         OI    LISTTYP,LISTTYPR                                                 
         B     XIT                                                              
         CLI   8(R2),C'G'          IF WANT GENERATED ONLY                       
         BNE   *+12                                                             
         OI    LISTTYP,LISTTYPG                                                 
         B     XIT                                                              
         CLI   8(R2),C'X'          IF WANT NON-GENERATED ONLY                   
         BNE   *+12                                                             
         OI    LISTTYP,LISTTYPX                                                 
         B     XIT                                                              
         CLI   8(R2),C'N'          IF WANT NON-DDS CLARUS ONLY                  
         BNE   *+12                                                             
         OI    LISTTYP,LISTTYPN                                                 
         B     XIT                                                              
         CLI   8(R2),C'+'          IF TWO USE ADVICES                           
         BNE   FLDINV                                                           
         OI    LISTTYP,LISTTYP2                                                 
         B     XIT                                                              
         SPACE 1                                                                
VALLTYP5 GOTO1 USEVAL,DMCB,(X'40',8(R2))  CHECK SPECIFIC USE ONLY               
         BNE   FLDINV                                                           
         OI    LISTTYP,LISTTYPU                                                 
*                                                                               
         CLI   5(R2),4             IF LENGTH IS FOUR CHARACTERS                 
         BNE   XIT                                                              
         CLI   11(R2),C'G'         CHECK FOR SPECIFIC USE GENERATED             
         BNE   *+12                                                             
         OI    LISTTYP,LISTTYPG    SET GENERATED TOO                            
         B     XIT                                                              
         CLI   11(R2),C'X'         CHECK FOR SPECIFIC USE NON-GENERATED         
         BNE   *+12                                                             
         OI    LISTTYP,LISTTYPX    SET NON-GENERATED TOO                        
         B     XIT                                                              
         CLI   11(R2),C'N'         CHECK FOR SPECIFIC USE NON-DDS CLAR          
         BNE   *+12                                                             
         OI    LISTTYP,LISTTYPN    SET NON-DDS CLARUS TOO                       
         B     XIT                                                              
         CLI   11(R2),C'+'         CHECK FOR SPECIFIC USE +                     
         BNE   FLDINV                                                           
         OI    LISTTYP,LISTTYP2    SET TWO USES TOO                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY FIELDS (RECEIVE)                         
*                                                                               
RVKEY    NTR1                                                                   
         XC    TIFILTS,TIFILTS     PRE-CLEAR SYSIO FILTERS                      
         SPACE 1                                                                
         CLI   ACTNUM,ACTRECV                                                   
         BNE   XIT                                                              
         MVI   TIFOFF,0                                                         
         LA    R2,SVROFFH          VALIDATE OFFICE                              
         CLI   5(R2),1             IF INPUT IS MORE THAN 1 CHARACTER,           
         BNH   RVK2                                                             
         CLC   =C'ALL',8(R2)       MUST BE RUNNING FOR ALL OFFICES              
         BE    RVK3                                                             
         B     FLDINV                                                           
RVK2     GOTO1 ANY                                                              
         GOTO1 RECVAL,DMCB,TLOFCDQ,(R2)                                         
         MVC   TIFOFF,TGOFF        SET SYSIO FILTER                             
*                                                                               
RVK3     LA    R2,SVRDTEH          PREVIOUS RECEIVE DATE  (OPTIONAL)            
         XC    RCVDATE,RCVDATE                                                  
         XC    RCVSTME,RCVSTME                                                  
         XC    RCVETME,RCVETME                                                  
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         CLI   SVRAGYH+5,0         REQUIRED IF AGENCY INPUT                     
         BNE   FLDMISS                                                          
         B     RVK4                                                             
         GOTO1 DATVAL,DMCB,8(R2),WORK  VALIDATE                                 
         OC    0(4,R1),0(R1)                                                    
         BZ    FLDINV                                                           
         GOTO1 DATCON,DMCB,(0,WORK),(1,RCVDATE)  CONVERT TO PWOS                
*                                                                               
RVK4     LA    R2,SVRTMEH          TIME    (OPTIONAL)                           
         CLI   5(R2),0             TIME NOT ENTERED?                            
         BE    RVK5                YES, CHECK AGENCY                            
         USING TIMTABD,R3                                                       
         LA    R3,TIMTABL                                                       
RVK4A    CLI   0(R3),X'00'         EOT                                          
         BE    FLDINV                                                           
         CLI   TIFOFF,0            OFFICE IS OPTIONAL                           
         BE    RVK4AA                                                           
         CLC   TIFOFF,TIMOFF       CHECK OFFICES                                
         BNE   RVK4B                                                            
RVK4AA   CLC   TIMSET,8(R2)        IS THIS SET IN THE TIME TABLE?               
         BNE   RVK4B                                                            
         MVC   RCVSTME,TIMSTR      MOVE TIME TO TIME FILTER                     
         MVC   RCVETME,TIMEND                                                   
         B     RVK5                                                             
*                                                                               
RVK4B    LA    R3,TIMTLNQ(R3)                                                   
         B     RVK4A                                                            
*                                                                               
RVK5     LA    R2,SVRAGYH          AGENCY  (OPTIONAL)                           
         XC    SVRAGYN,SVRAGYN                                                  
         OI    SVRAGYNH+6,X'80'                                                 
         CLI   5(R2),0                                                          
         BE    RVK6                                                             
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'08',(R2)),SVRAGYNH                        
         MVC   TIFAGY,TGAGY        SET SYSIO FILTER                             
*                                                                               
RVK6     BAS   RE,INIT             INITIALIZE SYSIO TO START OVER               
*                                                                               
***      MVI   WHEN,X'40'          SET AS IF 'NOW' REPORT                       
***      MVI   TWAWHEN,0                                                        
***      MVC   REMUSER(2),TGCTSTAF SET ID                                       
***      MVI   REMUSER+2,C' '                                                   
         B     XIT                                                              
         EJECT                                                                  
*              INITIALIZATION ROUTINES                                          
         SPACE 1                                                                
INIT     NTR1                                                                   
         XC    KEY,KEY             INITIALIZE KEY                               
         MVC   TIUSERID,TWAORIG    SET UP SOME FIELDS FOR SYSIO                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVI   TIQFLAGS,TIQFDIR    SET I WANT TO FILTER DIRECTORY               
         MVI   TIREAD,TLDVOCDQ     SET TO READ ADVICES BY OFF/SENT DT           
         SPACE 1                                                                
         CLI   ACTNUM,ACTRECV      IF ADVICE/LIST                               
         BE    XIT                                                              
         SPACE 1                                                                
         CLI   DUE,0               IF READING BY DUE DATE                       
         BE    *+10                                                             
         MVC   TIREAD,DUE          SET TO READ ADVICES BY DUE DATE              
         SPACE 1                                                                
         CLI   COMPLETE,C' '       IF USING COMPLETE/INCOMPLETE STAT            
         BE    *+8                                                              
         NI    TIQFLAGS,X'FF'-TIQFDIR TURN OFF DIRECTORY FILTER                 
*                                                                               
         OC    TIQSTART,TIQSTART   IF START COMMERCIAL SPECIFIED                
         BZ    INIT10                                                           
         OC    TIQPSTR,TIQPSTR     BUT NO SEND DATE                             
         BNZ   INIT10                                                           
         MVI   TIREAD,TLDVCDQ      SET TO READ ACTIVE POINTER                   
         MVI   TIFOFF,0            NO NEED FOR OFFICE FILTER                    
         NI    TIQFLAGS,X'FF'-TIQFDIR AND TURN OFF DIRECTORY FILTER             
*                                                                               
INIT10   CLI   RECNUM,PV                                                        
         BNE   XIT                                                              
         MVI   TIREAD,TLPVCDQ                                                   
         MVI   TIRDSUBT,TLPVSCDQ                                                
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS RECORD LISTING                                  
         SPACE 1                                                                
LREC     NTR1                                                                   
         TWAXC SDVSELH,SDVLSTH,PROT=Y  CLEAR SCREEN                             
         MVC   LTAEQ,=C'A='        SET LITERAL FOR SELECT INTO PAY              
         SPACE 1                                                                
         LA    R0,LRHOOK           SET HOOK FOR SYSIO                           
         CLI   ACTNUM,ACTRECV      IF ACTION IS RECEIVE                         
         BNE   *+12                                                             
         LA    R0,RVHOOK           SET I/O HOOK FOR RECEIVE                     
         OI    TIQFLAGS,TIQFUPDR   SET READ DIRECTORY FOR UPDATE                
         ST    R0,TIHOOK                                                        
         MVC   TIKHOOK,SETLSTK                                                  
         MVC   TIACOMFC,ACOMFACS                                                
         XC    TIABUFF,TIABUFF                                                  
         SPACE 1                                                                
         BAS   RE,RECEIVE          FAKE SYSIO FOR RECEIVE                       
         BE    LR10                                                             
         SPACE 1                                                                
         BAS   RE,PRTLIST          FAKE SYSIO FOR PRINTING                      
         BE    LR10                                                             
         SPACE 1                                                                
         MVI   NLISTS,16           IN ORDER TO GET CONTROL                      
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
         MVI   NLISTS,15           BACK AFTER 1 FULL PAGE                       
         SPACE 1                                                                
LR10     CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         CP    COUNTER,=P'0'       IF ANYTHING REPORTED                         
         BE    LRX                                                              
         EDIT  COUNTER,(5,P+1),COMMAS=YES,ALIGN=LEFT                            
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(14,R1),=C'ADVICE RECORDS'                                      
         BAS   RE,PRNTIT                                                        
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
         CLI   TIMODE,PROCDIR      IF DIRECTORY HOOK                            
         BNE   LRH10                                                            
         BAS   RE,FILTKEY          FILTER DIRECTORY KEY                         
         BE    YES                                                              
         B     NO                                                               
         SPACE 1                                                                
LRH10    CLI   TIMODE,PROCREC                                                   
         BNE   XIT                                                              
         L     R3,TIAREC           R3=A(RECORD                                  
         USING TLDVD,R3                                                         
         CLI   TLDVSEQ,0           IGNORE IF NOT BASE RECORD                    
         BNE   XIT                                                              
         SPACE 1                                                                
         ST    R3,AIO                                                           
         MVI   REGOTH,C'N'         SEE IF COMMENT ELEMENT                       
         MVI   ELCODE,TACMELQ      EXISTS FOR REGULAR USE                       
         GOTO1 GETL,DMCB,(1,=AL1(TACMTYPU))                                     
         BNE   *+8                                                              
         MVI   REGOTH,C'Y'                                                      
         SPACE 1                                                                
         MVI   MUSOTH,C'N'         SEE IF COMMENT ELEMENT                       
         MVI   ELCODE,TACMELQ      EXISTS FOR MUSIC USE                         
         GOTO1 GETL,DMCB,(1,=AL1(TACMTYPM))                                     
         BNE   *+8                                                              
         MVI   MUSOTH,C'Y'                                                      
         SPACE 1                                                                
         L     R3,TIAREC                                                        
         BAS   RE,FILTREC          FILTER RECORD                                
         BNE   XIT                                                              
         SPACE 1                                                                
         CLI   MODE,LISTRECS       IF LISTING ON SCREEN                         
         BNE   *+12                                                             
         CLI   LISTNUM,15          AND ALREADY COMPLETED THIS PAGE              
         BE    ENDPAGE             GIVE MESSAGE - MORE TO COME                  
         MVC   LISTAR,SPACES       CLEAR PREVIOUS LINE                          
         SPACE 1                                                                
         BAS   RE,FORMAT           FORMAT TO PRINT LINE                         
         SPACE 1                                                                
         CLI   MODE,PRINTREP                                                    
         BNE   LRH30                                                            
         BAS   RE,PRNTIT                                                        
         AP    COUNTER,=P'1'       INCREMENT COUNTER                            
         B     LRHX                                                             
         SPACE 1                                                                
LRH30    MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
         GOTO1 LISTMON             CALL LISTMON                                 
         SPACE 1                                                                
LRHX     B     XIT                                                              
         EJECT                                                                  
*              ROUTINE FILTERS ADVICES AT DIRECTORY LEVEL                       
         SPACE 1                                                                
FILTKEY  NTR1                                                                   
         MVC   BYTE,TIKEYST+1      PASS STATUS BYTE TO                          
         BAS   RE,FILTER           TO FILTER ROUTINE, RETURNS CC                
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE FILTERS ADVICES AT FILE RECORD LEVEL                     
         SPACE 1                                                                
FILTREC  NTR1                                                                   
         MVI   ELCODE,TADVELQ      GET ADVICE DETAILS ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         BNE   XIT                                                              
         USING TADVD,R3                                                         
         TM    TIQFLAGS,TIQFDIR    IF NOT FILTERING AT DIRECTORY LEVEL          
         BO    FILTREC5                                                         
         MVC   BYTE,TADVSTAT       PASS STATUS BYTE TO                          
         BAS   RE,FILTER           TO FILTER ROUTINE, RETURNS CC                
         BNE   XIT                                                              
*                                                                               
FILTREC5 OC    TIFCLI,TIFCLI       IF CLIENT DEFINED                            
         BZ    *+14                                                             
         CLC   TIFCLI,TADVCLI      MATCH ON IT                                  
         BNE   XIT                                                              
*                                                                               
         OC    TIQPSTR,TIQPSTR     IF SEND DATE DEFINED                         
         BZ    *+14                                                             
         CLC   TIQPSTR,TADVSDTE    MATCH ON IT                                  
         BNE   XIT                                                              
*                                                                               
         OC    PAIDDTE,PAIDDTE     IF PAID DATE DEFINED                         
         BZ    *+14                                                             
         CLC   PAIDDTE,TADVPDTE    MATCH ON IT                                  
         BNE   XIT                                                              
*                                                                               
         CLI   LISTTYP,0           IF FILTERING LIST BY TYPE                    
         BE    *+12                                                             
         BAS   RE,LRCKTYP          CHECK TYPE                                   
         BNE   XIT                                                              
*                                                                               
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE FILTERS ADVICE STATUS BASED ON REQUEST                   
*                                  NTRY - BYTE SET WITH STATUS                  
         SPACE 1                                                                
FILTER   NTR1                                                                   
         CLI   SDVVER,C'Y'         ONLY VERIFIED                                
         BNE   *+12                                                             
         TM    BYTE,TADVSVER                                                    
         BZ    NO                                                               
         CLI   SDVVER,C'N'         NOT VERIFIED                                 
         BNE   *+12                                                             
         TM    BYTE,TADVSVER                                                    
         BO    NO                                                               
         CLI   SDVSNT,C'Y'         ONLY SENT                                    
         BNE   *+12                                                             
         TM    BYTE,TADVSSNT                                                    
         BZ    NO                                                               
         CLI   SDVSNT,C'N'         NOT SENT                                     
         BNE   *+12                                                             
         TM    BYTE,TADVSSNT                                                    
         BO    NO                                                               
         CLI   SDVREC,C'Y'         ONLY RECEIVED                                
         BNE   *+12                                                             
         TM    BYTE,TADVSRCV                                                    
         BZ    NO                                                               
         CLI   SDVREC,C'N'         NOT RECEIVED                                 
         BNE   *+12                                                             
         TM    BYTE,TADVSRCV                                                    
         BO    NO                                                               
         CLI   SDVPAID,C'Y'        ONLY PAID                                    
         BNE   *+12                                                             
         TM    BYTE,TADVSPAY                                                    
         BZ    NO                                                               
         CLI   SDVPAID,C'N'        NOT PAID                                     
         BNE   *+12                                                             
         TM    BYTE,TADVSPAY                                                    
         BO    NO                                                               
         CLI   COMPLETE,C'Y'       COMPLETE                                     
         BNE   *+12                                                             
         TM    BYTE,TADVSCMP                                                    
         BZ    NO                                                               
         CLI   COMPLETE,C'N'       INCOMPLETE                                   
         BNE   *+12                                                             
         TM    BYTE,TADVSCMP                                                    
         BO    NO                                                               
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO CHECK TYPE                                            
         SPACE                                                                  
LRCKTYP  NTR1                                                                   
         L     R3,TIAREC                                                        
         MVI   ELCODE,TADVELQ      GET ADVICE DETAILS EL.                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADVD,R3                                                         
*                                                                               
         TM    LISTTYP,LISTTYPS    IF WANT SESSIONS                             
         BZ    *+12                                                             
         CLI   TADVTYPE,TADVTYPS   MUST BE A SESSION ADVICE                     
         BNE   NO                                                               
*                                                                               
         TM    LISTTYP,LISTTYPR    IF WANT REUSE                                
         BZ    *+12                                                             
         CLI   TADVTYPE,TADVTYPS   MUST BE A REUSE ADVICE                       
         BE    NO                                                               
*                                                                               
         TM    LISTTYP,LISTTYPU    IF FILTERING BY USE                          
         BZ    LRCKT20                                                          
         CLI   TADVTYPE,C'S'       AND THIS IS SESSION ADVICE                   
         BE    NO                  CAN'T DO                                     
         L     R3,TIAREC           CHECK MATCH AGAINST BOTH USES                
         MVI   ELCODE,TAVUELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         USING TAVUD,R3                                                         
         MVC   TGUSCDE,=C'OTH'     IF WANT TO MATCH ON OTHER                    
         CLI   REGOTH,C'Y'         MUST SET TGUSCDE MYSELF                      
         BE    LRCKT10                                                          
         GOTO1 USEVAL,DMCB,(X'C0',TAVUUSE),0                                    
         BNE   LRCKT15                                                          
LRCKT10  CLC   TGUSCDE,SDVTYP      IF ADVICE USE MATCHES                        
         BE    LRCKT20             SKIP AHEAD                                   
         SPACE 1                                                                
LRCKT15  MVC   TGUSCDE,=C'OTH'     IF WANT TO MATCH ON OTHER                    
         CLI   MUSOTH,C'Y'         RESET TGUSCDE TO OTHER                       
         BE    LRCKT16                                                          
         GOTO1 USEVAL,DMCB,(X'C0',TAVUUSE2),0                                   
         BNE   NO                                                               
LRCKT16  CLC   TGUSCDE,SDVTYP                                                   
         BNE   NO                                                               
*                                                                               
LRCKT20  TM    LISTTYP,LISTTYP2    IF WANT TWO USE ADVICES                      
         BZ    LRCKT30                                                          
         L     R3,TIAREC           MUST HAVE TWO USES                           
         MVI   ELCODE,TAVUELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   NO                                                               
         USING TAVUD,R3                                                         
         LHI   R1,0                                                             
         CLI   TAVUUSE,0           IF ANY TWO OF USE1                           
         BE    *+8                                                              
         AHI   R1,1                                                             
         CLI   TAVUUSE2,0          USE2                                         
         BE    *+8                                                              
         AHI   R1,1                                                             
         CLI   REGOTH,C'Y'         REGULAR COMMENT                              
         BNE   *+8                                                              
         AHI   R1,1                                                             
         CLI   MUSOTH,C'Y'         OR MUSIC COMMENT PRESENT                     
         BNE   *+8                                                              
         AHI   R1,1                                                             
         CHI   R1,2                                                             
         BNE   NO                  THEN YES                                     
*                                                                               
         USING TANXD,R3                                                         
LRCKT30  TM    LISTTYP,LISTTYPG+LISTTYPN                                        
         BZ    LRCKT50             IF WANT GENERATED OR NON-DDS                 
         L     R3,TIAREC           CLARUS ADVICES ONLY                          
         MVI   ELCODE,TANXELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   NO                  MUST HAVE TANXD ELEMENT                      
*                                                                               
         TM    LISTTYP,LISTTYPG                                                 
         BZ    LRCKT40                                                          
         TM    TANXSTAT,TANXNDDS                                                
         BZ    YES                                                              
         B     NO                                                               
*                                                                               
LRCKT40  TM    TANXSTAT,TANXNDDS                                                
         BO    YES                                                              
         B     NO                                                               
         DROP  R3                                                               
*                                                                               
LRCKT50  TM    LISTTYP,LISTTYPX    IF ONLY WANT NON-GENERATED ADVICES           
         BZ    LRCKTX                                                           
         L     R3,TIAREC                                                        
         MVI   ELCODE,TANXELQ                                                   
         BAS   RE,GETEL                                                         
         BE    NO                  CANNOT HAVE TANXD ELEMENT                    
LRCKTX   B     YES                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS AN ADVICE                                       
         SPACE 1                                                                
         USING LINED,R2            R2=A(OUTPUT AREA)                            
FORMAT   NTR1                                                                   
         MVI   ELCODE,TADVELQ      GET ADVICE DETAILS EL.                       
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADVD,R3            R3=A(ADVICE DETAILS EL.)                     
         SPACE 1                                                                
         MVC   TGAGY,TIAGY                                                      
         SPACE 1                                                                
         MVC   LINAGY,TIAGY        AGENCY                                       
         MVC   LINCID,TICID        COMMERCIAL ID                                
         MVC   LINADV,TIADV        ADVICE                                       
         BAS   RE,SETTYPE          SET TYPE AND USE                             
         SPACE 1                                                                
         OC    TADVSINF,TADVSINF    DISPLAY SEND INFO                           
         BZ    FRMT10                                                           
         GOTO1 DATCON,DMCB,(1,TADVSDTE),(4,LINSINF)                             
         SPACE 1                                                                
FRMT10   MVC   LINPINF,=C'IN'                                                   
         TM    TADVSTAT,TADVSCMP                                                
         BZ    FRMT30                                                           
         MVC   LINPINF,=C'CO'                                                   
         B     FRMT30                                                           
         SPACE 1                                                                
FRMT20   OC    TADVPINF,TADVPINF              DISPLAY PAID INFO                 
         BZ    FRMT30                                                           
         GOTO1 DATCON,DMCB,(1,TADVPDTE),(4,LINPINF)                             
         SPACE 1                                                                
FRMT30   OC    TADVRINF,TADVRINF            DISPLAY RECEIVE INFO                
         BZ    FRMT40                                                           
         GOTO1 DATCON,DMCB,(1,TADVRDTE),(4,LINRINF)                             
         SPACE 1                                                                
FRMT40   BAS   RE,VERLIFT                   DISPLAY VERSION/LIFT                
*        BAS   RE,FEEDS                     DISPLAY FEED CODES                  
         SPACE 1                                                                
         USING TADDD,R3                                                         
         L     R3,TIAREC                                                        
         MVI   ELCODE,TADDELQ      GET DUE DATE ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   FRMT55                                                           
         SPACE 1                                                                
         OC    TADDDATE,TADDDATE   DISPLAY REGULAR DUE DATE                     
         BZ    FRMT50                                                           
         GOTO1 DATCON,DMCB,(1,TADDDATE),(8,LINDUE)                              
         SPACE 1                                                                
FRMT50   OC    TADDDATM,TADDDATM   DISPLAY MUSIC DUE DATE                       
         BZ    FRMT55                                                           
         GOTO1 DATCON,DMCB,(1,TADDDATM),(8,LINDUE2)                             
         DROP  R3                                                               
         SPACE 1                                                                
         USING TAAID,R3                                                         
FRMT55   L     R3,TIAREC                                                        
         MVI   ELCODE,TAAIELQ      GET ADV/INV ASSIGN ELEMENT                   
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         SPACE 1                                                                
         XC    INVR(18),INVR       CLEAR INVOICE FIELDS                         
         SPACE 1                                                                
         OC    TAAIINUM,TAAIINUM   SET INV1 AS FIRST REG INVOICE                
         BZ    FRMT60                                                           
         GOTO1 TINVCON,DMCB,TAAIINUM,INVR,DATCON                                
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'00'                                                            
         XC    INVR,=6X'FF'                                                     
         MVC   INVRLAST,INVR                                                    
         SPACE 1                                                                
         CLI   TAAILEN,TAAIILNQ                                                 
         BNH   FRMT60                                                           
         OC    TAAIINUX,TAAIINUX   SET INVRLAST AS LAST REG INVOICE             
         BZ    FRMT60                                                           
         GOTO1 TINVCON,DMCB,TAAIINUX,INVRLAST,DATCON                            
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'00'                                                            
         XC    INVRLAST,=6X'FF'                                                 
         SPACE 1                                                                
FRMT60   CLI   TAAILEN,TAAIILNQ                                                 
         BL    FRMT70                                                           
         OC    TAAIINU2,TAAIINU2   SET INVM AS LAST MUSIC INVOICE               
         BZ    FRMT70                                                           
         GOTO1 TINVCON,DMCB,TAAIINU2,INVM,DATCON                                
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'00'                                                            
         XC    INVM,=6X'FF'                                                     
         DROP  R3                                                               
         SPACE 1                                                                
FRMT70   MVC   AIO,AIO3                                                         
         OC    INVR,INVR                                                        
         BZ    FRMT110                                                          
         SPACE 1                                                                
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A4',INVRLAST)                             
         BE    FRMT90                                                           
         MVC   LINDUE,=CL8'DELETED'                                             
         B     FRMT110                                                          
FRMT80   CLC   KEY+TLININV-TLIND(L'TLININV),INVR                                
         BE    FRMT100                                                          
         GOTO1 SEQ                                                              
         CLC   KEY(TLININV-TLIND),KEYSAVE                                       
         BNE   FRMT105                                                          
         CLC   KEY+TLININV-TLIND(L'TLININV),INVR                                
         BH    FRMT105                                                          
         GOTO1 GETREC                                                           
         SPACE 1                                                                
         USING TAIND,R3                                                         
FRMT90   L     R3,AIO3                                                          
         MVI   ELCODE,TAINELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   FRMT110                                                          
         OC    TAINPINF,TAINPINF                                                
         BZ    FRMT110                                                          
         CLC   INVR,INVRLAST                                                    
         BNE   FRMT80                                                           
         DROP  R3                                                               
         SPACE 1                                                                
FRMT100  MVC   LINDUE,=CL8'PAID'                                                
         B     FRMT110                                                          
FRMT105  CLC   LINDUE,=CL8'PAID'                                                
         BE    FRMT110                                                          
         MVC   LINDUE,=CL8'DELETED'                                             
         SPACE 1                                                                
FRMT110  OC    INVM,INVM                                                        
         BZ    FRMT130                                                          
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'A4',INVM)                                 
         BE    FRMT120                                                          
         MVC   LINDUE2,=CL8'DELETED'                                            
         B     FRMT130                                                          
         SPACE 1                                                                
         USING TAIND,R3                                                         
FRMT120  L     R3,AIO3                                                          
         MVI   ELCODE,TAINELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   FRMT130                                                          
         OC    TAINPINF,TAINPINF                                                
         BZ    FRMT130                                                          
         DROP  R3                                                               
         SPACE 1                                                                
         MVC   LINDUE2,=CL8'PAID'                                               
         SPACE 1                                                                
FRMT130  MVC   AIO,TIAREC                                                       
         MVC   KEY,TIKEY                                                        
         GOTO1 HIGH                                                             
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE DISPLAYS ADVICE TYPE AND USE                             
         SPACE 1                                                                
         USING TADVD,R3                                                         
SETTYPE  NTR1                                                                   
         CLI   TADVTYPE,TADVTYPS   IF SESSION ADVICE                            
         BNE   SETTYPE0                                                         
         MVI   LINTYPE,C'S'        PUT S IN FIRST TYPE FIELD                    
         B     SETTYPEX                                                         
*                                                                               
SETTYPE0 L     R3,TIAREC           IF NOT SESSION ADVICE                        
         MVI   ELCODE,TAVUELQ                                                   
         BAS   RE,GETEL            R3=A(ADVICE USE DETAILS ELEMENT)             
         BNE   SETTYPEX                                                         
         USING TAVUD,R3                                                         
*                                                                               
         CLI   REGOTH,C'Y'         IF REGULAR USE COMMENT EXISTS                
         BNE   *+14                                                             
         MVC   LINTYPE(3),=C'OTH'  USE IS OTHER                                 
         B     SETTYPE3                                                         
*                                                                               
         CLI   TAVUUSE,0           IF NO REGULAR USE - NOTHING                  
         BE    SETTYPE3                                                         
*                                                                               
         GOTO1 USEVAL,DMCB,(X'C0',TAVUUSE),0  OTHERWISE, CALL USEVAL            
         MVC   LINTYPE(L'TGUSCDE),TGUSCDE     AND OUTPUT USE CODE               
         CLI   TGUSXUNI,ALL-NON-AFM                                             
         BNE   SETTYPE3                                                         
         MVC   LINTYPE,SPACES                                                   
         MVC   LINTYPE2(L'TGUSCDE),TGUSCDE                                      
*                                                                               
SETTYPE3 CLI   MUSOTH,C'Y'         IF MUSIC USE COMMENT EXISTS                  
         BNE   *+14                                                             
         MVC   LINTYPE2(3),=C'OTH' USE IS OTHER                                 
         B     SETTYPE4                                                         
*                                                                               
         CLI   TAVUUSE2,0          IF NO MUSIC USE - NOTHING                    
         BE    SETTYPE4                                                         
*                                                                               
         GOTO1 USEVAL,DMCB,(X'C0',TAVUUSE2),0 OTHERWISE, CALL USEVAL            
         MVC   LINTYPE2(L'TGUSCDE),TGUSCDE    AND OUTPUT USE CODE               
*                                                                               
         USING TANXD,R3                                                         
SETTYPE4 L     R3,TIAREC           R3=A(ADVICE RECORD)                          
         MVI   ELCODE,TANXELQ                                                   
         BAS   RE,GETEL                                                         
         BNE   SETTYPEX                                                         
         MVI   LINTYPE+3,C'G'      INDICATE GENERATED ADVICE                    
         TM    TANXSTAT,TANXNDDS                                                
         BZ    SETTYPEX                                                         
         MVI   LINTYPE+3,C'N'      INDICATE NON-DDS CLARUS ADVICE               
         DROP  R3                                                               
*                                                                               
SETTYPEX B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO DISPLAY VERSION/LIFT                                  
         SPACE 1                                                                
VERLIFT  NTR1                                                                   
         MVC   AIO,TIAREC                                                       
         MVI   ELCODE,TAFNELQ                                                   
         GOTO1 GETL,DMCB,(1,=AL1(TAFNTVER))                                     
         BNE   VERLIFTX                                                         
         USING TAFND,R3                                                         
         L     R3,TGELEM                                                        
         MVC   LINLFT(1),TAFNNAME                                               
         SPACE 1                                                                
         TM    TGSYSTAT,TASYS3VR   DONE IF SYSTEM NOT SET TO HANDLE             
         BZ    VERLIFTX            3-CHARACTER VERSION CODES                    
         MVC   TGVER,TAFNNAME      ELSE SAVE VERSION                            
         DROP  R3                                                               
         SPACE 1                                                                
         L     R3,AIO              IF LIFT ELEMENT EXIST ON ADVICE              
         MVI   ELCODE,TALFELQ      NO NEED TO CONVERT VERSION CODE              
         BAS   RE,GETEL                                                         
         BE    VERLIFTX            ELSE, CONVERT IT                             
         EDIT  TGVER,LINLFT,ALIGN=LEFT                                          
         SPACE 1                                                                
VERLIFTX MVC   AIO,AIO1                                                         
         B     XIT                                                              
         SPACE 2                                                                
*&&DO                                                                           
*              ROUTINE TO DISPLAY FEED CODES                                    
         SPACE 1                                                                
FEEDS    NTR1                                                                   
         USING TANPD,R3                                                         
         L     R3,TIAREC           R3=A(ADVICE RECORD)                          
         MVI   ELCODE,TANPELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
FEEDS10  BAS   RE,NEXTEL           GET PROGRAM ELEMENTS                         
         BNE   FEEDSX                                                           
         CLI   TANPLEN,TANPLNQ3    SKIP IF NOT NEW LENGTH                       
         BL    FEEDS10                                                          
         CLC   TANPFEED,SPACES     SKIP IF FEED IS SPACES                       
         BE    FEEDS10                                                          
         CLC   TANPFEED,LINPFEED   SKIP IF FEED ALREADY ON                      
         BE    FEEDS10             PRINT LINE                                   
         CLC   LINPFEED,SPACES     IF THIS IS FIRST FOUND                       
         BNE   FEEDS20             FEED CODE, DISPLAY IT                        
         MVC   LINPFEED,TANPFEED                                                
         B     FEEDS10                                                          
FEEDS20  MVC   LINPFEED,=C'****'   IF MORE THAN ONE FEED CODE                   
         B     FEEDS10             ON ADVICE DISPLAY ASTERISKS                  
         DROP  R3                                                               
FEEDSX   B     XIT                                                              
*&&                                                                             
         SPACE 2                                                                
*              ROUTINE TO PRINT A LINE                                          
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              FAKE SYSIO FOR RECEIVING ADVICE RECORDS                          
         SPACE 1                                                                
RECEIVE  NTR1                                                                   
         CLI   ACTNUM,ACTRECV      IF ACTION IS RECEIVE                         
         BNE   NO                                                               
         SPACE 1                                                                
         MVI   TIMODE,PROCDIR                                                   
         SPACE 1                                                                
         XC    KEY,KEY             READ THE ADVICE KEYS                         
         MVC   KEY(L'TIREAD),TIREAD                                             
         MVC   KEY+1(L'TIFOFF),TIFOFF                                           
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         B     REC20                                                            
REC10    GOTO1 SEQ                                                              
REC20    CLI   TIFOFF,0            IF NO OFFICE FILTER                          
         BNE   REC25                                                            
         CLC   KEY(1),KEYSAVE      DON'T FILTER ON OFFICE                       
         B     REC26                                                            
REC25    CLC   KEY(2),KEYSAVE                                                   
REC26    BNE   REC30                                                            
         SPACE 1                                                                
         USING TLDRD,RE                                                         
         LA    RE,KEY                                                           
         MVC   TIKEY,KEY           SAVE KEY,                                    
         MVC   TIKEYST,TLDRSTAT    STATUS                                       
         MVC   TIDSKADD,TLDRDA     AND DISK ADDRESS LIKE SYSIO                  
         BAS   RE,RVHOOK           AND GO TO HOOK                               
         B     REC10                                                            
         DROP  RE                                                               
*                                                                               
REC30    ZAP   COUNTER,=P'0'                                                    
         B     YES                                                              
         EJECT                                                                  
*              FAKE SYSIO FOR PRINTING ADVICE LIST                              
         SPACE 1                                                                
PRTLIST  NTR1                                                                   
         CLI   MODE,PRINTREP       IF MODE IS PRINTREP                          
         BNE   NO                                                               
         SPACE 1                                                                
         LA    RE,KEY              GET SET TO READ ADVICE KEYS                  
         XC    KEY,KEY                                                          
         MVC   KEY(1),TIREAD                                                    
         SPACE 1                                                                
         USING TLDVD,RE                                                         
         CLI   KEY,TLDVCDQ         IF READING MAIN ADVICE KEY                   
         BNE   PL10                SET AGENCY FILTER                            
         MVC   TLDVAGY,TIFAGY                                                   
         B     PL20                                                             
         DROP  RE                                                               
         SPACE 1                                                                
         USING TLDVPD,RE                                                        
PL10     MVC   TLDVOOFF,TIFOFF     IF READING PASSIVE ADVICE KEY                
         MVC   TLDVOAGY,TIFAGY     SET OFFICE AND AGENCY FILTER                 
         DROP  RE                                                               
         SPACE 1                                                                
PL20     GOTO1 HIGH                                                             
         B     PL40                                                             
PL30     GOTO1 SEQ                                                              
PL40     CLC   KEY(8),KEYSAVE                                                   
         BNE   YES                                                              
         SPACE 1                                                                
         USING TLDRD,RE                                                         
         LA    RE,KEY                                                           
         MVC   TIKEY,KEY           SAVE KEY,                                    
         MVC   TIKEYST,TLDRSTAT    STATUS                                       
         MVC   TIDSKADD,TLDRDA     AND DISK ADDRESS LIKE SYSIO                  
         DROP  RE                                                               
         SPACE 1                                                                
         USING TLDVD,RE                                                         
PL50     GOTO1 GETREC              GET THE ADVICE RECORD                        
         MVC   TIAREC,AIO          SET TIAREC                                   
         L     RE,TIAREC           AND SET TASYSIO FIELDS                       
         MVC   TIAGY,TLDVAGY       FOR AGENCY                                   
         MVC   TICID,TLDVCID       COMMERCIAL ID                                
         MVC   TIADV,TLDVADV       AND ADVICE                                   
         DROP  RE                                                               
         SPACE 1                                                                
         MVI   TIMODE,PROCREC      SET TO PROCESS RECORD                        
         NI    TIQFLAGS,X'FF'-TIQFDIR                                           
         BAS   RE,LRHOOK           AND GO TO HOOK                               
         B     PL30                                                             
         EJECT                                                                  
*              I/O ROUTINE FOR RECEIVING ADVICE RECORDS                         
         SPACE 1                                                                
RVHOOK   NTR1                                                                   
         CLI   TIMODE,PROCDIR      RECOGNIZE ONLY DIRECTORY HOOK                
         BNE   XIT                                                              
         SPACE 1                                                                
         OC    RCVDATE,RCVDATE     IF RECEIVE DATE FILTER DEFINED               
         BZ    RVH05                                                            
         TM    TIKEYST+1,TADVSRCV  THEN MUST HAVE BEEN RECEIVED ALREADY         
         BZ    RVHX                                                             
         OC    TIFAGY,TIFAGY       IF AGENCY FILTER DEFINED                     
         BZ    RVH10                                                            
         CLC   TIFAGY,TIKEY+2      THEN MUST MATCH AGENCY                       
         BNE   RVHX                                                             
         B     RVH10                                                            
         SPACE 1                                                                
RVH05    TM    TIKEYST+1,TADVSSNT  ELSE MUST HAVE BEEN SENT                     
         BZ    RVHX                                                             
         TM    TIKEYST+1,TADVSRCV  AND NOT RECEIVED YET                         
         BO    RVHX                                                             
         MVI   RDUPDATE,C'Y'       SET TO READ FOR UPDATE                       
         SPACE 1                                                                
RVH10    MVC   KEY+TLDRDA-TLDRD(4),TIDSKADD  SET D/A IN GENCON KEY              
         GOTO1 GETREC              GET FILE RECORD                              
         GOTO1 SAVPTRS,DMCB,PTRBLK SAVE PASSIVE POINTERS                        
         SPACE 1                                                                
         USING TLDVD,R3                                                         
         L     R3,AIO                                                           
         MVC   TIAGY,TLDVAGY                                                    
         MVC   TICID,TLDVCID                                                    
         MVC   TIADV,TLDVADV                                                    
         DROP  R3                                                               
         SPACE 1                                                                
         MVI   ELCODE,TADVELQ      GET ADVICE DETAILS ELEMENT                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TADVD,R3                                                         
         CLI   TADVOFF2,0          IF SECOND OFFICE CODE SPECIFIED              
         BE    *+14                                                             
         CLC   TADVOFF2,TIKEY+TLDVOOFF-TLDVPD AND DIFFERENT FROM KEY            
         BNE   RVHX                DON'T PROCESS IT                             
         OC    RCVDATE,RCVDATE     IF RECEIVE DATE FILTER DEFINED               
         BZ    RVH30                                                            
         CLC   TADVRDTE,RCVDATE    MUST BE RECEIVED ON THAT DAY                 
         BNE   RVHX                                                             
** GH                                                                           
         OC    RCVSTME(6),RCVSTME  IF TIME FILTER NOT SET                       
         BZ    RVH200              LEAVE                                        
*                                                                               
         CLC   TADVRTIM,RCVSTME    RECEIVE TIME MUST BE WITHIN FILTER           
         BL    RVHX                                                             
         CLC   TADVRTIM,RCVETME                                                 
         BH    RVHX                                                             
** GH                                                                           
         B     RVH200              SKIP TO ADVICE PRINTING                      
         SPACE 1                                                                
RVH30    OI    TADVSTAT,TADVSRCV   SET RECEIVED                                 
         MVC   TGBYTE,TADVSTAT     SAVE STATUS FOR DIRECTORY                    
         MVC   TADVRID,TWAORIG     USER ID                                      
         MVC   TADVRST,TGCTSTAF    STAFF ID                                     
         MVC   TADVRDTE,TGTODAY1   DATE                                         
         TIME  DEC                                                              
         STCM  R0,14,TADVRTIM      TIME                                         
         SPACE 1                                                                
         CLI   TADVTYPE,TADVTYPS                                                
         BE    RVH40                                                            
         DROP  R3                                                               
         SPACE 1                                                                
         BAS   RE,PROINV                                                        
         SPACE 1                                                                
         USING TAAID,R3                                                         
         LA    R3,ELEM             ADD INVOICE ASSIGN ELEMENT                   
         XC    ELEM,ELEM                                                        
         MVI   TAAIEL,TAAIELQ                                                   
         MVI   TAAILEN,TAAIILNQ                                                 
         MVC   TAAIINUM,INVR                                                    
         MVC   TAAIINU2,INVM                                                    
         OC    INVRLAST,INVRLAST                                                
         BZ    RVH35                                                            
         MVI   TAAILEN,TAAIILNQ+L'TAAIINUX                                      
         MVC   TAAIINUX,INVRLAST                                                
RVH35    GOTO1 ADDELEM                                                          
         DROP  R3                                                               
         SPACE 1                                                                
RVH40    GOTO1 PUTREC              WRITE BACK THE RECORD                        
         GOTO1 ADDPTRS,DMCB,(X'20',PTRBLK)                                      
         SPACE 1                                                                
RVH200   CLC   TIAGY,MYAGY         IF AGENCY CHANGED                            
         BE    RVH50                                                            
         MVC   MYAGY,TIAGY         READ IT INTO AIO2 FOR PRINT MODULE           
         MVC   AIO,AIO2                                                         
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'A0',MYAGY)  GET AGENCY RECORD             
         SPACE 1                                                                
RVH50    MVC   AIO,AIO1            RESTORE A(ADVICE RECORD)                     
         MVC   TGCID,TICID         SET TG FIELDS FOR ADVPRNT                    
         MVC   TGADV,TIADV                                                      
         GOTO1 ADVPRNT,DMCB,(RC),ADCREC PRINT ADVICE RECORD                     
         AP    COUNTER,=P'1'       INCREMENT COUNTER                            
         SPACE 1                                                                
         MVC   KEY,TIKEY           RESET SYSIO'S KEY                            
         GOTO1 HIGH                RE-READ IT FOR SEQUENTIAL READ               
         SPACE 1                                                                
RVHX     B     NO                  ALWAYS RETURN CC NE                          
         EJECT                                                                  
*              ROUTINE TO PROCESS INVOICE ASSIGNMENT                            
         SPACE 1                                                                
PROINV   NTR1                                                                   
         SPACE 1                                                                
         USING TAVUD,R3                                                         
         L     R3,AIO             SAVE THE ADVICE'S USE TYPE                    
         MVI   ELCODE,TAVUELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         GOTO1 USEVAL,DMCB,(X'C0',TAVUUSE),0                                    
         DROP  R3                                                               
         SPACE 1                                                                
         XC    LASTCODE,LASTCODE                                                
         SPACE 1                                                                
         XR    R2,R2                                                            
         L     R3,AIO             COUNT THE NUMBER OF PROGRAM/                  
         MVI   ELCODE,TANPELQ     MARKET ELEMENTS ON THE ADVICE                 
         TM    TGUSSTA3,USEMTAB                                                 
         BZ    *+8                                                              
         MVI   ELCODE,TAMTELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
PINV10   BAS   RE,NEXTEL          AND SAVE THE NUMBER OF ADDITIONAL             
         BNE   PINV20             REUSE ADVICES NEEDED IN TANPCNT               
         CLI   ELCODE,TAMTELQ                                                   
         BNE   PINV15                                                           
         USING TAMTD,R3                                                         
         CLC   TAMTCODE,LASTCODE                                                
         BE    PINV10                                                           
         MVC   LASTCODE,TAMTCODE                                                
         DROP  R3                                                               
PINV15   AHI   R2,1                                                             
         B     PINV10                                                           
PINV20   STH   R2,TANPCNT                                                       
         LTR   RF,R2                                                            
         BZ    PINV30                                                           
         XR    RE,RE                                                            
         MVC   FULL,=F'20'                                                      
         TM    TGUSSTA3,USEMTAB                                                 
         BZ    *+10                                                             
         MVC   FULL,=F'28'                                                      
         D     RE,FULL                                                          
         LTR   RE,RE                                                            
         BNZ   *+8                                                              
         SHI   RF,1                                                             
         STH   RF,TANPCNT                                                       
         SPACE 1                                                                
PINV30   LHI   R2,1                                                             
         XC    INVR,INVR                                                        
         XC    INVRLAST,INVRLAST                                                
         XC    INVM,INVM                                                        
         SPACE 1                                                                
         USING TAVUD,R3                                                         
PINV40   L     R3,AIO              GET ADVICE USE DETAILS ELEMENT               
         MVI   ELCODE,TAVUELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
         SPACE 1                                                                
         CHI   R2,1                IF CHECKING FOR REUSE USE                    
         BNE   PINV50                                                           
         OC    TAVUCYC,TAVUCYC     AND REUSE CYCLE START IS PRESENT             
         BNZ   PINV60              GO PROCESS                                   
         B     PINV90                                                           
         SPACE 1                                                                
PINV50   CHI   R2,2                IF CHECKING FOR MUSIC USE                    
         BE    *+6                 AND MUSIC CYCLE START IS PRESENT             
         DC    H'00'               GO PROCESS                                   
         OC    TAVUMCYS(6),TAVUMCYS                                             
         BZ    PINV90                                                           
         DROP  R3                                                               
         SPACE                                                                  
PINV60   LA    R3,INVR                                                          
         CHI   R2,2                                                             
         BNE   *+8                                                              
         LA    R3,INVM                                                          
         SPACE                                                                  
PINV70   MVC   AIO,AIO2                  READ AGENCY RECORD INTO AIO2           
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'B4',TIAGY)                                
         BE    PINV75                                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'TLAYKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'00'                                                            
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         SPACE 1                                                                
PINV75   GOTO1 CHNINV,DMCB,=PL2'1',(R3)  UPDATE AGENCY RECORD WITH              
         BE    *+6                       NEXT INVOICE NUMBER                    
         DC    H'00'                                                            
         SPACE                                                                  
         XC    PTRBLK,PTRBLK             CLEAR PASSIVE POINTER BLOCK            
         USING TLIND,R3                                                         
         GOTO1 RECVAL,DMCB,TLINCDQ,(X'C0',0) BUILD INVOICE KEY                  
         L     R3,AIO                                                           
         MVC   TLINKEY,KEY               INITIALIZE RECORD                      
         MVC   TLINLEN,DATADISP                                                 
         XC    TLINSTAT(7),TLINSTAT                                             
         SPACE                                                                  
         USING TAIND,R4                                                         
         LA    R4,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   TAINEL,TAINELQ      BUILD INVOICE STATUS ELEMENT                 
         MVI   TAINLEN,TAINLNQ                                                  
         MVC   TAINIID,TWAORIG     SAVE CONNECT USER ID NUMBER                  
         MVC   TAINIST,TGCTSTAF    AND STAFF CODE                               
         MVC   TAINIDTE,TGTODAY1   DATE INVOICE ASSIGNED                        
         TIME  DEC                                                              
         STCM  R0,14,TAINITIM      TIME INVOICE ASSIGNED                        
         GOTO1 ADDELEM                                                          
         SPACE                                                                  
         USING TADYD,R4                                                         
         XC    ELEMENT,ELEMENT                                                  
         MVI   TADYEL,TADYELQ      BUILD DUMMY ELEMENT                          
         MVI   TADYLEN,TADYLNQ                                                  
         LA    R0,3                ADD 3 TO PREVENT OVERFLOW AFTER PAY          
PINV80   GOTO1 ADDELEM                                                          
         BCT   R0,PINV80                                                        
         DROP  R4                                                               
         SPACE                                                                  
         USING TAAID,R4                                                         
         XC    ELEMENT,ELEMENT     ADD ADVICE ASSIGNMENT ELEMENT                
         MVI   TAAIEL,TAAIELQ                                                   
         MVI   TAAILEN,TAAIALNQ                                                 
         MVC   TAAIACID,TICID                                                   
         MVC   TAAIAADV,TIADV                                                   
         STC   R2,TAAIAPTY                                                      
         GOTO1 ADDELEM                                                          
         DROP  R3                                                               
         SPACE                                                                  
         GOTO1 ADDREC                ADD NEW INVOICE RECORD                     
         GOTO1 ADDPTRS,DMCB,PTRBLK   ADD PASSIVE POINTERS                       
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
         OC    TANPCNT,TANPCNT       IF NEED MORE REUSE ADVICES                 
         BZ    PINV90                GO ADD THEM                                
         LA    R3,INVRLAST                                                      
         LH    RE,TANPCNT                                                       
         AHI   RE,-1                                                            
         STH   RE,TANPCNT                                                       
         B     PINV70                                                           
         SPACE 1                                                                
PINV90   AHI   R2,1                                                             
         CHI   R2,2                                                             
         BNH   PINV40                                                           
         SPACE 1                                                                
         MVC   AIO,AIO2                                                         
         MVI   RDUPDATE,C'Y'       SET TO READ FOR UPDATE                       
         MVC   KEY+TLDRDA-TLDRD(4),TIDSKADD  SET D/A IN GENCON KEY              
         GOTO1 GETREC              GET FILE RECORD                              
         XC    PTRBLK,PTRBLK                                                    
         GOTO1 SAVPTRS,DMCB,PTRBLK SAVE PASSIVE POINTERS                        
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 2                                                                
FLDINV   MVI   ERROR,INVALID       INVALID INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
FLDMISS  MVI   ERROR,MISSING       MISSING INPUT FIELD                          
         B     THEEND                                                           
         SPACE 1                                                                
ENDPAGE  MVC   MYMSGNO1,OKNO       MSG - HIT ENTER FOR NEXT                     
         MVI   MYMSYS,X'FF'                                                     
         OI    GENSTAT2,USGETTXT                                                
         LA    R2,SDVSELH                                                       
         B     THEEND                                                           
                                                                                
ERPPLSI  LHI   RE,ERRIAPP2         RECORD / ACTION INVALID FOR P+               
         STH   RE,MYMSGNO                                                       
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
                                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R3,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
TIMTABL  DS    0CL8                                                             
*                             CHICAGO: 9:00, 11:30, 14:00 AND 15:30             
         DC    C'1',C'1',X'000000',X'040000'                                    
         DC    C'1',C'2',X'040100',X'063000'                                    
         DC    C'1',C'3',X'063100',X'090000'                                    
         DC    C'1',C'4',X'090100',X'103000'                                    
*                                                                               
*                            NEW YORK: 10:00, 12:30, 14:30 AND 16:00            
         DC    C'2',C'1',X'000000',X'040000'                                    
         DC    C'2',C'2',X'040100',X'063000'                                    
         DC    C'2',C'3',X'063100',X'083000'                                    
         DC    C'2',C'4',X'083100',X'100000'                                    
*                                                                               
*                             ATLANTA: 9:00, 11:30, 14:00 AND 15:30             
         DC    C'4',C'1',X'000000',X'040000'                                    
         DC    C'4',C'2',X'040100',X'063000'                                    
         DC    C'4',C'3',X'063100',X'090000'                                    
         DC    C'4',C'4',X'090100',X'103000'                                    
*                                                                               
*                                  LA: 9:30, 11:30, 14:00 AND 15:30             
         DC    C'5',C'1',X'000000',X'063000'                                    
         DC    C'5',C'2',X'063100',X'083000'                                    
         DC    C'5',C'3',X'083100',X'110000'                                    
         DC    C'5',C'4',X'110100',X'123000'                                    
*                                                                               
*                            NEW YORK: 10:00, 12:30, 14:30 AND 16:00            
         DC    C'7',C'1',X'000000',X'040000'                                    
         DC    C'7',C'2',X'040100',X'063000'                                    
         DC    C'7',C'3',X'063100',X'083000'                                    
         DC    C'7',C'4',X'083100',X'100000'                                    
*                                                                               
         DC    X'00'                                                            
*              REPORT SPECS                                                     
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,56,REPORT                                                     
         SSPEC H1,73,PAGE                                                       
         SSPEC H2,56,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H1,35,C'ADVICE LIST'                                             
         SSPEC H2,35,C'-----------'                                             
         SPACE 1                                                                
         SSPEC H4,2,C'AGENCY COMMERCIAL   ADVICE L   TYPE DUE      MUS'         
         SSPEC H5,2,C'------ ----------   ------ --- ---- -------- ---'         
         SSPEC H4,51,C'DUE      SENT  RECVD  ST'                                
         SSPEC H5,51,C'-------- ----- -----  --'                                
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*STOREEQU EQU   42000                                                           
STOREEQU EQU   41000                                                            
         SPACE 1                                                                
PFTAB    DS    0C                                                               
         DC    AL1(PF19X-*,19,PFTINT+PFTCPROG,(PF19X-PF19)/KEYLNQ)              
         DC    AL1(PFTUSE)                                                      
         DC    CL3'   ',CL8'        ',CL8'PAY'                                  
PF19     DC    AL1(KEYTYCUR,L'LINAGY-1),AL2(LINAGY-LINED)                       
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCUR,L'LINCID-1),AL2(LINCID-LINED)                       
         DC    AL1(KEYTYCUR,L'LINLFT-1),AL2(LINLFT-LINED)                       
FIX1     DC    AL1(KEYTYWS+KEYTYXCM,L'LTAEQ-1),AL2(LTAEQ-DVLISTD)               
         DC    AL1(KEYTYCUR,L'LINADV-1),AL2(LINADV-LINED)                       
PF19X    EQU   *                                                                
         DC    AL1(PF21X-*,21,PFTINT+PFTCPROG,(PF21X-PF21)/KEYLNQ)              
         DC    AL1(PFTUSE)                                                      
         DC    CL3'   ',CL8'        ',CL8'PAY'                                  
PF21     DC    AL1(KEYTYCUR,L'LINAGY-1),AL2(LINAGY-LINED)                       
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCUR,L'LINCID-1),AL2(LINCID-LINED)                       
         DC    AL1(KEYTYCUR,L'LINLFT-1),AL2(LINLFT-LINED)                       
FIX2     DC    AL1(KEYTYWS+KEYTYXCM,L'LTAEQ-1),AL2(LTAEQ-DVLISTD)               
         DC    AL1(KEYTYCUR,L'LINADV-1),AL2(LINADV-LINED)                       
PF21X    EQU   *                                                                
         DC    AL1(PF22X-*,22,PFTINT+PFTCPROG,(PF22X-PF22)/KEYLNQ,0)            
         DC    CL3'V',CL8'ADVICE  ',CL8'VERIFY '                                
PF22     DC    AL1(KEYTYCUR,L'LINAGY-1),AL2(LINAGY-LINED)                       
         DC    AL1(KEYTYCUR,L'LINCID-1),AL2(LINCID-LINED)                       
         DC    AL1(KEYTYCUR,L'LINADV-1),AL2(LINADV-LINED)                       
PF22X    EQU   *                                                                
         DC    AL1(PF23X-*,23,PFTINT+PFTCPROG,(PF23X-PF23)/KEYLNQ,0)            
         DC    CL3'N',CL8'ADVICE  ',CL8'SEND   '                                
PF23     DC    AL1(KEYTYCUR,L'LINAGY-1),AL2(LINAGY-LINED)                       
         DC    AL1(KEYTYCUR,L'LINCID-1),AL2(LINCID-LINED)                       
         DC    AL1(KEYTYCUR,L'LINADV-1),AL2(LINADV-LINED)                       
PF23X    EQU   *                                                                
         DC    AL1(PF24X-*,24,PFTINT+PFTCPROG,(PF24X-PF24)/KEYLNQ,0)            
         DC    CL3'P',CL8'ADVICE  ',CL8'PULL   '                                
PF24     DC    AL1(KEYTYCUR,L'LINAGY-1),AL2(LINAGY-LINED)                       
         DC    AL1(KEYTYCUR,L'LINCID-1),AL2(LINCID-LINED)                       
         DC    AL1(KEYTYCUR,L'LINADV-1),AL2(LINADV-LINED)                       
PF24X    EQU   *                                                                
         DC    X'FF'                                                            
         EJECT                                                                  
*              DSECT FOR TIME TABLE                                             
TIMTABD  DSECT                                                                  
TIMOFF   DS    C                                                                
TIMSET   DS    C                                                                
TIMSTR   DS    PL3                                                              
TIMEND   DS    PL3                                                              
TIMTLNQ  EQU   *-TIMTABD                                                        
         EJECT                                                                  
*              DSECT COVERS LOCAL W/S                                           
         SPACE 1                                                                
DVLISTD  DSECT                                                                  
ADVPRNT  DS    A                   A(ADVICE PRINTING MODULE)                    
ADCREC   DS    A                   A(ADVICE CAST RECORD)                        
REGOTH   DS    CL1                                                              
MUSOTH   DS    CL1                                                              
LISTTYP  DS    CL1                 LIST TYPE                                    
LISTTYPR EQU   X'80'               REUSE ONLY                                   
LISTTYPS EQU   X'40'               SESSION ONLY                                 
LISTTYPG EQU   X'20'               GENERATED ADVICES                            
LISTTYPU EQU   X'10'               USE SPECIFIC                                 
LISTTYP2 EQU   X'08'               TWO USE ADVICES                              
LISTTYPX EQU   X'04'               NON-GENERATED ADVICES                        
LISTTYPN EQU   X'02'               NON-DDS CLARUS ADVICES                       
*                                                                               
TANPCNT  DS    H                   NUMBER OF PROGRAMS ON ADVICE                 
LASTCODE DS    XL6                 LAST TAMT CODE                               
*                                                                               
INVR     DS    CL6                 INVOICE NUMBER FOR REUSE                     
INVRLAST DS    CL6                 LAST INVOICE NUMBER FOR REUSE                
INVM     DS    CL6                 INVOICE NUMBER FOR MUSIC                     
*                                                                               
CNVINV   DS    XL6                                                              
*                                                                               
LTAEQ    DS    CL2                 'A=' EQUATE                                  
MYAGY    DS    CL6                 SAVED AGENCY                                 
COUNTER  DS    PL4                 RECORD COUNTER                               
RCVDATE  DS    PL3                 PREVIOUS RECEIVE DATE FOR RERUNS             
RCVSTME  DS    PL3                 PREVIOUS RECEIVE START TIME                  
RCVETME  DS    PL3                 PREVIOUS RECEIVE END TIME                    
PAIDDTE  DS    PL3                 PAID DATE                                    
COMPLETE DS    C                                                                
DUE      DS    C                                                                
PTRBLK   DS    CL(6*L'TLDRREC+1)   POINTER BLOCK                                
         ORG   PTRBLK                                                           
SAVE     DS    50F                                                              
         SPACE 3                                                                
*              DSECT TO COVER LIST LINE                                         
         SPACE 1                                                                
LINED    DSECT                                                                  
LINAGY   DS    CL6                 AGENCY                                       
         DS    CL1                                                              
LINCID   DS    CL12                COMMERCIAL ID                                
         DS    CL1                                                              
LINADV   DS    CL6                 ADVICE                                       
         DS    CL1                                                              
LINLFT   DS    CL3                 LIFT/VERSION                                 
         DS    CL1                                                              
LINTYPE  DS    CL4                 USEG (G INDICATES GENERATED ADVICE)          
         DS    CL1                                                              
LINDUE   DS    CL8                 REGULAR USE DUE DATE                         
         DS    CL1                                                              
LINTYPE2 DS    CL3                 MUSIC USE                                    
         DS    CL1                                                              
LINDUE2  DS    CL8                 MUSIC USE DUE DATE                           
         DS    CL1                                                              
LINSINF  DS    CL5                 SEND INFO                                    
         DS    CL1                                                              
LINRINF  DS    CL5                 RECEIVE INFO                                 
         DS    CL2                                                              
LINPINF  DS    CL2                 COMPLETE/INCOMPLETE                          
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR0DD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR8DD                                                       
         EJECT                                                                  
* TASYSIOD      (MUST FOLLOW LAST SCREEN)                                       
* DDGENTWA      (MUST FOLLOW LAST SCREEN)                                       
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAGENWORKD                                                                    
* TAGENEQUS                                                                     
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDMASTD                                                                       
* DDREMOTED                                                                     
* DMPRTQL                                                                       
         PRINT OFF                                                              
       ++INCLUDE TASYSIOD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDMASTD                                                        
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DMPRTQL                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'102TAGEN8D   03/20/15'                                      
         END                                                                    
