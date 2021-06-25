*          DATA SET TAGENCE    AT LEVEL 011 AS OF 06/17/15                      
*PHASE T702CEE                                                                  
         TITLE 'T702CE - AFM CONTRACT LIST'                                     
T702CE   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702CE                                                         
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
         CLI   MODE,VALKEY         FIRST TIME IN                                
         BNE   AF30                                                             
         TM    TRNSTAT,OKINTPFK    IF RETURNING FROM SELECT (DISPLAY)           
         BNO   *+8                                                              
         MVI   LISTSW,C'T'         SET TO RE-DISPLAY SAME PAGE                  
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 1                                                                
AF30     CLI   MODE,LISTRECS       LIST RECORDS ON SCREEN                       
         BE    AF40                                                             
         CLI   MODE,PRINTREP       PRINT LIST                                   
         BNE   AFX                                                              
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         XC    KEY,KEY             ENSURE START AT TOP OF LIST                  
         MVI   LISTSW,C'F'                                                      
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,HDHOOK                                                        
         ST    R2,HEADHOOK                                                      
         LA    R2,SAFDATAH         POINT TO 1ST DATA LINE                       
         ST    R2,ATHISLST         AND SET AS FIRST LIST LINE                   
AF40     BAS   RE,LREC             GO LIST THE RECORDS                          
         SPACE 1                                                                
AFX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE THE KEY                                      
         SPACE 1                                                                
VKEY     NTR1                                                                   
         MVI   KEYCHG,0                                                         
         TM    SAFAGYH+4,X'80'     VALIDATE AGENCY                              
         BO    VK10                                                             
         TM    SAFCLIH+4,X'80'     VALIDATE CLIENT                              
         BO    VK10                                                             
         TM    SAFSTRH+4,X'80'     VALIDATE CONTRACT NUMBER                     
         BO    VK10                                                             
         MVI   LISTSW,C'N'                                                      
         B     VK20                                                             
VK10     MVI   KEYCHG,1                                                         
         XC    AFMKEY,AFMKEY                                                    
         MVI   AFMFLAG,0                                                        
         MVI   TGAYSTA7,0                                                       
         XC    TGAGY,TGAGY                                                      
*                                                                               
VK20     CLI   KEYCHG,1            IF USER CHANGED ANYTHING                     
         BE    VK25                RE-VALIDATE EVERYTHING                       
         TM    SAFAGYH+4,X'20'                                                  
         BO    VK30                                                             
VK25     GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',SAFAGYH)                              
         MVC   TIFAGY,TGAGY                                                     
         GOTO1 RAVPPLSA,DMCB,0                                                  
         JNE   ERPPLSI                                                          
         NI    SAFSTRH+4,X'DF'                                                  
*                                                                               
VK30     CLI   KEYCHG,1            IF USER CHANGED ANYTHING                     
         BO    *+12                RE-VALIDATE EVERYTHING                       
         TM    SAFCLIH+4,X'20'                                                  
         BO    VK40                                                             
         XC    TIFCLI,TIFCLI                                                    
         CLI   SAFCLIH+5,0         INPUT NOT REQUIRED                           
         BE    VK40                                                             
         GOTO1 RECVAL,DMCB,TLCLCDQ,SAFCLIH                                      
         MVC   TIFCLI,TGCLI                                                     
         NI    SAFSTRH+4,X'DF'                                                  
*                                                                               
VK40     LA    R2,SAFSTRH          START AT CONTRACT NUMBER                     
         CLI   KEYCHG,1            IF USER CHANGED ANYTHING                     
         BO    *+12                RE-VALIDATE EVERYTHING                       
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         XC    TIQSTART,TIQSTART   START FROM BEGINING                          
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   TIQSTART(L'SAFSTR),8(R2)                                         
         OI    4(R2),X'20'         SET PREVIOUSLY VALIDATED                     
*                                                                               
         XC    KEY,KEY             KEY FIELD CHANGED - START OVER               
         MVI   LISTSW,C'F'                                                      
         XC    TGCLI,TGCLI         CLEAR SAVED CLIENT                           
*                                                                               
VKX      MVI   NLISTS,8            SET N'LIST LINES                             
         OI    GLSTSTAT,RETEXTRA   SET OKAY TO RETURN EXTRA FOR EOL             
         OI    GLSTSTAT,APPLCDSP   SET I WILL DISPLAY THE LIST                  
         MVC   LLIST,=Y(LINLNQ)    SET L'LIST LINE                              
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE CONTROLS RECORD LISTING                                  
*                                                                               
LREC     NTR1                                                                   
         TWAXC SAFSELH,SAFLSTH,PROT=Y  CLEAR THE SCREEN FOR NEW LIST            
         SPACE 1                                                                
         OC    AFMKEY,AFMKEY       IF IN THE MIDDLE OF SUBREAD                  
         BZ    LR10                                                             
         BAS   RE,GETACON          GO FINISH IT                                 
         SPACE 1                                                                
LR10     LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS                                                
         MVC   TIUSERID,TWAORIG    SET UP SOME FIELDS FOR SYSIO                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVI   TIREAD,TLCOACDQ     SET TO READ COMML AFM CONTRACT PTRS.         
         MVI   TISUBRD,0                                                        
         SPACE 1                                                                
         GOTO1 PGCNTL,DMCB,PGTBL,TIKEY,TIQSKEY  SET KEY                         
         SPACE 1                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD   OFF TO SYSIO TO READ RECORDS             
         SPACE 1                                                                
         XC    TIQSKEY,TIQSKEY     CLEAR CONTINUE KEY                           
         XC    TIQSTART,TIQSTART                                                
         NI    SAFAGYH+4,X'FF'-X'20'   END OF LIST - REVALIDATE KEY             
         NI    SAFCLIH+4,X'FF'-X'20'                                            
         NI    SAFSTRH+4,X'FF'-X'20'                                            
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         CP    COUNTER,=P'0'       IF ANYTHING REPORTED                         
         BE    LRX                                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         EDIT  COUNTER,(5,P),COMMAS=YES,ALIGN=LEFT                              
         LR    R1,R0                                                            
         LA    R1,P+1(R1)                                                       
         MVC   0(18,R1),=C'COMMERCIAL RECORDS'                                  
         GOTO1 SPOOL,DMCB,(R8)                                                  
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
*               PROCESS SYSIO RECORDS                                           
*                                                                               
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   LRHX                                                             
         CLI   MODE,PRINTREP       SKIP IF PRINTING                             
         BE    LRH40                                                            
*                                                                               
         CLC   LISTNUM,NLISTS      IF ALREADY DISPLAYED MAX                     
         BE    LHR10               GO RIGHT BACK TO LISTMON                     
         TM    AFMFLAG,AFMFLCON    AFM CONTRACT COMMERCIAL?                     
         BO    LRH20                                                            
         BAS   RE,DISPLAY          ELSE DISPLAY TO SCREEN                       
         MVC   DMDSKADD,TIDSKADD   SET DISK ADDRESS FOR LISTMON                 
LHR10    GOTO1 LISTMON                                                          
*                                                                               
         TM    AFMFLAG,AFMFLCON    AFM CONTRACT COMMERCIAL?                     
         BZ    LRHX                                                             
         CLC   LISTNUM,NLISTS      DISPLAYED MAX ON SCREEN?                     
         BNE   LRH20                                                            
         MVC   DMDSKADD,TIDSKADD   SET DISK ADDRESS FOR LISTMON                 
         GOTO1 LISTMON                                                          
LRH20    BAS   RE,GETACON          GET AFM CONTRACT COMMERCIALS                 
*                                                                               
         MVC   KEY,TIKEY           RE-READ SYSIO'S KEY                          
         GOTO1 HIGH                                                             
         B     LRHX                                                             
*                                                                               
LRH40    BAS   RE,DISPLAY          IF PRINTING, DISPLAY TO SCREEN               
         MVI   BYTE,C'P'                                                        
         GOTO1 PRTSCRN,DMCB,ATHISLST,ATHISEND,P-5    PRINT IT                   
         GOTO1 FLDVAL,DMCB,(X'01',ATHISLST),ATHISEND CLEAR FROM SCREEN          
         AP    COUNTER,=P'1'       ADD TO COUNTER                               
*                                                                               
LRHX     MVC   KEY,TIKEY           RE-READ SYSIO'S KEY                          
         GOTO1 HIGH                                                             
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY RECORD ROUTINE                                           
*                                                                               
DISPLAY  NTR1                                                                   
         L     R2,ATHISLST         R2 = A(SCREEN LINE)                          
         USING LINED,R2            ESTABLISH SCREEN LINE                        
*                                                                               
         LR    R3,R2                                                            
         AH    R3,LLIST                                                         
         ST    R3,ATHISEND         SAVE A(END OF THIS LIST LINE)                
*                                                                               
         MVI   AFMFLAG,0                                                        
*                                                                               
         LA    R3,TIKEY            R3 = A(POINTER)                              
         USING TLCOPD,R3           ESTABLISH PASSIVE POINTERS                   
*                                                                               
         CLI   TLCOPCD,TLCOTCDQ    SKIP IF PASSIVE TO ANOTHER CONTRACT          
         BE    DISPCON                                                          
*                                                                               
         MVC   LINCON,TLCOACON     AFM CONTRACT NUMBER                          
*                                                                               
         MVC   LINCID,TICID        COMMERCIAL ID                                
         MVC   LINCLI,TICLI        CLIENT CODE                                  
         MVC   LINTITLE,TINAME     COMMERCIAL TITLE                             
*                                                                               
         L     R3,TIAREC                                                        
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS EL.                   
         BAS   RE,GETEL                                                         
         BNE   DISP30                                                           
         USING TACOD,R3                                                         
*                                                                               
         CLI   TACOTYPE,CTYMUS     IF COMM'L TYPE IS MUSIC                      
         BNE   DISP30                                                           
*                                                                               
         OI    AFMFLAG,AFMFLCON                                                 
         MVC   LINRFU,=CL2'U='     DISPLAY REMAINING FREE USES                  
         EDIT  TACORUSE,LINRUSE,ZERO=NOBLANK                                    
*                                                                               
         B     DISP30                                                           
*                                                                               
*        DISPLAY SECONDARY CONTRACT                                             
*                                                                               
DISPCON  DS    0H                  DISPLAY SECONDARY CONTRACT                   
*                                                                               
*        FIND COMMERCIAL DETAILS ELEMENT                                        
*                                                                               
         L     R3,TIAMAIN          POINT TO SAVED MASTER CONTRACT               
         MVI   ELCODE,TACOELQ      GET COMMERCIAL DETAILS                       
         BAS   RE,GETEL                                                         
*                                                                               
         USING TACOD,R3            ESTABLISH DETAILS ELEMENT                    
*                                                                               
         MVC   LINCON,TACOCID      DISPLAY CONTRACT NUMBER                      
*                                                                               
*        FIND MUSIC DETAILS ELEMENT FOR TRACK                                   
*                                                                               
         L     R3,TIAMAIN          POINT TO SAVED MASTER CONTRACT               
         MVI   ELCODE,TAMCELQ      GET MUSIC CONTRACT DETAILS                   
         BAS   RE,GETEL                                                         
*                                                                               
DISPCONL DS    0H                                                               
*                                                                               
         USING TAMCD,R3            ESTABLISH MUSIC CONTRACT DETAILS             
*                                                                               
         BNE   DISPCOND            DONE AT END OF ELEMENTS                      
*                                                                               
*        MATCH ON TRACK NUMBER                                                  
*                                                                               
         CLC   TAMCTRK,(TLCOTTRK-TLCOPD)+TIKEY                                  
         BE    DISPCONF                                                         
*                                                                               
DISPCONC DS    0H                                                               
*                                                                               
         BAS   RE,NEXTEL                                                        
*                                                                               
         B     DISPCONL                                                         
*                                                                               
DISPCONF DS    0H                                                               
*                                                                               
         EDIT  (B2,TAMCLLEN),LINTLEN,0    DISPLAY TRACK LENGTH                  
         MVC   LINTID,TAMCTRK      DISPLAY TRACK ID                             
*                                                                               
DISPCOND DS    0H                                                               
*                                                                               
         MVC   LINCID,TICID        COMMERCIAL ID                                
         MVC   LINCLI,TICLI        CLIENT CODE                                  
*                                                                               
         LA    R3,IO               POINT TO TRACK RECORD                        
         MVI   ELCODE,TANAELQ      GET NAME ELEMENT                             
         BAS   RE,GETEL                                                         
         BNE   DISP05              NONE FOUND                                   
*                                                                               
         USING TANAD,R3            ESTABLISH NAME ELEMENT                       
*                                                                               
         LLC   RF,TANALEN          GET NAME ELM LENGTH                          
         SHI   RF,TANANAME-TANAD   DECREMENT BY HEADER LENGTH                   
         BZ    DISP05                 NO NAME                                   
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LINTITLE(0),TANANAME DISPLAY NAME                                
*                                                                               
DISP05   DS    0H                                                               
*                                                                               
         L     R3,TIAREC                                                        
         MVI   ELCODE,TAMCELQ      GET CONTRACT DETAILS EL.                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DISP20   BAS   RE,NEXTEL                                                        
*                                                                               
         BNE   DISP30                                                           
*                                                                               
         USING TAMCD,R3                                                         
*                                                                               
         CLC   TAMCCON,LINCON      MATCH WITH CORRECT CONTRACT                  
         BNE   DISP20                                                           
*                                                                               
         MVC   LINTID,TAMCTRK      DISPLAY TRACK ID                             
         EDIT  TAMCLLEN,LINTLEN    DISPLAY TRACK LENGTH                         
*                                                                               
DISP30   MVC   AIO,TIAREC                                                       
         MVI   ELCODE,TACSELQ      SET TO GET MUSIC STUDIO ELEMENT              
*                                                                               
         GOTO1 GETL,DMCB,(1,=AL1(TACSTYPM))                                     
         MVC   AIO,AIO1                                                         
         BNE   DISP40                                                           
*                                                                               
         L     R4,TGELEM                                                        
         USING TACSD,R4                                                         
*                                                                               
         MVC   LINSTUD,TACSSTUD           DISPLAY STUDIO NAME                   
         GOTO1 DATCON,DMCB,(1,TACSDATE),(8,LINDATE)  AND DATE                   
*                                                                               
DISP40   CLC   TGCLI,TICLI         IF CLIENT HAS CHANGED                        
         BE    DISP60                                                           
         MVC   TGCLI,TICLI         SET GLOBAL CLIENT                            
         GOTO1 XNAME,DMCB,(X'80',TLCLCDQ),CLINAME,TIKEY GET CLIENT NAME         
*                                                                               
DISP60   MVC   LINCLIN,CLINAME     DISPLAY CLIENT NAME                          
         SPACE 1                                                                
         MVI   BYTE,UMUS           SET TO LOOK UP MOST RECENT MUS PMT           
         BAS   RE,GETUSE                                                        
         BE    *+12                                                             
         MVI   BYTE,UFMU           NOT FOUND - TRY FIRST MUSIC USE              
         BAS   RE,GETUSE                                                        
         SPACE 1                                                                
         MVC   KEY,TIKEY           RE-READ SYSIO'S KEY                          
         GOTO1 HIGH                                                             
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE LOOKS FOR MUST RECENT USE AND DISPLAYS INFO              
         SPACE 1                                                                
*                                  BYTE = USE TYPE EQUATE                       
         USING LINED,R2            R2 = A(SCREEN LINE)                          
GETUSE   NTR1                                                                   
         GOTO1 USEVAL,DMCB,(X'C0',BYTE)  SET GLOBALS                            
         SPACE 1                                                                
         XC    KEY,KEY             SET TO BUILD KEY                             
         LA    R3,KEY                                                           
         USING TLUHD,R3                                                         
         MVI   TLUHCD,TLUHCDQ      RECORD CODE                                  
         MVC   TLUHCOM,TICOM       INTERNAL COMMERCIAL NUMBER                   
         MVC   TLUHUSE,TGUSCDE     USE CODE                                     
         SPACE 1                                                                
         GOTO1 HIGH                READ HIGH                                    
         SPACE 1                                                                
         CLC   TLUHKEY(TLUHINV-TLUHD),KEYSAVE                                   
         BNE   NO                  RETURN CC NE IF NOT FOUND                    
         SPACE 1                                                                
         XC    TLUHINV,HEXFFS                                                   
         GOTO1 TINVCON,DMCB,TLUHINV,LININV,DATCON  DISPLAY INVOICE NO.          
         SPACE 1                                                                
         GOTO1 GETREC              GET THE RECORD                               
         L     R3,AIO                                                           
         MVI   ELCODE,TAUHELQ      GET USAGE HISTORY DETAILS EL.                
         BAS   RE,GETEL                                                         
         BNE   GUSX                                                             
         USING TAUHD,R3                                                         
         GOTO1 USEVAL,DMCB,TGUSCDE,TAUHTYPE  SET GLOBALS FOR TYPE               
         SPACE 1                                                                
         MVC   LINUSEN,TGUSNAME    DISPLAY USE NAME                             
         SPACE 1                                                                
GUSX     B     YES                                                              
         EJECT                                                                  
*              ROUTINE LOOKS FOR AFM CONTRACT COMMERCIALS                       
         SPACE 1                                                                
*                                  BYTE = USE TYPE EQUATE                       
GETACON  NTR1                                                                   
         L     R2,ATHISLST         R2 = A(SCREEN LINE)                          
         USING LINED,R2            R2 = A(SCREEN LINE)                          
*                                                                               
         OC    AFMKEY,AFMKEY       MIDDLE OF SUBREADS FOR COMMERCIALS           
         BZ    GETAC02             UNDER AFM CONTRACT?                          
         XC    KEY,KEY                                                          
         MVC   KEY(L'AFMKEY),AFMKEY                                             
         GOTO1 HIGH                                                             
         B     GETACSEQ                                                         
*                                                                               
GETAC02  XC    KEY,KEY             SET TO BUILD KEY                             
         LA    R3,KEY                                                           
         USING TLCOPD,R3                                                        
         MVI   TLCOPCD,TLCOTCDQ                                                 
         MVC   TLCOTMCO,TICOM      INTERNAL COMMERCIAL NUMBER                   
         GOTO1 HIGH                READ HIGH                                    
         B     GETAC04                                                          
*                                                                               
GETACSEQ GOTO1 SEQ                                                              
GETAC04  CLC   KEY(TLCOTTRK-TLCOPD),KEYSAVE                                     
         BNE   GETACONX            RETURN CC NE IF NOT FOUND                    
         L     R2,ATHISLST         R2 = A(SCREEN LINE)                          
                                                                                
         LA    RE,SAFPFSH          RETURN IF AT END OF SCREEN                   
         CR    R2,RE                                                            
         BNL   XIT                                                              
*                                                                               
         LA    R3,KEY                                                           
         USING TLCOPD,R3                                                        
         CLI   TLCOTVER,1                                                       
         BH    GETACSEQ                                                         
         MVC   LINTID,TLCOTTRK     TRACK                                        
*                                                                               
         MVC   AFMKEY,KEY                                                       
         GOTO1 GETREC              GET THE RECORD                               
*                                                                               
         LA    R3,TIKEY            R3 = A(POINTER)                              
         USING TLCOPD,R3                                                        
         MVC   LINCON,TLCOACON     AFM CONTRACT NUMBER                          
*                                                                               
         L     R3,AIO                                                           
         USING TLCOD,R3                                                         
         MVC   LINCLI,TLCOCLI      CLIENT                                       
*                                                                               
         L     R3,AIO                                                           
         USING TAMCD,R3                                                         
         MVI   ELCODE,TAMCELQ      COMMERCIAL DETAILS ELEM                      
         BAS   RE,GETEL                                                         
         B     *+8                                                              
GETAC06  BAS   RE,NEXTEL                                                        
         BNE   GETAC08                                                          
         CLC   TAMCTRK,LINTID      LINTID FILLED WITH TLCOTTRK                  
         BNE   GETAC06                                                          
         EDIT  (B2,TAMCLLEN),LINTLEN,0    DISPLAY TRACK LENGTH                  
*                                                                               
GETAC08  L     R3,AIO                                                           
         USING TACOD,R3                                                         
         MVI   ELCODE,TACOELQ      COMMERCIAL DETAILS ELEM                      
         BAS   RE,GETEL                                                         
         BNE   GETAC10                                                          
         MVC   LINCID,TACOCID      COMMERCIAL ID                                
*                                                                               
GETAC10  L     R3,AIO                                                           
         USING TANAD,R3                                                         
         MVI   ELCODE,TANAELQ      NAME ELEM                                    
         BAS   RE,GETEL                                                         
         BNE   GETAC12                                                          
         ZIC   RF,TANALEN                                                       
         SHI   RF,3                ELEM CODE + ELEM LEN + EXMVC                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   LINTITLE(0),TANANAME   NAME                                      
*                                                                               
GETAC12  L     R3,AIO                                                           
         USING TACSD,R3                                                         
         MVI   ELCODE,TACSELQ      NAME ELEM                                    
         BAS   RE,GETEL                                                         
         B     *+8                                                              
GETAC14  BAS   RE,NEXTEL                                                        
         BNE   GETAC16                                                          
         CLI   TACSTYPE,TACSTYPM                                                
         BNE   GETAC14                                                          
         GOTO1 DATCON,DMCB,(1,TACSDATE),(8,LINDATE)  DATE                       
*                                                                               
GETAC16  DS    0H                                                               
         MVC   DMDSKADD,TIDSKADD   SET DISK ADDRESS FOR LISTMON                 
         GOTO1 LISTMON                                                          
         B     GETACSEQ                                                         
*                                                                               
GETACONX DS    0H                                                               
         XC    AFMKEY,AFMKEY                                                    
         MVI   AFMFLAG,0                                                        
         B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              HEADHOOK ROUTINE                                                 
         SPACE 2                                                                
HDHOOK   NTR1                                                                   
         MVI   BYTE,C'H'           PRINT HEADLINES                              
         GOTO1 PRTSCRN,DMCB,CONTAGH,SAFHDH,H4-1                                 
         GOTO1 (RF),(R1),SAFHDH,SAFSELH,H6-5                                    
         MVC   H6-5(5),SPACES                                                   
         MVI   BYTE,C'P'                                                        
         B     XIT                                                              
         EJECT                                                                  
*              EXITS, ETC.                                                      
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     END                                                              
                                                                                
END      GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
         GETEL R3,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
HEXFFS   DC    6X'FF'                                                           
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*              PFKEY TABLE                                                      
         SPACE 2                                                                
PFTAB    DS    0C                                                               
*                                                                               
         DC    AL1(PF10X-*,10,0,(PF10X-PF10)/KEYLNQ,0)                          
         DC    CL3'CA',CL8'CAST    ',CL8'LIST'                                  
PF10     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'LINCID-1),AL2(LINCID-LINLINE)                     
PF10X    EQU   *                                                                
*                                                                               
         DC    AL1(PF11X-*,11,0,(PF11X-PF11)/KEYLNQ,0)                          
         DC    CL3'HI',CL8'HISTORY ',CL8'LIST'                                  
PF11     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'LINCID-1),AL2(LINCID-LINLINE)                     
PF11X    EQU   *                                                                
*                                                                               
         DC    AL1(PF13X-*,13,0,(PF13X-PF13)/KEYLNQ,0)                          
         DC    CL3'CH',CL8'CHECK   ',CL8'LIST   '                               
PF13     DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYCUR,L'LININV-1),AL2(LININV-LINLINE)                     
         SPACE                                                                  
PF13X    EQU   *                                                                
*                                                                               
         DC    AL1(PF14X-*,14,0,(PF14X-PF14)/KEYLNQ,0)                          
         DC    CL3'HD',CL8'HISTORY ',CL8'DISPLAY'                               
PF14     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'LININV-1),AL2(LININV-LINLINE)                     
         SPACE                                                                  
PF14X    EQU   *                                                                
*                                                                               
         DC    AL1(PF22X-*,22,PFTINT+PFTCPROG)                                  
         DC    AL1((PF22X-PF22)/KEYLNQ,0)                                       
         DC    CL3'S',CL8'COMM    ',CL8'DISP'                                   
PF22     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'LINCID-1),AL2(LINCID-LINLINE)                     
         SPACE                                                                  
PF22X    EQU   *                                                                
*                                                                               
         DC    AL1(PF23X-*,23,PFTINT+PFTCPROG)                                  
         DC    AL1((PF23X-PF23)/KEYLNQ,0)                                       
         DC    CL3'C',CL8'COMM    ',CL8'CHA'                                    
PF23     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'LINCID-1),AL2(LINCID-LINLINE)                     
         SPACE                                                                  
PF23X    EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*              REPORT SPECS                                                     
         SPACE 2                                                                
MYSPECS  DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,56,REPORT                                                     
         SSPEC H1,73,PAGE                                                       
         SSPEC H2,56,REQUESTOR                                                  
         SPACE 1                                                                
         SSPEC H1,31,C'AFM CONTRACT LIST'                                       
         SSPEC H2,31,C'-----------------'                                       
         SPACE 1                                                                
         SSPEC H7,1,C'----------   ----------       ------'                     
         SSPEC H7,48,C'-----------------     -----'                             
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER LIST LINE                                         
         SPACE 1                                                                
LINED    DSECT                                                                  
         DS    CL8                                                              
LINLINE  DS    CL(L'SAFDATA)                                                    
         ORG   LINLINE                                                          
LINCON   DS    CL12                CONTRACT NUMBER                              
         DS    CL1                                                              
LINCID   DS    CL12                COMMERCIAL ID                                
         DS    CL5                                                              
LINCLI   DS    CL6                 CLIENT CODE                                  
         DS    CL11                                                             
LINUSEN  DS    CL16                MOST RECENT MUS USE NAME                     
         DS    CL6                                                              
LININV   DS    CL6                 CORRESPONDING INVOICE NUMBER                 
         DS    CL8                                                              
LINLINE2 DS    CL(L'SAFDATA)                                                    
         ORG   LINLINE2                                                         
         DS    CL3                                                              
LINRFU   DS    CL2                 U=                                           
LINRUSE  DS    CL1                 REMAINING FREE USES                          
         DS    CL1                                                              
LINTLEN  DS    CL3                 TRACK LENGTH                                 
         DS    CL1                                                              
LINTID   DS    CL1                 TRACK ID                                     
         DS    CL1                                                              
LINTITLE DS    CL16                COMMERCIAL TITLE                             
         DS    CL1                                                              
LINCLIN  DS    CL16                CLIENT NAME                                  
         DS    CL1                                                              
LINSTUD  DS    CL12                MUSIC STUDIO NAME                            
         DS    CL1                                                              
LINDATE  DS    CL8                 MUSIC DATE                                   
         ORG                                                                    
LINLNQ   EQU   *-LINED                                                          
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRCED                                                       
         SPACE 3                                                                
COUNTER  DS    PL4                 RECORD COUNTER                               
ATHISEND DS    A                                                                
CLINAME  DS    CL16                CLIENT NAME                                  
KEYCHG   DS    XL1                 KEY CHANGE FLAG                              
AFMFLAG  DS    XL1                                                              
AFMFLCON EQU   X'80'               AFM CONTRACT COMMERCIAL                      
AFMKEY   DS    XL32                                                             
         EJECT                                                                  
* TASYSIOD      (MUST FOLLOW LAST SCREEN)                                       
* DDGENTWA      (MUST FOLLOW LAST SCREEN)                                       
* TASYSDSECT                                                                    
* TASYSEQUS                                                                     
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* TAGENWORKD    (MUST BE LAST)                                                  
         PRINT OFF                                                              
       ++INCLUDE TASYSIOD                                                       
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE TAGENWORKD                                                     
         PRINT ON                                                               
         SPACE 2                                                                
         ORG   TWAHOLE                                                          
PGTBL    DS    CL(16*L'TLRCKEY)    SAVED KEYS FOR PAGE CONTROL RTN.             
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011TAGENCE   06/17/15'                                      
         END                                                                    
