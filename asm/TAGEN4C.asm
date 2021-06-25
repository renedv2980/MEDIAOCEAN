*          DATA SET TAGEN4C    AT LEVEL 075 AS OF 07/20/12                      
*PHASE T7024CC,*                                                                
         TITLE 'T7024C - ESTIMATE RECORD LIST'                                  
T7024C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 ESTRECL,T7024C,CLEAR=YES                                         
         LR    R3,RC                                                            
         SPACE 1                                                                
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         SPACE 3                                                                
         GOTO1 INITIAL,DMCB,PFTAB  INITIALIZE                                   
         ST    R3,AESTREC          A(FULL ESTIMATE RECORD)                      
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 3                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BNE   *+12                                                             
         BAS   RE,VKEY                                                          
         B     XIT                                                              
         SPACE 3                                                                
         CLI   MODE,LISTRECS                                                    
         BNE   MAIN20                                                           
         MVC   LISTAR,SPACES                                                    
         LA    R2,LISTAR                                                        
         B     MAIN40                                                           
         SPACE 3                                                                
MAIN20   CLI   MODE,PRINTREP                                                    
         BNE   XIT                                                              
         XC    KEY,KEY             INSURE START REPORT FROM BEG OF LIST         
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         LA    R2,MYSPECS                                                       
         ST    R2,SPECS                                                         
         LA    R2,P+1                                                           
         SPACE 2                                                                
MAIN40   BAS   RE,LREC                                                          
         B     XIT                                                              
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY                                          
         SPACE 1                                                                
VKEY     NTR1                                                                   
         LA    R2,SESAGYH          R2=A(AGENCY FIELD)                           
         CLI   SCRSTAT,0           IF SCREEN CHANGE                             
         BNE   VK5                                                              
         TM    4(R2),X'20'         OR NOT PREVIOUSLY VALIDATED                  
         BO    VK10                                                             
VK5      NI    SESSTRH+4,X'DF'                                                  
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'20',(R2)) VALIDATE AGENCY                 
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
         SPACE 1                                                                
VK10     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SESSTRH          R2=A(ESTIMATE START FIELD)                   
         TM    4(R2),X'20'                                                      
         BO    VK20                                                             
         NI    SESCLIH+4,X'DF'                                                  
         SPACE 1                                                                
VK20     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SESCLIH          R2=A(CLIENT FIELD)                           
         TM    4(R2),X'20'                                                      
         BO    VK30                                                             
         NI    SESPRDH+4,X'DF'                                                  
         XC    TGCLI,TGCLI                                                      
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         GOTO1 RECVAL,DMCB,TLCLCDQ,(R2)  VALIDATE IT                            
*                                                                               
VK30     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SESPRDH          R2=A(PRODUCT FIELD)                          
         TM    4(R2),X'20'                                                      
         BO    VK40                                                             
         NI    SESRDTEH+4,X'DF'                                                 
         XC    TGPRD,TGPRD                                                      
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         CLI   SESCLIH+5,0         IF NO CLIENT INPUT                           
         BNE   VK35                                                             
         MVC   TGPRD,8(R2)         TAKE UNVALIDATED INPUT                       
         OC    TGPRD,SPACES                                                     
         B     VK40                                                             
VK35     GOTO1 RECVAL,DMCB,TLPRCDQ,(R2)  ELSE, VALIDATE IT                      
         SPACE 1                                                                
VK40     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SESRDTEH         R2=A(REPORT DATE)                            
         TM    4(R2),X'20'                                                      
         BO    VK50                                                             
         NI    SESCDTEH+4,X'DF'                                                 
         XC    RPTINF,RPTINF                                                    
         CLI   5(R2),0                                                          
         BE    VK50                                                             
         GOTO1 ANY                                                              
         MVC   BLOCK,SPACES                                                     
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK(0),WORK                                                    
         LA    R2,BLOCK                                                         
         CLI   0(R2),C'-'                                                       
         BNE   *+14                                                             
         MVC   RPTTYPE,0(R2)                                                    
         LA    R2,1(R2)                                                         
         GOTO1 DTVAL,DMCB,(X'40',RPTDATE)                                       
         BNE   INVRDATE                                                         
         GOTO1 DATCON,DMCB,(1,RPTDATE),(8,RPTDATED)                             
*                                                                               
VK50     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SESCDTEH         R2=A(CHANGE DATE)                            
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         XC    CHGINF,CHGINF                                                    
         CLI   5(R2),0                                                          
         BE    VK60                                                             
         GOTO1 ANY                                                              
*                                                                               
         MVC   BLOCK,SPACES                                                     
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BLOCK(0),WORK                                                    
         LA    R2,BLOCK                                                         
         CLI   0(R2),C'-'                                                       
         BNE   *+14                                                             
         MVC   CHGTYPE,0(R2)                                                    
         LA    R2,1(R2)                                                         
         GOTO1 DTVAL,DMCB,(X'40',CHGDATE)                                       
         BNE   INVCDATE                                                         
         GOTO1 DATCON,DMCB,(1,CHGDATE),(8,CHGDATED)                             
*                                                                               
VK60     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         BAS   RE,INIT             AND INITIALIZE FOR SYSIO                     
         SPACE 1                                                                
VKX      B     XIT                                                              
         EJECT                                                                  
*              INITIALIZE FOR SYSIO                                             
         SPACE 1                                                                
INIT     NTR1                                                                   
         XC    KEY,KEY             INITIALIZE KEY                               
         LH    RF,=AL2(TIEND-TASYSIOD)                                          
         XCEFL TASYSIOD,(RF)                                                    
         MVC   TIUSERID,TWAORIG    SET UP NECCESSARY DATA FROM SCREEN           
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVC   TIFAGY,TGAGY        SET AGENCY                                   
         MVC   TIQSTART,SESSTR     AND START ESTIMATE                           
         MVI   TIREAD,TLESCDQ      SET FOR READS                                
*        OI    TIQFLAG2,TIQFNLIM                                                
INITX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO CONTROL LISTING RECORDS                               
         SPACE 1                                                                
*                                  R2=A(SCREEN LINE OR P)                       
LREC     NTR1                                                                   
         LA    R0,LRHOOK           SET HOOK TO SYSIO                            
         ST    R0,TIHOOK                                                        
         MVC   TIACOMFC,ACOMFACS   PASS SOME MORE THINGS                        
         MVC   TIKHOOK,SETLSTK                                                  
         MVI   NLISTS,15           SET N'LIST LINES                             
         OI    GLSTSTAT,RETEXTRA   SET OKAY TO RETURN EXTRA FOR EOL             
         SPACE 1                                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD  PASS CONTROL TO SYSIO FOR LIST            
         SPACE 1                                                                
         CLI   MODE,PRINTREP       IF WE'RE SPOOLING                            
         BNE   LRX                                                              
         CP    COUNTER,=P'0'       AND WE REPORTED SOMETHING                    
         BE    LRX                                                              
         MVC   P,SPACES                                                         
         BAS   RE,PRNTIT           SKIP A LINE ...                              
         SPACE 1                                                                
         EDIT  COUNTER,(8,P+1),COMMAS=YES,ALIGN=LEFT  AND PRINT TOTALS          
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(16,R1),=C'ESTIMATE RECORDS'                                    
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
         TM    WHEN,X'40'          IF SPOOLING NOW                              
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         SPACE 1                                                                
LRX      B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PROCESS SYSIO HOOKS                                   
         SPACE 1                                                                
         USING LINED,R2            R2=A(DISPLAY LINE)                           
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   XIT                                                              
         XC    LINED(LINLNQ),LINED                                              
         L     R4,TIAREC           R4=A(RECORD)                                 
         USING TLESD,R4                                                         
         CLI   TLESSEQ,0           IF PRIMARY RECORD                            
         BNE   XIT                                                              
         SPACE 1                                                                
         BAS   RE,GETEREC          GET FULL ESTIMATE RECORD                     
         MVC   AIO,AESTREC         INTO AESTREC                                 
         SPACE 1                                                                
         L     R4,AIO              R4=A(FULL ESTIMATE RECORD)                   
*        BAS   RE,CHKLIM           CHECK AGENCY/CLIENT LIMITS                   
*        BNE   LRHX                                                             
         SPACE 1                                                                
         BAS   RE,FILTCLI          FILTER CLIENT                                
         BNE   LRHX                                                             
         BAS   RE,FILTPRD          FILTER PRODUCT                               
         BNE   LRHX                                                             
         SPACE 1                                                                
         MVC   LINEST,TLESEST          DISPLAY ESTIMATE CODE                    
         GOTO1 CHAROUT,DMCB,TANAELQ,0  EXTRACT NAME FROM RECORD                 
         MVC   LINNAME,TGNAME          AND MOVE TO OUTPUT AREA                  
         SPACE 1                                                                
         BAS   RE,DISPDTS          DISPLAY LAST CHANGED&REPORTED DATES          
         BAS   RE,FILTDTS          FILTER DATES                                 
         BNE   LRHX                                                             
         SPACE 1                                                                
         CLI   MODE,PRINTREP       IF SPOOLING REPORT                           
         BNE   LRHK30                                                           
         GOTO1 CATCHIOS            INSURE DON'T DO TOO MANY I/O'S               
         BAS   RE,PRNTIT           PRINT REPORT                                 
         AP    COUNTER,=P'1'                                                    
         B     LRHX                                                             
         SPACE 1                                                                
LRHK30   MVC   DMDSKADD,TIDSKADD   ELSE SET DISK ADDRESS                        
         GOTO1 LISTMON             AND MOVE DISPLAY LINE TO SCREEN              
         SPACE 1                                                                
LRHX     XC    KEY,KEY             RE-READ SYSIO'S KEY                          
         MVC   KEY(L'TIKEY),TIKEY                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'TIKEY),KEYSAVE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         B     XIT                 RETURN TO SYSIO                              
         EJECT                                                                  
*              ROUTINE TO GET AN ESTIMATE RECORD                                
*                                  RETURNS WITH RECORD IN AESTREC               
         SPACE 1                                                                
GETEREC  NTR1                                                                   
         MVC   KEY,TIKEY           GET ESTIMATE RECORD                          
         GOTO1 HIGH                                                             
         SPACE 1                                                                
         LA    R4,KEY                                                           
         USING TLESD,R4                                                         
         CLC   TLESKEY,KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                DIE IF WE DIDN'T GET RECORD                  
         MVC   KEYSAVE,TLESKEY     SAVE ENTIRE DIR. REC. IN KEYSAVE             
         MVC   AIO,AESTREC         GET RECORD INTO AESTREC                      
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         SPACE 1                                                                
GREC4    GOTO1 SEQ                            LOOK FOR ANOTHER RECORD           
         LA    R4,KEY                                                           
         CLC   TLESKEY(TLESSEQ-TLESD),KEYSAVE WITH ALL SAME UP TO SEQ.          
         BNE   GRECX                                                            
         GOTO1 GETREC              GET NEW RECORD INTO IO1                      
         SPACE 1                                                                
         L     RE,AIO              NOW COMBINE THEM - RE=A(NEW RECORD)          
         L     R4,AESTREC                             R4=A(MAIN RECORD)         
         LH    RF,TLESLEN-TLESD(RE) L'NEW RECORD                                
         SH    RF,DATADISP          LESS DATADISP                               
         SPACE 1                                                                
         LH    R1,TLESLEN                                                       
         BCTR  R1,0                                                             
         LR    R0,R1               R0=R1=L'MAIN RECORD (-1 FOR EOR)             
         SPACE 1                                                                
         AR    R1,RF               PLUS L'NEW RECORD                            
         STH   R1,TLESLEN          IS L'COMBINED RECORD                         
         SPACE 1                                                                
         AR    R0,R4               R=A(END OF MAIN RECORD)                      
         AH    RE,DATADISP         RE=A(1ST EL. IN RECORD)                      
         LR    R1,RF               R1=RF=L'NEW RECORD ELEMENTS                  
         SPACE 1                                                                
         MVCL  R0,RE               MOVE NEW RECORD AFTER MAIN                   
         B     GREC4               LOOK FOR ANOTHER                             
         SPACE 1                                                                
GRECX    MVC   KEY,KEYSAVE         RESTORE ORIG. DIR. REC. TO KEY               
         B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              CHECK AGENCY/CLIENT LIMIT                                        
*&&DO                                                                           
CHKLIM   NTR1                                                                   
         USING TAESD,R4                                                         
                                                                                
         L     R4,AIO                                                           
         MVI   ELCODE,TAESELQ                                                   
         BAS   RE,GETEL                                                         
         B     CHKLIM20                                                         
CHKLIM10 BAS   RE,NEXTEL                                                        
CHKLIM20 BE    *+6                                                              
         DC    H'00'                                                            
         CLI   TAESTYPE,TAESTCLI   R4=A(CLIENT ESTIMATE ELEMENT)                
         BNE   CHKLIM10                                                         
*                                                                               
         LHI   R3,1                                                             
                                                                                
         USING FAWSSVRD,R1                                                      
CHKLIM30 LA    R1,LIMBLK                                                        
         XC    LIMBLK,LIMBLK                                                    
         MVC   FAWSTOKN(3),=C'STF'                                              
         STC   R3,FAWSTOKN+3                                                    
         MVI   FAWSACTN,FAWSARST   RECALL STAFF2 INFORMATION VIA WWSVR          
         XC    FAWSLEN,FAWSLEN                                                  
         MVC   FAWSADR,TGAS2ACC                                                 
         GOTO1 WSSVR,(R1)                                                       
         CLI   FAWSRTN,0           IF NOT FOUND, STAFF HAS NO ACCESS            
         BNE   NO                                                               
         DROP  R1                                                               
*                                                                               
         AHI   R3,1                                                             
*                                                                               
         USING TAVAD,R1                                                         
         L     R1,TGAS2ACC                                                      
         OC    TAVAAGY,TAVAAGY     IF STAFF HAS NO AGENCY LIMITS,               
         BZ    YES                 STAFF HAS ACCESS TO ALL RECORDS              
*                                                                               
CHKLIM40 CLI   0(R1),0             RECALL NEXT RECORD FROM WSSVR                
         BE    CHKLIM30                                                         
*                                                                               
         CLC   TIAGY,TAVAAGY       IF AGENCY IS FOUND IN STAFF LIMITS           
         BNE   CHKLIM60                                                         
*                                                                               
         CLI   TAVALEN,TAVALNQ     IF NO CLIENT LIMITS ARE DEFINED              
         BE    YES                 ACCESS IS GRANTED                            
         OC    TAESCLI,TAESCLI                                                  
         BZ    YES                                                              
*                                                                               
         ZIC   RE,TAVALEN                                                       
         SHI   RE,TAVALNQ                                                       
         LA    RF,TAVACLI                                                       
CHKLIM50 CLC   TAESCLI,0(RF)       IF CLIENT IS FOUND IN STAFF LIMITS           
         BE    YES                 ACCESS IS GRANTED                            
         LA    RF,L'TAVACLI(RF)                                                 
         SHI   RE,L'TAVACLI                                                     
         LTR   RE,RE                                                            
         BNZ   CHKLIM50                                                         
*                                                                               
CHKLIM60 ZIC   RE,TAVALEN          BUMP TO NEXT VALID AGENCY/CLIENT             
         AR    R1,RE               ELEMENT                                      
         B     CHKLIM40                                                         
         DROP  R1,R4                                                            
*&&                                                                             
         EJECT                                                                  
*              FILTER CLIENT                                                    
         SPACE 1                                                                
FILTCLI  NTR1                                                                   
         OC    TGCLI,TGCLI         IF CLIENT FILTER                             
         BZ    YES                                                              
         MVI   BYTE,TAESTCLI                                                    
         MVC   WORK(6),TGCLI       MATCH ON IT                                  
         BAS   RE,CKMATCH                                                       
         B     XIT                 CC SET                                       
         SPACE 2                                                                
*              FILTER PRODUCT                                                   
         SPACE 1                                                                
FILTPRD  NTR1                                                                   
         OC    TGPRD,TGPRD         IF PRODUCT FILTER                            
         BZ    YES                                                              
         MVI   BYTE,TAESTPRD                                                    
         MVC   WORK(6),TGPRD                                                    
         BAS   RE,CKMATCH          MATCH ON IT                                  
         B     XIT                 CC SET                                       
         SPACE 2                                                                
*              ROUTINE TO CHECK IF CLIENT OR PRODUCT IN                         
*              ESTIMATE RECORD                                                  
*                                  NTRY - BYTE = TAESTCLI OR TAESTPRD           
*                                         WORK = CLIENT OR PRODUCT CODE         
*                                  XIT -  CC SET                                
CKMATCH  NTR1                                                                   
         L     R4,AIO              R4=A(FULL ESTIMATE RECORD)                   
         MVI   ELCODE,TAESELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
CKMATCH5 BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         SPACE 1                                                                
         USING TAESD,R4                                                         
         CLC   TAESTYPE,BYTE       MATCH ON ELEMENT TYPE                        
         BNE   CKMATCH5                                                         
         CLC   TAESDATA(6),WORK    MATCH ON ELEMENT DATA                        
         BNE   CKMATCH5                                                         
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO DISPLAY DATES TO SCREEN LINE                          
*                                                                               
DISPDTS  NTR1                                                                   
         ZIC   R0,TWASCR                                                        
         MVI   TWASCR,SCR40                                                     
         GOTO1 ACTVOUT,DMCB,(X'C0',WORK)                                        
         STC   R0,TWASCR                                                        
         OC    WORK+9(8),WORK+9                                                 
         BZ    *+10                                                             
         MVC   LINRDATE,WORK+9     REPORT LAST CHANGED DATE                     
*                                                                               
         GOTO1 ACTVOUT,DMCB,(X'40',WORK)                                        
         OC    WORK+9(8),WORK+9                                                 
         BZ    *+10                                                             
         MVC   LINCDATE,WORK+9     MAINT. LAST CHANGED DATE                     
         B     XIT                                                              
         EJECT                                                                  
*              FILTER DATES                                                     
         SPACE 1                                                                
FILTDTS  NTR1                                                                   
         CLI   SESRDTEH+5,0        IF REPORT DATE FILTER                        
         BE    FILTD20                                                          
         MVC   CURDATE,LINRDATE                                                 
         GOTO1 CKDATE,DMCB,RPTTYPE,RPTDATE                                      
         BNE   XIT                 CC SET                                       
         SPACE 1                                                                
FILTD20  CLI   SESCDTEH+5,0        IF LAST CHANGED DATE FILTER                  
         BE    YES                                                              
         MVC   CURDATE,LINCDATE                                                 
         GOTO1 CKDATE,DMCB,CHGTYPE,CHGDATE                                      
         B     XIT                 CC SET                                       
         SPACE 2                                                                
*              ROUTINE CHECKS ACTIVITY DATES ON ESTIMATE                        
*                                  NTRY P1   = - MEANS <= DATE                  
*                                            = ELSE >= DATE                     
*                                       P2   = RPT OR LCHG DATE                 
*                                       CURDATE= CURRENT DATE                   
*                                  XIT  CC SET                                  
CKDATE   NTR1                                                                   
         LM    R3,R4,0(R1)         R3=A(DATE TYPE), R4=(PWOS DATE)              
         LA    R2,CURDATE          R2=A(SPACE PADDED DATE FOR DTVAL)            
*                                                                               
         CLI   0(R3),C'-'          IF WANT RECS <= DATE                         
         BNE   CKDATE9                                                          
*                                                                               
         CLC   0(8,R2),SPACES                                                   
         BNH   YES                                                              
         GOTO1 DTVAL,DMCB,(X'40',WORK+10)                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   WORK+10(3),0(R4)    REJECT DATES WITH HIGHER ACTIVITY            
         BH    NO                                                               
         B     YES                                                              
*                                                                               
CKDATE9  CLC   0(8,R2),SPACES                                                   
         BNH   NO                                                               
         GOTO1 DTVAL,DMCB,(X'40',WORK+10)                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   WORK+10(3),0(R4)    ELSE, REJECT DATES W/ LOWER ACTIVITY         
         BL    NO                                                               
         B     YES                                                              
         EJECT                                                                  
*              ROUTINE TO SEND A LINE TO QUEUE                                  
         SPACE 1                                                                
PRNTIT   NTR1                                                                   
         MVC   HEAD4+9(6),SESAGY                                                
         MVC   HEAD4+25(L'SESSTR),SESSTR                                        
         MVC   HEAD5+9(6),TGCLI                                                 
         MVC   HEAD5+25(6),TGPRD                                                
         LA    R1,HEAD5+43                                                      
         CLI   RPTTYPE,C'-'                                                     
         BNE   *+12                                                             
         MVI   0(R1),C'-'                                                       
         LA    R1,1(R1)                                                         
         MVC   0(8,R1),RPTDATED                                                 
         LA    R1,HEAD5+64                                                      
         CLI   CHGTYPE,C'-'                                                     
         BNE   *+12                                                             
         MVI   0(R1),C'-'                                                       
         LA    R1,1(R1)                                                         
         MVC   0(8,R1),CHGDATED                                                 
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
*              ERRORS/EXITS                                                     
         SPACE 1                                                                
MISSERRC LA    R2,SESCLIH                                                       
         MVI   ERROR,MISSING                                                    
         B     THEEND                                                           
         SPACE 1                                                                
INVRDATE LA    R2,SESRDTEH                                                      
         B     INVERR                                                           
INVCDATE LA    R2,SESCDTEH                                                      
         B     INVERR                                                           
         SPACE 1                                                                
INVERR   MVI   ERROR,INVALID                                                    
         B     THEEND                                                           
         SPACE 1                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
INFEND   OI    GENSTAT2,USGETTXT                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 3                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 2                                                                
PFTAB    DS    0C                                                               
         DC    AL1(PF23X-*,23,0,(PF23X-PF23)/KEYLNQ,0)                          
         DC    CL3'EST',CL8'ESTIMATE',CL8'CHANGE'                               
PF23     DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'LINEST-1),AL2(LINEST-LINED)                       
PF23X    EQU   *                                                                
         DC    AL1(PF24X-*,24,0,(PF24X-PF24)/KEYLNQ,PFTSETPR)                   
         DC    CL3' ',CL8'ESTIMATE',CL8'REPORT'                                 
PF24     DC    AL1(KEYTYCOM,0),AL2(0)                                           
         DC    AL1(KEYTYGLB,L'TGAGY-1),AL2(TGAGY-TGD)                           
         DC    AL1(KEYTYCUR,L'LINEST-1),AL2(LINEST-LINED)                       
PF24X    EQU   *                                                                
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
MYSPECS  DS    0H                                                               
         SSPEC H1,2,RUN                                                         
         SSPEC H1,32,C'ESTIMATE RECORD LIST'                                    
         SSPEC H2,32,C'--------------------'                                    
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
         SSPEC H4,2,C'AGENCY'                                                   
         SSPEC H4,17,C'START'                                                   
         SSPEC H5,2,C'CLIENT'                                                   
         SSPEC H5,17,C'PRODUCT'                                                 
         SSPEC H5,34,C'RPT DATE'                                                
         SSPEC H5,55,C'CHG DATE'                                                
         SPACE 1                                                                
         SSPEC H7,2,C'ESTIMATE             NAME'                                
         SSPEC H8,2,C'--------             ----'                                
         SSPEC H7,60,C'RPT DATE CHG DATE'                                       
         SSPEC H8,60,C'-------- --------'                                       
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER DISPLAY LINE                                      
         SPACE 1                                                                
LINED    DSECT                                                                  
LINEST   DS    CL(L'TLESEST)                                                    
         DS    CL1                                                              
LINNAME  DS    CL36                                                             
         DS    CL1                                                              
LINRDATE DS    CL8                                                              
         DS    CL1                                                              
LINCDATE DS    CL8                                                              
LINLNQ   EQU   *-LINED                                                          
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCR4CD                                                       
         EJECT                                                                  
*              SAVED STORAGE AT END OF SCREEN                                   
         SPACE 1                                                                
         DS    0A                                                               
AESTREC  DS    A                   A(FULL ESTIMATE RECORD)                      
ESTRECL  EQU   6000                                                             
COUNTER  DS    PL4                 LINE COUNTER                                 
*                                                                               
CURDATE  DS    CL8                 SPACE PADDED DATE                            
         DS    C' '                                                             
RPTINF   DS    0CL12                                                            
RPTTYPE  DS    CL1                                                              
RPTDATE  DS    PL3                 REPORT DATE                                  
RPTDATED DS    CL8                 REPORT DATE DISPLAY FORMAT                   
*                                                                               
CHGINF   DS    0CL12                                                            
CHGTYPE  DS    CL1                                                              
CHGDATE  DS    PL3                 LAST CHANGED DATE                            
CHGDATED DS    CL8                 DISPLAY FORMAT                               
*                                                                               
LIMBLK   DS    XL(FAWSSVRL)                                                     
         SPACE 3                                                                
       ++INCLUDE TASYSIOD                                                       
         EJECT                                                                  
* DDGENTWA                         MUST FOLLOW LAST SCREEN                      
* TASYSDSECT                                                                    
* TAGENWORKD                                                                    
* TAGENEQUS                                                                     
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAWSSVRD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAWSSVRD                                                       
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'075TAGEN4C   07/20/12'                                      
         END                                                                    
