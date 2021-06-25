*          DATA SET TAGEND5    AT LEVEL 037 AS OF 07/20/12                      
*PHASE T702D5C,*                                                                
         TITLE 'T702D5 - HOLD LIST (NETWORK/SPOT TRANSFER RECORDS)'             
T702D5   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T702D5                                                         
         L     RC,0(R1)            RC=GENCON STORAGE AREA                       
         USING GEND,RC                                                          
         L     RA,ATWA             RA=A(TWA)                                    
         USING T702FFD,RA                                                       
         L     R9,ASYSD            R9=ROOT STORAGE AREA                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD          R8=SPOOL DSECT                               
         USING SPOOLD,R8                                                        
         LHI   R5,TASYSBLK-T702FFD                                              
         AR    R5,RA                                                            
         USING TASYSIOD,R5                                                      
         EJECT                                                                  
*              MODE CONTROLLED ROUTINES                                         
         SPACE 1                                                                
         GOTO1 INITIAL,DMCB,0                                                   
         SPACE 1                                                                
         OI    SNXDELLH+1,X'0C'    MAKE HEADER ZERO INTENSITY                   
         OI    SNXDELH+1,X'2C'     MAKE FIELD ZERO INT & PROTECT                
         CLI   TGCTSTTY,TASTTYPP   FOR PROGRAMMERS                              
         BE    *+12                                                             
         CLI   TGCTSTTY,TASTTYP2   AND SYSTEM MANAGERS                          
         BNE   NX20                                                             
         NI    SNXDELLH+1,X'F3'    MAKE HEADER NORMAL INTENSITY                 
         NI    SNXDELH+1,X'D3'     MAKE FIELD NORM INT & UNPROTECT              
*                                                                               
NX20     OI    SNXDELLH+6,X'80'    AND TRANSMIT                                 
         OI    SNXDELH+6,X'80'     AND TRANSMIT                                 
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   *+12                                                             
         BAS   RE,VKEY             VALIDATE KEY FIELDS                          
         B     XIT                                                              
         SPACE 1                                                                
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BNE   NX30                                                             
         LA    R0,LRHOOK           SET A(SYSIO HOOK)                            
         B     NX40                                                             
         SPACE 1                                                                
NX30     CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   XIT                                                              
         ZAP   COUNTER,=P'0'       CLEAR LINE COUNTER                           
         LA    R2,MYSPECS          SET A(SPECS)                                 
         ST    R2,SPECS                                                         
         LA    R2,HOOK             SET A(HOOK)                                  
         ST    R2,HEADHOOK                                                      
         LA    R0,PRHOOK           SET A(SYSIO HOOK)                            
         SPACE 1                                                                
NX40     BAS   RE,LREC             GO LIST THE RECORDS                          
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO VALIDATE KEY FIELDS                                   
         SPACE 1                                                                
VKEY     NTR1                                                                   
*                                                                               
         MVC   LLIST,=Y(LLNQ)      L'DATA LINE                                  
         MVI   NLISTS,7            N'RECORDS PER SCREEN                         
         OI    GLSTSTAT,APPLCDSP+RETEXTRA                                       
*                                                                               
         LA    R2,SNXAGYH          AGENCY                                       
         CLI   SCRSTAT,0           IF SCREEN CHANGED                            
         BNE   VK5                                                              
         TM    4(R2),X'20'         OR NOT PREVIOUSLY VALIDATED                  
         BO    VK10                                                             
VK5      NI    SNXMATH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         GOTO1 RECVAL,DMCB,TLAYCDQ,(X'28',(R2)),SNXAGYNH                        
         MVC   TIFAGY,TGAGY        SET SYSIO FILTER                             
         GOTO1 RAVPPLSA,DMCB,0     SEE IF REC / ACT INVALID FOR P+              
         JNE   ERPPLSI                                                          
*                                                                               
VK10     LA    R2,SNXMATH          R2=A(MATCHED FIELD)                          
         TM    4(R2),X'20'                                                      
         BO    VK20                                                             
         NI    SNXMDTEH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         BAS   RE,YORNVAL          ALLOW Y OR N                                 
*                                                                               
VK20     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SNXMDTEH         R2=A(MATCHED DATE FIELD)                     
         TM    4(R2),X'20'                                                      
         BO    VK30                                                             
         NI    SNXNCLIH+4,X'DF'                                                 
         XC    MTCHDT,MTCHDT       CLEAR MATCHED DATE                           
         CLI   5(R2),0                                                          
         BE    VK30                                                             
         CLI   SNXMAT,C'N'         IF ASKING FOR UNMATCHED                      
         BE    FLDINV              THEN INPUT IN DATE FIELD INVALID             
         GOTO1 DTVAL,DMCB,MTCHDT   VALIDATE DATE                                
*                                                                               
VK30     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SNXNCLIH         R2=A(NETWORK CLIENT FIELD)                   
         TM    4(R2),X'20'                                                      
         BO    VK35                                                             
         NI    SNXNPRDH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         XC    TIFCLI,TIFCLI                                                    
         CLI   5(R2),0                                                          
         BE    VK35                                                             
         GOTO1 ANY                                                              
         MVC   TIFCLI,WORK                                                      
*                                                                               
VK35     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SNXNPRDH         R2=A(NETWORK PRODUCT FIELD)                  
         TM    4(R2),X'20'                                                      
         BO    VK40                                                             
         NI    SNXUSDH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         XC    TIFPRD,TIFPRD                                                    
         CLI   5(R2),0                                                          
         BE    VK40                                                             
         GOTO1 ANY                                                              
         MVC   TIFPRD,WORK                                                      
*                                                                               
VK40     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SNXUSDH          R2=A(USED FIELD)                             
         TM    4(R2),X'20'                                                      
         BO    VK45                                                             
         NI    SNXUDTEH+4,X'DF'    FORCE VALIDATION OF NEXT FIELD               
         BAS   RE,YORNVAL          ALLOW Y OR N                                 
         CLI   8(R2),C'Y'          IF ASKING FOR USED RECORDS                   
         BNE   VK45                                                             
         CLI   SNXMAT,C'N'         THEY ALL MUST BE MATCHED                     
         BE    FLDINV                                                           
         CLI   SNXMAT,C'Y'         IF NO INPUT                                  
         BE    VK45                                                             
         MVI   SNXMAT,C'Y'         SET IT                                       
         OI    SNXMATH+6,X'80'     TRANSMIT                                     
*                                                                               
VK45     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SNXUDTEH         R2=A(USED DATE FIELD)                        
         TM    4(R2),X'20'                                                      
         BO    VK50                                                             
         NI    SNXNIDH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         XC    USEDT,USEDT         CLEAR USED DATE                              
         CLI   5(R2),0                                                          
         BE    VK50                                                             
         CLI   SNXUSD,C'N'         IF ASKING FOR "UNUSED"                       
         BE    FLDINV              THEN INPUT IN DATE FIELD INVALID             
         GOTO1 DTVAL,DMCB,USEDT    VALIDATE DATE                                
*                                                                               
VK50     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SNXNIDH          R2=A(START NID)                              
         TM    4(R2),X'20'                                                      
         BO    VK55                                                             
         NI    SNXFMTH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         XC    TIQSTART,TIQSTART                                                
         CLI   5(R2),0                                                          
         BE    VK55                                                             
         MVC   TIQSTART(L'SNXNID),SNXNID                                        
         OC    TIQSTART,SPACES                                                  
*                                                                               
VK55     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SNXFMTH          R2=A(FORMAT FIELD)                           
         TM    1(R2),X'20'                                                      
         BO    VK60                                                             
         NI    SNXUIDH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         MVI   RDSEQ,C'C'          DEFAULT DISPLAY BY CODE                      
         CLI   5(R2),0                                                          
         BE    VK60                                                             
         CLI   8(R2),C'C'                                                       
         BE    VK60                                                             
         CLI   8(R2),C'A'                                                       
         BNE   FLDINV                                                           
         MVI   RDSEQ,C'A'          DISPLAY BY ALPHA NAME                        
*                                                                               
VK60     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SNXUIDH          R2=A(USER ID FIELD)                          
         TM    4(R2),X'20'                                                      
         BO    VK70                                                             
         NI    SNXDELH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD(S)            
         NI    SNXMEDH+4,X'DF'                                                  
         XC    NUID,NUID                                                        
         CLI   5(R2),0                                                          
         BE    VK70                                                             
         GOTO1 USERVAL,DMCB,(R2)                                                
         MVC   NUID,TGUSER         SAVE USER ID NUMBER                          
*                                                                               
VK70     LA    R2,SNXDELH          R2=A(DELETED FIELD)                          
         TM    1(R2),X'20'         IF NOT PROTECTED                             
         BO    VK80                                                             
         TM    4(R2),X'20'                                                      
         BO    VK80                                                             
         NI    SNXMEDH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         BAS   RE,YORNVAL          VALIDATE Y OR N                              
*                                                                               
VK80     OI    4(R2),X'20'         SET PREVIOUS FIELD VALIDATED                 
         LA    R2,SNXMEDH          R2=A(NETWORK/SPOT MEDIA FIELD)               
         TM    4(R2),X'20'                                                      
         BO    VK90                                                             
         NI    SNXUSEH+4,X'DF'     FORCE VALIDATION OF NEXT FIELD               
         XC    TIFMED,TIFMED                                                    
         CLI   5(R2),0                                                          
         BE    VK90                                                             
         GOTO1 MEDVAL,DMCB,8(R2)                                                
         BNE   FLDINV                                                           
         MVC   TIFMED,TGMENAME                                                  
*                                                                               
VK90     OI    4(R2),X'20'                                                      
         LA    R2,SNXUSEH          R2=A(NETWORK/SPOT USE FIELD)                 
         TM    4(R2),X'20'                                                      
         BO    VKEYX                                                            
         XC    FLTUSE,FLTUSE                                                    
         CLI   5(R2),0                                                          
         BE    VK95                                                             
         GOTO1 USEVAL,DMCB,(X'40',8(R2))                                        
         BNE   FLDINV                                                           
         MVC   FLTUSE,TGUSCDE                                                   
VK95     XC    KEY,KEY             RE-INITIALIZE LIST                           
*                                                                               
VKEYX    B     XIT                                                              
         EJECT                                                                  
*              ROUTINE VALIDATES YES OR NO FIELDS                               
         SPACE 1                                                                
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
*              ROUTINE CONTROLS RECORD LISTING                                  
         SPACE 1                                                                
LREC     NTR1                                                                   
         ST    R0,TIHOOK                                                        
*                                                                               
         MVC   TIUSERID,TWAORIG    SET UP SOME FIELDS FOR SYSIO                 
         MVC   TIQSTAFF,TGCTSTAF                                                
         MVI   TIREAD,TLNXCCDQ     SET RECORD TYPE FOR READS                    
         CLI   RDSEQ,C'A'          IF ALPHA NAME DISPLAY                        
         BNE   *+8                                                              
         MVI   TIREAD,TLNXNCDQ     READ PASSIVE POINTERS                        
         MVC   TIKHOOK,SETLSTK                                                  
         OI    TIQFLAGS,TIQFDIR    PASS DIRECTORY HOOKS                         
         MVC   TIACOMFC,ACOMFACS                                                
         GOTO1 TASYSIO,DMCB,TASYSIOD                                            
*                                                                               
         XC    TIQSKEY,TIQSKEY     EOL - CLEAR CONTINUE KEY                     
         CLI   MODE,PRINTREP                                                    
         BNE   LRX                                                              
         CP    COUNTER,=P'0'       IF ANYTHING REPORTED                         
         BE    LRX                                                              
         BAS   RE,PRNTIT           SKIP A LINE                                  
         EDIT  COUNTER,(5,P+1),COMMAS=YES,ALIGN=LEFT                            
         LR    R1,R0                                                            
         LA    R1,P+2(R1)                                                       
         MVC   0(12,R1),=C'HOLD RECORDS'                                        
         BAS   RE,PRNTIT                                                        
*                                                                               
         TM    WHEN,X'40'          IF SPOOLING AND NOW                          
         BZ    LRX                                                              
         XC    CONSERV,CONSERV     AUTO $DQU                                    
         MVC   CONSERV(4),=C'$DQU'                                              
         TWAXC SNXSELH,PROT=Y                                                   
*                                                                               
LRX      B     XIT                                                              
         EJECT                                                                  
*               PROCESS SYSIO RECORDS TO SCREEN                                 
         SPACE 1                                                                
LRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCDIR      PROCESS DIRECTORY                            
         BNE   LRH30                                                            
         BAS   RE,FILTDIR          FILTER DIRECTORY RECORD                      
         BNE   NO                                                               
         B     YES                                                              
*                                                                               
LRH30    CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   XIT                                                              
         BAS   RE,FILTREC          FILTER RECORD                                
         BNE   NO                                                               
         CLC   LISTNUM,NLISTS      IF ALREADY DISPLAYED MAX                     
         BE    LRH60               GO BACK TO LISTMON                           
*                                                                               
         BAS   RE,DISPLAY          OTHERWISE GO DISPLAY IT                      
         BNE   XIT                                                              
*                                                                               
         MVC   DMDSKADD,TIDSKADD   PASS DISK ADDRESS TO LISTMON                 
*                                                                               
LRH60    GOTO1 LISTMON             CALL LISTMON                                 
LRHX     B     XIT                                                              
         EJECT                                                                  
*               PROCESS SYSIO RECORDS TO PRINT LINE                             
         SPACE 1                                                                
         USING LINED,R3                                                         
PRHOOK   NTR1                                                                   
         CLI   TIMODE,PROCDIR      PROCESS DIRECTORY                            
         BNE   PRH30                                                            
         BAS   RE,FILTDIR          FILTER DIRECTORY RECORD                      
         BE    YES                                                              
         B     NO                                                               
*                                                                               
PRH30    CLI   TIMODE,PROCREC      PROCESS RECORDS                              
         BNE   PRHX                                                             
*                                                                               
         LA    R3,SNXFRSTH         SET A(CURRENT LINE TO 1ST LINE)              
         ST    R3,ATHISLST                                                      
         BAS   RE,DISPLAY          DISPLAY RECORD TO SCREEN                     
         BNE   XIT                                                              
*                                                                               
         MVI   BYTE,C'P'           PRINT LINES                                  
         GOTO1 PRTSCRN,DMCB,(R3),LNAMES,P-4                                     
         AP    COUNTER,=P'1'                                                    
*                                                                               
PRHX     B     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
*              ROUTINE TO FILTER AT DIRECTORY LEVEL                             
         SPACE                                                                  
FILTDIR  NTR1                                                                   
         LA    R6,TIKEY            R6=A(SYSIO'S KEY)                            
         USING TLNXD,R6                                                         
*                                                                               
         CLI   SNXDEL,C'Y'         DELETED ONLY                                 
         BNE   FDIR10                                                           
         TM    TIKEYST,TLNXSDEL    SHOW DELETES                                 
         BZ    NO                                                               
         B     FDIR20                                                           
FDIR10   TM    TIKEYST,TLNXSDEL    ELSE SKIP THEM                               
         BO    NO                                                               
*                                                                               
FDIR20   CLI   SNXMAT,C'Y'         MATCHED ONLY                                 
         BE    *+14                                                             
         OC    MTCHDT,MTCHDT                                                    
         BZ    FDIR30                                                           
         TM    TIKEYST,TLNXSMAT                                                 
         BO    FDIR40                                                           
         B     NO                                                               
*                                                                               
FDIR30   TM    TIKEYST,TLNXSMAT    UNMATCHED ONLY                               
         BO    NO                                                               
*                                                                               
FDIR40   CLI   SNXUSD,C'Y'         USED ONLY                                    
         BE    *+14                                                             
         OC    USEDT,USEDT                                                      
         BZ    FDIR50                                                           
         TM    TIKEYST,TLNXSUSD                                                 
         BO    YES                                                              
         B     NO                                                               
*                                                                               
FDIR50   TM    TIKEYST,TLNXSUSD    UNUSED ONLY                                  
         BO    NO                                                               
         B     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO FILTER AT DIRECTORY LEVEL                             
         SPACE                                                                  
FILTREC  NTR1                                                                   
         USING TLNXD,R6                                                         
         L     R6,TIAREC           R6=A(SYSIO'S KEY)                            
*                                                                               
         OC    NUID,NUID           IF USER ID NUMBER FILTER                     
         BZ    *+14                                                             
         CLC   NUID,TLNXUID        MATCH ON IT                                  
         BNE   NO                                                               
*                                                                               
         MVC   TIUSE,TLNXUSE       SAVE USE CODE                                
*                                                                               
         OC    FLTUSE,FLTUSE       IF USE FILTER                                
         BZ    YES                                                              
         CLC   FLTUSE,TLNXUSE      MATCH ON IT                                  
         BNE   NO                                                               
         B     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY ONE LINE OF RECORD INFO                       
DISPLAY  NTR1                                                                   
         L     R3,ATHISLST         R3=A(THIS LINE)                              
         USING LINED,R3                                                         
         MVC   AIO,TIAREC          SET IOAREA                                   
         L     R6,TIAREC           R6=A(SYSIO RECORD)                           
         USING TLNXD,R6                                                         
*                                                                               
         CLI   TLNXSEQ,0           IF BASE RECORD                               
         BNE   NO                                                               
         BAS   RE,FLTTANX          FILTER/DISPLAY TANXD DATES                   
         BNE   NO                                                               
*                                                                               
         OI    LINEH+6,X'80'       TRANSMIT                                     
         OI    LNAMEH+6,X'80'      TRANSMIT                                     
         XC    USECNT,USECNT       COUNT OF TANPD ELEMENTS                      
         MVC   LINNID,SPACES                                                    
         MVC   LINNID(8),TICID     NETWORK/SPOT COMMERCIAL ID                   
*                                                                               
DISP10   LR    R4,R6                                                            
         MVI   ELCODE,TANXELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST FIND ON BASE RECORD                     
         USING TANXD,R4                                                         
*                                                                               
         TM    TLNXSTAT,TLNXADID+TLNXPACK   AD-ID COMML OR PACKED?              
         BZ    DISP13                                                           
         CLI   TANXLEN,TANXLN2Q                                                 
         BL    DISP13                                                           
         MVC   LINNID,TANXADID                                                  
*                                                                               
DISP13   MVC   LINUID,TANXUID      NETWORK/SPOT USER ID                         
         EDIT  TANXSEC,(3,LINLEN)  LENGTH IN SECONDS                            
*                                                                               
         MVI   LINCHG,C' '                                                      
         CLI   TANXCCDE,0          IF CHANGE CODE                               
         BE    *+8                                                              
         MVI   LINCHG,C'*'         INDICATE WITH A '*'                          
*                                                                               
         MVC   LINNCLI,TICLI       NETWORK/SPOT CLIENT CODE                     
         MVC   LINNPRD,TIPRD       NETWORK/SPOT PRODUCT CODE                    
         MVC   LINMED,TIMED        MEDIA CODE                                   
         MVC   WORK(L'TLNXDATE),TLNXDATE                                        
         XC    WORK,XFFS                                                        
         GOTO1 DATCON,DMCB,(1,WORK),(16,LINDATE)                                
*                                                                               
         BAS   RE,DISPCID          DISPLAY TALENT CID                           
*                                                                               
         XC    LINUSES,LINUSES                                                  
         MVC   LINUSE,TIUSE        SHOW USE                                     
         CLC   TIUSE,=C'CLA'       IF HOLD RECORD FOR CLA                       
         BE    *+14                                                             
         CLC   TIUSE(2),=C'LN'     OR LATE NIGHT USES                           
         BNE   DISP20                                                           
         BAS   RE,DSPCNT                                                        
         EDIT  USECNT,(3,LINUSES)  DISPLAY COUNT OF TAND ELEMENTS               
*                                                                               
DISP20   BAS   RE,DSPNAMES         DISPLAY NAMES ON SECOND LINE                 
*                                                                               
         XC    KEY,KEY             RESET READ SEQUENCE FOR SYSIO                
         MVC   KEY(L'TIKEY),TIKEY                                               
         GOTO1 HIGH                                                             
         B     YES                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
*              ROUTINE TO FILTER DATES AND DISPLAY IF CC EQUAL                  
         SPACE                                                                  
FLTTANX  NTR1                                                                   
         LR    R4,R6                                                            
         MVI   ELCODE,TANXELQ      NETWORK/SPOT TRANSFER DTLS ELEMENT           
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TANXD,R4                                                         
*                                                                               
         OC    MTCHDT,MTCHDT       IF MATCHED FILTER DATE                       
         BZ    FLTTNX10                                                         
         CLC   MTCHDT,TANXMDTE     MATCH ON IT                                  
         BNE   NO                                                               
*                                                                               
FLTTNX10 OC    USEDT,USEDT         IF USED FILTER DATE                          
         BZ    *+14                                                             
         CLC   USEDT,TANXUDTE      MATCH ON IT                                  
         BNE   NO                                                               
*                                  DISPLAY MATCHED DATE                         
         GOTO1 DATCON,DMCB,(1,TANXMDTE),(16,LINMDTE)                            
*                                  DISPLAY USE DATE                             
         GOTO1 DATCON,DMCB,(1,TANXUDTE),(16,LINUDTE)                            
         B     YES                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY TALENT CID                                    
         SPACE                                                                  
         USING TANXD,R4            R4=A(NETWORK/SPOT TRANSFER DTL EL)           
DISPCID  NTR1                                                                   
         MVC   LINCID,SPACES                                                    
         OC    TANXCOM,TANXCOM     IF INTERNAL COMMERCIAL NUMBER                
         BZ    DISPCIDX                                                         
         MVC   AIO,AIO2            ELSE DISPLAY TALENT CID                      
         GOTO1 RECVAL,DMCB,TLCOCCDQ,(X'A0',TANXCOM)                             
         MVC   AIO,TIAREC                                                       
         BNE   DISPCIDX                                                         
         L     R4,AIO2                                                          
         MVI   ELCODE,TACOELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING TACOD,R4                                                         
         MVC   LINCID,TACOCID                                                   
DISPCIDX B     XIT                                                              
         DROP  R4                                                               
         SPACE 2                                                                
*              ROUTINE TO DISPLAY SECOND LINE OF NAMES                          
         SPACE                                                                  
         USING TANXD,R4            R4=A(NETWORK/SPOT TRANSFER DTL EL)           
DSPNAMES NTR1                                                                   
         GOTO1 CHAROUT,DMCB,TAFNELQ,0,TAFNTTTL                                  
         MVC   LINNTTL,TGNAME      NETWORK/SPOT COMML TITLE NAME                
*                                                                               
         GOTO1 CHAROUT,DMCB,TAFNELQ,0,TAFNTCLI                                  
         MVC   LINNCLIN,TGNAME     NETWORK/SPOT CLIENT NAME                     
*                                                                               
         GOTO1 CHAROUT,DMCB,TAFNELQ,0,TAFNTPRD                                  
         MVC   LINNPRDN,TGNAME     NETWORK/SPOT PRODUCT NAME                    
*                                                                               
         MVC   LINMETHD,SPACES                                                  
         LA    RE,MTCHTAB          RE=A(MATCHED METHOD TABLE)                   
DSPNAM10 CLI   0(RE),X'FF'         TEST END OF TABLE                            
         BE    DSPNAMX                                                          
         CLC   TANXTYPE,0(RE)      TEST MATCH TYPE                              
         BE    *+12                                                             
         LA    RE,L'MTCHTAB(RE)                                                 
         B     DSPNAM10                                                         
         MVC   LINMETHD,1(RE)      SET METHOD                                   
*                                                                               
         CLI   TANXTYPE,TANXTVER   PRINT VERSION LETTER                         
         BNE   DSPNAMX                                                          
         MVC   LINMETHD+5(1),TANXVERS                                           
DSPNAMX  B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*              ROUTINE TO DISPLAY COUNT OF USES FOR NETWORK COMM'L              
         SPACE                                                                  
         USING TLNXD,R6            R6=A(HOLD RECORD)                            
DSPCNT   NTR1                                                                   
         MVI   FRSTREC,C'Y'        FIRST RECORD                                 
         LR    R4,R6               R4=A(HOLD RECORD)                            
*                                                                               
DSPCNT5  LH    R1,USECNT           COUNTER FOR NUMBER OF USE ELEMENTS           
         MVI   ELCODE,TANPELQ                                                   
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DSPCNT10 BAS   RE,NEXTEL                                                        
         BNE   *+12                                                             
         LA    R1,1(R1)            BUMP COUNTER                                 
         B     DSPCNT10                                                         
*                                                                               
         STH   R1,USECNT                                                        
         CLI   FRSTREC,C'Y'        IF FIRST RECORD                              
         BNE   DSPCNT20                                                         
         MVI   FRSTREC,C'N'                                                     
         XC    KEY,KEY                                                          
         MVC   KEY(L'TLNXKEY),0(R6)                                             
         GOTO1 HIGH                RE-READ ACTIVE POINTER                       
         CLC   KEY(L'TLNXKEY),0(R6)                                             
         BE    *+6                                                              
         DC    H'0'                MUST EXIST                                   
*                                                                               
DSPCNT20 GOTO1 SEQ                                                              
         CLC   KEY(TLNXSEQ-TLNXD),KEYSAVE                                       
         BNE   DSPCNTX                                                          
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                     GET THE RECORD                        
         MVC   AIO,TIAREC                                                       
         L     R4,AIO2                                                          
         B     DSPCNT5                                                          
*                                                                               
DSPCNTX  B     XIT                                                              
         DROP  R3,R6                                                            
         EJECT                                                                  
*              ROUTINE TO PRINT A LINE                                          
         SPACE                                                                  
PRNTIT   NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         SPACE 3                                                                
*              HEADLINE HOOK                                                    
         SPACE                                                                  
HOOK     NTR1                                                                   
         MVI   BYTE,C'H'           MOVE KEY FIELDS TO HEADS                     
         GOTO1 PRTSCRN,DMCB,CONTAGH,SNXKEYXH,H5                                 
         MVI   BYTE,C'P'           RESET                                        
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
         LA    R2,SNXSELH                                                       
         B     THEEND                                                           
         SPACE 2                                                                
ERPPLSI  MVC   MYMSGNO,=Y(ERRIAPPA)   RECORD / ACTION INVALID FOR P+            
         J     ERREND                                                           
                                                                                
ERREND   MVI   MYMTYP,GTMERR       ERROR MESSAGE EXIT                           
         OI    GENSTAT2,USGETTXT                                                
         J     THEEND                                                           
                                                                                
THEEND   GOTO1 EXIT,DMCB,0                                                      
         SPACE 2                                                                
YES      XR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         SPACE 3                                                                
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
*              CONSTANTS, ETC.                                                  
         SPACE 1                                                                
XFFS     DC    6X'FF'                                                           
         SPACE 2                                                                
*              TABLE OF MATCHED METHODS                                         
MTCHTAB  DS    0CL9                                                             
         DC    AL1(TANXTTAL),CL8'TAL CID'                                       
         DC    AL1(TANXTLFT),CL8'LIFT ID'                                       
         DC    AL1(TANXTAKA),CL8'ALIAS'                                         
         DC    AL1(TANXTALF),CL8'ALIAS(L)'                                      
         DC    AL1(TANXTVER),CL8'VERS'                                          
         DC    X'FF'                                                            
         SPACE 2                                                                
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
         SSPEC H1,33,C'HOLD LIST'                                               
         SSPEC H2,33,C'---------'                                               
         SPACE 1                                                                
         SSPEC H9,2,C'TRAF ID   LEN USERID CLI PRD T HLD DATE'                  
         SSPEC H10,2,C'-------   --- ------ --- --- - --------'                 
         SSPEC H9,42,C'TALENT CID   USED     MATCHED  USE'                      
         SSPEC H10,42,C'----------   ----     -------  ---'                     
         SPACE 1                                                                
         DC    X'00'                                                            
         EJECT                                                                  
*              DSECT TO COVER LIST LINE                                         
         SPACE 1                                                                
LINED    DSECT                                                                  
LINEH    DS    CL8                 HEADER                                       
LINEDATA DS    CL74                                                             
         ORG   LINEDATA                                                         
LINNID   DS    CL12                NETWORK/SPOT COMMERCIAL ID                   
LINCHG   DS    CL1                 '*' IF CHANGED UNIT                          
         DS    CL1                                                              
LINLEN   DS    CL3                 LENGTH IN SECONDS                            
         DS    CL1                                                              
LINUID   DS    CL6                 USER-ID (NETWORK/SPOT WORKER FILE)           
         DS    CL1                                                              
LINNCLI  DS    CL3                 NETWORK/SPOT CLIENT CODE                     
         DS    CL1                                                              
LINNPRD  DS    CL3                 NETWORK/SPOT PRODUCT CODE                    
         DS    CL1                                                              
LINMED   DS    CL1                 MEDIA CODE                                   
         DS    CL1                                                              
LINDATE  DS    CL5                 HOLD DATE                                    
         DS    CL1                                                              
LINCID   DS    CL(L'TGCID)         TALENT COMMERCIAL ID                         
         DS    CL1                                                              
LINUDTE  DS    CL5                 USED DATE                                    
         DS    CL1                                                              
LINMDTE  DS    CL5                 MATCHED DATE                                 
         DS    CL1                                                              
LINUSE   DS    CL3                 USE                                          
         ORG                                                                    
*                                                                               
LNAMEH   DS    CL8                                                              
LNAMES   DS    CL74                                                             
         ORG   LNAMES                                                           
LINNTTL  DS    CL20                NETWORK TITLE NAME                           
         DS    CL1                                                              
LINNCLIN DS    CL20                NETWORK CLIENT NAME                          
         DS    CL4                                                              
LINNPRDN DS    CL16                NETWORK PRODUCT NAME                         
         DS    CL1                                                              
LINMETHD DS    CL8                 METHOD OF MATCH                              
         DS    CL1                                                              
LINUSES  DS    CL3                 NUMBER OF USE ELEMENTS (NOT FOR WSP)         
         ORG                                                                    
LLNQ     EQU   *-LINED                                                          
         EJECT                                                                  
       ++INCLUDE TAGENFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE TASCRD5D                                                       
COUNTER  DS    PL4                 RECORD COUNTER                               
USECNT   DS    H                   NUMBER OF USES                               
RDSEQ    DS    XL1                 ACTIVE/PASSIVE POINTER READ                  
FRSTREC  DS    XL1                 Y=FIRST READ ON RECORD                       
NUID     DS    XL2                 USERID ID NUMER                              
FLTUSE   DS    XL3                 USE CODE                                     
MTCHDT   DS    XL3                 REQUESTED MATCH DATE                         
USEDT    DS    XL3                 REQUESTED USE DATE                           
         EJECT                                                                  
* DDGENTWA      (MUST FOLLOW LAST SCREEN)                                       
* TASYSIOD      (MUST FOLLOW LAST SCREEN)                                       
* TASYSDSECT                                                                    
* TAGENEQUS                                                                     
* TASYSEQUS                                                                     
* TAGENWORKD                                                                    
* TAGENFILE                                                                     
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
*                                                                               
         ORG   CONTAGH+2300        CHANGE IF SCREEN 9F IS TOO BIG               
         DS    0D                                                               
TASYSBLK DS    (TIEND-TASYSIOD)C                                                
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         DSECT                                                                  
       ++INCLUDE TASYSIOD                                                       
         PRINT OFF                                                              
       ++INCLUDE TASYSDSECT                                                     
       ++INCLUDE TAGENEQUS                                                      
       ++INCLUDE TASYSEQUS                                                      
       ++INCLUDE TAGENWORKD                                                     
       ++INCLUDE TAGENFILE                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037TAGEND5   07/20/12'                                      
         END                                                                    
